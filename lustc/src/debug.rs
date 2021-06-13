//! Unwind info generation

//! This code is almost entirely borrowed from rustc_codegen_cranelift
//! by bjorn3. Most of the modifications are removal of parts I do not
//! understand. Future work on the garbage collector should repurpose
//! this to use more lust specific code.

use cranelift_codegen::isa::{unwind::UnwindInfo, TargetIsa};
use cranelift_module::FuncId;
use gimli::write::{Address, CieId, EhFrame, EndianVec, FrameTable, Writer};
use gimli::{RunTimeEndian, SectionId};

pub struct UnwindContext {
    endian: RunTimeEndian,
    frame_table: FrameTable,
    cie_id: Option<CieId>,
}

impl UnwindContext {
    pub(crate) fn new(isa: &dyn TargetIsa) -> Self {
        let endian = if cfg!(target_endian = "little") {
            RunTimeEndian::Little
        } else {
            RunTimeEndian::Big
        };
        let mut frame_table = FrameTable::default();

        let cie_id = match isa.create_systemv_cie() {
            Some(cie) => Some(frame_table.add_cie(cie)),
            None => None,
        };

        Self {
            endian,
            frame_table,
            cie_id,
        }
    }

    /// Registers a function with the unwind frame table.
    pub(crate) fn add_function(
        &mut self,
        func_id: FuncId,
        context: &cranelift_codegen::Context,
        isa: &dyn TargetIsa,
    ) -> Result<(), String> {
        let unwind_info = if let Some(unwind_info) =
            context.create_unwind_info(isa).map_err(|e| e.to_string())?
        {
            unwind_info
        } else {
            return Ok(());
        };

        match unwind_info {
            UnwindInfo::SystemV(unwind_info) => self.frame_table.add_fde(
                self.cie_id.unwrap(),
                unwind_info.to_fde(Address::Symbol {
                    symbol: func_id.as_u32() as usize,
                    addend: 0,
                }),
            ),
            _ => unimplemented!(
                "unwind info generation not implemented for: ({:?})",
                unwind_info
            ),
        }

        Ok(())
    }

    /// Registers all of the function in the unwind table with JIT_MODULE.
    pub(crate) unsafe fn register_jit(self, jit_module: &cranelift_jit::JITModule) {
        let mut eh_frame = EhFrame::from(WriterRelocate::new(self.endian));
        self.frame_table.write_eh_frame(&mut eh_frame).unwrap();

        if eh_frame.0.writer.slice().is_empty() {
            return;
        }

        let mut eh_frame = eh_frame.0.relocate_for_jit(jit_module);

        // GCC expects a terminating "empty" length, so write a 0
        // length at the end of the table.
        eh_frame.extend(&[0, 0, 0, 0]);

        #[allow(unused_variables)]
        let (eh_frame, eh_frame_len, _) = Vec::into_raw_parts(eh_frame);

        // ================================================================
        // Everything after this line up to the end of the file is
        // loosly based on
        // https://github.com/bytecodealliance/wasmtime/blob/4471a82b0c540ff48960eca6757ccce5b1b5c3e4/crates/jit/src/unwind/systemv.rs
        #[cfg(target_os = "macos")]
        {
            // On macOS, `__register_frame` takes a pointer to a single FDE
            let start = eh_frame;
            let end = start.add(eh_frame_len);
            let mut current = start;

            // Walk all of the entries in the frame table and register them
            while current < end {
                let len = std::ptr::read::<u32>(current as *const u32) as usize;

                // Skip over the CIE
                if current != start {
                    __register_frame(current);
                }

                // Move to the next table entry (+4 because the length itself is not inclusive)
                current = current.add(len + 4);
            }
        }
        #[cfg(not(target_os = "macos"))]
        {
            // On other platforms, `__register_frame` will walk the
            // FDEs until an entry of length 0
            __register_frame(eh_frame);
        }
    }
}

extern "C" {
    // libunwind import
    fn __register_frame(fde: *const u8);
}

#[derive(Clone)]
pub(crate) struct DebugReloc {
    pub(crate) offset: u32,
    pub(crate) size: u8,
    pub(crate) name: DebugRelocName,
    pub(crate) addend: i64,
    pub(crate) kind: object::RelocationKind,
}

#[derive(Clone)]
pub(crate) enum DebugRelocName {
    Section(SectionId),
    Symbol(usize),
}

/// A [`Writer`] that collects all necessary relocations.
#[derive(Clone)]
pub(super) struct WriterRelocate {
    pub(super) relocs: Vec<DebugReloc>,
    pub(super) writer: EndianVec<RunTimeEndian>,
}

impl WriterRelocate {
    pub(crate) fn new(endian: RunTimeEndian) -> Self {
        WriterRelocate {
            relocs: Vec::new(),
            writer: EndianVec::new(endian),
        }
    }

    /// Perform the collected relocations to be usable for JIT usage.
    pub(crate) fn relocate_for_jit(mut self, jit_module: &cranelift_jit::JITModule) -> Vec<u8> {
        use std::convert::TryInto;

        for reloc in self.relocs.drain(..) {
            match reloc.name {
                DebugRelocName::Section(_) => unreachable!(),
                DebugRelocName::Symbol(sym) => {
                    let addr = jit_module.get_finalized_function(
                        cranelift_module::FuncId::from_u32(sym.try_into().unwrap()),
                    );
                    let val = (addr as u64 as i64 + reloc.addend) as u64;
                    self.writer
                        .write_udata_at(reloc.offset as usize, val, reloc.size)
                        .unwrap();
                }
            }
        }
        self.writer.into_vec()
    }
}

impl Writer for WriterRelocate {
    type Endian = RunTimeEndian;

    fn endian(&self) -> Self::Endian {
        self.writer.endian()
    }

    fn len(&self) -> usize {
        self.writer.len()
    }

    fn write(&mut self, bytes: &[u8]) -> gimli::write::Result<()> {
        self.writer.write(bytes)
    }

    fn write_at(&mut self, offset: usize, bytes: &[u8]) -> gimli::write::Result<()> {
        self.writer.write_at(offset, bytes)
    }

    fn write_address(&mut self, address: Address, size: u8) -> gimli::write::Result<()> {
        match address {
            Address::Constant(val) => self.write_udata(val, size),
            Address::Symbol { symbol, addend } => {
                let offset = self.len() as u64;
                self.relocs.push(DebugReloc {
                    offset: offset as u32,
                    size,
                    name: DebugRelocName::Symbol(symbol),
                    addend: addend as i64,
                    kind: object::RelocationKind::Absolute,
                });
                self.write_udata(0, size)
            }
        }
    }

    fn write_offset(
        &mut self,
        val: usize,
        section: SectionId,
        size: u8,
    ) -> gimli::write::Result<()> {
        let offset = self.len() as u32;
        self.relocs.push(DebugReloc {
            offset,
            size,
            name: DebugRelocName::Section(section),
            addend: val as i64,
            kind: object::RelocationKind::Absolute,
        });
        self.write_udata(0, size)
    }

    fn write_offset_at(
        &mut self,
        offset: usize,
        val: usize,
        section: SectionId,
        size: u8,
    ) -> gimli::write::Result<()> {
        self.relocs.push(DebugReloc {
            offset: offset as u32,
            size,
            name: DebugRelocName::Section(section),
            addend: val as i64,
            kind: object::RelocationKind::Absolute,
        });
        self.write_udata_at(offset, 0, size)
    }

    fn write_eh_pointer(
        &mut self,
        address: Address,
        eh_pe: gimli::DwEhPe,
        size: u8,
    ) -> gimli::write::Result<()> {
        match address {
            // Address::Constant arm copied from gimli
            Address::Constant(val) => {
                // Indirect doesn't matter here.
                let val = match eh_pe.application() {
                    gimli::DW_EH_PE_absptr => val,
                    gimli::DW_EH_PE_pcrel => {
                        // TODO: better handling of sign
                        let offset = self.len() as u64;
                        offset.wrapping_sub(val)
                    }
                    _ => {
                        return Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe));
                    }
                };
                self.write_eh_pointer_data(val, eh_pe.format(), size)
            }
            Address::Symbol { symbol, addend } => match eh_pe.application() {
                gimli::DW_EH_PE_pcrel => {
                    let size = match eh_pe.format() {
                        gimli::DW_EH_PE_sdata4 => 4,
                        _ => return Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe)),
                    };
                    self.relocs.push(DebugReloc {
                        offset: self.len() as u32,
                        size,
                        name: DebugRelocName::Symbol(symbol),
                        addend,
                        kind: object::RelocationKind::Relative,
                    });
                    self.write_udata(0, size)
                }
                _ => Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe)),
            },
        }
    }
}
