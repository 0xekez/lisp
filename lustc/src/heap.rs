//! A heap without a garbage collector
//! A heap without manual dealocation
//! Some things truly never die

use cranelift::frontend::FunctionBuilder;
use cranelift::prelude::AbiParam;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::Value;
use cranelift_codegen::binemit::NullTrapSink;
use cranelift_module::Module;

use crate::compiler::JIT;

// Emits an 'alloc' function which when called makes a call to malloc.
pub fn define_alloc(
    jit: &mut JIT,
    unwind_context: &mut crate::debug::UnwindContext,
) -> Result<(), String> {
    let _t = crate::timer::timeit("emit alloc");
    let reftype = jit.reference_type();
    let wordtype = jit.module.target_config().pointer_type();

    // Take a size in bytes to allocate
    jit.context
        .func
        .signature
        .params
        .push(AbiParam::new(wordtype));

    // Return a pointer to the allocated bytes
    jit.context
        .func
        .signature
        .returns
        .push(AbiParam::new(reftype));

    let mut builder = FunctionBuilder::new(&mut jit.context.func, &mut jit.builder_context);
    let entry_block = builder.create_block();

    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);

    // Make a call to malloc:
    let mut sig = jit.module.make_signature();

    sig.params.push(AbiParam::new(wordtype));
    sig.returns.push(AbiParam::new(reftype));

    let callee = jit
        .module
        .declare_function("malloc", cranelift_module::Linkage::Import, &sig)
        .map_err(|e| e.to_string())?;

    let local_callee = jit.module.declare_func_in_func(callee, &mut builder.func);

    let size = builder.block_params(entry_block)[0];
    let args = vec![size];

    let call = builder.ins().call(local_callee, &args);
    let res = builder.inst_results(call)[0];
    let res = builder.ins().raw_bitcast(reftype, res);

    // Trigger the garbage collector
    let mut sig = jit.module.make_signature();
    sig.params.push(AbiParam::new(wordtype));

    let callee = jit
        .module
        .declare_function("do_gc", cranelift_module::Linkage::Import, &sig)
        .map_err(|e| e.to_string())?;

    let local_callee = jit.module.declare_func_in_func(callee, &mut builder.func);

    builder.ins().call(local_callee, &[size]);

    builder.ins().return_(&[res]);

    builder.seal_all_blocks();
    builder.finalize();

    let id = jit
        .module
        .declare_function(
            "alloc",
            cranelift_module::Linkage::Export,
            &jit.context.func.signature,
        )
        .unwrap();

    jit.module
        .define_function(
            id,
            &mut jit.context,
            &mut NullTrapSink {},
            &mut cranelift_codegen::binemit::NullStackMapSink {},
        )
        .unwrap();

    unwind_context.add_function(id, &jit.context, jit.module.isa())?;

    jit.module.clear_context(&mut jit.context);

    Ok(())
}

pub(crate) fn emit_alloc(size: i64, ctx: &mut crate::compiler::Context) -> Result<Value, String> {
    let reftype = ctx.reftype;
    let wordtype = ctx.wordtype;

    let mut sig = ctx.module.make_signature();

    sig.params.push(AbiParam::new(wordtype));
    sig.returns.push(AbiParam::new(reftype));

    let callee = ctx
        .module
        .declare_function("alloc", cranelift_module::Linkage::Import, &sig)
        .map_err(|e| e.to_string())?;

    let local_callee = ctx
        .module
        .declare_func_in_func(callee, &mut ctx.builder.func);

    let size = ctx.builder.ins().iconst(wordtype, size);
    let args = vec![size];

    let call = ctx.builder.ins().call(local_callee, &args);
    let res = ctx.builder.inst_results(call)[0];

    Ok(res)
}

pub(crate) fn emit_free(what: Value, ctx: &mut crate::compiler::Context) -> Result<(), String> {
    let mut sig = ctx.module.make_signature();
    sig.params.push(AbiParam::new(ctx.reftype));

    let callee = ctx
        .module
        .declare_function("free", cranelift_module::Linkage::Import, &sig)
        .map_err(|e| e.to_string())?;

    let local_callee = ctx
        .module
        .declare_func_in_func(callee, &mut ctx.builder.func);

    let args = vec![what];
    ctx.builder.ins().call(local_callee, &args);

    Ok(())
}
