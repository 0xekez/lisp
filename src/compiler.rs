use crate::primitives::emit_primcall;
use crate::Expr;
use cranelift::frontend::FunctionBuilder;
use cranelift::prelude::*;
use cranelift_module::{Linkage, Module};
use cranelift_simplejit::{SimpleJITBuilder, SimpleJITModule};

/// Manages the state needed for compilation and execution of a
/// program.
pub struct JIT {
    /// Context for building functions. Holds state that is cleared
    /// between functions so that we don't have to allocate new data
    /// structures every time.
    builder_context: FunctionBuilderContext,

    /// The main context for code generation.
    context: codegen::Context,

    /// Used to emit code directly into memory for execution.
    module: SimpleJITModule,
}

impl Default for JIT {
    fn default() -> Self {
        let builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());
        let module = SimpleJITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            context: module.make_context(),
            module,
        }
    }
}

/// Emits the code for an expression using the given builder.
pub fn emit_expr(
    expr: &Expr,
    builder: &mut FunctionBuilder,
    word: types::Type,
) -> Result<Value, String> {
    Ok(match expr {
        Expr::Integer(_) => builder.ins().iconst(word, expr.immediate_rep()),
        Expr::Char(_) => builder.ins().iconst(word, expr.immediate_rep()),
        Expr::Bool(_) => builder.ins().iconst(word, expr.immediate_rep()),
        Expr::Nil => builder.ins().iconst(word, expr.immediate_rep()),
        Expr::Symbol(_) => todo!("symbol evaluation"),
        Expr::List(v) => {
            if expr.is_primcall() {
                emit_primcall(expr.primcall_op(), &v[1..], builder, word)?
            } else {
                todo!("non primitive function application")
            }
        }
    })
}

/// Compiles an expression and returns the result converted back into
/// an expression.
///
/// NOTE: when our main mehtod gets more interesting this should only
/// be included during test builds.
pub fn roundtrip_expr(expr: Expr) -> Result<Expr, String> {
    let mut jit = JIT::default();

    let word = jit.module.target_config().pointer_type();

    // Signature for the function that we're compiling. This function
    // takes no arguments and returns an integer.
    jit.context.func.signature.returns.push(AbiParam::new(word));

    // Create a new builder for building our function and create a new
    // block to compile into.
    let mut builder = FunctionBuilder::new(&mut jit.context.func, &mut jit.builder_context);
    let entry_block = builder.create_block();

    // Give the paramaters that we set up earlier to this entry block.
    builder.append_block_params_for_function_params(entry_block);
    // Start putting code in the new block.
    builder.switch_to_block(entry_block);

    // Compile the value and get the "output" of the instrution stored
    // in `val`.
    let val = emit_expr(&expr, &mut builder, word)?;

    // Emit a return instruction to return the result.
    builder.ins().return_(&[val]);

    // Clean up
    builder.seal_all_blocks();
    builder.finalize();

    let id = jit
        .module
        .declare_function("lust_entry", Linkage::Export, &jit.context.func.signature)
        .map_err(|e| e.to_string())?;

    jit.module
        .define_function(id, &mut jit.context, &mut codegen::binemit::NullTrapSink {})
        .map_err(|e| e.to_string())?;

    jit.module.clear_context(&mut jit.context);

    jit.module.finalize_definitions();

    let code_ptr = jit.module.get_finalized_function(id);

    let code_fn = unsafe { std::mem::transmute::<_, fn() -> i64>(code_ptr) };

    Ok(Expr::from_immediate(code_fn()))
}
