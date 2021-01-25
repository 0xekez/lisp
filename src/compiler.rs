//! Crossword compiler whom did suddenly die
//! The doctors and police, didn't really no why
//! The compiler he was clever he left them a clue
//! Still the stupid ******* never new what to do
//! It wasn't cryptic it had no hidden undertones
//! It wasn't suicide, I'm messaging it on a phone
//! So how did he or she come to Meet their maker
//! Answers You will find in tomorrow's  local paper.
//!
//! - [Mark Bell](https://hellopoetry.com/poem/1927377/give-us-a-clue/)

use std::collections::HashMap;

use crate::conditional;
use crate::data;
use crate::escape;
use crate::heap::define_alloc;
use crate::locals;
use crate::primitives;
use crate::procedures;
use crate::renamer;
use crate::Expr;
use cranelift::frontend::FunctionBuilder;
use cranelift::prelude::*;
use cranelift_module::DataContext;
use cranelift_module::{Linkage, Module};
use cranelift_simplejit::{SimpleJITBuilder, SimpleJITModule};
use procedures::emit_procedure;
use procedures::LustFn;

/// Manages the state needed for compilation by cranelift and
/// execution of a program.
pub struct JIT {
    /// Context for building functions. Holds state that is cleared
    /// between functions so that we don't have to allocate new data
    /// structures every time.
    pub builder_context: FunctionBuilderContext,

    /// The main context for code generation.
    pub context: codegen::Context,

    /// Used to emit code directly into memory for execution.
    pub module: SimpleJITModule,

    // Stores information about data objects that the JIT owns.
    pub data_ctx: DataContext,
}

/// Manages the state needed for compilation of a function by lustc.
pub(crate) struct Context<'a> {
    pub builder: FunctionBuilder<'a>,
    pub module: &'a mut SimpleJITModule,
    pub word: types::Type,
    pub env: HashMap<String, Variable>,
    pub fnmap: HashMap<String, LustFn>,
    // A stack of variables that are curently being defined. These
    // variables are in a "defined but not initialized state" and
    // closures care about this.
    pub letstack: Vec<String>,
}

impl Default for JIT {
    fn default() -> Self {
        let builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());
        let module = SimpleJITModule::new(builder);
        let mut jit = Self {
            builder_context: FunctionBuilderContext::new(),
            context: module.make_context(),
            module,
            data_ctx: DataContext::new(),
        };
        define_alloc(&mut jit).unwrap();
        jit
    }
}

impl<'a> Context<'a> {
    pub fn new(
        builder: FunctionBuilder<'a>,
        module: &'a mut SimpleJITModule,
        word: types::Type,
        env: HashMap<String, Variable>,
        fnmap: HashMap<String, LustFn>,
        letstack: Vec<String>,
    ) -> Self {
        Self {
            builder,
            module,
            word,
            env,
            fnmap,
            letstack,
        }
    }
}

/// Emits the code for an expression using the given builder.
pub(crate) fn emit_expr(expr: &Expr, ctx: &mut Context) -> Result<Value, String> {
    Ok(match expr {
        Expr::Integer(_) => ctx.builder.ins().iconst(ctx.word, expr.immediate_rep()),
        Expr::Char(_) => ctx.builder.ins().iconst(ctx.word, expr.immediate_rep()),
        Expr::Bool(_) => ctx.builder.ins().iconst(ctx.word, expr.immediate_rep()),
        Expr::Nil => ctx.builder.ins().iconst(ctx.word, expr.immediate_rep()),
        Expr::Symbol(name) => locals::emit_var_access(name, ctx)?,
        Expr::List(v) => {
            if expr.is_primcall() {
                primitives::emit_primcall(expr.primcall_op(), &v[1..], ctx)?
            } else if let Some((s, e)) = expr.is_let() {
                locals::emit_let(s, e, ctx)?
            } else if let Some((cond, then, else_)) = expr.is_conditional() {
                conditional::emit_conditional(cond, then, else_, ctx)?
            } else if let Some((name, args)) = expr.is_fncall() {
                procedures::emit_fncall(name, args, ctx)?
            } else if v.len() == 0 {
                // A () == Expr::nil
                ctx.builder.ins().iconst(ctx.word, expr.immediate_rep())
            } else {
                return Err(format!("illegal function application {:?}", v));
            }
        }
    })
}

pub fn roundtrip_program(program: &mut [Expr]) -> Result<Expr, String> {
    let mut jit = JIT::default();

    // Rename symbols so that they are all unique.
    renamer::make_names_unique(program)?;

    // Initialize program data.
    let data = data::collect_data(program);
    // Replace it with references to its location in the JIT.
    data::replace_data(program, &data);
    // Store the data in the JIT.
    for d in data {
        data::create_data(d, &mut jit)?;
    }

    // Transforms the program so that anonymous functions are lifted
    // to the top of the program and replaced with their anyonmous
    // names. There is some cool manuvering here that happens to make
    // sure that the bodies of the collected functions are updated.
    let mut functions = procedures::collect_functions(program);
    // Annotation needs to happen before replacement so that we can
    // traverse the body of nested functions for free variables that
    // outer functions need to caputre.
    for mut f in &mut functions {
        procedures::annotate_free_variables(&mut f);
    }

    // Replace functions with their anonymous names.
    procedures::replace_functions(program, &mut functions);

    // Annotate escaped variables in closures
    escape::annotate_escaped_variables(&mut functions, program)?;

    // Build a map from anonymous names to values
    let fnmap = procedures::build_fn_map(functions);

    for (_, f) in &fnmap {
        emit_procedure(&mut jit, &f.name, &f.params, &f.body, &fnmap)?;
    }

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

    let env = HashMap::new();

    let mut ctx = Context::new(builder, &mut jit.module, word, env, fnmap, Vec::new());

    let vals = program
        .iter()
        .map(|e| emit_expr(e, &mut ctx))
        .collect::<Result<Vec<_>, _>>()?;

    // Emit a return instruction to return the result.
    ctx.builder.ins().return_(&[*vals
        .last()
        .ok_or("expected at least one expression".to_string())?]);

    // Clean up
    ctx.builder.seal_all_blocks();
    ctx.builder.finalize();

    let id = jit
        .module
        .declare_function("lust_entry", Linkage::Export, &jit.context.func.signature)
        .map_err(|e| e.to_string())?;

    jit.module
        .define_function(id, &mut jit.context, &mut codegen::binemit::NullTrapSink {})
        .map_err(|e| e.to_string())?;

    // If you want to dump the generated IR this is the way:
    // println!("{}", jit.context.func.display(jit.module.isa()));

    jit.module.clear_context(&mut jit.context);

    jit.module.finalize_definitions();

    let code_ptr = jit.module.get_finalized_function(id);

    let code_fn = unsafe { std::mem::transmute::<_, fn() -> i64>(code_ptr) };

    Ok(Expr::from_immediate(code_fn()))
}

/// Compiles an expression and returns the result converted back into
/// an expression.
#[cfg(test)]
pub fn roundtrip_expr(expr: Expr) -> Result<Expr, String> {
    let mut jit = JIT::default();

    let word = jit.module.target_config().pointer_type();

    // Signature for the function that we're compiling. This function
    // takes no arguments and returns an integer.
    jit.context.func.signature.returns.push(AbiParam::new(word));

    // This manuver is actually so unfourtinate. We basically need to
    // do it because we need to make ctx get dropped so that there
    // aren't outstanding mutable references to the jit's context once
    // we want to finalize things inside of it.
    //
    // Note that for some insane reason we are allowed to not do this
    // in roundtrip expressions...
    let signature = {
        // Create a new builder for building our function and create a new
        // block to compile into.
        let mut builder = FunctionBuilder::new(&mut jit.context.func, &mut jit.builder_context);
        let entry_block = builder.create_block();

        // Give the paramaters that we set up earlier to this entry block.
        builder.append_block_params_for_function_params(entry_block);
        // Start putting code in the new block.
        builder.switch_to_block(entry_block);

        let env = HashMap::new();

        let mut ctx = Context::new(
            builder,
            &mut jit.module,
            word,
            env,
            HashMap::new(),
            Vec::new(),
        );

        // Compile the value and get the "output" of the instrution stored
        // in `val`.
        let val = emit_expr(&expr, &mut ctx)?;

        // Emit a return instruction to return the result.
        ctx.builder.ins().return_(&[val]);

        // Clean up
        ctx.builder.seal_all_blocks();
        ctx.builder.finalize();

        ctx.builder.func.signature.clone()
    };

    let id = jit
        .module
        .declare_function("lust_entry", Linkage::Export, &signature)
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

#[cfg(test)]
pub fn roundtrip_exprs(exprs: &[Expr]) -> Result<Expr, String> {
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

    let env = HashMap::new();

    let mut ctx = Context::new(
        builder,
        &mut jit.module,
        word,
        env,
        HashMap::new(),
        Vec::new(),
    );

    let vals = exprs
        .iter()
        .map(|e| emit_expr(e, &mut ctx))
        .collect::<Result<Vec<_>, _>>()?;

    // Emit a return instruction to return the result.
    ctx.builder.ins().return_(&[*vals
        .last()
        .ok_or("expected at least one expression".to_string())?]);

    // Clean up
    ctx.builder.seal_all_blocks();
    ctx.builder.finalize();

    let id = jit
        .module
        .declare_function("lust_entry", Linkage::Export, &jit.context.func.signature)
        .map_err(|e| e.to_string())?;

    jit.module
        .define_function(id, &mut jit.context, &mut codegen::binemit::NullTrapSink {})
        .map_err(|e| e.to_string())?;

    // If you want to dump the generated IR this is the way:
    // println!("{}", jit.context.func.display(jit.module.isa()));

    jit.module.clear_context(&mut jit.context);

    jit.module.finalize_definitions();

    let code_ptr = jit.module.get_finalized_function(id);

    let code_fn = unsafe { std::mem::transmute::<_, fn() -> i64>(code_ptr) };

    Ok(Expr::from_immediate(code_fn()))
}
