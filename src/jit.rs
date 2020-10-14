// Following along with https://github.com/bytecodealliance/simplejit-demo

use std::collections::HashMap;

use cranelift::prelude::*;
use cranelift_module::{DataContext, FuncId, Linkage, Module};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};

use crate::parser::{Expr, ExprVal};

/// The basic JIT class.
pub struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data context, which is to data objects what `ctx` is to functions.
    data_ctx: DataContext,

    /// The module, with the simplejit backend, which manages the JIT'd
    /// functions.
    module: Module<SimpleJITBackend>,

    /// Maps between function names and their builtins. Likely needs
    /// to be replaced by a symbol table affair later.
    functions: HashMap<String, FuncId>,

    /// The number of anon functions this JIT has compiled in the
    /// REPL.
    anon_count: usize,
}

/// A value in the JIT. This is either a function that has been
/// defined or a variable.
#[derive(Debug)]
pub enum JITVal {
    Fn(FuncId),
    Var(Variable),
}

// Should use some sort of JITBuilder struct here to create the JIT
// like llvm so that install_builtins can return error in a cleanish
// way.
impl JIT {
    /// Create a new `JIT` instance.
    pub fn new() -> Self {
        let builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());
        let module = Module::new(builder);
        let mut jit = Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
            functions: HashMap::new(),
            anon_count: 0,
        };
        Self::install_builtins(&mut jit);
        jit
    }

    fn install_builtins(&mut self) {
        let mut sig_add = self.module.make_signature();
        sig_add.params.push(AbiParam::new(types::F32));
        sig_add.params.push(AbiParam::new(types::F32));
        sig_add.returns.push(AbiParam::new(types::F32));
        let fn_add = self
            .module
            .declare_function("+", Linkage::Local, &sig_add)
            .unwrap();
        self.ctx.func.signature = sig_add;

        {
            let mut fnbuilder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
            let block = fnbuilder.create_block();

            // Switch to the new block so that we can start compiling to it.
            fnbuilder.switch_to_block(block);
            // Add the current context's params to the block.
            fnbuilder.append_block_params_for_function_params(block);

            let left = fnbuilder.block_params(block)[0];
            let right = fnbuilder.block_params(block)[1];
            let add = fnbuilder.ins().fadd(left, right);
            fnbuilder.ins().return_(&[add]);

            // Sealing a block indicates to the builder that we have
            // added all the ways to reach this block to the JIT and
            // it can be sealed.
            fnbuilder.seal_all_blocks();
            // Indicate that we are done. This resets the
            // functionbuilder for another function.
            fnbuilder.finalize();
        }

        self.module
            .define_function(
                fn_add,
                &mut self.ctx,
                &mut codegen::binemit::NullTrapSink {},
            )
            .unwrap();
        // Clear the context now that we're done with this function.
        self.module.clear_context(&mut self.ctx);
        // Wrap up outstanding functions.
        self.module.finalize_definitions();

        self.functions.insert("+".to_string(), fn_add);
    }

    pub fn compile_call(&mut self, expr: Expr) -> Result<*const u8, String> {
        let expr_list: Vec<Expr> = match expr.val {
            ExprVal::List(v) => v,
            _ => return Err("provided expression is not a list".to_string()),
        };
        let name = match expr_list[0].val {
            ExprVal::Id(ref s) => s,
            _ => return Err("expected a function name".to_string()),
        };

        self.ctx
            .func
            .signature
            .returns
            .push(AbiParam::new(types::F32));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = builder.create_block();

        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);

        let predicate_fn = self.functions.get(name).unwrap();
        let local_fn = self
            .module
            .declare_func_in_func(*predicate_fn, &mut builder.func);

        let left = match expr_list[1].val {
            ExprVal::Number(f) => builder.ins().f32const(f),
            _ => return Err("bad call argument".to_string()),
        };

        let right = match expr_list[2].val {
            ExprVal::Number(f) => builder.ins().f32const(f),
            _ => return Err("bad call argument".to_string()),
        };
        let call = builder.ins().call(local_fn, &[left, right]);
        let res = builder.inst_results(call)[0].clone();
        builder.ins().return_(&[res]);

        builder.seal_all_blocks();
        builder.finalize();

        let id = self
            .module
            .declare_function(
                &format!("__anon_fn:{}", self.anon_count),
                Linkage::Export,
                &self.ctx.func.signature,
            )
            .map_err(|e| e.to_string())?;

        self.anon_count += 1;

        self.module
            .define_function(id, &mut self.ctx, &mut codegen::binemit::NullTrapSink {})
            .map_err(|e| e.to_string())?;

        // Now that compilation is finished, we can clear out the context state.
        self.module.clear_context(&mut self.ctx);

        // Finalize the functions which we just defined, which resolves any
        // outstanding relocations (patching in addresses, now that they're
        // available).
        self.module.finalize_definitions();

        self.functions.insert(name.clone(), id);

        // We can now retrieve a pointer to the machine code.
        let code = self.module.get_finalized_function(id);

        Ok(code)
    }

    // (defun foo (x) (+ x 1))
    // Must take only floats as arguments and return a single float.
    pub fn compile_defun(&mut self, expr: Expr) -> Result<*const u8, String> {
        let expr_list: Vec<Expr> = match expr.val {
            ExprVal::List(v) => v,
            _ => return Err("provided expression is not a list".to_string()),
        };
        let name = match expr_list[1].val {
            ExprVal::Id(ref s) => s,
            _ => return Err("expected a function name".to_string()),
        };
        let params: &Vec<Expr> = match expr_list[2].val {
            ExprVal::List(ref v) => v,
            _ => return Err("expected list of arguments as second value".to_string()),
        };
        for _p in params {
            self.ctx
                .func
                .signature
                .params
                .push(AbiParam::new(types::F32));
        }
        self.ctx
            .func
            .signature
            .returns
            .push(AbiParam::new(types::F32));

        // Create a new block to compile into.
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = builder.create_block();

        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);

        // Declare the variables.
        let mut variables = HashMap::new();
        let mut index = 0;

        for (i, expr) in params.iter().enumerate() {
            let val = builder.block_params(entry_block)[i];
            let var = Variable::new(index);
            let name = match expr.val {
                ExprVal::Id(ref s) => s,
                _ => return Err("params must all be IDs".to_string()),
            };
            variables.insert(name, var);
            builder.declare_var(var, types::F32);
            index += 1;
            builder.def_var(var, val);
        }

        // Compile the body. Lets start by assuming the body is one
        // expression in the form (+ <id/const> <id/const>)
        let body = match expr_list[3].val {
            ExprVal::List(ref v) => v,
            _ => return Err("bad body".to_string()),
        };

        let call = match body[0].val {
            ExprVal::Id(ref s) => s,
            _ => return Err("list predicate is not an id".to_string()),
        };

        let predicate_fn = self.functions.get(call).unwrap();
        let local_fn = self
            .module
            .declare_func_in_func(*predicate_fn, &mut builder.func);

        let left = match body[1].val {
            ExprVal::Number(f) => builder.ins().f32const(f),
            ExprVal::Id(ref s) => {
                let variable = variables.get(s).expect("variable not defined");
                builder.use_var(*variable)
            }
            _ => return Err("only id and number supported".to_string()),
        };

        let right = match body[2].val {
            ExprVal::Number(f) => builder.ins().f32const(f),
            ExprVal::Id(ref s) => {
                let variable = variables.get(s).expect("variable not defined");
                builder.use_var(*variable)
            }
            _ => return Err("only id and number supported".to_string()),
        };

        let call = builder.ins().call(local_fn, &[left, right]);
        let res = builder.inst_results(call)[0].clone();
        builder.ins().return_(&[res]);

        builder.seal_all_blocks();
        // Tell the builder we're done with this function.
        builder.finalize();

        let id = self
            .module
            .declare_function(&name, Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        self.module
            .define_function(id, &mut self.ctx, &mut codegen::binemit::NullTrapSink {})
            .map_err(|e| e.to_string())?;

        // Now that compilation is finished, we can clear out the context state.
        self.module.clear_context(&mut self.ctx);

        // Finalize the functions which we just defined, which resolves any
        // outstanding relocations (patching in addresses, now that they're
        // available).
        self.module.finalize_definitions();

        self.functions.insert(name.clone(), id);

        // We can now retrieve a pointer to the machine code.
        let code = self.module.get_finalized_function(id);

        Ok(code)
    }

    pub fn compile(&mut self) -> Result<*const u8, String> {
        let float = types::F32;

        self.ctx.func.signature.params.push(AbiParam::new(float));
        self.ctx.func.signature.params.push(AbiParam::new(float));

        self.ctx.func.signature.returns.push(AbiParam::new(float));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = builder.create_block();

        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);

        let fn_add = self.functions.get("+").unwrap();
        let local_add = self.module.declare_func_in_func(*fn_add, &mut builder.func);

        let left = builder.block_params(entry_block)[0];
        let right = builder.block_params(entry_block)[1];
        let call = builder.ins().call(local_add, &[left, right]);

        let val = builder.inst_results(call)[0].clone();
        builder.ins().return_(&[val]);

        builder.seal_all_blocks();
        // Tell the builder we're done with this function.
        builder.finalize();

        let id = self
            .module
            .declare_function("foo", Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        self.module
            .define_function(id, &mut self.ctx, &mut codegen::binemit::NullTrapSink {})
            .map_err(|e| e.to_string())?;

        // Now that compilation is finished, we can clear out the context state.
        self.module.clear_context(&mut self.ctx);

        // Finalize the functions which we just defined, which resolves any
        // outstanding relocations (patching in addresses, now that they're
        // available).
        self.module.finalize_definitions();

        // We can now retrieve a pointer to the machine code.
        let code = self.module.get_finalized_function(id);

        Ok(code)
    }
}
