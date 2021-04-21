use crate::parser::*;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::values::*;
use inkwell::types::*;
//use inkwell::execution_engine::JitFunction;
use inkwell::OptimizationLevel;

const MAIN_FN: &str = "main";

#[derive(Debug)]
pub struct Backend<'ctx> {
    pub name: String,
    pub ctx: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub entry_func: Option<FunctionValue<'ctx>>,
    pub current_func: Option<FunctionValue<'ctx>>,
    // current expression's value
    pub current_value: Option<BasicValueEnum<'ctx>>,
}

trait Codegen {
    fn codegen(&self, ctx: &mut Backend);
}

impl Codegen for ast::CompilationUnit {
    fn codegen(&self, ctx: &mut Backend) {
        self.0.iter().for_each(|t| t.codegen(ctx));
    }
}

impl Codegen for ast::Term {
    fn codegen(&self, ctx: &mut Backend) {
        eprintln!("Term::codegen");
        match self {
            ast::Term::Class(ref cls) => cls.codegen(ctx),
            ast::Term::Func(ref func) => func.codegen(ctx),
        }
    }
}

impl Codegen for ast::ClassDefinition {
    fn codegen(&self, ctx: &mut Backend) {
        eprintln!("Class::codegen");
    }
}

impl Codegen for ast::Expression {
    fn codegen(&self, ctx: &mut Backend) {
        eprintln!("Expression::codegen");
        match self {
            ast::Expression::DecimalExpr(n) => {
                let val = ctx.ctx.i32_type().const_int(*n as u64, false);
                ctx.current_value.replace(val.as_basic_value_enum());
            },
            _ => {
            }
        }
    }
}

impl Codegen for ast::Statement {
    fn codegen(&self, ctx: &mut Backend) {
        eprintln!("Statement::codegen");
        match self {
            ast::Statement::Return(maybe_expr) => {
                match maybe_expr {
                    Some(expr) => {
                        expr.codegen(ctx);
                        let retval = ctx.current_value.take().unwrap();
                        ctx.builder.build_return(Some(&retval));
                    },
                    _ => {
                        ctx.builder.build_return(None);
                    }
                }

            },
            _ => {
            }
        }
    }
}

impl Codegen for ast::FuncDefinition {
    fn codegen(&self, ctx: &mut Backend) {
        eprintln!("Func::codegen");

        // build function type
        let fn_ty = if let Some(ref ty_name) = self.rettype {
            let ty: Box<dyn BasicType> = if self.name == MAIN_FN || ty_name == "int32" || ty_name == "int" {
                Box::new(ctx.ctx.i32_type())
            } else if ty_name == "string" {
                Box::new(ctx.ctx.i32_type())
            } else if ty_name == "bool" {
                Box::new(ctx.ctx.bool_type())
            } else {
                Box::new(ctx.ctx.i32_type())
            };
            ty.fn_type(&[], false)
        } else {
            ctx.ctx.void_type().fn_type(&[], false)
        };

        let func = ctx.module.add_function(&self.name, fn_ty, None);
        assert!(ctx.current_func.is_none());
        ctx.current_func.replace(func);

        let bb = ctx.ctx.append_basic_block(func, "entry");
        ctx.builder.position_at_end(bb);

        self.statements.0.iter().for_each(|st| st.codegen(ctx));

        if self.name == MAIN_FN {
            assert!(ctx.entry_func.is_none());
            ctx.entry_func = Some(func);
        }

        ctx.current_func.take();
    }
}

impl ast::CompilationUnit {
    pub fn run(&self) {
        let ctx = Context::create();
        let module = ctx.create_module("ulang_mod");
        let builder = ctx.create_builder();

        let mut backend = Backend {
            name: "default".to_owned(),
            ctx: &ctx,
            module,
            builder,
            entry_func: None,
            current_value: None,
            current_func: None
        };

        self.codegen(&mut backend);
        let main = backend.entry_func.as_ref().unwrap().get_name().to_string_lossy();

        let jit = backend.module.create_jit_execution_engine(OptimizationLevel::Default).unwrap();
        unsafe {
            let func = jit.get_function::<unsafe extern "C" fn() -> i32>(&main).unwrap();
            let ret = func.call();
            println!("ret = {}", ret);
        }
    }
}
