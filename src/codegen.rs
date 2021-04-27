use crate::parser::*;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::values::*;
use inkwell::types::*;

pub const MAIN_FN: &str = "main";

#[derive(Debug)]
pub struct Backend<'a, 'ctx> {
    pub name: String,
    pub ctx: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: &'a Builder<'ctx>,
}

type CodegenResult<'ctx> = std::result::Result<Option<BasicValueEnum<'ctx>>, ()>;

pub trait Codegen {
    fn codegen<'a, 'b, 'ctx>(&self, cg: &'b mut Backend<'a, 'ctx>) -> CodegenResult<'ctx>;
}


impl Codegen for ast::CompilationUnit {
    fn codegen<'a, 'b, 'ctx>(&self, cg: &'b mut Backend<'a, 'ctx>) -> CodegenResult<'ctx> {
        self.0.iter().for_each(|t| { t.codegen(cg).expect("compile failed"); });
        Ok(None)
    }
}

impl Codegen for ast::Term {
    fn codegen<'a, 'b, 'ctx>(&self, cg: &'b mut Backend<'a, 'ctx>) -> CodegenResult<'ctx> {
        match self {
            ast::Term::Class(ref cls) => cls.codegen(cg),
            ast::Term::Func(ref func) => func.codegen(cg),
        }
    }
}

impl Codegen for ast::ClassDefinition {
    fn codegen<'a, 'b, 'ctx>(&self, cg: &'b mut Backend<'a, 'ctx>) -> CodegenResult<'ctx> {
        Ok(None)
    }
}

impl Codegen for ast::Expression {
    fn codegen<'a, 'b, 'ctx>(&self, cg: &'b mut Backend<'a, 'ctx>) -> CodegenResult<'ctx> {
        match self {
            ast::Expression::DecimalExpr(n) => {
                let val = cg.ctx.i32_type().const_int(*n as u64, false);
                Ok(Some(val.as_basic_value_enum()))
            },
            _ => {
                Ok(None)
            }
        }
    }
}

impl Codegen for ast::Statement {
    fn codegen<'a, 'b, 'ctx>(&self, cg: &'b mut Backend<'a, 'ctx>) -> CodegenResult<'ctx> {
        match self {
            ast::Statement::Return(maybe_expr) => {
                match maybe_expr {
                    Some(expr) => {
                        let retval = expr.codegen(cg);
                        let retval = retval.unwrap().unwrap();
                        cg.builder.build_return(Some(&retval));
                    },
                    _ => {
                        cg.builder.build_return(None);
                    }
                }

            },
            _ => {
            }
        }
        Ok(None)
    }
}

impl Codegen for ast::FuncDefinition {
    fn codegen<'a, 'b, 'ctx>(&self, cg: &'b mut Backend<'a, 'ctx>) -> CodegenResult<'ctx> {
        let ctx = cg.ctx;

        // build function type
        let fn_ty = if let Some(ref ty_name) = self.rettype {
            let ty: Box<dyn BasicType> = if self.name == MAIN_FN || ty_name == "int32" || ty_name == "int" {
                Box::new(ctx.i32_type())
            } else if ty_name == "string" {
                Box::new(ctx.i32_type())
            } else if ty_name == "bool" {
                Box::new(ctx.bool_type())
            } else {
                Box::new(ctx.i32_type())
            };
            ty.fn_type(&[], false)
        } else {
            ctx.void_type().fn_type(&[], false)
        };

        let func = cg.module.add_function(&self.name, fn_ty, None);

        let bb = ctx.append_basic_block(func, "entry");
        cg.builder.position_at_end(bb);

        self.statements.0.iter().for_each(|st| {st.codegen(cg).expect("func def");});

        Ok(None)
    }
}

impl<'a, 'ctx> Backend<'a, 'ctx> {
    pub fn new(ctx: &'ctx Context, module: &'a Module<'ctx>, builder: &'a Builder<'ctx>) -> Self {
        Backend {
            name: "default".to_owned(),
            ctx,
            module,
            builder,
        }
    }
}
