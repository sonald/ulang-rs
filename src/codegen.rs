use crate::parser::*;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::basic_block::BasicBlock;
use inkwell::values::*;
use inkwell::types::*;

use std::collections::HashMap;

pub const MAIN_FN: &str = "main";

#[derive(Debug, Clone)]
pub struct SymbolValue<'ctx> {
    value: BasicValueEnum<'ctx>,
}

#[derive(Debug)]
pub struct Env<'ctx> {
    syms: HashMap<String, SymbolValue<'ctx>>,
}

#[derive(Debug)]
pub struct Backend<'a, 'ctx> {
    pub name: String,
    pub ctx: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: &'a Builder<'ctx>,

    pub current_func: Option<FunctionValue<'ctx>>,
    pub current_block: Option<BasicBlock<'ctx>>,

    // map<func, env> right now, we only have functions
    pub envs: HashMap<String, Env<'ctx>>,
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
    fn codegen<'a, 'b, 'ctx>(&self,_cg: &'b mut Backend<'a, 'ctx>) -> CodegenResult<'ctx> {
        Ok(None)
    }
}

impl Codegen for ast::Expression {
    fn codegen<'a, 'b, 'ctx>(&self, cg: &'b mut Backend<'a, 'ctx>) -> CodegenResult<'ctx> {
        use ast::Expression as E;
        use ast::Operator as Op;
        use inkwell::IntPredicate as Pred;

        eprintln!("cg:{:?}", self);
        match self {
            E::DecimalExpr(n) => {
                let val = cg.ctx.i32_type().const_int(*n as u64, false);
                Ok(Some(val.as_basic_value_enum()))
            },
            E::BinaryExpr{op, lhs, rhs} => {
                let lval = lhs.codegen(cg).unwrap().unwrap();
                let rval = rhs.codegen(cg).unwrap().unwrap();
                let v = match op {
                    &Op::Minus => {
                        cg.builder.build_int_sub(
                            lval.into_int_value(), rval.into_int_value(), "subtmp")
                    },
                    &Op::Plus => {
                        cg.builder.build_int_add(
                            lval.into_int_value(), rval.into_int_value(), "addtmp")
                    },
                    &Op::Multiply => {
                        cg.builder.build_int_mul(
                            lval.into_int_value(), rval.into_int_value(), "multmp")
                    },
                    &Op::Divide => {
                        cg.builder.build_int_signed_div(
                            lval.into_int_value(), rval.into_int_value(), "divtmp")
                    },
                    &Op::Greater | &Op::GreaterEqual | &Op::Less 
                        | &Op::LessEqual | &Op::Equal | &Op::NotEqual => {
                        let pred = match op {
                            &Op::Greater => Pred::SGT,
                            &Op::GreaterEqual => Pred::SGE,
                            &Op::Less =>  Pred::SLT,
                            &Op::LessEqual => Pred::SLE,
                            &Op::Equal => Pred::EQ,
                            &Op::NotEqual => Pred::NE,
                            _ => {
                                unimplemented!("")
                            }
                        };

                        cg.builder.build_int_compare(
                            pred, lval.into_int_value(), rval.into_int_value(), "cmptmp")
                    },
                    _ => {
                        unimplemented!("")
                    }
                };
                Ok(Some(v.as_basic_value_enum()))
            },
            E::CallExpr{callee, args} => {
                match &**callee {
                    E::SymbolExpr(nm) => {
                        let func = cg.module.get_function(&nm).unwrap();
                        let ret = format!("{}_ret", nm);
                        let mut params = Vec::with_capacity(args.0.len());
                        for e in &args.0 {
                            params.push(e.codegen(cg).unwrap().unwrap());
                        }

                        let v = cg.builder.build_call(func, params.as_slice(), &ret)
                            .try_as_basic_value().left().unwrap();
                        Ok(Some(v))
                    },
                    _ => Ok(None)
                }
            },
            E::SymbolExpr(sym) => {
                let sym = cg.get_symbol(sym).unwrap();
                Ok(Some(sym.value))
            },
            _ => {
                unimplemented!("expression")
            }
        }
    }
}

impl Codegen for ast::Statement {
    fn codegen<'a, 'b, 'ctx>(&self, cg: &'b mut Backend<'a, 'ctx>) -> CodegenResult<'ctx> {
        use ast::Statement as S;
        match self {
            S::Return(maybe_expr) => {
                match maybe_expr {
                    Some(expr) => {
                        let retval = expr.codegen(cg).unwrap();
                        let retval = retval.unwrap();
                        cg.builder.build_return(Some(&retval));
                    },
                    _ => {
                        cg.builder.build_return(None);
                    }
                }
            },
            S::Let{val, init} => {
                let init = init.as_ref().expect("need an initializer");
                let init_val = init.codegen(cg).unwrap().unwrap();

                let func = cg.current_func.as_ref().unwrap();
                let nm = func.get_name().to_string_lossy();
                let env = cg.envs.get_mut(nm.as_ref()).unwrap();
                env.syms.entry(val.clone()).or_insert(SymbolValue {value: init_val});

                let ptr = cg.builder.build_alloca(init_val.get_type(), &val);
                cg.builder.build_store(ptr, init_val);
            },
            _ => {
                unimplemented!("stmt")
            }
        }
        Ok(None)
    }
}

fn name2type<'ctx>(ctx: &'ctx Context, ty_name: &str) -> Box<dyn BasicType<'ctx> + 'ctx> {
    if ty_name == "int32" || ty_name == "int" {
        Box::new(ctx.i32_type())
    } else if ty_name == "string" {
        Box::new(ctx.i32_type())
    } else if ty_name == "bool" {
        Box::new(ctx.bool_type())
    } else {
        Box::new(ctx.i32_type())
    }
}

impl Codegen for ast::FuncDefinition {
    fn codegen<'a, 'b, 'ctx>(&self, cg: &'b mut Backend<'a, 'ctx>) -> CodegenResult<'ctx> {
        eprintln!("cg::FuncDef");
        let ctx = cg.ctx;

        let arg_types = self.args.iter()
            .map(|s| name2type(&cg.ctx, &s.ty))
            .map(|t| t.as_basic_type_enum())
            .collect::<Vec<BasicTypeEnum>>();

        // build function type
        let fn_ty = if let Some(ref ty_name) = self.rettype {
            let ty: Box<dyn BasicType> = if self.name == MAIN_FN {
                Box::new(ctx.i32_type())
            } else {
                name2type(&cg.ctx, ty_name)
            };
            ty.fn_type(arg_types.as_slice(), false)
        } else {
            ctx.void_type().fn_type(arg_types.as_slice(), false)
        };

        let mut env = Env {
            syms: HashMap::new()
        };
        let func = cg.module.add_function(&self.name, fn_ty, None);

        for (i, param) in func.get_param_iter().enumerate() {
            param.set_name(&self.args[i].name);
            env.syms.insert(self.args[i].name.clone(), SymbolValue {value: param});
        }
        cg.envs.insert(self.name.clone(), env);

        cg.current_func.replace(func.clone());

        let bb = ctx.append_basic_block(func, "entry");
        cg.builder.position_at_end(bb);

        self.statements.0.iter().for_each(|st| {st.codegen(cg).expect("func def");});

        cg.current_func.take();
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

            current_func: None,
            current_block: None,

            envs: HashMap::new()
        }
    }

    pub fn get_symbol(&self, sym: &str) -> Option<&SymbolValue<'ctx>> {
        let func = self.current_func.as_ref().unwrap();
        let nm = func.get_name().to_string_lossy();
        let env = self.envs.get(nm.as_ref()).unwrap();
        env.syms.get(sym)
    }
}
