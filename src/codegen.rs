use crate::parser::*;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::passes::PassManager;
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
    // symbol table stack
    syms: Vec<HashMap<String, SymbolValue<'ctx>>>,
}

impl<'ctx> Env<'ctx> {
    pub fn get(&self, nm: &str) -> Option<&SymbolValue<'ctx>> {
        self.syms.iter().rev().find_map(|t| t.get(nm))
    }

    pub fn insert(&mut self, nm: String, value: BasicValueEnum<'ctx>) {
        if self.syms.is_empty() {
            self.syms.push(HashMap::new());
        }
        if self.syms.last().unwrap().contains_key(&nm) {
            panic!("symbol({}) has defined", &nm)
        }

        self.syms.last_mut().map(|tbl| tbl.entry(nm).or_insert(SymbolValue {value}));
    }
}

#[derive(Debug)]
pub struct Backend<'a, 'ctx> {
    pub name: String,
    pub ctx: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,

    pub current_func: Option<FunctionValue<'ctx>>,
    pub break_insr: Option<InstructionValue<'ctx>>,

    // map<func, env> right now, we only have functions
    pub envs: HashMap<String, Env<'ctx>>,
}

type CodegenResult<'ctx> = std::result::Result<Option<BasicValueEnum<'ctx>>, ()>;

pub trait Codegen {
    fn codegen<'a, 'b, 'ctx>(&self, cg: &'b mut Backend<'a, 'ctx>) -> CodegenResult<'ctx>;
}


impl Codegen for ast::CompilationUnit {
    // two passes
    // 1. collect all classes and functions declaration
    // 2. real codegen
    fn codegen<'a, 'b, 'ctx>(&self, cg: &'b mut Backend<'a, 'ctx>) -> CodegenResult<'ctx> {
        for t in &self.0 {
            match t {
                ast::Term::Class(cls) => {
                },
                ast::Term::Func(func) => {
                    func.collect(cg);
                }
            }
        }

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

        //eprintln!("cg:{:?}", self);
        match self {
            E::DecimalExpr(n) => {
                let val = cg.ctx.i32_type().const_int(*n as u64, false);
                Ok(Some(val.as_basic_value_enum()))
            },
            E::UnaryExpr{op, expr} => {
                let mut val = expr.codegen(cg).unwrap().unwrap();
                if val.is_pointer_value() {
                    val = cg.builder.build_load(val.into_pointer_value(), "p");
                }

                let fun = match op {
                    &Op::Minus => Builder::build_int_neg,
                    &Op::Not => Builder::build_not,
                    _ => unreachable!("invalid unary operator"),
                };
                Ok(Some(fun(cg.builder, val.into_int_value(), "").as_basic_value_enum()))
            },
            E::BinaryExpr{op, lhs, rhs} => {
                let mut lval = lhs.codegen(cg).unwrap().unwrap();
                let mut rval = rhs.codegen(cg).unwrap().unwrap();
                if lval.is_pointer_value() {
                    lval = cg.builder.build_load(lval.into_pointer_value(), "ltmp");
                }
                if rval.is_pointer_value() {
                    rval = cg.builder.build_load(rval.into_pointer_value(), "rtmp");
                }
                let v = match op {
                    &Op::Minus  | &Op::Plus | &Op::Multiply | &Op::Divide => {
                        let fun = match op {
                            &Op::Minus => Builder::build_int_sub,
                            &Op::Plus => Builder::build_int_add,
                            &Op::Multiply => Builder::build_int_mul,
                            &Op::Divide => Builder::build_int_signed_div,
                            _ => unreachable!(""),
                        };

                        fun(cg.builder, lval.into_int_value(), rval.into_int_value(), "")
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
                            pred, lval.into_int_value(), rval.into_int_value(), "b")
                    },
                    _ => { unimplemented!("") }
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
                let sym = cg.get_symbol(sym).expect(&format!("symbol({}) does not defined.", &sym));
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
        //eprintln!("cg:Stmt {:?}", self);
        use ast::Statement as S;
        match self {
            S::Expr(e) => {
                e.codegen(cg).unwrap();
            },
            S::Break => {
                let func = cg.current_func.unwrap();
                let cur_bb = cg.builder.get_insert_block().expect("invalid insert block");
                assert!(cur_bb.get_terminator().is_none());

                assert!(cg.break_insr.is_none());
                cg.break_insr = Some(cg.builder.build_unconditional_branch(cur_bb));
                eprintln!("---- break {:?}", cur_bb);
            },
            S::Loop(statements) => {
                let func = cg.current_func.unwrap();
                let loop_entry = cg.ctx.append_basic_block(func, "loop_entry");
                let loop_tail = cg.ctx.append_basic_block(func, "loop_tail");

                let cond_bb = cg.builder.get_insert_block().expect("invalid insert block");
                if cond_bb.get_terminator().is_none() {
                    cg.builder.position_at_end(cond_bb);
                    cg.builder.build_unconditional_branch(loop_entry);
                }

                cg.builder.position_at_end(loop_entry);
                statements.0.iter().for_each(|st| {st.codegen(cg).unwrap();});

                let next_bb = cg.builder.get_insert_block().expect("invalid insert block");
                if next_bb.get_terminator().is_none() {
                    cg.builder.build_unconditional_branch(loop_tail);
                }

                cg.builder.position_at_end(loop_tail);
                cg.builder.build_unconditional_branch(loop_entry);

                if let Some(insr) = cg.break_insr.take() {
                    let after = cg.ctx.append_basic_block(func, "loop_after");

                    let target = insr.get_parent().unwrap();
                    insr.erase_from_basic_block();
                    cg.builder.position_at_end(target);
                    cg.builder.build_unconditional_branch(after);

                    cg.builder.position_at_end(after);
                }
            },
            S::Conditional{predicate, positive, negative} => {
                let func = cg.current_func.unwrap();
                let mut cond_bb = cg.builder.get_insert_block().expect("invalid insert block");
                if cond_bb.get_terminator().is_some() {
                    cond_bb = cg.ctx.append_basic_block(func, "ifcond_bb");
                    cg.builder.position_at_end(cond_bb);
                }

                let mut cond = predicate.codegen(cg).unwrap().unwrap();
                if cond.is_pointer_value() {
                    cond = cg.builder.build_load(cond.into_pointer_value(), "p");
                }

                let true_bb = cg.ctx.append_basic_block(func, "true_bb");
                let false_bb = cg.ctx.append_basic_block(func, "false_bb");
                let next_bb = cg.ctx.append_basic_block(func, "next_bb");

                cg.builder.build_conditional_branch(cond.into_int_value(), true_bb, false_bb);

                cg.builder.position_at_end(true_bb);
                positive.0.iter().for_each(|st| {st.codegen(cg).unwrap();});

                let cur_bb = cg.builder.get_insert_block().expect("invalid insert block");
                if cur_bb.get_terminator().is_none() {
                    cg.builder.build_unconditional_branch(next_bb);
                }

                cg.builder.position_at_end(false_bb);
                negative.0.iter().for_each(|st| {st.codegen(cg).unwrap();});
                let cur_bb = cg.builder.get_insert_block().expect("invalid insert block");
                if cur_bb.get_terminator().is_none() {
                    cg.builder.build_unconditional_branch(next_bb);
                }

                cg.builder.position_at_end(next_bb);
            },
            S::Match{expr, arms} => {
                let func = cg.current_func.unwrap();
                let mut cond_bb = cg.builder.get_insert_block().expect("invalid insert block");
                if cond_bb.get_terminator().is_some() {
                    cond_bb = cg.ctx.append_basic_block(func, "match_cond_bb");
                    cg.builder.position_at_end(cond_bb);
                }

                let mut cond = expr.codegen(cg).unwrap().unwrap();
                if cond.is_pointer_value() {
                    cond = cg.builder.build_load(cond.into_pointer_value(), "p");
                }

                let default_bb = cg.ctx.append_basic_block(func, "default_bb");
                let next_bb = cg.ctx.append_basic_block(func, "next_bb");

                //FIXME: only support int now!
                let mut branches = vec![];
                for arm in &arms.0 {
                    if let S::MatchArm{condition, stat} = arm {
                        cg.builder.position_at_end(cond_bb);
                        let ival = condition.codegen(cg).unwrap().unwrap();

                        let arm_bb = cg.ctx.append_basic_block(func, "arm_bb");
                        cg.builder.position_at_end(arm_bb);
                        stat.0.iter().for_each(|st| {st.codegen(cg).unwrap();});
                        if arm_bb.get_terminator().is_none() {
                            cg.builder.build_unconditional_branch(next_bb);
                        }

                        branches.push((ival.into_int_value(), arm_bb));

                    } else {
                        panic!("invalid match arm statement")
                    }
                }

                cg.builder.position_at_end(cond_bb);
                cg.builder.build_switch(cond.into_int_value(), default_bb, branches.as_slice());

                cg.builder.position_at_end(default_bb);
                cg.builder.build_unconditional_branch(next_bb);
                cg.builder.position_at_end(next_bb);
            },
            S::Return(maybe_expr) => {
                let func = cg.current_func.unwrap();
                let mut cond_bb = cg.builder.get_insert_block().expect("invalid insert block");
                if cond_bb.get_terminator().is_some() {
                    eprintln!("{:?}", cond_bb);
                    cond_bb = cg.ctx.append_basic_block(func, "ret_bb");
                    cg.builder.position_at_end(cond_bb);
                }
                match maybe_expr {
                    Some(expr) => {
                        let retval = expr.codegen(cg).unwrap();
                        let mut retval = retval.unwrap();
                        if retval.is_pointer_value() {
                            retval = cg.builder.build_load(retval.into_pointer_value(), "");
                        }
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

                let ptr = cg.builder.build_alloca(init_val.get_type(), &val);
                cg.builder.build_store(ptr, init_val);

                cg.insert_symbol(val.clone(), ptr.as_basic_value_enum());
            },
            S::Assignment{lhs, op, rhs} => {
                use ast::Operator as Op;
                let mut rval = rhs.codegen(cg).unwrap().unwrap();
                if rval.is_pointer_value() {
                    rval = cg.builder.build_load(rval.into_pointer_value(), "p");
                }
                
                let lval = lhs.codegen(cg).unwrap().unwrap();
                let ptr = lval.into_pointer_value();


                let tmp = match op {
                    Op::Assign => rval.into_int_value(),
                    Op::PlusAssign => {
                        let lval_rval = cg.builder.build_load(ptr, "p");
                        cg.builder.build_int_add(lval_rval.into_int_value(), rval.into_int_value(), "tmp")
                    },
                    Op::MinusAssign => {
                        let lval_rval = cg.builder.build_load(ptr, "p");
                        cg.builder
                            .build_int_sub(lval_rval.into_int_value(), rval.into_int_value(), "tmp")
                    },
                    _ => unreachable!(),
                };

                cg.builder.build_store(ptr, tmp);
            },
            _ => {
                unimplemented!("stmt codegen")
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
        self.collect(cg);

        let mut env = Env {
            syms: vec![]
        };

        let func = cg.module.get_function(&self.name)
            .expect(&format!("unknown function({})", self.name));
        for (i, param) in func.get_param_iter().enumerate() {
            param.set_name(&self.args[i].name);
            env.insert(self.args[i].name.clone(), param);
        }
        cg.envs.insert(self.name.clone(), env);

        cg.current_func.replace(func.clone());

        let bb = cg.ctx.append_basic_block(func, "entry");
        cg.builder.position_at_end(bb);

        self.statements.0.iter().for_each(|st| {st.codegen(cg).expect("func def");});

        let f = cg.current_func.take().unwrap();
        cg.fpm.run_on(&f);
        Ok(None)
    }
}

impl ast::FuncDefinition {
    fn collect<'a, 'b, 'ctx>(&self, cg: &'b mut Backend<'a, 'ctx>) -> FunctionValue<'ctx> {
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
            if self.name == MAIN_FN {
                ctx.i32_type().fn_type(arg_types.as_slice(), false)
            } else {
                ctx.void_type().fn_type(arg_types.as_slice(), false)
            }
        };

        cg.module.add_function(&self.name, fn_ty, None)
    }
}

impl<'a, 'ctx> Backend<'a, 'ctx> {
    pub fn new(ctx: &'ctx Context, module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>, fpm: &'a PassManager<FunctionValue<'ctx>>) -> Self {
        Backend {
            name: "default".to_owned(),
            ctx,
            module,
            builder,
            fpm,

            current_func: None,
            break_insr: None,

            envs: HashMap::new()
        }
    }

    pub fn insert_symbol(&mut self, nm: String, value: BasicValueEnum<'ctx>) {
        let func = self.current_func.as_ref().unwrap();
        let func_nm = func.get_name().to_string_lossy();
        let env = self.envs.get_mut(func_nm.as_ref()).unwrap();

        env.insert(nm, value);
    }

    pub fn get_symbol(&self, sym: &str) -> Option<&SymbolValue<'ctx>> {
        let func = self.current_func.as_ref().unwrap();
        let nm = func.get_name().to_string_lossy();
        let env = self.envs.get(nm.as_ref()).unwrap();
        env.get(sym)
    }
}
