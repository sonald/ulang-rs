use ::ulang::parser::*;
use ::ulang::codegen::*;
use structopt::StructOpt;
use std::fs::File;
use std::io::{Error, ErrorKind, Result};
use std::io::prelude::*;
use inkwell::context::Context;
use inkwell::OptimizationLevel;

#[derive(Debug, StructOpt)]
struct Options {
    #[structopt(short, long)]
    verbose: bool,

    #[structopt(subcommand)]
    commands: Commands
}

#[derive(Debug, StructOpt)]
enum Commands {
    Compile(CompileCommand), 
    Repl
}

#[derive(Debug, StructOpt)]
struct CompileCommand {
    #[structopt(short, long)]
    dump_ast: bool,
    #[structopt(short="-D", long)]
    debug: bool,
    #[structopt(short, long, help="execute the compiled code")]
    execute: bool,

    filename: String
}

//TODO: repl
fn repl(dump_ast: bool) {
    let s = include_str!("../test_file.ulang");
    let mut p = UlangParser::new(s);
    let ast = p.parse();
    let ctx = Context::create();
    let module = ctx.create_module("ulang_mod");
    let builder = ctx.create_builder();
    let mut cg = Backend::new(&ctx, &module, &builder);
    ast.codegen(&mut cg).expect("compile failed");
    if dump_ast {
        module.print_to_stderr();
    }

    if let Some(main_fn) = module.get_function(MAIN_FN) {
        let main = main_fn.get_name().to_string_lossy();
        let jit = module.create_jit_execution_engine(OptimizationLevel::Default).unwrap();
        unsafe {
            let func = jit.get_function::<unsafe extern "C" fn() -> i32>(&main).unwrap();
            let ret = func.call();
            println!("ret = {}", ret);
        }
    }
}

fn run(filename: Option<&str>, cmd: &CompileCommand) -> Result<()> {
    let s = match filename {
        Some(filename) => {
            let mut f = File::open(filename)?;
            let mut s = String::new();
            f.read_to_string(&mut s)?;
            s
        },
        _ => include_str!("../test_file.ulang").to_owned()
    };
    let mut p = UlangParser::new(&s);
    let ast = p.parse();
    if cmd.debug {
        println!("{}", ast);
    }

    let ctx = Context::create();
    let module = ctx.create_module("ulang_mod");
    let builder = ctx.create_builder();
    let mut cg = Backend::new(&ctx, &module, &builder);

    ast.codegen(&mut cg).expect("compile failed");
    if cmd.dump_ast {
        module.print_to_stderr();
    }
    if let Err(err) = module.verify() {
        eprintln!("{}", err.to_string_lossy());

        return Err(Error::from(ErrorKind::InvalidInput));
    }

    if let Some(main_fn) = module.get_function(MAIN_FN) {
        let main = main_fn.get_name().to_string_lossy();
        let jit = module.create_jit_execution_engine(OptimizationLevel::Default).unwrap();
        unsafe {
            let func = jit.get_function::<unsafe extern "C" fn() -> i32>(&main).unwrap();
            let ret = func.call();
            println!("ret = {}", ret);
        }
    } else {
        eprintln!("no entry function (main) found");
    }
    Ok(())
}

fn main() {
    let opt = Options::from_args();
    match opt.commands {
        Commands::Compile(ref cmd) => run(Some(&cmd.filename), cmd).expect("error"),
        Commands::Repl => repl(false),
    }
}
