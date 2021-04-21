use ::ulang::parser::*;
//use ::ulang::codegen::*;
use structopt::StructOpt;
use std::fs::File;
use std::io::Result;
use std::io::prelude::*;

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
    #[structopt(short, long, help="execute the compiled code")]
    execute: bool,

    filename: String
}

//TODO: repl
fn repl(dump_ast: bool) {
    let s = include_str!("../test_file.ulang");
    let mut p = UlangParser::new(s);
    let ast = p.parse();
    if dump_ast {
        println!("{}", ast);
    }
    ast.run();
}

fn run(filename: Option<&str>, dump_ast: bool) -> Result<()> {
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
    if dump_ast {
        println!("{}", ast);
    }
    ast.run();
    Ok(())
}

fn main() {
    let opt = Options::from_args();
    match opt.commands {
        Commands::Compile(cmd) => run(Some(&cmd.filename), cmd.dump_ast).unwrap(),
        Commands::Repl => repl(false),
    }
}
