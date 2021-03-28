use ::ulang::parser::*;

fn compile() {
    let s = include_str!("../test_file.ulang");
    let mut p = UlangParser::new(s);
    //p.lexing_check();
    p.parse();
}

fn main() {
    compile();
}
