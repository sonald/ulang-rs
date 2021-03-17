use lalrpop;

pub fn main() {
    eprintln!("preprocess");
    lalrpop::process_root().unwrap();
}
