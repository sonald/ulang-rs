use lalrpop;

pub fn main() {
    println!("cargo:rustc-link-lib=dylib=LLVM-8");
    println!("cargo:rustc-link-search=native=/usr/lib64");

    eprintln!("preprocess");
    lalrpop::process_root().unwrap();
}
