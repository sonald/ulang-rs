mod lexer;
mod parser;

use lexer::*;
use logos::Logos;
use parser::*;

fn compile() {
    let prog = r"
// demo program
class App {
  fn partition(v: int[]) -> (int[], int[]) {
    let i = 0;
    let pivot = i;
    let j = v.length - 1;
    loop {
      if i == j { break }
      if v[i] > v[pivot] {
        v.swap(i, pivot);
        j -= 1;
      } else {
        i += 1;
      }
    }
    
    if v[i] <= v[pivot] {
      v.swap(i, pivot);
    }
    
    return (v[0:i], v[i+1:]);
  }
  
  fn quicksort(v: int[]) {
    match v.length {
      0 => return,
      1 => return,
      _ => {
        let (l, r) = partition(v);
        quicksort(l);
        quicksort(r);
      }
    }
  }
  
  pub fn main(args string[]) {
    let v = int32[]();
    for a in args {
      v.append(a.toInt32());
    }
    
    quicksort(v);
  }
}
    ";
    let s = r"
        // entry of the program
        pub fn main(args string[]) {

        }
        ";

    //let mut p = Parser::new(prog);
    let mut p = Parser::new(s);
    p.lexing_check();
}

fn main() {
    compile();
}
