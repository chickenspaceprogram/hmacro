use std::env;
use std::path::PathBuf;
mod lexer;
mod parser;
pub mod expand;
fn main() {
    let mut args = env::args();
    args.next();
    for arg in args {
        match expand::expand_new_file(PathBuf::from(arg).as_path()) {
            Ok(txt) => print!("{}", txt),
            Err(e) => println!("{:?}", e),
        }
    }
}
