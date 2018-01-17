extern crate mimikkyu_asm;

use std::io::Read;

pub fn main() {
    let mut s: String = String::new();
    std::io::stdin().read_to_string(&mut s).unwrap();

    let asm = mimikkyu_asm::asm::parse_asm(&s);
    println!("{:?}", asm);
}
