extern crate mimikkyu_asm;

use std::io::Read;

pub fn main() {
    let mut s: String = String::new();
    std::io::stdin().read_to_string(&mut s).unwrap();
    println!("{:?}", mimikkyu_asm::parse_asm(&s));
}
