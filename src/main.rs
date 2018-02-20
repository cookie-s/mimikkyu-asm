extern crate mimikkyu_asm;

use std::io::{Read, Write};
use std::mem::transmute;

pub fn main() {
    let mut s: String = String::new();
    std::io::stdin().read_to_string(&mut s).unwrap();

    let asm = mimikkyu_asm::asm::parse_asm(&s);
    //eprintln!("{:?}", &asm);

    let ops = mimikkyu_asm::asm::convert_to_realops(&asm);
    for (i, op) in (&ops).into_iter().enumerate() {
        eprintln!("{:0>8X} {:?}", i << 2, &op);
    }

    let mcs = ops.into_iter()
        .map(|op| mimikkyu_asm::op::convert_to_machinecode(&op))
        .collect::<Vec<u32>>();

    let mut stdout = std::io::stdout();
    for mc in &mcs {
        let bytes: [u8; 4] = unsafe { transmute(mc.to_le()) };
        stdout.write(&bytes).unwrap();
    }
}
