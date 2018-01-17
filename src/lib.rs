pub mod asm;
pub mod op;

type AsmOpList = Vec<asm::AsmOp>;
type OpList = Vec<op::Op>;

#[derive(Debug, Clone, Copy)]
pub struct GReg(i32);
#[derive(Debug, Clone, Copy)]
pub struct FReg(i32);
#[derive(Debug, Clone, Copy)]
pub struct CReg(i32);

#[derive(Debug)]
pub enum SPReg {
    CTR,
    LR,
}
fn id_to_spreg(x: i32) -> SPReg {
    match x {
        0b0100000000 => SPReg::LR,
        0b0100100000 => SPReg::CTR,
        _ => panic!("no such reg!"),
    }
}
fn spreg_to_id(r: SPReg) -> i32 {
    match r {
        SPReg::LR => 0b0100000000,
        SPReg::CTR => 0b0100100000,
    }
}
