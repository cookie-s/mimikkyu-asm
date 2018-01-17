pub mod asm;
pub mod op;

type AsmOpList = Vec<asm::AsmOp>;
type OpList = Vec<op::Op>;

#[derive(Debug, Clone, Copy)]
pub struct GReg(u32);
#[derive(Debug, Clone, Copy)]
pub struct FReg(u32);
#[derive(Debug, Clone, Copy)]
pub struct CReg(u32);

#[derive(Debug, Clone, Copy)]
pub enum SPReg {
    CTR,
    LK,
}
fn id_to_greg(x: u32) -> GReg {
    match x {
        0...31 => GReg(x),
        _ => panic!(),
    }
}
fn greg_to_id(x: GReg) -> u32 {
    let GReg(x) = x;
    x
}
fn id_to_freg(x: u32) -> FReg {
    match x {
        0...31 => FReg(x),
        _ => panic!(),
    }
}
fn freg_to_id(x: FReg) -> u32 {
    let FReg(x) = x;
    x
}
fn id_to_creg(x: u32) -> CReg {
    match x {
        0...7 => CReg(x),
        _ => panic!(),
    }
}
fn creg_to_id(x: CReg) -> u32 {
    let CReg(x) = x;
    x
}
fn id_to_spreg(x: u32) -> SPReg {
    match x {
        0b0100000000 => SPReg::LK,
        0b0100100000 => SPReg::CTR,
        _ => panic!("no such reg!"),
    }
}
fn spreg_to_id(r: SPReg) -> u32 {
    match r {
        SPReg::LK => 0b0100000000,
        SPReg::CTR => 0b0100100000,
    }
}
