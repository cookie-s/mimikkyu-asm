use super::*;

#[derive(Debug, Copy, Clone)]
pub enum Condition {
    AL,
    EQ,
    NE,
    LT,
    // LE,
    GT,
    // GE,
}

#[derive(Debug)]
pub enum Op {
    ADDI(GReg, GReg, u32),
    ADDIS(GReg, GReg, u32),
    ADD(GReg, GReg, GReg),
    SUBF(GReg, GReg, GReg),
    NEG(GReg, GReg),

    AND(GReg, GReg, GReg),
    ANDI(GReg, GReg, u32),
    ANDIS(GReg, GReg, u32),
    OR(GReg, GReg, GReg),
    ORI(GReg, GReg, u32),
    XOR(GReg, GReg, GReg),
    SLW(GReg, GReg, GReg),
    SRW(GReg, GReg, GReg),

    MFSPR(SPReg, GReg),
    MTSPR(SPReg, GReg),

    LWZ(GReg, u32, GReg),
    STW(GReg, u32, GReg),

    FADD(FReg, FReg, FReg),
    FSUB(FReg, FReg, FReg),
    FMUL(FReg, FReg, FReg),
    FDIV(FReg, FReg, FReg),
    FNEG(FReg, FReg),
    FMR(FReg, FReg),
    FCMP(CReg, FReg, FReg),

    LFS(FReg, u32, GReg),
    STFS(FReg, u32, GReg),
    CMP(CReg, GReg, GReg),
    CMPWI(CReg, GReg, u32),

    B(u32, bool),                   // LK
    BSPR(SPReg, bool),              // LK
    BC(CReg, u32, Condition, bool), // abs
    SC(u32, GReg, GReg),

    LONG(u32),
}

pub fn convert_to_machinecode(op: &Op) -> u32 {
    fn get_opcode(op: &Op) -> u32 {
        match *op {
            Op::ADDI(_, _, _) => 0b000000,
            Op::ADDIS(_, _, _) => 0b000001,
            Op::ANDI(_, _, _) => 0b000010,
            Op::ANDIS(_, _, _) => 0b000011,
            Op::ORI(_, _, _) => 0b000110,
            Op::CMPWI(_, _, _) => 0b000111,
            Op::LWZ(_, _, _) => 0b001001,
            Op::STW(_, _, _) => 0b001010,
            Op::LFS(_, _, _) => 0b001100,
            Op::STFS(_, _, _) => 0b001101,
            Op::B(_, _) => 0b100001,
            Op::BSPR(_, _) => 0b100010,
            Op::BC(_, _, _, _) => 0b100100,
            Op::CMP(_, _, _)
            | Op::ADD(_, _, _)
            | Op::SUBF(_, _, _)
            | Op::NEG(_, _)
            | Op::AND(_, _, _)
            | Op::OR(_, _, _)
            | Op::XOR(_, _, _)
            | Op::SLW(_, _, _)
            | Op::SRW(_, _, _) => 0b111110,
            Op::FCMP(_, _, _)
            | Op::FADD(_, _, _)
            | Op::FSUB(_, _, _)
            | Op::FMUL(_, _, _)
            | Op::FDIV(_, _, _)
            | Op::FNEG(_, _)
            | Op::FMR(_, _) => 0b111111,
            Op::MFSPR(_, _) => 0b110000,
            Op::MTSPR(_, _) => 0b110001,
            Op::SC(_, _, _) => 0b110111,
            Op::LONG(_) => unreachable!(),
        }
    }
    fn get_xocode(op: &Op) -> u32 {
        match *op {
            Op::CMP(_, _, _) => 0b000000,
            Op::ADD(_, _, _) => 0b000001,
            Op::SUBF(_, _, _) => 0b000010,
            Op::NEG(_, _) => 0b001000,
            Op::AND(_, _, _) => 0b010000,
            Op::OR(_, _, _) => 0b010001,
            Op::XOR(_, _, _) => 0b010010,
            Op::SLW(_, _, _) => 0b010100,
            Op::SRW(_, _, _) => 0b010101,
            Op::FCMP(_, _, _) => 0b000000,
            Op::FADD(_, _, _) => 0b000001,
            Op::FSUB(_, _, _) => 0b000010,
            Op::FMUL(_, _, _) => 0b000100,
            Op::FDIV(_, _, _) => 0b000101,
            Op::FNEG(_, _) => 0b001000,
            Op::FMR(_, _) => 0b001001,
            _ => unreachable!(),
        }
    }
    fn get_bicode(cr: CReg, cond: Condition) -> u32 {
        4 * creg_to_id(cr) + match cond {
            Condition::AL => 0,
            Condition::EQ | Condition::NE => 2,
            Condition::LT => 0,
            Condition::GT => 1,
        }
    }
    fn get_bocode(cond: Condition) -> u32 {
        match cond {
            Condition::AL => 0b11111,
            Condition::EQ => 0b01100,
            Condition::NE => 0b00100,
            Condition::LT => 0b01100,
            Condition::GT => 0b01100,
        }
    }
    fn to_bin(dat: &Vec<(u32, u32)>) -> u32 {
        let (_, sz): (Vec<_>, Vec<u32>) = dat.iter().cloned().unzip();
        assert_eq!(sz.iter().sum::<u32>(), 32);
        let mut res = 0u64;
        for &(bits, size) in dat {
            res <<= size;
            res += (bits & ((1 << size) - 1)) as u64;
        }
        res as u32
    }

    match *op {
        Op::CMPWI(cr, ra, imm) => to_bin(&vec![
            (get_opcode(&op), 6),
            (creg_to_id(cr), 3),
            (0, 1),
            (0, 1),
            (greg_to_id(ra), 5),
            (imm as u32 as u16 as u32, 16),
        ]),
        Op::ADDI(rt, ra, imm)
        | Op::ADDIS(rt, ra, imm)
        | Op::ORI(rt, ra, imm)
        | Op::ANDI(rt, ra, imm)
        | Op::ANDIS(rt, ra, imm) => to_bin(&vec![
            (get_opcode(&op), 6),
            (greg_to_id(rt), 5),
            (greg_to_id(ra), 5),
            (imm as u32 as u16 as u32, 16),
        ]),
        Op::LWZ(rt, off, ra) | Op::STW(rt, off, ra) => to_bin(&vec![
            (get_opcode(&op), 6),
            (greg_to_id(rt), 5),
            (greg_to_id(ra), 5),
            (off as u16 as u32, 16),
        ]),
        Op::LFS(frt, off, ra) | Op::STFS(frt, off, ra) => to_bin(&vec![
            (get_opcode(&op), 6),
            (freg_to_id(frt), 5),
            (greg_to_id(ra), 5),
            (off as u16 as u32, 16),
        ]),
        Op::B(addr, lk) => to_bin(&vec![
            (get_opcode(&op), 6),
            ((addr >> 2) as u32, 24),
            (1, 1), // TODO
            (lk as u32, 1),
        ]),
        Op::BC(cr, addr, cond, abs) => to_bin(&vec![
            (get_opcode(&op), 6),
            (get_bocode(cond), 5),
            (get_bicode(cr, cond), 5),
            ((addr >> 2) as u32, 14),
            (abs as u32, 1),
            (0, 1),
        ]),
        Op::BSPR(sp, lk) => to_bin(&vec![
            (get_opcode(&op), 6),
            (0, 5),
            (spreg_to_id(sp), 5),
            (0, 15),
            (lk as u32, 1),
        ]),
        Op::SC(code, rt, ra) => to_bin(&vec![
            (get_opcode(&op), 6),
            (greg_to_id(rt), 5),
            (greg_to_id(ra), 5),
            (0, 5),
            (code, 11),
        ]),
        Op::ADD(rt, ra, rb)
        | Op::SUBF(rt, ra, rb)
        | Op::AND(rt, ra, rb)
        | Op::OR(rt, ra, rb)
        | Op::XOR(rt, ra, rb)
        | Op::SLW(rt, ra, rb)
        | Op::SRW(rt, ra, rb) => to_bin(&vec![
            (get_opcode(&op), 6),
            (greg_to_id(rt), 5),
            (greg_to_id(ra), 5),
            (greg_to_id(rb), 5),
            (get_xocode(&op), 10),
            (1, 1),
        ]),
        Op::CMP(cr, ra, rb) => to_bin(&vec![
            (get_opcode(&op), 6),
            (creg_to_id(cr), 3),
            (0, 1),
            (0, 1),
            (greg_to_id(ra), 5),
            (greg_to_id(rb), 5),
            (get_xocode(&op), 10),
            (1, 1),
        ]),
        Op::NEG(rt, ra) => to_bin(&vec![
            (get_opcode(&op), 6),
            (greg_to_id(rt), 5),
            (greg_to_id(ra), 5),
            (0, 5),
            (get_xocode(&op), 10),
            (1, 1),
        ]),
        Op::MFSPR(sp, rt) => to_bin(&vec![
            (get_opcode(&op), 6),
            (greg_to_id(rt), 5),
            (spreg_to_id(sp), 5),
            (0, 16),
        ]),
        Op::MTSPR(sp, rs) => to_bin(&vec![
            (get_opcode(&op), 6),
            (spreg_to_id(sp), 5),
            (greg_to_id(rs), 5),
            (0, 16),
        ]),
        Op::FCMP(cr, fra, frb) => to_bin(&vec![
            (get_opcode(&op), 6),
            (creg_to_id(cr), 3),
            (0, 2),
            (freg_to_id(fra), 5),
            (freg_to_id(frb), 5),
            (get_xocode(&op), 10),
            (1, 1),
        ]),
        Op::FNEG(frt, fra) | Op::FMR(frt, fra) => to_bin(&vec![
            (get_opcode(&op), 6),
            (freg_to_id(frt), 5),
            (freg_to_id(fra), 5),
            (0, 5),
            (get_xocode(&op), 10),
            (1, 1),
        ]),
        Op::FADD(frt, fra, frb)
        | Op::FSUB(frt, fra, frb)
        | Op::FMUL(frt, fra, frb)
        | Op::FDIV(frt, fra, frb) => to_bin(&vec![
            (get_opcode(&op), 6),
            (freg_to_id(frt), 5),
            (freg_to_id(fra), 5),
            (freg_to_id(frb), 5),
            (0, 5),
            (get_xocode(&op), 5),
            (1, 1),
        ]),
        Op::LONG(x) => x as u32,
    }
}
