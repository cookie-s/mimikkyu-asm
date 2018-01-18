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
    BCTR(bool),                     // LK
    BLR(bool),                      // LK
    BC(CReg, u32, Condition, bool), // abs
    SC(),

    LONG(u32),
}

pub fn convert_to_machinecode(op: &Op) -> u32 {
    fn get_opcode(op: &Op) -> u32 {
        match *op {
            Op::CMPWI(_, _, _) => 11,
            Op::ADDI(_, _, _) => 14,
            Op::ADDIS(_, _, _) => 15,
            Op::ORI(_, _, _) => 24,
            Op::ANDI(_, _, _) => 28,
            Op::ANDIS(_, _, _) => 29,
            Op::BC(_, _, _, _) => 16,
            Op::B(_, _) => 18,
            Op::BLR(_) | Op::BCTR(_) => 19,
            Op::LWZ(_, _, _) => 32,
            Op::STW(_, _, _) => 36,
            Op::LFS(_, _, _) => 48,
            Op::STFS(_, _, _) => 52,
            Op::SC() => 17,
            Op::ADD(_, _, _)
            | Op::SUBF(_, _, _)
            | Op::NEG(_, _)
            | Op::AND(_, _, _)
            | Op::OR(_, _, _)
            | Op::XOR(_, _, _)
            | Op::SLW(_, _, _)
            | Op::SRW(_, _, _)
            | Op::CMP(_, _, _)
            | Op::MFSPR(_, _)
            | Op::MTSPR(_, _) => 31,
            Op::FADD(_, _, _)
            | Op::FSUB(_, _, _)
            | Op::FMUL(_, _, _)
            | Op::FDIV(_, _, _)
            | Op::FCMP(_, _, _)
            | Op::FNEG(_, _)
            | Op::FMR(_, _) => 63,
            Op::LONG(_) => 0, // TODO hoge
        }
    }
    fn get_xocode(op: &Op) -> u32 {
        match *op {
            Op::ADD(_, _, _) => 266,
            Op::SUBF(_, _, _) => 40,
            Op::NEG(_, _) => 104,
            Op::AND(_, _, _) => 28,
            Op::OR(_, _, _) => 444,
            Op::XOR(_, _, _) => 316,
            Op::SLW(_, _, _) => 24,
            Op::SRW(_, _, _) => 536,
            Op::CMP(_, _, _) => 0,
            Op::BLR(_) => 16,
            Op::BCTR(_) => 528,
            Op::MFSPR(_, _) => 339,
            Op::MTSPR(_, _) => 467,
            Op::FADD(_, _, _) => 21,
            Op::FSUB(_, _, _) => 20,
            Op::FMUL(_, _, _) => 25,
            Op::FDIV(_, _, _) => 18,
            Op::FCMP(_, _, _) => 0,
            Op::FNEG(_, _) => 40,
            Op::FMR(_, _) => 72,
            _ => 0,
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
        // FIXME kuso
        {
            let mut sum_sz = 0;
            for &(_, sz) in dat {
                sum_sz += sz;
            }
            assert_eq!(sum_sz, 32);
        }
        let mut res = 0u64;
        for &(bits, size) in dat {
            res <<= size;
            res += (bits & ((1 << size) - 1)) as u64;
        }
        res as u32
    }

    match *op {
        Op::CMP(cr, ra, rb) => to_bin(&vec![
            (get_opcode(&op), 6),
            (creg_to_id(cr), 3),
            (0, 1),
            (0, 1),
            (greg_to_id(ra), 5),
            (greg_to_id(rb), 5),
            (get_xocode(&op), 10),
            (0, 1),
        ]),
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
        | Op::ORI(ra, rt, imm)
        | Op::ANDI(ra, rt, imm)
        | Op::ANDIS(ra, rt, imm) => to_bin(&vec![
            (get_opcode(&op), 6),
            (greg_to_id(rt), 5),
            (greg_to_id(ra), 5),
            (imm as u32 as u16 as u32, 16),
        ]),
        Op::BC(cr, addr, cond, abs) => to_bin(&vec![
            (get_opcode(&op), 6),
            (get_bocode(cond), 5),
            (get_bicode(cr, cond), 5),
            ((addr >> 2) as u32, 14),
            (abs as u32, 1),
            (0, 1),
        ]),
        Op::B(addr, lk) => to_bin(&vec![
            (get_opcode(&op), 6),
            ((addr >> 2) as u32, 24),
            (1, 1), // TODO
            (lk as u32, 1),
        ]),

        Op::BLR(lk) => to_bin(&vec![
            (get_opcode(&op), 6),
            (get_bocode(Condition::AL), 5),
            (0, 5),
            (0, 3),
            (0, 2),
            (get_xocode(&op), 10),
            (lk as u32, 1),
        ]),
        Op::BCTR(lk) => to_bin(&vec![
            (get_opcode(&op), 6),
            (get_bocode(Condition::AL), 5),
            (0, 5),
            (0, 3),
            (0, 2),
            (get_xocode(&op), 10),
            (lk as u32, 1),
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
        Op::SC() => to_bin(&vec![(get_opcode(&op), 6), (0, 26)]),
        // swapping...
        Op::ADD(rt, ra, rb)
        | Op::SUBF(rt, ra, rb)
        | Op::AND(ra, rt, rb)
        | Op::OR(ra, rt, rb)
        | Op::XOR(ra, rt, rb)
        | Op::SLW(ra, rt, rb)
        | Op::SRW(ra, rt, rb) => to_bin(&vec![
            (get_opcode(&op), 6),
            (greg_to_id(rt), 5),
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
        Op::MFSPR(rt, ra) | Op::MTSPR(rt, ra) => to_bin(&vec![
            (get_opcode(&op), 6),
            (greg_to_id(ra), 5),
            (spreg_to_id(rt), 10),
            (get_xocode(&op), 10),
            (0, 1),
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
        Op::FNEG(frt, frb) | Op::FMR(frt, frb) => to_bin(&vec![
            (get_opcode(&op), 6),
            (freg_to_id(frt), 5),
            (0, 5),
            (freg_to_id(frb), 5),
            (get_xocode(&op), 10),
            (1, 1),
        ]),
        Op::FADD(frt, fra, frb) | Op::FSUB(frt, fra, frb) | Op::FDIV(frt, fra, frb) => {
            to_bin(&vec![
                (get_opcode(&op), 6),
                (freg_to_id(frt), 5),
                (freg_to_id(fra), 5),
                (freg_to_id(frb), 5),
                (0, 5),
                (get_xocode(&op), 5),
                (1, 1),
            ])
        }
        Op::FMUL(frt, fra, frb) => to_bin(&vec![
            (get_opcode(&op), 6),
            (freg_to_id(frt), 5),
            (freg_to_id(fra), 5),
            (0, 5),
            (freg_to_id(frb), 5),
            (get_xocode(&op), 5),
            (1, 1),
        ]),
        Op::LONG(x) => x as u32,
    }
}
