use super::*;

#[derive(Debug)]
pub enum Condition {
    EQ,
    NE,
    LT,
    // LE,
    GT,
    // GE,
}

#[derive(Debug)]
pub enum Op {
    ADDI(GReg, GReg, i32),
    ADDIS(GReg, GReg, i32),
    ADD(GReg, GReg, GReg),
    SUBF(GReg, GReg, GReg),
    NEG(GReg, GReg),

    AND(GReg, GReg, GReg),
    ANDI(GReg, GReg, i32),
    ANDIS(GReg, GReg, i32),
    OR(GReg, GReg, GReg),
    ORI(GReg, GReg, i32),
    XOR(GReg, GReg, GReg),
    SLW(GReg, GReg, GReg),
    SRW(GReg, GReg, GReg),

    MFSPR(SPReg, GReg),
    MTSPR(SPReg, GReg),

    LWZ(GReg, i32, GReg),
    STW(GReg, i32, GReg),

    FADD(FReg, FReg, FReg),
    FSUB(FReg, FReg, FReg),
    FMUL(FReg, FReg, FReg),
    FDIV(FReg, FReg, FReg),
    FNEG(FReg, FReg),
    FMR(FReg, FReg),
    FCMP(CReg, FReg, FReg),

    LFS(FReg, i32, GReg),
    STFS(FReg, i32, GReg),
    CMP(CReg, GReg, GReg),
    CMPWI(CReg, GReg, i32),

    B(i32, bool),      // LK
    BSPR(SPReg, bool), // LK
    BC(CReg, i32, Condition),
    SC(),

    LONG(i32),
}

pub fn convert_to_machinecode(op: Op) -> u32 {
    fn get_opcode(op: &Op) -> u32 {
        match *op {
            Op::CMPWI(_, _, _) => 11,
            Op::ADDI(_, _, _) => 14,
            Op::ADDIS(_, _, _) => 15,
            Op::ORI(_, _, _) => 24,
            Op::ANDI(_, _, _) => 28,
            Op::ANDIS(_, _, _) => 29,
            Op::BC(_, _, _) => 16,
            Op::B(_, _) => 18,
            Op::BSPR(_, _) => 19,
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
    fn to_bin(dat: &Vec<(u32, u32)>) -> u32 {
        // FIXME kuso
        {
            let mut sum_sz = 0;
            for &(_, sz) in dat {
                sum_sz += sz;
            }
            assert_eq!(sum_sz, 32);
        }
        let mut dat = dat.clone();
        if let Some(last) = dat.last_mut() {
            let (bits, _) = *last;
            *last = (bits, 0);
        }

        let mut res = 0;
        for (bits, size) in dat {
            res += bits;
            res <<= size;
        }
        res
    }

    match op {
        Op::CMPWI(_, _, _) => 11,
        Op::ADDI(_, _, _) => 14,
        Op::ADDIS(_, _, _) => 15,
        Op::ORI(_, _, _) => 24,
        Op::ANDI(_, _, _) => 28,
        Op::ANDIS(_, _, _) => 29,
        Op::BC(_, _, _) => 16,
        Op::B(_, _) => 18,
        Op::BSPR(_, _) => 19,
        Op::LWZ(_, _, _) => 32,
        Op::STW(_, _, _) => 36,
        Op::LFS(_, _, _) => 48,
        Op::STFS(_, _, _) => 52,
        Op::SC() => 17,
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
            (0, 1),
            (get_xocode(&op), 9),
            (1, 1),
        ]),
        Op::NEG(rt, ra) => 0,
        Op::MFSPR(rt, ra) | Op::MTSPR(rt, ra) => 0,
        Op::CMP(cr, ra, rb) => 0,
        Op::FCMP(cr, fra, frb) => 0,
        Op::FNEG(frt, fra) | Op::FMR(frt, fra) => 0,
        Op::FADD(frt, fra, frb)
        | Op::FSUB(frt, fra, frb)
        | Op::FMUL(frt, fra, frb)
        | Op::FDIV(frt, fra, frb) => 0,
        Op::LONG(_) => 0, // TODO hoge
    }
}
