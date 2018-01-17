use super::{CReg, FReg, GReg, OpList, SPReg};

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
