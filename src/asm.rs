extern crate std;
use std::collections::HashMap;

use super::{AsmOpList, CReg, FReg, GReg, OpList};
use super::op::Op;

#[derive(Debug)]
pub enum Const {
    Addr(String),
    AddrH(String),
    AddrL(String),
    Int(i32),
}

#[derive(Debug)]
pub enum AsmOp {
    LIS(GReg, Const),
    LI(GReg, Const),
    ADDI(GReg, GReg, Const),
    ADDIS(GReg, GReg, Const),
    ADD(GReg, GReg, GReg),
    SUBF(GReg, GReg, GReg),
    NEG(GReg, GReg),

    AND(GReg, GReg, GReg),
    ANDI(GReg, GReg, Const),
    ANDIS(GReg, GReg, Const),
    OR(GReg, GReg, GReg),
    ORI(GReg, GReg, Const),
    XOR(GReg, GReg, GReg),
    SLW(GReg, GReg, GReg),
    SRW(GReg, GReg, GReg),

    MFLR(GReg),
    MTLR(GReg),
    MTCTR(GReg),

    LWZ(GReg, Const, GReg),
    STW(GReg, Const, GReg),

    FADD(FReg, FReg, FReg),
    FSUB(FReg, FReg, FReg),
    FMUL(FReg, FReg, FReg),
    FDIV(FReg, FReg, FReg),
    FNEG(FReg, FReg),
    FMR(FReg, FReg),
    FCMP(CReg, FReg, FReg),

    LFS(FReg, Const, GReg),
    STFS(FReg, Const, GReg),
    CMP(CReg, GReg, GReg),
    CMPWI(CReg, GReg, Const),

    B(Const),
    BL(Const),
    BLR(),
    BCTR(),
    BCTRL(),
    BEQ(CReg, Const),
    BNE(CReg, Const),
    BLT(CReg, Const),
    //BLE(CReg, Const),
    BGT(CReg, Const),
    //BGE(CReg, Const),
    SC(),

    LABEL(String),
    LONG(Const),
}

pub fn parse_asm(assembly: &String) -> AsmOpList {
    fn parse_line(line: &str) -> Result<AsmOp, ()> {
        fn parse_const(cst: &str) -> Result<Const, ()> {
            if let Ok(i) = cst.parse::<i32>() {
                return Ok(Const::Int(i));
            };

            // FIXME kuso-es
            if cst.starts_with("(") && cst.ends_with(")@h") {
                let name = cst.trim_left_matches("(").trim_right_matches(")@h");
                Ok(Const::AddrH(String::from(name)))
            } else if cst.starts_with("(") && cst.ends_with(")@l") {
                let name = cst.trim_left_matches("(").trim_right_matches(")@l");
                Ok(Const::AddrL(String::from(name)))
            } else {
                Ok(Const::Addr(String::from(cst)))
            }
        }
        fn parse_greg(reg: &str) -> Result<GReg, ()> {
            if !reg.starts_with("%r") {
                return Err(());
            }
            Ok(match reg.trim_left_matches("%r").parse().or(Err(()))? {
                r @ 0...31 => GReg(r),
                _ => return Err(()),
            })
        }
        fn parse_freg(reg: &str) -> Result<FReg, ()> {
            if !reg.starts_with("%f") {
                return Err(());
            }
            Ok(match reg.trim_left_matches("%f").parse().or(Err(()))? {
                r @ 0...31 => FReg(r),
                _ => return Err(()),
            })
        }
        fn parse_creg(reg: &str) -> Result<CReg, ()> {
            if !reg.starts_with("%cr") {
                return Err(());
            }
            Ok(match reg.trim_left_matches("%cr").parse().or(Err(()))? {
                r @ 0...7 => CReg(r),
                _ => return Err(()),
            })
        }
        fn parse_label(name: &str) -> Result<String, ()> {
            if !name.ends_with(":") {
                return Err(());
            }
            Ok(String::from(name.trim_right_matches(":")))
        }
        fn parse_addr(addr: &str) -> Result<(Const, GReg), ()> {
            // FIXME kuso
            let (off, base) = addr.split_at(addr.rfind('(').ok_or(())?);
            let off = parse_const(off)?;
            let base = parse_greg(base.trim_left_matches('(').trim_right_matches(')'))?;
            Ok((off, base))
        }

        let (op, _) = line.split_at(line.find('#').unwrap_or(line.len()));
        let op = op.trim();
        let (opc, rest) = op.split_at(std::cmp::min(
            op.find(' ').unwrap_or(op.len()),
            op.find('\t').unwrap_or(op.len()),
        ));
        let opc = opc.trim();
        let mut parm = rest.split(',')
            .map(|x| x.trim())
            .collect::<Vec<&str>>()
            .into_iter();

        macro_rules! expect_line_result {
            ($x:expr) => ($x.unwrap_or_else(|x| {eprintln!("{:?}: {}", x, line); panic!()}))
        }
        macro_rules! expect_line_option {
            ($x:expr) => ($x.unwrap_or_else(|| {eprintln!("{}", line); panic!()}))
        }

        Ok(match opc {
            "" => return Err(()),
            ".globl" => return Err(()),
            ".text" => return Err(()),
            ".align" => return Err(()),
            ".data" => return Err(()),
            ".long" => AsmOp::LONG(expect_line_result!(parse_const(expect_line_option!(
                parm.next()
            )))),
            "lis" => AsmOp::LIS(
                expect_line_result!(parse_greg(expect_line_option!(parm.next()))),
                expect_line_result!(parse_const(expect_line_option!(parm.next()))),
            ),
            "li" => AsmOp::LI(
                expect_line_result!(parse_greg(expect_line_option!(parm.next()))),
                expect_line_result!(parse_const(expect_line_option!(parm.next()))),
            ),
            "neg" => {
                let rt = expect_line_result!(parse_greg(expect_line_option!(parm.next())));
                let ra = expect_line_result!(parse_greg(expect_line_option!(parm.next())));
                AsmOp::NEG(rt, ra)
            }
            "add" | "subf" | "or" | "and" | "xor" | "slw" | "srw" => {
                let rt = expect_line_result!(parse_greg(expect_line_option!(parm.next())));
                let ra = expect_line_result!(parse_greg(expect_line_option!(parm.next())));
                let rb = expect_line_result!(parse_greg(expect_line_option!(parm.next())));
                match opc {
                    "add" => AsmOp::ADD(rt, ra, rb),
                    "subf" => AsmOp::SUBF(rt, ra, rb),
                    "or" => AsmOp::OR(rt, ra, rb),
                    "and" => AsmOp::AND(rt, ra, rb),
                    "xor" => AsmOp::XOR(rt, ra, rb),
                    "slw" => AsmOp::SLW(rt, ra, rb),
                    "srw" => AsmOp::SRW(rt, ra, rb),
                    _ => unreachable!(),
                }
            }
            "fadd" | "fsub" | "fmul" | "fdiv" => {
                let frt = expect_line_result!(parse_freg(expect_line_option!(parm.next())));
                let fra = expect_line_result!(parse_freg(expect_line_option!(parm.next())));
                let frb = expect_line_result!(parse_freg(expect_line_option!(parm.next())));
                match opc {
                    "fadd" => AsmOp::FADD(frt, fra, frb),
                    "fsub" => AsmOp::FSUB(frt, fra, frb),
                    "fmul" => AsmOp::FMUL(frt, fra, frb),
                    "fdiv" => AsmOp::FDIV(frt, fra, frb),
                    _ => unreachable!(),
                }
            }
            "fmr" | "fneg" => {
                let frt = expect_line_result!(parse_freg(expect_line_option!(parm.next())));
                let fra = expect_line_result!(parse_freg(expect_line_option!(parm.next())));
                match opc {
                    "fmr" => AsmOp::FMR(frt, fra),
                    "fneg" => AsmOp::FNEG(frt, fra),
                    _ => unreachable!(),
                }
            }
            "addi" | "addis" | "ori" | "andi." | "andis." => {
                // TODO: delete dot?
                let rt = expect_line_result!(parse_greg(expect_line_option!(parm.next())));
                let ra = expect_line_result!(parse_greg(expect_line_option!(parm.next())));
                let ct = expect_line_result!(parse_const(expect_line_option!(parm.next())));
                match opc {
                    "addi" => AsmOp::ADDI(rt, ra, ct),
                    "addis" => AsmOp::ADDIS(rt, ra, ct),
                    "ori" => AsmOp::ORI(rt, ra, ct),
                    "andi." => AsmOp::ANDI(rt, ra, ct),
                    "andis." => AsmOp::ANDIS(rt, ra, ct),
                    _ => unreachable!(),
                }
            }
            "fcmpu" => AsmOp::FCMP(
                expect_line_result!(parse_creg(expect_line_option!(parm.next()))),
                expect_line_result!(parse_freg(expect_line_option!(parm.next()))),
                expect_line_result!(parse_freg(expect_line_option!(parm.next()))),
            ),
            "cmp" => AsmOp::CMP(
                expect_line_result!(parse_creg(expect_line_option!(parm.next()))),
                expect_line_result!(parse_greg(expect_line_option!(parm.next()))),
                expect_line_result!(parse_greg(expect_line_option!(parm.next()))),
            ),
            "cmpwi" => AsmOp::CMPWI(
                expect_line_result!(parse_creg(expect_line_option!(parm.next()))),
                expect_line_result!(parse_greg(expect_line_option!(parm.next()))),
                expect_line_result!(parse_const(expect_line_option!(parm.next()))),
            ),
            "lwz" | "stw" => {
                let rt = expect_line_result!(parse_greg(expect_line_option!(parm.next())));
                let (off, base) = expect_line_result!(parse_addr(expect_line_option!(parm.next())));

                match opc {
                    "lwz" => AsmOp::LWZ(rt, off, base),
                    "stw" => AsmOp::STW(rt, off, base),
                    _ => unreachable!(),
                }
            }
            "lfs" | "stfs" => {
                let rt = expect_line_result!(parse_freg(expect_line_option!(parm.next())));
                let (off, base) = expect_line_result!(parse_addr(expect_line_option!(parm.next())));

                match opc {
                    "lfs" => AsmOp::LFS(rt, off, base),
                    "stfs" => AsmOp::STFS(rt, off, base),
                    _ => unreachable!(),
                }
            }
            "b" | "bl" => {
                let ad = expect_line_result!(parse_const(expect_line_option!(parm.next())));
                match opc {
                    "b" => AsmOp::B(ad),
                    "bl" => AsmOp::BL(ad),
                    _ => unreachable!(),
                }
            }
            "blr" => AsmOp::BLR(),
            "bctr" => AsmOp::BCTR(),
            "bctrl" => AsmOp::BCTR(),
            "beq" | "bne" | "ble" | "blt" | "bge" | "bgt" => {
                let cr = expect_line_result!(parse_creg(expect_line_option!(parm.next())));
                let ad = expect_line_result!(parse_const(expect_line_option!(parm.next())));
                match opc {
                    "beq" => AsmOp::BEQ(cr, ad),
                    "bne" => AsmOp::BNE(cr, ad),
                    //"ble" => AsmOp::BLE(cr, ad),
                    "blt" => AsmOp::BLT(cr, ad),
                    //"bge" => AsmOp::BGE(cr, ad),
                    "bgt" => AsmOp::BGT(cr, ad),
                    _ => unreachable!(),
                }
            }
            "mflr" => AsmOp::MFLR(expect_line_result!(parse_greg(expect_line_option!(
                parm.next()
            )))),
            "mtlr" => AsmOp::MTLR(expect_line_result!(parse_greg(expect_line_option!(
                parm.next()
            )))),
            "mtctr" => AsmOp::MTLR(expect_line_result!(parse_greg(expect_line_option!(
                parm.next()
            )))),
            "sc" => AsmOp::SC(),
            _ => {
                if let Ok(name) = parse_label(opc) {
                    AsmOp::LABEL(name)
                } else {
                    panic!("{}", opc)
                }
            }
        })
    }

    assembly
        .lines()
        .filter_map(|ln| parse_line(ln).ok())
        .collect()
}

pub fn convert_to_realops(asm: AsmOpList) -> OpList {
    fn collect_labels(asm: &AsmOpList) -> HashMap<String, i32> {
        let mut res = HashMap::new();
        let mut addr = 0;
        for asmop in asm.into_iter() {
            if let &AsmOp::LABEL(ref label) = asmop {
                let label = label.clone();
                assert!(!res.contains_key(&label));
                res.insert(label, addr);
            } else {
                addr += 4;
            }
        }
        res
    }
    fn convert_one(asm: &AsmOp, labels: &HashMap<String, i32>) -> Op {
        fn resolve_const(cst: &Const, labels: &HashMap<String, i32>) -> i32 {
            match cst {
                &Const::Addr(ref label) => *labels.get(label).unwrap(),
                &Const::AddrH(ref label) => (*labels.get(label).unwrap() as u32 >> 16) as i32,
                &Const::AddrL(ref label) => {
                    (*labels.get(label).unwrap() as u32 & ((1 << 16) - 1)) as i32
                }
                &Const::Int(i) => i,
            }
        }

        match asm {
            &AsmOp::LIS(rt, ref dat) => Op::ADDIS(rt, GReg(0), resolve_const(dat, labels)),
            /*
            LI(GReg, Const),
            ADDI(GReg, GReg, Const),
            ADDIS(GReg, GReg, Const),
            ADD(GReg, GReg, GReg),
            SUBF(GReg, GReg, GReg),
            NEG(GReg, GReg),

            AND(GReg, GReg, GReg),
            ANDI(GReg, GReg, Const),
            ANDIS(GReg, GReg, Const),
            OR(GReg, GReg, GReg),
            ORI(GReg, GReg, Const),
            XOR(GReg, GReg, GReg),
            SLW(GReg, GReg, GReg),
            SRW(GReg, GReg, GReg),

            MFLR(GReg),
            MTLR(GReg),
            MTCTR(GReg),

            LWZ(GReg, Const, GReg),
            STW(GReg, Const, GReg),

            FADD(FReg, FReg, FReg),
            FSUB(FReg, FReg, FReg),
            FMUL(FReg, FReg, FReg),
            FDIV(FReg, FReg, FReg),
            FNEG(FReg, FReg),
            FMR(FReg, FReg),
            FCMP(CReg, FReg, FReg),

            LFS(FReg, Const, GReg),
            STFS(FReg, Const, GReg),
            CMP(CReg, GReg, GReg),
            CMPWI(CReg, GReg, Const),

            B(Const),
            BL(Const),
            BLR(),
            BCTR(),
            BCTRL(),
            BEQ(CReg, Const),
            BNE(CReg, Const),
            BLT(CReg, Const),
            //BLE(CReg, Const),
            BGT(CReg, Const),
            //BGE(CReg, Const),
            SC(),

            LABEL(String),
            LONG(Const),
            */
            _ => Op::ADDIS(GReg(0), GReg(0), 0),
        }
    }

    let labels = collect_labels(&asm);
    asm.into_iter()
        .map(|asm| convert_one(&asm, &labels))
        .collect()
}
