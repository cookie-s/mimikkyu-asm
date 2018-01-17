#[derive(Debug)]
pub struct GReg(i32);
#[derive(Debug)]
pub struct FReg(i32);
#[derive(Debug)]
pub struct CReg(i32);

#[derive(Debug)]
pub enum Const {
    Addr(String),
    AddrH(String),
    AddrL(String),
    Int(i32),
}

#[derive(Debug)]
pub enum Asm {
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

pub fn parse_asm(assembly: &String) -> Vec<Asm> {
    fn parse_line(line: &str) -> Result<Asm, ()> {
        fn parse_const(cst: &str) -> Result<Const, ()> {
            if let Ok(i) = cst.parse::<i32>() {
                return Ok(Const::Int(i));
            };

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
            ".long" => Asm::LONG(expect_line_result!(parse_const(expect_line_option!(
                parm.next()
            )))),
            "lis" => Asm::LIS(
                expect_line_result!(parse_greg(expect_line_option!(parm.next()))),
                expect_line_result!(parse_const(expect_line_option!(parm.next()))),
            ),
            "li" => Asm::LI(
                expect_line_result!(parse_greg(expect_line_option!(parm.next()))),
                expect_line_result!(parse_const(expect_line_option!(parm.next()))),
            ),
            "neg" => {
                let rt = expect_line_result!(parse_greg(expect_line_option!(parm.next())));
                let ra = expect_line_result!(parse_greg(expect_line_option!(parm.next())));
                Asm::NEG(rt, ra)
            }
            "add" | "subf" | "or" | "and" | "xor" | "slw" | "srw" => {
                let rt = expect_line_result!(parse_greg(expect_line_option!(parm.next())));
                let ra = expect_line_result!(parse_greg(expect_line_option!(parm.next())));
                let rb = expect_line_result!(parse_greg(expect_line_option!(parm.next())));
                match opc {
                    "add" => Asm::ADD(rt, ra, rb),
                    "subf" => Asm::SUBF(rt, ra, rb),
                    "or" => Asm::OR(rt, ra, rb),
                    "and" => Asm::AND(rt, ra, rb),
                    "xor" => Asm::XOR(rt, ra, rb),
                    "slw" => Asm::SLW(rt, ra, rb),
                    "srw" => Asm::SRW(rt, ra, rb),
                    _ => unreachable!(),
                }
            }
            "fadd" | "fsub" | "fmul" | "fdiv" => {
                let frt = expect_line_result!(parse_freg(expect_line_option!(parm.next())));
                let fra = expect_line_result!(parse_freg(expect_line_option!(parm.next())));
                let frb = expect_line_result!(parse_freg(expect_line_option!(parm.next())));
                match opc {
                    "fadd" => Asm::FADD(frt, fra, frb),
                    "fsub" => Asm::FSUB(frt, fra, frb),
                    "fmul" => Asm::FMUL(frt, fra, frb),
                    "fdiv" => Asm::FDIV(frt, fra, frb),
                    _ => unreachable!(),
                }
            }
            "fmr" | "fneg" => {
                let frt = expect_line_result!(parse_freg(expect_line_option!(parm.next())));
                let fra = expect_line_result!(parse_freg(expect_line_option!(parm.next())));
                match opc {
                    "fmr" => Asm::FMR(frt, fra),
                    "fneg" => Asm::FNEG(frt, fra),
                    _ => unreachable!(),
                }
            }
            "addi" | "addis" | "ori" | "andi." | "andis." => {
                // TODO: andi. andis.
                let rt = expect_line_result!(parse_greg(expect_line_option!(parm.next())));
                let ra = expect_line_result!(parse_greg(expect_line_option!(parm.next())));
                let ct = expect_line_result!(parse_const(expect_line_option!(parm.next())));
                match opc {
                    "addi" => Asm::ADDI(rt, ra, ct),
                    "addis" => Asm::ADDIS(rt, ra, ct),
                    "ori" => Asm::ORI(rt, ra, ct),
                    "andi." => Asm::ANDI(rt, ra, ct),
                    "andis." => Asm::ANDIS(rt, ra, ct),
                    _ => unreachable!(),
                }
            }
            "fcmpu" => Asm::FCMP(
                expect_line_result!(parse_creg(expect_line_option!(parm.next()))),
                expect_line_result!(parse_freg(expect_line_option!(parm.next()))),
                expect_line_result!(parse_freg(expect_line_option!(parm.next()))),
            ),
            "cmp" => Asm::CMP(
                expect_line_result!(parse_creg(expect_line_option!(parm.next()))),
                expect_line_result!(parse_greg(expect_line_option!(parm.next()))),
                expect_line_result!(parse_greg(expect_line_option!(parm.next()))),
            ),
            "cmpwi" => Asm::CMPWI(
                expect_line_result!(parse_creg(expect_line_option!(parm.next()))),
                expect_line_result!(parse_greg(expect_line_option!(parm.next()))),
                expect_line_result!(parse_const(expect_line_option!(parm.next()))),
            ),
            "lwz" | "stw" => {
                let rt = expect_line_result!(parse_greg(expect_line_option!(parm.next())));
                let (off, base) = expect_line_result!(parse_addr(expect_line_option!(parm.next())));

                match opc {
                    "lwz" => Asm::LWZ(rt, off, base),
                    "stw" => Asm::STW(rt, off, base),
                    _ => unreachable!(),
                }
            }
            "lfs" | "stfs" => {
                let rt = expect_line_result!(parse_freg(expect_line_option!(parm.next())));
                let (off, base) = expect_line_result!(parse_addr(expect_line_option!(parm.next())));

                match opc {
                    "lfs" => Asm::LFS(rt, off, base),
                    "stfs" => Asm::STFS(rt, off, base),
                    _ => unreachable!(),
                }
            }
            "b" | "bl" => {
                let ad = expect_line_result!(parse_const(expect_line_option!(parm.next())));
                match opc {
                    "b" => Asm::B(ad),
                    "bl" => Asm::BL(ad),
                    _ => unreachable!(),
                }
            }
            "blr" => Asm::BLR(),
            "bctr" => Asm::BCTR(),
            "bctrl" => Asm::BCTR(),
            "beq" | "bne" | "ble" | "blt" | "bge" | "bgt" => {
                let cr = expect_line_result!(parse_creg(expect_line_option!(parm.next())));
                let ad = expect_line_result!(parse_const(expect_line_option!(parm.next())));
                match opc {
                    "beq" => Asm::BEQ(cr, ad),
                    "bne" => Asm::BNE(cr, ad),
                    //"ble" => Asm::BLE(cr, ad),
                    "blt" => Asm::BLT(cr, ad),
                    //"bge" => Asm::BGE(cr, ad),
                    "bgt" => Asm::BGT(cr, ad),
                    _ => unreachable!(),
                }
            }
            "mflr" => Asm::MFLR(expect_line_result!(parse_greg(expect_line_option!(
                parm.next()
            )))),
            "mtlr" => Asm::MTLR(expect_line_result!(parse_greg(expect_line_option!(
                parm.next()
            )))),
            "mtctr" => Asm::MTLR(expect_line_result!(parse_greg(expect_line_option!(
                parm.next()
            )))),
            "sc" => Asm::SC(),
            _ => {
                if let Ok(name) = parse_label(opc) {
                    Asm::LABEL(name)
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
