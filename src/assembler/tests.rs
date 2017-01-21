use super::assemble;

macro_rules! assert_assemble_err {
    ( $ asm : expr ) => {
        let mut buf = Vec::<u8>::new();
        let asm = $asm;
        match assemble(asm.as_bytes(), &mut buf) {
            Ok(_) => panic!("Expected error"),
            _ => ()
        }
    };
}

macro_rules! assert_assemble {
    ( $ asm : expr , $ expected : expr ) => {{
        let asm = $asm;
        let mut buf = Vec::<u8>::new();
        match assemble(asm.as_bytes(), &mut buf) {
            Err(msg) => panic!(format!("Failed to assemble '{}': {}", asm, msg)),
            _ => {
                let expected = $expected;
                let buf = &buf[..];
                if expected.len() != buf.len() {
                    panic!(format!("Expected number of bytes written for '{}' to be {} but was {}",
                            asm, expected.len(), buf.len()))
                }

                if expected != &buf[..] {
                    panic!(format!("Expected '{}' to compile to {:?} but was {:?}",
                            asm, expected, buf))
                }
            }
        }
    }}
}

#[test]
fn adc() {
    // Absolute
    assert_assemble!("ADC $4400", &[0x6d, 0x00, 0x44]);

    // AbsoluteX
    assert_assemble!("ADC $4400,X", &[0x7d, 0x00, 0x44]);

    // AbsoluteY
    assert_assemble!("ADC $4400,Y", &[0x79, 0x00, 0x44]);

    // Accumulator
    assert_assemble_err!("ADC A");

    // Indirect
    assert_assemble_err!("ADC ($4400)");

    // IndexedIndirect
    assert_assemble!("ADC ($44,X)", &[0x61, 0x44]);

    // IndirectIndexed
    assert_assemble!("ADC ($44),Y", &[0x71, 0x44]);

    // Immediate
    assert_assemble!("ADC #$44", &[0x69, 0x44]);

    // Implied
    assert_assemble_err!("ADC\n");

    // Relative
    assert_assemble_err!("ADC -44");

    // ZeroPage
    assert_assemble!("ADC $44", &[0x65, 0x44]);

    // ZeroPageX
    assert_assemble!("ADC $44,X", &[0x75, 0x44]);

    // ZeroPageY
    assert_assemble_err!("ADC $44,Y");
}

#[test]
fn and() {
    // Absolute
    assert_assemble!("AND $4400", &[0x2d, 0x00, 0x44]);

    // AbsoluteX
    assert_assemble!("AND $4400,X", &[0x3d, 0x00, 0x44]);

    // AbsoluteY
    assert_assemble!("AND $4400,Y", &[0x39, 0x00, 0x44]);

    // Accumulator
    assert_assemble_err!("AND A");

    // Indirect
    assert_assemble_err!("AND ($4400)");

    // IndexedIndirect
    assert_assemble!("AND ($44,X)", &[0x21, 0x44]);

    // IndirectIndexed
    assert_assemble!("AND ($44),Y", &[0x31, 0x44]);

    // Immediate
    assert_assemble!("AND #$44", &[0x29, 0x44]);

    // Implied
    assert_assemble_err!("AND\n");

    // Relative
    assert_assemble_err!("ADC -44");

    // ZeroPage
    assert_assemble!("AND $44", &[0x25, 0x44]);

    // ZeroPageX
    assert_assemble!("AND $44,X", &[0x35, 0x44]);

    // ZeroPageY
    assert_assemble_err!("AND $44,Y");
}

#[test]
fn asl() {
    // Absolute
    assert_assemble!("ASL $4400", &[0x0e, 0x00, 0x44]);

    // AbsoluteX
    assert_assemble!("ASL $4400,X", &[0x1e, 0x00, 0x44]);

    // AbsoluteY
    assert_assemble_err!("ASL $4400,Y");

    // Indirect
    assert_assemble_err!("ASL ($4400)");

    // Accumulator
    assert_assemble!("ASL A", &[0x0a]);

    // IndexedIndirect
    assert_assemble_err!("ASL ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("ASL ($44),Y");

    // Immediate
    assert_assemble_err!("ASL #$44");

    // Implied
    assert_assemble_err!("ASL\n");

    // Relative
    assert_assemble_err!("ASL -44");

    // ZeroPage
    assert_assemble!("ASL $44", &[0x06, 0x44]);

    // ZeroPageX
    assert_assemble!("ASL $44,X", &[0x16, 0x44]);

    // ZeroPageY
    assert_assemble_err!("ASL $44,Y");
}

#[test]
fn bit() {
    // Absolute
    assert_assemble!("BIT $4400", &[0x2c, 0x00, 0x44]);

    // AbsoluteX
    assert_assemble_err!("BIT $4400,X");

    // AbsoluteY
    assert_assemble_err!("BIT $4400,Y");

    // Accumulator
    assert_assemble_err!("BIT A");

    // Indirect
    assert_assemble_err!("BIT ($4400)");

    // IndexedIndirect
    assert_assemble_err!("BIT ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("BIT ($44),Y");

    // Immediate
    assert_assemble_err!("BIT #$44");

    // Implied
    assert_assemble_err!("BIT\n");

    // Relative
    assert_assemble_err!("BIT -44");

    // ZeroPage
    assert_assemble!("BIT $44", &[0x24, 0x44]);

    // ZeroPageX
    assert_assemble_err!("BIT $44,X");

    // ZeroPageY
    assert_assemble_err!("BIT $44,Y");
}

#[test]
fn bcc() {
    // Absolute
    assert_assemble_err!("BCC $4400");

    // AbsoluteX
    assert_assemble_err!("BCC $4400,X");

    // AbsoluteY
    assert_assemble_err!("BCC $4400,Y");

    // Accumulator
    assert_assemble_err!("BCC A");

    // Indirect
    assert_assemble_err!("BCC ($4400)");

    // IndexedIndirect
    assert_assemble_err!("BCC ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("BCC ($44),Y");

    // Immediate
    assert_assemble_err!("BCC #$44");

    // Implied
    assert_assemble_err!("BCC\n");

    // Relative
    assert_assemble!("BCC -44", &[0x90, 0xd4]);

    // ZeroPageX
    assert_assemble_err!("BCC $44,X");

    // ZeroPageY
    assert_assemble_err!("BCC $44,Y");
}

#[test]
fn bcs() {
    // Absolute
    assert_assemble_err!("BCS $4400");

    // AbsoluteX
    assert_assemble_err!("BCS $4400,X");

    // AbsoluteY
    assert_assemble_err!("BCS $4400,Y");

    // Accumulator
    assert_assemble_err!("BCS A");

    // Indirect
    assert_assemble_err!("BCS ($4400)");

    // IndexedIndirect
    assert_assemble_err!("BCS ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("BCS ($44),Y");

    // Immediate
    assert_assemble_err!("BCS #$44");

    // Implied
    assert_assemble_err!("BCS\n");

    // Relative
    assert_assemble!("BCS -44", &[0xb0, 0xd4]);

    // ZeroPageX
    assert_assemble_err!("BCS $44,X");

    // ZeroPageY
    assert_assemble_err!("BCS $44,Y");
}

#[test]
fn beq() {
    // Absolute
    assert_assemble_err!("BEQ $4400");

    // AbsoluteX
    assert_assemble_err!("BEQ $4400,X");

    // AbsoluteY
    assert_assemble_err!("BEQ $4400,Y");

    // Accumulator
    assert_assemble_err!("BEQ A");

    // Indirect
    assert_assemble_err!("BEQ ($4400)");

    // IndexedIndirect
    assert_assemble_err!("BEQ ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("BEQ ($44),Y");

    // Immediate
    assert_assemble_err!("BEQ #$44");

    // Implied
    assert_assemble_err!("BEQ\n");

    // Relative
    assert_assemble!("BEQ -44", &[0xf0, 0xd4]);

    // ZeroPageX
    assert_assemble_err!("BEQ $44,X");

    // ZeroPageY
    assert_assemble_err!("BEQ $44,Y");
}

#[test]
fn bmi() {
    // Absolute
    assert_assemble_err!("BMI $4400");

    // AbsoluteX
    assert_assemble_err!("BMI $4400,X");

    // AbsoluteY
    assert_assemble_err!("BMI $4400,Y");

    // Accumulator
    assert_assemble_err!("BMI A");

    // Indirect
    assert_assemble_err!("BMI ($4400)");

    // IndexedIndirect
    assert_assemble_err!("BMI ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("BMI ($44),Y");

    // Immediate
    assert_assemble_err!("BMI #$44");

    // Implied
    assert_assemble_err!("BMI\n");

    // Relative
    assert_assemble!("BMI -44", &[0x30, 0xd4]);

    // ZeroPageX
    assert_assemble_err!("BMI $44,X");

    // ZeroPageY
    assert_assemble_err!("BMI $44,Y");
}

#[test]
fn bne() {
    // Absolute
    assert_assemble_err!("BNE $4400");

    // AbsoluteX
    assert_assemble_err!("BNE $4400,X");

    // AbsoluteY
    assert_assemble_err!("BNE $4400,Y");

    // Accumulator
    assert_assemble_err!("BNE A");

    // Indirect
    assert_assemble_err!("BNE ($4400)");

    // IndexedIndirect
    assert_assemble_err!("BNE ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("BNE ($44),Y");

    // Immediate
    assert_assemble_err!("BNE #$44");

    // Implied
    assert_assemble_err!("BNE\n");

    // Relative
    assert_assemble!("BNE -44", &[0xd0, 0xd4]);

    // ZeroPageX
    assert_assemble_err!("BNE $44,X");

    // ZeroPageY
    assert_assemble_err!("BNE $44,Y");
}

#[test]
fn bpl() {
    // Absolute
    assert_assemble_err!("BPL $4400");

    // AbsoluteX
    assert_assemble_err!("BPL $4400,X");

    // AbsoluteY
    assert_assemble_err!("BPL $4400,Y");

    // Accumulator
    assert_assemble_err!("BPL A");

    // Indirect
    assert_assemble_err!("BPL ($4400)");

    // IndexedIndirect
    assert_assemble_err!("BPL ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("BPL ($44),Y");

    // Immediate
    assert_assemble_err!("BPL #$44");

    // Implied
    assert_assemble_err!("BPL\n");

    // Relative
    assert_assemble!("BPL -44", &[0x10, 0xd4]);

    // ZeroPageX
    assert_assemble_err!("BPL $44,X");

    // ZeroPageY
    assert_assemble_err!("BPL $44,Y");
}

#[test]
fn bvc() {
    // Absolute
    assert_assemble_err!("BVC $4400");

    // AbsoluteX
    assert_assemble_err!("BVC $4400,X");

    // AbsoluteY
    assert_assemble_err!("BVC $4400,Y");

    // Accumulator
    assert_assemble_err!("BVC A");

    // Indirect
    assert_assemble_err!("BVC ($4400)");

    // IndexedIndirect
    assert_assemble_err!("BVC ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("BVC ($44),Y");

    // Immediate
    assert_assemble_err!("BVC #$44");

    // Implied
    assert_assemble_err!("BVC\n");

    // Relative
    assert_assemble!("BVC -44", &[0x50, 0xd4]);

    // ZeroPageX
    assert_assemble_err!("BVC $44,X");

    // ZeroPageY
    assert_assemble_err!("BVC $44,Y");
}

#[test]
fn bvs() {
    // Absolute
    assert_assemble_err!("BVS $4400");

    // AbsoluteX
    assert_assemble_err!("BVS $4400,X");

    // AbsoluteY
    assert_assemble_err!("BVS $4400,Y");

    // Accumulator
    assert_assemble_err!("BVS A");

    // Indirect
    assert_assemble_err!("BVS ($4400)");

    // IndexedIndirect
    assert_assemble_err!("BVS ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("BVS ($44),Y");

    // Immediate
    assert_assemble_err!("BVS #$44");

    // Implied
    assert_assemble_err!("BVS\n");

    // Relative
    assert_assemble!("BVS -44", &[0x70, 0xd4]);

    // ZeroPageX
    assert_assemble_err!("BVS $44,X");

    // ZeroPageY
    assert_assemble_err!("BVS $44,Y");
}

#[test]
fn brk() {
    // Absolute
    assert_assemble_err!("BRK $4400");

    // AbsoluteX
    assert_assemble_err!("BRK $4400,X");

    // AbsoluteY
    assert_assemble_err!("BRK $4400,Y");

    // Accumulator
    assert_assemble_err!("BRK A");

    // Indirect
    assert_assemble_err!("BRK ($4400)");

    // IndexedIndirect
    assert_assemble_err!("BRK ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("BRK ($44),Y");

    // Immediate
    assert_assemble_err!("BRK #$44");

    // Implied
    assert_assemble!("BRK\n", &[0x0, 0x0]);

    // Relative
    assert_assemble_err!("BRK -44");

    // ZeroPage
    assert_assemble_err!("BRK $44");

    // ZeroPageX
    assert_assemble_err!("BRK $44,X");

    // ZeroPageY
    assert_assemble_err!("BRK $44,Y");
}

#[test]
fn cmp() {
    // Absolute
    assert_assemble!("CMP $4400", &[0xcd, 0x0, 0x44]);

    // AbsoluteX
    assert_assemble!("CMP $4400,X", &[0xdd, 0x0, 0x44]);

    // AbsoluteY
    assert_assemble!("CMP $4400,Y", &[0xd9, 0x0, 0x44]);

    // Accumulator
    assert_assemble_err!("CMP A");

    // Indirect
    assert_assemble_err!("CMP ($4400)");

    // IndexedIndirect
    assert_assemble!("CMP ($44,X)", &[0xc1, 0x44]);

    // IndirectIndexed
    assert_assemble!("CMP ($44),Y", &[0xd1, 0x44]);

    // Immediate
    assert_assemble!("CMP #$44", &[0xc9, 0x44]);

    // Implied
    assert_assemble_err!("CMP\n");

    // ZeroPage
    assert_assemble!("CMP $44", &[0xc5, 0x44]);

    // Relative
    assert_assemble_err!("CMP -44");

    // ZeroPageX
    assert_assemble!("CMP $44,X", &[0xd5, 0x44]);

    // ZeroPageY
    assert_assemble_err!("CMP $44,Y");
}

#[test]
fn cpx() {
    // Absolute
    assert_assemble!("CPX $4400", &[0xec, 0x0, 0x44]);

    // AbsoluteX
    assert_assemble_err!("CPX $4400,X");

    // AbsoluteY
    assert_assemble_err!("CPX $4400,Y");

    // Accumulator
    assert_assemble_err!("CPX A");

    // Indirect
    assert_assemble_err!("CPX ($4400)");

    // IndexedIndirect
    assert_assemble_err!("CPX ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("CPX ($44),Y");

    // Immediate
    assert_assemble!("CPX #$44", &[0xe0, 0x44]);

    // Implied
    assert_assemble_err!("CPX\n");

    // Relative
    assert_assemble_err!("CPX -44");

    // ZeroPage
    assert_assemble!("CPX $44", &[0xe4, 0x44]);

    // ZeroPageX
    assert_assemble_err!("CPX $44,X");

    // ZeroPageY
    assert_assemble_err!("CPX $44,Y");
}

#[test]
fn cpy() {
    // Absolute
    assert_assemble!("CPY $4400", &[0xcc, 0x0, 0x44]);

    // AbsoluteX
    assert_assemble_err!("CPY $4400,X");

    // AbsoluteY
    assert_assemble_err!("CPY $4400,Y");

    // Accumulator
    assert_assemble_err!("CPY A");

    // Indirect
    assert_assemble_err!("CPY ($4400)");

    // IndexedIndirect
    assert_assemble_err!("CPY ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("CPY ($44),Y");

    // Immediate
    assert_assemble!("CPY #$44", &[0xc0, 0x44]);

    // Implied
    assert_assemble_err!("CPY\n");

    // Relative
    assert_assemble_err!("CPY -44");

    // ZeroPage
    assert_assemble!("CPY $44", &[0xc4, 0x44]);

    // ZeroPageX
    assert_assemble_err!("CPY $44,X");

    // ZeroPageY
    assert_assemble_err!("CPY $44,Y");
}

#[test]
fn dec() {
    // Absolute
    assert_assemble!("DEC $4400", &[0xce, 0x0, 0x44]);

    // AbsoluteX
    assert_assemble!("DEC $4400,X", &[0xde, 0x0, 0x44]);

    // AbsoluteY
    assert_assemble_err!("DEC $4400,Y");

    // Accumulator
    assert_assemble_err!("DEC A");

    // IndexedIndirect
    assert_assemble_err!("DEC ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("DEC ($44),Y");

    // Indirect
    assert_assemble_err!("DEC ($4400)");

    // Immediate
    assert_assemble_err!("DEC #$44");

    // Implied
    assert_assemble_err!("DEC\n");

    // Relative
    assert_assemble_err!("DEC -44");

    // ZeroPage
    assert_assemble!("DEC $44", &[0xc6, 0x44]);

    // ZeroPageX
    assert_assemble!("DEC $44,X", &[0xd6, 0x44]);

    // ZeroPageY
    assert_assemble_err!("DEC $44,Y");
}

#[test]
fn eor() {
    // Absolute
    assert_assemble!("EOR $4400", &[0x4d, 0x00, 0x44]);

    // AbsoluteX
    assert_assemble!("EOR $4400,X", &[0x5d, 0x00, 0x44]);

    // AbsoluteY
    assert_assemble!("EOR $4400,Y", &[0x59, 0x00, 0x44]);

    // Accumulator
    assert_assemble_err!("EOR A");

    // Indirect
    assert_assemble_err!("EOR ($4400)");

    // IndexedIndirect
    assert_assemble!("EOR ($44,X)", &[0x41, 0x44]);

    // IndirectIndexed
    assert_assemble!("EOR ($44),Y", &[0x51, 0x44]);

    // Immediate
    assert_assemble!("EOR #$44", &[0x49, 0x44]);

    // Implied
    assert_assemble_err!("EOR\n");

    // Relative
    assert_assemble_err!("EOR -44");

    // ZeroPage
    assert_assemble!("EOR $44", &[0x45, 0x44]);

    // ZeroPageX
    assert_assemble!("EOR $44,X", &[0x55, 0x44]);

    // ZeroPageY
    assert_assemble_err!("EOR $44,Y");
}

#[test]
fn clc() {
    // Absolute
    assert_assemble_err!("CLC $4400");

    // AbsoluteX
    assert_assemble_err!("CLC $4400,X");

    // AbsoluteY
    assert_assemble_err!("CLC $4400,Y");

    // Accumulator
    assert_assemble_err!("CLC A");

    // Indirect
    assert_assemble_err!("CLC ($4400)");

    // IndexedIndirect
    assert_assemble_err!("CLC ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("CLC ($44),Y");

    // Immediate
    assert_assemble_err!("CLC #$44");

    // Implied
    assert_assemble!("CLC\n", &[0x18]);

    // Relative
    assert_assemble_err!("CLC -44");

    // ZeroPage
    assert_assemble_err!("CLC $44");

    // ZeroPageX
    assert_assemble_err!("CLC $44,X");

    // ZeroPageY
    assert_assemble_err!("CLC $44,Y");
}

#[test]
fn cld() {
    // Absolute
    assert_assemble_err!("CLD $4400");

    // AbsoluteX
    assert_assemble_err!("CLD $4400,X");

    // AbsoluteY
    assert_assemble_err!("CLD $4400,Y");

    // Accumulator
    assert_assemble_err!("CLD A");

    // Indirect
    assert_assemble_err!("CLD ($4400)");

    // IndexedIndirect
    assert_assemble_err!("CLD ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("CLD ($44),Y");

    // Immediate
    assert_assemble_err!("CLD #$44");

    // Implied
    assert_assemble!("CLD\n", &[0xd8]);

    // Relative
    assert_assemble_err!("CLD -44");

    // ZeroPage
    assert_assemble_err!("CLD $44");

    // ZeroPageX
    assert_assemble_err!("CLD $44,X");

    // ZeroPageY
    assert_assemble_err!("CLD $44,Y");
}

#[test]
fn cli() {
    // Absolute
    assert_assemble_err!("CLI $4400");

    // AbsoluteX
    assert_assemble_err!("CLI $4400,X");

    // AbsoluteY
    assert_assemble_err!("CLI $4400,Y");

    // Accumulator
    assert_assemble_err!("CLI A");

    // Indirect
    assert_assemble_err!("CLI ($4400)");

    // IndexedIndirect
    assert_assemble_err!("CLI ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("CLI ($44),Y");

    // Immediate
    assert_assemble_err!("CLI #$44");

    // Implied
    assert_assemble!("CLI\n", &[0x58]);

    // Relative
    assert_assemble_err!("CLI -44");

    // ZeroPage
    assert_assemble_err!("CLI $44");

    // ZeroPageX
    assert_assemble_err!("CLI $44,X");

    // ZeroPageY
    assert_assemble_err!("CLI $44,Y");
}

#[test]
fn clv() {
    // Absolute
    assert_assemble_err!("CLV $4400");

    // AbsoluteX
    assert_assemble_err!("CLV $4400,X");

    // AbsoluteY
    assert_assemble_err!("CLV $4400,Y");

    // Accumulator
    assert_assemble_err!("CLV A");

    // Indirect
    assert_assemble_err!("CLV ($4400)");

    // IndexedIndirect
    assert_assemble_err!("CLV ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("CLV ($44),Y");

    // Immediate
    assert_assemble_err!("CLV #$44");

    // Implied
    assert_assemble!("CLV\n", &[0xb8]);

    // Relative
    assert_assemble_err!("CLV -44");

    // ZeroPage
    assert_assemble_err!("CLV $44");

    // ZeroPageX
    assert_assemble_err!("CLV $44,X");

    // ZeroPageY
    assert_assemble_err!("CLV $44,Y");
}

#[test]
fn sec() {
    // Absolute
    assert_assemble_err!("SEC $4400");

    // AbsoluteX
    assert_assemble_err!("SEC $4400,X");

    // AbsoluteY
    assert_assemble_err!("SEC $4400,Y");

    // Accumulator
    assert_assemble_err!("SEC A");

    // Indirect
    assert_assemble_err!("SEC ($4400)");

    // IndexedIndirect
    assert_assemble_err!("SEC ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("SEC ($44),Y");

    // Immediate
    assert_assemble_err!("SEC #$44");

    // Implied
    assert_assemble!("SEC\n", &[0x38]);

    // Relative
    assert_assemble_err!("SEC -44");

    // ZeroPage
    assert_assemble_err!("SEC $44");

    // ZeroPageX
    assert_assemble_err!("SEC $44,X");

    // ZeroPageY
    assert_assemble_err!("SEC $44,Y");
}

#[test]
fn sed() {
    // Absolute
    assert_assemble_err!("SED $4400");

    // AbsoluteX
    assert_assemble_err!("SED $4400,X");

    // AbsoluteY
    assert_assemble_err!("SED $4400,Y");

    // Accumulator
    assert_assemble_err!("SED A");

    // Indirect
    assert_assemble_err!("SED ($4400)");

    // IndexedIndirect
    assert_assemble_err!("SED ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("SED ($44),Y");

    // Immediate
    assert_assemble_err!("SED #$44");

    // Implied
    assert_assemble!("SED\n", &[0xf8]);

    // Relative
    assert_assemble_err!("SED -44");

    // ZeroPage
    assert_assemble_err!("SED $44");

    // ZeroPageX
    assert_assemble_err!("SED $44,X");

    // ZeroPageY
    assert_assemble_err!("SED $44,Y");
}

#[test]
fn sei() {
    // Absolute
    assert_assemble_err!("SEI $4400");

    // AbsoluteX
    assert_assemble_err!("SEI $4400,X");

    // AbsoluteY
    assert_assemble_err!("SEI $4400,Y");

    // Accumulator
    assert_assemble_err!("SEI A");

    // Indirect
    assert_assemble_err!("SEI ($4400)");

    // IndexedIndirect
    assert_assemble_err!("SEI ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("SEI ($44),Y");

    // Immediate
    assert_assemble_err!("SEI #$44");

    // Implied
    assert_assemble!("SEI\n", &[0x78]);

    // Relative
    assert_assemble_err!("SEI -44");

    // ZeroPage
    assert_assemble_err!("SEI $44");

    // ZeroPageX
    assert_assemble_err!("SEI $44,X");

    // ZeroPageY
    assert_assemble_err!("SEI $44,Y");
}

#[test]
fn inc() {
    // Absolute
    assert_assemble!("INC $4400", &[0xee, 0x0, 0x44]);

    // AbsoluteX
    assert_assemble!("INC $4400,X", &[0xfe, 0x0, 0x44]);

    // AbsoluteY
    assert_assemble_err!("INC $4400,Y");

    // Accumulator
    assert_assemble_err!("INC A");

    // Indirect
    assert_assemble_err!("INC ($4400)");

    // IndexedIndirect
    assert_assemble_err!("INC ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("INC ($44),Y");

    // Immediate
    assert_assemble_err!("INC #$44");

    // Implied
    assert_assemble_err!("INC\n");

    // Relative
    assert_assemble_err!("INC -44");

    // ZeroPage
    assert_assemble!("INC $44", &[0xe6, 0x44]);

    // ZeroPageX
    assert_assemble!("INC $44,X", &[0xf6, 0x44]);

    // ZeroPageY
    assert_assemble_err!("INC $44,Y");
}

#[test]
fn jmp() {
    // Absolute
    assert_assemble!("JMP $4400", &[0x4c, 0x00, 0x44]);

    // AbsoluteX
    assert_assemble_err!("JMP $4400,X");

    // AbsoluteY
    assert_assemble_err!("JMP $4400,Y");

    // Accumulator
    assert_assemble_err!("JMP A");

    // Indirect
    assert_assemble!("JMP ($4400)", &[0x6c, 0x0, 0x44]);

    // IndexedIndirect
    assert_assemble_err!("JMP ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("JMP ($44),Y");

    // Immediate
    assert_assemble_err!("JMP #$44");

    // Implied
    assert_assemble_err!("JMP\n");

    // Relative
    assert_assemble_err!("JMP -44");

    // ZeroPage
    assert_assemble_err!("JMP $44");

    // ZeroPageX
    assert_assemble_err!("JMP $44,X");

    // ZeroPageY
    assert_assemble_err!("JMP $44,Y");
}

#[test]
fn jsr() {
    // Absolute
    assert_assemble!("JSR $4400", &[0x20, 0x00, 0x44]);

    // AbsoluteX
    assert_assemble_err!("JSR $4400,X");

    // AbsoluteY
    assert_assemble_err!("JSR $4400,Y");

    // Accumulator
    assert_assemble_err!("JSR A");

    // Indirect
    assert_assemble_err!("JSR ($4400)");

    // IndexedIndirect
    assert_assemble_err!("JSR ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("JSR ($44),Y");

    // Immediate
    assert_assemble_err!("JSR #$44");

    // Implied
    assert_assemble_err!("JSR\n");

    // Relative
    assert_assemble_err!("JSR -44");

    // ZeroPage
    assert_assemble_err!("JSR $44");

    // ZeroPageX
    assert_assemble_err!("JSR $44,X");

    // ZeroPageY
    assert_assemble_err!("JSR $44,Y");
}

#[test]
fn lda() {
    // Absolute
    assert_assemble!("LDA $4400", &[0xad, 0x00, 0x44]);

    // AbsoluteX
    assert_assemble!("LDA $4400,X", &[0xbd, 0x00, 0x44]);

    // AbsoluteY
    assert_assemble!("LDA $4400,Y", &[0xb9, 0x00, 0x44]);

    // Accumulator
    assert_assemble_err!("LDA A");

    // Indirect
    assert_assemble_err!("LDA ($4400)");

    // IndexedIndirect
    assert_assemble!("LDA ($44,X)", &[0xa1, 0x44]);

    // IndirectIndexed
    assert_assemble!("LDA ($44),Y", &[0xb1, 0x44]);

    // Immediate
    assert_assemble!("LDA #$44", &[0xa9, 0x44]);

    // Implied
    assert_assemble_err!("LDA\n");

    // Relative
    assert_assemble_err!("LDA -44");

    // ZeroPage
    assert_assemble!("LDA $44", &[0xa5, 0x44]);

    // ZeroPageX
    assert_assemble!("LDA $44,X", &[0xb5, 0x44]);

    // ZeroPageY
    assert_assemble_err!("LDA $44,Y");
}

#[test]
fn ldx() {
    // Absolute
    assert_assemble!("LDX $4400", &[0xae, 0x00, 0x44]);

    // AbsoluteX
    assert_assemble_err!("LDX $4400,X");

    // AbsoluteY
    assert_assemble!("LDX $4400,Y", &[0xbe, 0x00, 0x44]);

    // Accumulator
    assert_assemble_err!("LDX A");

    // Indirect
    assert_assemble_err!("LDX ($4400)");

    // IndexedIndirect
    assert_assemble_err!("LDX ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("LDX ($44),Y");

    // Immediate
    assert_assemble!("LDX #$44", &[0xa2, 0x44]);

    // Implied
    assert_assemble_err!("LDX\n");

    // Relative
    assert_assemble_err!("LDX -44");

    // ZeroPage
    assert_assemble!("LDX $44", &[0xa6, 0x44]);

    // ZeroPageX
    assert_assemble_err!("LDX $44,X");

    // ZeroPageY
    assert_assemble!("LDX $44,Y", &[0xb6, 0x44]);
}

#[test]
fn ldy() {
    // Absolute
    assert_assemble!("LDY $4400", &[0xac, 0x00, 0x44]);

    // AbsoluteX
    assert_assemble!("LDY $4400,X", &[0xbc, 0x00, 0x44]);

    // AbsoluteY
    assert_assemble_err!("LDY $4400,Y");

    // Accumulator
    assert_assemble_err!("LDY A");

    // Indirect
    assert_assemble_err!("LDY ($4400)");

    // IndexedIndirect
    assert_assemble_err!("LDY ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("LDY ($44),Y");

    // Immediate
    assert_assemble!("LDY #$44", &[0xa0, 0x44]);

    // Implied
    assert_assemble_err!("LDY\n");

    // Relative
    assert_assemble_err!("LDY -44");

    // ZeroPage
    assert_assemble!("LDY $44", &[0xa4, 0x44]);

    // ZeroPageX
    assert_assemble!("LDY $44,X", &[0xb4, 0x44]);

    // ZeroPageY
    assert_assemble_err!("LDY $44,Y");
}

#[test]
fn lsr() {
    // Absolute
    assert_assemble!("LSR $4400", &[0x4e, 0x00, 0x44]);

    // AbsoluteX
    assert_assemble!("LSR $4400,X", &[0x5e, 0x00, 0x44]);

    // AbsoluteY
    assert_assemble_err!("LSR $4400,Y");

    // Indirect
    assert_assemble_err!("LSR ($4400)");

    // Accumulator
    assert_assemble!("LSR A", &[0x4a]);

    // IndexedIndirect
    assert_assemble_err!("LSR ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("LSR ($44),Y");

    // Immediate
    assert_assemble_err!("LSR #$44");

    // Implied
    assert_assemble_err!("LSR\n");

    // Relative
    assert_assemble_err!("LSR -44");

    // ZeroPage
    assert_assemble!("LSR $44", &[0x46, 0x44]);

    // ZeroPageX
    assert_assemble!("LSR $44,X", &[0x56, 0x44]);

    // ZeroPageY
    assert_assemble_err!("LSR $44,Y");
}

#[test]
fn nop() {
    // Absolute
    assert_assemble_err!("NOP $4400");

    // AbsoluteX
    assert_assemble_err!("NOP $4400,X");

    // AbsoluteY
    assert_assemble_err!("NOP $4400,Y");

    // Accumulator
    assert_assemble_err!("NOP A");

    // Indirect
    assert_assemble_err!("NOP ($4400)");

    // IndexedIndirect
    assert_assemble_err!("NOP ($44,X)");

    // IndirectIndexed
    assert_assemble_err!("NOP ($44),Y");

    // Immediate
    assert_assemble_err!("NOP #$44");

    // Implied
    assert_assemble!("NOP\n", &[0xea]);

    // Relative
    assert_assemble_err!("NOP -44");

    // ZeroPage
    assert_assemble_err!("NOP $44");

    // ZeroPageX
    assert_assemble_err!("NOP $44,X");

    // ZeroPageY
    assert_assemble_err!("NOP $44,Y");
}

#[test]
fn ora() {
    // Absolute
    assert_assemble!("ORA $4400", &[0x0d, 0x00, 0x44]);

    // AbsoluteX
    assert_assemble!("ORA $4400,X", &[0x1d, 0x00, 0x44]);

    // AbsoluteY
    assert_assemble!("ORA $4400,Y", &[0x19, 0x00, 0x44]);

    // Accumulator
    assert_assemble_err!("ORA A");

    // Indirect
    assert_assemble_err!("ORA ($4400)");

    // IndexedIndirect
    assert_assemble!("ORA ($44,X)", &[0x01, 0x44]);

    // IndirectIndexed
    assert_assemble!("ORA ($44),Y", &[0x11, 0x44]);

    // Immediate
    assert_assemble!("ORA #$44", &[0x09, 0x44]);

    // Implied
    assert_assemble_err!("ORA\n");

    // Relative
    assert_assemble_err!("ADC -44");

    // ZeroPage
    assert_assemble!("ORA $44", &[0x05, 0x44]);

    // ZeroPageX
    assert_assemble!("ORA $44,X", &[0x15, 0x44]);

    // ZeroPageY
    assert_assemble_err!("ORA $44,Y");
}
