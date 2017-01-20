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
