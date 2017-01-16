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
    assert_assemble_err!("ADC");

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
    assert_assemble_err!("AND");

    // Relative
    assert_assemble_err!("ADC -44");

    // ZeroPage
    assert_assemble!("AND $44", &[0x25, 0x44]);

    // ZeroPageX
    assert_assemble!("AND $44,X", &[0x35, 0x44]);

    // ZeroPageY
    assert_assemble_err!("AND $44,Y");
}
