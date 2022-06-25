use crate::parser::*;
use crate::tokens::*;
use nom::IResult;

macro_rules! assert_parse {
    ( $ left : expr , $ right : expr ) => {
        if let IResult::Ok((_, actual)) = $right {
            assert_eq!($left, actual)
        } else {
            panic!(
                "Expected parse result to be {:?}, but parsing failed with: {:?}",
                $left, $right
            )
        }
    };
}

macro_rules! assert_am_parse {
    ( $ input : expr , $ expected : expr ) => {
        let result = addressing_mode($input);
        assert_parse!($expected, result);
    };
}

macro_rules! assert_opcode_parse {
    ( $ input : expr , $ expected : expr ) => {
        assert_parse!($expected, instruction($input));
    };
}

macro_rules! assert_mnemonic_parse {
    ( $ input : expr , $ expected : expr ) => {
        let result = mnemonic($input);
        assert_parse!($expected, result);
    };
}

macro_rules! assert_parse_fail {
    ( $ result : expr ) => {
        if let IResult::Ok((_, actual)) = $result {
            panic!(
                "Expected parsing to fail, but it succeeded with: {:?}",
                actual
            );
        }
    };
}

#[test]
fn parse_mnemonic() {
    assert_mnemonic_parse!("ADC", Mnemonic::Adc);
    assert_mnemonic_parse!("AND", Mnemonic::And);
    assert_mnemonic_parse!("ASL", Mnemonic::Asl);
    assert_mnemonic_parse!("BCC", Mnemonic::Bcc);
    assert_mnemonic_parse!("BCS", Mnemonic::Bcs);
    assert_mnemonic_parse!("BEQ", Mnemonic::Beq);
    assert_mnemonic_parse!("BIT", Mnemonic::Bit);
    assert_mnemonic_parse!("BMI", Mnemonic::Bmi);
    assert_mnemonic_parse!("BNE", Mnemonic::Bne);
    assert_mnemonic_parse!("BPL", Mnemonic::Bpl);
    assert_mnemonic_parse!("BRK", Mnemonic::Brk);
    assert_mnemonic_parse!("BVC", Mnemonic::Bvc);
    assert_mnemonic_parse!("BVS", Mnemonic::Bvs);
    assert_mnemonic_parse!("CLC", Mnemonic::Clc);
    assert_mnemonic_parse!("CLD", Mnemonic::Cld);
    assert_mnemonic_parse!("CLI", Mnemonic::Cli);
    assert_mnemonic_parse!("CLV", Mnemonic::Clv);
    assert_mnemonic_parse!("CMP", Mnemonic::Cmp);
    assert_mnemonic_parse!("CPX", Mnemonic::Cpx);
    assert_mnemonic_parse!("CPY", Mnemonic::Cpy);
    assert_mnemonic_parse!("DEC", Mnemonic::Dec);
    assert_mnemonic_parse!("DEX", Mnemonic::Dex);
    assert_mnemonic_parse!("DEY", Mnemonic::Dey);
    assert_mnemonic_parse!("EOR", Mnemonic::Eor);
    assert_mnemonic_parse!("INC", Mnemonic::Inc);
    assert_mnemonic_parse!("INX", Mnemonic::Inx);
    assert_mnemonic_parse!("INY", Mnemonic::Iny);
    assert_mnemonic_parse!("JMP", Mnemonic::Jmp);
    assert_mnemonic_parse!("JSR", Mnemonic::Jsr);
    assert_mnemonic_parse!("LDA", Mnemonic::Lda);
    assert_mnemonic_parse!("LDX", Mnemonic::Ldx);
    assert_mnemonic_parse!("LDY", Mnemonic::Ldy);
    assert_mnemonic_parse!("LSR", Mnemonic::Lsr);
    assert_mnemonic_parse!("NOP", Mnemonic::Nop);
    assert_mnemonic_parse!("ORA", Mnemonic::Ora);
    assert_mnemonic_parse!("PHA", Mnemonic::Pha);
    assert_mnemonic_parse!("PHP", Mnemonic::Php);
    assert_mnemonic_parse!("PLA", Mnemonic::Pla);
    assert_mnemonic_parse!("PLP", Mnemonic::Plp);
    assert_mnemonic_parse!("ROL", Mnemonic::Rol);
    assert_mnemonic_parse!("ROR", Mnemonic::Ror);
    assert_mnemonic_parse!("RTI", Mnemonic::Rti);
    assert_mnemonic_parse!("RTS", Mnemonic::Rts);
    assert_mnemonic_parse!("SBC", Mnemonic::Sbc);
    assert_mnemonic_parse!("SEC", Mnemonic::Sec);
    assert_mnemonic_parse!("SED", Mnemonic::Sed);
    assert_mnemonic_parse!("SEI", Mnemonic::Sei);
    assert_mnemonic_parse!("STA", Mnemonic::Sta);
    assert_mnemonic_parse!("STX", Mnemonic::Stx);
    assert_mnemonic_parse!("STY", Mnemonic::Sty);
    assert_mnemonic_parse!("TAX", Mnemonic::Tax);
    assert_mnemonic_parse!("TAY", Mnemonic::Tay);
    assert_mnemonic_parse!("TSX", Mnemonic::Tsx);
    assert_mnemonic_parse!("TXA", Mnemonic::Txa);
    assert_mnemonic_parse!("TXS", Mnemonic::Txs);
    assert_mnemonic_parse!("TYA", Mnemonic::Tya);
}

#[test]
fn parse_accumulator() {
    assert_am_parse!("A", AddressingMode::Accumulator);
}

#[test]
fn parse_immediate_hex() {
    assert_am_parse!("#$1", AddressingMode::Immediate(0x1, Sign::Implied));
    assert_am_parse!("#$10", AddressingMode::Immediate(0x10, Sign::Implied));
    assert_am_parse!("#$ff", AddressingMode::Immediate(0xff, Sign::Implied));
    assert_parse_fail!(all_consuming(addressing_mode)("#$100"));
}

#[test]
fn parse_immediate_dec() {
    assert_am_parse!("#1", AddressingMode::Immediate(1, Sign::Implied));
    assert_am_parse!("#10", AddressingMode::Immediate(10, Sign::Implied));
    assert_am_parse!("#255", AddressingMode::Immediate(255, Sign::Implied));
    assert_am_parse!("#-10", AddressingMode::Immediate(10, Sign::Negative));
    assert_parse_fail!(all_consuming(addressing_mode)("#256"));
}

#[test]
fn parse_absolute_x_hex() {
    assert_am_parse!("$ffff,X", AddressingMode::AbsoluteX(0xffff));
    assert_am_parse!("$1000,X", AddressingMode::AbsoluteX(0x1000));
    assert_am_parse!("$100,X", AddressingMode::AbsoluteX(0x100));
}

#[test]
fn parse_absolute_x_dec() {
    assert_am_parse!("65535,X", AddressingMode::AbsoluteX(65535));
    assert_am_parse!("1000,X", AddressingMode::AbsoluteX(1000));
    assert_am_parse!("256,X", AddressingMode::AbsoluteX(256));
}

#[test]
fn parse_absolute_y_hex() {
    assert_am_parse!("$ffff,Y", AddressingMode::AbsoluteY(0xffff));
    assert_am_parse!("$1000,Y", AddressingMode::AbsoluteY(0x1000));
    assert_am_parse!("$100,Y", AddressingMode::AbsoluteY(0x100));
}

#[test]
fn parse_absolute_y_dec() {
    assert_am_parse!("65535,Y", AddressingMode::AbsoluteY(65535));
    assert_am_parse!("1000,Y", AddressingMode::AbsoluteY(1000));
    assert_am_parse!("256,Y", AddressingMode::AbsoluteY(256));
}

#[test]
fn parse_indexed_indirect_hex() {
    assert_am_parse!("($ff,X)", AddressingMode::IndexedIndirect(0xff));
    assert_am_parse!("($0,X)", AddressingMode::IndexedIndirect(0x0));
    assert_am_parse!("($10,X)", AddressingMode::IndexedIndirect(0x10));
}

#[test]
fn parse_indexed_indirect_dec() {
    assert_am_parse!("(255,X)", AddressingMode::IndexedIndirect(255));
    assert_am_parse!("(0,X)", AddressingMode::IndexedIndirect(0));
    assert_am_parse!("(10,X)", AddressingMode::IndexedIndirect(10));
}

#[test]
fn parse_indirect_indexed_hex() {
    assert_am_parse!("($ff),Y", AddressingMode::IndirectIndexed(0xff));
    assert_am_parse!("($0),Y", AddressingMode::IndirectIndexed(0x0));
    assert_am_parse!("($10),Y", AddressingMode::IndirectIndexed(0x10));
}

#[test]
fn parse_indirect_indexed_dec() {
    assert_am_parse!("(255),Y", AddressingMode::IndirectIndexed(255));
    assert_am_parse!("(0),Y", AddressingMode::IndirectIndexed(0));
    assert_am_parse!("(10),Y", AddressingMode::IndirectIndexed(10));
}

#[test]
fn parse_indirect_hex() {
    assert_am_parse!("($ffff)", AddressingMode::Indirect(0xffff));
    assert_am_parse!("($00)", AddressingMode::Indirect(0x0));
    assert_am_parse!("($100)", AddressingMode::Indirect(0x100));
}

#[test]
fn parse_indirect_dec() {
    assert_am_parse!("(65535)", AddressingMode::Indirect(65535));
    assert_am_parse!("(0)", AddressingMode::Indirect(0));
    assert_am_parse!("(10)", AddressingMode::Indirect(10));
}

#[test]
fn parse_zero_page_or_relative_hex() {
    assert_am_parse!(
        "$ff",
        AddressingMode::ZeroPageOrRelative(0xff, Sign::Implied)
    );
    assert_am_parse!("$0", AddressingMode::ZeroPageOrRelative(0x0, Sign::Implied));
    assert_am_parse!(
        "$10",
        AddressingMode::ZeroPageOrRelative(0x10, Sign::Implied)
    );
}

#[test]
fn parse_zero_page_or_relative_dec() {
    assert_am_parse!(
        "255",
        AddressingMode::ZeroPageOrRelative(255, Sign::Implied)
    );
    assert_am_parse!("0", AddressingMode::ZeroPageOrRelative(0, Sign::Implied));
    assert_am_parse!("10", AddressingMode::ZeroPageOrRelative(10, Sign::Implied));
    assert_am_parse!(
        "-10",
        AddressingMode::ZeroPageOrRelative(10, Sign::Negative)
    );
}

#[test]
fn parse_zero_page_x_hex() {
    assert_am_parse!("$ff,X", AddressingMode::ZeroPageX(0xff));
    assert_am_parse!("$0,X", AddressingMode::ZeroPageX(0x0));
    assert_am_parse!("$10,X", AddressingMode::ZeroPageX(0x10));
}

#[test]
fn parse_zero_page_x_dec() {
    assert_am_parse!("255,X", AddressingMode::ZeroPageX(255));
    assert_am_parse!("0,X", AddressingMode::ZeroPageX(0));
    assert_am_parse!("10,X", AddressingMode::ZeroPageX(10));
}

#[test]
fn parse_zero_page_y_hex() {
    assert_am_parse!("$ff,Y", AddressingMode::ZeroPageY(0xff));
    assert_am_parse!("$0,Y", AddressingMode::ZeroPageY(0x0));
    assert_am_parse!("$10,Y", AddressingMode::ZeroPageY(0x10));
}

#[test]
fn parse_zero_page_y_dec() {
    assert_am_parse!("255,Y", AddressingMode::ZeroPageY(255));
    assert_am_parse!("0,Y", AddressingMode::ZeroPageY(0));
    assert_am_parse!("10,Y", AddressingMode::ZeroPageY(10));
}

#[test]
fn parse_absolute_hex() {
    assert_am_parse!("$ffff", AddressingMode::Absolute(0xffff));
    assert_am_parse!("$1000", AddressingMode::Absolute(0x1000));
    assert_am_parse!("$100", AddressingMode::Absolute(0x100));
}

#[test]
fn parse_absolute_dec() {
    assert_am_parse!("65535", AddressingMode::Absolute(65535));
    assert_am_parse!("1000", AddressingMode::Absolute(1000));
    assert_am_parse!("256", AddressingMode::Absolute(256));
}

#[test]
fn parse_implied() {
    assert_am_parse!("\n", AddressingMode::Implied);
}

#[test]
fn parse_opcode() {
    assert_opcode_parse!(
        "ADC #1",
        Token::OpCode(OpCode(
            Mnemonic::Adc,
            AddressingMode::Immediate(1, Sign::Implied)
        ))
    );
}

#[test]
fn parse_lines() {
    match super::parse_lines("ADC #1\nSBC $FFFF\nJMP ($ff00)\n") {
        IResult::Ok((_, opcodes)) => {
            assert_eq!(3, opcodes.len());
            assert_eq!(
                Token::OpCode(OpCode(
                    Mnemonic::Adc,
                    AddressingMode::Immediate(1, Sign::Implied)
                )),
                opcodes[0]
            );
            assert_eq!(
                Token::OpCode(OpCode(Mnemonic::Sbc, AddressingMode::Absolute(0xffff))),
                opcodes[1]
            );
            assert_eq!(
                Token::OpCode(OpCode(Mnemonic::Jmp, AddressingMode::Indirect(0xff00))),
                opcodes[2]
            );
        }
        IResult::Err(e) => panic!("Parse lines failed with: {:?}", e),
    }
}

#[test]
fn several_implied_ops() {
    let parsed = super::parse_lines("NOP\nNOP\nBRK");
    let expected = vec![
        Token::OpCode(OpCode(Mnemonic::Nop, AddressingMode::Implied)),
        Token::OpCode(OpCode(Mnemonic::Nop, AddressingMode::Implied)),
        Token::OpCode(OpCode(Mnemonic::Brk, AddressingMode::Implied)),
    ];
    assert_eq!(parsed, Ok(("", expected)));
}

#[test]
fn empty_lines_in_front() {
    let parsed = super::parse_lines("\n   \nNOP\nBRK");
    let expected = vec![
        Token::OpCode(OpCode(Mnemonic::Nop, AddressingMode::Implied)),
        Token::OpCode(OpCode(Mnemonic::Brk, AddressingMode::Implied)),
    ];
    assert_eq!(parsed, Ok(("", expected)));
}

#[test]
fn empty_lines_in_middle() {
    let parsed = super::parse_lines("NOP\n    \nBRK");
    let expected = vec![
        Token::OpCode(OpCode(Mnemonic::Nop, AddressingMode::Implied)),
        Token::OpCode(OpCode(Mnemonic::Brk, AddressingMode::Implied)),
    ];
    assert_eq!(parsed, Ok(("", expected)));
}

#[test]
fn empty_lines_in_end() {
    let parsed = super::parse_lines("NOP\nBRK  \n\n");
    let expected = vec![
        Token::OpCode(OpCode(Mnemonic::Nop, AddressingMode::Implied)),
        Token::OpCode(OpCode(Mnemonic::Brk, AddressingMode::Implied)),
    ];
    assert_eq!(parsed, Ok(("", expected)));
}

#[test]
fn parsing_line_with_error() {
    let parsed = super::parse_lines("LDX #ab");
    assert!(parsed.is_err());
}

#[test]
fn single_label() {
    let parsed = super::parse_lines("label:");
    assert_eq!(
        parsed,
        Ok(("", vec![Token::Label("label".to_string())]))
    );
    assert!(parsed.is_ok());
}

#[test]
fn label_in_context() {
    let parsed = super::parse_lines("DEX\nlabel:\nDEX");
    assert_eq!(
        parsed,
        Ok((
            "",
            vec![
                Token::OpCode(OpCode(Mnemonic::Dex, AddressingMode::Implied)),
                Token::Label("label".to_string()),
                Token::OpCode(OpCode(Mnemonic::Dex, AddressingMode::Implied))
            ]
        ))
    );
}

// This feature was removed, but the test has been kept for now in case it may be
// re-instated
#[ignore]
#[test]
fn label_no_colon_in_context() {
    let parsed = super::parse_lines("DEX\nLabel\nDEX");
    assert_eq!(
        parsed,
        Ok((
            "",
            vec![
                Token::OpCode(OpCode(Mnemonic::Dex, AddressingMode::Implied)),
                Token::Label("Label".to_string()),
                Token::OpCode(OpCode(Mnemonic::Dex, AddressingMode::Implied))
            ]
        ))
    );
}

#[test]
fn label_in_opcode() {
    let parsed = super::parse_lines("BEQ LABEL");
    assert_eq!(
        parsed,
        Ok((
            "",
            vec![Token::OpCode(OpCode(
                Mnemonic::Beq,
                AddressingMode::Label("LABEL".to_string())
            )),]
        ))
    );
}

#[test]
fn parse_direct_bytes_only() {
    let parsed = super::parse_lines(".BYTE $4E, $45, $53, $1A");
    assert_eq!(
        parsed,
        Ok((
            "",
            vec![Token::ControlCommand(ControlCommand::Byte(vec![
                ('N' as u8, Sign::Implied),
                ('E' as u8, Sign::Implied),
                ('S' as u8, Sign::Implied),
                (0x1A, Sign::Implied)
            ]))]
        ))
    );
}

#[test]
fn parse_comments() {
    let parsed_with_comments = super::parse_lines(r#"
        NOP ; comment
        NOP; comment

        ; comment
        NOP
; comment
        NOP
;comment
    "#);
    let parsed_without_comments = super::parse_lines("NOP\nNOP\nNOP\nNOP");
    assert_eq!(parsed_with_comments, parsed_without_comments);
}

#[test]
fn parse_direct_bytes_interleved() {
    let parsed = super::parse_lines(r#"
        NOP
        .BYTE $4E, $45, $53, $1A
        NOP
    "#);
    assert_eq!(
        parsed,
        Ok((
            "",
            vec![
                Token::OpCode(OpCode(Mnemonic::Nop, AddressingMode::Implied)),
                Token::ControlCommand(ControlCommand::Byte(vec![
                    ('N' as u8, Sign::Implied),
                    ('E' as u8, Sign::Implied),
                    ('S' as u8, Sign::Implied),
                    (0x1A, Sign::Implied)
                ])),
                Token::OpCode(OpCode(Mnemonic::Nop, AddressingMode::Implied)),
            ]
        ))
    );
}

#[test]
fn parse_direct_bytes_db_alias() {
    let asm = ".BYTE $4E, $45, $53, $1A";
    let parsed_bytes = super::parse_lines(&asm);
    let parsed_db = super::parse_lines(&asm);
    assert_eq!(parsed_bytes, parsed_db);
}