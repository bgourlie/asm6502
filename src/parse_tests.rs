use nom::IResult;
use std::fmt::Debug;
use super::*;

macro_rules! assert_parse {
    ( $ left : expr , $ right : expr ) => {
        if let IResult::Done(_, actual) = $right {
            assert_eq!($left, actual)
        } else {
            panic!("Expected parse result to be {:?}, but parsing failed with: {:?}",
                   $left, $right)
        }
    };
}

macro_rules! assert_parse_fail {
    ( $ result : expr ) => {
        if let IResult::Done(_, actual) = $result {
            panic!("Expected parsing to fail, but it succeeded with: {:?}", actual);
        }
    };
}

#[test]
fn parse_mnemonic() {
    let result = mnemonic("ADC".as_bytes());
    assert_parse!(Mnemonic::ADC, result);

    let result = mnemonic("AND".as_bytes());
    assert_parse!(Mnemonic::AND, result);

    let result = mnemonic("ASL".as_bytes());
    assert_parse!(Mnemonic::ASL, result);

    let result = mnemonic("BCC".as_bytes());
    assert_parse!(Mnemonic::BCC, result);

    let result = mnemonic("BCS".as_bytes());
    assert_parse!(Mnemonic::BCS, result);

    let result = mnemonic("BEQ".as_bytes());
    assert_parse!(Mnemonic::BEQ, result);

    let result = mnemonic("BIT".as_bytes());
    assert_parse!(Mnemonic::BIT, result);

    let result = mnemonic("BMI".as_bytes());
    assert_parse!(Mnemonic::BMI, result);

    let result = mnemonic("BNE".as_bytes());
    assert_parse!(Mnemonic::BNE, result);

    let result = mnemonic("BPL".as_bytes());
    assert_parse!(Mnemonic::BPL, result);

    let result = mnemonic("BRK".as_bytes());
    assert_parse!(Mnemonic::BRK, result);

    let result = mnemonic("BVC".as_bytes());
    assert_parse!(Mnemonic::BVC, result);

    let result = mnemonic("BVS".as_bytes());
    assert_parse!(Mnemonic::BVS, result);

    let result = mnemonic("CLC".as_bytes());
    assert_parse!(Mnemonic::CLC, result);

    let result = mnemonic("CLD".as_bytes());
    assert_parse!(Mnemonic::CLD, result);

    let result = mnemonic("CLI".as_bytes());
    assert_parse!(Mnemonic::CLI, result);

    let result = mnemonic("CLV".as_bytes());
    assert_parse!(Mnemonic::CLV, result);

    let result = mnemonic("CMP".as_bytes());
    assert_parse!(Mnemonic::CMP, result);

    let result = mnemonic("CPX".as_bytes());
    assert_parse!(Mnemonic::CPX, result);

    let result = mnemonic("CPY".as_bytes());
    assert_parse!(Mnemonic::CPY, result);

    let result = mnemonic("DEC".as_bytes());
    assert_parse!(Mnemonic::DEC, result);

    let result = mnemonic("DEX".as_bytes());
    assert_parse!(Mnemonic::DEX, result);

    let result = mnemonic("DEY".as_bytes());
    assert_parse!(Mnemonic::DEY, result);

    let result = mnemonic("EOR".as_bytes());
    assert_parse!(Mnemonic::EOR, result);

    let result = mnemonic("INC".as_bytes());
    assert_parse!(Mnemonic::INC, result);

    let result = mnemonic("INX".as_bytes());
    assert_parse!(Mnemonic::INX, result);

    let result = mnemonic("INY".as_bytes());
    assert_parse!(Mnemonic::INY, result);

    let result = mnemonic("JMP".as_bytes());
    assert_parse!(Mnemonic::JMP, result);

    let result = mnemonic("JSR".as_bytes());
    assert_parse!(Mnemonic::JSR, result);

    let result = mnemonic("LDA".as_bytes());
    assert_parse!(Mnemonic::LDA, result);

    let result = mnemonic("LDX".as_bytes());
    assert_parse!(Mnemonic::LDX, result);

    let result = mnemonic("LDY".as_bytes());
    assert_parse!(Mnemonic::LDY, result);

    let result = mnemonic("LSR".as_bytes());
    assert_parse!(Mnemonic::LSR, result);

    let result = mnemonic("NOP".as_bytes());
    assert_parse!(Mnemonic::NOP, result);

    let result = mnemonic("ORA".as_bytes());
    assert_parse!(Mnemonic::ORA, result);

    let result = mnemonic("PHA".as_bytes());
    assert_parse!(Mnemonic::PHA, result);

    let result = mnemonic("PHP".as_bytes());
    assert_parse!(Mnemonic::PHP, result);

    let result = mnemonic("PLA".as_bytes());
    assert_parse!(Mnemonic::PLA, result);

    let result = mnemonic("PLP".as_bytes());
    assert_parse!(Mnemonic::PLP, result);

    let result = mnemonic("ROL".as_bytes());
    assert_parse!(Mnemonic::ROL, result);

    let result = mnemonic("ROR".as_bytes());
    assert_parse!(Mnemonic::ROR, result);

    let result = mnemonic("RTI".as_bytes());
    assert_parse!(Mnemonic::RTI, result);

    let result = mnemonic("RTS".as_bytes());
    assert_parse!(Mnemonic::RTS, result);

    let result = mnemonic("SBC".as_bytes());
    assert_parse!(Mnemonic::SBC, result);

    let result = mnemonic("SEC".as_bytes());
    assert_parse!(Mnemonic::SEC, result);

    let result = mnemonic("SED".as_bytes());
    assert_parse!(Mnemonic::SED, result);

    let result = mnemonic("SEI".as_bytes());
    assert_parse!(Mnemonic::SEI, result);

    let result = mnemonic("STA".as_bytes());
    assert_parse!(Mnemonic::STA, result);

    let result = mnemonic("STX".as_bytes());
    assert_parse!(Mnemonic::STX, result);

    let result = mnemonic("STY".as_bytes());
    assert_parse!(Mnemonic::STY, result);

    let result = mnemonic("TAX".as_bytes());
    assert_parse!(Mnemonic::TAX, result);

    let result = mnemonic("TAY".as_bytes());
    assert_parse!(Mnemonic::TAY, result);

    let result = mnemonic("TSX".as_bytes());
    assert_parse!(Mnemonic::TSX, result);

    let result = mnemonic("TXA".as_bytes());
    assert_parse!(Mnemonic::TXA, result);

    let result = mnemonic("TXS".as_bytes());
    assert_parse!(Mnemonic::TXS, result);

    let result = mnemonic("TYA".as_bytes());
    assert_parse!(Mnemonic::TYA, result);
}

#[test]
fn parse_accumulator() {
    let result = addressing_mode("A".as_bytes());
    assert_parse!(AddressingMode::Accumulator, result);
}

#[test]
fn parse_immediate_hex() {
    let result = addressing_mode("#$1".as_bytes());
    assert_parse!(AddressingMode::Immediate(0x1, Sign::Inferred), result);

    let result = addressing_mode("#$10".as_bytes());
    assert_parse!(AddressingMode::Immediate(0x10, Sign::Inferred), result);

    let result = addressing_mode("#$ff".as_bytes());
    assert_parse!(AddressingMode::Immediate(0xff, Sign::Inferred), result);

    let result = addressing_mode("#$100".as_bytes());
    assert_parse_fail!(result);
}

#[test]
fn parse_immediate_dec() {
    let result = addressing_mode("#1".as_bytes());
    assert_parse!(AddressingMode::Immediate(1, Sign::Inferred), result);

    let result = addressing_mode("#10".as_bytes());
    assert_parse!(AddressingMode::Immediate(10, Sign::Inferred), result);

    let result = addressing_mode("#255".as_bytes());
    assert_parse!(AddressingMode::Immediate(255, Sign::Inferred), result);

    let result = addressing_mode("#-10".as_bytes());
    assert_parse!(AddressingMode::Immediate(10, Sign::Negative), result);

    let result = addressing_mode("#256".as_bytes());
    assert_parse_fail!(result);
}

#[test]
#[ignore]
fn parse_zero_page_or_relative_hex() {
    let result = addressing_mode("$ff".as_bytes());
    assert_parse!(AddressingMode::ZeroPageOrRelative(0xff, Sign::Inferred), result);

    let result = addressing_mode("$0".as_bytes());
    assert_parse!(AddressingMode::ZeroPageOrRelative(0x0, Sign::Inferred), result);

    let result = addressing_mode("$10".as_bytes());
    assert_parse!(AddressingMode::ZeroPageOrRelative(0x10, Sign::Inferred), result);
}

#[test]
#[ignore]
fn parse_zero_page_or_relative_dec() {
    let result = addressing_mode("255".as_bytes());
    assert_parse!(AddressingMode::ZeroPageOrRelative(255, Sign::Inferred), result);

    let result = addressing_mode("0".as_bytes());
    assert_parse!(AddressingMode::ZeroPageOrRelative(0, Sign::Inferred), result);

    let result = addressing_mode("10".as_bytes());
    assert_parse!(AddressingMode::ZeroPageOrRelative(10, Sign::Inferred), result);

    let result = addressing_mode("-10".as_bytes());
    assert_parse!(AddressingMode::ZeroPageOrRelative(10, Sign::Negative), result);
}

#[test]
fn parse_zero_page_x_hex() {
    let result = addressing_mode("$ff,X".as_bytes());
    assert_parse!(AddressingMode::ZeroPageX(0xff), result);

    let result = addressing_mode("$0,X".as_bytes());
    assert_parse!(AddressingMode::ZeroPageX(0x0), result);

    let result = addressing_mode("$10,X".as_bytes());
    assert_parse!(AddressingMode::ZeroPageX(0x10), result);
}

#[test]
fn parse_zero_page_x_dec() {
    let result = addressing_mode("255,X".as_bytes());
    assert_parse!(AddressingMode::ZeroPageX(255), result);

    let result = addressing_mode("0,X".as_bytes());
    assert_parse!(AddressingMode::ZeroPageX(0), result);

    let result = addressing_mode("10,X".as_bytes());
    assert_parse!(AddressingMode::ZeroPageX(10), result);
}

#[test]
fn parse_zero_page_y_hex() {
    let result = addressing_mode("$ff,Y".as_bytes());
    assert_parse!(AddressingMode::ZeroPageY(0xff), result);

    let result = addressing_mode("$0,Y".as_bytes());
    assert_parse!(AddressingMode::ZeroPageY(0x0), result);

    let result = addressing_mode("$10,Y".as_bytes());
    assert_parse!(AddressingMode::ZeroPageY(0x10), result);
}

#[test]
fn parse_zero_page_y_dec() {
    let result = addressing_mode("255,Y".as_bytes());
    assert_parse!(AddressingMode::ZeroPageY(255), result);

    let result = addressing_mode("0,Y".as_bytes());
    assert_parse!(AddressingMode::ZeroPageY(0), result);

    let result = addressing_mode("10,Y".as_bytes());
    assert_parse!(AddressingMode::ZeroPageY(10), result);
}

#[test]
#[ignore]
fn parse_absolute_hex() {
    let result = addressing_mode("$ffff".as_bytes());
    assert_parse!(AddressingMode::Absolute(0xffff), result);

    let result = addressing_mode("$1000".as_bytes());
    assert_parse!(AddressingMode::Absolute(0x1000), result);

    let result = addressing_mode("$100".as_bytes());
    assert_parse!(AddressingMode::Absolute(0x100), result);
}

#[test]
#[ignore]
fn parse_absolute_dec() {
    let result = addressing_mode("65535".as_bytes());
    assert_parse!(AddressingMode::Absolute(65535), result);

    let result = addressing_mode("1000".as_bytes());
    assert_parse!(AddressingMode::Absolute(1000), result);

    let result = addressing_mode("256".as_bytes());
    assert_parse!(AddressingMode::Absolute(256), result);
}

#[test]
fn parse_absolute_x_hex() {
    let result = addressing_mode("$ffff,X".as_bytes());
    assert_parse!(AddressingMode::AbsoluteX(0xffff), result);

    let result = addressing_mode("$1000,X".as_bytes());
    assert_parse!(AddressingMode::AbsoluteX(0x1000), result);

    let result = addressing_mode("$100,X".as_bytes());
    assert_parse!(AddressingMode::AbsoluteX(0x100), result);
}

#[test]
fn parse_absolute_x_dec() {
    let result = addressing_mode("65535,X".as_bytes());
    assert_parse!(AddressingMode::AbsoluteX(65535), result);

    let result = addressing_mode("1000,X".as_bytes());
    assert_parse!(AddressingMode::AbsoluteX(1000), result);

    let result = addressing_mode("256,X".as_bytes());
    assert_parse!(AddressingMode::AbsoluteX(256), result);
}

#[test]
fn parse_absolute_y_hex() {
    let result = addressing_mode("$ffff,Y".as_bytes());
    assert_parse!(AddressingMode::AbsoluteY(0xffff), result);

    let result = addressing_mode("$1000,Y".as_bytes());
    assert_parse!(AddressingMode::AbsoluteY(0x1000), result);

    let result = addressing_mode("$100,Y".as_bytes());
    assert_parse!(AddressingMode::AbsoluteY(0x100), result);
}

#[test]
fn parse_absolute_y_dec() {
    let result = addressing_mode("65535,Y".as_bytes());
    assert_parse!(AddressingMode::AbsoluteY(65535), result);

    let result = addressing_mode("1000,Y".as_bytes());
    assert_parse!(AddressingMode::AbsoluteY(1000), result);

    let result = addressing_mode("256,Y".as_bytes());
    assert_parse!(AddressingMode::AbsoluteY(256), result);
}
