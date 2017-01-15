use nom::IResult;
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

macro_rules! assert_am_parse {
    ( $ input : expr , $ expected : expr ) => {
        let result = addressing_mode($input.as_bytes());
        assert_parse!($expected, result);
    };
}

macro_rules! assert_opcode_parse {
    ( $ input : expr , $ expected : expr ) => {
        assert_parse!($expected, opcode($input.as_bytes()));
    };
}

macro_rules! assert_mnemonic_parse {
    ( $ input : expr , $ expected : expr ) => {
        let result = mnemonic($input.as_bytes());
        assert_parse!($expected, result);
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
    assert_mnemonic_parse!("ADC", Mnemonic::ADC);
    assert_mnemonic_parse!("AND", Mnemonic::AND);
    assert_mnemonic_parse!("ASL", Mnemonic::ASL);
    assert_mnemonic_parse!("BCC", Mnemonic::BCC);
    assert_mnemonic_parse!("BCS", Mnemonic::BCS);
    assert_mnemonic_parse!("BEQ", Mnemonic::BEQ);
    assert_mnemonic_parse!("BIT", Mnemonic::BIT);
    assert_mnemonic_parse!("BMI", Mnemonic::BMI);
    assert_mnemonic_parse!("BNE", Mnemonic::BNE);
    assert_mnemonic_parse!("BPL", Mnemonic::BPL);
    assert_mnemonic_parse!("BRK", Mnemonic::BRK);
    assert_mnemonic_parse!("BVC", Mnemonic::BVC);
    assert_mnemonic_parse!("BVS", Mnemonic::BVS);
    assert_mnemonic_parse!("CLC", Mnemonic::CLC);
    assert_mnemonic_parse!("CLD", Mnemonic::CLD);
    assert_mnemonic_parse!("CLI", Mnemonic::CLI);
    assert_mnemonic_parse!("CLV", Mnemonic::CLV);
    assert_mnemonic_parse!("CMP", Mnemonic::CMP);
    assert_mnemonic_parse!("CPX", Mnemonic::CPX);
    assert_mnemonic_parse!("CPY", Mnemonic::CPY);
    assert_mnemonic_parse!("DEC", Mnemonic::DEC);
    assert_mnemonic_parse!("DEX", Mnemonic::DEX);
    assert_mnemonic_parse!("DEY", Mnemonic::DEY);
    assert_mnemonic_parse!("EOR", Mnemonic::EOR);
    assert_mnemonic_parse!("INC", Mnemonic::INC);
    assert_mnemonic_parse!("INX", Mnemonic::INX);
    assert_mnemonic_parse!("INY", Mnemonic::INY);
    assert_mnemonic_parse!("JMP", Mnemonic::JMP);
    assert_mnemonic_parse!("JSR", Mnemonic::JSR);
    assert_mnemonic_parse!("LDA", Mnemonic::LDA);
    assert_mnemonic_parse!("LDX", Mnemonic::LDX);
    assert_mnemonic_parse!("LDY", Mnemonic::LDY);
    assert_mnemonic_parse!("LSR", Mnemonic::LSR);
    assert_mnemonic_parse!("NOP", Mnemonic::NOP);
    assert_mnemonic_parse!("ORA", Mnemonic::ORA);
    assert_mnemonic_parse!("PHA", Mnemonic::PHA);
    assert_mnemonic_parse!("PHP", Mnemonic::PHP);
    assert_mnemonic_parse!("PLA", Mnemonic::PLA);
    assert_mnemonic_parse!("PLP", Mnemonic::PLP);
    assert_mnemonic_parse!("ROL", Mnemonic::ROL);
    assert_mnemonic_parse!("ROR", Mnemonic::ROR);
    assert_mnemonic_parse!("RTI", Mnemonic::RTI);
    assert_mnemonic_parse!("RTS", Mnemonic::RTS);
    assert_mnemonic_parse!("SBC", Mnemonic::SBC);
    assert_mnemonic_parse!("SEC", Mnemonic::SEC);
    assert_mnemonic_parse!("SED", Mnemonic::SED);
    assert_mnemonic_parse!("SEI", Mnemonic::SEI);
    assert_mnemonic_parse!("STA", Mnemonic::STA);
    assert_mnemonic_parse!("STX", Mnemonic::STX);
    assert_mnemonic_parse!("STY", Mnemonic::STY);
    assert_mnemonic_parse!("TAX", Mnemonic::TAX);
    assert_mnemonic_parse!("TAY", Mnemonic::TAY);
    assert_mnemonic_parse!("TSX", Mnemonic::TSX);
    assert_mnemonic_parse!("TXA", Mnemonic::TXA);
    assert_mnemonic_parse!("TXS", Mnemonic::TXS);
    assert_mnemonic_parse!("TYA", Mnemonic::TYA);
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
    assert_parse_fail!(addressing_mode("#$100".as_bytes()));
}

#[test]
fn parse_immediate_dec() {
    assert_am_parse!("#1", AddressingMode::Immediate(1, Sign::Implied));
    assert_am_parse!("#10", AddressingMode::Immediate(10, Sign::Implied));
    assert_am_parse!("#255", AddressingMode::Immediate(255, Sign::Implied));
    assert_am_parse!("#-10", AddressingMode::Immediate(10, Sign::Negative));
    assert_parse_fail!(addressing_mode("#256".as_bytes()));
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
    assert_am_parse!("$ff",
                     AddressingMode::ZeroPageOrRelative(0xff, Sign::Implied));
    assert_am_parse!("$0", AddressingMode::ZeroPageOrRelative(0x0, Sign::Implied));
    assert_am_parse!("$10",
                     AddressingMode::ZeroPageOrRelative(0x10, Sign::Implied));
}

#[test]
fn parse_zero_page_or_relative_dec() {
    assert_am_parse!("255",
                     AddressingMode::ZeroPageOrRelative(255, Sign::Implied));
    assert_am_parse!("0", AddressingMode::ZeroPageOrRelative(0, Sign::Implied));
    assert_am_parse!("10", AddressingMode::ZeroPageOrRelative(10, Sign::Implied));
    assert_am_parse!("-10",
                     AddressingMode::ZeroPageOrRelative(10, Sign::Negative));
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
fn parse_opcode() {
    assert_opcode_parse!("ADC #1",
                         OpCode(Mnemonic::ADC, AddressingMode::Immediate(1, Sign::Implied)))
}
