#[cfg(test)]
mod parse_tests;

#[macro_use]
extern crate nom;

use nom::{ErrorKind, IResult, digit, hex_digit};
use std::str::{FromStr, from_utf8};

#[derive(Debug, PartialEq, Eq)]
pub enum Mnemonic {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
}

enum Sign {
    Inferred,
    Positive,
    Negative,
}

#[derive(Debug, PartialEq, Eq)]
pub enum AddressingMode {
    IndexedIndirect(u8),
    IndirectIndexed(u8),
    ZeroPageOrRelative(u8),
    Immediate(u8),
    Absolute(u16),
    AbsoluteX(u16),
    AbsoluteY(u16),
    ZeroPageX(u8),
    ZeroPageY(u8),
    Implied,
    Accumulator,
}

#[derive(Debug, PartialEq, Eq)]
pub struct OpCode(Mnemonic, AddressingMode);

named!(opcode <OpCode>,
    do_parse!(
        mnemonic: mnemonic >>
        am: addressing_mode >>
        (OpCode(mnemonic, am))
    )
);

named!(addressing_mode <AddressingMode>,
    alt!(
        am_accumulator |
        am_immediate |
        am_abs_x |
        am_abs_y |
        am_abs |
        am_zp_x |
        am_zp_y |
        am_zp_or_relative
    )
);

named!(am_accumulator <AddressingMode>,
    do_parse!(
        tag!("A") >>
        (AddressingMode::Accumulator)
    )
);

named!(am_immediate <AddressingMode>,
    do_parse!(
        tag!("#") >>
        val: alt!(parse_byte_hex | parse_byte_dec) >>
        (AddressingMode::Immediate(val))
    )
);

named!(am_abs <AddressingMode>,
    do_parse!(
        val: alt!(parse_word_hex | parse_word_dec) >>
        (AddressingMode::Absolute(val))
    )
);

named!(am_zp_or_relative <AddressingMode>,
    do_parse!(
        val: alt!(parse_byte_hex | parse_byte_dec) >>
        (AddressingMode::ZeroPageOrRelative(val))
    )
);

named!(am_zp_x <AddressingMode>,
    do_parse!(
        val: alt!(parse_byte_hex | parse_byte_dec) >>
        tag!(",X") >>
        (AddressingMode::ZeroPageX(val))
    )
);

named!(am_zp_y <AddressingMode>,
    do_parse!(
        val: alt!(parse_byte_hex | parse_byte_dec) >>
        tag!(",Y") >>
        (AddressingMode::ZeroPageY(val))
    )
);

named!(am_abs_x <AddressingMode>,
    do_parse!(
        val: alt!(parse_word_hex | parse_word_dec) >>
        tag!(",X") >>
        (AddressingMode::AbsoluteX(val))
    )
);

named!(am_abs_y <AddressingMode>,
    do_parse!(
        val: alt!(parse_word_hex | parse_word_dec) >>
        tag!(",Y") >>
        (AddressingMode::AbsoluteY(val))
    )
);

named!(parse_hex <usize>,
    do_parse!(
        tag!("$") >>
        val: hex_digit >>
        (usize::from_str_radix(from_utf8(val).unwrap(), 16).unwrap())
    )
);

named!(parse_dec <usize>,
    do_parse!(
        val: digit >>
        (usize::from_str(from_utf8(val).unwrap()).unwrap())
    )
);

fn parse_byte_hex(i: &[u8]) -> IResult<&[u8], u8> {
    match parse_hex(i) {
        IResult::Done(rest, value) => {
            if value <= u8::max_value() as usize {
                IResult::Done(rest, value as u8)
            } else {
                IResult::Error(ErrorKind::Custom(0))
            }
        }
        IResult::Incomplete(needed) => IResult::Incomplete(needed),
        IResult::Error(e) => IResult::Error(e),
    }
}

fn parse_word_hex(i: &[u8]) -> IResult<&[u8], u16> {
    match parse_hex(i) {
        IResult::Done(rest, value) => {
            if value <= u8::max_value() as usize {
                IResult::Error(ErrorKind::Custom(0)) // Should be byte
            } else if value <= u16::max_value() as usize {
                IResult::Done(rest, value as u16)
            } else {
                IResult::Error(ErrorKind::Custom(0))
            }
        }
        IResult::Incomplete(needed) => IResult::Incomplete(needed),
        IResult::Error(e) => IResult::Error(e),
    }
}

fn parse_word_dec(i: &[u8]) -> IResult<&[u8], u16> {
    match parse_dec(i) {
        IResult::Done(rest, value) => {
            if value <= u8::max_value() as usize {
                IResult::Error(ErrorKind::Custom(0)) // Should be byte
            } else if value <= u16::max_value() as usize {
                IResult::Done(rest, value as u16)
            } else {
                IResult::Error(ErrorKind::Custom(0))
            }
        }
        IResult::Incomplete(needed) => IResult::Incomplete(needed),
        IResult::Error(e) => IResult::Error(e),
    }
}

fn parse_byte_dec(i: &[u8]) -> IResult<&[u8], u8> {
    match parse_dec(i) {
        IResult::Done(rest, value) => {
            if value <= u8::max_value() as usize {
                IResult::Done(rest, value as u8)
            } else {
                IResult::Error(ErrorKind::Custom(0))
            }
        }
        IResult::Incomplete(needed) => IResult::Incomplete(needed),
        IResult::Error(e) => IResult::Error(e),
    }
}

named!(mnemonic <Mnemonic>, alt!(
        tag!("ADC") => { |_| Mnemonic::ADC } |
        tag!("AND") => { |_| Mnemonic::AND } |
        tag!("ASL") => { |_| Mnemonic::ASL } |
        tag!("BCC") => { |_| Mnemonic::BCC } |
        tag!("BCS") => { |_| Mnemonic::BCS } |
        tag!("BEQ") => { |_| Mnemonic::BEQ } |
        tag!("BIT") => { |_| Mnemonic::BIT } |
        tag!("BMI") => { |_| Mnemonic::BMI } |
        tag!("BNE") => { |_| Mnemonic::BNE } |
        tag!("BPL") => { |_| Mnemonic::BPL } |
        tag!("BRK") => { |_| Mnemonic::BRK } |
        tag!("BVC") => { |_| Mnemonic::BVC } |
        tag!("BVS") => { |_| Mnemonic::BVS } |
        tag!("CLC") => { |_| Mnemonic::CLC } |
        tag!("CLD") => { |_| Mnemonic::CLD } |
        tag!("CLI") => { |_| Mnemonic::CLI } |
        tag!("CLV") => { |_| Mnemonic::CLV } |
        tag!("CMP") => { |_| Mnemonic::CMP } |
        tag!("CPX") => { |_| Mnemonic::CPX } |
        tag!("CPY") => { |_| Mnemonic::CPY } |
        tag!("DEC") => { |_| Mnemonic::DEC } |
        tag!("DEX") => { |_| Mnemonic::DEX } |
        tag!("DEY") => { |_| Mnemonic::DEY } |
        tag!("EOR") => { |_| Mnemonic::EOR } |
        tag!("INC") => { |_| Mnemonic::INC } |
        tag!("INX") => { |_| Mnemonic::INX } |
        tag!("INY") => { |_| Mnemonic::INY } |
        tag!("JMP") => { |_| Mnemonic::JMP } |
        tag!("JSR") => { |_| Mnemonic::JSR } |
        tag!("LDA") => { |_| Mnemonic::LDA } |
        tag!("LDX") => { |_| Mnemonic::LDX } |
        tag!("LDY") => { |_| Mnemonic::LDY } |
        tag!("LSR") => { |_| Mnemonic::LSR } |
        tag!("NOP") => { |_| Mnemonic::NOP } |
        tag!("ORA") => { |_| Mnemonic::ORA } |
        tag!("PHA") => { |_| Mnemonic::PHA } |
        tag!("PHP") => { |_| Mnemonic::PHP } |
        tag!("PLA") => { |_| Mnemonic::PLA } |
        tag!("PLP") => { |_| Mnemonic::PLP } |
        tag!("ROL") => { |_| Mnemonic::ROL } |
        tag!("ROR") => { |_| Mnemonic::ROR } |
        tag!("RTI") => { |_| Mnemonic::RTI } |
        tag!("RTS") => { |_| Mnemonic::RTS } |
        tag!("SBC") => { |_| Mnemonic::SBC } |
        tag!("SEC") => { |_| Mnemonic::SEC } |
        tag!("SED") => { |_| Mnemonic::SED } |
        tag!("SEI") => { |_| Mnemonic::SEI } |
        tag!("STA") => { |_| Mnemonic::STA } |
        tag!("STX") => { |_| Mnemonic::STX } |
        tag!("STY") => { |_| Mnemonic::STY } |
        tag!("TAX") => { |_| Mnemonic::TAX } |
        tag!("TAY") => { |_| Mnemonic::TAY } |
        tag!("TSX") => { |_| Mnemonic::TSX } |
        tag!("TXA") => { |_| Mnemonic::TXA } |
        tag!("TXS") => { |_| Mnemonic::TXS } |
        tag!("TYA") => { |_| Mnemonic::TYA }
        )
    );
