#![allow(dead_code)]

#[cfg(test)]
mod parse_tests;

#[macro_use]
extern crate nom;

use nom::{ErrorKind, IResult, space};

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

#[derive(Debug, PartialEq, Eq)]
pub enum Sign {
    Implied,
    Negative,
}

#[derive(Debug, PartialEq, Eq)]
pub enum AddressingMode {
    IndexedIndirect(u8),
    IndirectIndexed(u8),
    ZeroPageOrRelative(u8, Sign),
    Immediate(u8, Sign),
    Absolute(u16),
    AbsoluteX(u16),
    AbsoluteY(u16),
    ZeroPageX(u8),
    ZeroPageY(u8),
    Indirect(u16),
    Implied,
    Accumulator,
}

#[derive(Debug, PartialEq, Eq)]
pub struct OpCode(Mnemonic, AddressingMode);

named!(opcode <OpCode>, do_parse!(
        mnemonic: mnemonic >>
        space >>
        am: addressing_mode >>
        (OpCode(mnemonic, am))
    )
);

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

named!(addressing_mode <AddressingMode>,
    alt_complete!(
        am_accumulator |
        am_immediate |
        am_indirect |
        am_indexed_indirect |
        am_indirect_indexed |
        am_zp_x |
        am_zp_y |
        am_zp_or_relative |
        am_abs_x |
        am_abs_y |
        am_abs
    )
);

named!(am_indirect <AddressingMode>,
    do_parse!(
        word: delimited!(tag!("("), alt!(parse_word_hex | dec_u16), tag!(")")) >>
        not!(tag!(",")) >>
        (AddressingMode::Indirect(word))
    )
);

named!(am_indexed_indirect <AddressingMode>,
    do_parse!(
        byte: delimited!(tag!("("), alt!(parse_byte_hex | parse_byte_dec), tag!(",X")) >>
        ({ let (addr, _) = byte; AddressingMode::IndexedIndirect(addr) })
    )
);

named!(am_indirect_indexed <AddressingMode>,
    do_parse!(
        byte: delimited!(tag!("("), alt!(parse_byte_hex | parse_byte_dec), tag!("),Y")) >>
        ({ let (addr, _) = byte; AddressingMode::IndirectIndexed(addr) })
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
        val: preceded!(tag!("#"), alt!(parse_byte_hex | parse_byte_dec)) >>
        ({ let (byte, sign) = val; AddressingMode::Immediate(byte, sign)})
    )
);

named!(am_abs <AddressingMode>,
    do_parse!(
        val: alt!(parse_word_hex | dec_u16) >>
        (AddressingMode::Absolute(val))
    )
);

named!(am_zp_or_relative <AddressingMode>,
    do_parse!(
        val: alt!(parse_byte_hex | parse_byte_dec) >>
        ({ let (byte, sign) = val; AddressingMode::ZeroPageOrRelative(byte, sign)})
    )
);

named!(am_zp_x <AddressingMode>,
    do_parse!(
        val: terminated!(alt!(parse_byte_hex | parse_byte_dec), tag!(",X")) >>
        ({ let (byte, _) = val; AddressingMode::ZeroPageX(byte)})
    )
);

named!(am_zp_y <AddressingMode>,
    do_parse!(
        val: terminated!(alt!(parse_byte_hex | parse_byte_dec), tag!(",Y")) >>
        ({ let (byte, _) = val; AddressingMode::ZeroPageY(byte)})
    )
);

named!(am_abs_x <AddressingMode>,
    do_parse!(
        val: terminated!(alt!(parse_word_hex | dec_u16), tag!(",X")) >>
        (AddressingMode::AbsoluteX(val))
    )
);

named!(am_abs_y <AddressingMode>,
    do_parse!(
        val: terminated!(alt!(parse_word_hex | dec_u16), tag!(",Y")) >>
        (AddressingMode::AbsoluteY(val))
    )
);

named!(parse_word_hex <u16>,
    do_parse!(
        val: preceded!(tag!("$"), hex_u16) >>
        (val)
    )
);

named!(parse_byte_hex <(u8, Sign)>,
    do_parse!(
        val: preceded!(tag!("$"), hex_u8) >>
        (val, Sign::Implied)
    )
);

named!(parse_byte_dec <(u8, Sign)>,
    do_parse!(
        sign: parse_sign >>
        val: dec_u8 >>
        (val, sign)
    )
);

named!(parse_sign <Sign>,
    do_parse!(
        sign: opt!(tag!("-")) >>
        (if let Some(_) = sign {
            Sign::Negative
        } else {
            Sign::Implied
        })
    )
);


pub fn hex_u16(input: &[u8]) -> IResult<&[u8], u16> {
    match is_a!(input, &b"0123456789abcdef"[..]) {
        IResult::Error(e) => IResult::Error(e),
        IResult::Incomplete(e) => IResult::Incomplete(e),
        IResult::Done(i, o) => {
            let mut res = 0u16;

            // Do not parse more than 4 characters for a u16
            let mut remaining = i;
            let mut parsed = o;
            if o.len() > 4 {
                remaining = &input[4..];
                parsed = &input[..4];
            }

            for &e in parsed {
                let digit = e as char;
                let value = digit.to_digit(16).unwrap_or(0) as u16;
                res = value + (res << 4);
            }
            IResult::Done(remaining, res)
        }
    }
}

pub fn dec_u16(input: &[u8]) -> IResult<&[u8], u16> {
    match is_a!(input, &b"0123456789"[..]) {
        IResult::Error(e) => IResult::Error(e),
        IResult::Incomplete(e) => IResult::Incomplete(e),
        IResult::Done(remaining, parsed) => {
            // Do not parse more than 5 characters for a u16
            if parsed.len() > 5 {
                IResult::Error(ErrorKind::Custom(0))
            } else {
                let mut res = 0u32;
                for &e in parsed {
                    let digit = e as char;
                    let value = digit.to_digit(10).unwrap_or(0) as u32;
                    res = value + (res * 10);
                }
                if res > u16::max_value() as u32 {
                    IResult::Error(ErrorKind::Custom(0))
                } else {
                    IResult::Done(remaining, res as u16)
                }
            }
        }
    }
}

pub fn dec_u8(input: &[u8]) -> IResult<&[u8], u8> {
    match is_a!(input, &b"0123456789"[..]) {
        IResult::Error(e) => IResult::Error(e),
        IResult::Incomplete(e) => IResult::Incomplete(e),
        IResult::Done(remaining, parsed) => {
            // Do not parse more than 3 characters for a u16
            if parsed.len() > 3 {
                IResult::Error(ErrorKind::Custom(0))
            } else {
                let mut res = 0u16;
                for &e in parsed {
                    let digit = e as char;
                    let value = digit.to_digit(10).unwrap_or(0) as u16;
                    res = value + (res * 10);
                }
                if res > u8::max_value() as u16 {
                    IResult::Error(ErrorKind::Custom(0))
                } else {
                    IResult::Done(remaining, res as u8)
                }
            }
        }
    }
}

fn hex_u8(input: &[u8]) -> IResult<&[u8], u8> {
    match is_a!(input, &b"0123456789abcdef"[..]) {
        IResult::Error(e) => IResult::Error(e),
        IResult::Incomplete(e) => IResult::Incomplete(e),
        IResult::Done(remaining, parsed) => {
            // Not valid if exceeds 2 characters
            if parsed.len() > 2 {
                IResult::Error(ErrorKind::Custom(0))
            } else {
                let mut res = 0u8;
                for &e in parsed {
                    let digit = e as char;
                    let value = digit.to_digit(16).unwrap_or(0) as u8;
                    res = value + (res << 4);
                }
                IResult::Done(remaining, res)
            }

        }
    }
}
