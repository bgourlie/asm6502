#[macro_use]
extern crate nom;

use nom::{newline, eol};

use std::str;
use std::collections::HashMap;

#[derive(Debug)]
enum Mnemonic {
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

#[derive(Debug)]
enum AddressingMode {
    IndexedIndirect(u8),
    IndirectIndexed(u8),
    ZeroPage(u8),
    Immediate(u8),
    Absolute(u16),
    AbsoluteX(u16),
    AbsoluteY(u16),
    ZeroPageX(u8),
    Relative(i8),
    Implied,
    Accumulator,
}

#[derive(Debug)]
pub struct OpCode(Mnemonic, AddressingMode);

named!(opcode <OpCode>,
    do_parse!(
        mnemonic: mnemonic >>
        am: addressing_mode >>
        (OpCode(mnemonic, am))
    )
);

named!(pub parse<Vec<OpCode> >, many0!(opcode));

named!(addressing_mode <AddressingMode>,
    ws!(
        alt!(
            tag!("A") => { |_| AddressingMode::Accumulator } |
            tag!("#") => { |_| AddressingMode::Immediate(1) }
        )
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
