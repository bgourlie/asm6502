#![allow(dead_code)]

#[cfg(test)]
mod parse_tests;

#[macro_use]
extern crate nom;

use nom::{ErrorKind, IResult, line_ending, space};

#[derive(Debug, PartialEq, Eq)]
pub enum Mnemonic {
    Adc,
    And,
    Asl,
    Bcc,
    Bcs,
    Beq,
    Bit,
    Bmi,
    Bne,
    Bpl,
    Brk,
    Bvc,
    Bvs,
    Clc,
    Cld,
    Cli,
    Clv,
    Cmp,
    Cpx,
    Cpy,
    Dec,
    Dex,
    Dey,
    Eor,
    Inc,
    Inx,
    Iny,
    Jmp,
    Jsr,
    Lda,
    Ldx,
    Ldy,
    Lsr,
    Nop,
    Ora,
    Pha,
    Php,
    Pla,
    Plp,
    Rol,
    Ror,
    Rti,
    Rts,
    Sbc,
    Sec,
    Sed,
    Sei,
    Sta,
    Stx,
    Sty,
    Tax,
    Tay,
    Tsx,
    Txa,
    Txs,
    Tya,
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

named!(pub parse_lines <Vec<OpCode> >, separated_list!(line_ending, opcode));

named!(opcode <OpCode>, do_parse!(
        mnemonic: mnemonic >>
        space >>
        am: addressing_mode >>
        (OpCode(mnemonic, am))
    )
);

named!(mnemonic <Mnemonic>, alt!(
        tag!("ADC") => { |_| Mnemonic::Adc } |
        tag!("AND") => { |_| Mnemonic::And } |
        tag!("ASL") => { |_| Mnemonic::Asl } |
        tag!("BCC") => { |_| Mnemonic::Bcc } |
        tag!("BCS") => { |_| Mnemonic::Bcs } |
        tag!("BEQ") => { |_| Mnemonic::Beq } |
        tag!("BIT") => { |_| Mnemonic::Bit } |
        tag!("BMI") => { |_| Mnemonic::Bmi } |
        tag!("BNE") => { |_| Mnemonic::Bne } |
        tag!("BPL") => { |_| Mnemonic::Bpl } |
        tag!("BRK") => { |_| Mnemonic::Brk } |
        tag!("BVC") => { |_| Mnemonic::Bvc } |
        tag!("BVS") => { |_| Mnemonic::Bvs } |
        tag!("CLC") => { |_| Mnemonic::Clc } |
        tag!("CLD") => { |_| Mnemonic::Cld } |
        tag!("CLI") => { |_| Mnemonic::Cli } |
        tag!("CLV") => { |_| Mnemonic::Clv } |
        tag!("CMP") => { |_| Mnemonic::Cmp } |
        tag!("CPX") => { |_| Mnemonic::Cpx } |
        tag!("CPY") => { |_| Mnemonic::Cpy } |
        tag!("DEC") => { |_| Mnemonic::Dec } |
        tag!("DEX") => { |_| Mnemonic::Dex } |
        tag!("DEY") => { |_| Mnemonic::Dey } |
        tag!("EOR") => { |_| Mnemonic::Eor } |
        tag!("INC") => { |_| Mnemonic::Inc } |
        tag!("INX") => { |_| Mnemonic::Inx } |
        tag!("INY") => { |_| Mnemonic::Iny } |
        tag!("JMP") => { |_| Mnemonic::Jmp } |
        tag!("JSR") => { |_| Mnemonic::Jsr } |
        tag!("LDA") => { |_| Mnemonic::Lda } |
        tag!("LDX") => { |_| Mnemonic::Ldx } |
        tag!("LDY") => { |_| Mnemonic::Ldy } |
        tag!("LSR") => { |_| Mnemonic::Lsr } |
        tag!("NOP") => { |_| Mnemonic::Nop } |
        tag!("ORA") => { |_| Mnemonic::Ora } |
        tag!("PHA") => { |_| Mnemonic::Pha } |
        tag!("PHP") => { |_| Mnemonic::Php } |
        tag!("PLA") => { |_| Mnemonic::Pla } |
        tag!("PLP") => { |_| Mnemonic::Plp } |
        tag!("ROL") => { |_| Mnemonic::Rol } |
        tag!("ROR") => { |_| Mnemonic::Ror } |
        tag!("RTI") => { |_| Mnemonic::Rti } |
        tag!("RTS") => { |_| Mnemonic::Rts } |
        tag!("SBC") => { |_| Mnemonic::Sbc } |
        tag!("SEC") => { |_| Mnemonic::Sec } |
        tag!("SED") => { |_| Mnemonic::Sed } |
        tag!("SEI") => { |_| Mnemonic::Sei } |
        tag!("STA") => { |_| Mnemonic::Sta } |
        tag!("STX") => { |_| Mnemonic::Stx } |
        tag!("STY") => { |_| Mnemonic::Sty } |
        tag!("TAX") => { |_| Mnemonic::Tax } |
        tag!("TAY") => { |_| Mnemonic::Tay } |
        tag!("TSX") => { |_| Mnemonic::Tsx } |
        tag!("TXA") => { |_| Mnemonic::Txa } |
        tag!("TXS") => { |_| Mnemonic::Txs } |
        tag!("TYA") => { |_| Mnemonic::Tya }
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
        am_abs |
        am_implied
    )
);

named!(am_implied <AddressingMode>,
   do_parse!(
       line_ending >>
       (AddressingMode::Implied)
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
    match is_a!(input, &b"0123456789abcdefABCDEF"[..]) {
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
    match is_a!(input, &b"0123456789abcdefABCDEF"[..]) {
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
