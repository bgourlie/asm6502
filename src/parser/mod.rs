#[cfg(test)]
mod tests;

use crate::tokens::*;
use nom::error::ErrorKind;
use nom::IResult;
use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case},
    character::complete::{line_ending, space1},
    combinator::{eof, map, opt, value},
    multi::separated_list0,
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
};

pub fn parse_lines(input: &[u8]) -> IResult<&[u8], Vec<OpCode>> {
    separated_list0(line_ending, opcode)(input)
}

fn opcode(input: &[u8]) -> IResult<&[u8], OpCode> {
    let (input, (mnemonic, am)) =
        separated_pair(mnemonic, alt((space1, line_ending, eof)), addressing_mode)(input)?;
    Ok((input, OpCode(mnemonic, am)))
}

fn mnemonic(input: &[u8]) -> IResult<&[u8], Mnemonic> {
    alt((
        alt((
            value(Mnemonic::Adc, tag_no_case("ADC")),
            value(Mnemonic::And, tag_no_case("AND")),
            value(Mnemonic::Asl, tag_no_case("ASL")),
            value(Mnemonic::Bcc, tag_no_case("BCC")),
            value(Mnemonic::Bcs, tag_no_case("BCS")),
            value(Mnemonic::Beq, tag_no_case("BEQ")),
            value(Mnemonic::Bit, tag_no_case("BIT")),
            value(Mnemonic::Bmi, tag_no_case("BMI")),
            value(Mnemonic::Bne, tag_no_case("BNE")),
            value(Mnemonic::Bpl, tag_no_case("BPL")),
            value(Mnemonic::Brk, tag_no_case("BRK")),
            value(Mnemonic::Bvc, tag_no_case("BVC")),
            value(Mnemonic::Bvs, tag_no_case("BVS")),
            value(Mnemonic::Clc, tag_no_case("CLC")),
            value(Mnemonic::Cld, tag_no_case("CLD")),
            value(Mnemonic::Cli, tag_no_case("CLI")),
            value(Mnemonic::Clv, tag_no_case("CLV")),
            value(Mnemonic::Cmp, tag_no_case("CMP")),
            value(Mnemonic::Cpx, tag_no_case("CPX")),
            value(Mnemonic::Cpy, tag_no_case("CPY")),
            value(Mnemonic::Dec, tag_no_case("DEC")),
        )),
        alt((
            value(Mnemonic::Dex, tag_no_case("DEX")),
            value(Mnemonic::Dey, tag_no_case("DEY")),
            value(Mnemonic::Eor, tag_no_case("EOR")),
            value(Mnemonic::Inc, tag_no_case("INC")),
            value(Mnemonic::Inx, tag_no_case("INX")),
            value(Mnemonic::Iny, tag_no_case("INY")),
            value(Mnemonic::Jmp, tag_no_case("JMP")),
            value(Mnemonic::Jsr, tag_no_case("JSR")),
            value(Mnemonic::Lda, tag_no_case("LDA")),
            value(Mnemonic::Ldx, tag_no_case("LDX")),
            value(Mnemonic::Ldy, tag_no_case("LDY")),
            value(Mnemonic::Lsr, tag_no_case("LSR")),
            value(Mnemonic::Nop, tag_no_case("NOP")),
            value(Mnemonic::Ora, tag_no_case("ORA")),
            value(Mnemonic::Pha, tag_no_case("PHA")),
            value(Mnemonic::Php, tag_no_case("PHP")),
            value(Mnemonic::Pla, tag_no_case("PLA")),
            value(Mnemonic::Plp, tag_no_case("PLP")),
            value(Mnemonic::Rol, tag_no_case("ROL")),
            value(Mnemonic::Ror, tag_no_case("ROR")),
            value(Mnemonic::Rti, tag_no_case("RTI")),
        )),
        alt((
            value(Mnemonic::Rts, tag_no_case("RTS")),
            value(Mnemonic::Sbc, tag_no_case("SBC")),
            value(Mnemonic::Sec, tag_no_case("SEC")),
            value(Mnemonic::Sed, tag_no_case("SED")),
            value(Mnemonic::Sei, tag_no_case("SEI")),
            value(Mnemonic::Sta, tag_no_case("STA")),
            value(Mnemonic::Stx, tag_no_case("STX")),
            value(Mnemonic::Sty, tag_no_case("STY")),
            value(Mnemonic::Tax, tag_no_case("TAX")),
            value(Mnemonic::Tay, tag_no_case("TAY")),
            value(Mnemonic::Tsx, tag_no_case("TSX")),
            value(Mnemonic::Txa, tag_no_case("TXA")),
            value(Mnemonic::Txs, tag_no_case("TXS")),
            value(Mnemonic::Tya, tag_no_case("TYA")),
        )),
    ))(input)
}

fn addressing_mode(input: &[u8]) -> IResult<&[u8], AddressingMode> {
    alt((
        am_accumulator,
        am_immediate,
        am_indirect,
        am_indexed_indirect,
        am_indirect_indexed,
        am_zp_x,
        am_zp_y,
        am_zp_or_relative,
        am_abs_x,
        am_abs_y,
        am_abs,
        am_implied,
    ))(input)
}

fn am_implied(input: &[u8]) -> IResult<&[u8], AddressingMode> {
    value(AddressingMode::Implied, alt((line_ending, eof)))(input)
}

fn am_indirect(input: &[u8]) -> IResult<&[u8], AddressingMode> {
    let parser = tuple((
        tag("("),
        alt((parse_word_hex, dec_u16)),
        tag(")"),
        nom::combinator::not(tag(",")),
    ));
    map(parser, |(_, val, _, _)| AddressingMode::Indirect(val))(input)
}

fn am_indexed_indirect(input: &[u8]) -> IResult<&[u8], AddressingMode> {
    let parser = delimited(
        tag("("),
        alt((parse_byte_hex, parse_byte_dec)),
        tag_no_case(",X"),
    );
    map(parser, |(addr, _)| AddressingMode::IndexedIndirect(addr))(input)
}

fn am_indirect_indexed(input: &[u8]) -> IResult<&[u8], AddressingMode> {
    let parser = delimited(
        tag("("),
        alt((parse_byte_hex, parse_byte_dec)),
        tag_no_case("),Y"),
    );
    map(parser, |(addr, _)| AddressingMode::IndirectIndexed(addr))(input)
}

fn am_accumulator(input: &[u8]) -> IResult<&[u8], AddressingMode> {
    map(tag_no_case("A"), |_| AddressingMode::Accumulator)(input)
}

fn am_immediate(input: &[u8]) -> IResult<&[u8], AddressingMode> {
    let parser = preceded(tag("#"), alt((parse_byte_hex, parse_byte_dec)));
    map(parser, |(byte, sign)| AddressingMode::Immediate(byte, sign))(input)
}

fn am_abs(input: &[u8]) -> IResult<&[u8], AddressingMode> {
    let parser = alt((parse_word_hex, dec_u16));
    map(parser, |val| AddressingMode::Absolute(val))(input)
}

fn am_zp_or_relative(input: &[u8]) -> IResult<&[u8], AddressingMode> {
    let parser = alt((parse_byte_hex, parse_byte_dec));
    map(parser, |(byte, sign)| {
        AddressingMode::ZeroPageOrRelative(byte, sign)
    })(input)
}

fn am_zp_x(input: &[u8]) -> IResult<&[u8], AddressingMode> {
    let parser = terminated(alt((parse_byte_hex, parse_byte_dec)), tag_no_case(",X"));
    map(parser, |(byte, _)| AddressingMode::ZeroPageX(byte))(input)
}

fn am_zp_y(input: &[u8]) -> IResult<&[u8], AddressingMode> {
    let parser = terminated(alt((parse_byte_hex, parse_byte_dec)), tag_no_case(",Y"));
    map(parser, |(byte, _)| AddressingMode::ZeroPageY(byte))(input)
}

fn am_abs_x(input: &[u8]) -> IResult<&[u8], AddressingMode> {
    let parser = terminated(alt((parse_word_hex, dec_u16)), tag_no_case(",X"));
    map(parser, |val| AddressingMode::AbsoluteX(val))(input)
}

fn am_abs_y(input: &[u8]) -> IResult<&[u8], AddressingMode> {
    let parser = terminated(alt((parse_word_hex, dec_u16)), tag_no_case(",Y"));
    map(parser, |val| AddressingMode::AbsoluteY(val))(input)
}

fn parse_word_hex(input: &[u8]) -> IResult<&[u8], u16> {
    preceded(tag("$"), hex_u16)(input)
}

fn parse_byte_hex(input: &[u8]) -> IResult<&[u8], (u8, Sign)> {
    map(preceded(tag("$"), hex_u8), |val| (val, Sign::Implied))(input)
}

fn parse_byte_dec(input: &[u8]) -> IResult<&[u8], (u8, Sign)> {
    map(tuple((parse_sign, dec_u8)), |(sign, val)| (val, sign))(input)
}

fn parse_sign(input: &[u8]) -> IResult<&[u8], Sign> {
    map(opt(tag("-")), |sign| {
        if sign.is_some() {
            Sign::Negative
        } else {
            Sign::Implied
        }
    })(input)
}

fn hex_u16(input: &[u8]) -> IResult<&[u8], u16> {
    let (i, o) = nom::bytes::complete::is_a(&b"0123456789abcdefABCDEF"[..])(input)?;
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
    IResult::Ok((remaining, res))
}

fn dec_u16(input: &[u8]) -> IResult<&[u8], u16> {
    let (remaining, parsed) = nom::bytes::complete::is_a(&b"0123456789"[..])(input)?;
    // Do not parse more than 5 characters for a u16
    if parsed.len() > 5 {
        IResult::Err(nom::Err::Error(nom::error::make_error(
            input,
            ErrorKind::TooLarge,
        )))
    } else {
        let mut res = 0u32;
        for &e in parsed {
            let digit = e as char;
            let value = digit.to_digit(10).unwrap_or(0) as u32;
            res = value + (res * 10);
        }
        if res > u16::max_value() as u32 {
            // TODO: propper error handling. ErrorKind::TooLarge probably is not
            // the best choice here. Some custome error?
            IResult::Err(nom::Err::Error(nom::error::make_error(
                input,
                ErrorKind::TooLarge,
            )))
        } else {
            IResult::Ok((remaining, res as u16))
        }
    }
}

fn dec_u8(input: &[u8]) -> IResult<&[u8], u8> {
    let (remaining, parsed) = nom::bytes::complete::is_a(&b"0123456789"[..])(input)?;
    // Do not parse more than 3 characters for a u16
    if parsed.len() > 3 {
        IResult::Err(nom::Err::Error(nom::error::make_error(
            input,
            ErrorKind::TooLarge,
        )))
    } else {
        let mut res = 0u16;
        for &e in parsed {
            let digit = e as char;
            let value = digit.to_digit(10).unwrap_or(0) as u16;
            res = value + (res * 10);
        }
        if res > u8::max_value() as u16 {
            IResult::Err(nom::Err::Error(nom::error::make_error(
                input,
                ErrorKind::TooLarge,
            )))
        } else {
            IResult::Ok((remaining, res as u8))
        }
    }
}

fn hex_u8(input: &[u8]) -> IResult<&[u8], u8> {
    let (remaining, parsed) = nom::bytes::complete::is_a(&b"0123456789abcdefABCDEF"[..])(input)?;
    // Not valid if exceeds 2 characters
    if parsed.len() > 2 {
        IResult::Err(nom::Err::Error(nom::error::make_error(
            input,
            ErrorKind::TooLarge,
        )))
    } else {
        let mut res = 0u8;
        for &e in parsed {
            let digit = e as char;
            let value = digit.to_digit(16).unwrap_or(0) as u8;
            res = value + (res << 4);
        }
        IResult::Ok((remaining, res))
    }
}
