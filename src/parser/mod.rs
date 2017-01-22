#[cfg(test)]
mod tests;

use nom::{ErrorKind, IResult, line_ending, space};
use tokens::*;

named!(pub parse_lines <Vec<OpCode> >, separated_list!(line_ending, opcode));

named!(opcode <OpCode>, do_parse!(
        mnemonic: mnemonic >>
        am: addressing_mode >>
        (OpCode(mnemonic, am))
    )
);

named!(mnemonic <Mnemonic>, alt!(
        tag_no_case!("ADC") => { |_| Mnemonic::Adc } |
        tag_no_case!("AND") => { |_| Mnemonic::And } |
        tag_no_case!("ASL") => { |_| Mnemonic::Asl } |
        tag_no_case!("BCC") => { |_| Mnemonic::Bcc } |
        tag_no_case!("BCS") => { |_| Mnemonic::Bcs } |
        tag_no_case!("BEQ") => { |_| Mnemonic::Beq } |
        tag_no_case!("BIT") => { |_| Mnemonic::Bit } |
        tag_no_case!("BMI") => { |_| Mnemonic::Bmi } |
        tag_no_case!("BNE") => { |_| Mnemonic::Bne } |
        tag_no_case!("BPL") => { |_| Mnemonic::Bpl } |
        tag_no_case!("BRK") => { |_| Mnemonic::Brk } |
        tag_no_case!("BVC") => { |_| Mnemonic::Bvc } |
        tag_no_case!("BVS") => { |_| Mnemonic::Bvs } |
        tag_no_case!("CLC") => { |_| Mnemonic::Clc } |
        tag_no_case!("CLD") => { |_| Mnemonic::Cld } |
        tag_no_case!("CLI") => { |_| Mnemonic::Cli } |
        tag_no_case!("CLV") => { |_| Mnemonic::Clv } |
        tag_no_case!("CMP") => { |_| Mnemonic::Cmp } |
        tag_no_case!("CPX") => { |_| Mnemonic::Cpx } |
        tag_no_case!("CPY") => { |_| Mnemonic::Cpy } |
        tag_no_case!("DEC") => { |_| Mnemonic::Dec } |
        tag_no_case!("DEX") => { |_| Mnemonic::Dex } |
        tag_no_case!("DEY") => { |_| Mnemonic::Dey } |
        tag_no_case!("EOR") => { |_| Mnemonic::Eor } |
        tag_no_case!("INC") => { |_| Mnemonic::Inc } |
        tag_no_case!("INX") => { |_| Mnemonic::Inx } |
        tag_no_case!("INY") => { |_| Mnemonic::Iny } |
        tag_no_case!("JMP") => { |_| Mnemonic::Jmp } |
        tag_no_case!("JSR") => { |_| Mnemonic::Jsr } |
        tag_no_case!("LDA") => { |_| Mnemonic::Lda } |
        tag_no_case!("LDX") => { |_| Mnemonic::Ldx } |
        tag_no_case!("LDY") => { |_| Mnemonic::Ldy } |
        tag_no_case!("LSR") => { |_| Mnemonic::Lsr } |
        tag_no_case!("NOP") => { |_| Mnemonic::Nop } |
        tag_no_case!("ORA") => { |_| Mnemonic::Ora } |
        tag_no_case!("PHA") => { |_| Mnemonic::Pha } |
        tag_no_case!("PHP") => { |_| Mnemonic::Php } |
        tag_no_case!("PLA") => { |_| Mnemonic::Pla } |
        tag_no_case!("PLP") => { |_| Mnemonic::Plp } |
        tag_no_case!("ROL") => { |_| Mnemonic::Rol } |
        tag_no_case!("ROR") => { |_| Mnemonic::Ror } |
        tag_no_case!("RTI") => { |_| Mnemonic::Rti } |
        tag_no_case!("RTS") => { |_| Mnemonic::Rts } |
        tag_no_case!("SBC") => { |_| Mnemonic::Sbc } |
        tag_no_case!("SEC") => { |_| Mnemonic::Sec } |
        tag_no_case!("SED") => { |_| Mnemonic::Sed } |
        tag_no_case!("SEI") => { |_| Mnemonic::Sei } |
        tag_no_case!("STA") => { |_| Mnemonic::Sta } |
        tag_no_case!("STX") => { |_| Mnemonic::Stx } |
        tag_no_case!("STY") => { |_| Mnemonic::Sty } |
        tag_no_case!("TAX") => { |_| Mnemonic::Tax } |
        tag_no_case!("TAY") => { |_| Mnemonic::Tay } |
        tag_no_case!("TSX") => { |_| Mnemonic::Tsx } |
        tag_no_case!("TXA") => { |_| Mnemonic::Txa } |
        tag_no_case!("TXS") => { |_| Mnemonic::Txs } |
        tag_no_case!("TYA") => { |_| Mnemonic::Tya }
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
        space >>
        word: delimited!(tag!("("), alt!(parse_word_hex | dec_u16), tag!(")")) >>
        not!(tag!(",")) >>
        (AddressingMode::Indirect(word))
    )
);

named!(am_indexed_indirect <AddressingMode>,
    do_parse!(
        space >>
        byte: delimited!(tag!("("), alt!(parse_byte_hex | parse_byte_dec), tag_no_case!(",X")) >>
        ({ let (addr, _) = byte; AddressingMode::IndexedIndirect(addr) })
    )
);

named!(am_indirect_indexed <AddressingMode>,
    do_parse!(
        space >>
        byte: delimited!(tag!("("), alt!(parse_byte_hex | parse_byte_dec), tag_no_case!("),Y")) >>
        ({ let (addr, _) = byte; AddressingMode::IndirectIndexed(addr) })
    )
);

named!(am_accumulator <AddressingMode>,
    do_parse!(
        space >>
        tag_no_case!("A") >>
        (AddressingMode::Accumulator)
    )
);

named!(am_immediate <AddressingMode>,
    do_parse!(
        space >>
        val: preceded!(tag!("#"), alt!(parse_byte_hex | parse_byte_dec)) >>
        ({ let (byte, sign) = val; AddressingMode::Immediate(byte, sign)})
    )
);

named!(am_abs <AddressingMode>,
    do_parse!(
        space >>
        val: alt!(parse_word_hex | dec_u16) >>
        (AddressingMode::Absolute(val))
    )
);

named!(am_zp_or_relative <AddressingMode>,
    do_parse!(
        space >>
        val: alt!(parse_byte_hex | parse_byte_dec) >>
        ({ let (byte, sign) = val; AddressingMode::ZeroPageOrRelative(byte, sign)})
    )
);

named!(am_zp_x <AddressingMode>,
    do_parse!(
        space >>
        val: terminated!(alt!(parse_byte_hex | parse_byte_dec), tag_no_case!(",X")) >>
        ({ let (byte, _) = val; AddressingMode::ZeroPageX(byte)})
    )
);

named!(am_zp_y <AddressingMode>,
    do_parse!(
        space >>
        val: terminated!(alt!(parse_byte_hex | parse_byte_dec), tag_no_case!(",Y")) >>
        ({ let (byte, _) = val; AddressingMode::ZeroPageY(byte)})
    )
);

named!(am_abs_x <AddressingMode>,
    do_parse!(
        space >>
        val: terminated!(alt!(parse_word_hex | dec_u16), tag_no_case!(",X")) >>
        (AddressingMode::AbsoluteX(val))
    )
);

named!(am_abs_y <AddressingMode>,
    do_parse!(
        space >>
        val: terminated!(alt!(parse_word_hex | dec_u16), tag_no_case!(",Y")) >>
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

#[inline]
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

#[inline]
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

#[inline]
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

#[inline]
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
