#[cfg(test)]
mod tests;
use crate::parser::parse_lines;
use crate::tokens::*;
use nom::IResult;
use std::collections::HashMap;
use std::io::Write;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum AssembleError<'a> {
    #[error("Failed to read input file")]
    ReadIO(std::io::Error),

    #[error("Failed to write output")]
    WriteIO(std::io::Error),

    #[error("Failed to tokenize input")]
    TokenizeError(nom::Err<nom::error::VerboseError<&'a str>>),

    #[error("Failed to parse input")]
    ParseError(String)
}

pub type AssembleResult<'a> = Result<(), AssembleError<'a>>;

/// Translate 6502 assembly into machine code.
///
/// # Examples
///
/// ```
/// use asm6502::assemble;
/// let asm = "LDA #1\nADC #1\nCMP #2";
/// let mut buf = Vec::<u8>::new();
/// if let Err(msg) = assemble(asm, &mut buf) {
///     panic!("Failed to assemble: {}", msg);
/// }
///
/// assert_eq!(&[0xa9, 0x1, 0x69, 0x1, 0xc9, 0x2], &buf[..]);
/// ```
///
/// The the input and output parameters are generic over the `Read` and `Write` traits,
/// respectively. A more typical usage of this function would accept an input file and an output
/// file.
pub fn assemble<'a, W: Write>(input: &'a str, output: &mut W) -> AssembleResult<'a> {
    match parse_lines(input) {
        IResult::Ok((_, tokens)) => process_tokens(input, tokens, output),
        IResult::Err(err) => Err(AssembleError::TokenizeError(err)),
    }
}

fn process_tokens<'a, W: Write>(input: &'a str, tokens: Vec<Token>, output: &mut W) -> AssembleResult<'a> {
    #[derive(Debug)]
    struct Relocation {
        label: String,
        offset: usize,
    }

    // Contains list of places there we should insert concrete address instead of label.
    let mut relocation_table: Vec<Relocation> = vec![];
    let mut label_pos: HashMap<String, usize> = HashMap::new();
    let mut codegen = Vec::<u8>::new();

    for token in tokens {
        match token {
            Token::OpCode(OpCode(mnemonic, am)) => {
                if let AddressingMode::Label(label) = &am {
                    relocation_table.push(Relocation {
                        label: label.clone(),
                        offset: codegen.len() + 1,
                    });
                };
                process_opcode(input, mnemonic, am, &mut codegen)?;
            }
            Token::Label(name) => {
                label_pos.insert(name, codegen.len());
            }
            Token::ControlCommand(cmd) => {
                match cmd {
                    ControlCommand::Byte(bytes) => {
                        for (abs, sign) in bytes {
                            signed(input, abs, sign, &mut codegen)?;
                        }
                    }
                }
            }
        }
    }

    for relocation in relocation_table.into_iter() {
        if let Some(offset) = label_pos.get(&relocation.label) {
            let relative = *offset as i64 - relocation.offset as i64 - 1;
            if relative > 127 || relative < -128 {
                return Err(AssembleError::ParseError(format!(
                    "Label \"{}\" is too far from definition",
                    relocation.label
                )));
            }
            codegen[relocation.offset] = relative as u8;
        } else {
            return Err(AssembleError::ParseError(format!("Label \"{}\" is not defined", relocation.label)));
        }
    }

    output
        .write(&codegen)
        .map(|_| ())
        .map_err(|err| AssembleError::WriteIO(err))
}

fn process_opcode<'a, 'b>(input: &'a str, mnemonic: Mnemonic, am: AddressingMode, codegen: &'b mut Vec<u8>) -> AssembleResult<'a> {
    match mnemonic {
        Mnemonic::Adc => adc(input, am, codegen),
        Mnemonic::And => and(input, am, codegen),
        Mnemonic::Asl => asl(input, am, codegen),
        Mnemonic::Bit => bit(input, am, codegen),
        Mnemonic::Bcc => relative(input, 0x90, am, "BCC", codegen),
        Mnemonic::Bcs => relative(input, 0xb0, am, "BCS", codegen),
        Mnemonic::Beq => relative(input, 0xf0, am, "BEQ", codegen),
        Mnemonic::Bmi => relative(input, 0x30, am, "BMI", codegen),
        Mnemonic::Bne => relative(input, 0xd0, am, "BNE", codegen),
        Mnemonic::Bpl => relative(input, 0x10, am, "BPL", codegen),
        Mnemonic::Bvc => relative(input, 0x50, am, "BVC", codegen),
        Mnemonic::Bvs => relative(input, 0x70, am, "BVS", codegen),
        Mnemonic::Brk => brk(input, am, codegen),
        Mnemonic::Cmp => cmp(input, am, codegen),
        Mnemonic::Cpx => cpx(input, am, codegen),
        Mnemonic::Cpy => cpy(input, am, codegen),
        Mnemonic::Dec => dec(input, am, codegen),
        Mnemonic::Eor => eor(input, am, codegen),
        Mnemonic::Clc => implied(input, 0x18, am, "CLC", codegen),
        Mnemonic::Cld => implied(input, 0xd8, am, "CLD", codegen),
        Mnemonic::Cli => implied(input, 0x58, am, "CLI", codegen),
        Mnemonic::Clv => implied(input, 0xb8, am, "CLV", codegen),
        Mnemonic::Sec => implied(input, 0x38, am, "SEC", codegen),
        Mnemonic::Sed => implied(input, 0xf8, am, "SED", codegen),
        Mnemonic::Sei => implied(input, 0x78, am, "SEI", codegen),
        Mnemonic::Inc => inc(input, am, codegen),
        Mnemonic::Jmp => jmp(input, am, codegen),
        Mnemonic::Jsr => jsr(input, am, codegen),
        Mnemonic::Lda => lda(input, am, codegen),
        Mnemonic::Ldx => ldx(input, am, codegen),
        Mnemonic::Ldy => ldy(input, am, codegen),
        Mnemonic::Lsr => lsr(input, am, codegen),
        Mnemonic::Nop => implied(input, 0xea, am, "NOP", codegen),
        Mnemonic::Ora => ora(input, am, codegen),
        Mnemonic::Tax => implied(input, 0xaa, am, "TAX", codegen),
        Mnemonic::Txa => implied(input, 0x8a, am, "TXA", codegen),
        Mnemonic::Dex => implied(input, 0xca, am, "DEX", codegen),
        Mnemonic::Inx => implied(input, 0xe8, am, "INX", codegen),
        Mnemonic::Tay => implied(input, 0xa8, am, "TAY", codegen),
        Mnemonic::Tya => implied(input, 0x98, am, "TYA", codegen),
        Mnemonic::Dey => implied(input, 0x88, am, "DEY", codegen),
        Mnemonic::Iny => implied(input, 0xc8, am, "INY", codegen),
        Mnemonic::Rol => rol(input, am, codegen),
        Mnemonic::Ror => ror(input, am, codegen),
        Mnemonic::Rti => implied(input, 0x40, am, "RTI", codegen),
        Mnemonic::Rts => implied(input, 0x60, am, "RTS", codegen),
        Mnemonic::Sbc => sbc(input, am, codegen),
        Mnemonic::Sta => sta(input, am, codegen),
        Mnemonic::Txs => implied(input, 0x9a, am, "TXS", codegen),
        Mnemonic::Tsx => implied(input, 0xba, am, "TSX", codegen),
        Mnemonic::Pha => implied(input, 0x48, am, "PHA", codegen),
        Mnemonic::Pla => implied(input, 0x68, am, "PLA", codegen),
        Mnemonic::Php => implied(input, 0x08, am, "PHP", codegen),
        Mnemonic::Plp => implied(input, 0x28, am, "PLP", codegen),
        Mnemonic::Stx => stx(input, am, codegen),
        Mnemonic::Sty => sty(input, am, codegen),
    }
}

#[inline]
fn adc<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(input, 0x69, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0x65, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(input, 0x75, addr, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0x6d, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(input, 0x7d, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(input, 0x79, addr, output),
        AddressingMode::IndexedIndirect(addr) => memory_byte(input, 0x61, addr, output),
        AddressingMode::IndirectIndexed(addr) => memory_byte(input, 0x71, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for ADC: {:?}", am))),
    }
}

#[inline]
fn and<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(input, 0x29, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0x25, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(input, 0x35, addr, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0x2d, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(input, 0x3d, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(input, 0x39, addr, output),
        AddressingMode::IndexedIndirect(addr) => memory_byte(input, 0x21, addr, output),
        AddressingMode::IndirectIndexed(addr) => memory_byte(input, 0x31, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for AND: {:?}", am))),
    }
}

#[inline]
fn asl<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Accumulator => byte(input, 0x0a, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0x06, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(input, 0x16, addr, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0x0e, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(input, 0x1e, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for ASL: {:?}", am))),
    }
}

#[inline]
fn bit<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0x24, addr, sign, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0x2c, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for BIT: {:?}", am))),
    }
}

#[inline]
fn brk<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    // BRK is a 1 byte instruction but is followed by a padding byte.
    implied(input, 0x0, am, "BRK", output).and_then(|_| byte(input, 0x0, output))
}

#[inline]
fn cmp<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Absolute(addr) => memory_word(input, 0xcd, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(input, 0xdd, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(input, 0xd9, addr, output),
        AddressingMode::Immediate(val, sign) => immediate(input, 0xc9, val, sign, output),
        AddressingMode::IndexedIndirect(addr) => memory_byte(input, 0xc1, addr, output),
        AddressingMode::IndirectIndexed(addr) => memory_byte(input, 0xd1, addr, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0xc5, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(input, 0xd5, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for CMP: {:?}", am))),
    }
}

#[inline]
fn cpx<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(input, 0xe0, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0xe4, addr, sign, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0xec, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for CPX: {:?}", am))),
    }
}

#[inline]
fn cpy<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Absolute(addr) => memory_word(input, 0xcc, addr, output),
        AddressingMode::Immediate(val, sign) => immediate(input, 0xc0, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0xc4, addr, sign, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for CPY: {:?}", am))),
    }
}

#[inline]
fn dec<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Absolute(addr) => memory_word(input, 0xce, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(input, 0xde, addr, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0xc6, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(input, 0xd6, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for DEC: {:?}", am))),
    }
}

#[inline]
fn inc<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Absolute(addr) => memory_word(input, 0xee, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(input, 0xfe, addr, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0xe6, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(input, 0xf6, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for INC: {:?}", am))),
    }
}

#[inline]
fn eor<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(input, 0x49, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0x45, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(input, 0x55, addr, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0x4d, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(input, 0x5d, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(input, 0x59, addr, output),
        AddressingMode::IndexedIndirect(addr) => memory_byte(input, 0x41, addr, output),
        AddressingMode::IndirectIndexed(addr) => memory_byte(input, 0x51, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for EOR: {:?}", am))),
    }
}

#[inline]
fn jmp<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Absolute(addr) => memory_word(input, 0x4c, addr, output),
        AddressingMode::Indirect(addr) => memory_word(input, 0x6c, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for JMP: {:?}", am))),
    }
}

#[inline]
fn jsr<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Absolute(addr) => memory_word(input, 0x20, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for JSR: {:?}", am))),
    }
}

#[inline]
fn lda<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(input, 0xa9, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0xa5, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(input, 0xb5, addr, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0xad, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(input, 0xbd, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(input, 0xb9, addr, output),
        AddressingMode::IndexedIndirect(addr) => memory_byte(input, 0xa1, addr, output),
        AddressingMode::IndirectIndexed(addr) => memory_byte(input, 0xb1, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for LDA: {:?}", am))),
    }
}

#[inline]
fn ldx<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(input, 0xa2, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0xa6, addr, sign, output),
        AddressingMode::ZeroPageY(addr) => memory_byte(input, 0xb6, addr, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0xae, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(input, 0xbe, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for LDX: {:?}", am))),
    }
}

#[inline]
fn ldy<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(input, 0xa0, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0xa4, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(input, 0xb4, addr, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0xac, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(input, 0xbc, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for LDY: {:?}", am))),
    }
}

#[inline]
fn lsr<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Accumulator => byte(input, 0x4a, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0x46, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(input, 0x56, addr, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0x4e, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(input, 0x5e, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for LSR: {:?}", am))),
    }
}

#[inline]
fn ora<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(input, 0x09, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0x05, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(input, 0x15, addr, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0x0d, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(input, 0x1d, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(input, 0x19, addr, output),
        AddressingMode::IndexedIndirect(addr) => memory_byte(input, 0x01, addr, output),
        AddressingMode::IndirectIndexed(addr) => memory_byte(input, 0x11, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for ORA: {:?}", am))),
    }
}

#[inline]
fn rol<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Accumulator => byte(input, 0x2a, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0x26, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(input, 0x36, addr, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0x2e, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(input, 0x3e, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for ROL: {:?}", am))),
    }
}

#[inline]
fn ror<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Accumulator => byte(input, 0x6a, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0x66, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(input, 0x76, addr, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0x6e, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(input, 0x7e, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for ROR: {:?}", am))),
    }
}

#[inline]
fn sbc<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(input, 0xe9, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0xe5, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(input, 0xf5, addr, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0xed, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(input, 0xfd, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(input, 0xf9, addr, output),
        AddressingMode::IndexedIndirect(addr) => memory_byte(input, 0xe1, addr, output),
        AddressingMode::IndirectIndexed(addr) => memory_byte(input, 0xf1, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for SBC: {:?}", am))),
    }
}

#[inline]
fn sta<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0x85, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(input, 0x95, addr, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0x8d, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(input, 0x9d, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(input, 0x99, addr, output),
        AddressingMode::IndexedIndirect(addr) => memory_byte(input, 0x81, addr, output),
        AddressingMode::IndirectIndexed(addr) => memory_byte(input, 0x91, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for STA: {:?}", am))),
    }
}

#[inline]
fn stx<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0x86, addr, sign, output),
        AddressingMode::ZeroPageY(addr) => memory_byte(input, 0x96, addr, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0x8e, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for STX: {:?}", am))),
    }
}

#[inline]
fn sty<'a, 'b, T: Write>(input: &'a str, am: AddressingMode, output: &'b mut T) -> AssembleResult<'a> {
    match am {
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(input, 0x84, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(input, 0x94, addr, output),
        AddressingMode::Absolute(addr) => memory_word(input, 0x8c, addr, output),
        _ => Err(AssembleError::ParseError(format!("Unexpected operand encountered for STY: {:?}", am))),
    }
}

#[inline]
fn immediate<'a, T: Write>(input: &'a str, opcode: u8, val: u8, sign: Sign, output: &mut T) -> AssembleResult<'a> {
    byte(input, opcode, output).and_then(|_| signed(input, val, sign, output))
}

#[inline]
fn zero_page<'a, 'b, T: Write>(input: &'a str, opcode: u8, addr: u8, sign: Sign, output: &'b mut T) -> AssembleResult<'a> {
    err_if_negative(input, sign).and_then(|_| byte(input, opcode, output).and_then(|_| byte(input, addr, output)))
}

#[inline]
fn memory_word<'a, 'b, T: Write>(input: &'a str, opcode: u8, addr: u16, output: &'b mut T) -> AssembleResult<'a> {
    byte(input, opcode, output).and_then(|_| word(input, addr, output))
}

#[inline]
fn memory_byte<'a, 'b, T: Write>(input: &'a str, opcode: u8, addr: u8, output: &'b mut T) -> AssembleResult<'a> {
    byte(input, opcode, output).and_then(|_| byte(input, addr, output))
}

#[inline]
fn relative<'a, 'b, T: Write>(
    input: &'a str,
    opcode: u8,
    am: AddressingMode,
    mnemonic: &'static str,
    output: &'b mut T,
) -> AssembleResult<'a> {
    match am {
        AddressingMode::ZeroPageOrRelative(offset, sign) => {
            let sign = if sign == Sign::Implied {
                Sign::Positive
            } else {
                Sign::Negative
            };
            byte(input, opcode, output).and_then(|_| signed(input, offset, sign, output))
        }
        AddressingMode::Label(_) => {
            // We do not have information about labels yet, so we cannot write specific address.
            // Just write something of same type. We will insert propper address on relocation step
            byte(input, opcode, output).and_then(|_| byte(input, 0, output))
        }
        _ => Err(AssembleError::ParseError(format!(
            "Unexpected operand encountered for {}: {:?}",
            mnemonic, am
        ))),
    }
}

#[inline]
fn implied<'a, 'b, T: Write>(
    input: &'a str,
    opcode: u8,
    am: AddressingMode,
    mnemonic: &'static str,
    output: &mut T,
) -> AssembleResult<'a> {
    if let AddressingMode::Implied = am {
        byte(input, opcode, output)
    } else {
        Err(AssembleError::ParseError(format!(
            "Unexpected operand encountered for {}: {:?}",
            mnemonic, am
        )))
    }
}

#[inline]
fn signed<'a, 'b, T: Write>(input: &'a str, val: u8, sign: Sign, output: &'b mut T) -> AssembleResult<'a> {
    match sign {
        Sign::Implied => byte(input, val, output),
        Sign::Positive => {
            if val > 127 {
                Err(AssembleError::ParseError("Signed byte overflow".to_owned()))
            } else {
                byte(input, val, output)
            }
        }
        Sign::Negative => {
            if val > 128 {
                Err(AssembleError::ParseError("Signed byte overflow".to_owned()))
            } else {
                let val = !val as u16 + 1;
                byte(input, val as u8, output)
            }
        }
    }
}

#[inline]
fn byte<'a, 'b, T: Write>(_input: &'a str, val: u8, output: &'b mut T) -> AssembleResult<'a> {
    output
        .write(&[val])
        .map(|_| ())
        .map_err(|err| AssembleError::WriteIO(err))
}

#[inline]
fn word<'a, 'b, T: Write>(_input: &'a str, val: u16, output: &'b mut T) -> AssembleResult<'a> {
    let low_byte = (val & 0xff) as u8;
    let high_byte = ((val >> 8) & 0xff) as u8;
    output
        .write(&[low_byte, high_byte])
        .map(|_| ())
        .map_err(|err| AssembleError::WriteIO(err))
}

#[inline]
fn err_if_negative<'a>(_input: &'a str, sign: Sign) -> AssembleResult<'a> {
    if sign == Sign::Negative {
        Err(AssembleError::ParseError("Unexpected signed operand".to_owned()))
    } else {
        Ok(())
    }
}
