#[cfg(test)]
mod tests;

use crate::parser::parse_lines;
use crate::tokens::*;
use nom::IResult;
use std::collections::HashMap;
use std::io::{Read, Write};

type AssembleResult = Result<(), String>;

/// Translate 6502 assembly into machine code.
///
/// # Examples
///
/// ```
/// use asm6502::assemble;
/// let asm = "LDA #1\nADC #1\nCMP #2".as_bytes();
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
pub fn assemble<R: Read, W: Write>(mut input: R, output: &mut W) -> AssembleResult {
    let mut buf = Vec::<u8>::new();
    input
        .read_to_end(&mut buf)
        .map_err(|_| "Error reading input".to_owned())?;

    match parse_lines(&buf) {
        IResult::Ok((_, tokens)) => process_tokens(tokens, output),
        IResult::Err(_) => Err("An error occurred while parsing".to_owned()),
    }
}

fn process_tokens<W: Write>(tokens: Vec<Token>, output: &mut W) -> AssembleResult {
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
                process_opcode(mnemonic, am, &mut codegen)?;
            }
            Token::Label(name) => {
                label_pos.insert(name, codegen.len());
            }
        }
    }

    for relocation in relocation_table.into_iter() {
        if let Some(offset) = label_pos.get(&relocation.label) {
            let relative = *offset as i64 - relocation.offset as i64 - 1;
            if relative > 127 || relative < -128 {
                return Err(format!(
                    "Label \"{}\" is too far from definition",
                    relocation.label
                ));
            }
            codegen[relocation.offset] = relative as u8;
        } else {
            return Err(format!("Label \"{}\" is not defined", relocation.label));
        }
    }

    output
        .write(&codegen)
        .map(|_| ())
        .map_err(|_| "An error occurred while writing to the buffer".to_owned())
}

fn process_opcode(mnemonic: Mnemonic, am: AddressingMode, codegen: &mut Vec<u8>) -> AssembleResult {
    match mnemonic {
        Mnemonic::Adc => adc(am, codegen),
        Mnemonic::And => and(am, codegen),
        Mnemonic::Asl => asl(am, codegen),
        Mnemonic::Bit => bit(am, codegen),
        Mnemonic::Bcc => relative(0x90, am, "BCC", codegen),
        Mnemonic::Bcs => relative(0xb0, am, "BCS", codegen),
        Mnemonic::Beq => relative(0xf0, am, "BEQ", codegen),
        Mnemonic::Bmi => relative(0x30, am, "BMI", codegen),
        Mnemonic::Bne => relative(0xd0, am, "BNE", codegen),
        Mnemonic::Bpl => relative(0x10, am, "BPL", codegen),
        Mnemonic::Bvc => relative(0x50, am, "BVC", codegen),
        Mnemonic::Bvs => relative(0x70, am, "BVS", codegen),
        Mnemonic::Brk => brk(am, codegen),
        Mnemonic::Cmp => cmp(am, codegen),
        Mnemonic::Cpx => cpx(am, codegen),
        Mnemonic::Cpy => cpy(am, codegen),
        Mnemonic::Dec => dec(am, codegen),
        Mnemonic::Eor => eor(am, codegen),
        Mnemonic::Clc => implied(0x18, am, "CLC", codegen),
        Mnemonic::Cld => implied(0xd8, am, "CLD", codegen),
        Mnemonic::Cli => implied(0x58, am, "CLI", codegen),
        Mnemonic::Clv => implied(0xb8, am, "CLV", codegen),
        Mnemonic::Sec => implied(0x38, am, "SEC", codegen),
        Mnemonic::Sed => implied(0xf8, am, "SED", codegen),
        Mnemonic::Sei => implied(0x78, am, "SEI", codegen),
        Mnemonic::Inc => inc(am, codegen),
        Mnemonic::Jmp => jmp(am, codegen),
        Mnemonic::Jsr => jsr(am, codegen),
        Mnemonic::Lda => lda(am, codegen),
        Mnemonic::Ldx => ldx(am, codegen),
        Mnemonic::Ldy => ldy(am, codegen),
        Mnemonic::Lsr => lsr(am, codegen),
        Mnemonic::Nop => implied(0xea, am, "NOP", codegen),
        Mnemonic::Ora => ora(am, codegen),
        Mnemonic::Tax => implied(0xaa, am, "TAX", codegen),
        Mnemonic::Txa => implied(0x8a, am, "TXA", codegen),
        Mnemonic::Dex => implied(0xca, am, "DEX", codegen),
        Mnemonic::Inx => implied(0xe8, am, "INX", codegen),
        Mnemonic::Tay => implied(0xa8, am, "TAY", codegen),
        Mnemonic::Tya => implied(0x98, am, "TYA", codegen),
        Mnemonic::Dey => implied(0x88, am, "DEY", codegen),
        Mnemonic::Iny => implied(0xc8, am, "INY", codegen),
        Mnemonic::Rol => rol(am, codegen),
        Mnemonic::Ror => ror(am, codegen),
        Mnemonic::Rti => implied(0x40, am, "RTI", codegen),
        Mnemonic::Rts => implied(0x60, am, "RTS", codegen),
        Mnemonic::Sbc => sbc(am, codegen),
        Mnemonic::Sta => sta(am, codegen),
        Mnemonic::Txs => implied(0x9a, am, "TXS", codegen),
        Mnemonic::Tsx => implied(0xba, am, "TSX", codegen),
        Mnemonic::Pha => implied(0x48, am, "PHA", codegen),
        Mnemonic::Pla => implied(0x68, am, "PLA", codegen),
        Mnemonic::Php => implied(0x08, am, "PHP", codegen),
        Mnemonic::Plp => implied(0x28, am, "PLP", codegen),
        Mnemonic::Stx => stx(am, codegen),
        Mnemonic::Sty => sty(am, codegen),
    }
}

#[inline]
fn adc<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0x69, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x65, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(0x75, addr, output),
        AddressingMode::Absolute(addr) => memory_word(0x6d, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(0x7d, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(0x79, addr, output),
        AddressingMode::IndexedIndirect(addr) => memory_byte(0x61, addr, output),
        AddressingMode::IndirectIndexed(addr) => memory_byte(0x71, addr, output),
        _ => Err(format!("Unexpected operand encountered for ADC: {:?}", am)),
    }
}

#[inline]
fn and<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0x29, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x25, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(0x35, addr, output),
        AddressingMode::Absolute(addr) => memory_word(0x2d, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(0x3d, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(0x39, addr, output),
        AddressingMode::IndexedIndirect(addr) => memory_byte(0x21, addr, output),
        AddressingMode::IndirectIndexed(addr) => memory_byte(0x31, addr, output),
        _ => Err(format!("Unexpected operand encountered for AND: {:?}", am)),
    }
}

#[inline]
fn asl<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Accumulator => byte(0x0a, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x06, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(0x16, addr, output),
        AddressingMode::Absolute(addr) => memory_word(0x0e, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(0x1e, addr, output),
        _ => Err(format!("Unexpected operand encountered for ASL: {:?}", am)),
    }
}

#[inline]
fn bit<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x24, addr, sign, output),
        AddressingMode::Absolute(addr) => memory_word(0x2c, addr, output),
        _ => Err(format!("Unexpected operand encountered for BIT: {:?}", am)),
    }
}

#[inline]
fn brk<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    // BRK is a 1 byte instruction but is followed by a padding byte.
    implied(0x0, am, "BRK", output).and_then(|_| byte(0x0, output))
}

#[inline]
fn cmp<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Absolute(addr) => memory_word(0xcd, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(0xdd, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(0xd9, addr, output),
        AddressingMode::Immediate(val, sign) => immediate(0xc9, val, sign, output),
        AddressingMode::IndexedIndirect(addr) => memory_byte(0xc1, addr, output),
        AddressingMode::IndirectIndexed(addr) => memory_byte(0xd1, addr, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xc5, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(0xd5, addr, output),
        _ => Err(format!("Unexpected operand encountered for CMP: {:?}", am)),
    }
}

#[inline]
fn cpx<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0xe0, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xe4, addr, sign, output),
        AddressingMode::Absolute(addr) => memory_word(0xec, addr, output),
        _ => Err(format!("Unexpected operand encountered for CPX: {:?}", am)),
    }
}

#[inline]
fn cpy<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Absolute(addr) => memory_word(0xcc, addr, output),
        AddressingMode::Immediate(val, sign) => immediate(0xc0, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xc4, addr, sign, output),
        _ => Err(format!("Unexpected operand encountered for CPY: {:?}", am)),
    }
}

#[inline]
fn dec<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Absolute(addr) => memory_word(0xce, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(0xde, addr, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xc6, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(0xd6, addr, output),
        _ => Err(format!("Unexpected operand encountered for DEC: {:?}", am)),
    }
}

#[inline]
fn inc<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Absolute(addr) => memory_word(0xee, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(0xfe, addr, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xe6, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(0xf6, addr, output),
        _ => Err(format!("Unexpected operand encountered for INC: {:?}", am)),
    }
}

#[inline]
fn eor<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0x49, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x45, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(0x55, addr, output),
        AddressingMode::Absolute(addr) => memory_word(0x4d, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(0x5d, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(0x59, addr, output),
        AddressingMode::IndexedIndirect(addr) => memory_byte(0x41, addr, output),
        AddressingMode::IndirectIndexed(addr) => memory_byte(0x51, addr, output),
        _ => Err(format!("Unexpected operand encountered for EOR: {:?}", am)),
    }
}

#[inline]
fn jmp<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Absolute(addr) => memory_word(0x4c, addr, output),
        AddressingMode::Indirect(addr) => memory_word(0x6c, addr, output),
        _ => Err(format!("Unexpected operand encountered for JMP: {:?}", am)),
    }
}

#[inline]
fn jsr<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Absolute(addr) => memory_word(0x20, addr, output),
        _ => Err(format!("Unexpected operand encountered for JSR: {:?}", am)),
    }
}

#[inline]
fn lda<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0xa9, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xa5, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(0xb5, addr, output),
        AddressingMode::Absolute(addr) => memory_word(0xad, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(0xbd, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(0xb9, addr, output),
        AddressingMode::IndexedIndirect(addr) => memory_byte(0xa1, addr, output),
        AddressingMode::IndirectIndexed(addr) => memory_byte(0xb1, addr, output),
        _ => Err(format!("Unexpected operand encountered for LDA: {:?}", am)),
    }
}

#[inline]
fn ldx<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0xa2, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xa6, addr, sign, output),
        AddressingMode::ZeroPageY(addr) => memory_byte(0xb6, addr, output),
        AddressingMode::Absolute(addr) => memory_word(0xae, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(0xbe, addr, output),
        _ => Err(format!("Unexpected operand encountered for LDX: {:?}", am)),
    }
}

#[inline]
fn ldy<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0xa0, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xa4, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(0xb4, addr, output),
        AddressingMode::Absolute(addr) => memory_word(0xac, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(0xbc, addr, output),
        _ => Err(format!("Unexpected operand encountered for LDY: {:?}", am)),
    }
}

#[inline]
fn lsr<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Accumulator => byte(0x4a, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x46, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(0x56, addr, output),
        AddressingMode::Absolute(addr) => memory_word(0x4e, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(0x5e, addr, output),
        _ => Err(format!("Unexpected operand encountered for LSR: {:?}", am)),
    }
}

#[inline]
fn ora<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0x09, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x05, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(0x15, addr, output),
        AddressingMode::Absolute(addr) => memory_word(0x0d, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(0x1d, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(0x19, addr, output),
        AddressingMode::IndexedIndirect(addr) => memory_byte(0x01, addr, output),
        AddressingMode::IndirectIndexed(addr) => memory_byte(0x11, addr, output),
        _ => Err(format!("Unexpected operand encountered for ORA: {:?}", am)),
    }
}

#[inline]
fn rol<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Accumulator => byte(0x2a, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x26, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(0x36, addr, output),
        AddressingMode::Absolute(addr) => memory_word(0x2e, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(0x3e, addr, output),
        _ => Err(format!("Unexpected operand encountered for ROL: {:?}", am)),
    }
}

#[inline]
fn ror<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Accumulator => byte(0x6a, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x66, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(0x76, addr, output),
        AddressingMode::Absolute(addr) => memory_word(0x6e, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(0x7e, addr, output),
        _ => Err(format!("Unexpected operand encountered for ROR: {:?}", am)),
    }
}

#[inline]
fn sbc<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0xe9, val, sign, output),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xe5, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(0xf5, addr, output),
        AddressingMode::Absolute(addr) => memory_word(0xed, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(0xfd, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(0xf9, addr, output),
        AddressingMode::IndexedIndirect(addr) => memory_byte(0xe1, addr, output),
        AddressingMode::IndirectIndexed(addr) => memory_byte(0xf1, addr, output),
        _ => Err(format!("Unexpected operand encountered for SBC: {:?}", am)),
    }
}

#[inline]
fn sta<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x85, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(0x95, addr, output),
        AddressingMode::Absolute(addr) => memory_word(0x8d, addr, output),
        AddressingMode::AbsoluteX(addr) => memory_word(0x9d, addr, output),
        AddressingMode::AbsoluteY(addr) => memory_word(0x99, addr, output),
        AddressingMode::IndexedIndirect(addr) => memory_byte(0x81, addr, output),
        AddressingMode::IndirectIndexed(addr) => memory_byte(0x91, addr, output),
        _ => Err(format!("Unexpected operand encountered for STA: {:?}", am)),
    }
}

#[inline]
fn stx<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x86, addr, sign, output),
        AddressingMode::ZeroPageY(addr) => memory_byte(0x96, addr, output),
        AddressingMode::Absolute(addr) => memory_word(0x8e, addr, output),
        _ => Err(format!("Unexpected operand encountered for STX: {:?}", am)),
    }
}

#[inline]
fn sty<T: Write>(am: AddressingMode, output: &mut T) -> AssembleResult {
    match am {
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x84, addr, sign, output),
        AddressingMode::ZeroPageX(addr) => memory_byte(0x94, addr, output),
        AddressingMode::Absolute(addr) => memory_word(0x8c, addr, output),
        _ => Err(format!("Unexpected operand encountered for STY: {:?}", am)),
    }
}

#[inline]
fn immediate<T: Write>(opcode: u8, val: u8, sign: Sign, output: &mut T) -> AssembleResult {
    byte(opcode, output).and_then(|_| signed(val, sign, output))
}

#[inline]
fn zero_page<T: Write>(opcode: u8, addr: u8, sign: Sign, output: &mut T) -> AssembleResult {
    err_if_negative(sign).and_then(|_| byte(opcode, output).and_then(|_| byte(addr, output)))
}

#[inline]
fn memory_word<T: Write>(opcode: u8, addr: u16, output: &mut T) -> AssembleResult {
    byte(opcode, output).and_then(|_| word(addr, output))
}

#[inline]
fn memory_byte<T: Write>(opcode: u8, addr: u8, output: &mut T) -> AssembleResult {
    byte(opcode, output).and_then(|_| byte(addr, output))
}

#[inline]
fn relative<T: Write>(
    opcode: u8,
    am: AddressingMode,
    mnemonic: &'static str,
    output: &mut T,
) -> AssembleResult {
    match am {
        AddressingMode::ZeroPageOrRelative(offset, sign) => {
            let sign = if sign == Sign::Implied {
                Sign::Positive
            } else {
                Sign::Negative
            };
            byte(opcode, output).and_then(|_| signed(offset, sign, output))
        }
        AddressingMode::Label(_) => {
            // We do not have information about labels yet, so we cannot write specific address.
            // Just write something of same type. We will insert propper address on relocation step
            byte(opcode, output).and_then(|_| byte(0, output))
        }
        _ => Err(format!(
            "Unexpected operand encountered for {}: {:?}",
            mnemonic, am
        )),
    }
}

#[inline]
fn implied<T: Write>(
    opcode: u8,
    am: AddressingMode,
    mnemonic: &'static str,
    output: &mut T,
) -> AssembleResult {
    if let AddressingMode::Implied = am {
        byte(opcode, output)
    } else {
        Err(format!(
            "Unexpected operand encountered for {}: {:?}",
            mnemonic, am
        ))
    }
}

#[inline]
fn signed<T: Write>(val: u8, sign: Sign, output: &mut T) -> AssembleResult {
    match sign {
        Sign::Implied => byte(val, output),
        Sign::Positive => {
            if val > 127 {
                Err("Signed byte overflow".to_owned())
            } else {
                byte(val, output)
            }
        }
        Sign::Negative => {
            if val > 128 {
                Err("Signed byte overflow".to_owned())
            } else {
                let val = !val as u16 + 1;
                byte(val as u8, output)
            }
        }
    }
}

#[inline]
fn byte<T: Write>(val: u8, output: &mut T) -> AssembleResult {
    output
        .write(&[val])
        .map(|_| ())
        .map_err(|_| "An error occurred while writing to the buffer".to_owned())
}

#[inline]
fn word<T: Write>(val: u16, output: &mut T) -> AssembleResult {
    let low_byte = (val & 0xff) as u8;
    let high_byte = ((val >> 8) & 0xff) as u8;
    output
        .write(&[low_byte, high_byte])
        .map(|_| ())
        .map_err(|_| "An error occurred while writing to the buffer".to_owned())
}

#[inline]
fn err_if_negative(sign: Sign) -> AssembleResult {
    if sign == Sign::Negative {
        Err("Unexpected signed operand".to_owned())
    } else {
        Ok(())
    }
}
