#[cfg(test)]
mod tests;

use nom::IResult;
use parser::parse_lines;
use std::io::{Read, Write};
use tokens::*;

type AssembleResult = Result<(), String>;

pub fn assemble<R: Read, W: Write>(mut input: R, writer: &mut W) -> AssembleResult {
    let mut buf = Vec::<u8>::new();
    input.read_to_end(&mut buf).map_err(|_| "Error reading input".to_owned())?;
    match parse_lines(&buf) {
        IResult::Error(_) => Err("An error occurred while parsing".to_owned()),
        IResult::Incomplete(_) => {
            Err("An error occurred while parsing. Need more input.".to_owned())
        }
        IResult::Done(_, opcodes) => {
            let mut res: AssembleResult = Ok(());
            for opcode in opcodes {
                let OpCode(mnemonic, am) = opcode;
                match mnemonic {
                    Mnemonic::Adc => res = adc(am, writer),
                    Mnemonic::And => res = and(am, writer),
                    Mnemonic::Asl => res = asl(am, writer),
                    Mnemonic::Bit => res = bit(am, writer),
                    Mnemonic::Bcc => res = relative(0x90, am, "BCC", writer),
                    Mnemonic::Bcs => res = relative(0xb0, am, "BCS", writer),
                    Mnemonic::Beq => res = relative(0xf0, am, "BEQ", writer),
                    Mnemonic::Bmi => res = relative(0x30, am, "BMI", writer),
                    Mnemonic::Bne => res = relative(0xd0, am, "BNE", writer),
                    Mnemonic::Bpl => res = relative(0x10, am, "BPL", writer),
                    Mnemonic::Bvc => res = relative(0x50, am, "BVC", writer),
                    Mnemonic::Bvs => res = relative(0x70, am, "BVS", writer),
                    Mnemonic::Brk => res = brk(am, writer),
                    Mnemonic::Cmp => res = cmp(am, writer),
                    Mnemonic::Cpx => res = cpx(am, writer),
                    Mnemonic::Cpy => res = cpy(am, writer),
                    Mnemonic::Dec => res = dec(am, writer),
                    Mnemonic::Eor => res = eor(am, writer),
                    Mnemonic::Clc => res = implied(0x18, am, "CLC", writer),
                    Mnemonic::Cld => res = implied(0xd8, am, "CLD", writer),
                    Mnemonic::Cli => res = implied(0x58, am, "CLI", writer),
                    Mnemonic::Clv => res = implied(0xb8, am, "CLV", writer),
                    Mnemonic::Sec => res = implied(0x38, am, "SEC", writer),
                    Mnemonic::Sed => res = implied(0xf8, am, "SED", writer),
                    Mnemonic::Sei => res = implied(0x78, am, "SEI", writer),
                    Mnemonic::Inc => res = inc(am, writer),
                    Mnemonic::Jmp => res = jmp(am, writer),
                    Mnemonic::Jsr => res = jsr(am, writer),
                    Mnemonic::Lda => res = lda(am, writer),
                    Mnemonic::Ldx => res = ldx(am, writer),
                    Mnemonic::Ldy => res = ldy(am, writer),
                    Mnemonic::Lsr => res = lsr(am, writer),
                    Mnemonic::Nop => res = implied(0xea, am, "NOP", writer),
                    Mnemonic::Ora => res = ora(am, writer),
                    Mnemonic::Tax => res = implied(0xaa, am, "TAX", writer),
                    Mnemonic::Txa => res = implied(0x8a, am, "TXA", writer),
                    Mnemonic::Dex => res = implied(0xca, am, "DEX", writer),
                    Mnemonic::Inx => res = implied(0xe8, am, "INX", writer),
                    Mnemonic::Tay => res = implied(0xa8, am, "TAY", writer),
                    Mnemonic::Tya => res = implied(0x98, am, "TYA", writer),
                    Mnemonic::Dey => res = implied(0x88, am, "DEY", writer),
                    Mnemonic::Iny => res = implied(0xc8, am, "INY", writer),
                    _ => unimplemented!(),
                }
                if res.is_err() {
                    break;
                }
            }
            res
        }
    }
}

fn adc<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0x69, val, sign, writer),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x65, addr, sign, writer),
        AddressingMode::ZeroPageX(addr) => memory_byte(0x75, addr, writer),
        AddressingMode::Absolute(addr) => memory_word(0x6d, addr, writer),
        AddressingMode::AbsoluteX(addr) => memory_word(0x7d, addr, writer),
        AddressingMode::AbsoluteY(addr) => memory_word(0x79, addr, writer),
        AddressingMode::IndexedIndirect(addr) => memory_byte(0x61, addr, writer),
        AddressingMode::IndirectIndexed(addr) => memory_byte(0x71, addr, writer),
        _ => Err(format!("Unexpected operand encountered for ADC: {:?}", am)),
    }
}

fn and<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0x29, val, sign, writer),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x25, addr, sign, writer),
        AddressingMode::ZeroPageX(addr) => memory_byte(0x35, addr, writer),
        AddressingMode::Absolute(addr) => memory_word(0x2d, addr, writer),
        AddressingMode::AbsoluteX(addr) => memory_word(0x3d, addr, writer),
        AddressingMode::AbsoluteY(addr) => memory_word(0x39, addr, writer),
        AddressingMode::IndexedIndirect(addr) => memory_byte(0x21, addr, writer),
        AddressingMode::IndirectIndexed(addr) => memory_byte(0x31, addr, writer),
        _ => Err(format!("Unexpected operand encountered for AND: {:?}", am)),
    }
}

fn asl<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Accumulator => byte(0x0a, writer),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x06, addr, sign, writer),
        AddressingMode::ZeroPageX(addr) => memory_byte(0x16, addr, writer),
        AddressingMode::Absolute(addr) => memory_word(0x0e, addr, writer),
        AddressingMode::AbsoluteX(addr) => memory_word(0x1e, addr, writer),
        _ => Err(format!("Unexpected operand encountered for ASL: {:?}", am)),
    }
}

fn bit<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x24, addr, sign, writer),
        AddressingMode::Absolute(addr) => memory_word(0x2c, addr, writer),
        _ => Err(format!("Unexpected operand encountered for BIT: {:?}", am)),
    }
}

fn brk<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    // BRK is a 1 byte instruction but is followed by a padding byte.
    implied(0x0, am, "BRK", writer).and_then(|_| byte(0x0, writer))
}

fn cmp<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Absolute(addr) => memory_word(0xcd, addr, writer),
        AddressingMode::AbsoluteX(addr) => memory_word(0xdd, addr, writer),
        AddressingMode::AbsoluteY(addr) => memory_word(0xd9, addr, writer),
        AddressingMode::Immediate(val, sign) => immediate(0xc9, val, sign, writer),
        AddressingMode::IndexedIndirect(addr) => memory_byte(0xc1, addr, writer),
        AddressingMode::IndirectIndexed(addr) => memory_byte(0xd1, addr, writer),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xc5, addr, sign, writer),
        AddressingMode::ZeroPageX(addr) => memory_byte(0xd5, addr, writer),
        _ => Err(format!("Unexpected operand encountered for CMP: {:?}", am)),
    }
}

fn cpx<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0xe0, val, sign, writer),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xe4, addr, sign, writer),
        AddressingMode::Absolute(addr) => memory_word(0xec, addr, writer),
        _ => Err(format!("Unexpected operand encountered for CPX: {:?}", am)),
    }
}

fn cpy<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Absolute(addr) => memory_word(0xcc, addr, writer),
        AddressingMode::Immediate(val, sign) => immediate(0xc0, val, sign, writer),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xc4, addr, sign, writer),
        _ => Err(format!("Unexpected operand encountered for CPY: {:?}", am)),
    }
}

fn dec<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Absolute(addr) => memory_word(0xce, addr, writer),
        AddressingMode::AbsoluteX(addr) => memory_word(0xde, addr, writer),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xc6, addr, sign, writer),
        AddressingMode::ZeroPageX(addr) => memory_byte(0xd6, addr, writer),
        _ => Err(format!("Unexpected operand encountered for DEC: {:?}", am)),
    }
}

fn inc<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Absolute(addr) => memory_word(0xee, addr, writer),
        AddressingMode::AbsoluteX(addr) => memory_word(0xfe, addr, writer),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xe6, addr, sign, writer),
        AddressingMode::ZeroPageX(addr) => memory_byte(0xf6, addr, writer),
        _ => Err(format!("Unexpected operand encountered for INC: {:?}", am)),
    }
}

fn eor<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0x49, val, sign, writer),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x45, addr, sign, writer),
        AddressingMode::ZeroPageX(addr) => memory_byte(0x55, addr, writer),
        AddressingMode::Absolute(addr) => memory_word(0x4d, addr, writer),
        AddressingMode::AbsoluteX(addr) => memory_word(0x5d, addr, writer),
        AddressingMode::AbsoluteY(addr) => memory_word(0x59, addr, writer),
        AddressingMode::IndexedIndirect(addr) => memory_byte(0x41, addr, writer),
        AddressingMode::IndirectIndexed(addr) => memory_byte(0x51, addr, writer),
        _ => Err(format!("Unexpected operand encountered for ADC: {:?}", am)),
    }
}

fn jmp<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Absolute(addr) => memory_word(0x4c, addr, writer),
        AddressingMode::Indirect(addr) => memory_word(0x6c, addr, writer),
        _ => Err(format!("Unexpected operand encountered for JMP: {:?}", am)),
    }
}

fn jsr<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Absolute(addr) => memory_word(0x20, addr, writer),
        _ => Err(format!("Unexpected operand encountered for JSR: {:?}", am)),
    }
}

fn lda<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0xa9, val, sign, writer),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xa5, addr, sign, writer),
        AddressingMode::ZeroPageX(addr) => memory_byte(0xb5, addr, writer),
        AddressingMode::Absolute(addr) => memory_word(0xad, addr, writer),
        AddressingMode::AbsoluteX(addr) => memory_word(0xbd, addr, writer),
        AddressingMode::AbsoluteY(addr) => memory_word(0xb9, addr, writer),
        AddressingMode::IndexedIndirect(addr) => memory_byte(0xa1, addr, writer),
        AddressingMode::IndirectIndexed(addr) => memory_byte(0xb1, addr, writer),
        _ => Err(format!("Unexpected operand encountered for LDA: {:?}", am)),
    }
}

fn ldx<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0xa2, val, sign, writer),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xa6, addr, sign, writer),
        AddressingMode::ZeroPageY(addr) => memory_byte(0xb6, addr, writer),
        AddressingMode::Absolute(addr) => memory_word(0xae, addr, writer),
        AddressingMode::AbsoluteY(addr) => memory_word(0xbe, addr, writer),
        _ => Err(format!("Unexpected operand encountered for LDX: {:?}", am)),
    }
}

fn ldy<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0xa0, val, sign, writer),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0xa4, addr, sign, writer),
        AddressingMode::ZeroPageX(addr) => memory_byte(0xb4, addr, writer),
        AddressingMode::Absolute(addr) => memory_word(0xac, addr, writer),
        AddressingMode::AbsoluteX(addr) => memory_word(0xbc, addr, writer),
        _ => Err(format!("Unexpected operand encountered for LDY: {:?}", am)),
    }
}

fn lsr<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Accumulator => byte(0x4a, writer),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x46, addr, sign, writer),
        AddressingMode::ZeroPageX(addr) => memory_byte(0x56, addr, writer),
        AddressingMode::Absolute(addr) => memory_word(0x4e, addr, writer),
        AddressingMode::AbsoluteX(addr) => memory_word(0x5e, addr, writer),
        _ => Err(format!("Unexpected operand encountered for LSR: {:?}", am)),
    }
}

fn ora<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0x09, val, sign, writer),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x05, addr, sign, writer),
        AddressingMode::ZeroPageX(addr) => memory_byte(0x15, addr, writer),
        AddressingMode::Absolute(addr) => memory_word(0x0d, addr, writer),
        AddressingMode::AbsoluteX(addr) => memory_word(0x1d, addr, writer),
        AddressingMode::AbsoluteY(addr) => memory_word(0x19, addr, writer),
        AddressingMode::IndexedIndirect(addr) => memory_byte(0x01, addr, writer),
        AddressingMode::IndirectIndexed(addr) => memory_byte(0x11, addr, writer),
        _ => Err(format!("Unexpected operand encountered for ORA: {:?}", am)),
    }
}

fn immediate<T: Write>(opcode: u8, val: u8, sign: Sign, writer: &mut T) -> AssembleResult {
    byte(opcode, writer).and_then(|_| signed(val, sign, writer))
}

fn zero_page<T: Write>(opcode: u8, addr: u8, sign: Sign, writer: &mut T) -> AssembleResult {
    err_if_negative(sign).and_then(|_| byte(opcode, writer).and_then(|_| byte(addr, writer)))
}

fn memory_word<T: Write>(opcode: u8, addr: u16, writer: &mut T) -> AssembleResult {
    byte(opcode, writer).and_then(|_| word(addr, writer))
}

fn memory_byte<T: Write>(opcode: u8, addr: u8, writer: &mut T) -> AssembleResult {
    byte(opcode, writer).and_then(|_| byte(addr, writer))
}

fn relative<T: Write>(opcode: u8,
                      am: AddressingMode,
                      mnemonic: &'static str,
                      writer: &mut T)
                      -> AssembleResult {
    if let AddressingMode::ZeroPageOrRelative(offset, sign) = am {
        let sign = if sign == Sign::Implied {
            Sign::Positive
        } else {
            Sign::Negative
        };
        byte(opcode, writer).and_then(|_| signed(offset, sign, writer))
    } else {
        Err(format!("Unexpected operand encountered for {}: {:?}", mnemonic, am))
    }
}

fn implied<T: Write>(opcode: u8,
                     am: AddressingMode,
                     mnemonic: &'static str,
                     writer: &mut T)
                     -> AssembleResult {
    if let AddressingMode::Implied = am {
        byte(opcode, writer)
    } else {
        Err(format!("Unexpected operand encountered for {}: {:?}", mnemonic, am))
    }
}

fn signed<T: Write>(val: u8, sign: Sign, writer: &mut T) -> AssembleResult {
    match sign {
        Sign::Implied => byte(val, writer),
        Sign::Positive => {
            if val > 127 {
                Err("Signed byte overflow".to_owned())
            } else {
                byte(val, writer)
            }
        }
        Sign::Negative => {
            if val > 128 {
                Err("Signed byte overflow".to_owned())
            } else {
                let val = !val as u16 + 1;
                byte(val as u8, writer)
            }
        }
    }
}

fn byte<T: Write>(val: u8, writer: &mut T) -> AssembleResult {
    writer.write(&[val])
        .map(|_| ())
        .map_err(|_| "An error occurred while writing to the buffer".to_owned())
}

fn word<T: Write>(val: u16, writer: &mut T) -> AssembleResult {
    let low_byte = (val & 0xff) as u8;
    let high_byte = ((val >> 8) & 0xff) as u8;
    writer.write(&[low_byte, high_byte])
        .map(|_| ())
        .map_err(|_| "An error occurred while writing to the buffer".to_owned())
}

fn err_if_negative(sign: Sign) -> AssembleResult {
    if sign == Sign::Negative {
        Err("Unexpected signed operand".to_owned())
    } else {
        Ok(())
    }
}
