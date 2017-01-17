#[cfg(test)]
mod tests;

use nom::IResult;
use parser::parse_lines;
use std::io::{Read, Write};
use tokens::*;

type AssembleResult = Result<(), String>;

pub fn assemble<R: Read, W: Write>(mut input: R, writer: &mut W) -> AssembleResult {
    let mut buf = Vec::<u8>::new();
    input.read_to_end(&mut buf);
    match parse_lines(&buf) {
        IResult::Error(_) => Err("An error occurred while parsing".to_string()),
        IResult::Incomplete(_) => {
            Err("An error occurred while parsing. Need more input.".to_string())
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
        AddressingMode::ZeroPageX(addr) => zero_page_x(0x75, addr, writer),
        AddressingMode::Absolute(addr) => absolute(0x6d, addr, writer),
        AddressingMode::AbsoluteX(addr) => absolute_x(0x7d, addr, writer),
        AddressingMode::AbsoluteY(addr) => absolute_y(0x79, addr, writer),
        AddressingMode::IndexedIndirect(addr) => indexed_indirect(0x61, addr, writer),
        AddressingMode::IndirectIndexed(addr) => indirect_indexed(0x71, addr, writer),
        _ => Err(format!("Unexpected operand encountered for ADC: {:?}", am)),
    }
}

fn and<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => immediate(0x29, val, sign, writer),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x25, addr, sign, writer),
        AddressingMode::ZeroPageX(addr) => zero_page_x(0x35, addr, writer),
        AddressingMode::Absolute(addr) => absolute(0x2d, addr, writer),
        AddressingMode::AbsoluteX(addr) => absolute_x(0x3d, addr, writer),
        AddressingMode::AbsoluteY(addr) => absolute_y(0x39, addr, writer),
        AddressingMode::IndexedIndirect(addr) => indexed_indirect(0x21, addr, writer),
        AddressingMode::IndirectIndexed(addr) => indirect_indexed(0x31, addr, writer),
        _ => Err(format!("Unexpected operand encountered for AND: {:?}", am)),
    }
}

fn asl<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Accumulator => accumulator(0x0a, writer),
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x06, addr, sign, writer),
        AddressingMode::ZeroPageX(addr) => zero_page_x(0x16, addr, writer),
        AddressingMode::Absolute(addr) => absolute(0x0e, addr, writer),
        AddressingMode::AbsoluteX(addr) => absolute_x(0x1e, addr, writer),
        _ => Err(format!("Unexpected operand encountered for ASL: {:?}", am)),
    }
}

fn bit<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::ZeroPageOrRelative(addr, sign) => zero_page(0x24, addr, sign, writer),
        AddressingMode::Absolute(addr) => absolute(0x2c, addr, writer),
        _ => Err(format!("Unexpected operand encountered for BIT: {:?}", am)),
    }
}

fn accumulator<T: Write>(opcode: u8, writer: &mut T) -> AssembleResult {
    byte(opcode, writer)
}

fn immediate<T: Write>(opcode: u8, val: u8, sign: Sign, writer: &mut T) -> AssembleResult {
    byte(opcode, writer).and_then(|_| signed(val, sign, writer))
}

fn zero_page<T: Write>(opcode: u8, addr: u8, sign: Sign, writer: &mut T) -> AssembleResult {
    err_if_negative(sign).and_then(|_| byte(opcode, writer).and_then(|_| byte(addr, writer)))
}

fn zero_page_x<T: Write>(opcode: u8, addr: u8, writer: &mut T) -> AssembleResult {
    byte(opcode, writer).and_then(|_| byte(addr, writer))
}

fn absolute<T: Write>(opcode: u8, addr: u16, writer: &mut T) -> AssembleResult {
    byte(opcode, writer).and_then(|_| word(addr, writer))
}

fn absolute_x<T: Write>(opcode: u8, addr: u16, writer: &mut T) -> AssembleResult {
    byte(opcode, writer).and_then(|_| word(addr, writer))
}

fn absolute_y<T: Write>(opcode: u8, addr: u16, writer: &mut T) -> AssembleResult {
    byte(opcode, writer).and_then(|_| word(addr, writer))
}

fn indexed_indirect<T: Write>(opcode: u8, addr: u8, writer: &mut T) -> AssembleResult {
    byte(opcode, writer).and_then(|_| byte(addr, writer))
}

fn indirect_indexed<T: Write>(opcode: u8, addr: u8, writer: &mut T) -> AssembleResult {
    byte(opcode, writer).and_then(|_| byte(addr, writer))
}

fn signed<T: Write>(val: u8, sign: Sign, writer: &mut T) -> AssembleResult {
    match sign {
        Sign::Implied => byte(val, writer),
        Sign::Negative => {
            if val > 127 {
                Err("Signed byte overflow".to_string())
            } else {
                byte(val, writer)
            }
        }
    }
}

fn byte<T: Write>(val: u8, writer: &mut T) -> AssembleResult {
    writer.write(&[val])
        .map(|_| ())
        .map_err(|_| "An error occurred while writing to the buffer".to_string())
}

fn word<T: Write>(val: u16, writer: &mut T) -> AssembleResult {
    let low_byte = (val & 0xff) as u8;
    let high_byte = ((val >> 8) & 0xff) as u8;
    writer.write(&[low_byte, high_byte])
        .map(|_| ())
        .map_err(|_| "An error occurred while writing to the buffer".to_string())
}

fn err_if_negative(sign: Sign) -> AssembleResult {
    if sign == Sign::Negative {
        Err("Unexpected signed operand".to_string())
    } else {
        Ok(())
    }
}
