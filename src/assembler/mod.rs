#[cfg(test)]
mod tests;

use nom::IResult;
use parser::parse_lines;
use std::io::Write;
use tokens::*;

type AssembleResult = Result<(), String>;

pub fn assemble<T: Write>(input: &str, writer: &mut T) -> AssembleResult {
    match parse_lines(input.as_bytes()) {
        IResult::Error(_) => Err("An error occurred while parsing".to_string()),
        IResult::Incomplete(_) => {
            Err("An error occurred while parsing. Need more input.".to_string())
        }
        IResult::Done(_, opcodes) => {
            let mut res: AssembleResult = Ok(());
            for opcode in opcodes {
                let OpCode(mnemonic, am) = opcode;
                match mnemonic {
                    Mnemonic::Adc => res = write_adc(am, writer),
                    Mnemonic::And => res = write_and(am, writer),
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

fn write_adc<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => {
            write_byte(0x69, writer).and_then(|_| write_signed(val, sign, writer))
        }
        AddressingMode::ZeroPageOrRelative(addr, sign) => {
            err_if_negative(sign)
                .and_then(|_| write_byte(0x65, writer).and_then(|_| write_byte(addr, writer)))
        }
        AddressingMode::ZeroPageX(addr) => {
            write_byte(0x75, writer).and_then(|_| write_byte(addr, writer))
        }
        AddressingMode::Absolute(addr) => {
            write_byte(0x6d, writer).and_then(|_| write_word(addr, writer))
        }
        AddressingMode::AbsoluteX(addr) => {
            write_byte(0x7d, writer).and_then(|_| write_word(addr, writer))
        }
        AddressingMode::AbsoluteY(addr) => {
            write_byte(0x79, writer).and_then(|_| write_word(addr, writer))
        }
        AddressingMode::IndexedIndirect(addr) => {
            write_byte(0x61, writer).and_then(|_| write_byte(addr, writer))
        }
        AddressingMode::IndirectIndexed(addr) => {
            write_byte(0x71, writer).and_then(|_| write_byte(addr, writer))
        }
        _ => Err(format!("Unexpected operand encountered for ADC: {:?}", am)),
    }
}

fn write_and<T: Write>(am: AddressingMode, writer: &mut T) -> AssembleResult {
    match am {
        AddressingMode::Immediate(val, sign) => {
            write_byte(0x29, writer).and_then(|_| write_signed(val, sign, writer))
        }
        AddressingMode::ZeroPageOrRelative(addr, sign) => {
            err_if_negative(sign)
                .and_then(|_| write_byte(0x25, writer).and_then(|_| write_byte(addr, writer)))
        }
        AddressingMode::ZeroPageX(addr) => {
            write_byte(0x35, writer).and_then(|_| write_byte(addr, writer))
        }
        AddressingMode::Absolute(addr) => {
            write_byte(0x2d, writer).and_then(|_| write_word(addr, writer))
        }
        AddressingMode::AbsoluteX(addr) => {
            write_byte(0x3d, writer).and_then(|_| write_word(addr, writer))
        }
        AddressingMode::AbsoluteY(addr) => {
            write_byte(0x39, writer).and_then(|_| write_word(addr, writer))
        }
        AddressingMode::IndexedIndirect(addr) => {
            write_byte(0x21, writer).and_then(|_| write_byte(addr, writer))
        }
        AddressingMode::IndirectIndexed(addr) => {
            write_byte(0x31, writer).and_then(|_| write_byte(addr, writer))
        }
        _ => Err(format!("Unexpected operand encountered for AND: {:?}", am)),
    }
}

fn write_signed<T: Write>(val: u8, sign: Sign, writer: &mut T) -> AssembleResult {
    match sign {
        Sign::Implied => write_byte(val, writer),
        Sign::Negative => {
            if val > 127 {
                Err("Signed byte overflow".to_string())
            } else {
                write_byte(val, writer)
            }
        }
    }
}

fn write_byte<T: Write>(val: u8, writer: &mut T) -> AssembleResult {
    writer.write(&[val])
        .map(|_| ())
        .map_err(|_| "An error occurred while writing to the buffer".to_string())
}

fn write_word<T: Write>(val: u16, writer: &mut T) -> AssembleResult {
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
