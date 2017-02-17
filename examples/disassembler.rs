extern crate asm6502;

use asm6502::{Instruction, InstructionDecoder};
use std::fs::File;
use std::io::Read;

const PC_START: u16 = 0x400;

fn main() {
    let mut f = File::open("../rs-nes/test_roms/6502_functional_test.bin").unwrap();
    let mut rom = Vec::<u8>::new();
    let bytes_read = f.read_to_end(&mut rom).unwrap();
    assert!(bytes_read == 65536);
    let decoder = InstructionDecoder::new(&rom, PC_START);

    for instruction in decoder.take(100) {
        match instruction {
            Instruction::Known(offset, mnemonic, am) => {
                println!("{:0>4X}: {} {:?}", offset, mnemonic.to_string(), am)
            }
            Instruction::Undefined(offset) => println!("{:0>4X}: Undefined", offset),
        }
    }
}
