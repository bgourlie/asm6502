#[macro_use]
extern crate nom;

extern crate serde;
extern crate serde_json;

#[macro_use]
extern crate serde_derive;

mod assembler;
mod disassembler;
mod parser;
mod tokens;

pub use assembler::assemble;
pub use disassembler::{AddressingMode, Instruction, InstructionDecoder, Mnemonic};
