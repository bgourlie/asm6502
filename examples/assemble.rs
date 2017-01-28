extern crate asm6502;

use asm6502::assemble;

fn main() {
    let asm = "LDA #1\nADC #1\nCMP #2";
    println!("Assembling:\n{}\n", asm);
    let mut buf = Vec::<u8>::new();
    if let Err(msg) = assemble(asm.as_bytes(), &mut buf) {
        panic!("Failed to assemble: {}", msg);
    }

    println!("Assembly:\n{:?}", buf)
}
