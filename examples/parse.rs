extern crate asm6502;

use asm6502::parse;

fn main() {
    let res = parse("ROL A".as_bytes());
    println!("{:?}", res)
}
