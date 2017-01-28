# asm6502 [![Build Status](https://travis-ci.org/bgourlie/asm6502.svg?branch=master)](https://travis-ci.org/bgourlie/asm6502)

A work-in-progress 6502 assembler.

This assembler currently serves the immediate needs of my [NES emulator](https://github.com/bgourlie/rs-nes). As such,
it doesn't do anything except translate literal 6502 assembly into machine code. Not even labels are supported, and
relative addresses must specify a numeric offset. Any contributions are welcome, however. I'd love for this to 
become a more fully-featured assembler.

### Usage

```rust
use asm6502::assemble;

let asm = "LDA #1\nADC #1\nCMP #2".as_bytes();
let mut buf = Vec::<u8>::new();
if let Err(msg) = assemble(asm, &mut buf) {
     panic!("Failed to assemble: {}", msg);
}
 
assert_eq!(&[0xa9, 0x1, 0x69, 0x1, 0xc9, 0x2], &buf[..]);
```

The the input and output parameters of the `assemble` function are generic over the 
[`Read`](https://doc.rust-lang.org/stable/std/io/trait.Read.html) and 
[`Write`](https://doc.rust-lang.org/stable/std/io/trait.Write.html) traits, 
respectively. A more typical usage of this function would accept an input file and an output file.

### Known Issues

If the assembly ends with an implied instruction, it will need to be followed by a newline in order to be parsed
correctly, and will fail to parse otherwise. See my StackOverflow question 
[here](http://stackoverflow.com/q/41658386/547365) documenting the issue and the work-in-progress nom
[pull request](https://github.com/Geal/nom/pull/413) addressing the underlying issue.