# asm6502 [![Build Status](https://travis-ci.org/bgourlie/asm6502.svg?branch=master)](https://travis-ci.org/bgourlie/asm6502)

A work-in-progress 6502 assembler.

This assembler currently serves the immediate needs of my [NES emulator](https://github.com/bgourlie/rs-nes). As such,
it doesn't do anything except translate literal 6502 assembly into machine code. Not even labels are supported, and
relative addresses much specify a numeric offset. Any contributions are welcome, however. I'd love for this to 
become a more fully-featured assembler.