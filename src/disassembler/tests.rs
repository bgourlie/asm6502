use super::{AddressingMode, Instruction, InstructionDecoder, Mnemonic};

macro_rules! assert_disasm {
    ( $ buf : expr , $ expect_mnemonic : expr , $ expect_am : expr ) => {
        let buf = $buf;
        let mut decoder = InstructionDecoder::new(buf, 0);
        let expect_mnemonic = $expect_mnemonic;
        let expect_am = $expect_am;

        if let Some(Instruction::Known(_, mnemonic, am)) = decoder.next() {
            assert_eq!(expect_mnemonic, mnemonic);
            assert_eq!(expect_am, am);
            assert_eq!(buf.len(), decoder.pc as usize)
        } else {
            panic!("Failed to disassemble {:?} into {:?} {:?}", &buf, expect_mnemonic, expect_am)
        }
    };
}

#[test]
fn adc() {
    assert_disasm!(&[0x69, 0x44],
                   Mnemonic::ADC,
                   AddressingMode::Immediate(0x44));
    assert_disasm!(&[0x65, 0x44], Mnemonic::ADC, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0x75, 0x44],
                   Mnemonic::ADC,
                   AddressingMode::ZeroPageX(0x44));
    assert_disasm!(&[0x6d, 0x00, 0x44],
                   Mnemonic::ADC,
                   AddressingMode::Absolute(0x4400));
    assert_disasm!(&[0x7d, 0x00, 0x44],
                   Mnemonic::ADC,
                   AddressingMode::AbsoluteX(0x4400));
    assert_disasm!(&[0x79, 0x00, 0x44],
                   Mnemonic::ADC,
                   AddressingMode::AbsoluteY(0x4400));
    assert_disasm!(&[0x61, 0x44],
                   Mnemonic::ADC,
                   AddressingMode::IndexedIndirect(0x44));
    assert_disasm!(&[0x71, 0x44],
                   Mnemonic::ADC,
                   AddressingMode::IndirectIndexed(0x44));
}

#[test]
fn and() {
    assert_disasm!(&[0x25, 0x44], Mnemonic::AND, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0x35, 0x44],
                   Mnemonic::AND,
                   AddressingMode::ZeroPageX(0x44));
    assert_disasm!(&[0x2d, 0x00, 0x44],
                   Mnemonic::AND,
                   AddressingMode::Absolute(0x4400));
    assert_disasm!(&[0x3d, 0x00, 0x44],
                   Mnemonic::AND,
                   AddressingMode::AbsoluteX(0x4400));
    assert_disasm!(&[0x39, 0x00, 0x44],
                   Mnemonic::AND,
                   AddressingMode::AbsoluteY(0x4400));
    assert_disasm!(&[0x21, 0x44],
                   Mnemonic::AND,
                   AddressingMode::IndexedIndirect(0x44));
    assert_disasm!(&[0x31, 0x44],
                   Mnemonic::AND,
                   AddressingMode::IndirectIndexed(0x44));
}

#[test]
fn asl() {
    assert_disasm!(&[0x0a], Mnemonic::ASL, AddressingMode::Accumulator);
    assert_disasm!(&[0x06, 0x44], Mnemonic::ASL, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0x16, 0x44],
                   Mnemonic::ASL,
                   AddressingMode::ZeroPageX(0x44));
    assert_disasm!(&[0x0e, 0x00, 0x44],
                   Mnemonic::ASL,
                   AddressingMode::Absolute(0x4400));
    assert_disasm!(&[0x1e, 0x00, 0x44],
                   Mnemonic::ASL,
                   AddressingMode::AbsoluteX(0x4400));
}

#[test]
fn bit() {
    assert_disasm!(&[0x24, 0x44], Mnemonic::BIT, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0x2c, 0x00, 0x44],
                   Mnemonic::BIT,
                   AddressingMode::Absolute(0x4400));
}

#[test]
fn branches() {
    assert_disasm!(&[0x10, 0x44], Mnemonic::BPL, AddressingMode::Relative(0x44));
    assert_disasm!(&[0x30, 0x44], Mnemonic::BMI, AddressingMode::Relative(0x44));
    assert_disasm!(&[0x50, 0x44], Mnemonic::BVC, AddressingMode::Relative(0x44));
    assert_disasm!(&[0x70, 0x44], Mnemonic::BVS, AddressingMode::Relative(0x44));
    assert_disasm!(&[0x90, 0x44], Mnemonic::BCC, AddressingMode::Relative(0x44));
    assert_disasm!(&[0xb0, 0x44], Mnemonic::BCS, AddressingMode::Relative(0x44));
    assert_disasm!(&[0xd0, 0x44], Mnemonic::BNE, AddressingMode::Relative(0x44));
    assert_disasm!(&[0xf0, 0x44], Mnemonic::BEQ, AddressingMode::Relative(0x44));
}

#[test]
fn brk() {
    // BRK is special in that even though it is a one byte instruction, it increments the pc by 1
    let buf = &[0x0];
    let mut decoder = InstructionDecoder::new(buf, 0);

    if let Some(Instruction::Known(_, mnemonic, am)) = decoder.next() {
        assert_eq!(Mnemonic::BRK, mnemonic);
        assert_eq!(AddressingMode::Implied, am);
        assert_eq!(2, decoder.pc)
    } else {
        panic!("Failed to disassemble BRK instruction")
    }
}

#[test]
fn cmp() {
    assert_disasm!(&[0xc9, 0x44],
                   Mnemonic::CMP,
                   AddressingMode::Immediate(0x44));
    assert_disasm!(&[0xc5, 0x44], Mnemonic::CMP, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0xd5, 0x44],
                   Mnemonic::CMP,
                   AddressingMode::ZeroPageX(0x44));
    assert_disasm!(&[0xcd, 0x00, 0x44],
                   Mnemonic::CMP,
                   AddressingMode::Absolute(0x4400));
    assert_disasm!(&[0xdd, 0x00, 0x44],
                   Mnemonic::CMP,
                   AddressingMode::AbsoluteX(0x4400));
    assert_disasm!(&[0xd9, 0x00, 0x44],
                   Mnemonic::CMP,
                   AddressingMode::AbsoluteY(0x4400));
    assert_disasm!(&[0xc1, 0x44],
                   Mnemonic::CMP,
                   AddressingMode::IndexedIndirect(0x44));
    assert_disasm!(&[0xd1, 0x44],
                   Mnemonic::CMP,
                   AddressingMode::IndirectIndexed(0x44));
}

#[test]
fn cpx() {
    assert_disasm!(&[0xe0, 0x44],
                   Mnemonic::CPX,
                   AddressingMode::Immediate(0x44));
    assert_disasm!(&[0xe4, 0x44], Mnemonic::CPX, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0xec, 0x00, 0x44],
                   Mnemonic::CPX,
                   AddressingMode::Absolute(0x4400));
}

#[test]
fn cpy() {
    assert_disasm!(&[0xc0, 0x44],
                   Mnemonic::CPY,
                   AddressingMode::Immediate(0x44));
    assert_disasm!(&[0xc4, 0x44], Mnemonic::CPY, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0xcc, 0x00, 0x44],
                   Mnemonic::CPY,
                   AddressingMode::Absolute(0x4400));
}

#[test]
fn dec() {
    assert_disasm!(&[0xc6, 0x44], Mnemonic::DEC, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0xd6, 0x44],
                   Mnemonic::DEC,
                   AddressingMode::ZeroPageX(0x44));
    assert_disasm!(&[0xce, 0x00, 0x44],
                   Mnemonic::DEC,
                   AddressingMode::Absolute(0x4400));
    assert_disasm!(&[0xde, 0x00, 0x44],
                   Mnemonic::DEC,
                   AddressingMode::AbsoluteX(0x4400));
}

#[test]
fn eor() {
    assert_disasm!(&[0x49, 0x44],
                   Mnemonic::EOR,
                   AddressingMode::Immediate(0x44));
    assert_disasm!(&[0x45, 0x44], Mnemonic::EOR, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0x55, 0x44],
                   Mnemonic::EOR,
                   AddressingMode::ZeroPageX(0x44));
    assert_disasm!(&[0x4d, 0x00, 0x44],
                   Mnemonic::EOR,
                   AddressingMode::Absolute(0x4400));
    assert_disasm!(&[0x5d, 0x00, 0x44],
                   Mnemonic::EOR,
                   AddressingMode::AbsoluteX(0x4400));
    assert_disasm!(&[0x59, 0x00, 0x44],
                   Mnemonic::EOR,
                   AddressingMode::AbsoluteY(0x4400));
    assert_disasm!(&[0x41, 0x44],
                   Mnemonic::EOR,
                   AddressingMode::IndexedIndirect(0x44));
    assert_disasm!(&[0x51, 0x44],
                   Mnemonic::EOR,
                   AddressingMode::IndirectIndexed(0x44));
}

#[test]
fn flag_instructions() {
    assert_disasm!(&[0x18], Mnemonic::CLC, AddressingMode::Implied);
    assert_disasm!(&[0x38], Mnemonic::SEC, AddressingMode::Implied);
    assert_disasm!(&[0x58], Mnemonic::CLI, AddressingMode::Implied);
    assert_disasm!(&[0x78], Mnemonic::SEI, AddressingMode::Implied);
    assert_disasm!(&[0xb8], Mnemonic::CLV, AddressingMode::Implied);
    assert_disasm!(&[0xd8], Mnemonic::CLD, AddressingMode::Implied);
    assert_disasm!(&[0xf8], Mnemonic::SED, AddressingMode::Implied);
}

#[test]
fn inc() {
    assert_disasm!(&[0xe6, 0x44], Mnemonic::INC, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0xf6, 0x44],
                   Mnemonic::INC,
                   AddressingMode::ZeroPageX(0x44));
    assert_disasm!(&[0xee, 0x00, 0x44],
                   Mnemonic::INC,
                   AddressingMode::Absolute(0x4400));
    assert_disasm!(&[0xfe, 0x00, 0x44],
                   Mnemonic::INC,
                   AddressingMode::AbsoluteX(0x4400));
}

#[test]
fn jmp() {
    assert_disasm!(&[0x4c, 0x97, 0x55],
                   Mnemonic::JMP,
                   AddressingMode::Absolute(0x5597));
    assert_disasm!(&[0x6c, 0x97, 0x55],
                   Mnemonic::JMP,
                   AddressingMode::Indirect(0x5597));
}

#[test]
fn jsr() {
    assert_disasm!(&[0x20, 0x97, 0x55],
                   Mnemonic::JSR,
                   AddressingMode::Absolute(0x5597));
}

#[test]
fn lda() {
    assert_disasm!(&[0xa9, 0x44],
                   Mnemonic::LDA,
                   AddressingMode::Immediate(0x44));
    assert_disasm!(&[0xa5, 0x44], Mnemonic::LDA, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0xb5, 0x44],
                   Mnemonic::LDA,
                   AddressingMode::ZeroPageX(0x44));
    assert_disasm!(&[0xad, 0x00, 0x44],
                   Mnemonic::LDA,
                   AddressingMode::Absolute(0x4400));
    assert_disasm!(&[0xbd, 0x00, 0x44],
                   Mnemonic::LDA,
                   AddressingMode::AbsoluteX(0x4400));
    assert_disasm!(&[0xb9, 0x00, 0x44],
                   Mnemonic::LDA,
                   AddressingMode::AbsoluteY(0x4400));
    assert_disasm!(&[0xa1, 0x44],
                   Mnemonic::LDA,
                   AddressingMode::IndexedIndirect(0x44));
    assert_disasm!(&[0xb1, 0x44],
                   Mnemonic::LDA,
                   AddressingMode::IndirectIndexed(0x44));
}

#[test]
fn ldx() {
    assert_disasm!(&[0xa2, 0x44],
                   Mnemonic::LDX,
                   AddressingMode::Immediate(0x44));
    assert_disasm!(&[0xa6, 0x44], Mnemonic::LDX, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0xb6, 0x44],
                   Mnemonic::LDX,
                   AddressingMode::ZeroPageY(0x44));
    assert_disasm!(&[0xae, 0x00, 0x44],
                   Mnemonic::LDX,
                   AddressingMode::Absolute(0x4400));
    assert_disasm!(&[0xbe, 0x00, 0x44],
                   Mnemonic::LDX,
                   AddressingMode::AbsoluteY(0x4400));
}

#[test]
fn ldy() {
    assert_disasm!(&[0xa0, 0x44],
                   Mnemonic::LDY,
                   AddressingMode::Immediate(0x44));
    assert_disasm!(&[0xa4, 0x44], Mnemonic::LDY, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0xb4, 0x44],
                   Mnemonic::LDY,
                   AddressingMode::ZeroPageX(0x44));
    assert_disasm!(&[0xac, 0x00, 0x44],
                   Mnemonic::LDY,
                   AddressingMode::Absolute(0x4400));
    assert_disasm!(&[0xbc, 0x00, 0x44],
                   Mnemonic::LDY,
                   AddressingMode::AbsoluteX(0x4400));
}

#[test]
fn lsr() {
    assert_disasm!(&[0x4a], Mnemonic::LSR, AddressingMode::Accumulator);
    assert_disasm!(&[0x46, 0x44], Mnemonic::LSR, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0x56, 0x44],
                   Mnemonic::LSR,
                   AddressingMode::ZeroPageX(0x44));
    assert_disasm!(&[0x4e, 0x00, 0x44],
                   Mnemonic::LSR,
                   AddressingMode::Absolute(0x4400));
    assert_disasm!(&[0x5e, 0x00, 0x44],
                   Mnemonic::LSR,
                   AddressingMode::AbsoluteX(0x4400));
}

#[test]
fn nop() {
    assert_disasm!(&[0xea], Mnemonic::NOP, AddressingMode::Implied);
}

#[test]
fn ora() {
    assert_disasm!(&[0x09, 0x44],
                   Mnemonic::ORA,
                   AddressingMode::Immediate(0x44));
    assert_disasm!(&[0x05, 0x44], Mnemonic::ORA, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0x15, 0x44],
                   Mnemonic::ORA,
                   AddressingMode::ZeroPageX(0x44));
    assert_disasm!(&[0x0d, 0x00, 0x44],
                   Mnemonic::ORA,
                   AddressingMode::Absolute(0x4400));
    assert_disasm!(&[0x1d, 0x00, 0x44],
                   Mnemonic::ORA,
                   AddressingMode::AbsoluteX(0x4400));
    assert_disasm!(&[0x19, 0x00, 0x44],
                   Mnemonic::ORA,
                   AddressingMode::AbsoluteY(0x4400));
    assert_disasm!(&[0x01, 0x44],
                   Mnemonic::ORA,
                   AddressingMode::IndexedIndirect(0x44));
    assert_disasm!(&[0x11, 0x44],
                   Mnemonic::ORA,
                   AddressingMode::IndirectIndexed(0x44));
}

#[test]
fn register_instructions() {
    assert_disasm!(&[0xaa], Mnemonic::TAX, AddressingMode::Implied);
    assert_disasm!(&[0x8a], Mnemonic::TXA, AddressingMode::Implied);
    assert_disasm!(&[0xca], Mnemonic::DEX, AddressingMode::Implied);
    assert_disasm!(&[0xe8], Mnemonic::INX, AddressingMode::Implied);
    assert_disasm!(&[0xa8], Mnemonic::TAY, AddressingMode::Implied);
    assert_disasm!(&[0x98], Mnemonic::TYA, AddressingMode::Implied);
    assert_disasm!(&[0x88], Mnemonic::DEY, AddressingMode::Implied);
    assert_disasm!(&[0xc8], Mnemonic::INY, AddressingMode::Implied);
}

#[test]
fn rol() {
    assert_disasm!(&[0x2a], Mnemonic::ROL, AddressingMode::Accumulator);
    assert_disasm!(&[0x26, 0x44], Mnemonic::ROL, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0x36, 0x44],
                   Mnemonic::ROL,
                   AddressingMode::ZeroPageX(0x44));
    assert_disasm!(&[0x2e, 0x00, 0x44],
                   Mnemonic::ROL,
                   AddressingMode::Absolute(0x4400));
    assert_disasm!(&[0x3e, 0x00, 0x44],
                   Mnemonic::ROL,
                   AddressingMode::AbsoluteX(0x4400));
}

#[test]
fn ror() {
    assert_disasm!(&[0x6a], Mnemonic::ROR, AddressingMode::Accumulator);
    assert_disasm!(&[0x66, 0x44], Mnemonic::ROR, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0x76, 0x44],
                   Mnemonic::ROR,
                   AddressingMode::ZeroPageX(0x44));
    assert_disasm!(&[0x6e, 0x00, 0x44],
                   Mnemonic::ROR,
                   AddressingMode::Absolute(0x4400));
    assert_disasm!(&[0x7e, 0x00, 0x44],
                   Mnemonic::ROR,
                   AddressingMode::AbsoluteX(0x4400));
}

#[test]
fn rti() {
    assert_disasm!(&[0x40], Mnemonic::RTI, AddressingMode::Implied);
}

#[test]
fn rts() {
    assert_disasm!(&[0x60], Mnemonic::RTS, AddressingMode::Implied);
}

#[test]
fn sbc() {
    assert_disasm!(&[0xe9, 0x44],
                   Mnemonic::SBC,
                   AddressingMode::Immediate(0x44));
    assert_disasm!(&[0xe5, 0x44], Mnemonic::SBC, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0xf5, 0x44],
                   Mnemonic::SBC,
                   AddressingMode::ZeroPageX(0x44));
    assert_disasm!(&[0xed, 0x00, 0x44],
                   Mnemonic::SBC,
                   AddressingMode::Absolute(0x4400));
    assert_disasm!(&[0xfd, 0x00, 0x44],
                   Mnemonic::SBC,
                   AddressingMode::AbsoluteX(0x4400));
    assert_disasm!(&[0xf9, 0x00, 0x44],
                   Mnemonic::SBC,
                   AddressingMode::AbsoluteY(0x4400));
    assert_disasm!(&[0xe1, 0x44],
                   Mnemonic::SBC,
                   AddressingMode::IndexedIndirect(0x44));
    assert_disasm!(&[0xf1, 0x44],
                   Mnemonic::SBC,
                   AddressingMode::IndirectIndexed(0x44));
}

#[test]
fn sta() {
    assert_disasm!(&[0x85, 0x44], Mnemonic::STA, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0x95, 0x44],
                   Mnemonic::STA,
                   AddressingMode::ZeroPageX(0x44));
    assert_disasm!(&[0x8d, 0x00, 0x44],
                   Mnemonic::STA,
                   AddressingMode::Absolute(0x4400));
    assert_disasm!(&[0x9d, 0x00, 0x44],
                   Mnemonic::STA,
                   AddressingMode::AbsoluteX(0x4400));
    assert_disasm!(&[0x99, 0x00, 0x44],
                   Mnemonic::STA,
                   AddressingMode::AbsoluteY(0x4400));
    assert_disasm!(&[0x81, 0x44],
                   Mnemonic::STA,
                   AddressingMode::IndexedIndirect(0x44));
    assert_disasm!(&[0x91, 0x44],
                   Mnemonic::STA,
                   AddressingMode::IndirectIndexed(0x44));
}

#[test]
fn stack_instruction() {
    assert_disasm!(&[0x9a], Mnemonic::TXS, AddressingMode::Implied);
    assert_disasm!(&[0xba], Mnemonic::TSX, AddressingMode::Implied);
    assert_disasm!(&[0x48], Mnemonic::PHA, AddressingMode::Implied);
    assert_disasm!(&[0x68], Mnemonic::PLA, AddressingMode::Implied);
    assert_disasm!(&[0x08], Mnemonic::PHP, AddressingMode::Implied);
    assert_disasm!(&[0x28], Mnemonic::PLP, AddressingMode::Implied);
}

#[test]
fn stx() {
    assert_disasm!(&[0x86, 0x44], Mnemonic::STX, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0x96, 0x44],
                   Mnemonic::STX,
                   AddressingMode::ZeroPageY(0x44));
    assert_disasm!(&[0x8e, 0x00, 0x44],
                   Mnemonic::STX,
                   AddressingMode::Absolute(0x4400));
}

#[test]
fn sty() {
    assert_disasm!(&[0x84, 0x44], Mnemonic::STY, AddressingMode::ZeroPage(0x44));
    assert_disasm!(&[0x94, 0x44],
                   Mnemonic::STY,
                   AddressingMode::ZeroPageX(0x44));
    assert_disasm!(&[0x8c, 0x00, 0x44],
                   Mnemonic::STY,
                   AddressingMode::Absolute(0x4400));
}
