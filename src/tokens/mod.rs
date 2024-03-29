#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mnemonic {
    Adc,
    And,
    Asl,
    Bcc,
    Bcs,
    Beq,
    Bit,
    Bmi,
    Bne,
    Bpl,
    Brk,
    Bvc,
    Bvs,
    Clc,
    Cld,
    Cli,
    Clv,
    Cmp,
    Cpx,
    Cpy,
    Dec,
    Dex,
    Dey,
    Eor,
    Inc,
    Inx,
    Iny,
    Jmp,
    Jsr,
    Lda,
    Ldx,
    Ldy,
    Lsr,
    Nop,
    Ora,
    Pha,
    Php,
    Pla,
    Plp,
    Rol,
    Ror,
    Rti,
    Rts,
    Sbc,
    Sec,
    Sed,
    Sei,
    Sta,
    Stx,
    Sty,
    Tax,
    Tay,
    Tsx,
    Txa,
    Txs,
    Tya,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Sign {
    Implied,
    Positive,
    Negative,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AddressingMode {
    IndexedIndirect(u8),
    IndirectIndexed(u8),
    ZeroPageOrRelative(u8, Sign),
    Immediate(u8, Sign),
    Absolute(u16),
    AbsoluteX(u16),
    AbsoluteY(u16),
    ZeroPageX(u8),
    ZeroPageY(u8),
    Indirect(u16),
    Implied,
    Accumulator,
    Label(String),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ControlCommand {
    Byte(Vec<(u8, Sign)>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct OpCode(pub Mnemonic, pub AddressingMode);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    ControlCommand(ControlCommand),
    OpCode(OpCode),
    Label(String),
}
