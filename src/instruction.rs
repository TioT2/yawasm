use crate::Type;

/// Block in-out type representation function
#[derive(Copy, Clone, PartialEq)]
pub enum BlockType {
    /// () -> ()
    Void,

    /// () -> (out0)
    ResolvingTo(Type),

    /// (in0..inN) -> (out0..outN)
    Functional(u32),
} // enum BlockType

unsafe impl bytemuck::NoUninit for BlockType {}
unsafe impl bytemuck::Zeroable for BlockType {}

/// Block header representation structure
#[derive(Copy, Clone)]
pub struct BlockHeader {
    /// number of values 'consumed' from stack
    pub consume_count: u16,

    /// number of values 'outputted' into stack
    pub output_count: u16,

    /// block code length
    pub length: u32,
} // struct BlockHeader

unsafe impl bytemuck::Zeroable for BlockHeader {}
unsafe impl bytemuck::AnyBitPattern for BlockHeader {}
unsafe impl bytemuck::NoUninit for BlockHeader {}

/// Branch data representation structure
#[derive(Copy, Clone)]
pub struct BranchHeader {
    /// number of values 'consumed' from stack
    pub consume_count: u16,

    /// number of values 'outputted' into stack
    pub output_count: u16,

    /// then case length
    pub then_length: u32,

    /// else case length
    pub else_length: u32,
} // struct BranchHeader

unsafe impl bytemuck::Zeroable for BranchHeader {}
unsafe impl bytemuck::AnyBitPattern for BranchHeader {}
unsafe impl bytemuck::NoUninit for BranchHeader {}

unsafe impl bytemuck::NoUninit for Instruction {}

/// Own bytecode representation enumeration
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Instruction {
    /// Just empty instruction
    Nop,

    /// Block (used for branching/etc)
    Block,

    /// Loop (quite same as block, but with branching in block start)
    Loop,

    /// Obvious branching instruction
    If,

    /// Branching instruction
    Br,

    /// Conditional branching instruction
    BrIf,

    /// Execution interrupt instruction
    Return,

    /// Table-based branching instruction
    BrTable,

    /// Direct function call instruction
    Call,

    /// Indirect (by index from table) function call instruction
    CallIndirect,

    /// Single item from stack remove instruction
    Drop,

    /// Non-zero element selecting instruction
    Select,

    /// Typed element selecting instruction
    SelectTyped,

    /// Local variable getting instruction
    LocalGet,

    /// Local variable setting instruction
    LocalSet,

    /// Local variable setting without removing value from stack instruction
    LocalTee,

    /// Global variable getting instruction
    GlobalGet,

    /// Global variable setting instruction
    GlobalSet,

    /// Value from table getting instruction
    TableGet,

    /// Value from table setting instruction
    TableSet,

    /// I32 from heap to stack loading instruction
    I32Load,

    /// I64 from heap to stack loading instruction
    I64Load,

    /// F32 from heap to stack loading instruction
    F32Load,

    /// F64 from heap to stack loading instruction
    F64Load,

    /// I8 from heap to I32 stack entry loading instruction
    I32Load8S,

    /// U8 from heap to I32 stack entry loading instruction
    I32Load8U,

    /// I16 from heap to I32 stack entry loading instruction
    I32Load16S,

    /// U16 from heap to I32 stack entry loading instruction
    I32Load16U,

    /// I8 from heap to I64 stack entry loading instruction
    I64Load8S,

    /// U8 from heap to I64 stack entry loading instruction
    I64Load8U,

    /// I16 from heap to I64 stack entry loading instruction
    I64Load16S,

    /// U16 from heap to I64 stack entry loading instruction
    I64Load16U,

    /// I32 from heap to I64 stack entry loading instruction
    I64Load32S,

    /// U32 from heap to I64 stack entry loading instruction
    I64Load32U,

    /// I32 to heap storing instruction
    I32Store,

    /// I64 to heap storing instruction
    I64Store,

    /// F32 to heap storing instruction
    F32Store,

    /// F64 heap storing instruction
    F64Store,

    /// First I32 8 bits to heap storing instruction
    I32Store8,

    /// First I32 16 bits to heap storing instruction
    I32Store16,

    /// First I64 8 bits to heap storing instruction
    I64Store8,

    /// First I64 16 bits to heap storing instruction
    I64Store16,

    /// First I64 32 bits to heap storing instruction
    I64Store32,

    /// Heap (in 64kb memory pages) current size getting instruction
    MemorySize,

    /// Heap size extension request instruction
    MemoryGrow,

    /// Push 32-bit constant to stack
    Const32,

    /// Push 64-bit constant to stack
    Const64,

    /// I32 zero-equality testing instruction
    I32Eqz,

    /// I32 equality testing instructino
    I32Eq,

    /// I32 inequality testing instruction
    I32Ne,

    /// Signed I32 LT test
    I32LtS,

    /// Unsigned I32 LT test
    I32LtU,

    /// Signed I32 GT test
    I32GtS,

    /// Unsigned I32 GT test
    I32GtU,

    /// Signed I32 LE test
    I32LeS,

    /// Unsigned I32 LE test
    I32LeU,

    /// Signed I32 GE test
    I32GeS,

    /// Unsigned I32 GE test
    I32GeU,

    /// I64 zero-equality testing instruction
    I64Eqz,

    /// I64 equality testing instructino
    I64Eq,

    /// I64 inequality testing instruction
    I64Ne,

    /// Signed I64 LT test
    I64LtS,

    /// Unsigned I64 LT test
    I64LtU,

    /// Signed I64 GT test
    I64GtS,

    /// Unsigned I64 GT test
    I64GtU,

    /// Signed I64 LE test
    I64LeS,

    /// Unsigned I64 LE test
    I64LeU,

    /// Signed I64 GE test
    I64GeS,

    /// Unsigned I64 GE test
    I64GeU,

    /// F32 equality test
    F32Eq,

    /// F32 inequality test
    F32Ne,

    /// F32 LT test
    F32Lt,

    /// F32 GT test
    F32Gt,

    /// F32 LE test
    F32Le,

    /// F32 GE test
    F32Ge,

    /// F64 equality test
    F64Eq,

    /// F64 inequality test
    F64Ne,

    /// F64 LT test
    F64Lt,

    /// F64 GT test
    F64Gt,

    /// F64 LE test
    F64Le,

    /// F64 GE test
    F64Ge,

    /// I32 'count leading zeros' instruction
    I32Clz,

    /// I32 'count trailing zeros' instruction
    I32Ctz,

    /// I32 'count 1 bits' instruction
    I32Popcnt,

    /// I32 addition instruction
    I32Add,

    /// I32 substraction instruction
    I32Sub,

    /// I32 multiplication instruction
    I32Mul,

    /// I32 signed division instruction
    I32DivS,

    /// I32 unsigned division instruction
    I32DivU,

    /// I32 signed division remainder getting instruction
    I32RemS,

    /// I32 unsigned division remainder getting instruction
    I32RemU,

    /// I32 bitand instruction
    I32And,

    /// I32 bitor instruction
    I32Or,

    /// I32 bitxor instruction
    I32Xor,

    /// I32 bitshl instruction
    I32Shl,

    /// I32 signed bitshr instruction
    I32ShrS,

    /// I32 unsigned bitshr instruction
    I32ShrU,

    /// I32 rotl instruction
    I32Rotl,

    /// I32 rotr instruction
    I32Rotr,

    /// I64 'count leading zeros' instruction
    I64Clz,

    /// I64 'count trailing zeros' instruction
    I64Ctz,

    /// I64 'count 1 bits' instruction
    I64Popcnt,

    /// I64 addition instruction
    I64Add,

    /// I64 substraction instruction
    I64Sub,

    /// I64 multiplication instruction
    I64Mul,

    /// I64 signed division instruction
    I64DivS,

    /// I64 unsigned division instruction
    I64DivU,

    /// I64 signed division remainder getting instruction
    I64RemS,

    /// I64 unsigned division remainder getting instruction
    I64RemU,

    /// I64 bitand instruction
    I64And,

    /// I64 bitor instruction
    I64Or,

    /// I64 bitxor instruction
    I64Xor,

    /// I64 bitshl instruction
    I64Shl,

    /// I64 signed bitshr instruction
    I64ShrS,

    /// I64 unsigned bitshr instruction
    I64ShrU,

    /// I64 rotl instruction
    I64Rotl,

    /// I64 rotr instruction
    I64Rotr,

    /// F32 module calculation instruction
    F32Abs,

    /// F32 negation instruction
    F32Neg,

    /// F32 rounding to not less integer instruction
    F32Ceil,

    /// F32 rounding to not more integer instruction
    F32Floor,

    /// F32 floating point part truncation instruction
    F32Trunc,

    /// F32 to nearest integer rounding function
    F32Nearest,

    /// F32 square root getting instruction
    F32Sqrt,

    /// F32 addition instruction
    F32Add,

    /// F32 substraction instruction
    F32Sub,

    /// F32 multiplication instruction
    F32Mul,

    /// F32 division instruction
    F32Div,

    /// F32 minimal getting instruction
    F32Min,

    /// F32 maximal getting instruction
    F32Max,

    /// F32 copying sign from one operand to another instruction
    F32CopySign,

    /// F64 module calculation instruction
    F64Abs,

    /// F64 negation instruction
    F64Neg,

    /// F64 rounding to not less integer instruction
    F64Ceil,

    /// F64 rounding to not more integer instruction
    F64Floor,

    /// F64 floating point part truncation instruction
    F64Trunc,

    /// F64 to nearest integer rounding function
    F64Nearest,

    /// F64 square root getting instruction
    F64Sqrt,

    /// F64 addition instruction
    F64Add,

    /// F64 substraction instruction
    F64Sub,

    /// F64 multiplication instruction
    F64Mul,

    /// F64 division instruction
    F64Div,

    /// F64 minimal getting instruction
    F64Min,

    /// F64 maximal getting instruction
    F64Max,

    /// F64 copying sign from one operand to another instruction
    F64CopySign,

    /// I32 from I64 truncation instruction
    I32WrapI64,

    /// Signed I32 from F32 conversion instruction
    I32TruncF32S,

    /// Unsigned I32 from F32 conversion instruction
    I32TruncF32U,

    /// Signed I32 from F64 conversion instruction
    I32TruncF64S,

    /// Unsigned I32 from F64 conversion instruction
    I32TruncF64U,

    /// Signed I64 from I32 extending instruction
    I64ExtendI32S,

    /// Unsigned I64 from I32 extending instruction
    I64ExtendI32U,

    /// Signed I64 from F32 conversion instruction
    I64TruncF32S,

    /// Unsigned I64 from F32 conversion instruction
    I64TruncF32U,

    /// Signed I64 from F64 conversion instruction
    I64TruncF64S,

    /// Unsigned I64 from F64 conversion instruction
    I64TruncF64U,

    /// F32 from signed I32 conversion instruction
    F32ConvertI32S,

    /// F32 from unsigned I32 conversion instruction
    F32ConvertI32U,

    /// F32 from signed I64 conversion instruction
    F32ConvertI64S,

    /// F32 from unsigned I64 conversion instruction
    F32ConvertI64U,

    /// F32 from F64 conversion instruction
    F32DemoteF64,

    /// F64 from signed I32 conversion instruction
    F64ConvertI32S,

    /// F64 from unsigned I32 conversion instruction
    F64ConvertI32U,

    /// F64 from signed I64 conversion instruction
    F64ConvertI64S,

    /// F64 from unsigned I64 conversion instruction
    F64ConvertI64U,

    /// F64 from F23 conversion instruction
    F64PromoteF32,

    /// I32 from signed I8 extension function
    I32Extend8S,

    /// I32 from signed I16 extension function
    I32Extend16S,

    /// I64 from signed I8 extension function
    I64Extend8S,

    /// I64 from signed I16 extension function
    I64Extend16S,

    /// I64 from signed I32 extension function
    I64Extend32S,

    /// System instruction extension
    System,

    /// Vector instuction extension
    Vector,

    /// Execution finishing instruction
    Unreachable,
} // enum Instruction

// Try to read instruction from byte
impl TryFrom<u8> for Instruction {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value <= Instruction::Unreachable as u8 {
            Ok(unsafe { std::mem::transmute::<u8, Instruction>(value) })
        } else {
            Err(())
        }
    }
}

// file instruction.rs
