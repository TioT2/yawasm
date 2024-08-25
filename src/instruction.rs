use crate::{module, Type};

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
}

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

    /// I32 type constant to stack pushing function
    I32Const,

    /// I32 type constant to stack pushing function
    I64Const,

    /// F32 type constant to stack pushing function
    F32Const,

    /// F64 type constant to stack pushing function
    F64Const,

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

impl TryFrom<module::wasm::Opcode> for Instruction {
    type Error = ();

    fn try_from(value: module::wasm::Opcode) -> Result<Instruction, Self::Error> {
        type Main = module::wasm::Opcode;

        match value {
            Main::Unreachable          => Ok(Instruction::Unreachable),
            Main::Nop                  => Ok(Instruction::Nop),
            Main::Block                => Ok(Instruction::Block),
            Main::Loop                 => Ok(Instruction::Loop),
            Main::If                   => Ok(Instruction::If),
            Main::Br                   => Ok(Instruction::Br),
            Main::BrIf                 => Ok(Instruction::BrIf),
            Main::Return               => Ok(Instruction::Return),
            Main::BrTable              => Ok(Instruction::BrTable),
            Main::Call                 => Ok(Instruction::Call),
            Main::CallIndirect         => Ok(Instruction::CallIndirect),
            Main::Drop                 => Ok(Instruction::Drop),
            Main::Select               => Ok(Instruction::Select),
            Main::SelectTyped          => Ok(Instruction::SelectTyped),
            Main::LocalGet             => Ok(Instruction::LocalGet),
            Main::LocalSet             => Ok(Instruction::LocalSet),
            Main::LocalTee             => Ok(Instruction::LocalTee),
            Main::GlobalGet            => Ok(Instruction::GlobalGet),
            Main::GlobalSet            => Ok(Instruction::GlobalSet),
            Main::TableGet             => Ok(Instruction::TableGet),
            Main::TableSet             => Ok(Instruction::TableSet),
            Main::I32Load              => Ok(Instruction::I32Load),
            Main::I64Load              => Ok(Instruction::I64Load),
            Main::F32Load              => Ok(Instruction::F32Load),
            Main::F64Load              => Ok(Instruction::F64Load),
            Main::I32Load8S            => Ok(Instruction::I32Load8S),
            Main::I32Load8U            => Ok(Instruction::I32Load8U),
            Main::I32Load16S           => Ok(Instruction::I32Load16S),
            Main::I32Load16U           => Ok(Instruction::I32Load16U),
            Main::I64Load8S            => Ok(Instruction::I64Load8S),
            Main::I64Load8U            => Ok(Instruction::I64Load8U),
            Main::I64Load16S           => Ok(Instruction::I64Load16S),
            Main::I64Load16U           => Ok(Instruction::I64Load16U),
            Main::I64Load32S           => Ok(Instruction::I64Load32S),
            Main::I64Load32U           => Ok(Instruction::I64Load32U),
            Main::I32Store             => Ok(Instruction::I32Store),
            Main::I64Store             => Ok(Instruction::I64Store),
            Main::F32Store             => Ok(Instruction::F32Store),
            Main::F64Store             => Ok(Instruction::F64Store),
            Main::I32Store8            => Ok(Instruction::I32Store8),
            Main::I32Store16           => Ok(Instruction::I32Store16),
            Main::I64Store8            => Ok(Instruction::I64Store8),
            Main::I64Store16           => Ok(Instruction::I64Store16),
            Main::I64Store32           => Ok(Instruction::I64Store32),
            Main::MemorySize           => Ok(Instruction::MemorySize),
            Main::MemoryGrow           => Ok(Instruction::MemoryGrow),
            Main::I32Const             => Ok(Instruction::I32Const),
            Main::I64Const             => Ok(Instruction::I64Const),
            Main::F32Const             => Ok(Instruction::F32Const),
            Main::F64Const             => Ok(Instruction::F64Const),
            Main::I32Eqz               => Ok(Instruction::I32Eqz),
            Main::I32Eq                => Ok(Instruction::I32Eq),
            Main::I32Ne                => Ok(Instruction::I32Ne),
            Main::I32LtS               => Ok(Instruction::I32LtS),
            Main::I32LtU               => Ok(Instruction::I32LtU),
            Main::I32GtS               => Ok(Instruction::I32GtS),
            Main::I32GtU               => Ok(Instruction::I32GtU),
            Main::I32LeS               => Ok(Instruction::I32LeS),
            Main::I32LeU               => Ok(Instruction::I32LeU),
            Main::I32GeS               => Ok(Instruction::I32GeS),
            Main::I32GeU               => Ok(Instruction::I32GeU),
            Main::I64Eqz               => Ok(Instruction::I64Eqz),
            Main::I64Eq                => Ok(Instruction::I64Eq),
            Main::I64Ne                => Ok(Instruction::I64Ne),
            Main::I64LtS               => Ok(Instruction::I64LtS),
            Main::I64LtU               => Ok(Instruction::I64LtU),
            Main::I64GtS               => Ok(Instruction::I64GtS),
            Main::I64GtU               => Ok(Instruction::I64GtU),
            Main::I64LeS               => Ok(Instruction::I64LeS),
            Main::I64LeU               => Ok(Instruction::I64LeU),
            Main::I64GeS               => Ok(Instruction::I64GeS),
            Main::I64GeU               => Ok(Instruction::I64GeU),
            Main::F32Eq                => Ok(Instruction::F32Eq),
            Main::F32Ne                => Ok(Instruction::F32Ne),
            Main::F32Lt                => Ok(Instruction::F32Lt),
            Main::F32Gt                => Ok(Instruction::F32Gt),
            Main::F32Le                => Ok(Instruction::F32Le),
            Main::F32Ge                => Ok(Instruction::F32Ge),
            Main::F64Eq                => Ok(Instruction::F64Eq),
            Main::F64Ne                => Ok(Instruction::F64Ne),
            Main::F64Lt                => Ok(Instruction::F64Lt),
            Main::F64Gt                => Ok(Instruction::F64Gt),
            Main::F64Le                => Ok(Instruction::F64Le),
            Main::F64Ge                => Ok(Instruction::F64Ge),
            Main::I32Clz               => Ok(Instruction::I32Clz),
            Main::I32Ctz               => Ok(Instruction::I32Ctz),
            Main::I32Popcnt            => Ok(Instruction::I32Popcnt),
            Main::I32Add               => Ok(Instruction::I32Add),
            Main::I32Sub               => Ok(Instruction::I32Sub),
            Main::I32Mul               => Ok(Instruction::I32Mul),
            Main::I32DivS              => Ok(Instruction::I32DivS),
            Main::I32DivU              => Ok(Instruction::I32DivU),
            Main::I32RemS              => Ok(Instruction::I32RemS),
            Main::I32RemU              => Ok(Instruction::I32RemU),
            Main::I32And               => Ok(Instruction::I32And),
            Main::I32Or                => Ok(Instruction::I32Or),
            Main::I32Xor               => Ok(Instruction::I32Xor),
            Main::I32Shl               => Ok(Instruction::I32Shl),
            Main::I32ShrS              => Ok(Instruction::I32ShrS),
            Main::I32ShrU              => Ok(Instruction::I32ShrU),
            Main::I32Rotl              => Ok(Instruction::I32Rotl),
            Main::I32Rotr              => Ok(Instruction::I32Rotr),
            Main::I64Clz               => Ok(Instruction::I64Clz),
            Main::I64Ctz               => Ok(Instruction::I64Ctz),
            Main::I64Popcnt            => Ok(Instruction::I64Popcnt),
            Main::I64Add               => Ok(Instruction::I64Add),
            Main::I64Sub               => Ok(Instruction::I64Sub),
            Main::I64Mul               => Ok(Instruction::I64Mul),
            Main::I64DivS              => Ok(Instruction::I64DivS),
            Main::I64DivU              => Ok(Instruction::I64DivU),
            Main::I64RemS              => Ok(Instruction::I64RemS),
            Main::I64RemU              => Ok(Instruction::I64RemU),
            Main::I64And               => Ok(Instruction::I64And),
            Main::I64Or                => Ok(Instruction::I64Or),
            Main::I64Xor               => Ok(Instruction::I64Xor),
            Main::I64Shl               => Ok(Instruction::I64Shl),
            Main::I64ShrS              => Ok(Instruction::I64ShrS),
            Main::I64ShrU              => Ok(Instruction::I64ShrU),
            Main::I64Rotl              => Ok(Instruction::I64Rotl),
            Main::I64Rotr              => Ok(Instruction::I64Rotr),
            Main::F32Abs               => Ok(Instruction::F32Abs),
            Main::F32Neg               => Ok(Instruction::F32Neg),
            Main::F32Ceil              => Ok(Instruction::F32Ceil),
            Main::F32Floor             => Ok(Instruction::F32Floor),
            Main::F32Trunc             => Ok(Instruction::F32Trunc),
            Main::F32Nearest           => Ok(Instruction::F32Nearest),
            Main::F32Sqrt              => Ok(Instruction::F32Sqrt),
            Main::F32Add               => Ok(Instruction::F32Add),
            Main::F32Sub               => Ok(Instruction::F32Sub),
            Main::F32Mul               => Ok(Instruction::F32Mul),
            Main::F32Div               => Ok(Instruction::F32Div),
            Main::F32Min               => Ok(Instruction::F32Min),
            Main::F32Max               => Ok(Instruction::F32Max),
            Main::F32CopySign          => Ok(Instruction::F32CopySign),
            Main::F64Abs               => Ok(Instruction::F64Abs),
            Main::F64Neg               => Ok(Instruction::F64Neg),
            Main::F64Ceil              => Ok(Instruction::F64Ceil),
            Main::F64Floor             => Ok(Instruction::F64Floor),
            Main::F64Trunc             => Ok(Instruction::F64Trunc),
            Main::F64Nearest           => Ok(Instruction::F64Nearest),
            Main::F64Sqrt              => Ok(Instruction::F64Sqrt),
            Main::F64Add               => Ok(Instruction::F64Add),
            Main::F64Sub               => Ok(Instruction::F64Sub),
            Main::F64Mul               => Ok(Instruction::F64Mul),
            Main::F64Div               => Ok(Instruction::F64Div),
            Main::F64Min               => Ok(Instruction::F64Min),
            Main::F64Max               => Ok(Instruction::F64Max),
            Main::F64CopySign          => Ok(Instruction::F64CopySign),
            Main::I32WrapI64           => Ok(Instruction::I32WrapI64),
            Main::I32TruncF32S         => Ok(Instruction::I32TruncF32S),
            Main::I32TruncF32U         => Ok(Instruction::I32TruncF32U),
            Main::I32TruncF64S         => Ok(Instruction::I32TruncF64S),
            Main::I32TruncF64U         => Ok(Instruction::I32TruncF64U),
            Main::I64ExtendI32S        => Ok(Instruction::I64ExtendI32S),
            Main::I64ExtendI32U        => Ok(Instruction::I64ExtendI32U),
            Main::I64TruncF32S         => Ok(Instruction::I64TruncF32S),
            Main::I64TruncF32U         => Ok(Instruction::I64TruncF32U),
            Main::I64TruncF64S         => Ok(Instruction::I64TruncF64S),
            Main::I64TruncF64U         => Ok(Instruction::I64TruncF64U),
            Main::F32ConvertI32S       => Ok(Instruction::F32ConvertI32S),
            Main::F32ConvertI32U       => Ok(Instruction::F32ConvertI32U),
            Main::F32ConvertI64S       => Ok(Instruction::F32ConvertI64S),
            Main::F32ConvertI64U       => Ok(Instruction::F32ConvertI64U),
            Main::F32DemoteF64         => Ok(Instruction::F32DemoteF64),
            Main::F64ConvertI32S       => Ok(Instruction::F64ConvertI32S),
            Main::F64ConvertI32U       => Ok(Instruction::F64ConvertI32U),
            Main::F64ConvertI64S       => Ok(Instruction::F64ConvertI64S),
            Main::F64ConvertI64U       => Ok(Instruction::F64ConvertI64U),
            Main::F64PromoteF32        => Ok(Instruction::F64PromoteF32),
            Main::I32Extend8S          => Ok(Instruction::I32Extend8S),
            Main::I32Extend16S         => Ok(Instruction::I32Extend16S),
            Main::I64Extend8S          => Ok(Instruction::I64Extend8S),
            Main::I64Extend16S         => Ok(Instruction::I64Extend16S),
            Main::I64Extend32S         => Ok(Instruction::I64Extend32S),
            Main::System               => Ok(Instruction::System),
            Main::Vector               => Ok(Instruction::Vector),
            _ => Err(()),
        }
    }
}
