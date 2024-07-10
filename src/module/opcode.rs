
#![allow(unused)]

use crate::instruction::Instruction;

/// WASM Opcode
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Main {
    Unreachable        = 0x00, // Fatal error
    Nop                = 0x01, // Nop
    Block              = 0x02, // Just block
    Loop               = 0x03, // Loop
    If                 = 0x04, // Branching instruction
    Else               = 0x05, // Else. Must appear after 'If' instruction only
    ExpressionEnd      = 0x0B, // Expression end
    Br                 = 0x0C, // Goto
    BrIf               = 0x0D, // Goto if
    Return             = 0x0F, // Return
    BrTable            = 0x0E, // Switch/match
    Call               = 0x10, // Call function by local index
    CallIndirect       = 0x11, // Call from function ID

    Drop               = 0x1A, // Pop
    Select             = 0x1B, // Select nonzero numeric operand
    SelectTyped        = 0x1C, // Select nonzero operand of certain type

    LocalGet           = 0x20, // Load local
    LocalSet           = 0x21, // Store local
    LocalTee           = 0x22, // Store and push
    GlobalGet          = 0x23, // Load global
    GlobalSet          = 0x24, // Store global

    TableGet           = 0x25, // Get value from table
    TableSet           = 0x26, // Set value into table

    I32Load            = 0x28, // Load I32 from variable
    I64Load            = 0x29, // Load I64 from variable
    F32Load            = 0x2A, // Load F32 from variable
    F64Load            = 0x2B, // Load F64 from variable

    I32Load8S          = 0x2C, // Partially load variable from stack
    I32Load8U          = 0x2D, // Partially load variable from stack
    I32Load16S         = 0x2E, // Partially load variable from stack
    I32Load16U         = 0x2F, // Partially load variable from stack

    I64Load8S          = 0x30, // Partially load variable from stack
    I64Load8U          = 0x31, // Partially load variable from stack
    I64Load16S         = 0x32, // Partially load variable from stack
    I64Load16U         = 0x33, // Partially load variable from stack
    I64Load32S         = 0x34, // Partially load variable from stack
    I64Load32U         = 0x35, // Partially load variable from stack

    I32Store           = 0x36, // Store variable into stack
    I64Store           = 0x37, // Store variable into stack
    F32Store           = 0x38, // Store variable into stack
    F64Store           = 0x39, // Store variable into stack

    I32Store8          = 0x3A, // Partially store variable into stack
    I32Store16         = 0x3B, // Partially store variable into stack

    I64Store8          = 0x3C, // Partially store variable into stack
    I64Store16         = 0x3D, // Partially store variable into stack
    I64Store32         = 0x3E, // Partially store variable into stack

    MemorySize         = 0x3F, // Returns current memory size
    MemoryGrow         = 0x40, // Extends memory and returns old size

    I32Const           = 0x41, // Push int32 constant
    I64Const           = 0x42, // Push int64 constant
    F32Const           = 0x43, // Push float32 constant
    F64Const           = 0x44, // Push float64 constant

    I32Eqz             = 0x45, // Math operation
    I32Eq              = 0x46, // Math operation
    I32Ne              = 0x47, // Math operation
    I32LtS             = 0x48, // Math operation
    I32LtU             = 0x49, // Math operation
    I32GtS             = 0x4A, // Math operation
    I32GtU             = 0x4B, // Math operation
    I32LeS             = 0x4C, // Math operation
    I32LeU             = 0x4D, // Math operation
    I32GeS             = 0x4E, // Math operation
    I32GeU             = 0x4F, // Math operation
    I64Eqz             = 0x50, // Math operation
    I64Eq              = 0x51, // Math operation
    I64Ne              = 0x52, // Math operation
    I64LtS             = 0x53, // Math operation
    I64LtU             = 0x54, // Math operation
    I64GtS             = 0x55, // Math operation
    I64GtU             = 0x56, // Math operation
    I64LeS             = 0x57, // Math operation
    I64LeU             = 0x58, // Math operation
    I64GeS             = 0x59, // Math operation
    I64GeU             = 0x5A, // Math operation
    F32Eq              = 0x5B, // Math operation
    F32Ne              = 0x5C, // Math operation
    F32Lt              = 0x5D, // Math operation
    F32Gt              = 0x5E, // Math operation
    F32Le              = 0x5F, // Math operation
    F32Ge              = 0x60, // Math operation
    F64Eq              = 0x61, // Math operation
    F64Ne              = 0x62, // Math operation
    F64Lt              = 0x63, // Math operation
    F64Gt              = 0x64, // Math operation
    F64Le              = 0x65, // Math operation
    F64Ge              = 0x66, // Math operation
    I32Clz             = 0x67, // Math operation
    I32Ctz             = 0x68, // Math operation
    I32Popcnt          = 0x69, // Math operation
    I32Add             = 0x6A, // Math operation
    I32Sub             = 0x6B, // Math operation
    I32Mul             = 0x6C, // Math operation
    I32DivS            = 0x6D, // Math operation
    I32DivU            = 0x6E, // Math operation
    I32RemS            = 0x6F, // Math operation
    I32RemU            = 0x70, // Math operation
    I32And             = 0x71, // Math operation
    I32Or              = 0x72, // Math operation
    I32Xor             = 0x73, // Math operation
    I32Shl             = 0x74, // Math operation
    I32ShrS            = 0x75, // Math operation
    I32ShrU            = 0x76, // Math operation
    I32Rotl            = 0x77, // Math operation
    I32Rotr            = 0x78, // Math operation
    I64Clz             = 0x79, // Math operation
    I64Ctz             = 0x7A, // Math operation
    I64Popcnt          = 0x7B, // Math operation
    I64Add             = 0x7C, // Math operation
    I64Sub             = 0x7D, // Math operation
    I64Mul             = 0x7E, // Math operation
    I64DivS            = 0x7F, // Math operation
    I64DivU            = 0x80, // Math operation
    I64RemS            = 0x81, // Math operation
    I64RemU            = 0x82, // Math operation
    I64And             = 0x83, // Math operation
    I64Or              = 0x84, // Math operation
    I64Xor             = 0x85, // Math operation
    I64Shl             = 0x86, // Math operation
    I64ShrS            = 0x87, // Math operation
    I64ShrU            = 0x88, // Math operation
    I64Rotl            = 0x89, // Math operation
    I64Rotr            = 0x8A, // Math operation
    F32Abs             = 0x8B, // Math operation
    F32Neg             = 0x8C, // Math operation
    F32Ceil            = 0x8D, // Math operation
    F32Floor           = 0x8E, // Math operation
    F32Trunc           = 0x8F, // Math operation
    F32Nearest         = 0x90, // Math operation
    F32Sqrt            = 0x91, // Math operation
    F32Add             = 0x92, // Math operation
    F32Sub             = 0x93, // Math operation
    F32Mul             = 0x94, // Math operation
    F32Div             = 0x95, // Math operation
    F32Min             = 0x96, // Math operation
    F32Max             = 0x97, // Math operation
    F32CopySign        = 0x98, // Math operation
    F64Abs             = 0x99, // Math operation
    F64Neg             = 0x9A, // Math operation
    F64Ceil            = 0x9B, // Math operation
    F64Floor           = 0x9C, // Math operation
    F64Trunc           = 0x9D, // Math operation
    F64Nearest         = 0x9E, // Math operation
    F64Sqrt            = 0x9F, // Math operation
    F64Add             = 0xA0, // Math operation
    F64Sub             = 0xA1, // Math operation
    F64Mul             = 0xA2, // Math operation
    F64Div             = 0xA3, // Math operation
    F64Min             = 0xA4, // Math operation
    F64Max             = 0xA5, // Math operation
    F64CopySign        = 0xA6, // Math operation
    I32WrapI64         = 0xA7, // Math operation
    I32TruncF32S       = 0xA8, // Math operation
    I32TruncF32U       = 0xA9, // Math operation
    I32TruncF64S       = 0xAA, // Math operation
    I32TruncF64U       = 0xAB, // Math operation
    I64ExtendI32S      = 0xAC, // Math operation
    I64ExtendI32U      = 0xAD, // Math operation
    I64TruncF32S       = 0xAE, // Math operation
    I64TruncF32U       = 0xAF, // Math operation
    I64TruncF64S       = 0xB0, // Math operation
    I64TruncF64U       = 0xB1, // Math operation
    F32ConvertI32S     = 0xB2, // Math operation
    F32ConvertI32U     = 0xB3, // Math operation
    F32ConvertI64S     = 0xB4, // Math operation
    F32ConvertI64U     = 0xB5, // Math operation
    F32DemoteF64       = 0xB6, // Math operation
    F64ConvertI32S     = 0xB7, // Math operation
    F64ConvertI32U     = 0xB8, // Math operation
    F64ConvertI64S     = 0xB9, // Math operation
    F64ConvertI64U     = 0xBA, // Math operation
    F64PromoteF32      = 0xBB, // Math operation
    I32ReinterpretF32  = 0xBC, // Math operation
    I64ReinterpretF64  = 0xBD, // Math operation
    F32ReinterpretI32  = 0xBE, // Math operation
    F64ReinterpretI64  = 0xBF, // Math operation
    I32Extend8S        = 0xC0, // Math operation
    I32Extend16S       = 0xC1, // Math operation
    I64Extend8S        = 0xC2, // Math operation
    I64Extend16S       = 0xC3, // Math operation
    I64Extend32S       = 0xC4, // Math operation

    RefNull            = 0xD0, // Push 0
    RefIsNull          = 0xD1, // Push (Pop == 0)
    RefFunc            = 0xD2, // Push &Fn

    System             = 0xFC, // System instruction (extended by system_instruction)
    Vector             = 0xFD, // Vector instruction (extended by vector_instruction)
} // enum Opcode

/// TODO Fix unsafe
impl TryFrom<u8> for Main {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(unsafe { std::mem::transmute::<u8, Main>(value) })
    }
}

/// System (FCh) opcode extender
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum System {
    I32TruncSatF32S =  0, // Trunc i32 into f32 as signed
    I32TruncSatF32U =  1, // Trunc i32 into f32 as unsigned
    I32TruncSatF64S =  2, // Trunc i32 into f64 as signed
    I32TruncSatF64U =  3, // Trunc i32 into f64 as unsigned
    I64TruncSatF32S =  4, // Trunc i64 into f32 as signed
    I64TruncSatF32U =  5, // Trunc i64 into f32 as unsigned
    I64TruncSatF64S =  6, // Trunc i64 into f64 as signed
    I64TruncSatF64U =  7, // Trunc i64 into f64 as unsigned
    MemoryInit      =  8, // Initialize memory
    DataDrop        =  9, // Drop data segment (optimization hint)
    MemoryCopy      = 10, // Copy from wasm data segment
    MemoryFill      = 11, // Fill memory
    TableInit       = 12, // Initialize new table
    TableDrop       = 13, // Drop table
    TableCopy       = 14, // Copy to another one table
    TableGrow       = 15, // Grow table
    TableSize       = 16, // Resize table
    TableFill       = 17, // Fill table
} // enum SystemOpcode

/// Vector (FDh) opcode extender
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Vector {
    V128Load                  = 0x00,
    V128Load8x8S              = 0x01,
    V128Load8x8U              = 0x02,
    V128Load16x4S             = 0x03,
    V128Load16x4U             = 0x04,
    V128Load32x2S             = 0x05,
    V128Load32x2U             = 0x06,
    V128Load8Splat            = 0x07,
    V128Load16Splat           = 0x08,
    V128Load32Splat           = 0x09,
    V128Load64Splat           = 0x0A,
    V128Load32Zero            = 0x5C,
    V128Load64Zero            = 0x5D,
    V128Store                 = 0x0B,
    V128Load8Lane             = 0x54,
    V128Load16Lane            = 0x55,
    V128Load32Lane            = 0x56,
    V128Load64Lane            = 0x57,
    V128Store8Lane            = 0x58,
    V128Store16Lane           = 0x59,
    V128Store32Lane           = 0x5A,
    V128Store64Lane           = 0x5B,
    V128Const                 = 0x0C,

    I8x16Shuffle              = 0x0D,

    I8x16ExtractLaneS         = 0x15,
    I8x16ExtractLaneU         = 0x16,
    I8x16ReplaceLane          = 0x17,

    I16x8ExtractLaneS         = 0x18,
    I16x8ExtractLaneU         = 0x19,
    I16x8ReplaceLane          = 0x1A,

    I32x4ExtractLane          = 0x1B,
    I32x4ReplaceLane          = 0x1C,

    I64x2ExtractLane          = 0x1D,
    I64x2ReplaceLane          = 0x1E,

    F32x4ExtractLane          = 0x1F,
    F32x4ReplaceLane          = 0x20,

    F64x2ExtractLane          = 0x21,
    F64x2ReplaceLane          = 0x22,

    I8x16Swizzle              = 0x0E,

    I8x16Splat                = 0x0F,
    I16x8Splat                = 0x10,
    I32x4Splat                = 0x11,
    I64x2Splat                = 0x12,
    F32x4Splat                = 0x13,
    F64x2Splat                = 0x14,

    I8x16Eq                   = 0x23,
    I8x16Ne                   = 0x24,
    I8x16LtS                  = 0x25,
    I8x16LtU                  = 0x26,
    I8x16GtS                  = 0x27,
    I8x16GtU                  = 0x28,
    I8x16LeS                  = 0x29,
    I8x16LeU                  = 0x2A,
    I8x16GeS                  = 0x2B,
    I8x16GeU                  = 0x2C,

    I16x8Eq                   = 0x2D,
    I16x8Ne                   = 0x2E,
    I16x8LtS                  = 0x2F,
    I16x8LtU                  = 0x30,
    I16x8GtS                  = 0x31,
    I16x8GtU                  = 0x32,
    I16x8LeS                  = 0x33,
    I16x8LeU                  = 0x34,
    I16x8GeS                  = 0x35,
    I16x8GeU                  = 0x36,

    I32x4Eq                   = 0x37,
    I32x4Ne                   = 0x38,
    I32x4LtS                  = 0x39,
    I32x4LtU                  = 0x3A,
    I32x4GtS                  = 0x3B,
    I32x4GtU                  = 0x3C,
    I32x4LeS                  = 0x3D,
    I32x4LeU                  = 0x3E,
    I32x4GeS                  = 0x3F,
    I32x4GeU                  = 0x40,

    I64x2Eq                   = 0xD6,
    I64x2Ne                   = 0xD7,
    I64x2LtS                  = 0xD8,
    I64x2GtS                  = 0xD9,
    I64x2LeS                  = 0xDA,
    I64x2GeS                  = 0xDB,

    F32x4Eq                   = 0x41,
    F32x4Ne                   = 0x42,
    F32x4Lt                   = 0x43,
    F32x4Gt                   = 0x44,
    F32x4Le                   = 0x45,
    F32x4Ge                   = 0x46,

    F64x2Eq                   = 0x47,
    F64x2Ne                   = 0x48,
    F64x2Lt                   = 0x49,
    F64x2Gt                   = 0x4A,
    F64x2Le                   = 0x4B,
    F64x2Ge                   = 0x4C,

    V128Not                   = 0x4D,
    V128And                   = 0x4E,
    V128Andnot                = 0x4F,
    V128Or                    = 0x50,
    V128Xor                   = 0x51,
    V128Bitselect             = 0x52,
    V128AnyTrue               = 0x53,

    I8x16Abs                  = 0x60,
    I8x16Neg                  = 0x61,
    I8x16Popcnt               = 0x62,
    I8x16AllTrue              = 0x63,
    I8x16Bitmask              = 0x64,
    I8x16NarrowI16x8S         = 0x65,
    I8x16NarrowI16x8U         = 0x66,
    I8x16Shl                  = 0x6B,
    I8x16ShrS                 = 0x6C,
    I8x16ShrU                 = 0x6D,
    I8x16Add                  = 0x6E,
    I8x16AddSatS              = 0x6F,
    I8x16AddSatU              = 0x70,
    I8x16Sub                  = 0x71,
    I8x16SubSatS              = 0x72,
    I8x16SubSatU              = 0x73,
    I8x16MinS                 = 0x76,
    I8x16MinU                 = 0x77,
    I8x16MaxS                 = 0x78,
    I8x16MaxU                 = 0x79,
    I8x16AvgrU                = 0x7B,
    I16x8ExtaddPairwiseI8x16S = 0x7C,
    I16x8ExtaddPairwiseI8x16U = 0x7D,

    I16x8Abs                  = 0x80,
    I16x8Neg                  = 0x81,
    I16x8Q15mulrSatS          = 0x82,
    I16x8AllTrue              = 0x83,
    I16x8Bitmask              = 0x84,
    I16x8NarrowI32x4S         = 0x85,
    I16x8NarrowI32x4U         = 0x86,
    I16x8ExtendLowI8x16S      = 0x87,
    I16x8ExtendHighI8x16S     = 0x88,
    I16x8ExtendLowI8x16U      = 0x89,
    I16x8ExtendHighI8x16U     = 0x8A,
    I16x8Shl                  = 0x8B,
    I16x8ShrS                 = 0x8C,
    I16x8ShrU                 = 0x8D,
    I16x8Add                  = 0x8E,
    I16x8AddSatS              = 0x8F,
    I16x8AddSatU              = 0x90,
    I16x8Sub                  = 0x91,
    I16x8SubSatS              = 0x92,
    I16x8SubSatU              = 0x93,
    I16x8Mul                  = 0x95,
    I16x8MinS                 = 0x96,
    I16x8MinU                 = 0x97,
    I16x8MaxS                 = 0x98,
    I16x8MaxU                 = 0x99,
    I16x8AvgrU                = 0x9B,
    I16x8ExtmulLowI8x16S      = 0x9C,
    I16x8ExtmulHighI8x16S     = 0x9D,
    I16x8ExtmulLowI8x16U      = 0x9E,
    I16x8ExtmulHighI8x16U     = 0x9F,

    I32x4ExtaddPairwiseI16x8S = 0x7E,
    I32x4ExtaddPairwiseI16x8U = 0x7F,
    I32x4Abs                  = 0xA0,
    I32x4Neg                  = 0xA1,
    I32x4AllTrue              = 0xA3,
    I32x4Bitmask              = 0xA4,
    I32x4ExtendLowI16x8S      = 0xA7,
    I32x4ExtendHighI16x8S     = 0xA8,
    I32x4ExtendLowI16x8U      = 0xA9,
    I32x4ExtendHighI16x8U     = 0xAA,
    I32x4Shl                  = 0xAB,
    I32x4ShrS                 = 0xAC,
    I32x4ShrU                 = 0xAD,
    I32x4Add                  = 0xAE,
    I32x4Sub                  = 0xB1,
    I32x4Mul                  = 0xB5,
    I32x4MinS                 = 0xB6,
    I32x4MinU                 = 0xB7,
    I32x4MaxS                 = 0xB8,
    I32x4MaxU                 = 0xB9,
    I32x4DotI16x8S            = 0xBA,
    I32x4ExtmulLowI16x8S      = 0xBC,
    I32x4ExtmulHighI16x8S     = 0xBD,
    I32x4ExtmulLowI16x8U      = 0xBE,
    I32x4ExtmulHighI16x8U     = 0xBF,

    I64x2Abs                  = 0xC0,
    I64x2Neg                  = 0xC1,
    I64x2AllTrue              = 0xC3,
    I64x2Bitmask              = 0xC4,
    I64x2ExtendLowI32x4S      = 0xC7,
    I64x2ExtendHighI32x4S     = 0xC8,
    I64x2ExtendLowI32x4U      = 0xC9,
    I64x2ExtendHighI32x4U     = 0xCA,
    I64x2Shl                  = 0xCB,
    I64x2ShrS                 = 0xCC,
    I64x2ShrU                 = 0xCD,
    I64x2Add                  = 0xCE,
    I64x2Sub                  = 0xD1,
    I64x2Mul                  = 0xD5,
    I64x2ExtmulLowI32x4S      = 0xDC,
    I64x2ExtmulHighI32x4S     = 0xDD,
    I64x2ExtmulLowI32x4U      = 0xDE,
    I64x2ExtmulHighI32x4U     = 0xDF,

    F32x4Ceil                 = 0x67,
    F32x4Floor                = 0x68,
    F32x4Trunc                = 0x69,
    F32x4Nearest              = 0x6A,
    F32x4Abs                  = 0xE0,
    F32x4Neg                  = 0xE1,
    F32x4Sqrt                 = 0xE3,
    F32x4Add                  = 0xE4,
    F32x4Sub                  = 0xE5,
    F32x4Mul                  = 0xE6,
    F32x4Div                  = 0xE7,
    F32x4Min                  = 0xE8,
    F32x4Max                  = 0xE9,
    F32x4Pmin                 = 0xEA,
    F32x4Pmax                 = 0xEB,

    F64x2Ceil                 = 0x74,
    F64x2Floor                = 0x75,
    F64x2Trunc                = 0x7A,
    F64x2Nearest              = 0x94,
    F64x2Abs                  = 0xEC,
    F64x2Neg                  = 0xED,
    F64x2Sqrt                 = 0xEF,
    F64x2Add                  = 0xF0,
    F64x2Sub                  = 0xF1,
    F64x2Mul                  = 0xF2,
    F64x2Div                  = 0xF3,
    F64x2Min                  = 0xF4,
    F64x2Max                  = 0xF5,
    F64x2Pmin                 = 0xF6,
    F64x2Pmax                 = 0xF7,

    I32x4TruncSatF32x4S       = 0xF8,
    I32x4TruncSatF32x4U       = 0xF9,
    F32x4ConvertI32x4S        = 0xFA,
    F32x4ConvertI32x4U        = 0xFB,
    I32x4TruncSatF64x2SZero   = 0xFC,
    I32x4TruncSatF64x2UZero   = 0xFD,
    F64x2ConvertLowI32x4S     = 0xFE,
    F64x2ConvertLowI32x4U     = 0xFF,
    F32x4DemoteF64x2Zero      = 0x5E,
    F64x2PromoteLowF32x4      = 0x5F,
} // enum VectorOpcode

impl TryInto<Instruction> for Main {
    type Error = ();

    fn try_into(self) -> Result<Instruction, Self::Error> {
        match self {
            Main::Unreachable          => Ok(Instruction::Unreachable),
            Main::Nop                  => Ok(Instruction::Nop),
            Main::Block                => Ok(Instruction::Block),
            Main::Loop                 => Ok(Instruction::Loop),
            Main::If                   => Ok(Instruction::If),
            Main::Else                 => Ok(Instruction::Else),
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
            Main::I32ReinterpretF32    => Ok(Instruction::I32ReinterpretF32),
            Main::I64ReinterpretF64    => Ok(Instruction::I64ReinterpretF64),
            Main::F32ReinterpretI32    => Ok(Instruction::F32ReinterpretI32),
            Main::F64ReinterpretI64    => Ok(Instruction::F64ReinterpretI64),
            Main::I32Extend8S          => Ok(Instruction::I32Extend8S),
            Main::I32Extend16S         => Ok(Instruction::I32Extend16S),
            Main::I64Extend8S          => Ok(Instruction::I64Extend8S),
            Main::I64Extend16S         => Ok(Instruction::I64Extend16S),
            Main::I64Extend32S         => Ok(Instruction::I64Extend32S),
            Main::RefNull              => Ok(Instruction::RefNull),
            Main::RefIsNull            => Ok(Instruction::RefIsNull),
            Main::RefFunc              => Ok(Instruction::RefFunc),
            Main::System               => Ok(Instruction::System),
            Main::Vector               => Ok(Instruction::Vector),
            _ => Err(()),
        }
    }
}

// file bin.rs
