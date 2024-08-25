mod block_decode;
mod opcode;

pub type Opcode = opcode::Main;
pub type VectorOpcode = opcode::Vector;
pub type SystemOpcode = opcode::System;

use std::collections::HashMap;

use block_decode::decode_block;

use crate::{instruction, types::{self, FunctionType}, util::binary_stream::BinaryInputStream, Function, Mutability, Type};
use super::{Expression, GlobalDescriptor, ModuleImpl};

/// WASM enumeration type representation structure
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum EnumType {
    /// Opcode
    Opcode,

    /// System opcode
    SystemOpcode,

    /// Vector opcode
    VectorOpcode,

    /// Section identifier
    SectionId,

    /// Limit type
    LimitType,

    /// Reference type
    ReferenceType,

    /// Export type
    ExportType,

    /// Global mutability
    Mutability,

    /// Value type
    Type,
} // enum EnumType

impl std::fmt::Display for EnumType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match *self {
            Self::Opcode => "opcode",
            Self::SectionId => "section id",
            Self::LimitType => "limit type",
            Self::ReferenceType => "reference type",
            Self::ExportType => "export type",
            Self::Mutability => "mutability",
            Self::Type => "value type",
            Self::SystemOpcode => "system opcode",
            Self::VectorOpcode => "vector opcode",
        })
    } // fn fmt
} // impl std::fmt::Display for EnumType

impl EnumType {
    /// Enumeration type into decode error transforming function
    /// * `value` - byte, that caused decode error
    /// * Returns decode error  
    pub fn as_decode_error(self, value: u8) -> DecodeError {
        DecodeError::EnumError { ty: self, value }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum DecodeError {
    /// Enumeration decode error
    EnumError {
        /// Expected enumeration type
        ty: EnumType,

        /// Enumeration value
        value: u8,
    },

    /// Sizes unmatched
    FunctionTypesSizeAndCodeSizeUnmatched,

    /// UTF8 name decode error
    Utf8DecodeError,

    /// Invalid module magic number
    InvalidModuleMagic,

    /// Unsigned number decode error
    UnsignedDecodeError,

    /// Signed decode error
    SignedDecodeError {
        /// Count of decoded bits
        bit_count: u32
    },

    /// Unexpected stream end
    UnexpectedStreamEnd,

    /// Unsupported WASM feature
    UnsupportedFeature,

    /// Invalid block end
    InvalidBlockEnd,

    /// Execution validation error
    CodeValidationError(CodeValidationError),
} // enum DecodeError

impl std::fmt::Display for DecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::EnumError { ty, value } => write!(f, "unknown {}: {}", ty, value),
            Self::FunctionTypesSizeAndCodeSizeUnmatched => write!(f, "count of function type index section elements unmatched with code section element count"),
            Self::Utf8DecodeError => write!(f, "utf8 decode error"),
            Self::InvalidModuleMagic => write!(f, "invalid module magic"),
            Self::UnsignedDecodeError => write!(f, "unsigned decode error"),
            Self::SignedDecodeError { bit_count } => write!(f, "error durnig {}-bit signed decode", bit_count),
            Self::UnexpectedStreamEnd => write!(f, "unexpected stream end"),
            Self::CodeValidationError(err) => write!(f, "code validation error: {}", err),
            Self::UnsupportedFeature => write!(f, "unsupported feature (e.g. system/vector extension occured)"),
            Self::InvalidBlockEnd => write!(f, "invalid block ending opcode"),
        }
    } // fn fmt
} // impl std::fmt::Display for DecodeError

/// Code execution validation error
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CodeValidationError {
    /// Unexpected expression end
    UnexpectedExpressionEnd(u8),

    /// Invalid stack top
    InvalidStackTop,

    /// Lack of types in typed select
    NoTypesInTypedSelect,

    /// Invalid memory instruction contents
    InvalidMemoryInstructionData,

    /// Unknown function type index
    UnknownFunctionTypeIndex,

    /// Unknown function index
    UnknownFunctionIndex,

    /// Unknown local variable index
    UnknownLocalIndex,

    /// Unknown global variable index
    UnknownGlobalIndex,

    /// Unexpected else opcode occurence
    UnexpectedElseOpcode,

    /// Invalid branching depth
    InvalidBranchDepth,

    /// No operands for math operatoin
    NoOperands,

    /// Trying to mutate constant global variable
    MutatingConstantGlobal,

    /// Unexpected type of operand
    UnexpectedOperandType {
        /// Expected type
        expected: Type,

        /// Actual type
        actual: Type,
    },
} // enum CodeValidationError

impl From<CodeValidationError> for DecodeError {
    fn from(value: CodeValidationError) -> Self {
        Self::CodeValidationError(value)
    }
} // impl From<CodeValidationError> for DecodeError

impl std::fmt::Display for CodeValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "todo!")
    }
} // impl std::fmt::Display for CodeValidationError

// Own binary stream implementation
impl<'t> BinaryInputStream<'t> {
    /// Vector decode function
    /// * `decode_func` - function to decode values by
    /// * Returns vector of decoded values
    pub fn wasm_decode_vector<DT: bytemuck::AnyBitPattern, T>(&mut self, decode_func: &dyn Fn(&DT) -> Option<T>) -> Option<Vec<T>> {
        let len = self.decode_unsigned()?;
        self
            .get_slice::<DT>(len)?
            .iter()
            .map(decode_func)
            .collect()
    } // fn wasm_decode_error

    /// Limits decode function
    /// * Returns decoded limits
    pub fn wasm_decode_limits(&mut self) -> Result<types::Limits, DecodeError> {
        let t = self.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;

        match t {
            0 => Ok(types::Limits { min: self.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)? as u32, max: None }),
            1 => Ok(types::Limits { min: self.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)? as u32, max: Some(self.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)? as u32) }),
            _ => Err(EnumType::LimitType.as_decode_error(t)),
        }
    } // fn wasm_decode_limits

    /// Block type decode function
    /// * Returns block type
    pub fn wasm_decode_block_type(&mut self) -> Result<instruction::BlockType, DecodeError> {
        let byte = self.check::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;

        Ok(if byte == 0x40 {
            self.skip(1).unwrap();
            instruction::BlockType::Void
        } else if let Ok(val_type) = types::Type::try_from(byte) {
            self.skip(1).unwrap();
            instruction::BlockType::ResolvingTo(val_type)
        } else {
            instruction::BlockType::Functional(self.decode_signed::<32>().ok_or(DecodeError::SignedDecodeError { bit_count: 32 })? as u32)
        })
    } // fn wasm_decode_block_type

    /// Unsigned number decode function
    /// * Returns decoded number
    pub fn wasm_decode_unsigned(&mut self) -> Result<usize, DecodeError> {
        self.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)
    } // fn wasm_decode_unsigned
} // impl BinaryInputStream

/// Section table decode function
/// * `bits` - sections data
/// * Returns map with (section type -> section bits) map
fn decode_sections<'t>(bits: &'t [u8]) -> Result<HashMap<types::SectionID, &'t [u8]>, DecodeError> {
    let mut stream = BinaryInputStream::new(bits);

    if !stream.get::<types::Header>().ok_or(DecodeError::UnexpectedStreamEnd)?.validate() {
        return Err(DecodeError::UnexpectedStreamEnd);
    }

    let mut sections = HashMap::<types::SectionID, &[u8]>::new();

    while let Some(section_id) = stream.get::<u8>() {
        let section_id = types::SectionID::try_from(section_id).map_err(|_| EnumType::SectionId.as_decode_error(section_id))?;
        let length = stream.decode_unsigned().ok_or(DecodeError::UnexpectedStreamEnd)?;
        let data = stream.get_byte_slice(length).ok_or(DecodeError::UnexpectedStreamEnd)?;
        sections.insert(section_id, data);
    }

    Ok(sections)
} // fn decode_section

/// Function type section decode function
/// * section - section bits
/// * Returns vector of decoded function types
fn decode_function_type_section(section: &[u8]) -> Result<Vec<types::FunctionType>, DecodeError> {
    let mut stream = BinaryInputStream::new(section);

    let type_count = stream.decode_unsigned().ok_or(DecodeError::UnexpectedStreamEnd)?;

    (0..type_count).map(|_| {
            let functype_magic = stream.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;

            if functype_magic != types::WASM_FUNCTYPE_MAGIC {
                return Err(DecodeError::InvalidModuleMagic);
            }

            const DECODE_FUNC: &'static dyn Fn(&u8) -> Option<Type> = &|v: &u8| Type::try_from(*v).ok();

            Ok(types::FunctionType {
                inputs: stream.wasm_decode_vector(DECODE_FUNC).ok_or(DecodeError::UnexpectedStreamEnd)?,
                outputs: stream.wasm_decode_vector(DECODE_FUNC).ok_or(DecodeError::UnexpectedStreamEnd)?,
            })
        })
        .collect::<Result<Vec<types::FunctionType>, DecodeError>>()
} // fn decode_function_type_section

/// Function section decode function
/// * `section` - section byte array
/// * Returns vector of function type indices
fn decode_function_section(section: &[u8]) -> Result<Vec<u32>, DecodeError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(DecodeError::UnexpectedStreamEnd)?)
        .map(|_| stream
            .decode_unsigned()
            .map(|v| v as u32)
            .ok_or(DecodeError::UnexpectedStreamEnd)
        )
        .collect::<Result<Vec<u32>, DecodeError>>()
} // fn decode_function_section

/// Table section decode function
/// * `section` - binary section contents
/// * Returns vector of table types or module create error
fn decode_table_section(section: &[u8]) -> Result<Vec<types::TableType>, DecodeError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(DecodeError::UnexpectedStreamEnd)?)
        .map(|_| {
            let type_byte = stream.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;

            Ok(types::TableType {
                reference_type: TryInto::<types::ReferenceType>::try_into(type_byte).map_err(|_| EnumType::ReferenceType.as_decode_error(type_byte))?,
                limits: stream.wasm_decode_limits()?,
            })
        })
        .collect::<Result<Vec<types::TableType>, DecodeError>>()
} // fn decode_table_section

/// Memory section decode function
/// * `section` - binary section contents
/// * Returns vector of memory size limits or module create error
fn decode_memory_section(section: &[u8]) -> Result<Vec<types::Limits>, DecodeError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(DecodeError::UnexpectedStreamEnd)?)
        .map(|_| stream.wasm_decode_limits())
        .collect::<Result<Vec<types::Limits>, DecodeError>>()
} // fn decode_memory_section

/// Export section decode function
/// * `section` - binary section contents
/// * Returns hashmap of export object descriptors by exported names or module create error
fn decode_export_section(section: &[u8]) -> Result<HashMap<String, types::ExportDescriptor>, DecodeError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)?)
        .map(|_| -> Result<(String, types::ExportDescriptor), DecodeError> {
            let name = {
                let len = stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)?;
                String::from_utf8(stream.get_byte_slice(len).ok_or(DecodeError::UnsignedDecodeError)?.to_vec()).map_err(|_| DecodeError::Utf8DecodeError)?
            };
            let type_byte = stream.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;
            let desc = types::ExportDescriptor {
                ty: type_byte.try_into().map_err(|_| EnumType::ExportType.as_decode_error(type_byte))?,
                index: stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)? as u32,
            };

            Ok((name, desc))
        })
        .collect::<Result<HashMap<String, types::ExportDescriptor>, DecodeError>>()
} // fn decode_export_section

/// Start section decode function
/// * `section` - binary section contents
/// * Returns index of start section decoding or module create error
fn decode_start_section(section: &[u8]) -> Result<u32, DecodeError> {
    BinaryInputStream::new(section)
        .decode_unsigned()
        .map(|v| v as u32)
        .ok_or(DecodeError::UnsignedDecodeError)
} // fn decode_start_section

/// Code section decode function
/// * `section` - section bytes
/// * Returns vector of function bytecode blocks
fn decode_code_section(section: &[u8]) -> Result<Vec<&[u8]>, DecodeError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)?)
        .map(|_| -> Result<&[u8], DecodeError> {
            let length = stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)?;

            Ok(stream.get_byte_slice(length).ok_or(DecodeError::UnexpectedStreamEnd)?)
        })
        .collect::<Result<Vec<&[u8]>, DecodeError>>()
} // fn decode_code_section

/// Global section decode function
/// * `section` - section bytes
/// * `function_types` - known function types
/// * `function_type_idx` - indices of function types
/// * Returns global value descriptors
fn decode_global_section<'t>(
    section: &'t [u8],
    function_types: &'t [FunctionType],
    function_type_idx: &'t [u32],
) -> Result<Vec<GlobalDescriptor>, DecodeError> {
    let mut stream = BinaryInputStream::new(&section);

    (0..stream.wasm_decode_unsigned()?)
        .map(|_| {
            let value_type: Type = {
                let byte = stream.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;
                byte
                    .try_into()
                    .map_err(|_| EnumType::Type.as_decode_error(byte))?
            };
    
            let mutability: Mutability = {
                let byte = stream.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;
                byte
                    .try_into()
                    .map_err(|_| EnumType::Mutability.as_decode_error(byte))?
            };

            Ok(GlobalDescriptor {
                value_type,
                mutability,
                expression: decode_expression(
                    &mut stream,
                    function_types,
                    function_type_idx,
                    &[],
                    &[],
                    &[],
                    &[value_type]
                )?,
            })
        })
        .collect()
} // fn decode_global_section

/// Expression decode function
/// * `stream` - expression bytecode
/// * `function_types` - known function types
/// * `function_type_idx` - mapping (function index -> function type index)
/// * `locals` - local variables in expression
/// * `globals` - global variables in expression
/// * `inputs` - function stack input types
/// * `outputs` - function stack output types
/// * Returns decoded expression
fn decode_expression<'b, 't>(
    stream: &'b mut BinaryInputStream<'t>,
    function_types: &'b [FunctionType],
    function_type_idx: &'b [u32],
    locals: &'b [Type],
    globals: &'b [GlobalDescriptor],
    inputs: &'b [Type],
    outputs: &'b [Type],
) -> Result<Expression, DecodeError>
where 't: 'b {
    let instructions = decode_block(
        stream,
        function_types,
        function_type_idx,
        locals,
        globals,
        inputs,
        outputs,
    )?;

    return Ok(Expression { instructions })
} // fn decode_expression

impl ModuleImpl {
    /// Module from WASM bytecode decode function
    /// * `src` - module bytecode
    /// * Returns decoded WASM module
    pub fn from_wasm(src: &[u8]) -> Result<Self, DecodeError> {
        // Sections
        let sections = decode_sections(src)?;

        let function_types = if let Some(bits) = sections.get(&types::SectionID::Type) {
            decode_function_type_section(bits)?
        } else {
            Vec::new()
        };

        let function_type_idx = if let Some(bits) = sections.get(&types::SectionID::Function) {
            decode_function_section(bits)?
        } else {
            Vec::new()
        };

        let tables = if let Some(bits) = sections.get(&types::SectionID::Table) {
            decode_table_section(bits)?
        } else {
            Vec::new()
        };

        let memories = if let Some(bits) = sections.get(&types::SectionID::Memory) {
            decode_memory_section(bits)?
        } else {
            Vec::new()
        };

        let globals: Vec<GlobalDescriptor> = if let Some(bits) = sections.get(&types::SectionID::Global) {
            decode_global_section(
                bits,
                &function_types,
                &function_type_idx,
            )?
        } else {
            Vec::new()
        };

        let exports = if let Some(bits) = sections.get(&types::SectionID::Export) {
            decode_export_section(bits)?
        } else {
            HashMap::new()
        };

        let start = if let Some(bits) = sections.get(&types::SectionID::Start) {
            Some(decode_start_section(bits)?)
        } else {
            None
        };

        let code = if let Some(bits) = sections.get(&types::SectionID::Code) {
            decode_code_section(bits)?
        } else {
            Vec::new()
        };

        if function_type_idx.len() != code.len() {
            return Err(DecodeError::FunctionTypesSizeAndCodeSizeUnmatched);
        }

        let functions = function_type_idx.iter().map(|v| *v).zip(code.iter()).map(|(type_id, code)| -> Result<_, DecodeError> {
            let func_ty = function_types.get(type_id as usize).ok_or(CodeValidationError::UnknownFunctionIndex)?;

            let mut stream = BinaryInputStream::new(code);

            let locals = {
                let mut locals = Vec::<Type>::new();

                for _ in 0..stream.wasm_decode_unsigned()? {
                    let local_repeat_count = stream.wasm_decode_unsigned()?;
                    let valtype_byte = stream.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;
                    let ty: Type = valtype_byte.try_into().map_err(|_| EnumType::Type.as_decode_error(valtype_byte))?;

                    locals.extend(std::iter::repeat(ty).take(local_repeat_count));
                }

                locals
            };

            // local varaibles used during function execution
            let exec_locals = {
                let mut l = Vec::new();

                l.extend_from_slice(&func_ty.inputs);
                l.extend_from_slice(&locals);

                l
            };

            Ok(Function {
                expression: decode_expression(
                    &mut stream,
                    &function_types,
                    &function_type_idx,
                    &exec_locals,
                    &globals,
                    &[],
                    &func_ty.outputs,
                )?,
                type_id,
                locals,
            })
        }).collect::<Result<Vec<Function>, DecodeError>>()?;

        Ok(ModuleImpl {
            exports,
            functions,
            globals,
            tables,
            memories,
            imports: HashMap::new(),
            types: function_types,
            start,
        })
    } // fn from_wasm
} // impl Module

// file mod.rs
