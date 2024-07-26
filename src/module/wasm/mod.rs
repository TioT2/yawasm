mod block_decode;

use std::collections::HashMap;

use block_decode::decode_block;

use crate::{instruction, types, util::binary_stream::{BinaryInputStream, BinaryOutputStream}, Function};
use super::{opcode, Expression, GlobalDescriptor, ModuleImpl};


#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum EnumType {
    Opcode,
    SectionId,
    LimitType,
    ReferenceType,
    ExportType,
    Mutability,
    ValueType,
}

impl std::fmt::Display for EnumType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match *self {
            Self::Opcode => "opcode",
            Self::SectionId => "section id",
            Self::LimitType => "limit type",
            Self::ReferenceType => "reference type",
            Self::ExportType => "export type",
            Self::Mutability => "mutability",
            Self::ValueType => "value type",
        })
    }
}

impl EnumType {
    pub fn as_decode_error(self, value: u8) -> DecodeError {
        DecodeError::EnumError { ty: self, value }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum DecodeError {
    EnumError {
        ty: EnumType,
        value: u8,
    },

    FunctionTypesSizeAndCodeSizeUnmatched,
    Utf8DecodeError,
    InvalidModuleMagic,
    UnsignedDecodeError,
    SignedDecodeError { bit_count: u32 },
    UnexpectedStreamEnd,

    UnsupportedFeature,

    CodeValidationError(CodeValidationError),
}

impl std::fmt::Display for DecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::EnumError { ty, value } => write!(f, "unknown {}: {}", ty, value),
            Self::FunctionTypesSizeAndCodeSizeUnmatched => write!(f, "count of function type index section elements unmatched with code section element count"),
            Self::Utf8DecodeError => write!(f, "utf8 decode error"),
            Self::InvalidModuleMagic => write!(f, "invalid module magic"),
            Self::UnsignedDecodeError => write!(f, "unsigned decode error"),
            Self::SignedDecodeError { bit_count }  => write!(f, "error durnig {}-bit signed decode", bit_count),
            Self::UnexpectedStreamEnd => write!(f, "unexpected stream end"),
            Self::CodeValidationError(err) => write!(f, "code validation error: {}", err),
            Self::UnsupportedFeature => write!(f, "unsupported feature (e.g. system/vector extension occured)"),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CodeValidationError {
    UnexpectedExpressionEnd(u8),
    InvalidStackTop,

    InvalidMemoryInstructionData,
    UnknownFunctionTypeIndex,
    UnknownFunctionIndex,
}

impl Into<DecodeError> for CodeValidationError {
    fn into(self) -> DecodeError {
        todo!()
    }
}

impl std::fmt::Display for CodeValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "todo!")
    }
}

// Own binary stream implementation
impl<'t> BinaryInputStream<'t> {
    pub fn wasm_decode_vector<DT: bytemuck::AnyBitPattern, T>(&mut self, decode_func: &dyn Fn(&DT) -> Option<T>) -> Option<Vec<T>> {
        let len = self.decode_unsigned()?;
        self.get_slice::<DT>(len)?.iter().map(decode_func).collect()
    }

    pub fn wasm_decode_limits(&mut self) -> Result<types::Limits, DecodeError> {
        let t = self.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;
        match t {
            0 => Ok(types::Limits { min: self.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)? as u32, max: None }),
            1 => Ok(types::Limits { min: self.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)? as u32, max: Some(self.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)? as u32) }),
            _ => Err(EnumType::LimitType.as_decode_error(t)),
        }
    }

    pub fn wasm_decode_block_type(&mut self) -> Result<instruction::BlockType, DecodeError> {
        let byte = self.check::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;

        Ok(if byte == 0x40 {
            self.skip(1).unwrap();
            instruction::BlockType::Void
        } else if let Ok(val_type) = types::ValueType::try_from(byte) {
            self.skip(1).unwrap();
            instruction::BlockType::ResolvingTo(val_type)
        } else {
            instruction::BlockType::Functional(self.decode_signed::<32>().ok_or(DecodeError::SignedDecodeError { bit_count: 32 })? as u32)
        })
    }
}

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
}

fn decode_function_type_section(section: &[u8]) -> Result<Vec<types::FunctionType>, DecodeError> {
    let mut stream = BinaryInputStream::new(section);

    let type_count = stream.decode_unsigned().ok_or(DecodeError::UnexpectedStreamEnd)?;

    (0..type_count).map(|_| {
            let functype_magic = stream.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;

            if functype_magic != types::WASM_FUNCTYPE_MAGIC {
                return Err(DecodeError::InvalidModuleMagic);
            }

            const DECODE_FUNC: &'static dyn Fn(&u8) -> Option<types::ValueType> = &|v: &u8| types::ValueType::try_from(*v).ok();

            Ok(types::FunctionType {
                inputs: stream.wasm_decode_vector(DECODE_FUNC).ok_or(DecodeError::UnexpectedStreamEnd)?,
                outputs: stream.wasm_decode_vector(DECODE_FUNC).ok_or(DecodeError::UnexpectedStreamEnd)?,
            })
        })
        .collect::<Result<Vec<types::FunctionType>, DecodeError>>()
}

fn decode_function_section(section: &[u8]) -> Result<Vec<u32>, DecodeError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(DecodeError::UnexpectedStreamEnd)?)
        .map(|_| stream.decode_unsigned().map(|v| v as u32).ok_or(DecodeError::UnexpectedStreamEnd))
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

fn decode_code_section(section: &[u8]) -> Result<Vec<&[u8]>, DecodeError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)?)
        .map(|_| -> Result<&[u8], DecodeError> {
            let length = stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)?;

            Ok(stream.get_byte_slice(length).ok_or(DecodeError::UnexpectedStreamEnd)?)
        })
        .collect::<Result<Vec<&[u8]>, DecodeError>>()
}

fn decode_global_section(section: &[u8]) -> Result<Vec<GlobalDescriptor>, DecodeError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)?)
        .map(|_| -> Result<GlobalDescriptor, DecodeError> {
            let type_byte = stream.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;
            let mut_byte = stream.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;

            Ok(GlobalDescriptor {
                value_type: types::ValueType::try_from(type_byte).map_err(|_| EnumType::ValueType.as_decode_error(type_byte))?,
                mutability: types::Mutability::try_from(mut_byte).map_err(|_| EnumType::Mutability.as_decode_error(mut_byte))?,
                expression: decode_expression(&mut stream)?,
            })
        })
        .collect::<Result<Vec<GlobalDescriptor>, DecodeError>>()
}

fn decode_expression(stream: &mut BinaryInputStream) -> Result<Expression, DecodeError> {
    let (instructions, end) = decode_block(stream)?;

    if end != opcode::Main::ExpressionEnd as u8 {
        return Err(CodeValidationError::UnexpectedExpressionEnd(end).into());
    }

    return Ok(Expression { instructions })
}

impl ModuleImpl {
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
            decode_global_section(bits)?
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
            if function_types.len() <= type_id as usize {
                return Err(CodeValidationError::UnknownFunctionIndex.into());
            }

            let mut stream = BinaryInputStream::new(code);

            Ok(Function {
                type_id,
                locals: {
                    let mut locals = Vec::<types::ValueType>::new();

                    for _ in 0..stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)? {
                        let local_repeat_count = stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)?;
                        let valtype_byte = stream.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;
                        let ty: types::ValueType = valtype_byte.try_into().map_err(|_| EnumType::ValueType.as_decode_error(valtype_byte))?;

                        locals.extend(std::iter::repeat(ty).take(local_repeat_count));
                    }

                    locals
                },
                expression: decode_expression(&mut stream)?,
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
    }
} // impl Module
