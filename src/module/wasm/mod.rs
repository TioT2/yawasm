//! WAT->Module parser implementation module

use std::collections::HashMap;

use stream::Stream;

use super::{Module, NumberType, ReferenceType, Type, VectorType};

mod opcode;
mod stream;

fn parse_number_type(value: u8) -> Option<NumberType> {
    Some(match value {
        0x7C => NumberType::I32,
        0x7D => NumberType::F32,
        0x7E => NumberType::I64,
        0x7F => NumberType::F64,
        _ => return None,
    })
}

fn parse_vector_type(value: u8) -> Option<VectorType> {
    if value == 0x7B {
        Some(VectorType::V128)
    } else {
        None
    }
}

fn parse_reference_type(value: u8) -> Option<ReferenceType> {
    Some(match value {
        0x6F => ReferenceType::Extern,
        0x70 => ReferenceType::Func,
        _ => return None,
    })
}

fn parse_type(value: u8) -> Result<Type, DecodeError> {
    Ok(match value {
        0x6F => Type::ExternRef,
        0x70 => Type::FuncRef,
        0x7B => Type::V128,
        0x7F => Type::I32,
        0x7E => Type::I64,
        0x7D => Type::F32,
        0x7C => Type::F64,
        _ => return Err(EnumType::Type.as_decode_error(value)),
    })
}

fn parse_section_id(value: u8) -> Result<SectionId, DecodeError> {
    Ok(match value {
        0  => SectionId::Custom,
        1  => SectionId::Type,
        2  => SectionId::Import,
        3  => SectionId::Function,
        4  => SectionId::Table,
        5  => SectionId::Memory,
        6  => SectionId::Global,
        7  => SectionId::Export,
        8  => SectionId::Start,
        9  => SectionId::Element,
        10 => SectionId::Code,
        11 => SectionId::Data,
        12 => SectionId::DataCount,
        _ => return Err(EnumType::SectionId.as_decode_error(value)),
    })
}

/// Section identifier representation enumeration
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum SectionId {
    Custom = 0,
    Type = 1,
    Import = 2,
    Function = 3,
    Table = 4,
    Memory = 5,
    Global = 6,
    Export = 7,
    Start = 8,
    Element = 9,
    Code = 10,
    Data = 11,
    DataCount = 12,
} // enum SectionId

/// WASM magic number
pub const WASM_MAGIC: [u8; 4] = [0x00, 0x61, 0x73, 0x6D];

/// WAMS version number
pub const WASM_VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

/// WASM functype magic
pub const WASM_FUNCTYPE_MAGIC: u8 = 0x60;

/// WASM file header representation structure
#[repr(packed)]
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct Header {
    /// Magic number
    pub magic: [u8; 4],

    /// Version magic number
    pub version: [u8; 4],
} // struct Header

unsafe impl bytemuck::Zeroable      for Header {}
unsafe impl bytemuck::AnyBitPattern for Header {}

impl Header {
    /// Header validation function
    /// * Returns true if header is correct supported WASM header
    pub fn is_valid(self) -> bool {
        self.magic == WASM_MAGIC && self.version == WASM_VERSION
    } // fn validate
}

/// WASM bytecode enumeration type
#[derive(Copy, Clone, Debug)]
pub enum EnumType {
    /// Value type
    Type,

    /// Section identifier enumeration
    SectionId,
} // enum EnumType

impl EnumType {
    pub fn as_decode_error(self, value: u8) -> DecodeError {
        DecodeError::InvalidEnum { value, ty: self }
    }
}

#[derive(Clone, Debug)]
pub enum DecodeError {
    /// Invalid WASM file header
    InvalidWasmHeader,

    /// Unexpected end of input data stream
    UnexpectedStreamEnd,

    /// Invalid enumeration case
    InvalidEnum {
        /// Enumeration value
        value: u8,

        /// Required enumeration type
        ty: EnumType,
    },

    /// Invalid unsigned LEB128 number
    InvalidUnsigned,

    /// Invalid signed LEB128 number
    InvalidSigned,
} // enum WasmDecodeError

fn parse_sections<'t>(stream: &'t mut Stream) -> Result<HashMap<SectionId, &'t [u8]>, DecodeError> {
    let header = stream.read::<Header>().ok_or(DecodeError::UnexpectedStreamEnd)?;
    if !header.is_valid() {
        return Err(DecodeError::InvalidWasmHeader);
    }

    let mut result = HashMap::new();

    while let Some(section_id_u8) = stream.read::<u8>() {
        let section_id = parse_section_id(section_id_u8)?;
        let length = stream.decode_unsigned().ok_or(DecodeError::InvalidUnsigned)?;
        let data = stream.read_byte_slice(length).ok_or(DecodeError::UnexpectedStreamEnd)?;

        result.insert(section_id, data);
    }

    Ok(result)
} // fn parse_sections

// Module from WASM building function.
pub fn parse(wasm: &[u8]) -> Result<Module, DecodeError> {
    // parse WASM sections
    let sections = parse_sections(&mut stream::Stream::new(wasm))?;


    todo!()
}

// file mod.rs
