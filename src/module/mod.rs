mod decode_wasm;
mod opcode;

use std::{collections::HashMap, string::FromUtf8Error};

use crate::{types::{self, TableType}, Expression, Function, Limits, Module};

pub enum Source<'t> {
    WASM(&'t [u8]),
    WAT(&'t str),
}

pub struct GlobalDescriptor {
    pub value_type: types::ValueType,
    pub mutability: types::Mutability,
    pub expression: Expression,
}

pub enum DataType {
    /// Active - will be written to default memory during module initialization
    ActiveToDefaultMemory = 0,
    /// Passive - can be written to some memory during module function execution
    Passive = 1,
    /// Active - will be written to some memory during module initialization
    ActiveToSomeMemory = 2,
}

pub enum DataDescriptor {
    ActiveToDefaultMemory {
        offset: u32,
    },
    Passive,
    ActiveToSomeMemory {
        memory: u32,
        offset: u32,
    },
}

impl DataDescriptor {
    /// Data descriptor getting function
    pub fn get_type(&self) -> DataType {
        match self {
            DataDescriptor::ActiveToDefaultMemory { .. } => DataType::ActiveToDefaultMemory,
            DataDescriptor::ActiveToSomeMemory { .. } => DataType::ActiveToSomeMemory,
            DataDescriptor::Passive => DataType::Passive,
        }
    }
}

pub struct Data {
    pub data: DataDescriptor,
    pub content: Vec<usize>,
}

pub struct ModuleImpl {
    pub(crate) types: Vec<types::FunctionType>,
    pub(crate) imports: HashMap<String, HashMap<String, types::ImportDescriptor>>,
    pub(crate) exports: HashMap<String, types::ExportDescriptor>,
    pub(crate) globals: Vec<GlobalDescriptor>,
    pub(crate) functions: Vec<Function>,
    pub(crate) memories: Vec<Limits>,
    pub(crate) tables: Vec<TableType>,
    pub(crate) start: Option<u32>,
}

impl ModuleImpl {
    pub fn new(source: Source) -> Result<Self, ModuleCreateError> {
        match source {
            Source::WASM(bits) => Self::from_wasm(bits).map_err(ModuleCreateError::WASMDeocdeError),
            Source::WAT(_) => Err(ModuleCreateError::Unknown),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum WASMDecodeError {
    UnknownOpcode(u8),
    UnknownSectionId(u8),
    UnknownLimitType(u8),
    UnknownReferenceType(u8),
    UnknownExportType(u8),
    UnknownMutability(u8),
    UnknownValueType(u8),

    FunctionTypesSizeAndCodeSizeUnmatched,
    Utf8DecodeError,
    InvalidModuleMagic,
    UnsignedDecodeError,
    SignedDecodeError { bit_count: u32 },
    UnexpectedStreamEnd,

    ValidationError,
    UnsupportedFeature,
}

impl std::fmt::Display for WASMDecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let b: u8;
        match *self {
            Self::UnknownOpcode(byte) => write!(f, "unknown opcode: {}", byte),
            Self::UnknownSectionId(byte) => write!(f, "unknown section id: {}", byte),
            Self::UnknownLimitType(byte) => write!(f, "unknown limit type: {}", byte),
            Self::UnknownReferenceType(byte) => write!(f, "unknown reference type: {}", byte),
            Self::UnknownExportType(byte) => write!(f, "unknown : {}", byte),
            Self::UnknownMutability(byte) => write!(f, "unknown : {}", byte),
            Self::UnknownValueType(byte) => write!(f, "unknown : {}", byte),
            Self::FunctionTypesSizeAndCodeSizeUnmatched => write!(f, "count of function type index section elements unmatched with code section element count"),
            Self::Utf8DecodeError => write!(f, "utf8 decode error"),
            Self::InvalidModuleMagic => write!(f, "invalid module magic"),
            Self::UnsignedDecodeError => write!(f, "unsigned decode error"),
            Self::SignedDecodeError { bit_count }  => write!(f, "error durnig {}-bit signed decode", bit_count),
            Self::UnexpectedStreamEnd => write!(f, "unexpected stream end"),
            Self::ValidationError => write!(f, "validation error"),
            Self::UnsupportedFeature => write!(f, "unsupported feature (e.g. system/vector extension occured)"),
        }
    }
}

impl Into<ModuleCreateError> for WASMDecodeError {
    fn into(self) -> ModuleCreateError {
        ModuleCreateError::WASMDeocdeError(self)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ModuleCreateError {
    WATDecodeError,
    WASMDeocdeError(WASMDecodeError),
    Unknown,
} // enum ModuleCreateError

impl std::fmt::Display for ModuleCreateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ModuleCreateError::WASMDeocdeError(_) => "WASM file contents decode error",
            ModuleCreateError::WATDecodeError => "WAT file contents decode error",
            ModuleCreateError::Unknown => "unknown",
        })
    }
}
