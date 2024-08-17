mod wasm;
mod opcode;

use std::collections::HashMap;

use crate::{types::{self, TableType}, Expression, Function, Limits, Type};

pub enum Source<'t> {
    WASM(&'t [u8]),
    WAT(&'t str),
}

pub struct GlobalDescriptor {
    pub value_type: Type,
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

impl Into<ModuleCreateError> for wasm::DecodeError {
    fn into(self) -> ModuleCreateError {
        ModuleCreateError::WASMDeocdeError(self)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ModuleCreateError {
    WATDecodeError,
    WASMDeocdeError(wasm::DecodeError),
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
