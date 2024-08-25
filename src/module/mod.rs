pub mod wasm;

use std::collections::HashMap;

use crate::{types::{self, TableType}, Expression, Function, Limits, Type};

/// Raw WASM enumeration
pub enum Source<'t> {
    /// WASM bytecode
    WASM(&'t [u8]),

    /// WAT source
    WAT(&'t str),
} // enum Source

/// Global variable descriptor
pub struct GlobalDescriptor {
    /// Value type
    pub value_type: Type,

    /// Mutability
    pub mutability: types::Mutability,

    /// Initializer expression
    pub expression: Expression,
} // struct GlobalDescriptor

/// Table data type enumeration
pub enum DataType {
    /// Active - will be written to default memory during module initialization
    ActiveToDefaultMemory = 0,
    /// Passive - can be written to some memory during module function execution
    Passive = 1,
    /// Active - will be written to some memory during module initialization
    ActiveToSomeMemory = 2,
} // enum DataType

/// Table data descriptor
pub enum DataDescriptor {
    /// Active - will be written to default memory during module initialization
    ActiveToDefaultMemory {
        /// Write offset
        offset: u32,
    },

    /// Passive - can be written to some memory during module function execution
    Passive,

    /// Active - will be written to some memory during module initialization
    ActiveToSomeMemory {
        /// Memory to write to index
        memory: u32,

        /// Write offseet
        offset: u32,
    },
} // enum DataDescriptor

impl DataDescriptor {
    /// Data descriptor type getting function
    /// * Returns data type
    pub fn get_type(&self) -> DataType {
        match self {
            DataDescriptor::ActiveToDefaultMemory { .. } => DataType::ActiveToDefaultMemory,
            DataDescriptor::ActiveToSomeMemory { .. } => DataType::ActiveToSomeMemory,
            DataDescriptor::Passive => DataType::Passive,
        }
    } // fn get_type
} // impl DataDescriptor

/// Table data representation enumeration
pub struct Data {
    /// Descriptor
    pub data: DataDescriptor,

    /// Content
    pub content: Vec<u8>,
} // struct Data

/// WASM module internal structure
pub struct ModuleImpl {
    /// Function types
    pub(crate) types: Vec<types::FunctionType>,

    /// Required imports
    pub(crate) imports: HashMap<String, HashMap<String, types::ImportDescriptor>>,

    /// Module exports
    pub(crate) exports: HashMap<String, types::ExportDescriptor>,

    /// Descriptors of global variables
    pub(crate) globals: Vec<GlobalDescriptor>,

    /// Descriptors of functions
    pub(crate) functions: Vec<Function>,

    /// Descriptors of memory blocks (current specification of WASM allows only one block to exist, actually).
    /// Vector of descriptors is used for further specification development
    pub(crate) memories: Vec<Limits>,

    /// Descriptors of tables (same as memories, actually)
    pub(crate) tables: Vec<TableType>,

    /// Start function index, if presented
    pub(crate) start: Option<u32>,
} // struct ModuleImpl

impl ModuleImpl {
    /// Module internal data constructor
    /// * `source` - WASM source
    /// * Returns result with created module internal or error
    pub fn new(source: Source) -> Result<Self, ModuleCreateError> {
        match source {
            Source::WASM(bits) => Self::from_wasm(bits).map_err(ModuleCreateError::WASMDecodeError),
            Source::WAT(_) => unimplemented!("TODO: add WAT loading support to project"),
        }

    } // fn new
}

impl Into<ModuleCreateError> for wasm::DecodeError {
    fn into(self) -> ModuleCreateError {
        ModuleCreateError::WASMDecodeError(self)
    }
}

/// Module create error
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ModuleCreateError {
    /// WAT validation error
    WATDecodeError,

    /// WASM validation error,
    WASMDecodeError(wasm::DecodeError),
} // enum ModuleCreateError

impl std::fmt::Display for ModuleCreateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ModuleCreateError::WASMDecodeError(_) => "WASM file contents decode error",
            ModuleCreateError::WATDecodeError => "WAT file contents decode error",
        })
    } // fn fmt
} // impl std::fmt::Display for ModuleCreateError

// file mod.rs
