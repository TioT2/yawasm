mod runtime;
mod item;

use std::sync::Arc;

use crate::{table::Table, Memory, ModuleImpl, Mutability, Type};

pub(crate) use item::StackItem;

/// Global variable representation structure
pub(crate) struct Global {
    /// Value
    pub value: StackItem,

    /// Type (idk, how it can be used)
    pub ty: Type,

    /// Mutability (useful, actually)
    pub mutability: Mutability,
} // struct Global

/// WASM runtime error
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RuntimeError {
    /// Access to memory that doesn't exist
    MemoryAccessError,

    /// Unreachable instruction executed
    Unreachable,
} // enum RuntimeError

/// Function call error
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CallError {
    /// Invalid function index
    InvalidFunctionIndex,

    /// Runtime error
    RuntimeError(RuntimeError),
} // enum CallError

impl From<RuntimeError> for CallError {
    fn from(value: RuntimeError) -> Self {
        Self::RuntimeError(value)
    }
}

/// Instance implementation structure
pub struct InstanceImpl {
    /// Module reference
    module: Arc<ModuleImpl>,

    /// Stack
    stack: Vec<StackItem>,

    /// Memory (only one used, cuz of standard)
    memory: Memory,

    /// Table set
    tables: Vec<Table>,

    /// Global set
    globals: Vec<Global>,

    /// fatal error flag
    trapped: bool,
} // struct InstanceImpl

/// Block execution result
pub(super) enum BlockExecutionResult {
    /// All's good
    Ok,

    /// Function returned
    Return,

    /// Branch started
    Branch { depth: u16 },
} // enum BlockExecutionResult

/// Instance create error
pub enum InstanceCreateError {
    GlobalInitializationRuntimeError,
} // enum InstanceCreateError

impl ModuleImpl {
    /// Instance create function
    /// * Returns instance implementation
    pub fn create_instance(self: &Arc<Self>) -> Result<InstanceImpl, RuntimeError> {
        let mut instance = InstanceImpl {
            globals: Vec::new(),
            memory: Memory::new(),
            tables: self.tables
                .iter()
                .map(|ty| Table::new(*ty, None))
                .collect::<Vec<Table>>(),
            module: self.clone(),
            stack: Vec::new(),
            trapped: false,
        };

        // Setup global variables separately all another module
        instance.globals = self.globals
            .iter()
            .map(|descriptor| Ok(Global {
                ty: descriptor.value_type,
                mutability: descriptor.mutability,
                value: instance
                    .exec_expression(&descriptor.expression, &[descriptor.value_type])?
                    .get(0)
                    .copied()
                    .unwrap()
                    .into(),
            }))
            .collect::<Result<Vec<Global>, RuntimeError>>()?;

        // Call setup function
        if let Some(start_id) = self.start {
            instance.call_by_id(start_id);
        }

        Ok(instance)
    } // fn create_instance
} // impl module_impl

// file mod.rs
