mod runtime;
mod item;

use std::sync::Arc;

use crate::{Limits, Memory, ModuleImpl};

pub(crate) use item::StackItem;

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

    // /// Table set
    // tables: Vec<Table>,

    /// Global set
    globals: Vec<StackItem>,

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

impl ModuleImpl {
    /// Instance create function
    /// * Returns instance implementation
    pub fn create_instance(self: &Arc<Self>) -> Result<InstanceImpl, RuntimeError> {
        let mut instance = InstanceImpl {
            globals: Vec::new(),
            memory: Memory::new(if let Some(limits) = self.memories.get(0).copied() {
                limits
            } else {
                Limits { min: 0, max: None }
            }),
            // tables: self.tables
            //     .iter()
            //     .map(|ty| Table::new(*ty, None))
            //     .collect::<Vec<Table>>(),
            module: self.clone(),
            stack: Vec::new(),
            trapped: false,
        };

        // Setup global variables separately all another module
        instance.globals = self.globals
            .iter()
            .map(|descriptor| Ok(instance
                .exec_expression(&descriptor.expression, &[descriptor.value_type])?
                .get(0)
                .copied()
                .unwrap()
                .into()
            ))
            .collect::<Result<Vec<StackItem>, RuntimeError>>()?;

        // Call setup function
        if let Some(start_id) = self.start {
            instance.call_by_id(start_id)?;
        }

        Ok(instance)
    } // fn create_instance
} // impl module_impl

// file mod.rs
