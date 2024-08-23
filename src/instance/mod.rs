mod runtime;
mod item;

use std::sync::Arc;

use crate::{table::Table, Memory, ModuleImpl, Mutability, Type};

pub(crate) use item::StackItem;

pub(crate) struct Global {
    pub value: StackItem,
    pub ty: Type,
    pub mutability: Mutability,
}

pub struct InstanceImpl {
    module: Arc<ModuleImpl>,
    stack: Vec<StackItem>,
    memory: Memory,
    tables: Vec<Table>,
    globals: Vec<Global>,
    trapped: bool,
}

/// Block execution result
pub(super) enum BlockExecutionResult {
    Ok,
    Return,
    Branch { depth: u16 },
}

impl ModuleImpl {
    pub fn create_instance(self: &Arc<Self>) -> Option<InstanceImpl> {
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

        // Setup globals
        instance.globals = self.globals
            .iter()
            .map(|descriptor| Some(Global {
                ty: descriptor.value_type,
                mutability: descriptor.mutability,
                value: todo!("Globals unimplemented"),
            }))
            .collect::<Option<Vec<Global>>>()?;

        // Call setup function
        if let Some(start_id) = self.start {
            instance.call_by_id(start_id);
        }

        Some(instance)
    }
}
