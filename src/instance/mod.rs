mod runtime;

use std::sync::Arc;

use crate::{table::Table, types, Global, Memory, ModuleImpl};

pub struct InstanceImpl {
    module: Arc<ModuleImpl>,
    stack: Vec<types::Value>,
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
                value: *instance.exec_expression(&descriptor.expression, &[descriptor.value_type])?.get(0)?,
                mutability: descriptor.mutability,
            }))
            .collect::<Option<Vec<Global>>>()?;

        // Call setup function
        if let Some(start_id) = self.start {
            instance.call_by_id(start_id);
        }

        Some(instance)
    }
}
