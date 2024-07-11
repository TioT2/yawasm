mod runtime;

use std::sync::Arc;

use crate::{types, ModuleImpl};

pub struct InstanceImpl {
    module: Arc<ModuleImpl>,
    stack: Vec<types::Value>,
    heap: Vec<u8>,
    globals: Vec<types::Value>,
    trapped: bool,
}

/// Block execution result
pub(super) enum BlockExecutionResult {
    Ok,
    Return,
    Branch { depth: u16 },
}

impl ModuleImpl {
    pub fn create_instance(self: &Arc<Self>) -> InstanceImpl {
        let mut instance = InstanceImpl {
            globals: Vec::new(),
            heap: vec! [0; 65536],
            module: self.clone(),
            stack: Vec::new(),
            trapped: false,
        };

        // Call setup function
        if let Some(start_id) = self.start {
            instance.call_by_id(start_id);
        }

        instance
    }

}
