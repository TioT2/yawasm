mod runtime;

use crate::{types, Module};


pub struct Instance<'module> {
    module: &'module Module,
    stack: Vec<types::Value>,
    heap: Vec<u8>,
    globals: Vec<types::Value>,
    trapped: bool,
}

/// Block execution result
pub enum BlockExecutionResult {
    Ok,
    Return,
    Branch { depth: u16 },
}

impl Module {
    pub fn create_instance<'t>(&'t self) -> Instance<'t> {
        let mut instance = Instance {
            globals: Vec::new(),
            heap: vec! [0; 65536],
            module: self,
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

impl<'module> Instance<'module> {
    pub fn get_module(&self) -> &Module {
        &self.module
    }
}

