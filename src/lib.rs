mod memory;
mod table;
mod util;
mod types;
mod instruction;
mod module;
mod instance;

use std::{cell::RefCell, sync::Arc};

pub(crate) use module::ModuleImpl;
pub(crate) use instance::InstanceImpl;

pub use module::{ModuleCreateError, Source};
pub use memory::Memory;
pub use types::{Value, ValueType, Mutability, Limits, ReferenceType, NumberType, ExportType};

pub(crate) struct TableBranchData {
    labels: Vec<u32>,
    default: u32,
}

pub(crate) struct Expression {
    pub instructions: Vec<u8>,
    pub table_branch_datas: Vec<TableBranchData>,
}

pub(crate) struct Function {
    type_id: u32,
    locals: Vec<types::ValueType>,
    expression: Expression,
}

pub struct Module {
    module: Arc<ModuleImpl>,
}

impl Module {
    pub fn new(source: Source) -> Result<Module, ModuleCreateError> {
        Ok(Module {
            module: Arc::new(ModuleImpl::new(source)?)
        })
    }

    pub fn create_instance(&self) -> Instance {
        Instance {
            module: self.module.clone(),
            instance: Arc::new(RefCell::new(self.module.create_instance())),
        }
    }
}

pub struct Instance {
    module: Arc<ModuleImpl>,
    instance: Arc<RefCell<InstanceImpl>>,
}

impl Instance {
    pub fn get_module(&self) -> Module {
        Module {
            module: self.module.clone(),
        }
    }

    pub fn call(&self, name: &str, args: &[Value]) -> Option<Vec<Value>> {
        self.instance.try_borrow_mut().ok()?.call(name, args)
    }
}

pub struct Global {

}

pub struct Exception {

}
