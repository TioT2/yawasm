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
pub use types::{Value, ValueType, Mutability, Limits, ReferenceType, NumberType, ExportType, NativeValue, NativeValueSet};

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

pub struct Global {

}

pub struct Exception {

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

#[derive(Clone)]
pub struct FunctionReference {
    module: Arc<ModuleImpl>,
    instance: Arc<RefCell<InstanceImpl>>,
    func_id: u32,
}

impl FunctionReference {
    pub fn call(&self, args: &[Value]) -> Option<Vec<Value>> {
        self.instance.try_borrow_mut().ok()?.call(self.func_id, args)
    }

    pub fn as_callable<I: NativeValueSet, O: NativeValueSet>(&self) -> Option<Box<dyn Fn(I) -> Option<O>>> {
        let func = self.module.functions.get(self.func_id as usize)?;
        let ty = self.module.types.get(func.type_id as usize)?;

        // Failed type check
        if ty.inputs != I::VALUE_TYPES || ty.outputs != O::VALUE_TYPES {
            None
        } else {
            let instance = self.instance.clone();
            let func_id = self.func_id;

            Some(Box::new(move |i: I| -> Option<O> {
                O::try_from_values(
                    &instance.try_borrow_mut().ok()?.call(func_id, &i.into_values())?
                )
            }))
        }
    }
}

impl Instance {
    pub fn get_module(&self) -> Module {
        Module {
            module: self.module.clone(),
        }
    }

    pub fn get_function(&self, name: &str) -> Option<FunctionReference> {
        let export = self.module.exports.get(name)?;

        if export.ty == ExportType::Function {
            Some(FunctionReference {
                module: self.module.clone(),
                func_id: export.index,
                instance: self.instance.clone(),
            })
        } else {
            None
        }
    }
}
