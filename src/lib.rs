mod memory;
mod table;
mod util;
mod types;
mod instruction;
mod module;
mod instance;

use std::{cell::RefCell, sync::Arc};

use instance::RuntimeError;
pub(crate) use module::ModuleImpl;
pub(crate) use instance::InstanceImpl;

pub use module::{ModuleCreateError, Source};
pub use memory::Memory;
pub use types::{Value, Type, Mutability, Limits, ReferenceType, NumberType, ExportType, RawValue, RawValueSet};

/// Expression internal representation structure
pub(crate) struct Expression {
    /// Instructions, re-encoded in own simplified format to make usability quite easier
    pub instructions: Vec<u8>,
} // struct Expression

/// Function representation structure
pub(crate) struct Function {
    /// Function type id
    pub type_id: u32,

    /// Local variable types
    pub locals: Vec<Type>,

    /// Expression (in internal bytecode)
    pub expression: Expression,
} // struct Function

/// Module user handle
pub struct Module {
    /// Module internal holder
    module: Arc<ModuleImpl>,
} // struct Module

impl Module {
    /// Module constructor
    /// * `source` - module source
    /// * Returns module or create error
    pub fn new(source: Source) -> Result<Self, ModuleCreateError> {
        Ok(Module {
            module: Arc::new(ModuleImpl::new(source)?)
        })
    } // fn new

    /// Module instance create function
    /// * Returns option of instance
    pub fn create_instance(&self) -> Result<Instance, RuntimeError> {
        Ok(Instance {
            module: self.module.clone(),
            instance: Arc::new(RefCell::new(self.module.create_instance()?)),
        })
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

/// Function to typed function conversion error
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FunctionTypizationError {
    /// Inputs unmatched
    UnmatchingInputs,

    /// Outputs unmatched
    UnmatchingOutputs,
} // enum FunctionTypizationError

impl FunctionReference {
    pub fn call(&self, args: &[Value]) -> Result<Vec<Value>, RuntimeError> {
        self.instance
            .try_borrow_mut()
            .ok()
            .expect("Recursive external call problem...")
            .call(self.func_id, args)
            .map_err(|e| match e {
                instance::CallError::InvalidFunctionIndex => panic!("Invalid function index passed"),
                instance::CallError::RuntimeError(err) => err,
            })
    }

    pub fn as_untyped_callable(&self) -> Box<dyn Fn(&[Value]) -> Result<Vec<Value>, RuntimeError>> {
        let instance = self.instance.clone();
        let func_id = self.func_id;

        Box::new(move |values: &[Value]| -> Result<Vec<Value>, RuntimeError> {
            instance
                .try_borrow_mut()
                .ok()
                .expect("External call recursion...")
                .call(func_id, values)
                .map_err(|e| match e {
                    instance::CallError::InvalidFunctionIndex => panic!("Invalid function index passed"),
                    instance::CallError::RuntimeError(err) => err
                })
        })
    }

    pub fn as_typified<I: RawValueSet, O: RawValueSet>(
        &self
    ) -> Result<Box<dyn Fn(I) -> Result<O, RuntimeError>>, FunctionTypizationError> {
        let func = self.module.functions.get(self.func_id as usize).unwrap();
        let ty = self.module.types.get(func.type_id as usize).unwrap();

        if ty.inputs != I::VALUE_TYPES {
            return Err(FunctionTypizationError::UnmatchingInputs);
        }

        if ty.outputs != O::VALUE_TYPES {
            return Err(FunctionTypizationError::UnmatchingOutputs);
        }

        // Type check
        let instance = self.instance.clone();
        let func_id = self.func_id;

        Ok(Box::new(move |i: I| -> Result<O, RuntimeError> {
            Ok(O::try_from_values(
                &instance
                    .try_borrow_mut()
                    .ok()
                    .expect("Recursive external call...")
                    .call(func_id, &i.into_values())
                    .map_err(|e| match e {
                        instance::CallError::InvalidFunctionIndex => panic!("Invalid function index passed"),
                        instance::CallError::RuntimeError(err) => err,
                    })?
            ).expect("Invalid conversion"))
        }))
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
