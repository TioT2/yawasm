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

/// Expression internal representation structure
pub(crate) struct Expression {
    /// Instructions, re-encoded in own simplified format to make usability quite easier
    pub instructions: Vec<u8>,
}

pub(crate) struct Function {
    pub type_id: u32,
    pub locals: Vec<types::ValueType>,
    pub expression: Expression,
}

pub(crate) struct Global {
    pub mutability: Mutability,
    pub value: Value,
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

    pub fn create_instance(&self) -> Option<Instance> {
        Some(Instance {
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

impl FunctionReference {
    pub fn call(&self, args: &[Value]) -> Option<Vec<Value>> {
        self.instance.try_borrow_mut().ok()?.call(self.func_id, args)
    }

    pub fn as_untyped_callable(&self) -> Box<dyn Fn(&[Value]) -> Option<Vec<Value>>> {
        let instance = self.instance.clone();
        let func_id = self.func_id;

        Box::new(move |values: &[Value]| -> Option<Vec<Value>> {
            instance.try_borrow_mut().ok()?.call(func_id, values)
        })
    }

    pub fn as_typed_callable<I: NativeValueSet, O: NativeValueSet>(&self) -> Option<Box<dyn Fn(I) -> Option<O>>> {
        let func = self.module.functions.get(self.func_id as usize)?;
        let ty = self.module.types.get(func.type_id as usize)?;

        // Type check
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

/*

#[derive(Copy, Clone)]
pub enum CallUnwindInfo {
    Function {
        src_function_id: u32,
        unwind_ptr: usize,
    },
    Block {
        unwind_ptr: usize,
    },
}

pub struct CallStack {
    calls: Vec<CallUnwindInfo>,
}

impl CallStack {
    pub fn new() -> CallStack {
        CallStack { calls: Vec::new() }
    }

    pub fn insert_function_call(&mut self, src_function_id: u32, unwind_ptr: usize) {
        self.calls.push(CallUnwindInfo::Function { src_function_id , unwind_ptr })
    }

    pub fn insert_block(&mut self, unwind_ptr: usize) {
        self.calls.push(CallUnwindInfo::Block { unwind_ptr })
    }

    pub fn pop(&mut self) -> Option<CallUnwindInfo> {
        None
    }
}

/// Local variable sack element
pub enum LocalStackElement {
    Local(Value),
    FrameSeparator(usize),
}

pub struct LocalStack {
    locals: Vec<LocalStackElement>,
}

impl LocalStack {
    pub fn new() -> Self {
        LocalStack { locals: Vec::new() }
    }

    pub fn alloc_frame(&mut self, values: &[Value]) {
        self.locals.extend(values.iter().rev().map(|v| LocalStackElement::Local(*v)));
        self.locals.push(LocalStackElement::FrameSeparator(values.len()));
    }

    pub fn unwind(&mut self, frame_count: usize) -> Option<()> {
        let mut unwind_count = 0;

        if self.locals.len() == 0 {
            return None;
        }

        for _ in 0..frame_count {
            if let LocalStackElement::FrameSeparator(frame_size) = self.locals.last()? {
                unwind_count += frame_size + 1;
            } else {
                panic!("Last element of local stack MUST be frame separator.");
            }
        }

        self.locals.truncate(self.locals.len() - unwind_count);

        Some(())
    }
}

/// Element of operand stack representation structure
#[derive(Copy, Clone)]
pub(crate) enum OperandStackElement {
    /// Operand, actually
    Operand(Value),
    /// Stackframe separator
    FrameSeparator(usize),
}

/// Stack of operands representation structure
pub(crate) struct OperandStack {
    stack: Vec<OperandStackElement>,
    prev_frame_len: usize,
}

impl OperandStack {
    pub fn new() -> OperandStack {
        OperandStack {
            stack: Vec::new(),
            prev_frame_len: 0,
        }
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(OperandStackElement::Operand(value));
        self.prev_frame_len += 1;
    }

    pub fn pop(&mut self) -> Option<Value> {
        if let OperandStackElement::Operand(op) = *self.stack.last_mut()? {
            _ = self.stack.pop();
            self.prev_frame_len -= 1;
            Some(op)
        } else {
            None
        }
    }

    pub fn push_frame(&mut self, consumed_count: usize) -> Option<()> {
        if self.prev_frame_len < consumed_count {
            None
        } else {
            self.prev_frame_len -= consumed_count;

            // Push empty element
            self.stack.push(OperandStackElement::FrameSeparator(0));

            // Move consumed elements
            let copy_dst = self.stack.len() - consumed_count;
            let copy_range = (copy_dst - 1)..(self.stack.len() - 1);
            self.stack.copy_within(copy_range, copy_dst);

            self.stack[copy_dst - 1] = OperandStackElement::FrameSeparator(self.prev_frame_len);

            self.prev_frame_len = consumed_count;

            Some(())
        }
    }

    pub fn unwind_frames(&mut self, frame_count: usize, saved_count: usize) -> Option<()> {
        if self.prev_frame_len > saved_count || self.stack.is_empty() {
            return None;
        }

        if self.stack.is_empty() {
            return Some(());
        }

        let mut eptr = self.stack.len() - 1;

        for _ in 0..frame_count {
            eptr -= self.prev_frame_len;

            if let OperandStackElement::FrameSeparator(sep) = self.stack.get(eptr)? {
                self.prev_frame_len = *sep;
            } else {
                panic!("Frame separator must be by the eptr index");
            }
        }

        // Unwind and copy
        let l = self.stack.len();
        self.stack.copy_within((l-saved_count)..l, eptr);
        self.stack.truncate(eptr - saved_count);

        Some(())
    }
}

*/