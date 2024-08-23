use crate::{Type, Value};

#[derive(Copy, Clone)]
pub union StackItem {
    pub i32: i32,
    pub i64: i64,
    pub u32: u32,
    pub u64: u64,
    pub f32: f32,
    pub f64: f64,
}

impl StackItem {
    pub fn to_value(self, ty: Type) -> Value {
        match ty {
            Type::ExternRef => Value::ExternRef(unsafe { self.u32 }),
            Type::FuncRef => Value::FuncRef(unsafe { self.u32 }),
            Type::F32 => Value::F32(unsafe { self.f32 }),
            Type::F64 => Value::F64(unsafe { self.f64 }),
            Type::I32 => Value::I32(unsafe { self.i32 }),
            Type::I64 => Value::I64(unsafe { self.i64 }),

            // Can't properly convert StackItem into value - V128 will be represented as u64x2 on stack
            Type::V128 => Value::V128(0),
        }
    }
}

impl Default for StackItem {
    fn default() -> Self {
        Self { u64: 0 }
    }
}

impl From<Value> for StackItem {
    fn from(value: Value) -> Self {
        match value {
            Value::ExternRef(u32) => Self { u32 },
            Value::FuncRef(u32) => Self { u32 },
            Value::F32(f32) => Self { f32 },
            Value::F64(f64) => Self { f64 },
            Value::I32(i32) => Self { i32 },
            Value::I64(i64) => Self { i64 },
            Value::V128(v128) => unimplemented!("V128 values aren't implemented"),
        }
    }
}

macro_rules! impl_stack_item_for {
    ($t: ty, $field: ident, $as: ident, $as_mut: ident) => {
        impl From<$t> for StackItem {
            fn from(value: $t) -> Self {
                Self { $field: value }
            }
        }

        impl StackItem {
            pub unsafe fn $as(self) -> $t {
                self.$field
            }

            pub unsafe fn $as_mut(&mut self) -> &mut $t {
                &mut self.$field
            }
        }
    }
}

impl_stack_item_for!(i32, i32, as_i32, as_i32_mut);
impl_stack_item_for!(i64, i64, as_i64, as_i64_mut);
impl_stack_item_for!(u32, u32, as_u32, as_u32_mut);
impl_stack_item_for!(u64, u64, as_u64, as_u64_mut);
impl_stack_item_for!(f32, f32, as_f32, as_f32_mut);
impl_stack_item_for!(f64, f64, as_f64, as_f64_mut);
