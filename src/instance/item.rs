pub union StackItem {
    i32: i32,
    i64: i64,
    u32: u32,
    u64: u64,
    f32: f32,
    f64: f64,
}

macro_rules! impl_stack_item_from {
    ($t: ty, $field: ident) => {
        impl From<$t> for StackItem {
            fn from(value: $t) -> Self {
                Self { $field: value }
            }
        }
    }
}

impl_stack_item_from!(i32, i32);
impl_stack_item_from!(i64, i64);
impl_stack_item_from!(u32, u32);
impl_stack_item_from!(u64, u64);
