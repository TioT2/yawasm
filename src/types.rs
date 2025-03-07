
/// Functional type representation structure
#[derive(PartialEq)]
pub struct FunctionType {
    /// Inputs
    pub inputs: Vec<Type>,

    /// Outputs
    pub outputs: Vec<Type>,
} // struct FunctionTypes

/// Number type enumeration
#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub enum NumberType {
    /// 32-bit integer
    I32,

    /// 64-bit integer
    I64,

    /// 32-bit floating point
    F32,

    /// 64-bit floating point
    F64,
} // enum NumberType

#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub enum VectorType {
    /// 128-bit vector types
    V128,
} // enum VectorType

impl Into<Type> for VectorType {
    fn into(self) -> Type {
        match self {
            Self::V128 => Type::V128,
        }
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub enum ReferenceType {
    Extern,
    Func,
}

impl Into<Type> for ReferenceType {
    fn into(self) -> Type {
        match self {
            ReferenceType::Extern => Type::ExternRef,
            ReferenceType::Func => Type::FuncRef,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    V128,
    ExternRef,
    FuncRef,
}

#[derive(Copy, Clone)]
pub enum Value {
    /// 128-bit vector
    V128(u128),

    /// 64-bit floating point number
    F64(f64),

    /// 64-bit integer number
    I64(i64),

    /// 32-bit floating point number
    F32(f32),

    /// 32-bit integer number
    I32(i32),

    /// Function reference
    FuncRef(u32),

    /// External value reference
    ExternRef(u32),
} // enum Value

pub trait RawValue: Into<Value> + TryFrom<Value> + Sized {
    const VALUE_TYPE: Type;
}

pub trait RawValueSet: Sized {
    const VALUE_TYPES: &'static [Type];

    fn into_values(self) -> Vec<Value>;
    fn try_from_values(values: &[Value]) -> Option<Self>;
}

impl RawValueSet for () {
    const VALUE_TYPES: &'static [Type] = &[];

    fn into_values(self) -> Vec<Value> {
        Vec::new()
    }

    fn try_from_values(values: &[Value]) -> Option<Self> {
        if values.is_empty() {
            Some(())
        } else {
            None
        }
    }
}

impl<T: RawValue> RawValueSet for T {
    const VALUE_TYPES: &'static [Type] = &[T::VALUE_TYPE];

    fn into_values(self) -> Vec<Value> {
        vec![ self.into() ]
    }

    fn try_from_values(values: &[Value]) -> Option<Self> {
        (*values.get(0)?).try_into().ok()
    }
}

macro_rules! impl_native_value_set {
    ($($var_name: ident: $name: ident),*) => {
        impl<$($name: RawValue),*> RawValueSet for ($($name),*) {
            const VALUE_TYPES: &'static [Type] = &[ $($name::VALUE_TYPE),* ];

            fn into_values(self) -> Vec<Value> {
                let ($($var_name),*) = self;

                vec![
                    $($var_name.into()),*
                ]
            }

            fn try_from_values(values: &[Value]) -> Option<Self> {
                let [$($var_name),*] = values else {
                    return None
                };

                return Some((
                    $(TryInto::<$name>::try_into(*$var_name).ok()?),*
                ))
            }
        }
    }
}

impl_native_value_set!(a: A, b: B);
impl_native_value_set!(a: A, b: B, c: C);
impl_native_value_set!(a: A, b: B, c: C, d: D);
impl_native_value_set!(a: A, b: B, c: C, d: D, e: E);
impl_native_value_set!(a: A, b: B, c: C, d: D, e: E, f: F);
impl_native_value_set!(a: A, b: B, c: C, d: D, e: E, f: F, g: G);
impl_native_value_set!(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H);
impl_native_value_set!(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I);
impl_native_value_set!(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, k: K);
impl_native_value_set!(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, k: K, l: L);
impl_native_value_set!(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, k: K, l: L, m: M);



macro_rules! value_impl_from {
    ($src: ty, $arm: ident, $value_type: expr) => {
        impl From<$src> for Value {
            fn from(value: $src) -> Self {
                Self::$arm(unsafe { std::mem::transmute(value) })
            }
        }

        impl TryFrom<Value> for $src {
            type Error = ();

            fn try_from(value: Value) -> Result<$src, Self::Error> {
                match value {
                    Value::$arm(v) => Ok(unsafe { std::mem::transmute(v) }),
                    _ => Err(())
                }
            }
        }

        impl RawValue for $src {
            const VALUE_TYPE: Type = $value_type;
        }
    }
}

value_impl_from!(i32, I32, Type::I32);
value_impl_from!(u32, I32, Type::I32);
value_impl_from!(i64, I64, Type::I64);
value_impl_from!(u64, I64, Type::I64);
value_impl_from!(f32, F32, Type::F32);
value_impl_from!(f64, F64, Type::F64);
value_impl_from!(u128, V128, Type::V128);

macro_rules! value_impl_as {
    ($dst: ty, $name: ident, $mut_name: ident, $arm: ident) => {
        pub fn $name(&self) -> Option<$dst> {
            if let Self::$arm(v) = self {
                Some(unsafe { std::mem::transmute(*v) })
            } else {
                None
            }
        }

        pub fn $mut_name(&mut self) -> Option<&mut $dst> {
            if let Self::$arm(v) = self {
                Some(unsafe { std::mem::transmute(v) })
            } else {
                None
            }
        }
    }
}

impl Value {
    pub const fn get_type(&self) -> Type {
        match self {
            Self::V128(_) => Type::V128,
            Self::F64(_) => Type::F64,
            Self::I64(_) => Type::I64,
            Self::F32(_) => Type::F32,
            Self::I32(_) => Type::I32,
            Self::ExternRef(_) => Type::ExternRef,
            Self::FuncRef(_) => Type::FuncRef,
        }
    }

    pub fn default_with_type(ty: Type) -> Value {
        match ty {
            Type::F32 => Value::F32(Default::default()),
            Type::F64 => Value::F64(Default::default()),
            Type::I32 => Value::I32(Default::default()),
            Type::I64 => Value::I64(Default::default()),
            Type::ExternRef => Value::ExternRef(Default::default()),
            Type::FuncRef => Value::FuncRef(Default::default()),
            Type::V128 => Value::V128(Default::default()),
        }
    }

    value_impl_as!(i32, as_i32, as_i32_mut, I32);
    value_impl_as!(u32, as_u32, as_u32_mut, I32);
    value_impl_as!(i64, as_i64, as_i64_mut, I64);
    value_impl_as!(u64, as_u64, as_u64_mut, I64);

    value_impl_as!(f32, as_f32, as_f32_mut, F32);
    value_impl_as!(f64, as_f64, as_f64_mut, F64);

    value_impl_as!(u32, as_extern_ref, as_extern_ref_mut, ExternRef);
    value_impl_as!(u32, as_func_ref, as_func_ref_mut, FuncRef);
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct Limits {
    pub min: u32,
    pub max: Option<u32>,
}

impl Limits {
    pub fn validate(&self) -> bool {
        if let Some(max) = self.max {
            self.min <= max
        } else {
            true
        }
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub enum Mutability {
    Const = 0,
    Mut = 1,
}

impl TryFrom<u8> for Mutability {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Mutability::Const),
            1 => Ok(Mutability::Mut),
            _ => Err(())
        }
    }
}

#[derive(Copy, Clone)]
pub struct TableType {
    /// Table elements' reference type
    pub reference_type: ReferenceType,

    /// Table limits
    pub limits: Limits,
}

/// Export value type representation enumeration
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub enum ExportType {
    /// Function
    Function,

    /// Table
    Table,

    /// Memory block
    Memory,

    /// Global value
    Global,
} // enum ExportType

// /// Import representation enumeration
// pub enum ImportDescriptor {
//     /// Function
//     Function {
//         /// Index of function type
//         type_index: u32
//     },
//     /// Table
//     Table {
//         /// Type of table
//         ty: TableType,
//     },
//     /// Memory block
//     Memory {
//         /// Limits of page count
//         page_count_limits: Limits,
//     },
//     /// Global variable
//     Global {
//         /// Value type
//         ty: Type,
// 
//         /// Mutability
//         mutability: Mutability,
//     },
// } // enum ImportDescriptor

/// Export descriptor representation structure
pub struct ExportDescriptor {
    /// Export type
    pub ty: ExportType,

    /// Index
    pub index: u32,
} // enum ExportDescriptor

// file types.rs