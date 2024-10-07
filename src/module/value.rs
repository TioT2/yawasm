//! WASM Types and values declaration module

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
    V128,
} // enum VectorType

/// Reference type
#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub enum ReferenceType {
    /// Extern reference
    Extern,

    /// Function type
    Func,
} // enum ReferenceType

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    /// 32-bit integer type
    I32,

    /// 64-bit integer type
    I64,

    /// 32-bit integer type
    F32,

    /// 64-bit integer type
    F64,

    /// 128-bit vector type
    V128,

    /// External object reference
    ExternRef,

    /// Function object reference
    FuncRef,
} // enum Type

impl From<NumberType> for Type {
    fn from(value: NumberType) -> Self {
        match value {
            NumberType::I32 => Type::I32,
            NumberType::I64 => Type::I64,
            NumberType::F32 => Type::F32,
            NumberType::F64 => Type::F64,
        }
    }
}

impl From<ReferenceType> for Type {
    fn from(value: ReferenceType) -> Self {
        match value {
            ReferenceType::Extern => Type::ExternRef,
            ReferenceType::Func => Type::FuncRef,
        }
    }
}

impl From<VectorType> for Type {
    fn from(value: VectorType) -> Self {
        match value {
            VectorType::V128 => Type::V128,
        }
    }
}

/// Any primitive WASM value representation structure
#[derive(Copy, Clone)]
pub enum Value {
    /// 128-bit vector
    V128(u128),

    /// 64-bit floating point
    F64(f64),

    /// 64-bit integer
    I64(i64),

    /// 32-bit floating point
    F32(f32),

    /// 32-bit integer
    I32(i32),

    /// Functional reference
    FuncRef(u32),

    /// External reference
    ExternRef(u32),
} // enum Value

impl Value {
    pub fn ty(&self) -> Type {
        match self {
            Self::V128(_)      => Type::V128,
            Self::F64(_)       => Type::F64,
            Self::I64(_)       => Type::I64,
            Self::F32(_)       => Type::F32,
            Self::I32(_)       => Type::I32,
            Self::FuncRef(_)   => Type::FuncRef,
            Self::ExternRef(_) => Type::ExternRef,
        }
    } // fn ty

    pub fn zero_with_type(ty: Type) -> Self {
        match ty {
            Type::V128      => Self::V128(0),
            Type::F64       => Self::F64(0.0),
            Type::I64       => Self::I64(0),
            Type::F32       => Self::F32(0.0),
            Type::I32       => Self::I32(0),
            Type::FuncRef   => Self::FuncRef(0),
            Type::ExternRef => Self::ExternRef(0),
        }
    } // fn zero_with_type
}
