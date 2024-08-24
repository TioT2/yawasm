
#[derive(PartialEq)]
pub struct FunctionType {
    pub inputs: Vec<Type>,
    pub outputs: Vec<Type>,
}

#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub enum NumberType {
    I32 = 0x7F,
    I64 = 0x7E,
    F32 = 0x7D,
    F64 = 0x7C,
}

impl TryFrom<u8> for NumberType {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x7C..=0x7F => Ok(unsafe { std::mem::transmute::<u8, NumberType>(value) }),
            _ => Err(()),
        }
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub enum VectorType {
    V128,
}

impl TryFrom<u8> for VectorType {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value == 0x7B {
            Ok(VectorType::V128)
        } else {
            Err(())
        }
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub enum ReferenceType {
    Extern = 0x6F,
    Func = 0x70,
}

impl TryFrom<u8> for ReferenceType {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x6F => Ok(ReferenceType::Extern),
            0x70 => Ok(ReferenceType::Func),
            _ => Err(())
        }
    }
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

impl TryFrom<u8> for Type {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x6F => Ok(Type::ExternRef),
            0x70 => Ok(Type::FuncRef),
            0x7B => Ok(Type::V128),
            0x7F => Ok(Type::I32),
            0x7E => Ok(Type::I64),
            0x7D => Ok(Type::F32),
            0x7C => Ok(Type::F64),
            _ => Err(()),
        }
    }
}

#[derive(Copy, Clone)]
pub enum Value {
    V128(u128),
    F64(f64),
    I64(i64),
    F32(f32),
    I32(i32),
    FuncRef(u32),
    ExternRef(u32),
}

pub trait NativeValue: Into<Value> + TryFrom<Value> + Sized {
    const VALUE_TYPE: Type;
}

pub trait NativeValueSet: Sized {
    const VALUE_TYPES: &'static [Type];

    fn into_values(self) -> Vec<Value>;
    fn try_from_values(values: &[Value]) -> Option<Self>;
}

impl NativeValueSet for () {
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

impl<T: NativeValue> NativeValueSet for T {
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
        impl<$($name: NativeValue),*> NativeValueSet for ($($name),*) {
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

        impl NativeValue for $src {
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


#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum SectionID {
    Custom = 0,
    Type = 1,
    Import = 2,
    Function = 3,
    Table = 4,
    Memory = 5,
    Global = 6,
    Export = 7,
    Start = 8,
    Element = 9,
    Code = 10,
    Data = 11,
    DataCount = 12,
}

impl TryFrom<u8> for SectionID {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(SectionID::Custom),
            1 => Ok(SectionID::Type),
            2 => Ok(SectionID::Import),
            3 => Ok(SectionID::Function),
            4 => Ok(SectionID::Table),
            5 => Ok(SectionID::Memory),
            6 => Ok(SectionID::Global),
            7 => Ok(SectionID::Export),
            8 => Ok(SectionID::Start),
            9 => Ok(SectionID::Element),
            10 => Ok(SectionID::Code),
            11 => Ok(SectionID::Data),
            12 => Ok(SectionID::DataCount),
            _ => Err(())
        }
    }
}

#[derive(Copy, Clone)]
pub struct TableType {
    pub reference_type: ReferenceType,
    pub limits: Limits,
}

unsafe impl bytemuck::Zeroable for Header {}
unsafe impl bytemuck::AnyBitPattern for Header {}

/// WASM magic number
pub const WASM_MAGIC: [u8; 4] = [0x00, 0x61, 0x73, 0x6D];

/// WAMS version number
pub const WASM_VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

/// WASM functype magic
pub const WASM_FUNCTYPE_MAGIC: u8 = 0x60;

/// WASM file header representation structure
#[repr(packed)]
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct Header {
    /// Magic number
    pub magic: [u8; 4],

    /// Version magic number
    pub version: [u8; 4],
} // struct Header

impl Header {
    /// Header validation function
    /// * Returns true if header is correct supported WASM header
    pub fn validate(self) -> bool {
        self.magic == WASM_MAGIC && self.version == WASM_VERSION
    } // fn validate
}

/// Export value type representation enumeration
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub enum ExportType {
    /// Function
    Function = 0,

    /// Table
    Table = 1,

    /// Memory block
    Memory = 2,

    /// Global value
    Global = 3,
} // enum ExportType

impl TryFrom<u8> for ExportType {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(ExportType::Function),
            1 => Ok(ExportType::Table),
            2 => Ok(ExportType::Memory),
            3 => Ok(ExportType::Global),
            _ => Err(()),
        }
    }
}

/// Import representation enumeration
pub enum ImportDescriptor {
    /// Function
    Function {
        /// Index of function type
        type_index: u32
    },
    /// Table
    Table {
        /// Type of table
        ty: TableType,
    },
    /// Memory block
    Memory {
        /// Limits of page count
        page_count_limits: Limits,
    },
    /// Global variable
    Global {
        /// Value type
        ty: Type,

        /// Mutability
        mutability: Mutability,
    },
} // enum ImportDescriptor

/// Export descriptor representation structure
pub struct ExportDescriptor {
    /// Export type
    pub ty: ExportType,

    /// Index
    pub index: u32,
} // enum ExportDescriptor

// file types.rs