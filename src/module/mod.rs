use std::marker::PhantomData;

mod value;
pub mod wasm;
pub mod wat;

pub use value::*;

/// Function signature representaiton structure
pub struct FunctionType {
    /// Function inputs
    pub inputs: Vec<Type>,

    /// Function outputs
    pub outputs: Vec<Type>,
} // struct FunctionSignature

/// Function representaiton structure
pub struct Function {
    /// Function type index
    type_index: u32,

    /// Function instruciton pool
    instructions: Vec<u8>,
} // struct Function

/// Instuction iterator representation enumeration
pub struct Instructions<'t> {
    /// Empty, at least now
    _phd: std::marker::PhantomData<&'t ()>,
} // struct Instruction

impl Function {
    /// Function instructions streaming starting function
    pub fn instructions<'t>(&'t self) -> Instructions<'t> {
        Instructions {
            _phd: PhantomData::default(),
        }
    }
}

/// Module implementation structure
pub struct Module {
    /// Function signatures collection
    types: Vec<FunctionType>,

    /// Function implementations collection
    functions: Vec<Function>,

    /// Index of start function
    start_function_index: Option<u32>,
} // struct Module

/// WASM Source representation enumeration
pub enum ModuleSource<'t> {
    /// WASM Bits
    Wasm(&'t [u8]),

    /// WAT Text
    Wat(&'t str),
} // enum ModuleSource

/// Module create error
#[derive(Clone, Debug)]
pub enum ModuleCreateError {
    /// WASM Decode error
    WasmDecodeError(wasm::DecodeError),

    /// WAT Decode error
    WatDecodeError(wat::DecodeError),
} // enum ModuleCreaterror

/// Function reference representation structure
pub struct FunctionReference<'t> {
    /// Function reference, actually
    function: &'t Function,

    /// It's type
    ty: &'t FunctionType,
}

impl<'t> FunctionReference<'t> {
    /// Function type getting function
    pub fn ty(&self) -> &FunctionType {
        self.ty
    }

    /// Function's instruction stream getting function
    pub fn instructions(&self) -> Instructions {
        self.function.instructions()
    } // fn instructions
}

impl Module {
    /// Module from source parsing function
    pub fn from_source(source: ModuleSource) -> Result<Self, ModuleCreateError> {
        match source {
            ModuleSource::Wasm(wasm) => {
                wasm::parse(wasm)
                    .map_err(ModuleCreateError::WasmDecodeError)
            }
            ModuleSource::Wat(wat) => {
                wat::parse(wat)
                    .map_err(ModuleCreateError::WatDecodeError)
            }
        }
    } // fn from_source

    /// Function getting function
    pub fn get_function<'t>(&'t self, index: u32) -> Option<FunctionReference<'t>> {
        let function = self.functions.get(index as usize)?;
        let ty = self.types.get(function.type_index as usize).unwrap();

        Some(FunctionReference { function, ty })
    } // fn get_function
}


