pub mod module;
pub mod util;
pub mod opcode;
pub mod types;
pub mod instruction;
pub mod decode_wasm;
pub mod runtime;

use std::collections::HashMap;
use types::Value;

pub struct TableBranchData {
    labels: Vec<u32>,
    default: u32,
}

struct Function {
    type_id: u32,
    locals: Vec<types::ValueType>,
    table_branch_datas: Vec<TableBranchData>,
    expression: Vec<u8>,
}

pub struct Module {
    types: Vec<types::FunctionType>,
    imports: HashMap<String, HashMap<String, types::ImportDescriptor>>,
    exports: HashMap<String, types::ExportDescriptor>,
    functions: Vec<Function>,

    start: Option<u32>,
}

pub enum Source<'t> {
    WASM(&'t [u8]),
    WAT(&'t str),
}

impl Module {
    pub fn new(source: Source) -> Result<Self, ModuleCreateError> {
        match source {
            Source::WASM(bits) => Self::from_wasm(bits),
            Source::WAT(_) => Err(ModuleCreateError::Unknown),
        }
    }

    pub fn get_function_types(&self) -> &[types::FunctionType] {
        &self.types
    }

    pub fn create_instance<'t>(&'t self) -> Instance<'t> {
        Instance {
            globals: Vec::new(),
            heap: vec! [0; 65536],
            module: self,
            stack: Vec::new(),
            trapped: false,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ModuleCreateError {
    WASMDeocdeError,
    WATDecodeError,
    UnexpectedStreamEnd,
    Unknown,
} // enum ModuleCreateError

impl std::fmt::Display for ModuleCreateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ModuleCreateError::UnexpectedStreamEnd => "unexpected stream end",
            ModuleCreateError::WASMDeocdeError => "WASM decode error",
            ModuleCreateError::WATDecodeError => "WAT decode error",
            ModuleCreateError::Unknown => "unknown",
        })
    }
}

pub struct Instance<'module> {
    module: &'module Module,
    stack: Vec<types::Value>,
    heap: Vec<u8>,
    globals: Vec<types::Value>,
    trapped: bool,
}

/// Block execution result
pub enum BlockExecutionResult {
    Ok,
    Return,
    Branch { depth: u16 },
}

impl<'module> Instance<'module> {
    pub fn get_module(&self) -> &Module {
        &self.module
    }
}

pub struct Global {

}

pub struct Memory {

}

pub struct Table {

}

pub struct Exception {

}

// macro_rules! wasm_bind {
//     ($(fn $name: ident ($($l: ident: $t: ty),*) -> ($($rt: ty),*) $body: expr )*) => {
//         $(
//             fn $name(args: &[Value]) -> Vec<Value> {
//                 fn bound_func( $($l: $t),* ) -> ($($rt),*) {
//                     $body
//                 }
//
//                 Vec::new()
//             }
//
//         )*
//     };
// }

fn main() {
    let module = Module::new(Source::WASM(include_bytes!("../test/math.wasm"))).unwrap();
    let mut instance = module.create_instance();

    let mut wasm_vec3f_norm = |v: (f32, f32, f32)| -> (f32, f32, f32) {
        let rs = instance.call("vec3f_norm", &[v.0.into(), v.1.into(), v.2.into()]).expect("No return values");

        if rs.len() != 3 {
            panic!("Unexpected number of values returned");
        }

        (
            rs[0].as_f32().expect("First return must be f32"),
            rs[1].as_f32().expect("Second return must be f32"),
            rs[2].as_f32().expect("Third return must be f32"),
        )
    };

    println!("{:?}", wasm_vec3f_norm((3.0, 4.0, 5.0)));
} // fn main

// file main.rs
