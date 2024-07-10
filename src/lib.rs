mod memory;
mod table;
mod util;
mod types;
mod instruction;
mod module;
mod instance;

use std::collections::HashMap;

pub use module::{Module, ModuleCreateError, Source};
pub use instance::{BlockExecutionResult, Instance};
pub use memory::Memory;

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

pub struct Global {

}

pub struct Exception {

}
