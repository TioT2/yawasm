pub mod module;
pub mod util;
pub mod opcode;
pub mod types;
pub mod instruction;
pub mod decode_wasm;

use std::collections::HashMap;
use types::Value;

use crate::util::binary_stream::BinaryInputStream;

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

impl<'module> Instance<'module> {
    pub fn get_module(&self) -> &Module {
        &self.module
    }

    fn set_trap(&mut self) {
        self.trapped = true;
    }

    fn exec_block(&mut self, block: &[u8], locals: &mut [types::Value]) -> Option<()> {
        if self.trapped {
            return None;
        }

        let mut stream = BinaryInputStream::new(block);

        while let Some(byte) = stream.get::<u8>() {
            let instruction = instruction::Instruction::try_from(byte).expect(format!("Fatal error: unknown instruction {:02X}", byte).as_str());

            macro_rules! pop {
                ($exp_t: ident) => (self.stack.pop()?.$exp_t()?);
                () => (self.stack.pop()?);
            }

            macro_rules! top {
                ($exp_t: ident) => (self.stack.last_mut()?.$exp_t()?);
                () => (self.stack.last_mut()?);
            }

            macro_rules! push {
                ($value: expr) => {
                    {
                        let v = $value;
                        self.stack.push(v.into())
                    }
                }
            }

            macro_rules! load {
                ($t: ty, $addr: expr) => {
                    {
                        let ptr = ($addr) as usize;
                        if let Some(b) = self.heap.get(ptr..(ptr + std::mem::size_of::<$t>())) {
                            *bytemuck::from_bytes::<$t>(b)
                        } else {
                            self.set_trap();
                            return None;
                        }
                    }
                }
            }

            macro_rules! store {
                ($t: ty, $addr: expr, $val: expr) => {
                    {
                        let ptr = ($addr) as usize;
                        if let Some(b) = self.heap.get_mut(ptr..(ptr + std::mem::size_of::<$t>())) {
                            *bytemuck::from_bytes_mut::<$t>(b) = $val;
                        } else {
                            self.set_trap();
                            return None;
                        }
                    }
                }
            }

            match instruction {
                instruction::Instruction::Unreachable => {
                    self.set_trap();
                    return None;
                }
                instruction::Instruction::Nop => {

                }

                instruction::Instruction::Block => {
                    let header = stream.get::<instruction::BlockHeader>()?;
                    let code = stream.get_byte_slice(header.length as usize)?;

                    let stack_h = self.stack.len();
                    self.exec_block(code, locals)?;

                    let exp_h = stack_h + match header.ty {
                        instruction::BlockType::Functional(i) => self.module.types[i as usize].outputs.len(),
                        instruction::BlockType::ResolvingTo(_) => 1,
                        instruction::BlockType::Void => 0,
                    };

                    if stack_h != exp_h { panic!("Unexpected height"); }
                }

                instruction::Instruction::Loop => {}
                instruction::Instruction::If => {}
                instruction::Instruction::Else => {}
                instruction::Instruction::Br => {}
                instruction::Instruction::BrIf => {}
                instruction::Instruction::Return => {}
                instruction::Instruction::BrTable => {}
                instruction::Instruction::Call => {}
                instruction::Instruction::CallIndirect => {}
                instruction::Instruction::Drop => _ = pop!(),
                instruction::Instruction::Select => push!({
                    let cond = pop!(as_i32);
                    let lhs = pop!();
                    let rhs = pop!();

                    if cond != 0 { rhs } else { lhs }
                }),
                instruction::Instruction::SelectTyped => {}
                instruction::Instruction::LocalGet => push!(*locals.get(stream.get::<u32>()? as usize)?),
                instruction::Instruction::LocalSet => {
                    let l = locals.get_mut(stream.get::<u32>()? as usize)?;
                    let v = pop!();

                    if l.get_type() != v.get_type() {
                        return None;
                    }

                    *l = v;
                }
                instruction::Instruction::LocalTee => {
                    let l = locals.get_mut(stream.get::<u32>()? as usize)?;
                    let v = *self.stack.last()?;

                    if l.get_type() != v.get_type() {
                        return None;
                    }

                    *l = v;
                }
                instruction::Instruction::GlobalGet => push!(*self.globals.get(stream.get::<u32>()? as usize)?),
                instruction::Instruction::GlobalSet => {
                    let l = self.globals.get_mut(stream.get::<u32>()? as usize)?;
                    let v = pop!();

                    if l.get_type() != v.get_type() {
                        return None;
                    }

                    *l = v;
                }
                instruction::Instruction::TableGet => {}
                instruction::Instruction::TableSet => {}
                instruction::Instruction::I32Load | instruction::Instruction::F32Load => push!(load!(u32, stream.get::<u32>()? + pop!(as_u32))),
                instruction::Instruction::I64Load | instruction::Instruction::F64Load => push!(load!(u64, stream.get::<u32>()? + pop!(as_u32))),
                instruction::Instruction::I32Load8S  => push!(load!(i8,  stream.get::<u32>()? + pop!(as_u32)) as i32),
                instruction::Instruction::I32Load8U  => push!(load!(u8,  stream.get::<u32>()? + pop!(as_u32)) as u32),
                instruction::Instruction::I32Load16S => push!(load!(i16, stream.get::<u32>()? + pop!(as_u32)) as i32),
                instruction::Instruction::I32Load16U => push!(load!(u16, stream.get::<u32>()? + pop!(as_u32)) as u32),
                instruction::Instruction::I64Load8S  => push!(load!(i8,  stream.get::<u32>()? + pop!(as_u32)) as i64),
                instruction::Instruction::I64Load8U  => push!(load!(u8,  stream.get::<u32>()? + pop!(as_u32)) as u64),
                instruction::Instruction::I64Load16S => push!(load!(i16, stream.get::<u32>()? + pop!(as_u32)) as i64),
                instruction::Instruction::I64Load16U => push!(load!(u16, stream.get::<u32>()? + pop!(as_u32)) as u64),
                instruction::Instruction::I64Load32S => push!(load!(i32, stream.get::<u32>()? + pop!(as_u32)) as i64),
                instruction::Instruction::I64Load32U => push!(load!(u32, stream.get::<u32>()? + pop!(as_u32)) as u64),
                instruction::Instruction::I32Store   => store!(i32, stream.get::<u32>()? + pop!(as_u32), pop!(as_i32)),
                instruction::Instruction::I64Store   => store!(i64, stream.get::<u32>()? + pop!(as_u32), pop!(as_i64)),
                instruction::Instruction::F32Store   => store!(f32, stream.get::<u32>()? + pop!(as_u32), pop!(as_f32)),
                instruction::Instruction::F64Store   => store!(f64, stream.get::<u32>()? + pop!(as_u32), pop!(as_f64)),
                instruction::Instruction::I32Store8  => store!(u8,  stream.get::<u32>()? + pop!(as_u32), (pop!(as_u32) & 0x000000FF) as u8 ),
                instruction::Instruction::I32Store16 => store!(u16, stream.get::<u32>()? + pop!(as_u32), (pop!(as_u32) & 0x0000FFFF) as u16),
                instruction::Instruction::I64Store8  => store!(u8,  stream.get::<u32>()? + pop!(as_u32), (pop!(as_u64) & 0x000000FF) as u8 ),
                instruction::Instruction::I64Store16 => store!(u16, stream.get::<u32>()? + pop!(as_u32), (pop!(as_u64) & 0x0000FFFF) as u16),
                instruction::Instruction::I64Store32 => store!(u32, stream.get::<u32>()? + pop!(as_u32), (pop!(as_u64) & 0xFFFFFFFF) as u32),
                instruction::Instruction::MemorySize => {}
                instruction::Instruction::MemoryGrow => {}
                instruction::Instruction::I32Const => push!(stream.get::<i32>()?),
                instruction::Instruction::I64Const => push!(stream.get::<i64>()?),
                instruction::Instruction::F32Const => push!(stream.get::<f32>()?),
                instruction::Instruction::F64Const => push!(stream.get::<f64>()?),
                instruction::Instruction::I32Eqz  => push!((pop!(as_i32) == 0) as i32),
                instruction::Instruction::I32Eq   => push!((pop!(as_i32) == pop!(as_i32)) as i32),
                instruction::Instruction::I32Ne   => push!((pop!(as_i32) != pop!(as_i32)) as i32),
                instruction::Instruction::I32LtS  => push!((pop!(as_i32) < pop!(as_i32)) as i32),
                instruction::Instruction::I32LtU  => push!((pop!(as_u32) < pop!(as_u32)) as i32),
                instruction::Instruction::I32GtS  => push!((pop!(as_i32) > pop!(as_i32)) as i32),
                instruction::Instruction::I32GtU  => push!((pop!(as_u32) > pop!(as_u32)) as i32),
                instruction::Instruction::I32LeS  => push!((pop!(as_i32) <= pop!(as_i32)) as i32),
                instruction::Instruction::I32LeU  => push!((pop!(as_u32) <= pop!(as_u32)) as i32),
                instruction::Instruction::I32GeS  => push!((pop!(as_i32) >= pop!(as_i32)) as i32),
                instruction::Instruction::I32GeU  => push!((pop!(as_u32) >= pop!(as_u32)) as i32),
                instruction::Instruction::I64Eqz => push!((pop!(as_i64) == 0) as i32),
                instruction::Instruction::I64Eq => push!((pop!(as_i64) == pop!(as_i64)) as i32),
                instruction::Instruction::I64Ne => push!((pop!(as_i64) != pop!(as_i64)) as i32),
                instruction::Instruction::I64LtS => push!((pop!(as_i64) < pop!(as_i64)) as i32),
                instruction::Instruction::I64LtU => push!((pop!(as_u64) < pop!(as_u64)) as i32),
                instruction::Instruction::I64GtS => push!((pop!(as_i64) > pop!(as_i64)) as i32),
                instruction::Instruction::I64GtU => push!((pop!(as_u64) > pop!(as_u64)) as i32),
                instruction::Instruction::I64LeS => push!((pop!(as_i64) <= pop!(as_i64)) as i32),
                instruction::Instruction::I64LeU => push!((pop!(as_u64) <= pop!(as_u64)) as i32),
                instruction::Instruction::I64GeS => push!((pop!(as_i64) >= pop!(as_i64)) as i32),
                instruction::Instruction::I64GeU => push!((pop!(as_u64) >= pop!(as_u64)) as i32),
                instruction::Instruction::F32Eq => push!((pop!(as_f32) == pop!(as_f32)) as i32),
                instruction::Instruction::F32Ne => push!((pop!(as_f32) != pop!(as_f32)) as i32),
                instruction::Instruction::F32Lt => push!((pop!(as_f32) < pop!(as_f32)) as i32),
                instruction::Instruction::F32Gt => push!((pop!(as_f32) > pop!(as_f32)) as i32),
                instruction::Instruction::F32Le => push!((pop!(as_f32) <= pop!(as_f32)) as i32),
                instruction::Instruction::F32Ge => push!((pop!(as_f32) >= pop!(as_f32)) as i32),
                instruction::Instruction::F64Eq => push!((pop!(as_f64) == pop!(as_f64)) as i32),
                instruction::Instruction::F64Ne => push!((pop!(as_f64) != pop!(as_f64)) as i32),
                instruction::Instruction::F64Lt => push!((pop!(as_f64) < pop!(as_f64)) as i32),
                instruction::Instruction::F64Gt => push!((pop!(as_f64) > pop!(as_f64)) as i32),
                instruction::Instruction::F64Le => push!((pop!(as_f64) <= pop!(as_f64)) as i32),
                instruction::Instruction::F64Ge => push!((pop!(as_f64) >= pop!(as_f64)) as i32),
                instruction::Instruction::I32Clz => push!(pop!(as_i32).leading_zeros()),
                instruction::Instruction::I32Ctz => push!(pop!(as_i32).trailing_zeros()),
                instruction::Instruction::I32Popcnt => push!(pop!(as_i32).count_ones()),
                instruction::Instruction::I32Add  => push!(pop!(as_i32) + pop!(as_i32)),
                instruction::Instruction::I32Sub  => push!(pop!(as_i32) - pop!(as_i32)),
                instruction::Instruction::I32Mul  => push!(pop!(as_i32) * pop!(as_i32)),
                instruction::Instruction::I32DivS => push!(pop!(as_i32) / pop!(as_i32)),
                instruction::Instruction::I32DivU => push!(pop!(as_u32) / pop!(as_u32)),
                instruction::Instruction::I32RemS => push!(pop!(as_i32) % pop!(as_i32)),
                instruction::Instruction::I32RemU => push!(pop!(as_u32) % pop!(as_u32)),
                instruction::Instruction::I32And  => push!(pop!(as_i32) & pop!(as_i32)),
                instruction::Instruction::I32Or   => push!(pop!(as_i32) | pop!(as_i32)),
                instruction::Instruction::I32Xor  => push!(pop!(as_i32) ^ pop!(as_i32)),
                instruction::Instruction::I32Shl  => push!(pop!(as_i32) << pop!(as_i32)),
                instruction::Instruction::I32ShrS => push!(pop!(as_i32) >> pop!(as_i32)),
                instruction::Instruction::I32ShrU => push!(pop!(as_u32) >> pop!(as_u32)),
                instruction::Instruction::I32Rotl => push!(pop!(as_u32).rotate_left(pop!(as_u32))),
                instruction::Instruction::I32Rotr => push!(pop!(as_u32).rotate_right(pop!(as_u32))),
                instruction::Instruction::I64Clz => push!(pop!(as_i64).leading_zeros()),
                instruction::Instruction::I64Ctz => push!(pop!(as_i64).trailing_zeros()),
                instruction::Instruction::I64Popcnt => push!(pop!(as_i64).count_ones()),
                instruction::Instruction::I64Add => push!(pop!(as_i64) + pop!(as_i64)),
                instruction::Instruction::I64Sub => push!(pop!(as_i64) - pop!(as_i64)),
                instruction::Instruction::I64Mul => push!(pop!(as_i64) * pop!(as_i64)),
                instruction::Instruction::I64DivS => push!(pop!(as_i64) / pop!(as_i64)),
                instruction::Instruction::I64DivU => push!(pop!(as_u64) / pop!(as_u64)),
                instruction::Instruction::I64RemS => push!(pop!(as_i64) % pop!(as_i64)),
                instruction::Instruction::I64RemU => push!(pop!(as_u64) % pop!(as_u64)),
                instruction::Instruction::I64And => push!(pop!(as_i64) & pop!(as_i64)),
                instruction::Instruction::I64Or => push!(pop!(as_i64) | pop!(as_i64)),
                instruction::Instruction::I64Xor => push!(pop!(as_i64) ^ pop!(as_i64)),
                instruction::Instruction::I64Shl => push!(pop!(as_i64) << pop!(as_i64)),
                instruction::Instruction::I64ShrS => push!(pop!(as_i64) >> pop!(as_i64)),
                instruction::Instruction::I64ShrU => push!(pop!(as_u64) >> pop!(as_u64)),
                instruction::Instruction::I64Rotl => push!(pop!(as_u64).rotate_left(pop!(as_u64) as u32)),
                instruction::Instruction::I64Rotr => push!(pop!(as_u64).rotate_right(pop!(as_u64) as u32)),
                instruction::Instruction::F32Abs => *top!(as_f32_mut) = top!(as_f32).abs(),
                instruction::Instruction::F32Neg => *top!(as_f32_mut) = -top!(as_f32),
                instruction::Instruction::F32Ceil => *top!(as_f32_mut) = top!(as_f32).ceil(),
                instruction::Instruction::F32Floor => *top!(as_f32_mut) = top!(as_f32).floor(),
                instruction::Instruction::F32Trunc => *top!(as_f32_mut) = top!(as_f32).trunc(),
                instruction::Instruction::F32Nearest => *top!(as_f32_mut) = top!(as_f32).round(),
                instruction::Instruction::F32Sqrt => *top!(as_f32_mut) = top!(as_f32).sqrt(),
                instruction::Instruction::F32Add => *top!(as_f32_mut) += pop!(as_f32),
                instruction::Instruction::F32Sub => *top!(as_f32_mut) -= pop!(as_f32),
                instruction::Instruction::F32Mul => *top!(as_f32_mut) *= pop!(as_f32),
                instruction::Instruction::F32Div => *top!(as_f32_mut) /= pop!(as_f32),
                instruction::Instruction::F32Min => *top!(as_f32_mut) = f32::min(top!(as_f32), pop!(as_f32)),
                instruction::Instruction::F32Max => *top!(as_f32_mut) = f32::max(top!(as_f32), pop!(as_f32)),
                instruction::Instruction::F32CopySign => *top!(as_f32_mut) = f32::copysign(top!(as_f32), pop!(as_f32)),
                instruction::Instruction::F64Abs => push!(pop!(as_f64).abs()),
                instruction::Instruction::F64Neg => push!(-pop!(as_f64)),
                instruction::Instruction::F64Ceil => push!(pop!(as_f64).ceil()),
                instruction::Instruction::F64Floor => push!(pop!(as_f64).floor()),
                instruction::Instruction::F64Trunc => push!(pop!(as_f64).trunc()),
                instruction::Instruction::F64Nearest => push!(pop!(as_f64).round()),
                instruction::Instruction::F64Sqrt => push!(pop!(as_f64).sqrt()),
                instruction::Instruction::F64Add => push!(pop!(as_f64) + pop!(as_f64)),
                instruction::Instruction::F64Sub => push!(pop!(as_f64) - pop!(as_f64)),
                instruction::Instruction::F64Mul => push!(pop!(as_f64) * pop!(as_f64)),
                instruction::Instruction::F64Div => push!(pop!(as_f64) / pop!(as_f64)),
                instruction::Instruction::F64Min => push!(f64::min(pop!(as_f64), pop!(as_f64))),
                instruction::Instruction::F64Max => push!(f64::max(pop!(as_f64), pop!(as_f64))),
                instruction::Instruction::F64CopySign => push!(f64::copysign(pop!(as_f64), pop!(as_f64))),
                instruction::Instruction::I32WrapI64 => push!((pop!(as_i64) & 0xFFFFFFFF) as i32),
                instruction::Instruction::I32TruncF32S => push!(pop!(as_f32) as i32),
                instruction::Instruction::I32TruncF32U => push!(pop!(as_f32) as u32),
                instruction::Instruction::I32TruncF64S => push!(pop!(as_f64) as i32),
                instruction::Instruction::I32TruncF64U => push!(pop!(as_f64) as u32),
                instruction::Instruction::I64ExtendI32S => push!(pop!(as_i32) as i64),
                instruction::Instruction::I64ExtendI32U => push!(pop!(as_u32) as u64),
                instruction::Instruction::I64TruncF32S => push!(pop!(as_f32) as i64),
                instruction::Instruction::I64TruncF32U => push!(pop!(as_f32) as u64),
                instruction::Instruction::I64TruncF64S => push!(pop!(as_f64) as i64),
                instruction::Instruction::I64TruncF64U => push!(pop!(as_f64) as u64),
                instruction::Instruction::F32ConvertI32S => push!(pop!(as_i32) as f32),
                instruction::Instruction::F32ConvertI32U => push!(pop!(as_u32) as f32),
                instruction::Instruction::F32ConvertI64S => push!(pop!(as_i64) as f32),
                instruction::Instruction::F32ConvertI64U => push!(pop!(as_u64) as f32),
                instruction::Instruction::F32DemoteF64 => push!(pop!(as_f64) as f32),
                instruction::Instruction::F64ConvertI32S => push!(pop!(as_i32) as f64),
                instruction::Instruction::F64ConvertI32U => push!(pop!(as_u32) as f64),
                instruction::Instruction::F64ConvertI64S => push!(pop!(as_i64) as f64),
                instruction::Instruction::F64ConvertI64U => push!(pop!(as_u64) as f64),
                instruction::Instruction::F64PromoteF32 => push!(pop!(as_f32) as f64),
                instruction::Instruction::I32ReinterpretF32 => push!(f32::to_bits(pop!(as_f32))),
                instruction::Instruction::I64ReinterpretF64 => push!(f64::to_bits(pop!(as_f64))),
                instruction::Instruction::F32ReinterpretI32 => push!(f32::from_bits(pop!(as_u32))),
                instruction::Instruction::F64ReinterpretI64 => push!(f64::from_bits(pop!(as_u64))),
                instruction::Instruction::I32Extend8S => {}
                instruction::Instruction::I32Extend16S => {}
                instruction::Instruction::I64Extend8S => {}
                instruction::Instruction::I64Extend16S => {}
                instruction::Instruction::I64Extend32S => {}
                instruction::Instruction::RefNull => {}
                instruction::Instruction::RefIsNull => {}
                instruction::Instruction::RefFunc => {}
                instruction::Instruction::System => {}
                instruction::Instruction::Vector => {}
            }
        }

        Some(())
    }

    pub fn call(&mut self, name: &str, arguments: &[types::Value]) -> Option<Vec<types::Value>> {
        let export = self.module.exports.get(name)?;

        if export.ty != types::ExportType::Function {
            return None;
        }

        let func = self.module.functions.get(export.index as usize)?;
        let func_ty = self.module.types.get(func.type_id as usize)?;


        // Validate arguments
        if arguments.len() != func_ty.inputs.len() {
            return None;
        }
        let mut locals = arguments.iter().zip(&func_ty.inputs)
            .map(|(value, expected_type)| -> Option<Value> {
                if value.get_type() != *expected_type {
                    None
                } else {
                    Some(*value)
                }
            })
            .chain(func.locals.iter().map(|ty| Some(Value::default_with_type(*ty))))
            .collect::<Option<Vec<Value>>>()?;

        self.exec_block(&func.expression, &mut locals)?;

        if self.stack.len() != func_ty.outputs.len() {
            self.trapped = true;
            return None;
        }
        let mut s = Vec::new();
        std::mem::swap(&mut s, &mut self.stack);
        s.reverse();

        Some(s)
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

fn main() {
    let module = Module::new(Source::WASM(include_bytes!("../test/math.wasm"))).unwrap();

    let mut instance = module.create_instance();

    let v = instance.call("f_inv_sqrt", &[4.0f32.into()]).expect("No return values");

    println!("Result: {}", v[0].as_f32().expect("Return value must be f32"));
} // fn main

// file main.rs
