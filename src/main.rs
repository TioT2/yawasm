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

    fn call_by_id(&mut self, func_id: u32) -> Option<BlockExecutionResult> {
        let func = self.module.functions.get(func_id as usize)?;
        let func_ty = self.module.types.get(func.type_id as usize)?;

        let mut locals = func_ty.inputs.iter()
            .map(|expected_type| -> Option<Value> {
                let p = self.stack.pop()?;

                if *expected_type != p.get_type() {
                    return None;
                }

                Some(p)
            })
            .chain(func.locals.iter().map(|ty| Some(Value::default_with_type(*ty))))
            .collect::<Option<Vec<Value>>>()?;

        if let BlockExecutionResult::Branch { depth } = self.exec_block(&func.expression, &mut locals)? {
            Some(BlockExecutionResult::Branch { depth: depth - 1 })
        } else {
            Some(BlockExecutionResult::Ok)
        }
    }

    fn exec_block(&mut self, block: &[u8], locals: &mut [types::Value]) -> Option<BlockExecutionResult> {
        if self.trapped {
            panic!("Can't execute while trapped");
        }

        let mut stream = BinaryInputStream::new(block);

        'execution_loop: while let Some(byte) = stream.get::<u8>() {
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
                };
            }

            macro_rules! load {
                ($t: ty, $addr: expr) => {
                    {
                        let ptr = ($addr) as usize;
                        if let Some(b) = self.heap.get(ptr..(ptr + std::mem::size_of::<$t>())) {
                            *bytemuck::from_bytes::<$t>(b)
                        } else {
                            return None;
                        }
                    }
                };
            }

            macro_rules! store {
                ($t: ty, $addr: expr, $val: expr) => {
                    {
                        let ptr = ($addr) as usize;
                        if let Some(b) = self.heap.get_mut(ptr..(ptr + std::mem::size_of::<$t>())) {
                            *bytemuck::from_bytes_mut::<$t>(b) = $val;
                        } else {
                            return None;
                        }
                    }
                };
            }

            macro_rules! exec_block {
                ($code: expr, $locals: expr) => {
                    match self.exec_block($code, $locals)? {
                        BlockExecutionResult::Return => return Some(BlockExecutionResult::Return),
                        BlockExecutionResult::Branch { depth } => if depth > 0 { return Some(BlockExecutionResult::Branch { depth: depth - 1 }) }
                        BlockExecutionResult::Ok => {}
                    }
                };
            }

            match instruction {
                instruction::Instruction::Unreachable => {
                    return None;
                }
                instruction::Instruction::Nop => {

                }
                instruction::Instruction::Block => {
                    let header = stream.get::<instruction::BlockHeader>()?;
                    let code = stream.get_byte_slice(header.length as usize)?;

                    let stack_h = self.stack.len();

                    exec_block!(code, locals);

                    let exp_h = stack_h + match header.ty {
                        instruction::BlockType::Functional(i) => self.module.types[i as usize].outputs.len(),
                        instruction::BlockType::ResolvingTo(_) => 1,
                        instruction::BlockType::Void => 0,
                    };

                    if stack_h != exp_h { panic!("Unexpected height"); }
                }
                instruction::Instruction::Loop => {

                }
                instruction::Instruction::If => {
                    let header = stream.get::<instruction::BlockHeader>()?;
                    let code = stream.get_byte_slice(header.length as usize)?;

                    if pop!(as_u32) != 0 {
                        exec_block!(code, locals);

                        if stream.check::<u8>()? == instruction::Instruction::Else as u8 {
                            stream.skip(1)?;
                            let l = stream.get::<instruction::BlockHeader>()?.length;
                            stream.skip(l as usize)?;
                        }
                    } else if stream.check::<u8>()? == instruction::Instruction::Else as u8 {
                        stream.skip(1)?;
                        let else_header = stream.get::<instruction::BlockHeader>()?;
                        exec_block!(stream.get_byte_slice(else_header.length as usize)?, locals);
                    }
                }
                instruction::Instruction::Else => {
                    unreachable!("'Else' instruction must occur after 'If' instruction only.");
                }
                instruction::Instruction::Br => {

                }
                instruction::Instruction::BrIf => {

                }
                instruction::Instruction::Return => {
                    return Some(BlockExecutionResult::Return);
                }
                instruction::Instruction::BrTable => {

                }
                instruction::Instruction::Call => {
                    match self.call_by_id(stream.get::<u32>()?)? {
                        BlockExecutionResult::Branch { depth } => if depth > 0 { return Some(BlockExecutionResult::Branch { depth: depth - 1 }) }
                        BlockExecutionResult::Ok | BlockExecutionResult::Return => {
                        }
                    }
                }
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

                instruction::Instruction::I32Eqz  => *top!() = ((top!(as_i32) == 0) as i32).into(),
                instruction::Instruction::I32Eq   => *top!() = ((pop!(as_i32) == top!(as_i32)) as i32).into(),
                instruction::Instruction::I32Ne   => *top!() = ((pop!(as_i32) != top!(as_i32)) as i32).into(),
                instruction::Instruction::I32LtS  => *top!() = ((pop!(as_i32) > top!(as_i32)) as i32).into(),
                instruction::Instruction::I32LtU  => *top!() = ((pop!(as_u32) > top!(as_u32)) as i32).into(),
                instruction::Instruction::I32GtS  => *top!() = ((pop!(as_i32) < top!(as_i32)) as i32).into(),
                instruction::Instruction::I32GtU  => *top!() = ((pop!(as_u32) < top!(as_u32)) as i32).into(),
                instruction::Instruction::I32LeS  => *top!() = ((pop!(as_i32) >= top!(as_i32)) as i32).into(),
                instruction::Instruction::I32LeU  => *top!() = ((pop!(as_u32) >= top!(as_u32)) as i32).into(),
                instruction::Instruction::I32GeS  => *top!() = ((pop!(as_i32) <= top!(as_i32)) as i32).into(),
                instruction::Instruction::I32GeU  => *top!() = ((pop!(as_u32) <= top!(as_u32)) as i32).into(),

                instruction::Instruction::I64Eqz  => *top!() = ((top!(as_i64) == 0) as i32).into(),
                instruction::Instruction::I64Eq   => *top!() = ((pop!(as_i64) == top!(as_i64)) as i32).into(),
                instruction::Instruction::I64Ne   => *top!() = ((pop!(as_i64) != top!(as_i64)) as i32).into(),
                instruction::Instruction::I64LtS  => *top!() = ((pop!(as_i64) > top!(as_i64)) as i32).into(),
                instruction::Instruction::I64LtU  => *top!() = ((pop!(as_u64) > top!(as_u64)) as i32).into(),
                instruction::Instruction::I64GtS  => *top!() = ((pop!(as_i64) < top!(as_i64)) as i32).into(),
                instruction::Instruction::I64GtU  => *top!() = ((pop!(as_u64) < top!(as_u64)) as i32).into(),
                instruction::Instruction::I64LeS  => *top!() = ((pop!(as_i64) >= top!(as_i64)) as i32).into(),
                instruction::Instruction::I64LeU  => *top!() = ((pop!(as_u64) >= top!(as_u64)) as i32).into(),
                instruction::Instruction::I64GeS  => *top!() = ((pop!(as_i64) <= top!(as_i64)) as i32).into(),
                instruction::Instruction::I64GeU  => *top!() = ((pop!(as_u64) <= top!(as_u64)) as i32).into(),

                instruction::Instruction::F32Eq => *top!() = ((pop!(as_f32) == top!(as_f32)) as i32).into(),
                instruction::Instruction::F32Ne => *top!() = ((pop!(as_f32) != top!(as_f32)) as i32).into(),
                instruction::Instruction::F32Lt => *top!() = ((pop!(as_f32) > top!(as_f32)) as i32).into(),
                instruction::Instruction::F32Gt => *top!() = ((pop!(as_f32) < top!(as_f32)) as i32).into(),
                instruction::Instruction::F32Le => *top!() = ((pop!(as_f32) >= top!(as_f32)) as i32).into(),
                instruction::Instruction::F32Ge => *top!() = ((pop!(as_f32) <= top!(as_f32)) as i32).into(),

                instruction::Instruction::F64Eq => *top!() = ((pop!(as_f64) == top!(as_f64)) as i32).into(),
                instruction::Instruction::F64Ne => *top!() = ((pop!(as_f64) != top!(as_f64)) as i32).into(),
                instruction::Instruction::F64Lt => *top!() = ((pop!(as_f64) > top!(as_f64)) as i32).into(),
                instruction::Instruction::F64Gt => *top!() = ((pop!(as_f64) < top!(as_f64)) as i32).into(),
                instruction::Instruction::F64Le => *top!() = ((pop!(as_f64) >= top!(as_f64)) as i32).into(),
                instruction::Instruction::F64Ge => *top!() = ((pop!(as_f64) <= top!(as_f64)) as i32).into(),

                instruction::Instruction::I32Clz => *top!(as_u32_mut) = top!(as_i32).leading_zeros(),
                instruction::Instruction::I32Ctz => *top!(as_u32_mut) = top!(as_i32).trailing_zeros(),
                instruction::Instruction::I32Popcnt => *top!(as_u32_mut) = top!(as_i32).count_ones(),
                instruction::Instruction::I32Add  => *top!(as_i32_mut) += pop!(as_i32),
                instruction::Instruction::I32Sub  => *top!(as_i32_mut) -= pop!(as_i32),
                instruction::Instruction::I32Mul  => *top!(as_i32_mut) *= pop!(as_i32),
                instruction::Instruction::I32DivS => *top!(as_i32_mut) /= pop!(as_i32),
                instruction::Instruction::I32DivU => *top!(as_u32_mut) /= pop!(as_u32),
                instruction::Instruction::I32RemS => *top!(as_i32_mut) %= pop!(as_i32),
                instruction::Instruction::I32RemU => *top!(as_u32_mut) %= pop!(as_u32),
                instruction::Instruction::I32And  => *top!(as_i32_mut) &= pop!(as_i32),
                instruction::Instruction::I32Or   => *top!(as_i32_mut) |= pop!(as_i32),
                instruction::Instruction::I32Xor  => *top!(as_i32_mut) ^= pop!(as_i32),
                instruction::Instruction::I32Shl  => *top!(as_i32_mut) <<= pop!(as_i32),
                instruction::Instruction::I32ShrS => *top!(as_i32_mut) >>= pop!(as_i32),
                instruction::Instruction::I32ShrU => *top!(as_u32_mut) >>= pop!(as_u32),
                instruction::Instruction::I32Rotl => *top!(as_u32_mut) = top!(as_u32).rotate_left(pop!(as_u32)),
                instruction::Instruction::I32Rotr => *top!(as_u32_mut) = top!(as_u32).rotate_right(pop!(as_u32)),

                instruction::Instruction::I64Clz => *top!(as_u64_mut) = top!(as_i64).leading_zeros() as u64,
                instruction::Instruction::I64Ctz => *top!(as_u64_mut) = top!(as_i64).trailing_zeros() as u64,
                instruction::Instruction::I64Popcnt => *top!(as_u64_mut) = top!(as_i64).count_ones() as u64,
                instruction::Instruction::I64Add => *top!(as_i64_mut) += pop!(as_i64),
                instruction::Instruction::I64Sub => *top!(as_i64_mut) -= pop!(as_i64),
                instruction::Instruction::I64Mul => *top!(as_i64_mut) *= pop!(as_i64),
                instruction::Instruction::I64DivS => *top!(as_i64_mut) /= pop!(as_i64),
                instruction::Instruction::I64DivU => *top!(as_u64_mut) /= pop!(as_u64),
                instruction::Instruction::I64RemS => *top!(as_i64_mut) %= pop!(as_i64),
                instruction::Instruction::I64RemU => *top!(as_u64_mut) %= pop!(as_u64),
                instruction::Instruction::I64And => *top!(as_i64_mut) &= pop!(as_i64),
                instruction::Instruction::I64Or => *top!(as_i64_mut) |= pop!(as_i64),
                instruction::Instruction::I64Xor => *top!(as_i64_mut) ^= pop!(as_i64),
                instruction::Instruction::I64Shl => *top!(as_i64_mut) <<= pop!(as_i64),
                instruction::Instruction::I64ShrS => *top!(as_i64_mut) >>= pop!(as_i64),
                instruction::Instruction::I64ShrU => *top!(as_u64_mut) >>= pop!(as_u64),
                instruction::Instruction::I64Rotl => *top!(as_u64_mut) = top!(as_u64).rotate_left(pop!(as_u64) as u32),
                instruction::Instruction::I64Rotr => *top!(as_u64_mut) = top!(as_u64).rotate_right(pop!(as_u64) as u32),

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

                instruction::Instruction::F64Abs => *top!(as_f64_mut) = top!(as_f64).abs(),
                instruction::Instruction::F64Neg => *top!(as_f64_mut) = -top!(as_f64),
                instruction::Instruction::F64Ceil => *top!(as_f64_mut) = top!(as_f64).ceil(),
                instruction::Instruction::F64Floor => *top!(as_f64_mut) = top!(as_f64).floor(),
                instruction::Instruction::F64Trunc => *top!(as_f64_mut) = top!(as_f64).trunc(),
                instruction::Instruction::F64Nearest => *top!(as_f64_mut) = top!(as_f64).round(),
                instruction::Instruction::F64Sqrt => *top!(as_f64_mut) = top!(as_f64).sqrt(),
                instruction::Instruction::F64Add => *top!(as_f64_mut) += pop!(as_f64),
                instruction::Instruction::F64Sub => *top!(as_f64_mut) -= pop!(as_f64),
                instruction::Instruction::F64Mul => *top!(as_f64_mut) *= pop!(as_f64),
                instruction::Instruction::F64Div => *top!(as_f64_mut) /= pop!(as_f64),
                instruction::Instruction::F64Min => *top!(as_f64_mut) = f64::min(top!(as_f64), pop!(as_f64)),
                instruction::Instruction::F64Max => *top!(as_f64_mut) = f64::max(top!(as_f64), pop!(as_f64)),
                instruction::Instruction::F64CopySign => *top!(as_f64_mut) = f64::copysign(top!(as_f64), pop!(as_f64)),

                instruction::Instruction::I32WrapI64 => *top!() = ((top!(as_i64) & 0xFFFFFFFF) as i32).into(),
                instruction::Instruction::I32TruncF32S => *top!() = (top!(as_f32) as i32).into(),
                instruction::Instruction::I32TruncF32U => *top!() = (top!(as_f32) as u32).into(),
                instruction::Instruction::I32TruncF64S => *top!() = (top!(as_f64) as i32).into(),
                instruction::Instruction::I32TruncF64U => *top!() = (top!(as_f64) as u32).into(),
                instruction::Instruction::I64ExtendI32S => *top!() = (top!(as_i32) as i64).into(),
                instruction::Instruction::I64ExtendI32U => *top!() = (top!(as_u32) as u64).into(),
                instruction::Instruction::I64TruncF32S => *top!() = (top!(as_f32) as i64).into(),
                instruction::Instruction::I64TruncF32U => *top!() = (top!(as_f32) as u64).into(),
                instruction::Instruction::I64TruncF64S => *top!() = (top!(as_f64) as i64).into(),
                instruction::Instruction::I64TruncF64U => *top!() = (top!(as_f64) as u64).into(),
                instruction::Instruction::F32ConvertI32S => *top!() = (top!(as_i32) as f32).into(),
                instruction::Instruction::F32ConvertI32U => *top!() = (top!(as_u32) as f32).into(),
                instruction::Instruction::F32ConvertI64S => *top!() = (top!(as_i64) as f32).into(),
                instruction::Instruction::F32ConvertI64U => *top!() = (top!(as_u64) as f32).into(),
                instruction::Instruction::F32DemoteF64 => *top!() = (top!(as_f64) as f32).into(),
                instruction::Instruction::F64ConvertI32S => *top!() = (top!(as_i32) as f64).into(),
                instruction::Instruction::F64ConvertI32U => *top!() = (top!(as_u32) as f64).into(),
                instruction::Instruction::F64ConvertI64S => *top!() = (top!(as_i64) as f64).into(),
                instruction::Instruction::F64ConvertI64U => *top!() = (top!(as_u64) as f64).into(),
                instruction::Instruction::F64PromoteF32 => *top!() = (top!(as_f32) as f64).into(),
                instruction::Instruction::I32ReinterpretF32 => *top!() = f32::to_bits(top!(as_f32)).into(),
                instruction::Instruction::I64ReinterpretF64 => *top!() = f64::to_bits(top!(as_f64)).into(),
                instruction::Instruction::F32ReinterpretI32 => *top!() = f32::from_bits(top!(as_u32)).into(),
                instruction::Instruction::F64ReinterpretI64 => *top!() = f64::from_bits(top!(as_u64)).into(),
                instruction::Instruction::I32Extend8S  => *top!() = (unsafe { std::mem::transmute::< u8,  i8>((top!(as_u32) & 0x000000FF) as  u8) } as i32).into(),
                instruction::Instruction::I32Extend16S => *top!() = (unsafe { std::mem::transmute::<u16, i16>((top!(as_u32) & 0x0000FFFF) as u16) } as i32).into(),
                instruction::Instruction::I64Extend8S  => *top!() = (unsafe { std::mem::transmute::< u8,  i8>((top!(as_u64) & 0x000000FF) as  u8) } as i64).into(),
                instruction::Instruction::I64Extend16S => *top!() = (unsafe { std::mem::transmute::<u16, i16>((top!(as_u64) & 0x0000FFFF) as u16) } as i64).into(),
                instruction::Instruction::I64Extend32S => *top!() = (unsafe { std::mem::transmute::<u32, i32>((top!(as_u64) & 0xFFFFFFFF) as u32) } as i64).into(),
                instruction::Instruction::RefNull => push!(Value::default_with_type(types::ValueType::Reference(types::ReferenceType::try_from(stream.get::<u8>()?).ok()?))),
                instruction::Instruction::RefIsNull => {
                    let t = top!();

                    *t = ((0 == match t {
                        types::Value::FuncRef(u) => *u,
                        types::Value::ExternRef(u) => *u,
                        _ => {
                            return None;
                        }
                    }) as i32).into();
                }
                instruction::Instruction::RefFunc => {}
                instruction::Instruction::System => todo!("TODO: Implement system instruction extension"),
                instruction::Instruction::Vector => todo!("TODO: Implement vector instruction extension"),
            }
        }

        Some(BlockExecutionResult::Ok)
    }

    pub fn call(&mut self, name: &str, arguments: &[types::Value]) -> Option<Vec<types::Value>> {
        let export = self.module.exports.get(name)?;

        if export.ty != types::ExportType::Function {
            return None;
        }

        let func = self.module.functions.get(export.index as usize)?;
        let func_ty = self.module.types.get(func.type_id as usize)?;

        for a in arguments {
            self.stack.push(*a);
        }

        if let BlockExecutionResult::Branch { depth } = self.call_by_id(export.index)? {
            if depth > 0 {
                return None;
            }
        }

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
