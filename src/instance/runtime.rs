use crate::{instruction::{self, BlockType}, types::{self, FunctionType, Value}, util::binary_stream::BinaryInputStream, Expression, InstanceImpl, Mutability, NumberType, Type};
use super::{BlockExecutionResult, StackItem};

impl InstanceImpl {
    pub(crate) fn call_by_id(&mut self, func_id: u32) -> Option<BlockExecutionResult> {
        let module = self.module.clone();
        let func = module.functions.get(func_id as usize)?;
        let func_ty = module.types.get(func.type_id as usize)?;

        // iiiu, but quite performant iiiu))
        if self.stack.len() < func_ty.inputs.len() {
            return None;
        }

        let mut locals = Vec::with_capacity(func_ty.inputs.len() + func.locals.len());

        // extend from stack top
        locals.extend(unsafe {
            let l = self.stack.len();
            self.stack.get_unchecked(l - func_ty.inputs.len()..l)
        }.iter().rev());

        // truncate stack
        self.stack.truncate(self.stack.len() - func_ty.inputs.len());

        locals.extend(std::iter::repeat(StackItem::default()).take(func.locals.len()));

        let block_exec_result = self.exec_block(
            0,
            func_ty.outputs.len(),
            &func.expression.instructions,
            &mut locals
        )?;

        if let BlockExecutionResult::Branch { depth } = block_exec_result {
            Some(BlockExecutionResult::Branch { depth: depth - 1 })
        } else {
            Some(BlockExecutionResult::Ok)
        }
    }

    fn exec_block(&mut self, inputs: usize, outputs: usize, block: &[u8], locals: &mut [StackItem]) -> Option<BlockExecutionResult> {
        if self.trapped {
            panic!("Can't execute while trapped");
        }

        let mut stream = BinaryInputStream::new(block);

        let initial_stack_height = self.stack.len();

        let exec_result = 'instruction_exec: loop {
            let byte = match stream.get::<u8>() {
                Some(v) => v,
                None => break 'instruction_exec BlockExecutionResult::Ok,
            };

            let instruction = instruction::Instruction::try_from(byte).expect(format!("Fatal error: unknown instruction {:02X}", byte).as_str());

            macro_rules! pop {
                ($exp_t: ident) => {
                    unsafe {
                        (self.stack.pop()?).$exp_t()
                    }
                };
                () => (self.stack.pop()?);
            }

            macro_rules! top {
                ($exp_t: ident) => (unsafe { self.stack.last_mut()?.$exp_t() } );
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
                ($t: ty, $addr: expr) => { self.memory.load_unaligned::<$t>(($addr) as usize)? };
            }

            macro_rules! store {
                ($t: ty, $addr: expr, $val: expr) => { self.memory.store::<$t>(($addr) as usize, $val)? };
            }

            macro_rules! exec_block {
                ($inputs: expr, $outputs: expr, $code: expr, $locals: expr) => {
                    match self.exec_block($inputs, $outputs, $code, $locals)? {
                        BlockExecutionResult::Return => break 'instruction_exec BlockExecutionResult::Return,
                        BlockExecutionResult::Branch { depth } => if depth > 0 { break 'instruction_exec BlockExecutionResult::Branch { depth: depth - 1 } }
                        BlockExecutionResult::Ok => {}
                    }
                };
            }

            match instruction {
                instruction::Instruction::Unreachable => {
                    // Works exactly as panic/exception
                    return None;
                }
                instruction::Instruction::Nop => {
                }
                instruction::Instruction::Block => {
                    let header = stream.get::<instruction::BlockHeader>()?;
                    let code = stream.get_byte_slice(header.length as usize)?;

                    let stack_h = self.stack.len();

                    exec_block!(
                        header.consume_count as usize,
                        header.output_count as usize, 
                        code,
                        locals
                    );

                    if stack_h != stack_h + header.output_count as usize { panic!("Unexpected stack height"); }
                }
                instruction::Instruction::Loop => {

                }
                instruction::Instruction::If => {
                    let header = stream.get::<instruction::BranchHeader>()?;

                    let code = stream.get_byte_slice(header.then_length as usize)?;

                    let (inputs, outputs) = (header.consume_count as usize, header.output_count as usize);

                    if pop!(as_u32) != 0 {
                        exec_block!(inputs, outputs, code, locals);

                        if stream.check::<u8>()? == instruction::Instruction::Else as u8 {
                            stream.skip(1)?;
                            stream.skip(header.else_length as usize)?;
                        }
                    } else if stream.check::<u8>()? == instruction::Instruction::Else as u8 {
                        stream.skip(1)?;
                        exec_block!(inputs, outputs, stream.get_byte_slice(header.else_length as usize)?, locals);
                    }
                }
                instruction::Instruction::Else => {
                    panic!("'Else' instruction must occur after 'If' instruction only.");
                }
                instruction::Instruction::Br => {

                }
                instruction::Instruction::BrIf => {

                }
                instruction::Instruction::Return => {
                    break 'instruction_exec BlockExecutionResult::Return;
                }
                instruction::Instruction::BrTable => {

                }
                instruction::Instruction::Call => {
                    match self.call_by_id(stream.get::<u32>()?)? {
                        BlockExecutionResult::Branch { depth } => if depth > 0 { break 'instruction_exec BlockExecutionResult::Branch { depth: depth - 1 } }
                        BlockExecutionResult::Ok | BlockExecutionResult::Return => {  }
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
                    *locals.get_mut(stream.get::<u32>()? as usize)? = pop!();
                }
                instruction::Instruction::LocalTee => {
                    *locals.get_mut(stream.get::<u32>()? as usize)? = *self.stack.last()?;
                }
                instruction::Instruction::GlobalGet => push!(self.globals.get(stream.get::<u32>()? as usize)?.value),
                instruction::Instruction::GlobalSet => {
                    let l = self.globals.get_mut(stream.get::<u32>()? as usize)?;
                    let v = pop!();

                    if l.mutability != Mutability::Mut {
                        return None;
                    }
                    l.value = v;
                }
                instruction::Instruction::TableGet => {}
                instruction::Instruction::TableSet => {}
                instruction::Instruction::I32Load => push!(load!(u32, stream.get::<u32>()? + pop!(as_u32))),
                instruction::Instruction::F32Load => push!(load!(f32, stream.get::<u32>()? + pop!(as_u32))),
                instruction::Instruction::I64Load => push!(load!(u64, stream.get::<u32>()? + pop!(as_u32))),
                instruction::Instruction::F64Load => push!(load!(f64, stream.get::<u32>()? + pop!(as_u32))),
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
                instruction::Instruction::MemorySize => push!(self.memory.page_count() as u32),

                // TODO Add memory grow failure
                instruction::Instruction::MemoryGrow => {
                    let size = self.memory.page_count();
                    let page_count = pop!(as_u32) as usize;

                    self.memory.grow(page_count);

                    push!(size as u32);
                }

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
                instruction::Instruction::RefFunc => {}
                instruction::Instruction::System => todo!("TODO: Implement system instruction extension"),
                instruction::Instruction::Vector => todo!("TODO: Implement vector instruction extension"),
            }
        };

        // validate returnvalues
        if self.stack.len() < outputs {
            None
        } else {
            let copy_range = (self.stack.len() - outputs)..self.stack.len();
            let copy_dest = initial_stack_height - inputs;

            // copy 'return' operands
            self.stack.copy_within(copy_range, copy_dest);
            // truncate stack
            self.stack.truncate(copy_dest + outputs);

            Some(exec_result)
        }
    } // fn exec_block


    pub fn exec_expression(&mut self, expr: &Expression, expected_result: &[Type]) -> Option<Vec<Value>> {
        if expected_result.len() > 1 {
            panic!("TODO: fix expression execution...");
        }

        self.exec_block(0, expected_result.len(), &expr.instructions, &mut [])?;

        return Some(self.stack
            .get(self.stack.len().checked_sub(expected_result.len())?..self.stack.len())?
            .iter()
            .zip(expected_result.iter())
            .map(|(elem, ty)| elem.to_value(*ty))
            .collect::<Vec<Value>>())
    }

    pub fn call(&mut self, id: u32, arguments: &[Value]) -> Option<Vec<Value>> {
        let module = self.module.clone();

        let func = module.functions.get(id as usize)?;
        let func_ty = module.types.get(func.type_id as usize)?;

        for a in arguments {
            self.stack.push(StackItem::from(*a));
        }

        if let BlockExecutionResult::Branch { depth } = self.call_by_id(id)? {
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

        Some(s
            .iter()
            .rev()
            .zip(func_ty.outputs.iter())
            .map(|(item, ty)| item.to_value(*ty))
            .collect::<Vec<_>>()
        )
    }
}
