use crate::{
    instruction::{self, BlockHeader, BlockType, BranchHeader, Instruction},
    module::GlobalDescriptor,
    types::FunctionType,
    util::binary_stream::{BinaryInputStream, BinaryOutputStream},
    Mutability,
    Type
};

use super::{DecodeError, Opcode, CodeValidationError, EnumType, decode_type};

/// Input/Output ftype blocktype decoder
struct BlockTypeIODecoder {
    /// Just data binding
    out_binding: [Type; 1],
}

impl BlockTypeIODecoder {
    /// inout decoder representation structure
    pub fn new() -> Self {
        Self {
            out_binding: [Type::F32]
        }
    } // fn new

    /// Types getting function
    /// * `ty` - block to get type of
    /// * `func_types` - arrray of module function types
    /// * Returns Option<inputs, outputs>
    pub fn get<'t>(&'t mut self, ty: BlockType, func_types: &'t [FunctionType]) -> Option<(&'t [Type], &'t [Type])> {
        Some(match ty {
            BlockType::Functional(fn_id) => {
                let ftype = func_types.get(fn_id as usize)?;
                (&ftype.inputs, &ftype.outputs)
            }
            BlockType::ResolvingTo(res_ty) => {
                self.out_binding[0] = res_ty;
                (&[], &self.out_binding)
            }
            BlockType::Void => {
                (&[], &[])
            }
        })
    } // fn get
}

/// Validation stack frame representation structure
#[derive(Clone)]
struct ValidationStackFrame {
    /// Expected result then block ends
    pub expected_result: Vec<Type>,

    /// Type stack
    pub stack: Vec<Type>,
} // struct ValidationStackFrame

/// Block decode context
struct DecodeContext<'t, 'b> where 't: 'b {
    /// Stream to read raw bytecode from
    stream: &'b mut BinaryInputStream<'t>,

    /// Module function type set
    function_types: &'b [FunctionType],

    /// Module function type indices
    function_type_idx: &'b [u32],

    /// Module global descriptors
    globals: &'b [GlobalDescriptor],

    /// Module local descriptors
    locals: &'b [Type],

    /// Stack of validation frames, used for better safety
    stack: Vec<ValidationStackFrame>,
} // struct DecodeContext

impl ValidationStackFrame {
    /// Check stack frame top
    pub fn check_top(&self, expected_types: &[Type]) -> bool {
        let range = self.stack.len() - expected_types.len()..self.stack.len();

        match self.stack.get(range) {
            Some(actual_types) => actual_types == expected_types,
            None => false
        }
    }

    /// Consume inputs
    pub fn consume_inputs(&mut self, inputs: &[Type]) -> Result<(), DecodeError> {
        let range = self.stack.len() - inputs.len()..self.stack.len();

        let vs = self.stack.get(range).ok_or(CodeValidationError::InvalidStackTop)?;

        if vs != inputs {
            Err(CodeValidationError::InvalidStackTop.into())
        } else {
            self.stack.truncate(self.stack.len() - inputs.len());
            Ok(())
        }
    }
}

impl<'t, 'b> DecodeContext<'t, 'b> {
    /// Decode context initialization function
    /// * `stream` - stream to read data from
    /// * `function_types` - module function types
    /// * `function_type_idx` - indices of functions' function types
    /// * `locals` - local descriptors
    /// * `globals` - global descriptors
    pub fn new(
        stream: &'b mut BinaryInputStream<'t>,
        function_types: &'b [FunctionType],
        function_type_idx: &'b [u32],
        locals: &'b [Type],
        globals: &'b [GlobalDescriptor]
    ) -> Self {
        Self {
            function_types,
            function_type_idx,
            locals,
            globals,
            stream,
            stack: Vec::new(),
        }
    } // fn new

    /// Block decode function
    /// * `inputs` - block input types
    /// * `expected_outputs` - block output types
    /// * Returns result with vector of internal bytecode and trailing WASM opcode
    pub fn decode_block(&mut self, inputs: &[Type], expected_outputs: &[Type]) -> Result<(Vec<u8>, Opcode), DecodeError> {
        // Stack of operand types, needed for validation and further type remove
        let mut output_stream = BinaryOutputStream::new();

        let mut frame = ValidationStackFrame {
            stack: inputs.to_vec(),
            expected_result: expected_outputs.to_vec(),
        };

        macro_rules! pop {
            ($t: ident) => {
                {
                    let v = frame.stack.pop().ok_or(CodeValidationError::NoOperands)?;

                    if v != Type::$t {
                        return Err(CodeValidationError::UnexpectedOperandType {
                            expected: Type::$t,
                            actual: v,
                        }.into());
                    }
                }
            };
            () => {
                {
                    frame.stack.pop().ok_or(CodeValidationError::NoOperands)?;
                }
            };
        }

        macro_rules! push {
            ($t: ident) => {
                frame.stack.push(Type::$t);
            }
        }

        macro_rules! binary_operation {
            ($lhs_t: ident, $rhs_t: ident, $res_t: ident) => {
                {
                    pop!($rhs_t);
                    pop!($lhs_t);
                    push!($res_t);
                }
            }
        }

        macro_rules! unary_operation {
            ($t: ident, $res_t: ident) => {
                {
                    pop!($t);
                    push!($res_t);
                }
            }
        }

        let expression_end = 'block_parsing: loop {
            let opcode_byte = self.stream.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;
            let opcode = Opcode::try_from(opcode_byte).map_err(|_| EnumType::Opcode.as_decode_error(opcode_byte))?;

            let instruction: Option::<instruction::Instruction> = opcode.try_into().ok();

             match opcode {
                // block terminators
                Opcode::ExpressionEnd | Opcode::Else => {
                    break 'block_parsing opcode;
                }
                Opcode::Block => {
                    let ty = self.stream.wasm_decode_block_type()?;

                    let mut io = BlockTypeIODecoder::new();
                    let (inputs, outputs) = io.get(ty, self.function_types).ok_or(CodeValidationError::UnknownFunctionTypeIndex)?;

                    frame.consume_inputs(inputs)?;

                    self.stack.push(frame);
                    let (code, end) = self.decode_block(inputs, outputs)?;
                    frame = self.stack.pop().expect("Block parsing function worked as sh*t");

                    if end != Opcode::ExpressionEnd {
                        return Err(CodeValidationError::UnexpectedExpressionEnd(end as u8).into());
                    }

                    let header = BlockHeader {
                        consume_count: inputs.len() as u16,
                        output_count: outputs.len() as u16,
                        length: code.len() as u32,
                    };

                    output_stream.write(&instruction::Instruction::Block);
                    output_stream.write(&header);
                    output_stream.write_slice(&code);
                }
                Opcode::If => {
                    let ty = self.stream.wasm_decode_block_type()?;

                    let mut io = BlockTypeIODecoder::new();
                    let (inputs, outputs) = io.get(ty, &self.function_types).ok_or(CodeValidationError::UnknownFunctionIndex)?;

                    frame.consume_inputs(inputs)?;

                    self.stack.push(frame);
                    let (then_code, then_end) = self.decode_block(inputs, expected_outputs)?;
                    let (else_code, else_end) = match then_end {
                        Opcode::Else => self.decode_block(inputs, expected_outputs)?,
                        Opcode::ExpressionEnd => (Vec::<u8>::new(), Opcode::ExpressionEnd),
                        _ => return Err(CodeValidationError::UnexpectedExpressionEnd(then_end as u8).into()),
                    };

                    if else_end != Opcode::ExpressionEnd {
                        return Err(CodeValidationError::UnexpectedExpressionEnd(else_end as u8).into());
                    }

                    frame = self.stack.pop().unwrap();

                    let header = BranchHeader {
                        consume_count: inputs.len() as u16,
                        output_count: outputs.len() as u16,
                        then_length: then_code.len() as u32,
                        else_length: else_code.len() as u32,
                    };

                    output_stream.write(&instruction::Instruction::If);
                    output_stream.write(&header);
                    output_stream.write_slice(&then_code);
                    output_stream.write_slice(&else_code);
                }
                Opcode::F32Add | Opcode::F32Sub | Opcode::F32Mul | Opcode::F32Div | Opcode::F32Min | Opcode::F32Max | Opcode::F32CopySign => {
                    binary_operation!(F32, F32, F32);
                    output_stream.write(&instruction.unwrap());
                }
                Opcode::F32Ceil | Opcode::F32Floor | Opcode::F32Trunc | Opcode::F32Abs | Opcode::F32Nearest | Opcode::F32Neg | Opcode::F32Sqrt => {
                    unary_operation!(F32, F32);
                    output_stream.write(&instruction.unwrap());
                }
                Opcode::F32Eq | Opcode::F32Ge | Opcode::F32Gt | Opcode::F32Le | Opcode::F32Lt | Opcode::F32Ne => {
                    binary_operation!(F32, F32, I32);
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::F32Const => {
                    push!(F32);

                    let val = self.stream.get::<f32>()
                        .ok_or(DecodeError::UnexpectedStreamEnd)?;
                    output_stream.write(&Instruction::Const32);
                    output_stream.write(&val);
                }

                Opcode::F32Load => {
                    unary_operation!(I32, F32);

                    let (_align, offset) = (self.stream.wasm_decode_unsigned()? as u32, self.stream.wasm_decode_unsigned()? as u32);

                    output_stream.write(&instruction.unwrap());
                    output_stream.write(&offset);
                }
                Opcode::F32Store => {
                    pop!(F32);
                    pop!(I32);

                    let (_align, offset) = (self.stream.wasm_decode_unsigned()? as u32, self.stream.wasm_decode_unsigned()? as u32);

                    output_stream.write(&instruction.unwrap());
                    output_stream.write(&offset);
                }
                Opcode::F32DemoteF64 => {
                    unary_operation!(F64, F32);
                    output_stream.write(&instruction.unwrap());
                }
                Opcode::F32ReinterpretI32 => {
                    unary_operation!(I32, F32);
                }
                Opcode::F32ConvertI32S | Opcode::F32ConvertI32U => {
                    unary_operation!(I32, F32);
                    output_stream.write(&instruction.unwrap());
                }
                Opcode::F32ConvertI64S | Opcode::F32ConvertI64U => {
                    unary_operation!(I64, F32);
                    output_stream.write(&instruction.unwrap());
                }
                Opcode::F64Add | Opcode::F64Sub | Opcode::F64Mul | Opcode::F64Div | Opcode::F64Min | Opcode::F64Max | Opcode::F64CopySign => {
                    binary_operation!(F64, F64, F64);
                    output_stream.write(&instruction.unwrap());
                }
                Opcode::F64Ceil | Opcode::F64Floor | Opcode::F64Trunc | Opcode::F64Abs | Opcode::F64Nearest | Opcode::F64Neg | Opcode::F64Sqrt => {
                    unary_operation!(F64, F64);
                    output_stream.write(&instruction.unwrap());
                }
                Opcode::F64Eq | Opcode::F64Ge | Opcode::F64Gt | Opcode::F64Le | Opcode::F64Lt | Opcode::F64Ne => {
                    binary_operation!(F64, F64, I32);
                    output_stream.write(&instruction.unwrap());
                }
                Opcode::F64Const => {
                    push!(F64);

                    let val = self.stream.get::<f64>().ok_or(DecodeError::UnexpectedStreamEnd)?;
                    output_stream.write(&Instruction::Const64);
                    output_stream.write(&val);
                }
                Opcode::F64Load => {
                    unary_operation!(I32, F64);

                    let (_align, offset) = (self.stream.wasm_decode_unsigned()? as u32, self.stream.wasm_decode_unsigned()? as u32);

                    output_stream.write(&instruction.unwrap());
                    output_stream.write(&offset);
                }
                Opcode::F64Store => {
                    pop!(F64);
                    pop!(I32);

                    let (_align, offset) = (self.stream.wasm_decode_unsigned()? as u32, self.stream.wasm_decode_unsigned()? as u32);

                    output_stream.write(&instruction.unwrap());
                    output_stream.write(&offset);
                }
                Opcode::F64PromoteF32 => {
                    unary_operation!(F32, F64);
                    output_stream.write(&instruction.unwrap());
                }
                Opcode::F64ReinterpretI64 => {
                    unary_operation!(I64, F64);
                }
                Opcode::F64ConvertI64S | Opcode::F64ConvertI64U => {
                    unary_operation!(I64, F64);
                    output_stream.write(&instruction.unwrap());
                }
                Opcode::F64ConvertI32S | Opcode::F64ConvertI32U => {
                    unary_operation!(I32, F64);
                    output_stream.write(&instruction.unwrap());
                }

                /* I32 Code */
                Opcode::I32Add | Opcode::I32Sub | Opcode::I32Mul | Opcode::I32DivS | Opcode::I32DivU |
                Opcode::I32And | Opcode::I32Or | Opcode::I32Xor | Opcode::I32RemS | Opcode::I32RemU |
                Opcode::I32Shl | Opcode::I32ShrS | Opcode::I32ShrU | Opcode::I32Rotl | Opcode::I32Rotr => {
                    binary_operation!(I32, I32, I32);
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::I32Clz | Opcode::I32Ctz | Opcode::I32Popcnt => {
                    unary_operation!(I32, I32);
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::I32Const => {
                    push!(I32);

                    let n = self.stream.wasm_decode_unsigned()? as u32;

                    output_stream.write(&instruction::Instruction::Const32);
                    output_stream.write(&n);
                }

                Opcode::I32GeS | Opcode::I32GeU | Opcode::I32LeS | Opcode::I32LeU |
                Opcode::I32GtS | Opcode::I32GtU | Opcode::I32LtS | Opcode::I32LtU |
                Opcode::I32Eq  | Opcode::I32Ne => {
                    binary_operation!(I32, I32, I32);
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::I32Eqz => {
                    unary_operation!(I32, I32);
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::I32Extend8S | Opcode::I32Extend16S => {
                    unary_operation!(I32, I32);
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::I32Load | Opcode::I32Load8S | Opcode::I32Load8U | Opcode::I32Load16S | Opcode::I32Load16U => {
                    let (_align, offset) = (self.stream.wasm_decode_unsigned()? as u32, self.stream.wasm_decode_unsigned()? as u32);

                    unary_operation!(I32, I32);

                    output_stream.write(&instruction.unwrap());
                    output_stream.write(&offset);
                }

                Opcode::I32Store | Opcode::I32Store8 | Opcode::I32Store16 => {
                    let (_align, offset) = (self.stream.wasm_decode_unsigned()? as u32, self.stream.wasm_decode_unsigned()? as u32);

                    pop!(I32);
                    pop!(I32);

                    output_stream.write(&instruction.unwrap());
                    output_stream.write(&offset);
                }

                Opcode::I32ReinterpretF32 => {
                    unary_operation!(F32, I32);
                }

                Opcode::I32TruncF32S | Opcode::I32TruncF32U => {
                    unary_operation!(F32, I32);
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::I32TruncF64S | Opcode::I32TruncF64U => {
                    unary_operation!(F64, I32);
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::I32WrapI64 => {
                    // Do wrap operation really required?
                    unary_operation!(I64, I32);
                    output_stream.write(&instruction.unwrap());
                }

                /* I64 Code */

                Opcode::I64Add | Opcode::I64Sub | Opcode::I64Mul | Opcode::I64DivS | Opcode::I64DivU |
                Opcode::I64And | Opcode::I64Or | Opcode::I64Xor | Opcode::I64RemS | Opcode::I64RemU |
                Opcode::I64Shl | Opcode::I64ShrS | Opcode::I64ShrU | Opcode::I64Rotl | Opcode::I64Rotr => {
                    binary_operation!(I64, I64, I64);
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::I64Clz | Opcode::I64Ctz | Opcode::I64Popcnt => {
                    unary_operation!(I64, I64);
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::I64Const => {
                    push!(I64);

                    let n = self.stream.wasm_decode_unsigned()? as u64;

                    output_stream.write(&Instruction::Const64);
                    output_stream.write(&n);
                }

                Opcode::I64GeS | Opcode::I64GeU | Opcode::I64LeS | Opcode::I64LeU |
                Opcode::I64GtS | Opcode::I64GtU | Opcode::I64LtS | Opcode::I64LtU |
                Opcode::I64Eq  | Opcode::I64Ne => {
                    binary_operation!(I64, I64, I32);
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::I64Eqz => {
                    unary_operation!(I64, I32);
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::I64Extend8S | Opcode::I64Extend16S | Opcode::I64Extend32S => {
                    unary_operation!(I64, I64);
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::I64ExtendI32S | Opcode::I64ExtendI32U => {
                    unary_operation!(I32, I64);
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::I64Load | Opcode::I64Load8S | Opcode::I64Load8U | Opcode::I64Load16S | Opcode::I64Load16U | Opcode::I64Load32S | Opcode::I64Load32U => {
                    let (_align, offset) = (self.stream.wasm_decode_unsigned()? as u32, self.stream.wasm_decode_unsigned()? as u32);

                    unary_operation!(I32, I64);

                    output_stream.write(&instruction.unwrap());
                    output_stream.write(&offset);
                }

                Opcode::I64Store | Opcode::I64Store8 | Opcode::I64Store16 | Opcode::I64Store32 => {
                    let (_align, offset) = (self.stream.wasm_decode_unsigned()? as u32, self.stream.wasm_decode_unsigned()? as u32);

                    pop!(I64);
                    pop!(I32);

                    output_stream.write(&instruction.unwrap());
                    output_stream.write(&offset);
                }

                Opcode::I64ReinterpretF64 => {
                    unary_operation!(F64, I64);
                }

                Opcode::I64TruncF32S | Opcode::I64TruncF32U => {
                    unary_operation!(F32, I64);
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::I64TruncF64S | Opcode::I64TruncF64U => {
                    unary_operation!(F64, I64);
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::Br | Opcode::BrIf | Opcode::BrTable => {
                    if opcode == Opcode::Br || opcode == Opcode::BrTable {
                        pop!(I32);
                    }

                    output_stream.write(&instruction.unwrap());

                    let depth_iter: &mut dyn Iterator<Item = Result<usize, DecodeError>> = if opcode == Opcode::Br || opcode == Opcode::BrIf {
                        &mut std::iter::once(self.stream.wasm_decode_unsigned())
                    } else {
                        let count = self.stream.wasm_decode_unsigned()? + 1;

                        output_stream.write(&(count as u32));

                        &mut (0..count).map(|_| self.stream.wasm_decode_unsigned())
                    };

                    for depth_opt in depth_iter {
                        let depth = depth_opt?;

                        // check if branching from this frame or not
                        let branch_frame = if depth == 0 {
                            &frame
                        } else {
                            self.stack.get(self.stack.len() - depth).ok_or(CodeValidationError::InvalidBranchDepth)?
                        };

                        if !branch_frame.check_top(&branch_frame.expected_result) {
                            return Err(CodeValidationError::InvalidStackTop.into());
                        }

                        output_stream.write(&(depth as u32));
                    }
                }

                Opcode::Call | Opcode::CallIndirect => {
                    output_stream.write(&instruction.unwrap());

                    let type_id = if opcode == Opcode::CallIndirect {
                        pop!(I32);
                        let i = self.stream.wasm_decode_unsigned()?;
                        output_stream.write(&(i as u32));

                        i
                    } else {
                        let func_id = self.stream.wasm_decode_unsigned()?;

                        output_stream.write(&(func_id as u32));

                        self.function_type_idx
                            .get(func_id)
                            .map(|v| *v as usize)
                            .ok_or(CodeValidationError::UnknownFunctionIndex)?
                    };

                    let ty = self.function_types.get(type_id).ok_or(CodeValidationError::UnknownFunctionTypeIndex)?;
                    frame.consume_inputs(&ty.inputs)?;
                    frame.stack.extend_from_slice(&ty.outputs);
                }
                Opcode::Drop => {
                    output_stream.write(&instruction.unwrap());
                    pop!()
                }
                Opcode::GlobalGet => {
                    let global_index = self.stream.wasm_decode_unsigned()?;
                    let desc = self.globals.get(global_index).ok_or(CodeValidationError::UnknownGlobalIndex)?;
                    frame.stack.push(desc.value_type);

                    output_stream.write(&instruction.unwrap());
                    output_stream.write(&(global_index as u32));
                }

                Opcode::GlobalSet => {
                    let global_index = self.stream.wasm_decode_unsigned()?;
                    let desc = self.globals.get(global_index).ok_or(CodeValidationError::UnknownGlobalIndex)?;
                    let expected = frame.stack.pop().ok_or(CodeValidationError::NoOperands)?;

                    if desc.mutability != Mutability::Mut {
                        return Err(CodeValidationError::MutatingConstantGlobal.into());
                    }

                    if desc.value_type != expected {
                        return Err(CodeValidationError::UnexpectedOperandType { expected, actual: desc.value_type }.into());
                    }

                    output_stream.write(&instruction.unwrap());
                    output_stream.write(&(global_index as u32));
                }

                Opcode::LocalGet => {
                    let local_index = self.stream.wasm_decode_unsigned()?;
                    let ty = self.locals.get(local_index).ok_or(CodeValidationError::UnknownLocalIndex)?;
                    frame.stack.push(*ty);

                    output_stream.write(&instruction::Instruction::LocalGet);
                    output_stream.write(&(local_index as u32));
                }

                Opcode::LocalSet => {
                    let local_index = self.stream.wasm_decode_unsigned()?;
                    let actual = *self.locals.get(local_index).ok_or(CodeValidationError::UnknownLocalIndex)?;
                    let expected = frame.stack.pop().ok_or(CodeValidationError::NoOperands)?;

                    if actual != expected {
                        return Err(CodeValidationError::UnexpectedOperandType { expected, actual }.into());
                    }

                    output_stream.write(&instruction::Instruction::LocalSet);
                    output_stream.write(&(local_index as u32));
                }

                Opcode::LocalTee => {
                    let local_index = self.stream.wasm_decode_unsigned()?;
                    let ty = *self.locals.get(local_index).ok_or(CodeValidationError::UnknownGlobalIndex)?;
                    let expected = frame.stack.pop().ok_or(CodeValidationError::NoOperands)?;

                    if ty != expected {
                        return Err(CodeValidationError::UnexpectedOperandType { expected, actual: ty }.into());
                    }

                    frame.stack.push(expected);
                    output_stream.write(&instruction.unwrap());
                    output_stream.write(&(local_index as u32));
                }

                Opcode::Nop => {
                    output_stream.write(&instruction.unwrap());
                }

                Opcode::RefNull => {
                    let ref_type = self.stream.wasm_decode_reference_type()?;

                    frame.stack.push(ref_type.into());

                    output_stream.write(&instruction::Instruction::Const32);
                    output_stream.write(&0u32);
                }

                Opcode::RefIsNull => {
                    let top = frame.stack.pop().ok_or(CodeValidationError::NoOperands)?;

                    if top != Type::FuncRef || top != Type::ExternRef {
                        return Err(CodeValidationError::InvalidStackTop.into());
                    }

                    output_stream.write(&instruction::Instruction::I32Eqz);
                }

                Opcode::Select | Opcode::SelectTyped => {
                    let required_type = if opcode == Opcode::SelectTyped {
                        Some(self.stream
                            .wasm_decode_vector(&|b: &u8| decode_type(*b))
                            ?
                            .get(0)
                            .copied()
                            .ok_or(CodeValidationError::NoTypesInTypedSelect)?
                        )
                    } else {
                        None
                    };

                    // Pop index
                    pop!(I32);

                    let first = frame.stack.pop().ok_or(CodeValidationError::NoOperands)?;
                    let second = frame.stack.pop().ok_or(CodeValidationError::NoOperands)?;

                    if let Some(ty) = required_type {
                        if first != ty {
                            return Err(CodeValidationError::UnexpectedOperandType {
                                actual: first,
                                expected: ty,
                            }.into());
                        }
                    }

                    if first != second {
                        return Err(CodeValidationError::UnexpectedOperandType {
                            actual: second,
                            expected: first,
                        }.into());
                    }

                    // insert value
                    frame.stack.push(first);

                    output_stream.write(&instruction::Instruction::Select);
                }

                Opcode::Return => {
                    output_stream.write(&instruction::Instruction::Return);
                }

                Opcode::Unreachable => {
                    output_stream.write(&instruction::Instruction::Unreachable);
                }

                Opcode::RefFunc => {
                    let func_index = self.stream.wasm_decode_unsigned()?;

                    push!(FuncRef);

                    output_stream.write(&instruction::Instruction::Const32);
                    output_stream.write(&(func_index as u32));
                }

                Opcode::System => {
                    todo!("Implement system instruction set")
                }

                Opcode::Vector => {
                    todo!("Implement vector instruction set")
                }

                _ => {
                    todo!("Unimplemented opcode: {:?}", opcode);
                }
            }
        };

        if frame.stack == expected_outputs {
            Ok((output_stream.finish(), expression_end))
        } else {
            Err(CodeValidationError::InvalidStackTop.into())
        }
    } // fn decode_block

    /// Decode function
    /// * `inputs` - set of stack inputs
    /// * `outputs` - set of expected stack outputs
    /// * Returns result with vector of internal bytecode
    pub fn decode(mut self, inputs: &[Type], expected_outputs: &[Type]) -> Result<Vec<u8>, DecodeError> {
        let (code, ending_instruction) = self.decode_block(inputs, expected_outputs)?;

        if ending_instruction == Opcode::ExpressionEnd {
            Ok(code)
        } else {
            Err(DecodeError::InvalidBlockEnd)
        }
    } // fn decode
} // impl DecodeContext

/// Validated block decode function
/// * `stream` - stream to read
/// * `function_types` - types of functions in context
/// * `function_type_idx` - indices of function types of functions
/// * `local_types` - types of local variables
/// * `globals` - global variables
/// * `initial_values` - set of values initially moved to stack
/// * `expected_result` - expected stack at block execution end
/// * Returns vector of decoded bytecode or decode error
pub(super) fn decode_block<'t, 'b>(
    stream: &'b mut BinaryInputStream<'t>,
    function_types: &'b [FunctionType],
    function_type_idx: &'b [u32],
    local_types: &'b [Type],
    globals: &'b [GlobalDescriptor],
    initial_values: &'b [Type],
    expected_result: &'b [Type],
) -> Result<Vec<u8>, DecodeError> 
where 't: 'b {
    DecodeContext::new(stream, function_types, function_type_idx, local_types, globals)
        .decode(initial_values, expected_result)
} // fn decode_block

// file block_decode.rs
