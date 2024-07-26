use crate::{instruction::{self, BlockType}, module::{opcode, wasm::{CodeValidationError, EnumType}}, types::FunctionType, util::binary_stream::{BinaryInputStream, BinaryOutputStream}, ReferenceType, ValueType};

use super::DecodeError;

/// Input/Output ftype blocktype decoder
struct BlockTypeIODecoder {
    out_binding: [ValueType; 1],
}

impl BlockTypeIODecoder {
    pub fn new() -> Self {
        Self {
            out_binding: [ValueType::Number(crate::NumberType::F32)]
        }
    }

    pub fn get<'t>(&'t mut self, ty: BlockType, func_types: &'t [FunctionType]) -> Option<(&'t [ValueType], &'t [ValueType])> {
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
    }
}


struct ValidationStackFrame {
    pub expected_result: Vec<ValueType>,
    pub stack: Vec<ValueType>,
}

struct BlockDecoder<'t> {
    stream: &'t mut BinaryInputStream<'t>,
    output: BinaryOutputStream,
    function_types: &'t [FunctionType],
    locals: &'t [ValueType],
    stack: Vec<ValidationStackFrame>,
}

impl<'t> BlockDecoder<'t> {
    pub fn new(stream: &'t mut BinaryInputStream<'t>, function_types: &'t [FunctionType], locals: &'t [ValueType]) -> Self {
        Self {
            function_types,
            output: BinaryOutputStream::new(),
            locals,
            stream,
            stack: Vec::new(),
        }
    }

    pub fn decode_block(&mut self, inputs: &[ValueType], expected_outputs: &[ValueType]) -> Result<Vec<u8>, DecodeError> {
        // Stack of operand types, needed for validation and further type remove
        let mut output_stream = BinaryOutputStream::new();

        type Opcode = super::opcode::Main;

        impl ValidationStackFrame {
            pub fn consume_inputs(&mut self, inputs: &[ValueType]) -> Result<(), DecodeError> {
                let range = self.stack.len() - inputs.len()..self.stack.len();

                let vs = self.stack.get(range).ok_or(CodeValidationError::InvalidStackTop.into())?;

                if vs != inputs {
                    Err(CodeValidationError::InvalidStackTop.into())
                } else {
                    self.stack.truncate(inputs.len());
                    Ok(())
                }
            }
        }

        let mut block_stack = Vec::<ValidationStackFrame>::new();
        let mut frame = ValidationStackFrame {
            stack: inputs.to_vec(),
            expected_result: expected_outputs.to_vec(),
        };

        loop {
            let opcode_byte = self.stream.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;
            let opcode = opcode::Main::try_from(opcode_byte).map_err(|_| EnumType::Opcode.as_decode_error(opcode_byte))?;

            let instruction: Option::<instruction::Instruction> = opcode.try_into().ok();

            match opcode {
                opcode::Main::Block => {
                    let ty = self.stream.wasm_decode_block_type()?;

                    let mut io = BlockTypeIODecoder::new();
                    let (inputs, outputs) = io.get(ty, self.function_types).ok_or(CodeValidationError::UnknownFunctionTypeIndex.into())?;

                    frame.consume_inputs(inputs)?;
                    frame.stack.truncate(frame.stack.len() - inputs.len());

                    block_stack.push(frame);

                    frame = ValidationStackFrame {
                        expected_result: outputs.to_vec(),
                        stack: inputs.to_vec(),
                    };

                }
                opcode::Main::ExpressionEnd => {

                }
                _ => {},
            }
        }

        todo!()
    }

    pub fn decode(mut self, inputs: &[ValueType], expected_outputs: &[ValueType]) -> Result<Vec<u8>, DecodeError> {
        self.decode_block(inputs, expected_outputs)?;

        Ok(self.output.finish())
    }
}

/// Validated block decode function
pub(super) fn decode_block_validated<'t>(stream: &'t mut BinaryInputStream<'t>, function_types: &'t [FunctionType], local_types: &'t [ValueType], initial_values: &'t [ValueType], expected_result: &[ValueType]) -> Result<Vec<u8>, DecodeError> {
    BlockDecoder::new(stream, function_types, local_types).decode(initial_values, expected_result)
}

pub(super) fn decode_block(stream: &mut BinaryInputStream) -> Result<(Vec<u8>, u8), DecodeError> {
    let mut instruction_stream = BinaryOutputStream::new();

    let ending_byte = 'block_parsing_loop: loop {
        let opcode_u = stream.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;
        let opcode = opcode::Main::try_from(opcode_u).map_err(|_| EnumType::Opcode.as_decode_error(opcode_u))?;

        let instruction: Option<instruction::Instruction> = opcode.try_into().ok();

        type Opcode = opcode::Main;

        match opcode {
            Opcode::Else | Opcode::ExpressionEnd => {
                break 'block_parsing_loop opcode_u;
            }
            // Decode block
            Opcode::Block | Opcode::Loop | Opcode::If => {
                // Requires block parsing

                let ty = stream.wasm_decode_block_type()?;
                let (block_bits, ending_byte) = decode_block(stream)?;

                instruction_stream.write(&instruction.unwrap());

                // Write conditional
                instruction_stream.write(&instruction::BlockHeader {
                    length: block_bits.len() as u32,
                    ty,
                });
                instruction_stream.write_slice(&block_bits);

                if opcode == Opcode::If && ending_byte == Opcode::Else as u8 {
                    let (else_bits, ending_byte) = decode_block(stream)?;
                    if ending_byte != Opcode::ExpressionEnd as u8 {
                        return Err(CodeValidationError::UnexpectedExpressionEnd(ending_byte).into());
                    }

                    instruction_stream.write(&instruction::Instruction::Else);
                    instruction_stream.write(&instruction::BlockHeader {
                        length: else_bits.len() as u32,
                        ty,
                    });
                    instruction_stream.write_slice(&else_bits);
                }
            }
            Opcode::BrTable => {
                // Parse branch table
                instruction_stream.write(&instruction::Instruction::BrTable);

                let label_count = stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)? as u32;

                // Write label count
                instruction_stream.write(&label_count);
                // Write labels and default
                for _ in 0..(label_count + 1) {
                    instruction_stream.write(&(stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)? as u32));
                }
            }
            Opcode::CallIndirect => {
                let ty = stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)? as u32;
                let table_index = stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)? as u32;

                instruction_stream.write(&instruction::Instruction::CallIndirect);
                instruction_stream.write(&ty);
                instruction_stream.write(&table_index);
            }
            Opcode::I64Const => {
                instruction_stream.write(&instruction::Instruction::I64Const);
                instruction_stream.write(&(stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)? as u64))
            }
            Opcode::F32Const => {
                instruction_stream.write(&instruction::Instruction::F32Const);
                instruction_stream.write(&stream.get::<f32>().ok_or(DecodeError::UnexpectedStreamEnd)?)
            }
            Opcode::F64Const => {
                instruction_stream.write(&instruction::Instruction::F64Const);
                instruction_stream.write(&stream.get::<f64>().ok_or(DecodeError::UnexpectedStreamEnd)?)
            }
            Opcode::I32Load | Opcode::I64Load | Opcode::F32Load | Opcode::F64Load | Opcode::I32Load8S | Opcode::I32Load8U | Opcode::I32Load16S | Opcode::I32Load16U |
            Opcode::I64Load16S | Opcode::I64Load16U | Opcode::I64Load8S | Opcode::I64Load8U | Opcode::I64Load32S | Opcode::I64Load32U | Opcode::I32Store | Opcode::I64Store |
            Opcode::F32Store | Opcode::F64Store | Opcode::I32Store8 | Opcode::I32Store16 | Opcode::I64Store8 | Opcode::I64Store16 | Opcode::I64Store32 => {
                let _align = stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)? as u32;
                let offset = stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)? as u32;

                instruction_stream.write(&instruction.unwrap());
                instruction_stream.write(&offset);
            }
            Opcode::MemorySize | Opcode::MemoryGrow => {
                if stream.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)? != 0 {
                    return Err(CodeValidationError::InvalidMemoryInstructionData.into());
                }
            }
            Opcode::RefFunc | Opcode::Br | Opcode::BrIf | Opcode::Call | Opcode::LocalGet | Opcode::LocalSet | Opcode::LocalTee |
            Opcode::GlobalGet | Opcode::GlobalSet | Opcode::TableGet | Opcode::TableSet | Opcode::I32Const => {
                instruction_stream.write(&instruction.unwrap());
                instruction_stream.write(&(stream.decode_unsigned().ok_or(DecodeError::UnsignedDecodeError)? as u32));
            }
            Opcode::RefNull => {
                let byte = stream.get::<u8>().ok_or(DecodeError::UnexpectedStreamEnd)?;
                instruction_stream.write(&instruction::Instruction::RefNull);
                instruction_stream.write(&(ReferenceType::try_from(byte).map_err(|_| EnumType::ReferenceType.as_decode_error(byte))? as u8));
            }
            // Vector and system instrucitons
            Opcode::Vector | Opcode::System => return Err(DecodeError::UnsupportedFeature),
            _ => {
                instruction_stream.write(&instruction.unwrap())
            }
        }
    };

    Ok((
        instruction_stream.finish(),
        ending_byte
    ))
}

