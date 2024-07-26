use std::collections::HashMap;
use crate::{instruction::{self, BlockType}, types::{self, FunctionType}, util::binary_stream::{BinaryInputStream, BinaryOutputStream}, Function, ValueType};
use super::{opcode, Expression, GlobalDescriptor, ModuleImpl, WASMDecodeError};

pub struct InputsOutputs {
    out_binding: [ValueType; 1],
}

impl InputsOutputs {
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

// Own binary stream implementation
impl<'t> BinaryInputStream<'t> {
    pub fn wasm_decode_vector<DT: bytemuck::AnyBitPattern, T>(&mut self, decode_func: &dyn Fn(&DT) -> Option<T>) -> Option<Vec<T>> {
        let len = self.decode_unsigned()?;
        self.get_slice::<DT>(len)?.iter().map(decode_func).collect()
    }

    pub fn wasm_decode_limits(&mut self) -> Result<types::Limits, WASMDecodeError> {
        let t = self.get::<u8>().ok_or(WASMDecodeError::UnexpectedStreamEnd)?;
        match t {
            0 => Ok(types::Limits { min: self.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)? as u32, max: None }),
            1 => Ok(types::Limits { min: self.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)? as u32, max: Some(self.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)? as u32) }),
            _ => Err(WASMDecodeError::UnknownLimitType(t)),
        }
    }

    pub fn wasm_decode_block_type(&mut self) -> Result<instruction::BlockType, WASMDecodeError> {
        let byte = self.check::<u8>().ok_or(WASMDecodeError::UnexpectedStreamEnd)?;

        Ok(if byte == 0x40 {
            self.skip(1).unwrap();
            instruction::BlockType::Void
        } else if let Ok(val_type) = types::ValueType::try_from(byte) {
            self.skip(1).unwrap();
            instruction::BlockType::ResolvingTo(val_type)
        } else {
            instruction::BlockType::Functional(self.decode_signed::<32>().ok_or(WASMDecodeError::SignedDecodeError { bit_count: 32 })? as u32)
        })
    }
}

fn decode_sections<'t>(bits: &'t [u8]) -> Result<HashMap<types::SectionID, &'t [u8]>, WASMDecodeError> {
    let mut stream = BinaryInputStream::new(bits);

    if !stream.get::<types::Header>().ok_or(WASMDecodeError::UnexpectedStreamEnd)?.validate() {
        return Err(WASMDecodeError::UnexpectedStreamEnd);
    }

    let mut sections = HashMap::<types::SectionID, &[u8]>::new();

    while let Some(section_id) = stream.get::<u8>() {
        let section_id = types::SectionID::try_from(section_id).map_err(|_| WASMDecodeError::UnknownSectionId(section_id))?;
        let length = stream.decode_unsigned().ok_or(WASMDecodeError::UnexpectedStreamEnd)?;
        let data = stream.get_byte_slice(length).ok_or(WASMDecodeError::UnexpectedStreamEnd)?;
        sections.insert(section_id, data);
    }

    Ok(sections)
}

fn decode_function_type_section(section: &[u8]) -> Result<Vec<types::FunctionType>, WASMDecodeError> {
    let mut stream = BinaryInputStream::new(section);

    let type_count = stream.decode_unsigned().ok_or(WASMDecodeError::UnexpectedStreamEnd)?;

    (0..type_count).map(|_| {
            let functype_magic = stream.get::<u8>().ok_or(WASMDecodeError::UnexpectedStreamEnd)?;

            if functype_magic != types::WASM_FUNCTYPE_MAGIC {
                return Err(WASMDecodeError::InvalidModuleMagic);
            }

            const DECODE_FUNC: &'static dyn Fn(&u8) -> Option<types::ValueType> = &|v: &u8| types::ValueType::try_from(*v).ok();

            Ok(types::FunctionType {
                inputs: stream.wasm_decode_vector(DECODE_FUNC).ok_or(WASMDecodeError::UnexpectedStreamEnd)?,
                outputs: stream.wasm_decode_vector(DECODE_FUNC).ok_or(WASMDecodeError::UnexpectedStreamEnd)?,
            })
        })
        .collect::<Result<Vec<types::FunctionType>, WASMDecodeError>>()
}

fn decode_function_section(section: &[u8]) -> Result<Vec<u32>, WASMDecodeError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(WASMDecodeError::UnexpectedStreamEnd)?)
        .map(|_| stream.decode_unsigned().map(|v| v as u32).ok_or(WASMDecodeError::UnexpectedStreamEnd))
        .collect::<Result<Vec<u32>, WASMDecodeError>>()
} // fn decode_function_section

/// Table section decode function
/// * `section` - binary section contents
/// * Returns vector of table types or module create error
fn decode_table_section(section: &[u8]) -> Result<Vec<types::TableType>, WASMDecodeError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(WASMDecodeError::UnexpectedStreamEnd)?)
        .map(|_| {
            let type_byte = stream.get::<u8>().ok_or(WASMDecodeError::UnexpectedStreamEnd)?;

            Ok(types::TableType {
                reference_type: TryInto::<types::ReferenceType>::try_into(type_byte).map_err(|_| WASMDecodeError::UnknownReferenceType(type_byte))?,
                limits: stream.wasm_decode_limits()?,
            })
        })
        .collect::<Result<Vec<types::TableType>, WASMDecodeError>>()
} // fn decode_table_section

/// Memory section decode function
/// * `section` - binary section contents
/// * Returns vector of memory size limits or module create error
fn decode_memory_section(section: &[u8]) -> Result<Vec<types::Limits>, WASMDecodeError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(WASMDecodeError::UnexpectedStreamEnd)?)
        .map(|_| stream.wasm_decode_limits())
        .collect::<Result<Vec<types::Limits>, WASMDecodeError>>()
} // fn decode_memory_section

/// Export section decode function
/// * `section` - binary section contents
/// * Returns hashmap of export object descriptors by exported names or module create error
fn decode_export_section(section: &[u8]) -> Result<HashMap<String, types::ExportDescriptor>, WASMDecodeError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)?)
        .map(|_| -> Result<(String, types::ExportDescriptor), WASMDecodeError> {
            let name = {
                let len = stream.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)?;
                String::from_utf8(stream.get_byte_slice(len).ok_or(WASMDecodeError::UnsignedDecodeError)?.to_vec()).map_err(|_| WASMDecodeError::Utf8DecodeError)?
            };
            let type_byte = stream.get::<u8>().ok_or(WASMDecodeError::UnexpectedStreamEnd)?;
            let desc = types::ExportDescriptor {
                ty: type_byte.try_into().map_err(|_| WASMDecodeError::UnknownExportType(type_byte))?,
                index: stream.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)? as u32,
            };

            Ok((name, desc))
        })
        .collect::<Result<HashMap<String, types::ExportDescriptor>, WASMDecodeError>>()
} // fn decode_export_section

/// Start section decode function
/// * `section` - binary section contents
/// * Returns index of start section decoding or module create error
fn decode_start_section(section: &[u8]) -> Result<u32, WASMDecodeError> {
    BinaryInputStream::new(section)
        .decode_unsigned()
        .map(|v| v as u32)
        .ok_or(WASMDecodeError::UnsignedDecodeError)
} // fn decode_start_section

fn decode_code_section(section: &[u8]) -> Result<Vec<&[u8]>, WASMDecodeError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)?)
        .map(|_| -> Result<&[u8], WASMDecodeError> {
            let length = stream.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)?;

            Ok(stream.get_byte_slice(length).ok_or(WASMDecodeError::UnexpectedStreamEnd)?)
        })
        .collect::<Result<Vec<&[u8]>, WASMDecodeError>>()
}

fn decode_global_section(section: &[u8]) -> Result<Vec<GlobalDescriptor>, WASMDecodeError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)?)
        .map(|_| -> Result<GlobalDescriptor, WASMDecodeError> {
            let type_byte = stream.get::<u8>().ok_or(WASMDecodeError::UnexpectedStreamEnd)?;
            let mut_byte = stream.get::<u8>().ok_or(WASMDecodeError::UnexpectedStreamEnd)?;

            Ok(GlobalDescriptor {
                value_type: types::ValueType::try_from(type_byte).map_err(|_| WASMDecodeError::UnknownValueType(type_byte))?,
                mutability: types::Mutability::try_from(mut_byte).map_err(|_| WASMDecodeError::UnknownMutability(mut_byte))?,
                expression: decode_expression(&mut stream)?,
            })
        })
        .collect::<Result<Vec<GlobalDescriptor>, WASMDecodeError>>()
}

fn decode_expression(stream: &mut BinaryInputStream) -> Result<Expression, WASMDecodeError> {
    let (instructions, end) = decode_block(stream)?;

    if end != opcode::Main::ExpressionEnd as u8 {
        return Err(WASMDecodeError::ValidationError);
    }

    return Ok(Expression { instructions })
}

/// Validated block decode function
fn decode_block_validated(stream: &mut BinaryInputStream, function_types: &[FunctionType], local_types: &[ValueType], initial_values: &[ValueType], expected_result: &[ValueType]) -> Result<(Vec<u8>, u8), WASMDecodeError> {
    // Stack of operand types, needed for validation and further type remove
    let mut output_stream = BinaryOutputStream::new();

    type Opcode = opcode::Main;

    struct ValidationStackFrame {
        pub expected_result: Vec<ValueType>,
        pub stack: Vec<ValueType>,
    }

    impl ValidationStackFrame {
        pub fn check_top(&self, types: &[ValueType]) -> bool {
            let range = self.stack.len() - types.len()..self.stack.len();

            self.stack
                .get(range)
                .map(|v| v == types)
                .unwrap_or(false)
        }
    }

    let mut block_stack = Vec::<ValidationStackFrame>::new();
    let mut frame = ValidationStackFrame {
        expected_result: expected_result.to_vec(),
        stack: initial_values.to_vec(),
    };

    'global_validation: loop {
        let opcode_byte = stream.get::<u8>().ok_or(WASMDecodeError::UnexpectedStreamEnd)?;
        let opcode = opcode::Main::try_from(opcode_byte).map_err(|_| WASMDecodeError::UnknownOpcode(opcode_byte))?;

        let instruction: Option::<instruction::Instruction> = opcode.try_into().ok();

        match opcode {
            opcode::Main::Block => {
                let ty = stream.wasm_decode_block_type()?;

                let mut io = InputsOutputs::new();
                let (inputs, outputs) = io.get(ty, function_types).ok_or(WASMDecodeError::ValidationError)?;

                if !frame.check_top(inputs) {
                    return Err(WASMDecodeError::ValidationError);
                }
                frame.stack.truncate(frame.stack.len() - inputs.len());

                let mut new_frame = ValidationStackFrame {
                    expected_result: outputs.to_vec(),
                    stack: inputs.to_vec(),
                };

            }
            _ => {},
        }
    }

    todo!()
}

fn decode_block(stream: &mut BinaryInputStream) -> Result<(Vec<u8>, u8), WASMDecodeError> {
    let mut instruction_stream = BinaryOutputStream::new();

    let ending_byte = 'block_parsing_loop: loop {
        let opcode_u = stream.get::<u8>().ok_or(WASMDecodeError::UnexpectedStreamEnd)?;
        let opcode = opcode::Main::try_from(opcode_u).map_err(|_| WASMDecodeError::UnknownOpcode(opcode_u))?;

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
                        return Err(WASMDecodeError::ValidationError);
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

                let label_count = stream.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)? as u32;

                // Write label count
                instruction_stream.write(&label_count);
                // Write labels and default
                for _ in 0..(label_count + 1) {
                    instruction_stream.write(&(stream.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)? as u32));
                }
            }
            Opcode::CallIndirect => {
                let ty = stream.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)? as u32;
                let table_index = stream.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)? as u32;

                instruction_stream.write(&instruction::Instruction::CallIndirect);
                instruction_stream.write(&ty);
                instruction_stream.write(&table_index);
            }
            Opcode::I64Const => {
                instruction_stream.write(&instruction::Instruction::I64Const);
                instruction_stream.write(&(stream.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)? as u64))
            }
            Opcode::F32Const => {
                instruction_stream.write(&instruction::Instruction::F32Const);
                instruction_stream.write(&stream.get::<f32>().ok_or(WASMDecodeError::UnexpectedStreamEnd)?)
            }
            Opcode::F64Const => {
                instruction_stream.write(&instruction::Instruction::F64Const);
                instruction_stream.write(&stream.get::<f64>().ok_or(WASMDecodeError::UnexpectedStreamEnd)?)
            }
            Opcode::I32Load | Opcode::I64Load | Opcode::F32Load | Opcode::F64Load | Opcode::I32Load8S | Opcode::I32Load8U | Opcode::I32Load16S | Opcode::I32Load16U |
            Opcode::I64Load16S | Opcode::I64Load16U | Opcode::I64Load8S | Opcode::I64Load8U | Opcode::I64Load32S | Opcode::I64Load32U | Opcode::I32Store | Opcode::I64Store |
            Opcode::F32Store | Opcode::F64Store | Opcode::I32Store8 | Opcode::I32Store16 | Opcode::I64Store8 | Opcode::I64Store16 | Opcode::I64Store32 => {
                let _align = stream.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)? as u32;
                let offset = stream.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)? as u32;

                instruction_stream.write(&instruction.unwrap());
                instruction_stream.write(&offset);
            }
            Opcode::MemorySize | Opcode::MemoryGrow => {
                if stream.get::<u8>().ok_or(WASMDecodeError::UnexpectedStreamEnd)? != 0 {
                    return Err(WASMDecodeError::ValidationError);
                }
            }
            Opcode::RefFunc | Opcode::Br | Opcode::BrIf | Opcode::Call | Opcode::LocalGet | Opcode::LocalSet | Opcode::LocalTee |
            Opcode::GlobalGet | Opcode::GlobalSet | Opcode::TableGet | Opcode::TableSet | Opcode::I32Const => {
                instruction_stream.write(&instruction.unwrap());
                instruction_stream.write(&(stream.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)? as u32));
            }
            Opcode::RefNull => {
                let byte = stream.get::<u8>().ok_or(WASMDecodeError::UnexpectedStreamEnd)?;
                instruction_stream.write(&instruction::Instruction::RefNull);
                instruction_stream.write(&(types::ReferenceType::try_from(byte).map_err(|_| WASMDecodeError::UnknownReferenceType(byte))? as u8));
            }
            // Vector and system instrucitons
            Opcode::Vector | Opcode::System => return Err(WASMDecodeError::UnsupportedFeature),
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

impl ModuleImpl {
    pub fn from_wasm(src: &[u8]) -> Result<Self, WASMDecodeError> {
        // Sections
        let sections = decode_sections(src)?;

        let function_types = if let Some(bits) = sections.get(&types::SectionID::Type) {
            decode_function_type_section(bits)?
        } else {
            Vec::new()
        };

        let function_type_idx = if let Some(bits) = sections.get(&types::SectionID::Function) {
            decode_function_section(bits)?
        } else {
            Vec::new()
        };

        let tables = if let Some(bits) = sections.get(&types::SectionID::Table) {
            decode_table_section(bits)?
        } else {
            Vec::new()
        };

        let memories = if let Some(bits) = sections.get(&types::SectionID::Memory) {
            decode_memory_section(bits)?
        } else {
            Vec::new()
        };

        let globals: Vec<GlobalDescriptor> = if let Some(bits) = sections.get(&types::SectionID::Global) {
            decode_global_section(bits)?
        } else {
            Vec::new()
        };

        let exports = if let Some(bits) = sections.get(&types::SectionID::Export) {
            decode_export_section(bits)?
        } else {
            HashMap::new()
        };

        let start = if let Some(bits) = sections.get(&types::SectionID::Start) {
            Some(decode_start_section(bits)?)
        } else {
            None
        };

        let code = if let Some(bits) = sections.get(&types::SectionID::Code) {
            decode_code_section(bits)?
        } else {
            Vec::new()
        };

        if function_type_idx.len() != code.len() {
            return Err(WASMDecodeError::FunctionTypesSizeAndCodeSizeUnmatched);
        }

        let functions = function_type_idx.iter().map(|v| *v).zip(code.iter()).map(|(type_id, code)| -> Result<_, WASMDecodeError> {
            if function_types.len() <= type_id as usize {
                return Err(WASMDecodeError::ValidationError);
            }

            let mut stream = BinaryInputStream::new(code);

            Ok(Function {
                type_id,
                locals: {
                    let mut locals = Vec::<types::ValueType>::new();

                    for _ in 0..stream.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)? {
                        let local_repeat_count = stream.decode_unsigned().ok_or(WASMDecodeError::UnsignedDecodeError)?;
                        let valtype_byte = stream.get::<u8>().ok_or(WASMDecodeError::UnexpectedStreamEnd)?;
                        let ty: types::ValueType = valtype_byte.try_into().map_err(|_| WASMDecodeError::UnknownValueType(valtype_byte))?;

                        locals.extend(std::iter::repeat(ty).take(local_repeat_count));
                    }

                    locals
                },
                expression: decode_expression(&mut stream)?,
            })
        }).collect::<Result<Vec<Function>, WASMDecodeError>>()?;

        Ok(ModuleImpl {
            exports,
            functions,
            globals,
            tables,
            memories,
            imports: HashMap::new(),
            types: function_types,
            start,
        })
    }
} // impl Module
