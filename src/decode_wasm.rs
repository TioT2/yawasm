use std::collections::HashMap;
use crate::{instruction, opcode, types, util::binary_stream::{BinaryInputStream, BinaryOutputStream}, Function, Module, ModuleCreateError, TableBranchData};

// Own binary stream implementation
impl<'t> BinaryInputStream<'t> {
    pub fn wasm_decode_vector<DT: bytemuck::AnyBitPattern, T>(&mut self, decode_func: &dyn Fn(&DT) -> Option<T>) -> Option<Vec<T>> {
        let len = self.decode_unsigned()?;
        self.get_slice::<DT>(len)?.iter().map(decode_func).collect()
    }

    pub fn wasm_decode_limits(&mut self) -> Option<types::Limits> {
        match self.get::<u8>()? {
            0 => Some(types::Limits { min: self.decode_unsigned()? as u32, max: None }),
            1 => Some(types::Limits { min: self.decode_unsigned()? as u32, max: Some(self.decode_unsigned()? as u32) }),
            _ => None,
        }
    }

    pub fn wasm_decode_block_type(&mut self) -> Option<instruction::BlockType> {
        let byte = self.check::<u8>()?;

        if byte == 0x40 {
            self.skip(1).unwrap();
            Some(instruction::BlockType::Void)
        } else if let Ok(val_type) = types::ValueType::try_from(byte) {
            self.skip(1).unwrap();
            Some(instruction::BlockType::ResolvingTo(val_type))
        } else {
            Some(instruction::BlockType::Functional(self.decode_signed::<32>()? as u32))
        }
    }
}

fn decode_sections<'t>(bits: &'t [u8]) -> Result<HashMap<types::SectionID, &'t [u8]>, ModuleCreateError> {
    let mut stream = BinaryInputStream::new(bits);

    if !stream.get::<types::Header>().ok_or(ModuleCreateError::UnexpectedStreamEnd)?.validate() {
        return Err(ModuleCreateError::WASMDeocdeError);
    }

    let mut sections = HashMap::<types::SectionID, &[u8]>::new();

    while let Some(section_id) = stream.get::<u8>() {
        let section_id = types::SectionID::try_from(section_id).map_err(|_| ModuleCreateError::WASMDeocdeError)?;
        let length = stream.decode_unsigned().ok_or(ModuleCreateError::WASMDeocdeError)?;
        let data = stream.get_byte_slice(length).ok_or(ModuleCreateError::WASMDeocdeError)?;
        sections.insert(section_id, data);
    }

    Ok(sections)
}

fn decode_function_type_section(section: &[u8]) -> Result<Vec<types::FunctionType>, ModuleCreateError> {
    let mut stream = BinaryInputStream::new(section);

    let type_count = stream.decode_unsigned().ok_or(ModuleCreateError::WASMDeocdeError)?;

    (0..type_count).map(|_| {
            let functype_magic = stream.get::<u8>().ok_or(ModuleCreateError::UnexpectedStreamEnd)?;

            if functype_magic != types::WASM_FUNCTYPE_MAGIC {
                return Err(ModuleCreateError::WASMDeocdeError);
            }

            const DECODE_FUNC: &'static dyn Fn(&u8) -> Option<types::ValueType> = &|v: &u8| types::ValueType::try_from(*v).ok();

            Ok(types::FunctionType {
                inputs: stream.wasm_decode_vector(DECODE_FUNC).ok_or(ModuleCreateError::WASMDeocdeError)?,
                outputs: stream.wasm_decode_vector(DECODE_FUNC).ok_or(ModuleCreateError::WASMDeocdeError)?,
            })
        })
        .collect::<Result<Vec<types::FunctionType>, ModuleCreateError>>()
}

fn decode_function_section(section: &[u8]) -> Result<Vec<u32>, ModuleCreateError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)?)
        .map(|_| stream.decode_unsigned().map(|v| v as u32).ok_or(ModuleCreateError::UnexpectedStreamEnd))
        .collect::<Result<Vec<u32>, ModuleCreateError>>()
}

fn decode_table_section(section: &[u8]) -> Result<Vec<types::TableType>, ModuleCreateError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)?)
        .map(|_| Ok(types::TableType {
            reference_type: TryInto::<types::ReferenceType>::try_into(stream.get::<u8>().ok_or(ModuleCreateError::UnexpectedStreamEnd)?).map_err(|_| ModuleCreateError::WASMDeocdeError)?,
            limits: stream.wasm_decode_limits().ok_or(ModuleCreateError::WASMDeocdeError)?,
        }))
        .collect::<Result<Vec<types::TableType>, ModuleCreateError>>()
}

fn decode_memory_section(section: &[u8]) -> Result<Vec<types::Limits>, ModuleCreateError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)?)
        .map(|_| stream.wasm_decode_limits().ok_or(ModuleCreateError::WASMDeocdeError))
        .collect::<Result<Vec<types::Limits>, ModuleCreateError>>()
}

fn decode_export_section(section: &[u8]) -> Result<HashMap<String, types::ExportDescriptor>, ModuleCreateError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)?)
        .map(|_| -> Result<(String, types::ExportDescriptor), ModuleCreateError> {
            Ok(({
                let len = stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)?;

                String::from_utf8(stream.get_byte_slice(len).ok_or(ModuleCreateError::UnexpectedStreamEnd)?.to_vec()).map_err(|_| ModuleCreateError::WASMDeocdeError)?
            }, types::ExportDescriptor {
                ty: (stream.get::<u8>().ok_or(ModuleCreateError::UnexpectedStreamEnd)?).try_into().map_err(|_| ModuleCreateError::WASMDeocdeError)?,
                index: stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)? as u32,
            }))
        })
        .collect::<Result<HashMap<String, types::ExportDescriptor>, ModuleCreateError>>()
}

fn decode_start_section(section: &[u8]) -> Result<u32, ModuleCreateError> {
    BinaryInputStream::new(section)
        .decode_unsigned()
        .map(|v| v as u32)
        .ok_or(ModuleCreateError::UnexpectedStreamEnd)
}

fn decode_code_section(section: &[u8]) -> Result<Vec<&[u8]>, ModuleCreateError> {
    let mut stream = BinaryInputStream::new(section);

    (0..stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)?)
        .map(|_| -> Result<&[u8], ModuleCreateError> {
            let length = stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)?;

            Ok(stream.get_byte_slice(length).ok_or(ModuleCreateError::UnexpectedStreamEnd)?)
        })
        .collect::<Result<Vec<&[u8]>, ModuleCreateError>>()
}

fn decode_expression(stream: &mut BinaryInputStream, table_branch_datas: &mut Vec<TableBranchData>) -> Result<(Vec<u8>, u8), ModuleCreateError> {
    let mut instruction_stream = BinaryOutputStream::new();

    let ending_byte = 'block_parsing_loop: loop {
        let opcode_u = stream.get::<u8>().ok_or(ModuleCreateError::UnexpectedStreamEnd)?;
        let opcode = opcode::Main::try_from(opcode_u).map_err(|_| ModuleCreateError::WASMDeocdeError)?;

        let instruction = instruction::Instruction::try_from(opcode).ok();

        type Opcode = opcode::Main;

        match opcode {
            Opcode::Else | Opcode::ExpressionEnd => {
                break 'block_parsing_loop opcode_u;
            }
            // Decode block
            Opcode::Block | Opcode::Loop | Opcode::If => {
                // Requires block parsing

                let ty = stream.wasm_decode_block_type().ok_or(ModuleCreateError::UnexpectedStreamEnd)?;
                let (block_bits, ending_byte) = decode_expression(stream, table_branch_datas)?;

                instruction_stream.write(&instruction.unwrap());

                // Write conditional
                instruction_stream.write(&instruction::BlockHeader {
                    length: block_bits.len() as u32,
                    ty,
                });
                instruction_stream.write_slice(&block_bits);

                if opcode == Opcode::If && ending_byte == Opcode::Else as u8 {
                    let (else_bits, ending_byte) = decode_expression(stream, table_branch_datas)?;
                    if ending_byte != Opcode::ExpressionEnd as u8 {
                        return Err(ModuleCreateError::WASMDeocdeError);
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
                instruction_stream.write(&(table_branch_datas.len() as u32));

                // Parse table branch data
                table_branch_datas.push(TableBranchData {
                    labels: (0..stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)?)
                        .map(|_| stream
                            .decode_unsigned()
                            .map(|v| v as u32)
                            .ok_or(ModuleCreateError::UnexpectedStreamEnd)
                        )
                        .collect::<Result<Vec<u32>, ModuleCreateError>>()?,

                    default: stream
                        .decode_unsigned()
                        .ok_or(ModuleCreateError::UnexpectedStreamEnd)? as u32,
                });
            }
            Opcode::CallIndirect => {
                let ty = stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)? as u32;
                let table_index = stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)? as u32;

                instruction_stream.write(&instruction::Instruction::CallIndirect);
                instruction_stream.write(&ty);
                instruction_stream.write(&table_index);
            }
            Opcode::I64Const => {
                instruction_stream.write(&instruction::Instruction::I64Const);
                instruction_stream.write(&(stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)? as u64))
            }
            Opcode::F32Const => {
                instruction_stream.write(&instruction::Instruction::F32Const);
                instruction_stream.write(&stream.get::<f32>().ok_or(ModuleCreateError::UnexpectedStreamEnd)?)
            }
            Opcode::F64Const => {
                instruction_stream.write(&instruction::Instruction::F64Const);
                instruction_stream.write(&stream.get::<f64>().ok_or(ModuleCreateError::UnexpectedStreamEnd)?)
            }
            Opcode::I32Load | Opcode::I64Load | Opcode::F32Load | Opcode::F64Load | Opcode::I32Load8S | Opcode::I32Load8U | Opcode::I32Load16S | Opcode::I32Load16U |
            Opcode::I64Load16S | Opcode::I64Load16U | Opcode::I64Load8S | Opcode::I64Load8U | Opcode::I64Load32S | Opcode::I64Load32U | Opcode::I32Store | Opcode::I64Store |
            Opcode::F32Store | Opcode::F64Store | Opcode::I32Store8 | Opcode::I32Store16 | Opcode::I64Store8 | Opcode::I64Store16 | Opcode::I64Store32 => {
                let _align = stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)? as u32;
                let offset = stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)? as u32;

                instruction_stream.write(&instruction.unwrap());
                instruction_stream.write(&offset);
            }
            Opcode::MemorySize | Opcode::MemoryGrow => {
                if stream.get::<u8>().ok_or(ModuleCreateError::UnexpectedStreamEnd)? != 0 {
                    return Err(ModuleCreateError::WASMDeocdeError);
                }
            }
            Opcode::RefFunc | Opcode::Br | Opcode::BrIf | Opcode::Call | Opcode::LocalGet | Opcode::LocalSet | Opcode::LocalTee |
            Opcode::GlobalGet | Opcode::GlobalSet | Opcode::TableGet | Opcode::TableSet | Opcode::I32Const => {
                instruction_stream.write(&instruction.unwrap());
                instruction_stream.write(&(stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)? as u32));
            }
            Opcode::RefNull => {
                instruction_stream.write(&instruction::Instruction::RefNull);
                instruction_stream.write(&(types::ReferenceType::try_from(stream.get::<u8>().ok_or(ModuleCreateError::UnexpectedStreamEnd)?).map_err(|_| ModuleCreateError::WASMDeocdeError)? as u8));
            }
            // Vector and system instrucitons
            Opcode::Vector | Opcode::System => {
                todo!("Build vector and system instruction decoder")
            }
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

impl crate::Module {
    pub fn from_wasm(src: &[u8]) -> Result<Self, ModuleCreateError> {
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
        _ = tables;

        let memories = if let Some(bits) = sections.get(&types::SectionID::Memory) {
            decode_memory_section(bits)?
        } else {
            Vec::new()
        };
        _ = memories;

        let globals: Vec<types::GlobalDescriptor> = if let Some(bits) = sections.get(&types::SectionID::Global) {
            _ = bits;
            todo!("Add global variable support");
        } else {
            Vec::new()
        };
        _ = globals;

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
            return Err(ModuleCreateError::WASMDeocdeError);
        }

        let functions = function_type_idx.iter().map(|v| *v).zip(code.iter()).map(|(type_id, code)| -> Result<_, ModuleCreateError> {
            if function_types.len() <= type_id as usize {
                return Err(ModuleCreateError::WASMDeocdeError);
            }

            let mut stream = BinaryInputStream::new(code);

            let mut locals = Vec::<types::ValueType>::new();

            for _ in 0..stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)? {
                let count = stream.decode_unsigned().ok_or(ModuleCreateError::UnexpectedStreamEnd)?;
                let ty: types::ValueType = (stream.get::<u8>().ok_or(ModuleCreateError::UnexpectedStreamEnd)?).try_into().map_err(|_| ModuleCreateError::WASMDeocdeError)?;

                for _ in 0..count {
                    locals.push(ty);
                }
            }

            let mut table_branch_datas = Vec::<TableBranchData>::new();
            let (expression, ending_byte) = decode_expression(&mut stream, &mut table_branch_datas)?;

            if ending_byte != opcode::Main::ExpressionEnd as u8 {
                return Err(ModuleCreateError::WASMDeocdeError);
            }

            Ok(Function {
                locals,
                type_id,
                expression,
                table_branch_datas,
            })
        }).collect::<Result<Vec<Function>, ModuleCreateError>>()?;

        Ok(Module {
            exports,
            functions,
            imports: HashMap::new(),
            types: function_types,
            start,
        })
    }
} // impl Module
