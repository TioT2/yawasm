use std::collections::HashMap;

use crate::util::binary_stream::BinaryInputStream;

use super::{DecodeError, EnumType};


#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum SectionId {
    Custom = 0,
    Type = 1,
    Import = 2,
    Function = 3,
    Table = 4,
    Memory = 5,
    Global = 6,
    Export = 7,
    Start = 8,
    Element = 9,
    Code = 10,
    Data = 11,
    DataCount = 12,
}

impl TryFrom<u8> for SectionId {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0 => Self::Custom,
            1 => Self::Type,
            2 => Self::Import,
            3 => Self::Function,
            4 => Self::Table,
            5 => Self::Memory,
            6 => Self::Global,
            7 => Self::Export,
            8 => Self::Start,
            9 => Self::Element,
            10 => Self::Code,
            11 => Self::Data,
            12 => Self::DataCount,
            _ => return Err(())
        })
    }
}

/// WASM magic number
const WASM_MAGIC: [u8; 4] = [0x00, 0x61, 0x73, 0x6D];

/// WASM version number
const WASM_VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

/// WASM functype magic
pub const WASM_FUNCTYPE_MAGIC: u8 = 0x60;

/// WASM file header representation structure
#[repr(packed)]
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
struct Header {
    /// Magic number
    pub magic: [u8; 4],

    /// Version magic number
    pub version: [u8; 4],
} // struct Header

unsafe impl bytemuck::Zeroable for Header {}
unsafe impl bytemuck::AnyBitPattern for Header {}

impl Header {
    /// Header validation function
    /// * Returns true if header is correct supported WASM header
    pub fn is_valid(self) -> bool {
        self.magic == WASM_MAGIC && self.version == WASM_VERSION
    } // fn validate
}

/// WASM marked file represetnation structure
pub struct File<'t> {
    sections: HashMap<SectionId, &'t [u8]>,
} // struct File

impl<'t> File<'t> {
    pub fn decode(wasm: &'t [u8]) -> Result<File<'t>, DecodeError> {
        let mut stream = BinaryInputStream::new(wasm);

        if !stream.get::<Header>().ok_or(DecodeError::UnexpectedStreamEnd)?.is_valid() {
            return Err(DecodeError::InvalidHeader);
        }
    
        let mut sections = HashMap::<SectionId, &[u8]>::new();
    
        while let Some(section_id) = stream.get::<u8>() {
            let section_id = SectionId::try_from(section_id).map_err(|_| EnumType::SectionId.as_decode_error(section_id))?;
            let length = stream.decode_unsigned().ok_or(DecodeError::UnexpectedStreamEnd)?;
            let data = stream.get_byte_slice(length).ok_or(DecodeError::UnexpectedStreamEnd)?;
            sections.insert(section_id, data);
        }
    
        Ok(File {
            sections,
        })
    }

    /// Section getting function
    pub fn get_section(&self, id: SectionId) -> Option<&'t [u8]> {
        self.sections
            .get(&id)
            .copied()
    }
}

// file file.rs
