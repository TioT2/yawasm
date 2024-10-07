//! WAT -> Module parser implementation module
//! 
//! WARNING: WAT Isn't supported by YAWASM at least now...

use super::Module;

/// WAT file parsing error
#[derive(Clone, Debug)]
pub enum DecodeError {
    /// There is no Wat support yet...
    Unimplemented,
} // enum WatDecodeError

pub fn parse<'t>(wat: &'t str) -> Result<Module, DecodeError> {
    _ = wat;
    Err(DecodeError::Unimplemented)
}
