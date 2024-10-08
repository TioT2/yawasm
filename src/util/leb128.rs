
impl<'t> super::binary_stream::BinaryInputStream<'t> {
    pub fn decode_unsigned(&mut self) -> Option<usize> {
        let mut result = 0;
        let mut shift = 0;
        let mut byte;

        loop {
            byte = self.get::<u8>()?;
            result |= ((byte & 0x7F) as usize) << shift;
            shift += 7;

            if byte & 0x80 == 0 {
                return Some(result);
            }
        }
    }

    pub fn decode_signed<const BIT_COUNT: u32>(&mut self) -> Option<isize> {
        let mut result = 0;
        let mut shift = 0;
        let mut byte;

        loop {
            byte = self.get::<u8>()?;
            result |= ((byte & 0x7F) as usize) << shift;
            shift += 7;

            if byte & 0x80 == 0 {
                return Some(unsafe { std::mem::transmute::<usize, isize>(if shift < BIT_COUNT && byte & 0x40 == 1 {
                    result | (!0usize << shift)
                } else {
                    result
                })})
            }
        }
    }
}
