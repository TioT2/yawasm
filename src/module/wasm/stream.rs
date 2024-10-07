pub struct Stream<'t> {
    rest: &'t [u8],
}

impl<'t> Stream<'t> {
    pub fn new(data: &'t [u8]) -> Self {
        Self { rest: data }
    }

    pub fn read_byte_slice(&mut self, len: usize) -> Option<&'t [u8]> {
        let (data, rest) = self.rest.split_at_checked(len)?;
        self.rest = rest;
        Some(data)
    }

    pub fn read<T: bytemuck::AnyBitPattern>(&mut self) -> Option<T> {
        let (data, rest) = self.rest.split_at_checked(std::mem::size_of::<T>())?;
        let result = bytemuck::try_pod_read_unaligned::<T>(data).ok()?;
        self.rest = rest;
        Some(result)
    }

    pub fn decode_unsigned(&mut self) -> Option<usize> {
        let mut result = 0;
        let mut shift = 0;
        let mut byte;

        loop {
            byte = self.read::<u8>()?;
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
            byte = self.read::<u8>()?;
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
