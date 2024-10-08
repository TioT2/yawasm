pub struct BinaryInputStream<'t> {
    bytes: &'t [u8],
}

impl<'t> BinaryInputStream<'t> {
    pub fn new(stream: &'t [u8]) -> Self {
        Self {
            bytes: stream,
        }
    }

    pub fn get_byte_slice(&mut self, byte_count: usize) -> Option<&'t [u8]> {
        if self.bytes.len() < byte_count {
            None
        } else {
            let ret;
            (ret, self.bytes) = self.bytes.split_at(byte_count);
            Some(ret)
        }
    }

    pub fn get<T: bytemuck::AnyBitPattern>(&mut self) -> Option<T> {
        self
            .get_byte_slice(std::mem::size_of::<T>())
            .map(|v| bytemuck::try_pod_read_unaligned::<T>(v).ok())
            .flatten()
    }

    pub fn get_slice<T: bytemuck::AnyBitPattern>(&mut self, count: usize) -> Option<&'t [T]> {
        self.get_byte_slice(std::mem::size_of::<T>() * count).map(bytemuck::cast_slice::<u8, T>)
    }

    pub fn check_byte_slice(&self, byte_count: usize) -> Option<&'t [u8]> {
        if self.bytes.len() < byte_count {
            None
        } else {
            Some(self.bytes.split_at(byte_count).0)
        }
    }

    pub fn check<T: bytemuck::AnyBitPattern>(&self) -> Option<T> {
        self
            .check_byte_slice(std::mem::size_of::<T>())
            .map(|v| bytemuck::try_pod_read_unaligned::<T>(v).ok())
            .flatten()
    }

    pub fn skip(&mut self, byte_count: usize) -> Option<()> {
        if self.bytes.len() < byte_count {
            None
        } else {
            self.bytes = self.bytes.split_at(byte_count).1;
            Some(())
        }
    }
}

pub struct BinaryOutputStream {
    collector: Vec<u8>,
}

impl BinaryOutputStream {
    pub fn new() -> Self {
        Self { collector: Vec::new() }
    }

    pub fn write_slice<T: bytemuck::NoUninit>(&mut self, data: &[T]) {
        self.collector.extend_from_slice(bytemuck::cast_slice(data));
    }

    pub fn write<T: bytemuck::NoUninit>(&mut self, data: &T) {
        self.collector.extend_from_slice(bytemuck::bytes_of(data))
    }

    pub fn finish(self) -> Vec<u8> {
        self.collector
    }
}