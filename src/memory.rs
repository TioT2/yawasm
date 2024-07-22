
const WASM_PAGE_SIZE: usize = 65536;
const ALLOCATION_ALIGNMENT: usize = 1;

pub struct Memory {
    page_count: usize,
    ptr: *mut u8,
}

impl Memory {
    pub fn new() -> Memory {
        let alloc_layout = std::alloc::Layout::from_size_align(WASM_PAGE_SIZE, 1).unwrap();
        let ptr = unsafe { std::alloc::alloc_zeroed(alloc_layout.clone()) };

        if ptr.is_null() {
            std::alloc::handle_alloc_error(alloc_layout);
        }

        return Memory { ptr, page_count: 1 };
    }

    pub fn size(&self) -> usize {
        self.page_count * WASM_PAGE_SIZE
    }

    pub fn page_count(&self) -> usize {
        self.page_count
    }

    pub fn load_unaligned<T: bytemuck::AnyBitPattern>(&self, ptr: usize) -> Option<T> {
        if ptr + std::mem::size_of::<T>() < self.page_count * WASM_PAGE_SIZE {
            Some(unsafe {
                std::mem::transmute::<*mut u8, *mut T>(self.ptr.add(ptr)).read_unaligned()
            })
        } else {
            None
        }
    }

    pub fn store<T: bytemuck::AnyBitPattern>(&self, ptr: usize, data: T) -> Option<()> {
        if ptr + std::mem::size_of::<T>() < self.page_count * WASM_PAGE_SIZE {
            unsafe {
                std::mem::transmute::<*mut u8, *mut T>(self.ptr.add(ptr)).write(data)
            }
            Some(())
        } else {
            None
        }
    }

    pub fn grow(&mut self, grow_page_count: usize) {
        let new_page_count = self.page_count + grow_page_count;
        let new_alloc_layout = std::alloc::Layout::from_size_align(new_page_count * WASM_PAGE_SIZE, ALLOCATION_ALIGNMENT).unwrap();

        unsafe {
            self.ptr = std::alloc::realloc(self.ptr, std::alloc::Layout::from_size_align(self.page_count * WASM_PAGE_SIZE, ALLOCATION_ALIGNMENT).unwrap(), new_alloc_layout.size());

            if self.ptr.is_null() {
                std::alloc::handle_alloc_error(new_alloc_layout);
            }

            // Initialize newly allocated memory blocks
            self.ptr.add(self.page_count * 65536).write_bytes(0, grow_page_count * WASM_PAGE_SIZE);
        }

        self.page_count = new_page_count;
    }

    pub fn as_slice<'t>(&'t self) -> &'t [u8] {
        unsafe {
            std::slice::from_raw_parts(self.ptr, self.page_count * WASM_PAGE_SIZE)
        }
    }

    pub fn as_slice_mut<'t>(&'t mut self) -> &'t mut [u8] {
        unsafe {
            std::slice::from_raw_parts_mut(self.ptr, self.page_count * WASM_PAGE_SIZE)
        }
    }

    pub fn to_vec(self) -> Vec<u8> {
        unsafe {
            Vec::from_raw_parts(self.ptr, self.page_count * WASM_PAGE_SIZE, self.page_count * WASM_PAGE_SIZE)
        }
    }
}

impl Drop for Memory {
    fn drop(&mut self) {
        unsafe {
            std::alloc::dealloc(self.ptr, std::alloc::Layout::from_size_align(self.page_count * WASM_PAGE_SIZE, ALLOCATION_ALIGNMENT).unwrap());
        }
    }
}
