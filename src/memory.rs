use crate::Limits;


/// Size of single memory page
pub const PAGE_SIZE: usize = 65536;

/// Alignment of memory allocations
const ALLOCATION_ALIGNMENT: usize = 1;

/// WASM memory representation structure
pub struct Memory {
    allocation_limits: Limits,

    /// Count of memory pages allocated
    page_count: usize,

    /// Pointer to memory block
    ptr: *mut u8,
} // struct Memory

impl Memory {
    /// Memory constructor
    /// * Returns new memory with single page allocated
    pub fn new(allocation_limits: Limits) -> Memory {
        let alloc_layout = std::alloc::Layout::from_size_align(PAGE_SIZE * allocation_limits.min as usize, 1).unwrap();
        let ptr = unsafe { std::alloc::alloc_zeroed(alloc_layout.clone()) };

        if ptr.is_null() {
            std::alloc::handle_alloc_error(alloc_layout);
        }

        return Memory {
            allocation_limits,
            ptr,
            page_count: 1
        };
    } // fn new

    /// Memory size getting function
    /// * Returns memory size in bytes
    pub fn size(&self) -> usize {
        self.page_count * PAGE_SIZE
    } // fn size

    /// Memory page count getting function
    /// * Returns allocated memory page count
    pub fn page_count(&self) -> usize {
        self.page_count
    } // fn page_count
    
    /// Unaligned loading from memory function
    /// * `ptr` - pointer to load value from
    /// * Returns option of loaded object
    pub fn load_unaligned<T: bytemuck::AnyBitPattern>(&self, ptr: usize) -> Option<T> {
        if ptr + std::mem::size_of::<T>() < self.page_count * PAGE_SIZE {
            Some(unsafe {
                std::mem::transmute::<*mut u8, *mut T>(self.ptr.add(ptr)).read_unaligned()
            })
        } else {
            None
        }
    } // fn load_unaligned

    /// Memory unaligned storage function
    /// * `ptr` - pointer to write data to
    /// * `data`  - data to write
    /// * Returns Some(()) if succeed and None otherwise
    pub fn store_unaligned<T: bytemuck::AnyBitPattern>(&self, ptr: usize, data: T) -> Option<()> {
        if ptr + std::mem::size_of::<T>() < self.page_count * PAGE_SIZE {
            unsafe {
                std::mem::transmute::<*mut u8, *mut T>(self.ptr.add(ptr)).write_unaligned(data)
            }
            Some(())
        } else {
            None
        }
    } // fn store_unaligned

    /// Memory resizing function
    /// * `grow_page_count` - count of pages to grow memory by
    pub fn grow(&mut self, grow_page_count: usize) -> Option<()> {
        let new_page_count = self.page_count + grow_page_count;

        if let Some(max_count) = self.allocation_limits.max {
            if new_page_count > max_count as usize {
                return None;
            }
        }

        let new_alloc_layout = std::alloc::Layout::from_size_align(new_page_count * PAGE_SIZE, ALLOCATION_ALIGNMENT).unwrap();

        unsafe {
            self.ptr = std::alloc::realloc(self.ptr, std::alloc::Layout::from_size_align(self.page_count * PAGE_SIZE, ALLOCATION_ALIGNMENT).unwrap(), new_alloc_layout.size());

            if self.ptr.is_null() {
                std::alloc::handle_alloc_error(new_alloc_layout);
            }

            // Initialize newly allocated memory blocks
            self.ptr.add(self.page_count * 65536).write_bytes(0, grow_page_count * PAGE_SIZE);
        }

        self.page_count = new_page_count;

        Some(())
    } // fn grow

    /// Memory as slice getting function
    /// * Returns all memory as byte slice
    pub fn as_slice<'t>(&'t self) -> &'t [u8] {
        unsafe {
            std::slice::from_raw_parts(self.ptr, self.page_count * PAGE_SIZE)
        }
    } // fn as_slice

    /// Memory as mutable slice getting function
    /// * Returns all memory as mutable byte slice
    pub fn as_slice_mut<'t>(&'t mut self) -> &'t mut [u8] {
        unsafe {
            std::slice::from_raw_parts_mut(self.ptr, self.page_count * PAGE_SIZE)
        }
    } // fn as_slice_mut

    /// Memory to vec<u8> transformation function
    pub fn to_vec(self) -> Vec<u8> {
        unsafe {
            Vec::from_raw_parts(self.ptr, self.page_count * PAGE_SIZE, self.page_count * PAGE_SIZE)
        }
    } // fn to_vec
}

impl Drop for Memory {
    fn drop(&mut self) {
        unsafe {
            std::alloc::dealloc(self.ptr, std::alloc::Layout::from_size_align(self.page_count * PAGE_SIZE, ALLOCATION_ALIGNMENT).unwrap());
        }
    }
}

// file memory.rs