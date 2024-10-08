use crate::types::TableType;

/// Table representation structure
pub struct Table {
    /// Content type
    ty: TableType,

    /// Element types
    elements: Vec<u32>,
} // struct Table

impl Table {
    /// Table constructor
    /// * `ty` - table type
    /// * `initial_value` - initial value to fill table with
    /// * Returns constructed table
    pub fn new(ty: TableType, initial_value: Option<u32>) -> Table {
        Table {
            elements: vec! [initial_value.unwrap_or(0); ty.limits.min as usize],
            ty,
        }
    } // fn new

    /// Table element getting function
    /// * `index` - element index
    /// * Returns table element
    pub fn get(&self, index: u32) -> Option<u32> {
        self.elements.get(index as usize).copied()
    } // fn get

    /// Table element mutable reference getting function
    /// * `index` - element index
    /// * Returns element mutable reference
    pub fn get_mut(&mut self, index: u32) -> Option<&mut u32> {
        self.elements.get_mut(index as usize)
    } // fn get_mut

    /// Table growing by n elements function
    /// * `by` - count of elements to grow table on
    /// * Returns action option
    pub fn grow(&mut self, by: u32) -> Option<()> {
        if let Some(max) = self.ty.limits.max {
            if self.elements.len() + by as usize > max as usize {
                return None;
            }
        }

        self.elements.resize(self.elements.len() + by as usize, 0);

        Some(())
    } // fn grow
} // impl Table

// file stable.rs