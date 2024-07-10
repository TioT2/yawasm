use crate::types::{Limits, TableType};

pub struct Table {
    element_type: TableType,
    max_size: Option<usize>,
    elements: Vec<u32>,
}

impl Table {
    pub fn new(element_type: TableType, size: Limits, initial_value: Option<u32>) -> Option<Table> {
        if !size.validate() {
            return None;
        }
        Some(Table {
            element_type,
            elements: vec! [initial_value.unwrap_or(0); size.min as usize],
            max_size: size.max.map(|v| v as usize),
        })
    }

    pub fn get(&self, index: u32) -> Option<u32> {
        self.elements.get(index as usize).copied()
    }

    pub fn get_mut(&mut self, index: u32) -> Option<&mut u32> {
        self.elements.get_mut(index as usize)
    }

    pub fn grow(&mut self, by: u32) -> bool {
        if let Some(max) = self.max_size {
            if self.elements.len() + by as usize > max {
                return false;
            }
        }

        return true;
    }

    pub fn as_slice(&self) -> &[u32] {
        &self.elements
    }

    pub fn as_slice_mut(&mut self) -> &mut [u32] {
        &mut self.elements
    }

    pub fn into_vec(self) -> Vec<u32> {
        self.elements
    }
}