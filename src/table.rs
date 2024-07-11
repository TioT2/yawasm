use crate::types::{Limits, TableType};

pub struct Table {
    ty: TableType,
    elements: Vec<u32>,
}

impl Table {
    pub fn new(ty: TableType, initial_value: Option<u32>) -> Option<Table> {
        if !ty.limits.validate() {
            return None;
        }
        Some(Table {
            elements: vec! [initial_value.unwrap_or(0); ty.limits.min as usize],
            ty,
        })
    }

    pub fn get(&self, index: u32) -> Option<u32> {
        self.elements.get(index as usize).copied()
    }

    pub fn get_mut(&mut self, index: u32) -> Option<&mut u32> {
        self.elements.get_mut(index as usize)
    }

    pub fn grow(&mut self, by: u32) -> bool {
        if let Some(max) = self.ty.limits.max {
            if self.elements.len() + by as usize > max as usize {
                return false;
            }
        }

        self.elements.resize(self.elements.len() + by as usize, 0);

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