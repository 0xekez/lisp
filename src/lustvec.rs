use rev_slice::{RevSlice, SliceExt};
use std::ops::{Index, IndexMut};

/// A vec wrapper that supports efficent append and pop at the
/// beginning of the list.
#[derive(Debug, Clone)]
pub struct LustVec<T> {
    pub data: Vec<T>,
}

impl<T> LustVec<T>
where
    T: Clone + Default,
{
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn with_capacity(size: usize) -> Self {
        Self {
            data: Vec::with_capacity(size),
        }
    }

    pub fn with_len(size: usize) -> Self {
        Self {
            data: vec![Default::default(); size],
        }
    }

    pub fn from_slice(slice: &[T]) -> Self {
        Self {
            data: slice.to_vec(),
        }
    }

    pub fn push_front(&mut self, item: T) {
        self.data.push(item);
    }

    pub fn first(&self) -> Option<&T> {
        self.data.last()
    }

    pub fn split_first(&self) -> Option<(&T, &[T])> {
        self.data.split_last()
    }

    pub fn remove_first_view(&self) -> &RevSlice<T> {
        &(&self.data[..]).rev()[1..self.data.len()]
    }

    pub fn remove_first(&self) -> Option<Self> {
        match self.split_first() {
            Some((_, s)) => Some(Self::from_slice(s)),
            None => None,
        }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn iter(&self) -> std::iter::Rev<std::slice::Iter<'_, T>> {
        self.data.iter().rev()
    }
}

impl<T> Index<usize> for LustVec<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.data[self.data.len() - 1 - index]
    }
}

impl<T> IndexMut<usize> for LustVec<T> {
    fn index_mut<'a>(&'a mut self, idx: usize) -> &'a mut T {
        &mut self.data[idx]
    }
}
