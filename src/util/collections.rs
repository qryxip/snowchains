use std::ops::{Index, IndexMut};
use std::slice::{self, SliceIndex};

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct NonEmptyVec<T>(Vec<T>);

impl<T> NonEmptyVec<T> {
    pub(crate) fn try_new(vec: Vec<T>) -> Option<Self> {
        if vec.is_empty() {
            None
        } else {
            Some(NonEmptyVec(vec))
        }
    }

    pub(crate) fn last(&self) -> &T {
        self.0.last().unwrap()
    }

    pub(crate) fn iter(&self) -> slice::Iter<T> {
        self.0.iter()
    }

    pub(crate) fn max<R: Ord>(&self, f: impl Fn(&T) -> R) -> R {
        self.0.iter().map(&f).max().unwrap()
    }
}

impl<T: Default> Default for NonEmptyVec<T> {
    fn default() -> Self {
        NonEmptyVec(vec![T::default()])
    }
}

impl<T, I: SliceIndex<[T]>> Index<I> for NonEmptyVec<T> {
    type Output = I::Output;

    fn index(&self, index: I) -> &I::Output {
        &self.0[index]
    }
}

impl<T, I: SliceIndex<[T]>> IndexMut<I> for NonEmptyVec<T> {
    fn index_mut(&mut self, index: I) -> &mut <Self as Index<I>>::Output {
        &mut self.0[index]
    }
}

impl<'a, T> IntoIterator for &'a NonEmptyVec<T> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    fn into_iter(self) -> slice::Iter<'a, T> {
        self.0.iter()
    }
}
