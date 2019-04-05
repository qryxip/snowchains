pub(crate) trait RemoveItem_ {
    type Item;
    fn remove_item_(&mut self, item: &Self::Item) -> Option<Self::Item>;
}

impl<T: PartialEq> RemoveItem_ for Vec<T> {
    type Item = T;

    fn remove_item_(&mut self, item: &T) -> Option<T> {
        let pos = self.iter().position(|x| *x == *item)?;
        Some(self.remove(pos))
    }
}

#[cfg(test)]
mod tests {
    use crate::util::std_unstable::RemoveItem_ as _;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_remove_item_() {
        assert_eq!(vec!['a'].remove_item_(&'a'), Some('a'));
        assert_eq!(vec!['b'].remove_item_(&'a'), None);
    }
}
