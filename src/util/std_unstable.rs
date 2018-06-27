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
