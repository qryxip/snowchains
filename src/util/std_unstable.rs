use std::time::Duration;

pub(crate) trait AsMillis_ {
    fn as_millis_(self) -> u128;
}

impl AsMillis_ for Duration {
    fn as_millis_(self) -> u128 {
        1000 * u128::from(self.as_secs()) + u128::from(self.subsec_millis())
    }
}

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
