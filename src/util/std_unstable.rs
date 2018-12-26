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

#[cfg(test)]
mod tests {
    use crate::util::std_unstable::{AsMillis_, RemoveItem_};

    use std::time::Duration;

    #[test]
    fn test_as_millis_() {
        fn test(dur: Duration, expected: u128) {
            assert_eq!(dur.as_millis_(), expected);
        }

        test(Duration::from_nanos(999_999), 0);
        test(Duration::from_nanos(1_000_000), 1);
    }

    #[test]
    fn test_remove_item_() {
        assert_eq!(vec!['a'].remove_item_(&'a'), Some('a'));
        assert_eq!(vec!['b'].remove_item_(&'a'), None);
    }
}
