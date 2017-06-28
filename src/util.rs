pub trait OkAsRefOr {
    type Item;
    fn ok_as_ref_or<E>(&self, e: E) -> Result<&Self::Item, E>;
}

impl<T> OkAsRefOr for Option<T> {
    type Item = T;
    fn ok_as_ref_or<E>(&self, e: E) -> Result<&T, E> {
        match *self {
            Some(ref x) => Ok(x),
            None => Err(e),
        }
    }
}


pub trait UnwrapAsRefMut {
    type Item;
    fn unwrap_as_ref_mut(&mut self) -> &mut Self::Item;
}

impl<T> UnwrapAsRefMut for Option<T> {
    type Item = T;
    fn unwrap_as_ref_mut(&mut self) -> &mut T {
        match *self {
            Some(ref mut x) => x,
            None => {
                panic!("called `<Option as UnwrapAsRefMut>::unwrap_as_ref_mut` \
                        on a `None` value")
            }
        }
    }
}
