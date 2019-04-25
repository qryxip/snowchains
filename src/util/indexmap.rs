use indexmap::IndexSet;

use std::fmt;
use std::hash::Hash;

#[derive(Debug)]
pub(crate) struct FormatAsStrList<'a, S: 'a + AsRef<str> + Eq + Hash> {
    set: &'a IndexSet<S>,
}

impl<'a, S: 'a + AsRef<str> + Eq + Hash> fmt::Display for FormatAsStrList<'a, S> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_list()
            .entries(self.set.iter().map(AsRef::as_ref))
            .finish()
    }
}

pub(crate) trait IndexSetAsRefStrExt {
    type Item: AsRef<str> + Eq + Hash;
    fn format_as_str_list(&self) -> FormatAsStrList<Self::Item>;
}

impl<S: AsRef<str> + Eq + Hash> IndexSetAsRefStrExt for IndexSet<S> {
    type Item = S;

    fn format_as_str_list(&self) -> FormatAsStrList<S> {
        FormatAsStrList { set: self }
    }
}
