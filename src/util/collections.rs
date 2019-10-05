use indexmap::{IndexMap, IndexSet};
use itertools::Itertools as _;
use serde::ser::SerializeMap as _;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use std::cmp::Ord;
use std::collections::BTreeMap;
use std::hash::Hash;
use std::num::NonZeroUsize;
use std::ops::{Deref, Index, IndexMut};
use std::slice::{self, SliceIndex};
use std::vec;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
#[serde(transparent)]
pub(crate) struct NonEmptyVec<T>(Vec<T>);

impl<T> NonEmptyVec<T> {
    pub(crate) fn try_new(vec: Vec<T>) -> Option<Self> {
        if vec.is_empty() {
            None
        } else {
            Some(NonEmptyVec(vec))
        }
    }

    pub(crate) fn len(&self) -> NonZeroUsize {
        NonZeroUsize::new(self.0.len()).unwrap()
    }

    pub(crate) fn last(&self) -> &T {
        self.0.last().unwrap()
    }

    pub(crate) fn iter(&self) -> slice::Iter<T> {
        self.0.iter()
    }

    /// # Panics
    ///
    /// Panics if `self.len() != other.count()`.
    pub(crate) fn zip_eq<I: IntoIterator>(self, other: I) -> NonEmptyVec<(T, I::Item)> {
        NonEmptyVec(self.into_iter().zip_eq(other).collect())
    }

    pub(crate) fn ref_map<B, F: FnMut(&T) -> B>(&self, f: F) -> NonEmptyVec<B> {
        NonEmptyVec(self.0.iter().map(f).collect())
    }

    pub(crate) fn enumerate_map<B, F: FnMut(usize, T) -> B>(self, mut f: F) -> NonEmptyVec<B> {
        NonEmptyVec(
            self.0
                .into_iter()
                .enumerate()
                .map(|(i, x)| f(i, x))
                .collect(),
        )
    }

    pub(crate) fn map<B, F: FnMut(T) -> B>(self, f: F) -> NonEmptyVec<B> {
        NonEmptyVec(self.0.into_iter().map(f).collect())
    }
}

impl<T: Default> Default for NonEmptyVec<T> {
    fn default() -> Self {
        NonEmptyVec(vec![T::default()])
    }
}

impl<T> Into<Vec<T>> for NonEmptyVec<T> {
    fn into(self) -> Vec<T> {
        self.0
    }
}

impl<T> Deref for NonEmptyVec<T> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        &self.0
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

impl<T> IntoIterator for NonEmptyVec<T> {
    type Item = T;
    type IntoIter = vec::IntoIter<T>;

    fn into_iter(self) -> vec::IntoIter<T> {
        self.0.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a NonEmptyVec<T> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    fn into_iter(self) -> slice::Iter<'a, T> {
        self.0.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut NonEmptyVec<T> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;

    fn into_iter(self) -> slice::IterMut<'a, T> {
        self.0.iter_mut()
    }
}

#[derive(Debug, Serialize)]
#[serde(transparent)]
pub(crate) struct NonEmptyIndexMap<K: Eq + Hash, V>(IndexMap<K, V>);

impl<K: Eq + Hash, V> NonEmptyIndexMap<K, V> {
    pub(crate) fn try_new(map: IndexMap<K, V>) -> Option<Self> {
        if map.is_empty() {
            None
        } else {
            Some(Self(map))
        }
    }

    pub(crate) fn into_element(mut self, key: &(impl Hash + indexmap::Equivalent<K>)) -> Option<V> {
        self.0.swap_remove(key)
    }
}

impl<K: Eq + Hash, V> Deref for NonEmptyIndexMap<K, V> {
    type Target = IndexMap<K, V>;

    fn deref(&self) -> &IndexMap<K, V> {
        &self.0
    }
}

impl<'a, K: Eq + Hash, V> IntoIterator for &'a NonEmptyIndexMap<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = indexmap::map::Iter<'a, K, V>;

    fn into_iter(self) -> indexmap::map::Iter<'a, K, V> {
        self.0.iter()
    }
}

impl<K: Eq + Hash, V> IntoIterator for NonEmptyIndexMap<K, V> {
    type Item = (K, V);
    type IntoIter = indexmap::map::IntoIter<K, V>;

    fn into_iter(self) -> indexmap::map::IntoIter<K, V> {
        self.0.into_iter()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct NonEmptyIndexSet<T: Hash + Eq>(IndexSet<T>);

impl<T: Hash + Eq> NonEmptyIndexSet<T> {
    pub(crate) fn try_new(set: IndexSet<T>) -> Option<Self> {
        guard!(!set.is_empty());
        Some(Self(set))
    }
}

impl<T: Hash + Eq> NonEmptyIndexSet<T> {
    pub(crate) fn ref_map<B: Hash + Eq, F: FnMut(&T) -> B>(&self, f: F) -> NonEmptyIndexSet<B> {
        Self(self.0.iter().map(f).collect())
    }

    pub(crate) fn map<B: Hash + Eq, F: FnMut(T) -> B>(self, f: F) -> NonEmptyIndexSet<B> {
        Self(self.0.into_iter().map(f).collect())
    }
}

impl<T: Hash + Eq> Deref for NonEmptyIndexSet<T> {
    type Target = IndexSet<T>;

    fn deref(&self) -> &IndexSet<T> {
        &self.0
    }
}

impl<'a, T: Hash + Eq> IntoIterator for &'a NonEmptyIndexSet<T> {
    type Item = &'a T;
    type IntoIter = indexmap::set::Iter<'a, T>;

    fn into_iter(self) -> indexmap::set::Iter<'a, T> {
        self.0.iter()
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug)]
pub(crate) struct SingleKeyValue<K, V> {
    pub(crate) key: K,
    pub(crate) value: V,
}

impl<K: Serialize, V: Serialize> Serialize for SingleKeyValue<K, V> {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        let mut map = serializer.serialize_map(Some(1))?;
        map.serialize_entry(&self.key, &self.value)?;
        map.end()
    }
}

impl<'de, K: Ord + Deserialize<'de>, V: Deserialize<'de>> Deserialize<'de>
    for SingleKeyValue<K, V>
{
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        let map = BTreeMap::<K, V>::deserialize(deserializer)?;
        if map.len() == 1 {
            let (key, value) = map.into_iter().next().unwrap();
            Ok(Self { key, value })
        } else {
            Err(serde::de::Error::custom("expected single key-value pair"))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::util::collections::{NonEmptyVec, SingleKeyValue};

    use pretty_assertions::assert_eq;

    #[test]
    fn test_non_empty_vec() {
        assert_eq!(NonEmptyVec::<()>::try_new(vec![]), None);
        assert_eq!(NonEmptyVec::try_new(vec![()]), Some(NonEmptyVec(vec![()])));
        assert_eq!(*NonEmptyVec::try_new(vec![42]).unwrap().last(), 42);
        assert_eq!(
            NonEmptyVec::try_new(vec![(); 10]).unwrap().iter().count(),
            10
        );
        assert_eq!(&mut NonEmptyVec::<()>::default()[0], &mut ());
        assert_eq!(NonEmptyVec::<()>::default(), NonEmptyVec(vec![()]));
        assert_eq!(
            (&NonEmptyVec::try_new(vec![(); 10]).unwrap())
                .into_iter()
                .count(),
            10
        );
    }

    #[test]
    fn test_ser_single_key_value() -> serde_json::Result<()> {
        let serialized = serde_json::to_string(&SingleKeyValue {
            key: "key",
            value: "value",
        })?;
        assert_eq!(serialized, r#"{"key":"value"}"#);
        Ok(())
    }

    #[test]
    fn test_de_single_key_value() -> serde_json::Result<()> {
        let deserialized =
            serde_json::from_str::<SingleKeyValue<String, String>>(r#"{"key":"value"}"#)?;
        assert_eq!(
            deserialized,
            SingleKeyValue {
                key: "key".to_owned(),
                value: "value".to_owned(),
            }
        );
        let err = serde_json::from_str::<SingleKeyValue<String, String>>(
            r#"{"key1":"value1","key2":"value2"}"#,
        )
        .unwrap_err();
        assert_eq!(err.to_string(), "expected single key-value pair");
        Ok(())
    }
}
