use serde::Serializer;

pub(crate) fn ser_as_ref_str<S: Serializer>(
    s: impl AsRef<str>,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    serializer.serialize_str(s.as_ref())
}
