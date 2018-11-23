macro_rules! ensure_opt {
    ($x:expr) => {
        if !$x {
            return None;
        }
    };
}

macro_rules! plural {
    ($n:expr, $singular:expr, $plural:expr) => {
        format_args!("{} {}", $n, if $n > 1 { $plural } else { $singular })
    };
}

macro_rules! lazy_regex {
    ($expr:expr) => {{
        use once_cell::sync_lazy;
        sync_lazy!(::regex::Regex::new($expr).unwrap())
    }};
}

macro_rules! selector {
    // Suppress rustfmt
    (> $($tt:tt)+) => {
        _parse_selector!(First $($tt)+)
    };
    ($($tt:tt)+) => {
        _parse_selector!(First $($tt)+)
    };
}

macro_rules! _parse_selector {
    // https://github.com/rust-lang/rfcs/blob/master/text/1576-macros-literal-matcher.md

    (FirstOr($p:expr)) => {
        $p
    };
    (ChildOr($parent:expr, $p:expr)) => {
        ::select::predicate::Child($parent, $p)
    };
    (DescendantOr($ancestor:expr, $p:expr)) => {
        ::select::predicate::Descendant($ancestor, $p)
    };

    (First # $word:ident $($rest:tt)*) => {
        _parse_selector!(FirstReadingId(stringify!($word)) $($rest)*)
    };

    (First $name:ident $($rest:tt)*) => {
        _parse_selector!(FirstAnd(::select::predicate::Name(stringify!($name))) $($rest)*)
    };
    (FirstOr($p:expr) $name:ident $($rest:tt)*) => {
        _parse_selector!(FirstOrAnd($p, ::select::predicate::Name(stringify!($name))) $($rest)*)
    };
    (Child($parent:expr) $name:ident $($rest:tt)*) => {
        _parse_selector!(ChildAnd($parent, ::select::predicate::Name(stringify!($name))) $($rest)*)
    };
    (ChildOr($p:expr, $parent:expr) $name:ident $($rest:tt)*) => {
        _parse_selector!(ChildOrAnd($p, $parent, ::select::predicate::Name(stringify!($name))) $($rest)*)
    };
    (Descendant($ancestor:expr) $name:ident $($rest:tt)*) => {
        _parse_selector!(DescendantAnd($ancestor, ::select::predicate::Name(stringify!($name))) $($rest)*)
    };
    (DescendantOr($p:expr, $ancestor:expr) $name:ident $($rest:tt)*) => {
        _parse_selector!(DescendantOrAnd($p, $ancestor, ::select::predicate::Name(stringify!($name))) $($rest)*)
    };

    (ChildAnd($parent:expr, $q:expr) . col - sm - 12 $($rest:tt)*) => {
        _parse_selector!(ChildAnd($parent, ::select::predicate::And($q, ::select::predicate::Class("col-sm-12"))) $($rest)*)
    };

    (First . $word:ident $($rest:tt)*) => {
        _parse_selector!(FirstReadingClass(stringify!($word)) $($rest)*)
    };
    (FirstOr($p:expr) . $word:ident $($rest:tt)*) => {
        _parse_selector!(FirstOrReadingClass($p, stringify!($word)) $($rest)*)
    };
    (FirstAnd($q:expr) . $word:ident $($rest:tt)*) => {
        _parse_selector!(FirstAndReadingClass($q, stringify!($word)) $($rest)*)
    };
    (FirstOrAnd($p:expr, $q:expr) . $word:ident $($rest:tt)*) => {
        _parse_selector!(FirstOrAndReadingClass($p, $q, stringify!($word)) $($rest)*)
    };
    (Child($parent:expr) . $word:ident $($rest:tt)*) => {
        _parse_selector!(ChildReadingClass($parent, stringify!($word)) $($rest)*)
    };
    (ChildOr($parent:expr, $p:expr) . $word:ident $($rest:tt)*) => {
        _parse_selector!(ChildOrReadingClass($parent, $p, stringify!($word)) $($rest)*)
    };
    (ChildAnd($parent:expr, $q:expr) . $word:ident $($rest:tt)*) => {
        _parse_selector!(ChildAndReadingClass($parent, $q, stringify!($word)) $($rest)*)
    };
    (ChildOrAnd($parent:expr, $p:expr, $q:expr) . $word:ident $($rest:tt)*) => {
        _parse_selector!(ChildOrAndReadingClass($parent, $p, $q, stringify!($word)) $($rest)*)
    };
    (Descendant($ancestor:expr) . $word:ident $($rest:tt)*) => {
        _parse_selector!(DescendantReadingClass($ancestor, stringify!($word)) $($rest)*)
    };
    (DescendantAnd($ancestor:expr, $q:expr) . $word:ident $($rest:tt)*) => {
        _parse_selector!(DescendantAndReadingClass($ancestor, $q, stringify!($word)) $($rest)*)
    };
    (DescendantOr($ancestor:expr, $p:expr) . $word:ident $($rest:tt)*) => {
        _parse_selector!(DescendantOrReadingClass($ancestor, $p, stringify!($word)) $($rest)*)
    };
    (DescendantOrAnd($ancestor:expr, $p:expr, $q:expr) . $word:ident $($rest:tt)*) => {
        _parse_selector!(DescendantOrAndReadingClass($ancestor, $p, $q, stringify!($word)) $($rest)*)
    };

    (FirstOr($p:expr) , $($rest:tt)+) => {
        _parse_selector!(FirstOr($p) $($rest)+)
    };
    (ChildOr($parent:expr, $p:expr) , $($rest:tt)+) => {
        _parse_selector!(ChildOr($parent, $p) $($rest)+)
    };
    (DescendantOr($ancestor:expr, $p:expr) , $($rest:tt)+) => {
        _parse_selector!(DescendantOr($ancestor, $p) $($rest)+)
    };

    (FirstOr($p:expr) > $($rest:tt)+) => {
        _parse_selector!(Child($p) $($rest)*)
    };
    (ChildOr($parent:expr, $p:expr) > $($rest:tt)+) => {
        _parse_selector!(Child(::select::predicate::Child($parent, $p)) $($rest)+)
    };
    (DescendantOr($ancestor:expr, $p:expr) > $($rest:tt)+) => {
        _parse_selector!(Child(::select::predicate::Descendant($ancestor, $p)) $($rest)+)
    };

    (FirstOr($p:expr) >> $($rest:tt)+) => {
        _parse_selector!(Descendant($p) $($rest)*)
    };
    (ChildOr($parent:expr, $p:expr) >> $($rest:tt)+) => {
        _parse_selector!(Descendant(::select::predicate::Child($parent, $p)) $($rest)+)
    };
    (DescendantOr($ancestor:expr, $p:expr) >> $($rest:tt)+) => {
        _parse_selector!(Descendant(::select::predicate::Descendant($ancestor, $p)) $($rest)+)
    };

    (FirstReadingId($s:expr) - $word:ident $($rest:tt)*) => {
        _parse_selector!(FirstReadingId(concat!($s, '-', stringify!($word))) $($rest)*)
    };
    (FirstReadingClass($s:expr) - $word:ident $($rest:tt)*) => {
        _parse_selector!(FirstReadingClass(concat!($s '-', stringify!($word))) $($rest)*)
    };
    (FirstOrReadingClass($p:expr, $s:expr) - $word:ident $($rest:tt)*) => {
        _parse_selector!(FirstOrReadingClass($p, concat!($s, '-', stringify!($word))) $($rest)*)
    };
    (FirstAndReadingClass($q:expr, $s:expr) - $word:ident $($rest:tt)*) => {
        _parse_selector!(FirstAndReadingClass($q, concat!($s, '-', stringify!($word))) $($rest)*)
    };
    (FirstOrAndReadingClass($p:expr, $q:expr, $s:expr) - $word:ident $($rest:tt)*) => {
        _parse_selector!(FirstOrAndReadingClass($p, $q, concat!($s, '-', stringify!($word))) $($rest)*)
    };
    (ChildReadingClass($parent:expr, $s:expr) - $word:ident $($rest:tt)*) => {
        _parse_selector!(ChildReadingClass($parent, concat!($s, '-', stringify!($word))) $($rest)*)
    };
    (ChildOrReadingClass($parent:expr, $p:expr, $s:expr) - $word:ident $($rest:tt)*) => {
        _parse_selector!(ChildOrReadingClass($parent, $p, concat!($s, '-', stringify!($word))) $($rest)*)
    };
    (ChildAndReadingClass($parent:expr, $q:expr, $s:expr) - $word:ident $($rest:tt)*) => {
        _parse_selector!(ChildAndReadingClass($parent, $q, concat!($s, '-', stringify!($word))) $($rest)*)
    };
    (ChildOrAndReadingClass($parent:expr, $p:expr, $q:expr, $s:expr) - $word:ident $($rest:tt)*) => {
        _parse_selector!(ChildOrAndReadingClass($parent, $p, $q, concat!($s, '-', stringify!($word))) $($rest)*)
    };
    (DescendantReadingClass($ancestor:expr, $s:expr) - $word:ident $($rest:tt)*) => {
        _parse_selector!(DescendantReadingClass($ancestor, concat!($s, '-', stringify!($word))) $($rest)*)
    };
    (DescendantOrReadingClass($ancestor:expr, $p:expr, $s:expr) - $word:ident $($rest:tt)*) => {
        _parse_selector!(DescendantAndReadingClass($ancestor, $p, concat!($s, '-', stringify!($word))) $($rest)*)
    };
    (DescendantAndReadingClass($ancestor:expr, $q:expr, $s:expr) - $word:ident $($rest:tt)*) => {
        _parse_selector!(DescendantAndReadingClass($ancestor, $q, concat!($s, '-', stringify!($word))) $($rest)*)
    };
    (DescendantAndReadingClass($ancestor:expr, $p:expr, $q:expr, $s:expr) - $word:ident $($rest:tt)*) => {
        _parse_selector!(DescendantOrAndReadingClass($ancestor, $p, $q, concat!($s, '-', stringify!($word))) $($rest)*)
    };

    (FirstReadingId($s:expr) $($rest:tt)*) => {
        _parse_selector!(FirstAnd(::select::predicate::Attr("id", $s)) $($rest)*)
    };
    (FirstReadingClass($s:expr) $($rest:tt)*) => {
        _parse_selector!(FirstAnd(::select::predicate::Class($s)) $($rest)*)
    };
    (FirstOrReadingClass($p:expr, $s:expr) $($rest:tt)*) => {
        _parse_selector!(FirstOrAnd($p, ::select::predicate::Class($s)) $($rest)*)
    };
    (FirstAndReadingClass($q:expr, $s:expr) $($rest:tt)*) => {
        _parse_selector!(FirstAnd(::select::predicate::And($q, ::select::predicate::Class($s))) $($rest)*)
    };
    (FirstOrAndReadingClass($p:expr, $q:expr, $s:expr) $($rest:tt)*) => {
        _parse_selector!(FirstOrAnd($p, ::select::predicate::And($q, ::select::predicate::Class($s))) $($rest)*)
    };
    (ChildReadingClass($parent:expr, $s:expr) $($rest:tt)*) => {
        _parse_selector!(ChildAnd($parent, ::select::predicate::Class($s)) $($rest)*)
    };
    (ChildOrReadingClass($parent:expr, $p:expr, $s:expr) $($rest:tt)*) => {
        _parse_selector!(ChildOrAnd($parent, $p, ::select::predicate::Class($s)) $($rest)*)
    };
    (ChildAndReadingClass($parent:expr, $q:expr, $s:expr) $($rest:tt)*) => {
        _parse_selector!(ChildAnd($parent, ::select::predicate::And($q, ::select::predicate::Class($s))) $($rest)*)
    };
    (ChildOrAndReadingClass($parent:expr, $p:expr, $q:expr, $s:expr) $($rest:tt)*) => {
        _parse_selector!(ChildOrAnd($parent, $p, ::select::predicate::And($q, ::select::predicate::Class($s))) $($rest)*)
    };
    (DescendantReadingClass($ancestor:expr, $s:expr) $($rest:tt)*) => {
        _parse_selector!(DescendantAnd($ancestor, ::select::predicate::Class($s)) $($rest)*)
    };
    (DescendantOrReadingClass($ancestor:expr, $p:expr, $s:expr) $($rest:tt)*) => {
        _parse_selector!(DescendantOrAnd($ancestor, $p, ::select::predicate::Class($s)) $($rest)*)
    };
    (DescendantAndReadingClass($ancestor:expr, $q:expr, $s:expr) $($rest:tt)*) => {
        _parse_selector!(DescendantAnd($ancestor, ::select::predicate::And($q, ::select::predicate::Class($s))) $($rest)*)
    };
    (DescendantOrReadingClass($ancestor:expr, $p:expr, $q:expr, $s:expr) $($rest:tt)*) => {
        _parse_selector!(DescendantOrAnd($ancestor, $p, ::select::predicate::And($q, ::select::predicate::Class($s))) $($rest)*)
    };

    (FirstAnd($q:expr) $($rest:tt)*) => {
        _parse_selector!(FirstOr($q) $($rest)*)
    };
    (FirstOrAnd($p:expr, $q:expr) $($rest:tt)*) => {
        _parse_selector!(FirstOr(::select::predicate::Or($p, $q)) $($rest)*)
    };
    (ChildAnd($parent:expr, $q:expr) $($rest:tt)*) => {
        _parse_selector!(ChildOr($parent, $q) $($rest)*)
    };
    (ChildOrAnd($parent:expr, $p:expr, $q:expr) $($rest:tt)*) => {
        _parse_selector!(ChildOr($parent, ::select::predicate::Or($p, $q)) $($rest)*)
    };
    (DescendantAnd($ancestor:expr, $q:expr) $($rest:tt)*) => {
        _parse_selector!(DescendantOr($ancestor, $q) $($rest)*)
    };
    (DescendantOrAnd($ancestor:expr, $p:expr, $q:expr) $($rest:tt)*) => {
        _parse_selector!(DescendantOr($ancestor, ::select::predicate::Or($p, $q)) $($rest)*)
    };
}
