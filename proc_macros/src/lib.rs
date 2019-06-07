#![recursion_limit = "128"]

extern crate proc_macro;

use heck::{KebabCase as _, SnakeCase as _};
use if_chain::if_chain;
use itertools::Itertools as _;
use proc_macro2::Span;
use quote::quote;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Attribute, Data, DeriveInput, Field, Fields,
    FieldsUnnamed, GenericArgument, ItemEnum, ItemStruct, Lit, Meta, MetaList, MetaNameValue,
    NestedMeta, PathArguments, PathSegment, Type, TypePath,
};

use std::str::FromStr;
use std::{convert, fmt};

macro_rules! try_syn {
    ($expr:expr) => {
        match $expr {
            Ok(expr) => expr,
            Err::<_, syn::Error>(err) => return err.to_compile_error().into(),
        }
    };
}

#[proc_macro_derive(DoubleFrom, attributes(double_from))]
pub fn derive_double_from(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemEnum);
    let name = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let variants = input.variants;

    let derivations = try_syn!(variants
        .iter()
        .filter_map(|variant| {
            let mut from_ty = None;
            for meta in variant.attrs.iter().flat_map(Attribute::parse_meta) {
                match &meta {
                    Meta::NameValue(MetaNameValue { ident, lit, .. }) if ident == "double_from" => {
                        match lit {
                            Lit::Str(s) => match s.parse::<Type>() {
                                Err(err) => return Some(Err(err)),
                                Ok(ty) => from_ty = Some(ty),
                            },
                            lit => return Some(Err(lit.error("expected str literal"))),
                        }
                    }
                    Meta::Word(ident) | Meta::List(MetaList { ident, .. })
                        if ident == "double_from" =>
                    {
                        return Some(Err(ident.error("invalid attr")));
                    }
                    _ => {}
                }
            }
            let from_ty = from_ty?;
            let variant_ident = &variant.ident;
            Some(match &variant.fields {
                Fields::Unnamed(FieldsUnnamed { unnamed, .. }) if unnamed.len() == 1 => Ok(quote!(
                    #[automatically_derived]
                    impl #impl_generics From<#from_ty> for #name #ty_generics
                        #where_clause
                    {
                        fn from(from: #from_ty) -> Self {
                            #name::#variant_ident(from.into())
                        }
                    }
                )),
                fields @ Fields::Unnamed(_) | fields @ Fields::Named(_) | fields @ Fields::Unit => {
                    Err(fields.error("expected unit field"))
                }
            })
        })
        .collect::<syn::Result<Vec<_>>>());

    quote!(#(#derivations)*).into()
}

#[proc_macro_derive(FailPair)]
pub fn derive_fail_pair(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemStruct);
    let error = input.ident;

    if !input.generics.params.is_empty() {
        return input.generics.compile_error("generics must be empty");
    }

    let error_kind = try_syn!(extract_error_kind(&input.fields));

    quote!(
        #[automatically_derived]
        impl From<#error_kind> for #error {
            fn from(from: #error_kind) -> Self {
                #error(::failure::Context::new(from))
            }
        }

        #[automatically_derived]
        impl From<::failure::Context<#error_kind>> for #error {
            fn from(from: ::failure::Context<#error_kind>) -> Self {
                #error(from)
            }
        }

        #[automatically_derived]
        impl ::std::ops::Deref for #error {
            type Target = ::failure::Context<#error_kind>;

            fn deref(&self) -> &::failure::Context<#error_kind> {
                &self.0
            }
        }

        #[automatically_derived]
        impl ::std::fmt::Display for #error {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                ::std::fmt::Display::fmt(&self.0, f)
            }
        }

        #[automatically_derived]
        impl ::failure::Fail for #error {
            fn cause(&self) -> Option<&dyn (::failure::Fail)> {
                ::failure::Fail::cause(&self.0)
            }

            fn backtrace(&self) -> Option<&::failure::Backtrace> {
                ::failure::Fail::backtrace(&self.0)
            }
        }
    )
    .into()
}

#[proc_macro_derive(PartialFailPair)]
pub fn derive_partial_fail_pair(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemEnum);
    let error = input.ident;

    if !input.generics.params.is_empty() {
        return input.generics.compile_error("generics must be empty");
    }

    let variants = input.variants;

    let variant = try_syn!(variants
        .iter()
        .find(|v| v.ident == "Context")
        .ok_or_else(|| variants.error("no `Context` field")));

    if variant.discriminant.is_some() {
        return variant.compile_error("must not have discriminant");
    }

    let error_kind = try_syn!(extract_error_kind(&variant.fields));

    let pairs = try_syn!(variants
        .iter()
        .map(|variant| {
            if let Some((_, expr)) = &variant.discriminant {
                return Err(expr.error("no"));
            }
            let unnamed = match &variant.fields {
                Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => unnamed,
                _ => return Err(variant.fields.error("expected 1 unnamed field")),
            };
            if unnamed.len() != 1 {
                return Err(unnamed.error("expected 1 unnamed field"));
            }
            match &unnamed.iter().next().unwrap().ty {
                Type::Path(ty) => Ok((&variant.ident, &ty.path)),
                ty => Err(ty.error("expected path")),
            }
        })
        .collect::<syn::Result<Vec<_>>>());

    let from_derivations = pairs.iter().map(|(ident, path)| {
        quote!(
            #[automatically_derived]
            impl From<#path> for #error {
                fn from(from: #path) -> Self {
                    #error::#ident(from)
                }
            }
        )
    });

    let display_arms = pairs
        .iter()
        .map(|(ident, _)| quote!(#error::#ident(x) => ::std::fmt::Display::fmt(x, f),));

    let cause_arms = pairs
        .iter()
        .map(|(ident, _)| quote!(#error::#ident(x) => ::failure::Fail::cause(x),));

    let backtrace_arms = pairs
        .iter()
        .map(|(ident, _)| quote!(#error::#ident(x) => ::failure::Fail::backtrace(x),));

    quote!(
        #[automatically_derived]
        impl From<#error_kind> for #error {
            fn from(from: #error_kind) -> Self {
                #error::Context(::failure::Context::new(from))
            }
        }

        #(#from_derivations)*

        #[automatically_derived]
        impl ::std::fmt::Display for #error {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                match self {
                    #(#display_arms)*
                }
            }
        }

        #[automatically_derived]
        impl ::failure::Fail for #error {
            fn cause(&self) -> Option<&dyn (::failure::Fail)> {
                match self {
                    #(#cause_arms)*
                }
            }

            fn backtrace(&self) -> Option<&::failure::Backtrace> {
                match self {
                    #(#backtrace_arms)*
                }
            }
        }
    )
    .into()
}

fn extract_error_kind(fields: &Fields) -> syn::Result<&Type> {
    if_chain! {
        if let Fields::Unnamed(FieldsUnnamed { unnamed, .. }) = fields;
        if unnamed.len() == 1;
        let Field { ty, .. } = unnamed.iter().next().unwrap();
        if let Type::Path(TypePath { path, .. }) = ty;
        if let Some(PathSegment { ident, arguments }) = path.segments.iter().last();
        if *ident == "Context";
        if let PathArguments::AngleBracketed(args) = arguments;
        if let AngleBracketedGenericArguments { args, .. } = args;
        if args.len() == 1;
        if let GenericArgument::Type(error_kind) = args.iter().next().unwrap();
        then {
            Ok(error_kind)
        } else {
            Err(fields.error("expected `..::Context<$ty>`"))
        }
    }
}

#[proc_macro_derive(ArgEnum, attributes(arg_enum))]
pub fn derive_arg_enum(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    #[derive(Clone, Copy, Default)]
    struct EnumAttrs {
        case: Option<CaseSensitivity>,
        rename_all: Option<CaseConversion>,
    }

    #[derive(Clone, Copy, PartialEq)]
    enum CaseSensitivity {
        Insensitive,
        Sensitive,
    }

    impl Default for CaseSensitivity {
        fn default() -> Self {
            CaseSensitivity::Insensitive
        }
    }

    impl FromStr for CaseSensitivity {
        type Err = &'static str;

        fn from_str(s: &str) -> std::result::Result<Self, &'static str> {
            match s {
                s if s.eq_ignore_ascii_case("insensitive") => Ok(CaseSensitivity::Insensitive),
                s if s.eq_ignore_ascii_case("sensitive") => Ok(CaseSensitivity::Sensitive),
                _ => Err(r#"valid values: ["insensitive", "sensitive"]"#),
            }
        }
    }

    let input = parse_macro_input!(input as ItemEnum);

    if let Some(span) = input
        .variants
        .iter()
        .flat_map(|v| &v.attrs)
        .flat_map(Attribute::parse_meta)
        .flat_map(|m| find_attr(&m, "arg_enum"))
        .next()
    {
        return syn::Error::new(span, "not allowed here")
            .to_compile_error()
            .into();
    }

    let enum_attrs = try_syn!(input
        .attrs
        .iter()
        .flat_map(Attribute::parse_meta)
        .flat_map(|meta| match &meta {
            Meta::Word(ident) | Meta::NameValue(MetaNameValue { ident, .. })
                if ident == "arg_enum" =>
            {
                Some(Err(meta.error("expected `#[arg_enum(_)]`")))
            }
            Meta::List(MetaList { ident, nested, .. }) if ident == "arg_enum" => {
                let mut enum_attrs = EnumAttrs::default();
                for nested in nested {
                    match nested {
                        NestedMeta::Meta(Meta::NameValue(MetaNameValue { ident, lit, .. }))
                            if ident == "case" =>
                        {
                            if enum_attrs.case.is_some() {
                                return Some(Err(ident.error("multiple `case`s")));
                            }
                            let s = match lit {
                                Lit::Str(s) => s,
                                lit => return Some(Err(lit.error("expected string"))),
                            };
                            match s.value().parse() {
                                Err(e) => return Some(Err(s.error(e))),
                                Ok(case) => enum_attrs.case = Some(case),
                            }
                        }
                        NestedMeta::Meta(Meta::NameValue(MetaNameValue { ident, lit, .. }))
                            if ident == "rename_all" =>
                        {
                            if enum_attrs.rename_all.is_some() {
                                return Some(Err(ident.error("multiple `rename_all`s")));
                            }
                            let s = match lit {
                                Lit::Str(s) => s,
                                lit => return Some(Err(lit.error("expected string"))),
                            };
                            match s.value().parse() {
                                Err(e) => return Some(Err(s.error(e))),
                                Ok(rename_all) => enum_attrs.rename_all = Some(rename_all),
                            }
                        }
                        nested => {
                            return Some(Err(
                                nested.error(r#"expected `case = ".."` or `rename_all = ".."`"#)
                            ))
                        }
                    }
                }
                Some(Ok(enum_attrs))
            }
            _ => None,
        })
        .collect::<syn::Result<Vec<_>>>());

    let enum_attrs = if enum_attrs.len() > 1 {
        return input.compile_error("multiple `arg_enum`s");
    } else {
        convert::identity(enum_attrs).pop().unwrap_or_default()
    };

    let variant_idents = try_syn!(input
        .variants
        .iter()
        .map(|variant| {
            if variant.fields == Fields::Unit {
                Ok(&variant.ident)
            } else {
                Err(variant.fields.error("all fields must be unit"))
            }
        })
        .collect::<syn::Result<Vec<_>>>());

    let variant_ident_strs = variant_idents
        .iter()
        .map(ToString::to_string)
        .map(|s| match enum_attrs.rename_all {
            None => s,
            Some(CaseConversion::Lower) => s.to_lowercase(),
            Some(CaseConversion::Snake) => s.to_snake_case(),
            Some(CaseConversion::Kebab) => s.to_kebab_case(),
        })
        .collect::<Vec<_>>();

    let ItemEnum { ident, vis, .. } = &input;

    let variants_elements = &variant_ident_strs;
    let variants_len = input.variants.len();

    let from_str_ok_arms =
        variant_idents
            .iter()
            .zip_eq(&variant_ident_strs)
            .map(|(v_ident, v_ident_s)| {
                let lhs = match enum_attrs.case.unwrap_or_default() {
                    CaseSensitivity::Insensitive => {
                        quote!(s if str::eq_ignore_ascii_case(s, #v_ident_s))
                    }
                    CaseSensitivity::Sensitive => quote!(#v_ident_s),
                };
                quote!(#lhs => ::std::result::Result::Ok(#ident::#v_ident),)
            });

    let from_str_error = format!(
        "valid values: {:?} (case {})",
        variant_ident_strs,
        match enum_attrs.case.unwrap_or_default() {
            CaseSensitivity::Insensitive => "insensitive",
            CaseSensitivity::Sensitive => "sensitive",
        }
    );

    let display_arms = variant_idents
        .iter()
        .zip_eq(&variant_ident_strs)
        .map(|(i, s)| quote!(#ident::#i => ::std::fmt::Display::fmt(#s, fmt),));

    quote!(
        impl #ident {
            #vis fn variants() -> [&'static str; #variants_len] {
                [
                    #(#variants_elements, )*
                ]
            }
        }

        impl ::std::str::FromStr for #ident {
            type Err = &'static str;

            fn from_str(s: &str) -> ::std::result::Result<Self, &'static str> {
                match s {
                    #(#from_str_ok_arms)*
                    _ => ::std::result::Result::Err(#from_str_error),
                }
            }
        }

        impl ::std::fmt::Display for #ident {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                match self {
                    #(#display_arms)*
                }
            }
        }
    )
    .into()
}

#[proc_macro_derive(SerializeAsString)]
pub fn derive_serialize_as_string(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    if !input.generics.params.is_empty() {
        return input.generics.compile_error("generics must be empty");
    }

    let ident = input.ident;

    quote!(
        impl ::serde::Serialize for #ident {
            fn serialize<S: ::serde::Serializer>(
                &self,
                serializer: S,
            ) -> ::std::result::Result<S::Ok, S::Error> {
                let s = <Self as ::std::string::ToString>::to_string(self);
                ::serde::Serializer::collect_str(serializer, &s)
            }
        }
    )
    .into()
}

#[proc_macro_derive(DeserializeAsString, attributes(deserialize_as_string))]
pub fn derive_deserialize_as_string(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let DeriveInput {
        attrs,
        ident,
        generics,
        data,
        ..
    } = parse_macro_input!(input as DeriveInput);

    let field_variant_attrs = match &data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => fields.named.iter().flat_map(|f| &f.attrs).collect(),
            Fields::Unnamed(fields) => fields.unnamed.iter().flat_map(|f| &f.attrs).collect(),
            Fields::Unit => vec![],
        },
        Data::Enum(data) => data.variants.iter().flat_map(|v| &v.attrs).collect(),
        _ => return ident.compile_error("expected struct or enum"),
    };

    if !generics.params.is_empty() {
        return generics.compile_error("generics must be empty");
    }

    if let Some(span) = field_variant_attrs
        .into_iter()
        .flat_map(Attribute::parse_meta)
        .flat_map(|m| find_attr(&m, "deserialize_as_string"))
        .next()
    {
        return syn::Error::new(span, "not allowed here")
            .to_compile_error()
            .into();
    }

    let map_err_into_serde_style = try_syn!(attrs
        .iter()
        .flat_map(Attribute::parse_meta)
        .flat_map(|meta| match &meta {
            Meta::List(MetaList { ident, nested, .. }) if ident == "deserialize_as_string" => {
                Some(match nested.len() {
                    0 => Ok(false),
                    1 => match nested.iter().next().unwrap() {
                        NestedMeta::Meta(Meta::Word(ident))
                            if ident == "map_err_into_serde_style" =>
                        {
                            Ok(true)
                        }
                        nested => Err(nested.error("expected `map_err_into_serde_style`")),
                    },
                    _ => Err(nested.error("expected 0 or 1 element")),
                })
            }
            Meta::Word(ident) | Meta::NameValue(MetaNameValue { ident, .. })
                if ident == "deserialize_as_string" =>
            {
                Some(Err(meta.error("expected list")))
            }
            _ => None,
        })
        .collect::<syn::Result<Vec<_>>>());

    let map_err_into_serde_style = if map_err_into_serde_style.len() > 1 {
        return ident.compile_error("multiple `deserialize_as_string`s");
    } else {
        convert::identity(map_err_into_serde_style).pop()
    };

    let map_err_into_serde_style = if map_err_into_serde_style.unwrap_or(false) {
        static ERR_MSG: &str = "must be unit enum when `map_err_into_serde_style` specified";

        let variants = try_syn!(match &data {
            Data::Struct(data) => Err(data.struct_token.error(ERR_MSG)),
            Data::Union(data) => Err(data.union_token.error(ERR_MSG)),
            Data::Enum(data) => data
                .variants
                .iter()
                .map(|variant| match &variant.fields {
                    Fields::Unit => {
                        let variant_ident = &variant.ident;
                        Ok(quote!(#ident::#variant_ident))
                    }
                    fields => Err(fields.error(ERR_MSG)),
                })
                .collect::<syn::Result<Vec<_>>>(),
        });

        let mut fmt = "unknown variant `{}`, expected ".to_owned();
        match variants.len() {
            1 => fmt += "{}",
            2 => fmt += "{} or {}",
            n => {
                fmt += "one of ";
                fmt += &(0..n).map(|_| "`{}`").join(", ");
            }
        }

        Some(quote! {
            let r = ::std::result::Result::map_err(r, |_| ::std::format!(#fmt, s, #(#variants, )*));
        })
    } else {
        None
    };

    quote!(
        impl<'de> ::serde::Deserialize<'de> for #ident {
            fn deserialize<D: ::serde::Deserializer<'de>>(
                deserializer: D,
            ) -> ::std::result::Result<Self, D::Error> {
                let s = <::std::string::String as ::serde::Deserialize>::deserialize(deserializer)?;
                let r = <Self as ::std::str::FromStr>::from_str(&s);
                #map_err_into_serde_style
                ::std::result::Result::map_err(r, ::serde::de::Error::custom)
            }
        }
    )
    .into()
}

fn find_attr(meta: &Meta, attr: &'static str) -> Option<Span> {
    match meta {
        Meta::Word(ident) | Meta::NameValue(MetaNameValue { ident, .. }) => {
            if ident == attr {
                Some(ident.span())
            } else {
                None
            }
        }
        Meta::List(list) => list
            .nested
            .iter()
            .flat_map(|m| match m {
                NestedMeta::Meta(m) => find_attr(m, attr),
                NestedMeta::Literal(Lit::Str(s)) if s.value() == attr => Some(s.span()),
                NestedMeta::Literal(_) => None,
            })
            .next(),
    }
}

trait SpannedExt {
    fn error(&self, mes: impl fmt::Display) -> syn::Error;

    fn compile_error(&self, mes: impl fmt::Display) -> proc_macro::TokenStream {
        self.error(mes).to_compile_error().into()
    }
}

impl<T: Spanned> SpannedExt for T {
    fn error(&self, mes: impl fmt::Display) -> syn::Error {
        syn::Error::new(self.span(), mes)
    }
}

#[derive(Clone, Copy, PartialEq)]
enum CaseConversion {
    Lower,
    Snake,
    Kebab,
}

impl FromStr for CaseConversion {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, &'static str> {
        match s {
            "lowercase" => Ok(CaseConversion::Lower),
            "snake_case" => Ok(CaseConversion::Snake),
            "kebab-case" => Ok(CaseConversion::Kebab),
            _ => Err(r#"valid values: ["lowercase", snake_case", "kebab-case"]"#),
        }
    }
}
