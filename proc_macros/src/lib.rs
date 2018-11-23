#![recursion_limit = "512"]

extern crate if_chain;
extern crate proc_macro;
extern crate quote;
extern crate syn;

use if_chain::if_chain;
use quote::quote;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Attribute, Field, Fields, FieldsUnnamed,
    GenericArgument, Ident, ItemEnum, ItemStruct, Lit, Meta, MetaList, MetaNameValue, Path,
    PathArguments, PathSegment, Type, TypePath,
};

#[proc_macro_derive(DoubleFrom, attributes(double_from))]
pub fn derive_double_from(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemEnum);
    let name = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let variants = input.variants;
    let derivations = variants.iter().filter_map(|variant| {
        let mut from_ty = None;
        for meta in variant.attrs.iter().flat_map(Attribute::parse_meta) {
            match &meta {
                Meta::NameValue(MetaNameValue { ident, lit, .. }) if ident == "double_from" => {
                    match lit {
                        Lit::Str(s) => {
                            let ty = s.parse::<Type>().unwrap_or_else(|e| panic!("{}", e));
                            from_ty = Some(ty);
                        }
                        _ => panic!("expected str literal"),
                    }
                }
                Meta::Word(ident) | Meta::List(MetaList { ident, .. })
                    if ident == "double_from" =>
                {
                    panic!("invalid attr")
                }
                _ => {}
            }
        }
        let variant_ident = &variant.ident;
        let from_ty = from_ty?;
        match &variant.fields {
            Fields::Unnamed(FieldsUnnamed { unnamed, .. }) if unnamed.len() == 1 => Some(quote!(
                        #[automatically_derived]
                        impl #impl_generics From<#from_ty> for #name #ty_generics
                            #where_clause
                        {
                            fn from(from: #from_ty) -> Self {
                                #name::#variant_ident(from.into())
                            }
                        }
                    )),
            Fields::Unnamed(_) => panic!("tuples not supported"),
            Fields::Named(_) => panic!("named fields not supported"),
            Fields::Unit => panic!("unit fields not supported"),
        }
    });
    quote!(#(#derivations)*).into()
}

#[proc_macro_derive(FailPair)]
pub fn derive_fail_pair(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemStruct);
    let error = input.ident;
    if_chain! {
        if input.generics.params.is_empty();
        if let Some(error_kind) = extract_error_kind(&input.fields);
        then {
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

                    fn deref(&self) -> &Self::Target {
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
            ).into()
        } else {
            panic!("invalid")
        }
    }
}

#[proc_macro_derive(PartialFailPair)]
pub fn derive_partial_fail_pair(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemEnum);
    let error = input.ident;
    if_chain! {
        if input.generics.params.is_empty();
        let variants = &input.variants;
        if let Some(variant) = variants.iter().find(|v| v.ident == "Context");
        if variant.discriminant.is_none();
        if let Some(error_kind) = extract_error_kind(&variant.fields);
        let pairs = variants
            .iter()
            .filter(|v| v.discriminant.is_none())
            .filter_map(|v| match &v.fields {
                Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => Some((&v.ident, unnamed)),
                _ => None,
            }).filter(|(_, unnamed)| unnamed.len() == 1)
            .filter_map(
                |(ident, unnamed)| match &unnamed.iter().next().unwrap().ty {
                    Type::Path(ty) => Some((ident, ty)),
                    _ => None,
                },
            ).filter(|(_, ty)| ty.qself.is_none())
            .map(|(ident, ty)| (ident, &ty.path))
            .collect::<Vec<(&Ident, &Path)>>();
        if pairs.len() == variants.len();
        let from_derivations = pairs
            .iter()
            .map(|(ident, path)| quote!(
                #[automatically_derived]
                impl From<#path> for #error {
                    fn from(from: #path) -> Self {
                        #error::#ident(from)
                    }
                }
            ));
        let display_arms = pairs
            .iter()
            .map(|(ident, _)| quote!(#error::#ident(x) => ::std::fmt::Display::fmt(x, f),));
        let cause_arms = pairs
            .iter()
            .map(|(ident, _)| quote!(#error::#ident(x) => ::failure::Fail::cause(x),));
        let backtrace_arms = pairs
            .iter()
            .map(|(ident, _)| quote!(#error::#ident(x) => ::failure::Fail::backtrace(x),));
        then {
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
            ).into()
        } else {
            panic!("invalid")
        }
    }
}

fn extract_error_kind(fields: &Fields) -> Option<&Type> {
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
            Some(error_kind)
        } else {
            None
        }
    }
}
