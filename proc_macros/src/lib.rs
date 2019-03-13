#![recursion_limit = "256"]

extern crate proc_macro;

use if_chain::if_chain;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Attribute, Field, Fields, FieldsUnnamed,
    GenericArgument, Ident, ItemEnum, ItemStruct, Lit, LitStr, Meta, MetaList, MetaNameValue, Path,
    PathArguments, PathSegment, Token, Type, TypePath,
};

use std::mem;

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

#[proc_macro]
pub fn def_gen_predicate(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    struct LitStrWithComma(LitStr);

    impl Parse for LitStrWithComma {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let litstr = input.parse()?;
            input.parse::<Option<Token![,]>>()?;
            Ok(LitStrWithComma(litstr))
        }
    }

    let input = parse_macro_input!(input as LitStrWithComma);
    let pred = parse_selector(&input.0.value()).unwrap_or_else(|e| panic!("{}", e));
    quote!(
        #[inline]
        fn gen() -> impl ::select::predicate::Predicate {
            #pred
        }
    )
    .into()
}

fn parse_selector(input: &str) -> std::result::Result<impl ToTokens, String> {
    use combine::char::{char, space, spaces, string};
    use combine::stream::state::{Positioner, State};
    use combine::stream::Resetable;
    use combine::{attempt, choice, easy, eof, many1, satisfy, Parser};

    #[derive(Debug)]
    enum Token {
        Name(String),
        Id(String),
        Class(String),
        AttrEq(String, String),
        Or,
        Child,
        Descendant,
    }

    #[derive(Default)]
    struct OnelinePosition {
        pos: usize,
    }

    impl Positioner<char> for OnelinePosition {
        type Position = usize;

        fn position(&self) -> usize {
            self.pos
        }

        fn update(&mut self, _: &char) {
            self.pos += 1;
        }
    }

    impl Resetable for OnelinePosition {
        type Checkpoint = usize;

        fn checkpoint(&self) -> usize {
            self.pos
        }

        fn reset(&mut self, checkpoint: usize) {
            self.pos = checkpoint;
        }
    }

    fn ident<'a>(
    ) -> impl Parser<Input = easy::Stream<State<&'a str, OnelinePosition>>, Output = String> {
        many1(satisfy(|c| {
            'a' <= c && c <= 'z'
                || 'A' <= c && c <= 'Z'
                || '0' <= c && c <= '9'
                || c == '-'
                || c == '_'
        }))
    }

    fn op<'a>(c: char) -> impl Parser<Input = easy::Stream<State<&'a str, OnelinePosition>>> {
        spaces().with(char(c)).skip(spaces())
    }

    let name = ident().map(Token::Name);
    let id = char('#').with(ident()).map(Token::Id);
    let class = char('.').with(ident()).map(Token::Class);
    let attr_eq = char('[')
        .skip(spaces())
        .with(ident())
        .skip(spaces())
        .skip(char('='))
        .skip(spaces())
        .skip(char('"'))
        .and(many1(satisfy(|c: char| {
            !(c.is_whitespace() || c == '"' || c == '\\')
        })))
        .skip(spaces())
        .skip(string("\"]"))
        .map(|(k, v)| Token::AttrEq(k, v));
    let or = op(',').map(|_| Token::Or);
    let child = op('>').map(|_| Token::Child);
    let descendant = many1::<String, _>(space()).map(|_| Token::Descendant);

    let tokens = many1::<Vec<_>, _>(choice((
        name,
        id,
        class,
        attr_eq,
        attempt(or),
        attempt(child),
        descendant,
    )))
    .skip(eof())
    .easy_parse(State::with_positioner(input, OnelinePosition::default()))
    .map(|(ts, _)| ts)
    .map_err(|e| format!("Failed to lex {:?}\n{}", input, e))?;

    enum Ancestor<S: ToTokens> {
        None,
        Parent(S),
        Ancestor(S),
    }

    let (mut and, mut or, mut ancestor) = (None, None, Ancestor::None);

    for token in tokens {
        match token {
            Token::Name(name) => {
                let name = LitStr::new(&name, proc_macro2::Span::call_site());
                let new = quote!(::select::predicate::Name(#name));
                if and.is_none() {
                    and = Some(new);
                } else {
                    and = and.map(|p| quote!(::select::predicate::And(#p, #new)));
                }
            }
            Token::Id(id) => {
                let id = LitStr::new(&id, proc_macro2::Span::call_site());
                let new = quote!(::select::predicate::Attr("id", #id));
                if and.is_none() {
                    and = Some(new);
                } else {
                    and = and.map(|p| quote!(::select::predicate::And(#p, #new)));
                }
            }
            Token::Class(class) => {
                let class = LitStr::new(&class, proc_macro2::Span::call_site());
                let new = quote!(::select::predicate::Class(#class));
                if and.is_none() {
                    and = Some(new);
                } else {
                    and = and.map(|p| quote!(::select::predicate::And(#p, #new)));
                }
            }
            Token::AttrEq(key, value) => {
                let key = LitStr::new(&key, proc_macro2::Span::call_site());
                let value = LitStr::new(&value, proc_macro2::Span::call_site());
                let new = quote!(::select::predicate::Attr(#key, #value));
                if and.is_none() {
                    and = Some(new);
                } else {
                    and = and.map(|p| quote!(::select::predicate::And(#p, #new)));
                }
            }
            Token::Or if and.is_none() => {
                return Err("Unexpected ','".to_owned());
            }
            Token::Or if or.is_none() => {
                or = Some(and.take().unwrap());
            }
            Token::Or => {
                let and = and.take().unwrap();
                or = or.map(|p| quote!(::select::predicate::Or(#p, #and)));
            }
            Token::Child if and.is_none() && or.is_none() => {
                return Err("Unexpected '>'".to_owned());
            }
            Token::Child if and.is_some() && or.is_none() => {
                let current = and.take().unwrap();
                ancestor = Ancestor::Parent(match mem::replace(&mut ancestor, Ancestor::None) {
                    Ancestor::None => current,
                    Ancestor::Parent(p) => quote!(::select::predicate::Child(#p, #current)),
                    Ancestor::Ancestor(p) => quote!(::select::predicate::Descendant(#p, #current)),
                });
            }
            Token::Child if and.is_none() && or.is_some() => {
                let current = or.take().unwrap();
                ancestor = Ancestor::Parent(match mem::replace(&mut ancestor, Ancestor::None) {
                    Ancestor::None => current,
                    Ancestor::Parent(p) => quote!(::select::predicate::Child(#p, #current)),
                    Ancestor::Ancestor(p) => quote!(::select::predicate::Descendant(#p, #current)),
                });
            }
            Token::Child => {
                let and = and.take().unwrap();
                let or = or.take().unwrap();
                let current = quote!(::select::predicate::Or(#and, #or));
                ancestor = Ancestor::Parent(match mem::replace(&mut ancestor, Ancestor::None) {
                    Ancestor::None => current,
                    Ancestor::Parent(p) => quote!(::select::predicate::Child(#p, #current)),
                    Ancestor::Ancestor(p) => quote!(::select::predicate::Descendant(#p, #current)),
                });
            }
            Token::Descendant if and.is_none() && or.is_none() => {
                return Err("Unexpected space".to_owned());
            }
            Token::Descendant if and.is_some() && or.is_none() => {
                let current = and.take().unwrap();
                ancestor = Ancestor::Ancestor(match mem::replace(&mut ancestor, Ancestor::None) {
                    Ancestor::None => current,
                    Ancestor::Parent(p) => quote!(::select::predicate::Child(#p, #current)),
                    Ancestor::Ancestor(p) => quote!(::select::predicate::Descendant(#p, #current)),
                });
            }
            Token::Descendant if and.is_none() && or.is_some() => {
                let current = or.take().unwrap();
                ancestor = Ancestor::Ancestor(match mem::replace(&mut ancestor, Ancestor::None) {
                    Ancestor::None => current,
                    Ancestor::Parent(p) => quote!(::select::predicate::Child(#p, #current)),
                    Ancestor::Ancestor(p) => quote!(::select::predicate::Descendant(#p, #current)),
                });
            }
            Token::Descendant => {
                let and = and.take().unwrap();
                let or = or.take().unwrap();
                let current = quote!(::select::predicate::Or(#and, #or));
                ancestor = Ancestor::Ancestor(match mem::replace(&mut ancestor, Ancestor::None) {
                    Ancestor::None => current,
                    Ancestor::Parent(p) => quote!(::select::predicate::Child(#p, #current)),
                    Ancestor::Ancestor(p) => quote!(::select::predicate::Descendant(#p, #current)),
                });
            }
        }
    }

    let edge = match (and, or) {
        (None, None) => return Err("Unexpected end".to_owned()),
        (Some(p), None) | (None, Some(p)) => p,
        (Some(p1), Some(p2)) => quote!(::select::predicate::Or(#p1, #p2)),
    };
    Ok(match ancestor {
        Ancestor::None => edge,
        Ancestor::Parent(p) => quote!(::select::predicate::Child(#p, #edge)),
        Ancestor::Ancestor(p) => quote!(::select::predicate::Descendant(#p, #edge)),
    })
}
