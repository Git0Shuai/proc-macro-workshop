use proc_macro2::{TokenStream, TokenTree};
use std::iter::FromIterator;
use syn::{parse::Parse, spanned::Spanned, token, Expr, ExprLit, ExprRange, Ident, Lit, Token};

struct Seq {
    ident: Ident,
    #[allow(dead_code)]
    in_token: Token![in],
    range_expr: ExprRange,
    #[allow(dead_code)]
    brace: token::Brace,
    content: TokenStream,
}

impl Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Seq {
            ident: input.parse()?,
            in_token: input.parse()?,
            range_expr: input.parse()?,
            brace: syn::braced!(content in input),
            content: content.parse()?,
        })
    }
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let seq = syn::parse_macro_input!(input as Seq);

    let loop_var = &seq.ident;
    let range_expr = &seq.range_expr;

    let from = if let Some(from_expr) = &range_expr.from {
        if let Expr::Lit(ExprLit {
            lit: Lit::Int(ref lit_int),
            ..
        }) = **from_expr
        {
            match lit_int.base10_parse() {
                Ok(v) => v,
                Err(e) => {
                    return e.into_compile_error().into();
                }
            }
        } else {
            return syn::Error::new((**from_expr).span(), "expect range start int literal")
                .into_compile_error()
                .into();
        }
    } else {
        0
    };

    let mut to: i32 = if let Some(to_expr) = &range_expr.to {
        if let Expr::Lit(ExprLit {
            lit: Lit::Int(ref lit_int),
            ..
        }) = **to_expr
        {
            match lit_int.base10_parse() {
                Ok(v) => v,
                Err(e) => {
                    return e.into_compile_error().into();
                }
            }
        } else {
            return syn::Error::new((**to_expr).span(), "expect range end int literal")
                .into_compile_error()
                .into();
        }
    } else {
        return syn::Error::new(range_expr.span(), "expect an int literal end")
            .into_compile_error()
            .into();
    };

    if let syn::RangeLimits::HalfOpen(_) = &range_expr.limits {
        to -= 1;
    }

    fn replace(ts: TokenStream, s: &str, i: i32) -> TokenStream {
        TokenStream::from_iter(ts.into_iter().map(|tt| match tt {
            TokenTree::Group(group) => TokenTree::Group(proc_macro2::Group::new(
                group.delimiter(),
                replace(group.stream(), s, i),
            )),
            TokenTree::Ident(ident) if ident == s => {
                TokenTree::Literal(proc_macro2::Literal::i32_unsuffixed(i))
            }
            _ => tt,
        }))
    }

    let output = TokenStream::from_iter(
        (from..=to).map(|v| replace((&seq.content).clone(), &loop_var.to_string(), v)),
    );

    output.into()
}

