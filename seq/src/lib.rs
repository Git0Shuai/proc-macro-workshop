use proc_macro2::{Delimiter, TokenStream, TokenTree};
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

    fn replace_loop_var(ts: TokenStream, s: &str, i: i32) -> TokenStream {
        let tt_vec = ts.into_iter().collect::<Vec<TokenTree>>();
        let length = tt_vec.len();
        let mut result = Vec::with_capacity(length);
        let mut idx = 0;
        while idx < length {
            let tt = &tt_vec[idx];
            let r = match tt {
                TokenTree::Group(group) => {
                    idx += 1;
                    TokenTree::Group(proc_macro2::Group::new(
                        group.delimiter(),
                        replace_loop_var(group.stream(), s, i),
                    ))
                }
                TokenTree::Ident(ident) if ident == s => {
                    idx += 1;
                    TokenTree::Literal(proc_macro2::Literal::i32_unsuffixed(i))
                }
                TokenTree::Ident(ident) if idx < length - 2 => {
                    match (&tt_vec[idx + 1], &tt_vec[idx + 2]) {
                        (TokenTree::Punct(punct_1), TokenTree::Ident(ident_2))
                            if punct_1.as_char() == '#' && ident_2 == s =>
                        {
                            let mut ident = quote::format_ident!("{}{}", ident, i as u32);
                            idx += 3;
                            if idx < length - 1 {
                                match (&tt_vec[idx], &tt_vec[idx + 1]) {
                                    (TokenTree::Punct(punct), TokenTree::Ident(ident_1))
                                        if punct.as_char() == '#' =>
                                    {
                                        ident = quote::format_ident!("{}{}", ident, ident_1);
                                        idx += 2;
                                    }
                                    _ => {}
                                };
                            }
                            proc_macro2::TokenTree::Ident(ident)
                        }
                        _ => {
                            idx += 1;
                            tt.clone()
                        }
                    }
                }
                _ => {
                    idx += 1;
                    tt.clone()
                }
            };
            result.push(r);
        }

        TokenStream::from_iter(result.into_iter())
    }

    fn replace_section(
        ts: TokenStream,
        from: i32,
        to: i32,
        loop_var: &Ident,
    ) -> (TokenStream, bool) {
        let tt_vec = ts.into_iter().collect::<Vec<TokenTree>>();
        let length = tt_vec.len();
        let mut result = Vec::with_capacity(length);
        let mut idx = 0;
        let mut replaced = false;

        while idx < length {
            let tt = &tt_vec[idx];

            if let TokenTree::Group(g) = tt {
                let ts = replace_section(g.stream(), from, to, loop_var);
                result.push(TokenTree::Group(proc_macro2::Group::new(
                    g.delimiter(),
                    ts.0,
                )));
                replaced = replaced || ts.1;
                idx += 1;
                continue;
            }

            if length >= 3 && idx <= length - 3 {
                match (tt, &tt_vec[idx + 1], &tt_vec[idx + 2]) {
                    (TokenTree::Punct(tt), TokenTree::Group(g), TokenTree::Punct(p))
                        if tt.as_char() == '#'
                            && g.delimiter() == Delimiter::Parenthesis
                            && p.as_char() == '*' =>
                    {
                        for ts in (from..=to)
                            .map(|v| replace_loop_var(g.stream(), &loop_var.to_string(), v))
                        {
                            result.extend(ts.into_iter());
                        }
                        replaced = true;
                        idx += 3;
                        continue;
                    }
                    _ => {}
                }
            }
            result.push(tt.clone());
            idx += 1;
        }

        (TokenStream::from_iter(result), replaced)
    }

    let (ts, replaced) = replace_section(seq.content.clone(), from, to, loop_var);
    if !replaced {
        TokenStream::from_iter(
            (from..=to).map(|v| replace_loop_var(ts.clone(), &loop_var.to_string(), v)),
        )
    } else {
        ts
    }
    .into()
}
