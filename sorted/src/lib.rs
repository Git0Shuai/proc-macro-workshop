use quote::ToTokens;
use syn::parse_macro_input;

#[proc_macro_attribute]
pub fn sorted(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let _ = args;
    let input = parse_macro_input!(input as syn::Item);
    let r = sorted_inner(input);
    r.unwrap_or_else(|e| e.into_compile_error().into())
}

fn sorted_inner(input: syn::Item) -> Result<proc_macro::TokenStream, syn::Error> {
    let item_enum = match input {
        syn::Item::Enum(e) => Ok(e),
        _ => Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "expected enum or match expression",
        )),
    }?;

    let syn::ItemEnum { variants, .. } = &item_enum;
    let variants = variants.into_iter().collect::<Vec<_>>();
    if variants.len() == 0 {
        return Ok(item_enum.to_token_stream().into());
    }

    for window in variants.windows(2) {
        let a = window[0];
        let b = window[1];
        let name = b.ident.to_string();
        if a.ident.to_string() > name {
            let v = variants
                .iter()
                .map(|v| v.ident.to_string())
                .find(|it| it > &name)
                .unwrap();
            return Err(syn::Error::new(
                b.ident.span(),
                format!("{} should sort before {}", name, v),
            ));
        }
    }

    Ok(item_enum.to_token_stream().into())
}
