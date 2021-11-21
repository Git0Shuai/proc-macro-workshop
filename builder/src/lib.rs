use quote::{quote, ToTokens};
use syn::{spanned::Spanned, Data, DataStruct, Fields, PathSegment, Type};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ts = syn::parse_macro_input!(input as syn::DeriveInput);

    let fields = if let Data::Struct(DataStruct {
        fields: Fields::Named(ref fields),
        ..
    }) = ts.data
    {
        &fields.named
    } else {
        panic!("support struct with named fields only")
    };

    let vis = ts.vis;
    let ident = ts.ident;
    let builder_ident = quote::format_ident!("{}Builder", ident);

    let get_inner_ty: for<'a> fn(&'a syn::Type, &str) -> Option<&'a syn::Type> = |ty, name| {
        if let Type::Path(syn::TypePath {
            path: syn::Path { segments, .. },
            ..
        }) = ty
        {
            if let Some(PathSegment {
                ident,
                arguments:
                    syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                        args, ..
                    }),
            }) = segments.first()
            {
                if ident == name {
                    if let Some(syn::GenericArgument::Type(ty_arg)) = args.first() {
                        Some(ty_arg)
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    };

    let field_names = fields
        .iter()
        .map(|f| &f.ident)
        .flatten()
        .map(|f| f.to_token_stream())
        .collect::<Vec<proc_macro2::TokenStream>>();
    let builder_fields_def = fields.iter().map(|f| {
        let mut f = f.clone();
        f.attrs.clear();
        let ty = &f.ty;
        if get_inner_ty(&ty, "Option").is_none() {
            f.ty = syn::Type::Verbatim(quote! { std::option::Option<#ty> });
        }
        f.to_token_stream()
    });
    // this will be iterated multiple times
    let fields_setter = fields.iter().map(|f| {
        let ident = f.ident.as_ref().unwrap();
        let mut ty = &f.ty;
        if let Some(inner_ty) = get_inner_ty(&ty, "Option") {
            ty = inner_ty;
        }

        let get_each_value_from_meta: fn(&syn::Meta) -> Option<syn::LitStr> = |m| {
            let get_each_value_from_nested: fn(
                &syn::punctuated::Punctuated<syn::NestedMeta, syn::token::Comma>,
            ) -> Option<syn::LitStr> = |nested| {
                nested.iter().find_map(|m| {
                    if let syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                        path,
                        lit,
                        ..
                    })) = m
                    {
                        match lit {
                            syn::Lit::Str(lit) if path.is_ident("each") => Some(lit.clone()),
                            _ => None,
                        }
                    } else {
                        None
                    }
                })
            };

            if let syn::Meta::List(syn::MetaList { path, nested, .. }) = m {
                match &path.segments.first() {
                    Some(syn::PathSegment { ident, .. }) if ident == "builder" => {
                        get_each_value_from_nested(nested)
                    }
                    _ => None,
                }
            } else {
                None
            }
        };

        let mut each_value = None;
        if let Some(attr) = &f.attrs.first() {
            let m = attr.parse_meta().unwrap();
            each_value = get_each_value_from_meta(&m);

            if each_value.is_none() {
                return syn::Error::new(m.span(), "expected `builder(each = \"...\")`")
                    .into_compile_error();
            }
        }

        let direct_setter = quote! {
            pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = std::option::Option::Some(#ident);
                self
            }
        };

        if let Some(v) = each_value {
            if let Some(item_ty) = get_inner_ty(&ty, "Vec") {
                let item_ident = quote::format_ident!("{}", v.value());
                let mut setter = quote! {
                    pub fn #item_ident(&mut self, item: #item_ty) -> &mut Self {
                        if let std::option::Option::Some(ref mut v) = self.#ident {
                            v.push(item);
                        } else {
                            self.#ident = std::option::Option::Some(vec![item]);
                        }
                        self
                    }
                };

                if ident != &v.value() {
                    setter.extend(direct_setter);
                }
                setter
            } else {
                direct_setter
            }
        } else {
            direct_setter
        }
    });

    let none_checker = fields.iter().map(|f| {
        let field_name = &f.ident;

        if get_inner_ty(&f.ty, "Option").is_some() {
            quote! {
                let #field_name = self.#field_name.clone();
            }
        } else if get_inner_ty(&f.ty, "Vec").is_some() {
            quote! {
                let #field_name = if let std::option::Option::Some(f) = &self.#field_name {
                    f.clone()
                } else {
                    std::vec::Vec::new()
                };
            }
        } else {
            let msg = format!("{} is None", &f.ident.to_token_stream());
            quote! {
                    let #field_name = if let std::option::Option::Some(f) = &self.#field_name {
                        f.clone()
                    } else {
                        return std::result::Result::Err(#msg.to_owned().into());
                    };
            }
        }
    });

    let output = quote! {
        #vis struct #builder_ident {
            #(#builder_fields_def),*
        }

        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#field_names: std::option::Option::None),*
                }
            }
        }

        impl #builder_ident {
            #(#fields_setter)*

            pub fn build(&mut self) -> std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> {
                #(#none_checker)*

                std::result::Result::Ok(#ident{#(#field_names),*})
            }
        }
    };

    output.into()
}
