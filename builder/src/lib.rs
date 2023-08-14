use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{self, spanned::Spanned, Field};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    match do_expand(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token![,]>;

fn get_fields_from_derive_input(st: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = st.data
    {
        return Ok(named);
    }
    Err(syn::Error::new_spanned(
        st,
        "Must define on a Struct, not Enum".to_string(),
    ))
}

fn generate_builder_struct_fields_def(
    st: &syn::DeriveInput,
) -> syn::Result<proc_macro2::TokenStream> {
    let fields = get_fields_from_derive_input(st)?;
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: syn::Result<Vec<_>> = fields
        .iter()
        .map(|f| {
            if let Some(inner_type) = get_generic_inner_type(&f.ty, "Option") {
                Ok(quote::quote!(
                    std::option::Option<#inner_type>
                ))
            } else if get_user_specified_ident_for_vec(f)?.is_some() {
                let origin_type = &f.ty;
                Ok(quote::quote!(
                    #origin_type
                ))
            } else {
                let origin_type = &f.ty;
                Ok(quote::quote!(
                    std::option::Option<#origin_type>
                ))
            }
        })
        .collect();

    let types = types?;
    let token_stream = quote! {
        #(#idents: #types),*
    };
    Ok(token_stream)
}

fn generate_builder_struct_factory_init_cluases(
    st: &syn::DeriveInput,
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let fields = get_fields_from_derive_input(st)?;

    let init_cluase: syn::Result<Vec<_>> = fields
        .iter()
        .map(|f| {
            let ident = &f.ident;
            if get_user_specified_ident_for_vec(&f)?.is_some() {
                Ok(quote::quote!(
                    #ident: std::vec::Vec::new()
                ))
            } else {
                Ok(quote::quote!(
                    #ident: std::option::Option::None
                ))
            }
        })
        .collect();

    Ok(init_cluase?)
}

fn generate_setter_functions(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fields = get_fields_from_derive_input(st)?;
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    let mut final_tokenstream = proc_macro2::TokenStream::new();

    for (index, (ident, type_)) in idents.iter().zip(types.iter()).enumerate() {
        let tokenstream_piece = if let Some(inner_type) = get_generic_inner_type(type_, "Option") {
            quote::quote!(
                fn #ident(&mut self, #ident: #inner_type) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            )
        } else if let Some(ref user_specified_ident) = get_user_specified_ident_for_vec(&fields[index])? {
            let inner_type = get_generic_inner_type(type_, "Vec").ok_or(syn::Error::new(fields[index].span(), "each field must be specified with Vec field"))?;

            let mut tokenstream_piece = quote! {
                fn #user_specified_ident(&mut self, #user_specified_ident: #inner_type) -> &mut Self {
                    self.#ident.push(#user_specified_ident);
                    self
                }
            };

            // 如果用户指定的setter名字和原始字段的名字不一样，那么产生另一个setter，这个setter是一次性传入一个列表的
            if user_specified_ident != ident.as_ref().unwrap() {
                tokenstream_piece.extend(
                    quote! {
                        fn #ident(&mut self, #ident: #type_) -> &mut Self {
                            self.#ident = #ident.clone();
                            self
                        }
                    }
                );
            }

            tokenstream_piece
        }
        else {
            quote::quote!(
                fn #ident(&mut self, #ident: #type_) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            )
        };

        final_tokenstream.extend(tokenstream_piece);
    }

    Ok(final_tokenstream)
}

fn generate_build_function(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fields = get_fields_from_derive_input(st)?;
    let types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    let mut checker_code_pieces = Vec::new();

    for idx in 0..fields.len() {
        if get_generic_inner_type(&fields[idx].ty, "Option").is_none() && get_user_specified_ident_for_vec(&fields[idx])?.is_none() {
            let ident = &fields[idx].ident;

            checker_code_pieces.push(quote::quote!(
                if self.#ident.is_none() {
                    let err = format!("{} field is missing", stringify!(#ident));
                    return std::result::Result::Err(err.into());
                }
            ))
        }
    }

    let mut fill_result_clauses = Vec::new();
    for idx in 0..fields.len() {
        let ident = &fields[idx].ident;
        if  get_user_specified_ident_for_vec(&fields[idx])?.is_some() {
            fill_result_clauses.push(
                quote::quote!(
                    #ident: self.#ident.clone()
                )
            )
        } else if get_generic_inner_type(&fields[idx].ty, "Option").is_none() {
            fill_result_clauses.push(quote::quote!(
                #ident: self.#ident.clone().unwrap()
            ))
        } else {
            fill_result_clauses.push(quote::quote!(
                #ident: self.#ident.clone()
            ))
        }
    }

    let original_struct_ident = &st.ident;

    let token_stream = quote::quote!(
        pub fn build(&mut self) -> std::result::Result<#original_struct_ident, std::boxed::Box<dyn std::error::Error>> {
            #(#checker_code_pieces)*

            let ret = #original_struct_ident {
                #(#fill_result_clauses),*
            };

            std::result::Result::Ok(ret)
        }
    );

    Ok(token_stream)
}

fn get_generic_inner_type<'a>(
    t: &'a syn::Type,
    outer_ident_name: &'a str,
) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = t
    {
        if let Some(seg) = segments.last() {
            if seg.ident.to_string() == outer_ident_name {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    args,
                    ..
                }) = &seg.arguments
                {
                    if let Some(syn::GenericArgument::Type(inner_type)) = args.first() {
                        return Some(inner_type);
                    }
                }
            }
        }
    }

    None
}

fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name_literal = st.ident.to_string();
    let builder_name_literal = format!("{}Builder", struct_name_literal);
    let builder_name_ident = syn::Ident::new(&builder_name_literal, st.span()); // span用于指示错误位置

    let struct_ident = &st.ident; // 模板代码中不可以使用`.`来访问结构体成员，所以要在模板代码外面将标识符放到一个独立的变量中

    let builder_struct_fields_def = generate_builder_struct_fields_def(st)?;

    let builder_struct_factory_init_cluases = generate_builder_struct_factory_init_cluases(st)?;

    let setter_functions = generate_setter_functions(st)?;

    // let struct_ident = &st.ident;

    // let fields = get_fields_from_derive_input(st)?;    // let build_function = generate_bulid_function(st)?;
    // let generated_builder_functions = generate_build_function(fields,struct_ident)?;
    let generated_builder_functions = generate_build_function(st)?;
    // println!("{:#?}", generated_builder_functions.to_string());

    let ret = quote! {
        pub struct #builder_name_ident {
            #builder_struct_fields_def
        }
        impl #struct_ident {
            pub fn builder() -> #builder_name_ident {
                #builder_name_ident{
                    #(#builder_struct_factory_init_cluases),*
                }
            }
        }

        impl #builder_name_ident {
            #setter_functions

            #generated_builder_functions
        }
    };

    return Ok(ret);
}

fn get_user_specified_ident_for_vec(f: &Field) -> syn::Result<Option<syn::Ident>> {
    for attr in &f.attrs {
        if let Ok(syn::Meta::List(syn::MetaList {
            ref path,
            ref nested,
            ..
        })) = attr.parse_meta()
        {
            if let Some(p) = path.segments.first() {
                if p.ident == "builder" {
                    if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(kv))) = nested.first() {
                        if kv.path.is_ident("each") {
                            if let syn::Lit::Str(ref ident_str) = kv.lit {
                                return Ok(Some(syn::Ident::new(
                                    ident_str.value().as_str(),
                                    attr.span(),
                                )));
                            }
                        } else {
                            if let Ok(syn::Meta::List(ref list)) = attr.parse_meta() {
                                return Err(syn::Error::new_spanned(list, r#"expected `builder(each = "...")`"#))
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}

// #[proc_macro_derive(ExploreAttribute)]
// pub fn attribute_explore(input: TokenStream) -> TokenStream {
//     let st = syn::parse_macro_input!(input as syn::DeriveInput);
//     let attr = st.attrs.first().unwrap();
//     let meta = attr.parse_meta(); // 解析为`sny::Meta`对象
//     eprintln!("{:#?}", meta);

//     proc_macro2::TokenStream::new().into()
// }
