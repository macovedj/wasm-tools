use crate::DecodedWasm;

use anyhow::anyhow;
use id_arena::Id;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use wit_parser::{
    Enum, Flags, Function, FunctionKind, Handle, InterfaceId, Record, Resolve, Result_, Results,
    Tuple, Type, TypeDefKind, TypeId, TypeOwner, Variant, WorldId,
};

/// Package namespace and name.
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Package {
    /// Package namespace.
    pub namespace: String,
    /// Package name.
    pub name: String,
}

/// Wasm binary kind.
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum BinaryKind {
    /// Core Wasm module.
    Module,
    /// Wit package.
    Wit,
    /// Wasm component.
    Component,
}

/// Parsed package information.
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PackageInfo {
    /// Package name
    pub name: String,
    /// Package binary kind.
    pub kind: BinaryKind,
    /// List of interfaces defined.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub interfaces: Vec<Interface>,
    /// Type definitions.
    #[serde(flatten)]
    pub type_defs: Types,
    /// List of type definitions.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub funcs: Vec<Func>,
    /// List of world definitions.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub worlds: Vec<World>,
}

impl PackageInfo {
    fn new(name: String, kind: BinaryKind) -> Self {
        Self {
            name,
            kind,
            interfaces: Vec::new(),
            type_defs: Types {
                use_decls: Vec::new(),
                doc_types: Vec::new(),
            },
            funcs: Vec::new(),
            worlds: Vec::new(),
        }
    }
}

/// Import / export direction.
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Direction {
    /// Import direction.
    Import,
    /// Export direction.
    Export,
}

/// Component interface definition.
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Interface {
    /// Import / export direction.
    pub direction: Direction,
    /// Package namespace and name.
    pub package: Package,
    /// Doc comments.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub docs: Option<String>,
    /// Interface name.
    pub name: String,
    /// Type definitions.
    #[serde(flatten)]
    pub type_defs: Types,
    /// List of function definitions.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub funcs: Vec<Func>,
}

struct Iface {
    type_defs: Types,
    funcs: Vec<Func>,
}

/// Component world definition.
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct World {
    /// World name.
    pub name: String,
    /// List of imports.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub imports: Vec<(Option<String>, WorldItem)>,
    /// List of exports.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub exports: Vec<(Option<String>, WorldItem)>,
}

/// Component world item.
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum WorldItem {
    /// Interface type in a world.
    Interface,
    /// Function type defined in a world.
    Function((Vec<Param>, Vec<FuncResult>)),
    /// Type defined in a world.
    Type(DocType),
}

/// Component type definitions.
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Types {
    /// Use import type statements.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub use_decls: Vec<UsedType>,
    /// Type definitions.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub doc_types: Vec<DocType>,
}

/// Use type statement information.
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct UsedType {
    /// Import name.
    pub name: String,
    /// Optional alias name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub alias: Option<String>,
    /// Import path? (TODO)
    pub decl: String,
}

/// Component resource definition.
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Resource {
    /// Resource name.
    pub name: String,
    /// List of resource methods.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub methods: Vec<Func>,
}

/// Type declaration kind.
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum Declaration {
    /// Simple type.
    Simple(DocType),
    /// Resource type.
    Resource(Resource),
}

/// Declared type information.
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DeclaredType {
    /// Type name.
    pub name: String,
    /// Optional doc comments.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub docs: Option<String>,
    /// Type declaration information.
    pub decl: Declaration,
}

/// Type declaration information.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DocType {
    /// Name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    /// Type definition kind.
    pub kind: TypeDef,
    /// Owner.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub owner: Option<String>,
    /// Optional doc comments.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub docs: Option<String>,
}

/// Type definition.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum TypeDef {
    /// Primitive type.
    Primitive(Box<DocPrimitive>),
    /// Resource type.
    Resource(ResourceDoc),
    /// Resource handle type.
    Handle(Box<ResourceHandle>),
    /// Record type.
    Record(Box<Vec<DocTypeRef>>),
    /// Flags type.
    Flags(Vec<Flag>),
    /// Tuple type.
    Tuple(Box<Vec<DocTypeRef>>),
    /// Variant type.
    Variant(Box<Vec<DocTypeRef>>),
    /// Enum type.
    Enum(Vec<EnumCase>),
    /// Option type.
    Option(Box<DocTypeRef>),
    /// Result type.
    Result(Box<ResultTypes>),
    /// List type.
    List(Box<DocTypeRef>),
    /// Future type. TODO
    Future,
    /// Stream type. TODO
    Stream,
    /// Type reference type.
    Type(Box<DocTypeRef>),
    /// Unknown type.
    Unknown,
}

/// Enum type definition.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EnumCase {
    /// Name.
    pub name: String,
    /// Optional doc comments.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub docs: Option<String>,
}

/// Flag type definition.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Flag {
    /// Name.
    pub name: String,
    /// Optional doc comments.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub docs: Option<String>,
}

/// Resource handle owned or borrow.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum ResourceHandle {
    /// Owned resource handle.
    Own(DocTypeRef),
    /// Borrowed resource handle.
    Borrow(DocTypeRef),
}

/// Function result type.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ResultTypes {
    /// OK variant case.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ok: Option<DocTypeRef>,
    /// Err variant case.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<DocTypeRef>,
}

/// Resource definition.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ResourceDoc {
    /// Name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    /// Owner.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub owner: Option<String>,
    /// Optional doc comments.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub docs: Option<String>,
    /// List of function methods.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub methods: Vec<Func>,
}

/// Function definition.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Func {
    /// Name.
    pub name: String,
    /// If function is static method
    #[serde(default, skip_serializing_if = "is_false")]
    pub is_static: bool,
    /// Optional doc comments.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub docs: Option<String>,
    /// Function parameters.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub params: Vec<Param>,
    /// Function return result.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub results: Vec<FuncResult>,
}

/// Function parameter definition.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Param {
    /// Name.
    pub name: String,
    /// Type information.
    pub ty: DocTypeRef,
}

/// Function return result definition.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FuncResult {
    /// Name.
    pub name: String,
    /// Type information.
    pub ty: DocTypeRef,
}

/// Doc type kind.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum DocPrimitive {
    /// Boolean kind
    Bool,
    /// Unsigned 8-bit integer.
    U8,
    /// Unsigned 16-bit integer.
    U16,
    /// Unsigned 32-bit integer.
    U32,
    /// Unsigned 64-bit integer.
    U64,
    /// Signed 8-bit integer.
    S8,
    /// Signed 16-bit integer.
    S16,
    /// Signed 32-bit integer.
    S32,
    /// Signed 64-bit integer.
    S64,
    /// Signed 32-bit floating point number.
    F32,
    /// Signed 64-bit floating point number.
    F64,
    /// Char type.
    Char,
    /// String type.
    String,
    /// Reference to type definition.
    Id(Box<TypeDef>), // TODO needs Box<>???
}

/// Doc type reference.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DocTypeRef {
    /// Name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    /// Owner.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub owner: Option<String>,
    /// Optional doc comments.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub docs: Option<String>,
    /// Type definition information.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ty: Option<TypeDef>,
}

impl DocTypeRef {
    fn from_type(value: Type, resolve: &Resolve) -> Self {
        match value {
            Type::Bool => Self {
                name: None,
                owner: None,
                docs: None,
                ty: Some(TypeDef::Primitive(Box::new(DocPrimitive::Bool))),
            },
            Type::U8 => Self {
                name: None,
                owner: None,
                docs: None,
                ty: Some(TypeDef::Primitive(Box::new(DocPrimitive::U8))),
            },
            Type::U16 => Self {
                name: None,
                owner: None,
                docs: None,
                ty: Some(TypeDef::Primitive(Box::new(DocPrimitive::U16))),
            },
            Type::U32 => Self {
                name: None,
                owner: None,
                docs: None,
                ty: Some(TypeDef::Primitive(Box::new(DocPrimitive::U32))),
            },
            Type::U64 => Self {
                name: None,
                owner: None,
                docs: None,
                ty: Some(TypeDef::Primitive(Box::new(DocPrimitive::U64))),
            },
            Type::S8 => Self {
                name: None,
                owner: None,
                docs: None,
                ty: Some(TypeDef::Primitive(Box::new(DocPrimitive::S8))),
            },
            Type::S16 => Self {
                name: None,
                owner: None,
                docs: None,
                ty: Some(TypeDef::Primitive(Box::new(DocPrimitive::S16))),
            },
            Type::S32 => Self {
                name: None,
                owner: None,
                docs: None,
                ty: Some(TypeDef::Primitive(Box::new(DocPrimitive::S32))),
            },
            Type::S64 => Self {
                name: None,
                owner: None,
                docs: None,
                ty: Some(TypeDef::Primitive(Box::new(DocPrimitive::S64))),
            },
            Type::F32 => Self {
                name: None,
                owner: None,
                docs: None,
                ty: Some(TypeDef::Primitive(Box::new(DocPrimitive::F32))),
            },
            Type::F64 => Self {
                name: None,
                owner: None,
                docs: None,
                ty: Some(TypeDef::Primitive(Box::new(DocPrimitive::F64))),
            },
            Type::Char => Self {
                name: None,
                owner: None,
                docs: None,
                ty: Some(TypeDef::Primitive(Box::new(DocPrimitive::Char))),
            },
            Type::String => Self {
                name: None,
                owner: None,
                docs: None,
                ty: Some(TypeDef::Primitive(Box::new(DocPrimitive::String))),
            },
            Type::Id(id) => {
                let ty = &resolve.types[id];
                match ty.kind.clone() {
                    TypeDefKind::Record(_) => Self {
                        name: ty.name.clone(),
                        owner: print_owner(ty.owner, resolve),
                        docs: ty.docs.contents.clone(),
                        ty: None,
                    },
                    TypeDefKind::Resource => Self {
                        name: ty.name.clone(),
                        owner: print_owner(ty.owner, resolve),
                        docs: ty.docs.contents.clone(),
                        ty: None,
                    },
                    TypeDefKind::Handle(handle) => {
                        let handle_ref = match handle {
                            Handle::Own(owned_id) => {
                                let owned_ty = &resolve.types[owned_id];
                                ResourceHandle::Own(DocTypeRef {
                                    name: owned_ty.name.clone(),
                                    owner: print_owner(owned_ty.owner, resolve),
                                    docs: owned_ty.docs.contents.clone(),
                                    ty: Some(TypeDef::from_type_def_kind(&owned_ty.kind, resolve)),
                                })
                            }
                            Handle::Borrow(borrowed_id) => {
                                let borrowed_ty = &resolve.types[borrowed_id];
                                ResourceHandle::Borrow(DocTypeRef {
                                    name: borrowed_ty.name.clone(),
                                    owner: print_owner(borrowed_ty.owner, resolve),
                                    docs: borrowed_ty.docs.contents.clone(),
                                    ty: Some(TypeDef::from_type_def_kind(
                                        &borrowed_ty.kind,
                                        resolve,
                                    )),
                                })
                            }
                        };
                        Self {
                            name: ty.name.clone(),
                            owner: print_owner(ty.owner, resolve),
                            docs: ty.docs.contents.clone(),
                            ty: Some(TypeDef::Handle(Box::new(handle_ref))),
                        }
                    }
                    TypeDefKind::Flags(_) => Self {
                        name: ty.name.clone(),
                        owner: print_owner(ty.owner, resolve),
                        docs: ty.docs.contents.clone(),
                        ty: None,
                    },
                    TypeDefKind::Tuple(Tuple { types }) => {
                        if let Some(name) = &ty.name {
                            return Self {
                                name: Some(name.clone()),
                                owner: print_owner(ty.owner, resolve),
                                docs: ty.docs.contents.clone(),
                                ty: Some(TypeDef::Type(Box::new(DocTypeRef {
                                    name: Some(name.clone()),
                                    owner: print_owner(ty.owner, resolve),
                                    docs: ty.docs.contents.clone(),
                                    ty: None,
                                }))),
                            };
                        } else {
                            let mut type_refs = Vec::new();
                            for ty in types {
                                let type_ref = if let Type::Id(id) = ty {
                                    let inner = &resolve.types[id];
                                    if let Some(_) = inner.name.clone() {
                                        DocTypeRef {
                                            name: inner.name.clone(),
                                            owner: print_owner(inner.owner, resolve),
                                            docs: inner.docs.contents.clone(),
                                            ty: None,
                                        }
                                    } else {
                                        DocTypeRef::from_type(ty, resolve)
                                    }
                                } else {
                                    DocTypeRef::from_type(ty, resolve)
                                };
                                type_refs.push(type_ref);
                            }
                            Self {
                                name: ty.name.clone(),
                                owner: print_owner(ty.owner, resolve),
                                docs: ty.docs.contents.clone(),
                                ty: Some(TypeDef::Tuple(Box::new(type_refs))),
                            }
                        }
                    }
                    TypeDefKind::Variant(_) => Self {
                        name: ty.name.clone(),
                        owner: print_owner(ty.owner, resolve),
                        docs: ty.docs.contents.clone(),
                        ty: None,
                    },
                    TypeDefKind::Enum(_) => Self {
                        name: ty.name.clone(),
                        owner: print_owner(ty.owner, resolve),
                        docs: ty.docs.contents.clone(),
                        ty: None,
                    },
                    TypeDefKind::Option(inner) => Self {
                        name: ty.name.clone(),
                        owner: print_owner(ty.owner, resolve),
                        docs: ty.docs.contents.clone(),
                        ty: if let Some(_) = &ty.name {
                            None
                        } else {
                            Some(TypeDef::Option(Box::new(DocTypeRef::from_type(
                                inner, resolve,
                            ))))
                        },
                    },
                    TypeDefKind::Result(Result_ { ok, err }) => {
                        let ok_ref = if let Some(ok) = ok {
                            Some(DocTypeRef::from_type(ok, resolve))
                        } else {
                            None
                        };
                        let err_ref = if let Some(err) = err {
                            Some(DocTypeRef::from_type(err, resolve))
                        } else {
                            None
                        };
                        return Self {
                            name: ty.name.clone(),
                            owner: print_owner(ty.owner, resolve),
                            docs: ty.docs.contents.clone(),
                            ty: Some(TypeDef::Result(Box::new(ResultTypes {
                                ok: ok_ref,
                                error: err_ref,
                            }))),
                        };
                    }
                    TypeDefKind::List(inner_ty) => {
                        if let Some(inner_name) = &ty.name {
                            return DocTypeRef {
                                name: Some(inner_name.clone()),
                                owner: print_owner(ty.owner, resolve),
                                docs: ty.docs.contents.clone(),
                                ty: Some(TypeDef::Type(Box::new(DocTypeRef {
                                    name: Some(inner_name.clone()),
                                    owner: print_owner(ty.owner, resolve),
                                    docs: ty.docs.contents.clone(),
                                    ty: None,
                                }))),
                            };
                        } else {
                            if let Type::Id(id) = inner_ty {
                                let list_ty = &resolve.types[id];
                                if let Some(name) = &list_ty.name {
                                    return DocTypeRef {
                                        name: None,
                                        owner: None,
                                        docs: None,
                                        ty: Some(TypeDef::List(Box::new(DocTypeRef {
                                            name: Some(name.clone()),
                                            owner: print_owner(list_ty.owner, resolve),
                                            docs: list_ty.docs.contents.clone(),
                                            ty: None,
                                        }))),
                                    };
                                } else {
                                    return DocTypeRef {
                                        name: None,
                                        owner: None,
                                        docs: None,
                                        ty: Some(TypeDef::List(Box::new(DocTypeRef {
                                            name: None,
                                            owner: None,
                                            docs: None,
                                            ty: Some(TypeDef::from_type(&inner_ty, Some(resolve))),
                                        }))),
                                    };
                                }
                            } else {
                                return Self {
                                    name: None,
                                    owner: None,
                                    docs: None,
                                    ty: Some(TypeDef::List(Box::new(DocTypeRef {
                                        name: None,
                                        owner: None,
                                        docs: None,
                                        ty: Some(TypeDef::from_type(&inner_ty, Some(resolve))),
                                    }))),
                                };
                            }
                        }
                    }
                    TypeDefKind::Future(_) => unreachable!(),
                    TypeDefKind::Stream(_) => unreachable!(),
                    TypeDefKind::Type(_) => Self {
                        name: ty.name.clone(),
                        owner: print_owner(ty.owner, resolve),
                        docs: ty.docs.contents.clone(),
                        ty: None,
                    },
                    TypeDefKind::Unknown => unreachable!(),
                }
            }
        }
    }
}

impl TypeDef {
    fn from_type(value: &Type, resolve: Option<&Resolve>) -> Self {
        match value {
            Type::Bool => TypeDef::Primitive(Box::new(DocPrimitive::Bool)),
            Type::U8 => TypeDef::Primitive(Box::new(DocPrimitive::U8)),
            Type::U16 => TypeDef::Primitive(Box::new(DocPrimitive::U16)),
            Type::U32 => TypeDef::Primitive(Box::new(DocPrimitive::U32)),
            Type::U64 => TypeDef::Primitive(Box::new(DocPrimitive::U64)),
            Type::S8 => TypeDef::Primitive(Box::new(DocPrimitive::S8)),
            Type::S16 => TypeDef::Primitive(Box::new(DocPrimitive::S16)),
            Type::S32 => TypeDef::Primitive(Box::new(DocPrimitive::S32)),
            Type::S64 => TypeDef::Primitive(Box::new(DocPrimitive::S64)),
            Type::F32 => TypeDef::Primitive(Box::new(DocPrimitive::F32)),
            Type::F64 => TypeDef::Primitive(Box::new(DocPrimitive::F64)),
            Type::Char => TypeDef::Primitive(Box::new(DocPrimitive::Char)),
            Type::String => TypeDef::Primitive(Box::new(DocPrimitive::String)),
            Type::Id(id) => {
                if let Some(resolve) = resolve {
                    let ty = &resolve.types[*id];
                    let owner = print_owner(ty.owner, resolve);
                    TypeDef::Type(Box::new(DocTypeRef {
                        name: ty.name.clone(),
                        owner,
                        docs: ty.docs.contents.clone(),
                        ty: Some(TypeDef::from_type_def_kind(&ty.kind, resolve)),
                    }))
                } else {
                    TypeDef::Unknown
                }
            }
        }
    }

    fn from_type_def_kind(value: &TypeDefKind, resolve: &Resolve) -> Self {
        match value {
            TypeDefKind::Record(Record { fields }) => {
                let mut field_types = Vec::new();
                for field in fields {
                    let doc_field = DocTypeRef {
                        name: Some(field.name.clone()),
                        owner: None,
                        docs: field.docs.contents.clone(),
                        ty: Some(TypeDef::from_type(&field.ty, Some(resolve))),
                    };
                    field_types.push(doc_field);
                }
                TypeDef::Record(Box::new(field_types))
            }
            TypeDefKind::Resource => TypeDef::Resource(ResourceDoc {
                owner: None,
                name: None,
                docs: None,
                methods: vec![],
            }),
            TypeDefKind::Handle(handle) => {
                let handle_ref = match handle {
                    Handle::Own(owned_id) => {
                        let owned_ty = &resolve.types[*owned_id];
                        ResourceHandle::Own(DocTypeRef {
                            name: owned_ty.name.clone(),
                            owner: print_owner(owned_ty.owner, resolve),
                            docs: owned_ty.docs.contents.clone(),
                            ty: Some(TypeDef::from_type_def_kind(&owned_ty.kind, resolve)),
                        })
                    }
                    Handle::Borrow(borrowed_id) => {
                        let borrowed_ty = &resolve.types[*borrowed_id];
                        ResourceHandle::Borrow(DocTypeRef {
                            name: borrowed_ty.name.clone(),
                            owner: print_owner(borrowed_ty.owner, resolve),
                            docs: borrowed_ty.docs.contents.clone(),
                            ty: Some(TypeDef::from_type_def_kind(&borrowed_ty.kind, resolve)),
                        })
                    }
                };
                TypeDef::Handle(Box::new(handle_ref))
            }
            TypeDefKind::Flags(Flags { flags }) => TypeDef::Flags(
                flags
                    .iter()
                    .map(|f| Flag {
                        name: f.name.clone(),
                        docs: f.docs.contents.clone(),
                    })
                    .collect(),
            ),
            TypeDefKind::Tuple(Tuple { types }) => {
                let mut type_refs = Vec::new();
                for ty in types {
                    type_refs.push(DocTypeRef::from_type(*ty, resolve))
                }
                TypeDef::Tuple(Box::new(type_refs))
            }
            TypeDefKind::Variant(Variant { cases }) => {
                let mut doc_cases = Vec::new();
                for case in cases {
                    if let Some(case_ty) = case.ty {
                        if let Type::Id(id) = case_ty {
                            let inner = &resolve.types[id];
                            let doc_case = DocTypeRef {
                                name: Some(case.name.clone()),
                                owner: print_owner(inner.owner, resolve),
                                docs: case.docs.contents.clone(),
                                ty: Some(TypeDef::Type(Box::new(DocTypeRef::from_type(
                                    case_ty, resolve,
                                )))),
                            };
                            doc_cases.push(doc_case);
                        } else {
                            let doc_case = DocTypeRef {
                                name: Some(case.name.clone()),
                                owner: None,
                                docs: case.docs.contents.clone(),
                                ty: Some(TypeDef::Type(Box::new(DocTypeRef::from_type(
                                    case_ty, resolve,
                                )))),
                            };
                            doc_cases.push(doc_case);
                        }
                    } else {
                        let doc_case = DocTypeRef {
                            name: Some(case.name.clone()),
                            docs: case.docs.contents.clone(),
                            owner: None,
                            ty: None,
                        };
                        doc_cases.push(doc_case);
                    }
                }
                TypeDef::Variant(Box::new(doc_cases))
            }
            TypeDefKind::Enum(Enum { cases }) => TypeDef::Enum(
                cases
                    .into_iter()
                    .map(|case| EnumCase {
                        name: case.name.clone(),
                        docs: case.docs.contents.clone(),
                    })
                    .collect(),
            ),
            TypeDefKind::Option(inner) => {
                TypeDef::Option(Box::new(DocTypeRef::from_type(*inner, resolve)))
            }
            TypeDefKind::Result(Result_ { ok, err }) => TypeDef::Result(Box::new(ResultTypes {
                ok: if let Some(ok_ty) = &ok {
                    Some(DocTypeRef::from_type(*ok_ty, resolve))
                } else {
                    None
                },
                error: if let Some(err_ty) = &err {
                    Some(DocTypeRef::from_type(*err_ty, resolve))
                } else {
                    None
                },
            })),
            TypeDefKind::List(inner) => {
                TypeDef::List(Box::new(DocTypeRef::from_type(*inner, resolve)))
            }
            TypeDefKind::Future(_) => TypeDef::Future,
            TypeDefKind::Stream(_) => TypeDef::Stream,
            TypeDefKind::Type(inner) => {
                TypeDef::Type(Box::new(DocTypeRef::from_type(*inner, resolve)))
            }
            TypeDefKind::Unknown => TypeDef::Unknown,
        }
    }
}

fn print_path_to_interface(resolve: &Resolve, interface: InterfaceId) -> String {
    let mut path = String::new();
    let iface = &resolve.interfaces[interface];
    let pkg = &resolve.packages[iface.package.unwrap()].name;
    path.push_str(&format!(
        "{}:{}/{}",
        &pkg.namespace,
        &pkg.name,
        &iface.name.as_ref().unwrap()
    ));
    path
}

fn print_owner(owner: TypeOwner, resolve: &Resolve) -> Option<String> {
    match owner {
        // May Need to implement world branch
        TypeOwner::World(id) => {
            let world = &resolve.worlds[id];
            if let Some(pkg) = world.package {
                let pkg = resolve.packages[pkg].clone();
                let namespace = pkg.name.namespace.clone();
                let name = pkg.name.name.clone();
                let full = Some(format!("{}:{}#{}", namespace, name, world.name));
                full
            } else {
                Some(resolve.worlds[id].name.clone())
            }
        }
        TypeOwner::Interface(id) => {
            let iface = &resolve.interfaces[id];
            if let Some(pkg) = iface.package {
                let pkg = resolve.packages[pkg].clone();

                let namespace = pkg.name.namespace.clone();
                let name = pkg.name.name.clone();
                let full = if let Some(iface) = iface.name.clone() {
                    Some(format!("{}:{}#{}", namespace, name, iface))
                } else {
                    Some(format!("{}:{}", namespace, name))
                };
                full
            } else {
                resolve.interfaces[id].name.clone()
            }
        }
        TypeOwner::None => None,
    }
}

fn print_resource(resolve: &Resolve, id: TypeId, funcs: &[&Function]) -> DocType {
    let ty = &resolve.types[id];
    let owner = print_owner(ty.owner, resolve);
    let mut methods = Vec::new();
    for func in funcs {
        let docs = func.docs.contents.clone();
        match func.kind {
            FunctionKind::Freestanding => unreachable!(),
            FunctionKind::Method(_) => {
                let (params, results) = print_function(resolve, func);
                methods.push(Func {
                    name: func.name.clone(),
                    is_static: false,
                    docs,
                    params,
                    results,
                });
            }
            FunctionKind::Static(_) => {
                let (params, results) = print_function(resolve, func);
                methods.push(Func {
                    name: func.name.clone(),
                    is_static: true,
                    docs,
                    params,
                    results,
                });
            }
            FunctionKind::Constructor(_) => {
                let (params, results) = print_function(resolve, func);
                methods.push(Func {
                    name: func.name.clone(),
                    is_static: false,
                    docs,
                    params,
                    results,
                });
            }
        }
    }
    DocType {
        kind: TypeDef::Resource(ResourceDoc {
            owner: owner.clone(),
            name: ty.name.clone(),
            docs: ty.docs.contents.clone(),
            methods,
        }),
        owner,
        name: ty.name.clone(),
        docs: None,
    }
}

fn render_type(
    resolve: &Resolve,
    id: &Id<wit_parser::TypeDef>,
    resource_funcs: &HashMap<TypeId, Vec<&Function>>,
) -> DocType {
    let type_def = &resolve.types[*id];
    match type_def.kind.clone() {
        TypeDefKind::Record(Record { fields }) => {
            let mut field_tys = Vec::new();
            let mut doc_fields = Vec::new();
            for field in fields {
                match field.ty {
                    Type::Id(inner_id) => {
                        let check = print_doc_type(Some(field.name.clone()), resolve, &field.ty);
                        field_tys.push(check);
                        let inner = &resolve.types[inner_id];
                        let doc_field = DocTypeRef {
                            name: Some(field.name.clone()),
                            owner: print_owner(inner.owner, resolve),
                            docs: field.docs.contents.clone(),
                            ty: if let Some(_) = inner.name.clone() {
                                Some(TypeDef::Type(Box::new(DocTypeRef {
                                    name: inner.name.clone(),
                                    owner: print_owner(inner.owner, resolve),
                                    docs: inner.docs.contents.clone(),
                                    ty: None,
                                })))
                            } else {
                                Some(TypeDef::from_type(&field.ty, Some(resolve)))
                            },
                        };
                        doc_fields.push(doc_field);
                    }
                    _ => {
                        let doc_field = DocTypeRef {
                            name: Some(field.name.clone()),
                            owner: None,
                            docs: field.docs.contents.clone(),
                            ty: Some(TypeDef::from_type(&field.ty, Some(resolve))),
                        };
                        doc_fields.push(doc_field);
                        field_tys.push(print_doc_type(Some(field.name), resolve, &field.ty))
                    }
                }
            }
            DocType {
                kind: TypeDef::Record(Box::new(doc_fields)),
                owner: None,
                name: type_def.name.clone(),
                docs: type_def.docs.contents.clone(),
            }
        }
        TypeDefKind::Flags(Flags { flags }) => DocType {
            kind: TypeDef::Flags(
                flags
                    .iter()
                    .map(|f| Flag {
                        name: f.name.clone(),
                        docs: f.docs.contents.clone(),
                    })
                    .collect(),
            ),
            owner: None,
            name: type_def.name.clone(),
            docs: type_def.docs.contents.clone(),
        },
        TypeDefKind::Tuple(Tuple { types }) => {
            let mut tys = Vec::new();
            for ty in types {
                let type_ref = DocTypeRef::from_type(ty, resolve);
                tys.push(type_ref);
            }
            DocType {
                kind: TypeDef::Tuple(Box::new(tys)),
                owner: None,
                name: type_def.name.clone(),
                docs: type_def.docs.contents.clone(),
            }
        }
        TypeDefKind::Variant(Variant { cases }) => {
            let mut case_types = Vec::new();
            for case in cases {
                if let Some(ty) = case.ty {
                    match ty {
                        Type::Id(id) => {
                            let case_ty = &resolve.types[id];
                            let doc_case = DocTypeRef {
                                name: Some(case.name.clone()),
                                owner: print_owner(case_ty.owner, resolve),
                                docs: case.docs.contents.clone(),
                                ty: Some(TypeDef::Type(Box::new(DocTypeRef::from_type(
                                    ty, resolve,
                                )))),
                            };
                            case_types.push(doc_case);
                        }
                        _ => case_types.push(DocTypeRef {
                            owner: print_owner(type_def.owner, resolve),
                            name: Some(case.name.clone()),
                            docs: case.docs.contents.clone(),
                            ty: Some(TypeDef::from_type(&ty, Some(resolve))),
                        }),
                    }
                } else {
                    let owner = print_owner(type_def.owner, resolve);
                    case_types.push(DocTypeRef {
                        name: Some(case.name.clone()),
                        owner,
                        docs: case.docs.contents,
                        ty: None,
                    });
                }
            }
            DocType {
                name: type_def.name.clone(),
                kind: TypeDef::Variant(Box::new(case_types)),
                owner: None,
                docs: type_def.docs.contents.clone(),
            }
        }
        TypeDefKind::Enum(Enum { cases }) => {
            let mut case_types = Vec::new();
            let mut doc_cases = Vec::new();
            for case in cases {
                doc_cases.push(EnumCase {
                    name: case.name.clone(),
                    docs: case.docs.contents.clone(),
                });
                case_types.push(DocType {
                    kind: TypeDef::Primitive(Box::new(DocPrimitive::Bool)),
                    owner: None,
                    name: Some(case.name.clone()),
                    docs: case.docs.contents,
                });
            }
            DocType {
                kind: TypeDef::Enum(doc_cases),
                owner: None,
                name: type_def.name.clone(),
                docs: type_def.docs.contents.clone(),
            }
        }
        TypeDefKind::Resource => {
            print_resource(resolve, *id, resource_funcs.get(&id).unwrap_or(&Vec::new()))
        }
        TypeDefKind::Type(ty) => DocType {
            name: type_def.name.clone(),
            kind: TypeDef::Type(Box::new(DocTypeRef::from_type(ty, resolve))),
            owner: print_owner(type_def.owner, resolve),
            docs: type_def.docs.contents.clone(),
        },
        TypeDefKind::Handle(_) => unreachable!(),
        TypeDefKind::Option(ty) => DocType {
            kind: TypeDef::Option(Box::new(
                DocTypeRef::from_type(ty, resolve),
                // print_type_def_ref(&ty, resolve),
            )),
            owner: None,
            name: type_def.name.clone(),
            docs: type_def.docs.contents.clone(),
        },
        TypeDefKind::Result(Result_ { ok, err }) => {
            let mut children = Vec::new();
            if let Some(ok) = ok {
                children.push(print_doc_type(None, resolve, &ok));
            };
            if let Some(err) = err {
                children.push(print_doc_type(None, resolve, &err));
            };
            DocType {
                kind: TypeDef::Result(Box::new(ResultTypes {
                    ok: if let Some(ok_ty) = &ok {
                        // Some(print_type_def_ref(ok_ty, resolve))
                        Some(DocTypeRef::from_type(*ok_ty, resolve))
                    } else {
                        None
                    },
                    error: if let Some(err_ty) = &err {
                        // Some(print_type_def_ref(err_ty, resolve))
                        Some(DocTypeRef::from_type(*err_ty, resolve))
                    } else {
                        None
                    },
                })),
                owner: None,
                name: type_def.name.clone(),
                docs: type_def.docs.contents.clone(),
            }
        }
        TypeDefKind::List(ty) => DocType {
            // kind: TypeDef::List(Box::new(print_type_def_ref(&ty, resolve))),
            kind: TypeDef::List(Box::new(DocTypeRef::from_type(ty, resolve))),
            owner: None,
            name: type_def.name.clone(),
            docs: type_def.docs.contents.clone(),
        },
        TypeDefKind::Future(_) => unreachable!(),
        TypeDefKind::Stream(_) => unreachable!(),
        TypeDefKind::Unknown => unreachable!(),
    }
}

fn print_types<'a>(
    resolve: &Resolve,
    owner: TypeOwner,
    types: impl Iterator<Item = (&'a str, TypeId)>,
    resource_funcs: &HashMap<TypeId, Vec<&Function>>,
) -> Types {
    let mut use_decls = Vec::new();
    let mut doc_types = Vec::new();
    // Partition types defined in this interface into either those imported
    // from foreign interfaces or those defined locally.
    let mut types_to_declare = Vec::new();
    let mut types_to_import: Vec<(_, Vec<_>)> = Vec::new();
    for (name, ty_id) in types {
        let ty = &resolve.types[ty_id];
        if let TypeDefKind::Type(Type::Id(other)) = ty.kind {
            let other = &resolve.types[other];
            match other.owner {
                TypeOwner::None => {}
                other_owner if owner != other_owner => {
                    let other_name = other
                        .name
                        .as_ref()
                        .ok_or_else(|| anyhow!("cannot import unnamed type"))
                        .unwrap();
                    if let Some((owner, list)) = types_to_import.last_mut() {
                        if *owner == other_owner {
                            list.push((name, other_name));
                            continue;
                        }
                    }
                    types_to_import.push((other_owner, vec![(name, other_name)]));
                    continue;
                }
                _ => {}
            }
        }

        types_to_declare.push(ty_id);
    }

    // Generate a `use` statement for all imported types.
    for (owner, tys) in types_to_import {
        let id = match owner {
            TypeOwner::Interface(id) => id,
            // it's only possible to import types from interfaces at
            // this time.
            _ => unreachable!(),
        };
        let path = print_path_to_interface(resolve, id);
        for (other_name, my_name) in tys {
            if my_name == other_name {
                let decl = format!("{path}.{{{my_name}}}");
                use_decls.push(UsedType {
                    name: my_name.clone(),
                    alias: None,
                    decl,
                });
            } else {
                let decl = format!("{path}.{{{my_name}}}");
                use_decls.push(UsedType {
                    name: my_name.to_string(),
                    alias: Some(other_name.to_string()),
                    decl,
                });
            }
        }
    }

    for id in types_to_declare {
        doc_types.push(render_type(resolve, &id, resource_funcs));
    }
    Types {
        use_decls,
        doc_types,
    }
}

fn print_doc_type(name: Option<String>, resolve: &Resolve, ty: &Type) -> DocType {
    match ty {
        Type::Bool => DocType {
            kind: TypeDef::Primitive(Box::new(DocPrimitive::Bool)),
            owner: None,
            name,
            docs: None,
        },
        Type::U8 => DocType {
            kind: TypeDef::Primitive(Box::new(DocPrimitive::U8)),
            owner: None,
            name,
            docs: None,
        },
        Type::U16 => DocType {
            kind: TypeDef::Primitive(Box::new(DocPrimitive::U16)),
            owner: None,
            name,
            docs: None,
        },
        Type::U32 => DocType {
            kind: TypeDef::Primitive(Box::new(DocPrimitive::U32)),
            owner: None,
            name,
            docs: None,
        },
        Type::U64 => DocType {
            kind: TypeDef::Primitive(Box::new(DocPrimitive::U64)),
            owner: None,
            name,
            docs: None,
        },
        Type::S8 => DocType {
            kind: TypeDef::Primitive(Box::new(DocPrimitive::S8)),
            owner: None,
            name,
            docs: None,
        },
        Type::S16 => DocType {
            kind: TypeDef::Primitive(Box::new(DocPrimitive::S16)),
            owner: None,
            name,
            docs: None,
        },
        Type::S32 => DocType {
            kind: TypeDef::Primitive(Box::new(DocPrimitive::S32)),
            owner: None,
            name,
            docs: None,
        },
        Type::S64 => DocType {
            kind: TypeDef::Primitive(Box::new(DocPrimitive::S64)),
            owner: None,
            name,
            docs: None,
        },
        Type::F32 => DocType {
            kind: TypeDef::Primitive(Box::new(DocPrimitive::F32)),
            owner: None,
            name,
            docs: None,
        },
        Type::F64 => DocType {
            kind: TypeDef::Primitive(Box::new(DocPrimitive::F64)),
            owner: None,
            name,
            docs: None,
        },
        Type::Char => DocType {
            kind: TypeDef::Primitive(Box::new(DocPrimitive::Char)),
            owner: None,
            name,
            docs: None,
        },
        Type::String => DocType {
            kind: TypeDef::Primitive(Box::new(DocPrimitive::String)),
            owner: None,
            name,
            docs: None,
        },
        Type::Id(id) => {
            let ty = &resolve.types[*id];
            let owner = print_owner(ty.owner, resolve);
            let doc_ty = match &ty.kind {
                TypeDefKind::Record(Record { fields }) => {
                    let mut doc_fields = Vec::new();
                    for field in fields {
                        let doc_field = DocTypeRef {
                            name: Some(field.name.clone()),
                            owner: None,
                            docs: field.docs.contents.clone(),
                            ty: Some(TypeDef::from_type(&field.ty, Some(resolve))),
                        };
                        doc_fields.push(doc_field);
                    }
                    DocType {
                        kind: TypeDef::Record(Box::new(doc_fields)),
                        owner: owner.clone(),
                        name,
                        docs: ty.docs.contents.clone(),
                    }
                }
                TypeDefKind::Resource => {
                    if let Some(_) = ty.name.clone() {
                        DocType {
                            kind: TypeDef::Resource(ResourceDoc {
                                owner: owner.clone(),
                                name: name.clone(),
                                docs: ty.docs.contents.clone(),
                                methods: vec![],
                            }),
                            owner: owner.clone(),
                            name,
                            docs: ty.docs.contents.clone(),
                        }
                    } else {
                        DocType {
                            kind: TypeDef::Resource(ResourceDoc {
                                owner: owner.clone(),
                                name: name.clone(),
                                docs: ty.docs.contents.clone(),
                                methods: vec![],
                            }),
                            owner: owner.clone(),
                            name,
                            docs: ty.docs.contents.clone(),
                        }
                    }
                }
                TypeDefKind::Handle(handle) => {
                    let handle_ref = match handle {
                        Handle::Own(owned_id) => {
                            let owned_ty = &resolve.types[*owned_id];
                            ResourceHandle::Own(DocTypeRef {
                                name: owned_ty.name.clone(),
                                owner: print_owner(owned_ty.owner, resolve),
                                docs: owned_ty.docs.contents.clone(),
                                ty: Some(TypeDef::from_type_def_kind(&owned_ty.kind, resolve)),
                            })
                        }
                        Handle::Borrow(borrowed_id) => {
                            let borrowed_ty = &resolve.types[*borrowed_id];
                            ResourceHandle::Borrow(DocTypeRef {
                                name: borrowed_ty.name.clone(),
                                owner: print_owner(borrowed_ty.owner, resolve),
                                docs: borrowed_ty.docs.contents.clone(),
                                ty: Some(TypeDef::from_type_def_kind(&borrowed_ty.kind, resolve)),
                            })
                        }
                    };
                    DocType {
                        kind: TypeDef::Handle(Box::new(handle_ref)),
                        owner,
                        name,
                        docs: ty.docs.contents.clone(),
                    }
                }
                TypeDefKind::Flags(Flags { flags }) => DocType {
                    kind: TypeDef::Flags(
                        flags
                            .iter()
                            .map(|f| Flag {
                                name: f.name.clone(),
                                docs: f.docs.contents.clone(),
                            })
                            .collect(),
                    ),
                    owner: None,
                    name: None,
                    docs: ty.docs.contents.clone(),
                },
                TypeDefKind::Variant(Variant { cases }) => {
                    let mut case_types = Vec::new();
                    let mut doc_cases = Vec::new();
                    for case in cases {
                        if let Some(case_ty) = case.ty {
                            let doc_type =
                                print_doc_type(Some(case.name.clone()), resolve, &case_ty);
                            case_types.push(doc_type);
                            let doc_case = DocTypeRef {
                                name: Some(case.name.clone()),
                                owner: None,
                                docs: case.docs.contents.clone(),
                                ty: Some(TypeDef::from_type(&case_ty, None)),
                            };
                            doc_cases.push(doc_case);
                        } else {
                            case_types.push(DocType {
                                kind: TypeDef::Variant(Box::new(vec![])),
                                owner: None,
                                name: Some(case.name.clone()),
                                docs: case.docs.contents.clone(),
                            });
                            let doc_case = DocTypeRef {
                                name: Some(case.name.clone()),
                                owner: None,
                                docs: case.docs.contents.clone(),
                                ty: None,
                            };
                            doc_cases.push(doc_case);
                        };
                    }
                    DocType {
                        kind: TypeDef::Variant(Box::new(doc_cases)),
                        owner: None,
                        name: None,
                        docs: ty.docs.contents.clone(),
                    }
                }
                TypeDefKind::Tuple(Tuple { types }) => {
                    let mut tys = Vec::new();
                    for ty in types {
                        let type_ref = DocTypeRef::from_type(*ty, resolve);
                        tys.push(type_ref);
                    }
                    DocType {
                        kind: TypeDef::Tuple(Box::new(tys)),
                        owner: None,
                        name,
                        docs: ty.docs.contents.clone(),
                    }
                }
                TypeDefKind::Enum(Enum { cases }) => {
                    let mut case_types = Vec::new();
                    let mut doc_cases = Vec::new();
                    for case in cases {
                        doc_cases.push(EnumCase {
                            name: case.name.clone(),
                            docs: case.docs.contents.clone(),
                        });
                        case_types.push(DocType {
                            kind: TypeDef::Primitive(Box::new(DocPrimitive::Bool)),
                            owner: None,
                            name: Some(case.name.clone()),
                            docs: case.docs.contents.clone(),
                        });
                    }
                    DocType {
                        kind: TypeDef::Enum(doc_cases),
                        owner: None,
                        name: None,
                        docs: ty.docs.contents.clone(),
                    }
                }
                TypeDefKind::Option(option_ty) => match option_ty {
                    Type::Id(id) => {
                        let ty = &resolve.types[*id];
                        let owner = print_owner(ty.owner, resolve);
                        DocType {
                            // kind: TypeDef::Option(Box::new(
                            kind: TypeDef::Option(Box::new(DocTypeRef::from_type(
                                *option_ty, resolve,
                            ))),
                            // print_type_def_ref(&option_ty, resolve),
                            // )),
                            owner: owner.clone(),
                            name: None,
                            docs: ty.docs.contents.clone(),
                        }
                    }
                    _ => DocType {
                        // kind: TypeDef::Option(Box::new(
                        //     print_type_def_ref(&option_ty, resolve),
                        // )),
                        kind: TypeDef::Option(Box::new(DocTypeRef::from_type(*option_ty, resolve))),
                        owner,
                        name: name.clone(),
                        docs: ty.docs.contents.clone(),
                    },
                },
                TypeDefKind::Result(Result_ { ok, err }) => {
                    let mut result_types = Vec::new();
                    if let Some(ok) = ok {
                        match ok {
                            Type::Id(id) => {
                                let ok_ty = &resolve.types[*id];
                                let owner = print_owner(ok_ty.owner, resolve);
                                if let Some(_) = &ok_ty.name {
                                    result_types.push(DocType {
                                        kind: TypeDef::from_type_def_kind(&ok_ty.kind, resolve),
                                        owner,
                                        name: name.clone(),
                                        docs: ok_ty.docs.contents.clone(),
                                    });
                                }
                            }
                            _ => {
                                result_types.push(print_doc_type(None, resolve, &ok));
                            }
                        }
                    }
                    if let Some(err) = err {
                        match err {
                            Type::Id(id) => {
                                let err_ty = &resolve.types[*id];
                                let owner = print_owner(err_ty.owner, resolve);
                                if let Some(_) = &err_ty.name {
                                    result_types.push(DocType {
                                        kind: TypeDef::from_type_def_kind(&err_ty.kind, resolve),
                                        owner,
                                        name: None,
                                        docs: err_ty.docs.contents.clone(),
                                    });
                                }
                            }
                            _ => {
                                result_types.push(print_doc_type(None, resolve, &err));
                            }
                        }
                    }
                    DocType {
                        kind: TypeDef::Result(Box::new(ResultTypes {
                            ok: if let Some(ok_ty) = &ok {
                                // Some(print_type_def_ref(ok_ty, resolve))
                                Some(DocTypeRef::from_type(*ok_ty, resolve))
                            } else {
                                None
                            },
                            error: if let Some(err_ty) = &err {
                                // Some(print_type_def_ref(err_ty, resolve))
                                Some(DocTypeRef::from_type(*err_ty, resolve))
                            } else {
                                None
                            },
                        })),
                        owner: None,
                        name,
                        docs: ty.docs.contents.clone(),
                    }
                }
                TypeDefKind::List(list_ty) => DocType {
                    // kind: TypeDef::List(Box::new(
                    //   print_type_def_ref(&list_ty, resolve),
                    // )),
                    kind: TypeDef::Option(Box::new(DocTypeRef::from_type(*list_ty, resolve))),
                    owner: None,
                    name,
                    docs: ty.docs.contents.clone(),
                },
                TypeDefKind::Future(_) => unreachable!(),
                TypeDefKind::Stream(_) => unreachable!(),
                TypeDefKind::Type(ty) => match ty {
                    Type::Id(id) => {
                        let ty = &resolve.types[*id];
                        let owner = print_owner(ty.owner, resolve);
                        if let Some(_) = &ty.name {
                            DocType {
                                kind: TypeDef::from_type_def_kind(&ty.kind, resolve),
                                owner,
                                name,
                                docs: ty.docs.contents.clone(),
                            }
                        } else {
                            DocType {
                                kind: TypeDef::from_type_def_kind(&ty.kind, resolve),
                                owner,
                                name: None,
                                docs: ty.docs.contents.clone(),
                            }
                        }
                    }
                    _ => print_doc_type(None, resolve, ty),
                },
                TypeDefKind::Unknown => unreachable!(),
            };
            doc_ty
        }
    }
}

fn print_function(resolve: &Resolve, func: &Function) -> (Vec<Param>, Vec<FuncResult>) {
    let mut params = Vec::new();
    let mut ret = Vec::new();

    // Methods don't print their `self` argument
    let params_to_skip = match &func.kind {
        FunctionKind::Method(_) => 1,
        _ => 0,
    };
    for (name, ty) in func.params.iter().skip(params_to_skip) {
        params.push(Param {
            name: name.clone(),
            // ty: print_type_def_ref(ty, resolve),
            ty: DocTypeRef::from_type(*ty, resolve),
        });
    }

    match &func.results {
        Results::Named(rs) => match rs.len() {
            0 => (),
            _ => {
                for (name, ty) in rs.iter() {
                    ret.push(FuncResult {
                        name: name.clone(),
                        // ty: print_type_def_ref(ty, resolve),
                        ty: DocTypeRef::from_type(*ty, resolve),
                    });
                }
            }
        },
        Results::Anon(ty) => {
            ret.push(FuncResult {
                name: "".to_string(),
                // ty: print_type_def_ref(ty, resolve),
                ty: DocTypeRef::from_type(*ty, resolve),
            });
        }
    }
    (params, ret)
}

fn print_world(resolve: &Resolve, id: WorldId) -> World {
    let world = &resolve.worlds[id];
    let mut printing_world = World {
        name: world.name.clone(),
        imports: Vec::new(),
        exports: Vec::new(),
    };
    let mut resource_funcs = HashMap::new();
    let mut freestanding = Vec::new();
    for (_, item) in &world.imports {
        if let wit_parser::WorldItem::Function(func) = item {
            if let Some(id) = register_resource_func(func) {
                resource_funcs.entry(id).or_insert(Vec::new()).push(func);
            } else {
                freestanding.push((func.name.clone(), func));
            }
        }
    }
    for (_, item) in &world.exports {
        if let wit_parser::WorldItem::Function(func) = item {
            if let Some(id) = register_resource_func(func) {
                resource_funcs.entry(id).or_insert(Vec::new()).push(func);
            } else {
                freestanding.push((func.name.clone(), func));
            }
        }
    }
    for (_, item) in &world.imports {
        match item {
            wit_parser::WorldItem::Interface(i) => {
                let iface = &resolve.interfaces[*i];
                if let Some(pkg_id) = iface.package {
                    let pkg = &resolve.packages[pkg_id];
                    let import_name = if let Some(world_pkg) = world.package {
                        if pkg_id == world_pkg {
                            if let Some(name) = &iface.name {
                                name.to_owned()
                            } else {
                                String::new()
                            }
                        } else {
                            if let Some(name) = &iface.name {
                                if let Some(v) = &pkg.name.version {
                                    format!(
                                        "{}:{}/{}@{}",
                                        pkg.name.namespace, pkg.name.name, name, v
                                    )
                                } else {
                                    format!("{}:{}/{}", pkg.name.namespace, pkg.name.name, name)
                                }
                            } else {
                                String::new()
                            }
                        }
                    } else {
                        String::new()
                    };
                    printing_world
                        .imports
                        .push((Some(import_name.to_owned()), WorldItem::Interface));
                }
            }
            wit_parser::WorldItem::Function(func) => {
                let printing_func = print_function(resolve, func);
                printing_world
                    .imports
                    .push((Some(func.name.clone()), WorldItem::Function(printing_func)));
            }
            wit_parser::WorldItem::Type(id) => {
                let doc_ty = render_type(resolve, id, &resource_funcs);
                printing_world
                    .imports
                    .push((doc_ty.name.clone(), WorldItem::Type(doc_ty)));
            }
        }
    }

    for (_, item) in &world.exports {
        match item {
            wit_parser::WorldItem::Interface(i) => {
                let iface = &resolve.interfaces[*i];
                if let Some(pkg_id) = iface.package {
                    let pkg = &resolve.packages[pkg_id];
                    let export_name = if let Some(world_pkg) = world.package {
                        if pkg_id == world_pkg {
                            if let Some(name) = &iface.name {
                                name.to_owned()
                            } else {
                                String::new()
                            }
                        } else {
                            if let Some(name) = &iface.name {
                                if let Some(v) = &pkg.name.version {
                                    format!(
                                        "{}:{}/{}@{}",
                                        pkg.name.namespace, pkg.name.name, name, v
                                    )
                                } else {
                                    format!("{}:{}/{}", pkg.name.namespace, pkg.name.name, name)
                                }
                            } else {
                                String::new()
                            }
                        }
                    } else {
                        String::new()
                    };
                    printing_world
                        .exports
                        .push((Some(export_name.to_owned()), WorldItem::Interface));
                }
            }
            wit_parser::WorldItem::Function(func) => {
                let printing_func = print_function(resolve, func);
                printing_world
                    .exports
                    .push((Some(func.name.clone()), WorldItem::Function(printing_func)));
            }
            wit_parser::WorldItem::Type(id) => {
                let doc_ty = render_type(resolve, id, &resource_funcs);
                printing_world
                    .exports
                    .push((doc_ty.name.clone(), WorldItem::Type(doc_ty)));
            }
        }
    }
    printing_world
}

fn print_interface(resolve: &Resolve, id: InterfaceId) -> Iface {
    let interface = &resolve.interfaces[id];

    let mut resource_funcs = HashMap::new();
    let mut freestanding = Vec::new();
    for (name, func) in interface.functions.iter() {
        if let Some(id) = register_resource_func(func) {
            resource_funcs.entry(id).or_insert(Vec::new()).push(func);
        } else {
            freestanding.push((name, func));
        }
    }

    let type_defs = print_types(
        resolve,
        TypeOwner::Interface(id),
        interface
            .types
            .iter()
            .map(|(name, id)| (name.as_str(), *id)),
        &resource_funcs,
    );

    let mut funcs = Vec::new();
    for (name, func) in freestanding {
        let docs = func.docs.contents.clone();
        let func = print_function(resolve, &func);
        funcs.push(Func {
            name: name.to_string(),
            is_static: false,
            docs,
            params: func.0,
            results: func.1,
        });
    }
    Iface { type_defs, funcs }
}

/// Process package info rendering docs
pub fn package_info(decoded: &DecodedWasm) -> PackageInfo {
    let pkg_id = decoded.package();
    match decoded {
        DecodedWasm::WitPackage(resolve, package_id) => {
            let pkg = &resolve.packages[*package_id];
            let mut printing_pkg = PackageInfo::new(pkg.name.name.clone(), BinaryKind::Wit);
            for (name, id) in pkg.interfaces.iter() {
                let interface = print_interface(&resolve, *id);
                let rendered = Interface {
                    direction: Direction::Import,
                    package: Package {
                        namespace: pkg.name.namespace.clone(),
                        name: pkg.name.name.clone(),
                    },
                    docs: resolve.interfaces[*id].docs.contents.clone(),
                    name: name.to_string(),
                    type_defs: interface.type_defs,
                    funcs: interface.funcs,
                };
                printing_pkg.interfaces.push(rendered);
            }
            for (_, id) in pkg.worlds.iter() {
                let world = print_world(&resolve, *id);
                printing_pkg.worlds.push(world);
            }
            printing_pkg
        }
        DecodedWasm::Component(resolve, world_id) => {
            let pkg = &resolve.packages.get(pkg_id).unwrap();

            let world = &resolve.worlds[*world_id];
            let mut printing_pkg = PackageInfo::new(pkg.name.name.clone(), BinaryKind::Component);
            let mut resource_funcs = HashMap::new();
            let mut freestanding = Vec::new();
            for (_, item) in &world.imports {
                if let wit_parser::WorldItem::Function(func) = item {
                    if let Some(id) = register_resource_func(func) {
                        resource_funcs.entry(id).or_insert(Vec::new()).push(func);
                    } else {
                        freestanding.push((func.name.clone(), func));
                    }
                }
            }
            for (_, item) in &world.exports {
                if let wit_parser::WorldItem::Function(func) = item {
                    if let Some(id) = register_resource_func(func) {
                        resource_funcs.entry(id).or_insert(Vec::new()).push(func);
                    } else {
                        freestanding.push((func.name.clone(), func));
                    }
                }
            }
            for (_, item) in &world.imports {
                match item {
                    wit_parser::WorldItem::Interface(id) => {
                        let iface = &resolve.interfaces[*id];
                        let package = &resolve.packages[iface.package.unwrap()];
                        let docs = iface.docs.contents.clone();
                        let interface = print_interface(&resolve, *id);
                        let rendered = if let Some(name) = iface.name.clone() {
                            Interface {
                                direction: Direction::Import,
                                package: Package {
                                    namespace: package.name.namespace.clone(),
                                    name: package.name.name.clone(),
                                },
                                docs,
                                name,
                                type_defs: interface.type_defs,
                                funcs: interface.funcs,
                            }
                        } else {
                            Interface {
                                direction: Direction::Import,
                                package: Package {
                                    namespace: package.name.namespace.clone(),
                                    name: package.name.name.clone(),
                                },
                                docs,
                                name: "".to_string(),
                                type_defs: interface.type_defs,
                                funcs: interface.funcs,
                            }
                        };
                        printing_pkg.interfaces.push(rendered);
                    }
                    wit_parser::WorldItem::Function(func) => {
                        let docs = func.docs.contents.clone();
                        let (params, results) = print_function(resolve, func);
                        printing_pkg.funcs.push(Func {
                            name: func.name.clone(),
                            is_static: false,
                            docs,
                            params,
                            results,
                        });
                    }
                    wit_parser::WorldItem::Type(id) => {
                        let type_def = &resolve.types[*id];
                        match &type_def.kind {
                            TypeDefKind::Record(Record { fields }) => {
                                let mut field_tys = Vec::new();
                                let mut doc_fields = Vec::new();
                                for field in fields {
                                    match field.ty {
                                        Type::Id(inner_id) => {
                                            let check = print_doc_type(
                                                Some(field.name.clone()),
                                                resolve,
                                                &field.ty,
                                            );
                                            field_tys.push(check);
                                            let inner = &resolve.types[inner_id];
                                            let doc_field = DocTypeRef {
                                                name: Some(field.name.clone()),
                                                owner: print_owner(inner.owner, resolve),
                                                docs: field.docs.contents.clone(),
                                                ty: if let Some(_) = inner.name.clone() {
                                                    Some(TypeDef::Type(Box::new(DocTypeRef {
                                                        name: inner.name.clone(),
                                                        owner: print_owner(inner.owner, resolve),
                                                        docs: inner.docs.contents.clone(),
                                                        ty: None,
                                                    })))
                                                } else {
                                                    Some(TypeDef::from_type(
                                                        &field.ty,
                                                        Some(resolve),
                                                    ))
                                                },
                                            };
                                            doc_fields.push(doc_field);
                                        }
                                        _ => {
                                            let doc_field = DocTypeRef {
                                                name: Some(field.name.clone()),
                                                owner: None,
                                                docs: field.docs.contents.clone(),
                                                ty: Some(TypeDef::from_type(
                                                    &field.ty,
                                                    Some(resolve),
                                                )),
                                            };
                                            doc_fields.push(doc_field);
                                            field_tys.push(print_doc_type(
                                                Some(field.name.clone()),
                                                resolve,
                                                &field.ty,
                                            ))
                                        }
                                    }
                                }
                                printing_pkg.type_defs.doc_types.push(DocType {
                                    kind: TypeDef::Record(Box::new(doc_fields)),
                                    owner: None,
                                    name: type_def.name.clone(),
                                    docs: type_def.docs.contents.clone(),
                                });
                            }
                            TypeDefKind::Resource => {
                                let resource = print_resource(
                                    resolve,
                                    *id,
                                    resource_funcs.get(&id).unwrap_or(&Vec::new()),
                                );
                                printing_pkg.type_defs.doc_types.push(resource);
                            }
                            TypeDefKind::Handle(_) => unreachable!(),
                            TypeDefKind::Flags(Flags { flags }) => {
                                printing_pkg.type_defs.doc_types.push(DocType {
                                    kind: TypeDef::Flags(
                                        flags
                                            .iter()
                                            .map(|f| Flag {
                                                name: f.name.clone(),
                                                docs: f.docs.contents.clone(),
                                            })
                                            .collect(),
                                    ),
                                    owner: None,
                                    name: type_def.name.clone(),
                                    docs: type_def.docs.contents.clone(),
                                })
                            }
                            TypeDefKind::Tuple(Tuple { types }) => {
                                let mut tys = Vec::new();
                                for ty in types {
                                    let type_ref = DocTypeRef::from_type(*ty, resolve);
                                    tys.push(type_ref);
                                }
                                printing_pkg.type_defs.doc_types.push(DocType {
                                    name: type_def.name.clone(),
                                    kind: TypeDef::Tuple(Box::new(tys)),
                                    owner: None,
                                    docs: type_def.docs.contents.clone(),
                                })
                            }
                            TypeDefKind::Variant(Variant { cases }) => {
                                let mut case_types = Vec::new();
                                let mut doc_cases = Vec::new();
                                for case in cases {
                                    if let Some(ty) = case.ty {
                                        match ty {
                                            Type::Id(id) => {
                                                let child_ty = &resolve.types[id];
                                                let owner = print_owner(child_ty.owner, resolve);
                                                match child_ty.kind {
                                                    TypeDefKind::Handle(handle) => match handle {
                                                        Handle::Own(owned) => {
                                                            let owned_ty = &resolve.types[owned];
                                                            let owner = print_owner(
                                                                owned_ty.owner,
                                                                &resolve,
                                                            );
                                                            let doc_case = DocTypeRef {
                                                                name: Some(case.name.clone()),
                                                                owner: None,
                                                                docs: owned_ty
                                                                    .docs
                                                                    .contents
                                                                    .clone(),
                                                                ty: Some(TypeDef::from_type(
                                                                    &ty, None,
                                                                )),
                                                            };
                                                            doc_cases.push(doc_case);
                                                            case_types.push(DocType {
                                                                kind: TypeDef::Variant(Box::new(
                                                                    vec![],
                                                                )),
                                                                owner: owner.clone(),
                                                                name: None,
                                                                docs: case.docs.contents.clone(),
                                                            })
                                                        }
                                                        Handle::Borrow(borrow) => {
                                                            let borrowed_ty =
                                                                &resolve.types[borrow];
                                                            let owner = print_owner(
                                                                borrowed_ty.owner,
                                                                &resolve,
                                                            );
                                                            let doc_case = DocTypeRef {
                                                                name: Some(case.name.clone()),
                                                                docs: borrowed_ty
                                                                    .docs
                                                                    .contents
                                                                    .clone(),
                                                                owner: None,
                                                                ty: Some(TypeDef::from_type(
                                                                    &ty, None,
                                                                )),
                                                            };
                                                            doc_cases.push(doc_case);
                                                            case_types.push(DocType {
                                                                kind: TypeDef::Variant(Box::new(
                                                                    vec![],
                                                                )),
                                                                owner: owner.clone(),
                                                                name: None,
                                                                docs: case.docs.contents.clone(),
                                                            })
                                                        }
                                                    },
                                                    _ => {
                                                        if let Some(_) = child_ty.name.clone() {
                                                            let doc_case = DocTypeRef {
                                                                name: Some(case.name.clone()),
                                                                docs: child_ty
                                                                    .docs
                                                                    .contents
                                                                    .clone(),
                                                                owner: None,
                                                                ty: Some(TypeDef::from_type(
                                                                    &ty, None,
                                                                )),
                                                            };
                                                            doc_cases.push(doc_case);
                                                            case_types.push(DocType {
                                                                kind: TypeDef::Variant(Box::new(
                                                                    vec![],
                                                                )),
                                                                owner: owner.clone(),
                                                                name: Some(case.name.clone()),
                                                                docs: case.docs.contents.clone(),
                                                            });
                                                        }
                                                    }
                                                }
                                            }
                                            _ => {
                                                let doc_case = DocTypeRef {
                                                    name: Some(case.name.clone()),
                                                    owner: None,
                                                    docs: case.docs.contents.clone(),
                                                    ty: Some(TypeDef::from_type(&ty, None)),
                                                };
                                                doc_cases.push(doc_case);
                                                case_types.push(print_doc_type(None, resolve, &ty));
                                            }
                                        }
                                    } else {
                                        let doc_case = DocTypeRef {
                                            name: Some(case.name.clone()),
                                            docs: case.docs.contents.clone(),
                                            owner: None,
                                            ty: None,
                                        };
                                        doc_cases.push(doc_case);
                                        case_types.push(DocType {
                                            kind: TypeDef::Variant(Box::new(vec![])),
                                            owner: None,
                                            name: Some(case.name.clone()),
                                            docs: case.docs.contents.clone(),
                                        });
                                    }
                                }
                                printing_pkg.type_defs.doc_types.push(DocType {
                                    name: type_def.name.clone(),
                                    kind: TypeDef::Variant(Box::new(doc_cases)),
                                    owner: None,
                                    docs: type_def.docs.contents.clone(),
                                });
                            }
                            TypeDefKind::Enum(Enum { cases }) => {
                                let mut case_types = Vec::new();
                                let mut doc_cases = Vec::new();
                                for case in cases {
                                    doc_cases.push(EnumCase {
                                        name: case.name.clone(),
                                        docs: case.docs.contents.clone(),
                                    });
                                    case_types.push(DocType {
                                        name: Some(case.name.clone()),
                                        kind: TypeDef::Primitive(Box::new(DocPrimitive::Bool)),
                                        owner: None,
                                        docs: case.docs.contents.clone(),
                                    });
                                }
                                printing_pkg.type_defs.doc_types.push(DocType {
                                    name: type_def.name.clone(),
                                    kind: TypeDef::Enum(doc_cases),
                                    owner: None,
                                    docs: type_def.docs.contents.clone(),
                                });
                            }
                            TypeDefKind::Option(inner) => {
                                printing_pkg.type_defs.doc_types.push(DocType {
                                    kind: TypeDef::Option(Box::new(DocTypeRef::from_type(
                                        *inner, resolve,
                                    ))),
                                    owner: None,
                                    name: type_def.name.clone(),
                                    docs: type_def.docs.contents.clone(),
                                })
                            }
                            TypeDefKind::Result(Result_ { ok, err }) => {
                                printing_pkg.type_defs.doc_types.push(DocType {
                                    kind: TypeDef::Result(Box::new(ResultTypes {
                                        ok: if let Some(ok_ty) = &ok {
                                            Some(DocTypeRef::from_type(*ok_ty, resolve))
                                        } else {
                                            None
                                        },
                                        error: if let Some(err_ty) = &err {
                                            Some(DocTypeRef::from_type(*err_ty, resolve))
                                        } else {
                                            None
                                        },
                                    })),
                                    owner: None,
                                    name: type_def.name.clone(),
                                    docs: type_def.docs.contents.clone(),
                                })
                            }
                            TypeDefKind::List(ty) => {
                                printing_pkg.type_defs.doc_types.push(DocType {
                                    kind: TypeDef::List(Box::new(DocTypeRef::from_type(
                                        *ty, resolve,
                                    ))),
                                    owner: None,
                                    name: type_def.name.clone(),
                                    docs: type_def.docs.contents.clone(),
                                })
                            }
                            TypeDefKind::Future(_) => unreachable!(),
                            TypeDefKind::Stream(_) => unreachable!(),
                            TypeDefKind::Type(ty) => {
                                printing_pkg.type_defs.doc_types.push(print_doc_type(
                                    type_def.name.clone(),
                                    resolve,
                                    &ty,
                                ));
                            }
                            TypeDefKind::Unknown => unreachable!(),
                        }
                    }
                }
            }

            for (_, item) in &world.exports {
                match item {
                    wit_parser::WorldItem::Interface(id) => {
                        let iface = &resolve.interfaces[*id];
                        let package = &resolve.packages[iface.package.unwrap()];
                        let docs = iface.docs.contents.clone();
                        let interface = print_interface(&resolve, *id);
                        let rendered = if let Some(name) = iface.name.clone() {
                            Interface {
                                direction: Direction::Export,
                                package: Package {
                                    namespace: package.name.namespace.clone(),
                                    name: package.name.name.clone(),
                                },
                                docs,
                                name,
                                type_defs: interface.type_defs,
                                funcs: interface.funcs,
                            }
                        } else {
                            Interface {
                                direction: Direction::Export,
                                package: Package {
                                    namespace: package.name.namespace.clone(),
                                    name: package.name.name.clone(),
                                },
                                docs,
                                name: "".to_string(),
                                type_defs: interface.type_defs,
                                funcs: interface.funcs,
                            }
                        };
                        printing_pkg.interfaces.push(rendered);
                    }
                    wit_parser::WorldItem::Function(func) => {
                        let docs = func.docs.contents.clone();
                        let (params, results) = print_function(resolve, func);
                        printing_pkg.funcs.push(Func {
                            name: func.name.clone(),
                            is_static: false,
                            docs,
                            params,
                            results,
                        });
                    }
                    wit_parser::WorldItem::Type(_) => {
                        // Appears Not Possible to export types from world
                    }
                }
            }
            printing_pkg
        }
    }
}

fn register_resource_func(f: &Function) -> Option<TypeId> {
    match f.kind {
        FunctionKind::Freestanding => None,
        FunctionKind::Method(id) | FunctionKind::Constructor(id) | FunctionKind::Static(id) => {
            Some(id)
        }
    }
}

fn is_false(b: &bool) -> bool {
    !b
}
