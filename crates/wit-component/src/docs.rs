use crate::DecodedWasm;

use anyhow::anyhow;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fs, mem};
use wit_parser::{
    Docs, Enum, Flags, Function, FunctionKind, Handle, InterfaceId, Record, Resolve, Result_,
    Results, Tuple, Type, TypeDefKind, TypeId, TypeOwner, Variant,
};

fn resource_func(f: &Function) -> Option<TypeId> {
    match f.kind {
        FunctionKind::Freestanding => None,
        FunctionKind::Method(id) | FunctionKind::Constructor(id) | FunctionKind::Static(id) => {
            Some(id)
        }
    }
}

/// Data structure for printing docs JSON
#[derive(Deserialize, Serialize)]
pub struct DocsPrinter {
    // Count of how many items in this current block have been printed to print
    // a blank line between each item, but not the first item.
    any_items: bool,

    // Whether to print doc comments.
    emit_docs: bool,

    print_semicolons: bool,

    interfaces: Vec<Interface>,
}

#[derive(Serialize, Deserialize)]
struct Interface {
    docs: String,
    name: String,
    type_defs: Types,
    funcs: Vec<Func>,
}

const PRINT_SEMICOLONS_DEFAULT: bool = true;
impl Default for DocsPrinter {
    fn default() -> Self {
        Self {
            any_items: false,
            emit_docs: true,
            print_semicolons: match std::env::var("WIT_REQUIRE_SEMICOLONS") {
                Ok(s) => s == "1",
                Err(_) => PRINT_SEMICOLONS_DEFAULT,
            },
            interfaces: Vec::new(),
        }
    }
}

#[derive(Deserialize, Serialize)]
struct Types {
    use_decls: Vec<UsedType>,
    decomposed_types: Vec<DecomposedType>,
}

#[derive(Deserialize, Serialize)]
struct UsedType {
    name: String,
    decl: String,
}

#[derive(Deserialize, Serialize)]
struct Resource {
    name: String,
    methods: Vec<Func>,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
enum Declaration {
    Simple(DecomposedType),
    Resource(Resource),
}
#[derive(Deserialize, Serialize)]
struct DeclaredType {
    name: String,
    docs: String,
    decl: Declaration,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct DecomposedType {
    owner: Option<String>,
    name: Option<String>,
    docs: Option<String>,
    val: String,
    children: Option<Vec<DecomposedType>>,
    methods: Option<Vec<Func>>,
}

impl DocsPrinter {
    /// Print Docs
    pub fn print_docs(&mut self, docs: &Docs) -> String {
        let mut output = String::new();
        if self.emit_docs {
            if let Some(contents) = &docs.contents {
                for line in contents.lines() {
                    output.push_str(line);
                    output.push_str("\n");
                }
            }
        }
        output
    }

    fn print_path_to_interface(&mut self, resolve: &Resolve, interface: InterfaceId) -> String {
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

    fn print_decomposed_resource(
        &mut self,
        resolve: &Resolve,
        id: TypeId,
        funcs: &[&Function],
    ) -> DecomposedType {
        let ty = &resolve.types[id];
        let owner = match ty.owner {
            TypeOwner::World(_) => todo!(),
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
        };
        let mut methods = Vec::new();
        for func in funcs {
            match func.kind {
                FunctionKind::Freestanding => todo!(),
                FunctionKind::Method(_) => {
                    let (params, result) = self.print_function(resolve, func);
                    // methods.push(self.print_function(resolve, func))
                    let docs = if let Some(docs) = func.docs.contents.clone() {
                        docs
                    } else {
                        "".to_string()
                    };
                    methods.push(Func {
                        name: func.name.clone(),
                        docs,
                        params,
                        result,
                    })
                }
                FunctionKind::Static(_) => todo!(),
                FunctionKind::Constructor(_) => todo!(),
            }
        }
        DecomposedType {
            owner,
            name: ty.name.clone(),
            docs: ty.docs.contents.clone(),
            val: "resource".to_string(),
            children: None,
            methods: Some(methods),
        }
    }

    fn print_types<'a>(
        &mut self,
        resolve: &Resolve,
        owner: TypeOwner,
        types: impl Iterator<Item = (&'a str, TypeId)>,
        resource_funcs: &HashMap<TypeId, Vec<&Function>>,
    ) -> Types {
        let mut use_decls = Vec::new();
        let mut decomposed_types = Vec::new();
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
            self.any_items = true;
            let id = match owner {
                TypeOwner::Interface(id) => id,
                // it's only possible to import types from interfaces at
                // this time.
                _ => unreachable!(),
            };
            let path = self.print_path_to_interface(resolve, id);
            for (my_name, other_name) in tys {
                if my_name == other_name {
                    let decl = format!("{path}.{{{other_name}}}");
                    use_decls.push(UsedType {
                        name: my_name.to_string(),
                        decl,
                    });
                }
            }
        }

        for id in types_to_declare {
            self.print_docs(&resolve.types[id].docs);
            let type_def = &resolve.types[id];
            match type_def.kind.clone() {
                TypeDefKind::Record(Record { fields }) => {
                    let mut field_tys = Vec::new();
                    for field in fields {
                        match field.ty {
                            Type::Id(id) => {
                                let child_ty = &resolve.types[id];
                                let owner = match child_ty.owner {
                                    TypeOwner::World(_) => todo!(),
                                    TypeOwner::Interface(id) => &resolve.interfaces[id].name,
                                    TypeOwner::None => &None,
                                };
                                if let Some(name) = child_ty.name.clone() {
                                    field_tys.push(DecomposedType {
                                        owner: owner.clone(),
                                        name: Some(field.name.clone()),
                                        docs: field.docs.contents,
                                        val: name,
                                        children: None,
                                        methods: None,
                                    });
                                }
                            }
                            _ => field_tys.push(self.print_decomposed_type(
                                Some(field.name),
                                resolve,
                                &field.ty,
                            )),
                        }
                    }
                    decomposed_types.push(DecomposedType {
                        owner: None,
                        name: type_def.name.clone(),
                        docs: type_def.docs.contents.clone(),
                        val: "record".to_string(),
                        children: Some(field_tys),
                        methods: None,
                    });
                }
                TypeDefKind::Flags(Flags { flags }) => {
                    let mut flag_types = Vec::new();
                    for flag in flags {
                        flag_types.push(DecomposedType {
                            owner: None,
                            name: Some(flag.name.clone()),
                            docs: flag.docs.contents,
                            val: flag.name.clone(),
                            children: None,
                            methods: None,
                        });
                    }
                    decomposed_types.push(DecomposedType {
                        owner: None,
                        name: type_def.name.clone(),
                        docs: type_def.docs.contents.clone(),
                        val: "flags".to_string(),
                        children: Some(flag_types),
                        methods: None,
                    });
                }
                TypeDefKind::Tuple(Tuple { types }) => {
                    let mut tys = Vec::new();
                    for ty in types {
                        match ty {
                            Type::Id(id) => {
                                let ty = &resolve.types[id];
                                let child_ty = &resolve.types[id];
                                let owner = match ty.owner {
                                    TypeOwner::World(_) => todo!(),
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
                                };
                                if let Some(name) = child_ty.name.clone() {
                                    tys.push(DecomposedType {
                                        owner: owner.clone(),
                                        name: ty.name.clone(),
                                        docs: ty.docs.contents.clone(),
                                        val: name,
                                        children: None,
                                        methods: None,
                                    })
                                }
                            }
                            Type::Bool => {
                                tys.push(DecomposedType {
                                    owner: None,
                                    name: None,
                                    docs: None,
                                    val: "bool".to_string(),
                                    children: None,
                                    methods: None,
                                });
                            }
                            Type::U8 => {
                                tys.push(DecomposedType {
                                    owner: None,
                                    name: None,
                                    docs: None,
                                    val: "u8".to_string(),
                                    children: None,
                                    methods: None,
                                });
                            }
                            Type::U16 => tys.push(DecomposedType {
                                owner: None,
                                name: None,
                                docs: None,
                                val: "u16".to_string(),
                                children: None,
                                methods: None,
                            }),
                            Type::U32 => tys.push(DecomposedType {
                                owner: None,
                                name: None,
                                docs: None,
                                val: "u32".to_string(),
                                children: None,
                                methods: None,
                            }),
                            Type::U64 => tys.push(DecomposedType {
                                owner: None,
                                name: None,
                                docs: None,
                                val: "u64".to_string(),
                                children: None,
                                methods: None,
                            }),
                            Type::S8 => tys.push(DecomposedType {
                                owner: None,
                                name: None,
                                docs: None,
                                val: "s8".to_string(),
                                children: None,
                                methods: None,
                            }),
                            Type::S16 => tys.push(DecomposedType {
                                owner: None,
                                name: None,
                                docs: None,
                                val: "s16".to_string(),
                                children: None,
                                methods: None,
                            }),
                            Type::S32 => tys.push(DecomposedType {
                                owner: None,
                                name: None,
                                docs: None,
                                val: "s32".to_string(),
                                children: None,
                                methods: None,
                            }),
                            Type::S64 => tys.push(DecomposedType {
                                owner: None,
                                name: None,
                                docs: None,
                                val: "s64".to_string(),
                                children: None,
                                methods: None,
                            }),
                            Type::Float32 => tys.push(DecomposedType {
                                owner: None,
                                name: None,
                                docs: None,
                                val: "Float32".to_string(),
                                children: None,
                                methods: None,
                            }),
                            Type::Float64 => tys.push(DecomposedType {
                                owner: None,
                                name: None,
                                docs: None,
                                val: "Float64".to_string(),
                                children: None,
                                methods: None,
                            }),
                            Type::Char => tys.push(DecomposedType {
                                owner: None,
                                name: None,
                                docs: None,
                                val: "Char".to_string(),
                                children: None,
                                methods: None,
                            }),
                            Type::String => tys.push(DecomposedType {
                                owner: None,
                                name: None,
                                docs: None,
                                val: "String".to_string(),
                                children: None,
                                methods: None,
                            }),
                        }
                    }
                    decomposed_types.push(DecomposedType {
                        owner: None,
                        name: type_def.name.clone(),
                        docs: type_def.docs.contents.clone(),
                        val: "tuple".to_string(),
                        children: Some(tys),
                        methods: None,
                    });
                }
                TypeDefKind::Variant(Variant { cases }) => {
                    let mut case_types = Vec::new();
                    for case in cases {
                        if let Some(ty) = case.ty {
                            match ty {
                                Type::Id(id) => {
                                    let child_ty = &resolve.types[id];
                                    let owner = match child_ty.owner {
                                        TypeOwner::World(_) => todo!(),
                                        TypeOwner::Interface(id) => &resolve.interfaces[id].name,
                                        TypeOwner::None => &None,
                                    };
                                    if let Some(name) = child_ty.name.clone() {
                                        case_types.push(DecomposedType {
                                            owner: owner.clone(),
                                            name: Some(case.name.clone()),
                                            docs: case.docs.contents,
                                            val: case.name.clone(),
                                            children: Some(vec![DecomposedType {
                                                owner: owner.clone(),
                                                name: None,
                                                docs: child_ty.docs.contents.clone(),
                                                val: name,
                                                children: None,
                                                methods: None,
                                            }]),
                                            methods: None,
                                        });
                                    }
                                }
                                _ => {
                                    case_types.push(self.print_decomposed_type(None, resolve, &ty));
                                }
                            }
                        } else {
                            case_types.push(DecomposedType {
                                owner: None,
                                name: Some(case.name.clone()),
                                docs: case.docs.contents,
                                val: case.name.clone(),
                                children: None,
                                methods: None,
                            });
                        }
                    }
                    decomposed_types.push(DecomposedType {
                        owner: None,
                        name: type_def.name.clone(),
                        docs: type_def.docs.contents.clone(),
                        val: "variant".to_string(),
                        children: Some(case_types),
                        methods: None,
                    });
                }
                TypeDefKind::Enum(Enum { cases }) => {
                    let mut case_types = Vec::new();
                    for case in cases {
                        case_types.push(DecomposedType {
                            owner: None,
                            name: Some(case.name.clone()),
                            docs: case.docs.contents,
                            val: case.name.clone(),
                            children: None,
                            methods: None,
                        });
                    }
                    decomposed_types.push(DecomposedType {
                        owner: None,
                        name: type_def.name.clone(),
                        docs: type_def.docs.contents.clone(),
                        val: "enum".to_string(),
                        children: Some(case_types),
                        methods: None,
                    });
                }
                TypeDefKind::Resource => {
                    decomposed_types.push(self.print_decomposed_resource(
                        resolve,
                        id,
                        resource_funcs.get(&id).unwrap_or(&Vec::new()),
                    ));
                }
                TypeDefKind::Type(ty) => {
                    decomposed_types.push(self.print_decomposed_type(
                        type_def.name.clone(),
                        resolve,
                        &ty,
                    ));
                }
                TypeDefKind::Handle(_) => todo!(),
                TypeDefKind::Option(ty) => {
                    decomposed_types.push(DecomposedType {
                        owner: None,
                        name: type_def.name.clone(),
                        docs: type_def.docs.contents.clone(),
                        val: "option".to_string(),
                        children: Some(vec![self.print_decomposed_type(
                            type_def.name.clone(),
                            resolve,
                            &ty,
                        )]),
                        methods: None,
                    });
                }
                TypeDefKind::Result(Result_ { ok, err }) => {
                    let mut children = Vec::new();
                    if let Some(ok) = ok {
                        children.push(self.print_decomposed_type(None, resolve, &ok));
                    };
                    if let Some(err) = err {
                        children.push(self.print_decomposed_type(None, resolve, &err));
                    };
                    decomposed_types.push(DecomposedType {
                        owner: None,
                        name: type_def.name.clone(),
                        docs: type_def.docs.contents.clone(),
                        val: "result".to_string(),
                        children: Some(children),
                        methods: None,
                    });
                }
                TypeDefKind::List(ty) => decomposed_types.push(DecomposedType {
                    owner: None,
                    name: type_def.name.clone(),
                    docs: type_def.docs.contents.clone(),
                    val: "list".to_string(),
                    children: Some(vec![self.print_decomposed_type(None, resolve, &ty)]),
                    methods: None,
                }),
                TypeDefKind::Future(_) => todo!(),
                TypeDefKind::Stream(_) => todo!(),
                TypeDefKind::Unknown => todo!(),
            }
        }
        Types {
            use_decls,
            decomposed_types,
        }
    }

    fn print_decomposed_type(
        &mut self,
        name: Option<String>,
        resolve: &Resolve,
        ty: &Type,
    ) -> DecomposedType {
        match ty {
            Type::Bool => DecomposedType {
                owner: None,
                name,
                docs: None,
                val: "bool".to_string(),
                children: None,
                methods: None,
            },
            Type::U8 => DecomposedType {
                owner: None,
                name,
                docs: None,
                val: "u8".to_string(),
                children: None,
                methods: None,
            },
            Type::U16 => DecomposedType {
                owner: None,
                name,
                docs: None,
                val: "u16".to_string(),
                children: None,
                methods: None,
            },
            Type::U32 => DecomposedType {
                owner: None,
                name,
                docs: None,
                val: "u32".to_string(),
                children: None,
                methods: None,
            },
            Type::U64 => DecomposedType {
                owner: None,
                name,
                docs: None,
                val: "u64".to_string(),
                children: None,
                methods: None,
            },
            Type::S8 => DecomposedType {
                owner: None,
                name,
                docs: None,
                val: "s8".to_string(),
                children: None,
                methods: None,
            },
            Type::S16 => DecomposedType {
                owner: None,
                name,
                docs: None,
                val: "s16".to_string(),
                children: None,
                methods: None,
            },
            Type::S32 => DecomposedType {
                owner: None,
                name,
                docs: None,
                val: "s32".to_string(),
                children: None,
                methods: None,
            },
            Type::S64 => DecomposedType {
                owner: None,
                name,
                docs: None,
                val: "s64".to_string(),
                children: None,
                methods: None,
            },
            Type::Float32 => DecomposedType {
                owner: None,
                name,
                docs: None,
                val: "float32".to_string(),
                children: None,
                methods: None,
            },
            Type::Float64 => DecomposedType {
                owner: None,
                name,
                docs: None,
                val: "float64".to_string(),
                children: None,
                methods: None,
            },
            Type::Char => DecomposedType {
                owner: None,
                name,
                docs: None,
                val: "char".to_string(),
                children: None,
                methods: None,
            },
            Type::String => DecomposedType {
                owner: None,
                name,
                docs: None,
                val: "string".to_string(),
                children: None,
                methods: None,
            },
            Type::Id(id) => {
                let ty = &resolve.types[*id];
                let owner = match ty.owner {
                    TypeOwner::World(_) => todo!(),
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
                };
                let decomposed = match &ty.kind {
                    TypeDefKind::Record(Record { fields }) => {
                        let mut decomposed_fields = Vec::new();
                        for field in fields {
                            decomposed_fields.push(self.print_decomposed_type(
                                Some(field.name.clone()),
                                resolve,
                                &field.ty,
                            ))
                        }
                        DecomposedType {
                            owner: owner.clone(),
                            name,
                            docs: ty.docs.contents.clone(),
                            val: "record".to_string(),
                            children: Some(decomposed_fields),
                            methods: None,
                        }
                    }
                    TypeDefKind::Resource => {
                        if let Some(ty_name) = ty.name.clone() {
                            DecomposedType {
                                owner: owner.clone(),
                                name,
                                docs: ty.docs.contents.clone(),
                                val: ty_name,
                                children: None,
                                methods: None,
                            }
                        } else {
                            DecomposedType {
                                owner: owner.clone(),
                                name,
                                docs: ty.docs.contents.clone(),
                                val: "resource".to_string(),
                                children: None,
                                methods: None,
                            }
                        }
                    }
                    TypeDefKind::Handle(handle) => match handle {
                        Handle::Own(owned_id) => {
                            let owned_ty = &resolve.types[*owned_id];
                            let owner = match ty.owner {
                                TypeOwner::World(_) => todo!(),
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
                            };
                            let child = match owned_ty.kind {
                                TypeDefKind::Record(_) => todo!(),
                                TypeDefKind::Resource => DecomposedType {
                                    owner: owner.clone(),
                                    name: None,
                                    docs: owned_ty.docs.contents.clone(),
                                    val: if let Some(name) = owned_ty.name.clone() {
                                        name
                                    } else {
                                        "".to_string()
                                    },
                                    children: None,
                                    methods: None,
                                },
                                TypeDefKind::Handle(_) => todo!(),
                                TypeDefKind::Flags(_) => todo!(),
                                TypeDefKind::Tuple(_) => todo!(),
                                TypeDefKind::Variant(_) => todo!(),
                                TypeDefKind::Enum(_) => todo!(),
                                TypeDefKind::Option(_) => todo!(),
                                TypeDefKind::Result(_) => todo!(),
                                TypeDefKind::List(_) => todo!(),
                                TypeDefKind::Future(_) => todo!(),
                                TypeDefKind::Stream(_) => todo!(),
                                TypeDefKind::Type(ty) => {
                                    self.print_decomposed_type(None, resolve, &ty)
                                }
                                TypeDefKind::Unknown => todo!(),
                            };
                            child
                        }
                        Handle::Borrow(borrowed_id) => {
                            let borrowed_ty = &resolve.types[*borrowed_id];
                            let owner = match borrowed_ty.owner {
                                TypeOwner::World(_) => todo!(),
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
                            };
                            let children = match borrowed_ty.kind {
                                TypeDefKind::Record(_) => todo!(),
                                TypeDefKind::Resource => {
                                    if let Some(borrowed_name) = &borrowed_ty.name {
                                        Some(vec![DecomposedType {
                                            owner: owner.clone(),
                                            name: name.clone(),
                                            docs: borrowed_ty.docs.contents.clone(),
                                            val: borrowed_name.clone(),
                                            children: None,
                                            methods: None,
                                        }])
                                    } else {
                                        None
                                    }
                                }
                                TypeDefKind::Handle(_) => todo!(),
                                TypeDefKind::Flags(_) => todo!(),
                                TypeDefKind::Tuple(_) => todo!(),
                                TypeDefKind::Variant(_) => todo!(),
                                TypeDefKind::Enum(_) => todo!(),
                                TypeDefKind::Option(_) => todo!(),
                                TypeDefKind::Result(_) => todo!(),
                                TypeDefKind::List(_) => todo!(),
                                TypeDefKind::Future(_) => todo!(),
                                TypeDefKind::Stream(_) => todo!(),
                                TypeDefKind::Type(ty) => {
                                    self.print_decomposed_type(None, resolve, &ty).children
                                }
                                TypeDefKind::Unknown => todo!(),
                            };
                            DecomposedType {
                                owner,
                                name,
                                docs: ty.docs.contents.clone(),
                                val: "borrow".to_string(),
                                children,
                                methods: None,
                            }
                        }
                    },
                    TypeDefKind::Flags(Flags { flags }) => {
                        let mut flag_types = Vec::new();
                        for flag in flags {
                            flag_types.push(DecomposedType {
                                owner: None,
                                name: None,
                                docs: flag.docs.contents.clone(),
                                val: flag.name.clone(),
                                children: None,
                                methods: None,
                            });
                        }
                        DecomposedType {
                            owner: None,
                            name: None,
                            docs: ty.docs.contents.clone(),
                            val: "flags".to_string(),
                            children: Some(flag_types),
                            methods: None,
                        }
                    }
                    TypeDefKind::Variant(Variant { cases }) => {
                        let mut case_types = Vec::new();
                        for case in cases {
                            case_types.push(DecomposedType {
                                owner: None,
                                name: None,
                                docs: case.docs.contents.clone(),
                                val: case.name.clone(),
                                children: None,
                                methods: None,
                            });
                        }
                        DecomposedType {
                            owner: None,
                            name: None,
                            docs: ty.docs.contents.clone(),
                            val: "cases".to_string(),
                            methods: None,
                            children: Some(case_types),
                        }
                    }
                    TypeDefKind::Tuple(Tuple { types }) => {
                        dbg!("TUPLE TY");
                        let mut tys = Vec::new();
                        for ty in types {
                            tys.push(self.print_decomposed_type(None, resolve, ty));
                        }
                        DecomposedType {
                            owner: None,
                            name,
                            docs: ty.docs.contents.clone(),
                            val: "tuple".to_string(),
                            children: Some(tys),
                            methods: None,
                        }
                    }
                    TypeDefKind::Enum(Enum { cases }) => {
                        let mut case_types = Vec::new();
                        for case in cases {
                            case_types.push(DecomposedType {
                                owner: None,
                                name: Some(case.name.clone()),
                                docs: case.docs.contents.clone(),
                                val: case.name.clone(),
                                children: None,
                                methods: None,
                            });
                        }
                        DecomposedType {
                            owner: None,
                            name: None,
                            docs: ty.docs.contents.clone(),
                            val: "enum".to_string(),
                            children: Some(case_types),
                            methods: None,
                        }
                    }
                    TypeDefKind::Option(option_ty) => DecomposedType {
                        owner: None,
                        name: None,
                        docs: ty.docs.contents.clone(),
                        val: "option".to_string(),
                        children: Some(vec![self.print_decomposed_type(name, resolve, option_ty)]),
                        methods: None,
                    },
                    TypeDefKind::Result(Result_ { ok, err }) => {
                        let mut result_types = Vec::new();
                        if let Some(ok) = ok {
                            match ok {
                                Type::Id(id) => {
                                    let ok_ty = &resolve.types[*id];
                                    let owner = match ok_ty.owner {
                                        TypeOwner::World(_) => todo!(),
                                        TypeOwner::Interface(id) => {
                                            resolve.interfaces[id].name.clone()
                                        }
                                        TypeOwner::None => None,
                                    };
                                    if let Some(ok_name) = &ok_ty.name {
                                        result_types.push(DecomposedType {
                                            owner,
                                            name: name.clone(),
                                            docs: ok_ty.docs.contents.clone(),
                                            val: ok_name.to_string(),
                                            children: None,
                                            methods: None,
                                        });
                                    }
                                }
                                _ => {
                                    result_types
                                        .push(self.print_decomposed_type(None, resolve, &ok));
                                }
                            }
                        }
                        if let Some(err) = err {
                            match err {
                                Type::Id(id) => {
                                    let err_ty = &resolve.types[*id];
                                    let owner = match err_ty.owner {
                                        TypeOwner::World(_) => todo!(),
                                        TypeOwner::Interface(id) => {
                                            resolve.interfaces[id].name.clone()
                                        }
                                        TypeOwner::None => None,
                                    };
                                    if let Some(err_name) = &err_ty.name {
                                        result_types.push(DecomposedType {
                                            owner,
                                            name: None,
                                            docs: err_ty.docs.contents.clone(),
                                            val: err_name.to_string(),
                                            children: None,
                                            methods: None,
                                        });
                                    }
                                }
                                _ => {
                                    result_types
                                        .push(self.print_decomposed_type(None, resolve, &err));
                                }
                            }
                        }
                        DecomposedType {
                            owner: None,
                            name,
                            docs: ty.docs.contents.clone(),
                            val: "result".to_string(),
                            children: Some(result_types),
                            methods: None,
                        }
                    }
                    TypeDefKind::List(list_ty) => DecomposedType {
                        owner: None,
                        name,
                        docs: ty.docs.contents.clone(),
                        val: "list".to_string(),
                        children: Some(vec![self.print_decomposed_type(None, resolve, &list_ty)]),
                        methods: None,
                    },
                    TypeDefKind::Future(_) => todo!(),
                    TypeDefKind::Stream(_) => todo!(),
                    TypeDefKind::Type(ty) => match ty {
                        Type::Id(id) => {
                            let ty = &resolve.types[*id];
                            let owner = match ty.owner {
                                TypeOwner::World(_) => todo!(),
                                TypeOwner::Interface(id) => resolve.interfaces[id].name.clone(),
                                TypeOwner::None => None,
                            };
                            if let Some(ty_name) = &ty.name {
                                DecomposedType {
                                    owner,
                                    name,
                                    docs: ty.docs.contents.clone(),
                                    val: ty_name.to_string(),
                                    children: None,
                                    methods: None,
                                }
                            } else {
                                DecomposedType {
                                    owner,
                                    name: None,
                                    docs: ty.docs.contents.clone(),
                                    val: "".to_string(),
                                    children: None,
                                    methods: None,
                                }
                            }
                        }
                        _ => self.print_decomposed_type(None, resolve, ty),
                    },
                    TypeDefKind::Unknown => todo!(),
                };
                decomposed
            }
        }
    }

    fn print_function(
        &mut self,
        resolve: &Resolve,
        func: &Function,
    ) -> (Vec<(String, DecomposedType)>, Vec<(String, DecomposedType)>) {
        let mut params = Vec::new();
        let mut ret = Vec::new();

        // Methods don't print their `self` argument
        let params_to_skip = match &func.kind {
            FunctionKind::Method(_) => 1,
            _ => 0,
        };
        for (name, ty) in func.params.iter().skip(params_to_skip) {
            params.push((name.clone(), self.print_decomposed_type(None, resolve, ty)))
        }

        match &func.results {
            Results::Named(rs) => match rs.len() {
                0 => (),
                _ => {
                    for (name, ty) in rs.iter() {
                        ret.push((name.clone(), self.print_decomposed_type(None, resolve, ty)));
                    }
                }
            },
            Results::Anon(ty) => {
                let decomposed = self.print_decomposed_type(None, resolve, ty);
                ret.push(("".to_string(), decomposed));
            }
        }
        (params, ret)
    }

    fn print_interface(&mut self, resolve: &Resolve, id: InterfaceId) -> Iface {
        let prev_items = mem::replace(&mut self.any_items, false);
        let interface = &resolve.interfaces[id];

        let mut resource_funcs = HashMap::new();
        let mut freestanding = Vec::new();
        for (name, func) in interface.functions.iter() {
            if let Some(id) = resource_func(func) {
                resource_funcs.entry(id).or_insert(Vec::new()).push(func);
            } else {
                freestanding.push((name, func));
            }
        }

        let type_defs = self.print_types(
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
            let docs = self.print_docs(&func.docs);
            let (params, result) = self.print_function(resolve, func);
            funcs.push(Func {
                name: name.to_string(),
                docs,
                params,
                result,
            })
        }
        self.any_items = prev_items;
        Iface { type_defs, funcs }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
struct Func {
    name: String,
    docs: String,
    params: Vec<(String, DecomposedType)>,
    result: Vec<(String, DecomposedType)>,
}

struct Iface {
    type_defs: Types,
    funcs: Vec<Func>,
}

/// Print JSON with info for rendering docs
pub fn print_docs(decoded: &DecodedWasm) -> String {
    let mut printer = DocsPrinter::default();
    match decoded {
        DecodedWasm::WitPackage(resolve, package_id) => {
            let pkg = &resolve.packages[*package_id];
            for (name, id) in pkg.interfaces.iter() {
                let docs = printer.print_docs(&resolve.interfaces[*id].docs);
                let interface = printer.print_interface(&resolve, *id);
                let rendered = Interface {
                    docs,
                    name: name.to_string(),
                    type_defs: interface.type_defs,
                    funcs: interface.funcs,
                };
                printer.interfaces.push(rendered);
            }
        }
        DecodedWasm::Component(resolve, _) => {
            let mut names = HashMap::new();
            for (_id, pkg) in resolve.packages.iter() {
                let cnt = names
                    .entry(&pkg.name.name)
                    .or_insert(HashMap::new())
                    .entry(&pkg.name.namespace)
                    .or_insert(0);
                *cnt += 1;
            }
        }
    }
    let strung = serde_json::to_string(&printer.interfaces).unwrap();

    fs::write("./docs.json", strung.clone()).unwrap();
    strung
}
