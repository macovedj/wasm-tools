use crate::builder::ComponentBuilder;
use crate::encoding::types::{FunctionKey, ValtypeEncoder};
use anyhow::Result;
use indexmap::IndexSet;
use std::collections::HashMap;
use std::mem;
use wasm_encoder::*;
use wit_parser::*;

/// Encodes the given `package` within `resolve` to a binary WebAssembly
/// representation.
///
/// This function is the root of the implementation of serializing a WIT package
/// into a WebAssembly representation. The wasm representation serves two
/// purposes:
///
/// * One is to be a binary encoding of a WIT document which is ideally more
///   stable than the WIT textual format itself.
/// * Another is to provide a clear mapping of all WIT features into the
///   component model through use of its binary representation.
///
/// The `resolve` provided is a "world" of packages and types and such and the
/// `package` argument is an ID within the world provided. The documents within
/// `package` will all be encoded into the binary returned.
///
/// The binary returned can be [`decode`d](crate::decode) to recover the WIT
/// package provided.
pub fn encode(resolve: &Resolve, package: PackageId) -> Result<Vec<u8>> {
    Ok(encode_component(resolve, package)?.finish())
}

/// Exactly like `encode`, except gives an unfinished `ComponentBuilder` in case you need
/// to append anything else before finishing.
pub fn encode_component(resolve: &Resolve, package: PackageId) -> Result<ComponentBuilder> {
    let mut encoder = Encoder {
        component: ComponentBuilder::default(),
        resolve,
        package,
    };
    dbg!(&resolve.interfaces);
    encoder.run()?;
    Ok(encoder.component)
}

struct Encoder<'a> {
    component: ComponentBuilder,
    resolve: &'a Resolve,
    package: PackageId,
}

impl Encoder<'_> {
    fn run(&mut self) -> Result<()> {
        // Build a set of interfaces reachable from this document, including the
        // interfaces in the document itself. This is used to import instances
        // into the component type we're encoding. Note that entire interfaces
        // are imported with all their types as opposed to just the needed types
        // in an interface for this document. That's done to assist with the
        // decoding process where everyone's view of a foreign document agrees
        // notably on the order that types are defined in to assist with
        // roundtripping.
        let mut interfaces = IndexSet::new();
        for (_, id) in self.resolve.packages[self.package].interfaces.iter() {
            self.add_live_interfaces(&mut interfaces, *id);
        }

        // Seed the set of used names with all exported interfaces to ensure
        // that imported interfaces choose different names as the import names
        // aren't used during decoding.
        let mut used_names = IndexSet::new();
        for id in interfaces.iter() {
            let iface = &self.resolve.interfaces[*id];
            if iface.package == Some(self.package) {
                let first = used_names.insert(iface.name.as_ref().unwrap().clone());
                assert!(first);
            }
        }
        for (name, _world) in self.resolve.packages[self.package].worlds.iter() {
            let first = used_names.insert(name.clone());
            assert!(first);
        }

        // Encode all interfaces, foreign and local, into this component type.
        // Local interfaces get their functions defined as well and are
        // exported. Foreign interfaces are imported and only have their types
        // encoded.
        let mut encoder = InterfaceEncoder::new(self.resolve);
        for interface in interfaces {
            encoder.interface = Some(interface);
            let iface = &self.resolve.interfaces[interface];
            let name = self.resolve.id_of(interface).unwrap();
            log::trace!("encoding interface {name}");
            if iface.package == Some(self.package) {
                let idx = encoder.encode_instance(interface)?;
                encoder.outer.export(&name, ComponentTypeRef::Instance(idx));
            } else {
                encoder.push_instance();
                for (_, id) in iface.types.iter() {
                    encoder.encode_valtype(self.resolve, &Type::Id(*id))?;
                }
                let instance = encoder.pop_instance();
                let idx = encoder.outer.type_count();
                encoder.outer.ty().instance(&instance);
                encoder.import_map.insert(interface, encoder.instances);
                encoder.instances += 1;
                encoder.outer.import(&name, ComponentTypeRef::Instance(idx));
            }
        }
        encoder.interface = None;

        for (name, world) in self.resolve.packages[self.package].worlds.iter() {
            let world = &self.resolve.worlds[*world];
            let mut component = InterfaceEncoder::new(self.resolve);
            log::trace!("encoding world {}", world.name);

            for (name, import) in world.imports.iter() {
                let name = self.resolve.name_world_key(name);
                log::trace!("encoding import {name}");
                let ty = match import {
                    WorldItem::Interface(i) => {
                        component.interface = Some(*i);
                        let idx = component.encode_instance(*i)?;
                        ComponentTypeRef::Instance(idx)
                    }
                    WorldItem::Function(f) => {
                        component.interface = None;
                        let idx = component.encode_func_type(self.resolve, f)?;
                        ComponentTypeRef::Func(idx)
                    }
                    WorldItem::Type(t) => {
                        component.interface = None;
                        component.import_types = true;
                        component.encode_valtype(self.resolve, &Type::Id(*t))?;
                        component.import_types = false;
                        continue;
                    }
                };
                component.outer.import(&name, ty);
            }

            for (name, export) in world.exports.iter() {
                let name = self.resolve.name_world_key(name);
                log::trace!("encoding export {name}");
                let ty = match export {
                    WorldItem::Interface(i) => {
                        component.interface = Some(*i);
                        let idx = component.encode_instance(*i)?;
                        ComponentTypeRef::Instance(idx)
                    }
                    WorldItem::Function(f) => {
                        component.interface = None;
                        let idx = component.encode_func_type(self.resolve, f)?;
                        ComponentTypeRef::Func(idx)
                    }
                    WorldItem::Type(_) => unreachable!(),
                };
                component.outer.export(&name, ty);
            }
            let idx = encoder.outer.type_count();
            encoder.outer.ty().component(&component.outer);
            let id = self.resolve.packages[self.package].name.interface_id(&name);
            encoder.outer.export(&id, ComponentTypeRef::Component(idx));
        }

        let ty = self.component.component_type(&encoder.outer);
        let id = self.resolve.packages[self.package].name.interface_id("wit");
        self.component
            .export(&id, ComponentExportKind::Type, ty, None);
        Ok(())
    }

    /// Recursively add all live interfaces reachable from `id` into the
    /// `interfaces` set, and then add `id` to the set.
    fn add_live_interfaces(&self, interfaces: &mut IndexSet<InterfaceId>, id: InterfaceId) {
        if interfaces.contains(&id) {
            return;
        }

        // Other interfaces reachable from `id` are only reachable from defined
        // types, and only when the defined type points to another `Type::Id`.
        // Use this knowledge to filter over all types find find types of this
        // pattern.
        for (_, ty) in self.resolve.interfaces[id].types.iter() {
            let ty = match self.resolve.types[*ty].kind {
                TypeDefKind::Type(Type::Id(id)) => id,
                _ => continue,
            };
            let owner = match self.resolve.types[ty].owner {
                TypeOwner::Interface(id) => id,
                _ => continue,
            };
            if owner != id {
                self.add_live_interfaces(interfaces, owner);
            }
        }
        assert!(interfaces.insert(id));
    }
}

struct InterfaceEncoder<'a> {
    resolve: &'a Resolve,
    outer: ComponentType,
    ty: Option<InstanceType>,
    func_type_map: HashMap<FunctionKey<'a>, u32>,
    type_map: HashMap<TypeId, u32>,
    saved_types: Option<(HashMap<TypeId, u32>, HashMap<FunctionKey<'a>, u32>)>,
    import_map: HashMap<InterfaceId, u32>,
    outer_type_map: HashMap<TypeId, u32>,
    instances: u32,
    import_types: bool,
    interface: Option<InterfaceId>,
}

impl InterfaceEncoder<'_> {
    fn new(resolve: &Resolve) -> InterfaceEncoder<'_> {
        InterfaceEncoder {
            resolve,
            outer: ComponentType::new(),
            ty: None,
            type_map: Default::default(),
            func_type_map: Default::default(),
            import_map: Default::default(),
            outer_type_map: Default::default(),
            instances: 0,
            saved_types: None,
            import_types: false,
            interface: None,
        }
    }

    fn encode_instance(&mut self, interface: InterfaceId) -> Result<u32> {
        self.push_instance();
        let iface = &self.resolve.interfaces[interface];
        for (_, id) in iface.types.iter() {
            self.encode_valtype(self.resolve, &Type::Id(*id))?;
        }
        for (name, func) in iface.functions.iter() {
            let ty = self.encode_func_type(self.resolve, func)?;
            self.ty
                .as_mut()
                .unwrap()
                .export(name, ComponentTypeRef::Func(ty));
        }
        let instance = self.pop_instance();
        let idx = self.outer.type_count();
        self.outer.ty().instance(&instance);
        self.import_map.insert(interface, self.instances);
        self.instances += 1;
        Ok(idx)
    }

    fn push_instance(&mut self) {
        assert!(self.ty.is_none());
        assert!(self.saved_types.is_none());
        self.saved_types = Some((
            mem::take(&mut self.type_map),
            mem::take(&mut self.func_type_map),
        ));
        self.ty = Some(InstanceType::default());
    }

    fn pop_instance(&mut self) -> InstanceType {
        let (types, funcs) = self.saved_types.take().unwrap();
        self.type_map = types;
        self.func_type_map = funcs;
        mem::take(&mut self.ty).unwrap()
    }
}

impl<'a> ValtypeEncoder<'a> for InterfaceEncoder<'a> {
    fn defined_type(&mut self) -> (u32, ComponentDefinedTypeEncoder<'_>) {
        match &mut self.ty {
            Some(ty) => (ty.type_count(), ty.ty().defined_type()),
            None => (self.outer.type_count(), self.outer.ty().defined_type()),
        }
    }
    fn define_function_type(&mut self) -> (u32, ComponentFuncTypeEncoder<'_>) {
        match &mut self.ty {
            Some(ty) => (ty.type_count(), ty.ty().function()),
            None => (self.outer.type_count(), self.outer.ty().function()),
        }
    }
    fn export_type(&mut self, index: u32, name: &'a str) -> Option<u32> {
        match &mut self.ty {
            Some(ty) => {
                assert!(!self.import_types);
                let ret = ty.type_count();
                ty.export(name, ComponentTypeRef::Type(TypeBounds::Eq(index)));
                Some(ret)
            }
            None => {
                let ret = self.outer.type_count();
                if self.import_types {
                    self.outer
                        .import(name, ComponentTypeRef::Type(TypeBounds::Eq(index)));
                } else {
                    self.outer
                        .export(name, ComponentTypeRef::Type(TypeBounds::Eq(index)));
                }
                Some(ret)
            }
        }
    }
    fn type_map(&mut self) -> &mut HashMap<TypeId, u32> {
        &mut self.type_map
    }
    fn interface(&self) -> Option<InterfaceId> {
        self.interface
    }
    fn import_type(&mut self, owner: InterfaceId, id: TypeId) -> u32 {
        let ty = &self.resolve.types[id];
        let instance = self.import_map[&owner];
        let outer_idx = *self.outer_type_map.entry(id).or_insert_with(|| {
            let ret = self.outer.type_count();
            self.outer.alias(Alias::InstanceExport {
                instance,
                name: ty.name.as_ref().unwrap(),
                kind: ComponentExportKind::Type,
            });
            ret
        });
        match &mut self.ty {
            Some(ty) => {
                let ret = ty.type_count();
                ty.alias(Alias::Outer {
                    count: 1,
                    index: outer_idx,
                    kind: ComponentOuterAliasKind::Type,
                });
                ret
            }
            None => outer_idx,
        }
    }
    fn func_type_map(&mut self) -> &mut HashMap<FunctionKey<'a>, u32> {
        &mut self.func_type_map
    }
}
