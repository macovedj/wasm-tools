use std::hash::Hash;

use crate::{
    encode_section, ComponentExportKind, ComponentSection, ComponentSectionId, ComponentValType,
    Encode,
};

/// Represents the possible type bounds for type references.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum TypeBounds {
    /// The type is bounded by equality to the type index specified.
    Eq(u32),
    /// This type is a fresh resource type,
    SubResource,
}

impl Encode for TypeBounds {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            Self::Eq(i) => {
                sink.push(0x00);
                i.encode(sink);
            }
            Self::SubResource => sink.push(0x01),
        }
    }
}

/// Represents a reference to a type.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum ComponentTypeRef {
    /// The reference is to a core module type.
    ///
    /// The index is expected to be core type index to a core module type.
    Module(u32),
    /// The reference is to a function type.
    ///
    /// The index is expected to be a type index to a function type.
    Func(u32),
    /// The reference is to a value type.
    Value(ComponentValType),
    /// The reference is to a bounded type.
    Type(TypeBounds),
    /// The reference is to an instance type.
    ///
    /// The index is expected to be a type index to an instance type.
    Instance(u32),
    /// The reference is to a component type.
    ///
    /// The index is expected to be a type index to a component type.
    Component(u32),
}

impl ComponentTypeRef {
    /// Gets the export kind of the reference.
    pub fn kind(&self) -> ComponentExportKind {
        match self {
            Self::Module(_) => ComponentExportKind::Module,
            Self::Func(_) => ComponentExportKind::Func,
            Self::Value(_) => ComponentExportKind::Value,
            Self::Type(..) => ComponentExportKind::Type,
            Self::Instance(_) => ComponentExportKind::Instance,
            Self::Component(_) => ComponentExportKind::Component,
        }
    }
}

impl Encode for ComponentTypeRef {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.kind().encode(sink);

        match self {
            Self::Module(idx) | Self::Func(idx) | Self::Instance(idx) | Self::Component(idx) => {
                idx.encode(sink);
            }
            Self::Value(ty) => ty.encode(sink),
            Self::Type(bounds) => bounds.encode(sink),
        }
    }
}

/// An encoder for the import section of WebAssembly components.
///
/// # Example
///
/// ```rust
/// use wasm_encoder::{Component, ComponentTypeSection, PrimitiveValType, ComponentImportName, ComponentImportSection, ComponentTypeRef, ComponentExportName};
///
/// let mut types = ComponentTypeSection::new();
///
/// // Define a function type of `[string, string] -> string`.
/// types
///   .function()
///   .params(
///     [
///       ("a", PrimitiveValType::String),
///       ("b", PrimitiveValType::String)
///     ]
///   )
///   .result(PrimitiveValType::String);
///
/// // This imports a function named `f` with the type defined above
/// let mut imports = ComponentImportSection::new();
/// let name = ComponentImportName::Kebab("f");
/// imports.import(name, ComponentTypeRef::Func(0));
///
/// let mut component = Component::new();
/// component.section(&types);
/// component.section(&imports);
///
/// let bytes = component.finish();
/// ```
#[derive(Clone, Debug, Default)]
pub struct ComponentImportSection {
    bytes: Vec<u8>,
    num_added: u32,
}

impl ComponentImportSection {
    /// Create a new component import section encoder.
    pub fn new() -> Self {
        Self::default()
    }

    /// The number of imports in the section.
    pub fn len(&self) -> u32 {
        self.num_added
    }

    /// Determines if the section is empty.
    pub fn is_empty(&self) -> bool {
        self.num_added == 0
    }

    /// Define an import in the component import section.
    pub fn import(&mut self, name: impl AsComponentImportName, ty: ComponentTypeRef) -> &mut Self {
        name.as_component_import_name().encode(&mut self.bytes);
        ty.encode(&mut self.bytes);
        self.num_added += 1;
        self
    }
}

impl Encode for ComponentImportSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        encode_section(sink, self.num_added, &self.bytes);
    }
}

impl ComponentSection for ComponentImportSection {
    fn id(&self) -> u8 {
        ComponentSectionId::Import.into()
    }
}

/// The different names that can be assigned to component imports
#[derive(Debug, Copy, Clone)]
pub enum ComponentImportName<'a> {
    /// This is a "kebab name" along the lines of "a-foo-bar"
    Kebab(&'a str),
    /// This is an ID along the lines of "wasi:http/types@2.0"
    Interface(&'a str),
    /// External Url
    Url((&'a str, &'a str, Option<&'a str>)),
    /// Relative path
    Relative((&'a str, &'a str, Option<&'a str>)),
    /// Just Integrity
    Naked((&'a str, &'a str)),
    /// Locked Registry Import
    Locked((&'a str, &'a str)),
    /// Unocked Registry Import
    Unlocked(&'a str),
}

/// The different names that can be assigned to component exports
#[derive(Debug, Copy, Clone)]
pub enum ComponentExportName<'a> {
    /// This is a "kebab name" along the lines of "a-foo-bar"
    Kebab(&'a str),
    /// This is an ID along the lines of "wasi:http/types@2.0"
    Interface(&'a str),
}

impl Encode for ComponentImportName<'_> {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            ComponentImportName::Kebab(name) => {
                sink.push(0x00);
                name.encode(sink);
            }
            ComponentImportName::Interface(name) => {
                sink.push(0x01);
                name.encode(sink);
            }
            ComponentImportName::Url((name, location, integrity)) => {
                sink.push(0x02);
                name.encode(sink);
                location.encode(sink);
                if let Some(integ) = integrity {
                    integ.encode(sink);
                }
            }
            ComponentImportName::Relative((name, location, integrity)) => {
                sink.push(0x03);
                name.encode(sink);
                location.encode(sink);
                if let Some(integ) = integrity {
                    integ.encode(sink);
                }
            }
            ComponentImportName::Naked((name, integrity)) => {
                sink.push(0x04);
                name.encode(sink);
                integrity.encode(sink);
            }
            ComponentImportName::Locked((name, integrity)) => {
                sink.push(0x05);
                let hash = format!("sha256-{}", sha256::digest(integrity.to_string()));
                name.encode(sink);
                hash.encode(sink);
            }
            ComponentImportName::Unlocked(name) => {
                sink.push(0x06);
                name.encode(sink);
            }
        }
    }
}

impl Encode for ComponentExportName<'_> {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            ComponentExportName::Kebab(name) => {
                sink.push(0x00);
                name.encode(sink);
            }
            ComponentExportName::Interface(name) => {
                sink.push(0x01);
                name.encode(sink);
            }
        }
    }
}

/// Helper trait to convert into a `ComponentExportName` either from that type
/// or from a string.
pub trait AsComponentExportName {
    /// Converts this receiver into a `ComponentExportName`.
    fn as_component_export_name(&self) -> ComponentExportName<'_>;
}

impl AsComponentExportName for ComponentExportName<'_> {
    fn as_component_export_name(&self) -> ComponentExportName<'_> {
        *self
    }
}

impl<S: AsRef<str>> AsComponentExportName for S {
    fn as_component_export_name(&self) -> ComponentExportName<'_> {
        let s = self.as_ref();
        if s.contains("/") {
            ComponentExportName::Interface(s)
        } else {
            ComponentExportName::Kebab(s)
        }
    }
}

/// Helper trait to convert into a `ComponentName` either from that type
/// or from a string.
pub trait AsComponentImportName {
    /// Converts this receiver into a `ComponentImportName`.
    fn as_component_import_name(&self) -> ComponentImportName<'_>;
}

impl AsComponentImportName for ComponentImportName<'_> {
    fn as_component_import_name(&self) -> ComponentImportName<'_> {
        *self
    }
}

impl<S: AsRef<str>> AsComponentImportName for S {
    fn as_component_import_name(&self) -> ComponentImportName<'_> {
        let s = self.as_ref();
        if s.contains("/") {
            ComponentImportName::Interface(s)
        } else if s.contains(":") {
            // let integrity = sha256::Hash(s);
            ComponentImportName::Locked((s, s))
        } else {
            ComponentImportName::Kebab(s)
        }
    }
}
