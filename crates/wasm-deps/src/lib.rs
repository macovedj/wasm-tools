use anyhow::{bail, Result};
use wasmparser::{
    Chunk, ComponentImport, ComponentImportSectionReader, ComponentType, ComponentTypeDeclaration,
    ComponentTypeSectionReader, Parser, Payload,
};

pub struct DepsParser {}

impl DepsParser {
    pub fn new() -> Self {
        Self {}
    }
    pub fn parse_type_imports(
        &mut self,
        parser: ComponentTypeSectionReader,
        deps: &mut Vec<String>,
    ) -> Result<()> {
        for ty in parser.into_iter_with_offsets() {
            let (_offset, ty) = ty?;
            match ty {
                ComponentType::Component(decls) => {
                    for decl in decls.into_vec() {
                        match decl {
                            ComponentTypeDeclaration::Type(ty) => match ty {
                                ComponentType::Component(cdecls) => {
                                    for cdecl in cdecls.into_vec() {
                                        match cdecl {
                                            ComponentTypeDeclaration::Import(import) => {
                                                let name = import.name.as_str().to_owned();
                                                deps.push(name);
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                                ComponentType::Instance(idecls) => {
                                    for idecl in idecls.into_vec() {}
                                }
                                _ => {}
                            },
                            ComponentTypeDeclaration::Import(_) => {}
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    pub fn parse_imports<'a>(
        &mut self,
        parser: ComponentImportSectionReader<'a>,
        deps: &mut Vec<ComponentImport<'a>>,
    ) -> Result<()> {
        for import in parser.into_iter_with_offsets() {
            let (_, imp) = import.unwrap().clone();
            deps.push(imp);
        }
        Ok(())
    }

    pub fn parse<'a>(&mut self, mut bytes: &'a [u8]) -> Result<Vec<ComponentImport<'a>>> {
        let mut parser = Parser::new(0);
        let mut _consumed = 0;
        let mut deps = Vec::new();
        loop {
            let payload = match parser.parse(bytes, true)? {
                Chunk::NeedMoreData(_) => unreachable!(),
                Chunk::Parsed { payload, consumed } => {
                    bytes = &bytes[consumed..];
                    payload
                }
            };
            match payload {
                Payload::ComponentImportSection(s) => {
                    self.parse_imports(s, &mut deps)?;
                }
                Payload::CodeSectionStart {
                    count: _,
                    range: _,
                    size: _,
                } => {
                    parser.skip_section();
                }
                Payload::ModuleSection { range, .. } => {
                    let offset = range.end - range.start;
                    if offset > bytes.len() {
                        bail!("invalid module or component section range");
                    }
                    bytes = &bytes[offset..];
                }
                Payload::ComponentSection { range, .. } => {
                    let offset = range.end - range.start;
                    if offset > bytes.len() {
                        bail!("invalid module or component section range");
                    }
                    bytes = &bytes[offset..];
                }
                Payload::End(_) => {
                    break;
                }
                _ => {}
            }
        }
        Ok(deps)
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
