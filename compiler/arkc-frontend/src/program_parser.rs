use std::collections::{HashMap, VecDeque};
use std::fs;
use std::io::{Error, Read};
use std::path::PathBuf;
use std::sync::Arc;

use crate::compilation::{Module, ModuleId, ModuleName};
use crate::error::msg::ErrorMessage;
use crate::sema::Sema;

use parser::{ast, ast::dump::dump_file};
use parser::parser::NodeIdGenerator;
use parser::{Parser, SourceFile, SourceFileId, Span};

pub struct ProgramParser<'a> {
    sa: &'a mut Sema,
    files_to_parse: VecDeque<SourceFileId>,
}

impl<'a> ProgramParser<'a> {
    pub fn new(sa: &'a mut Sema) -> ProgramParser {
        ProgramParser {
            sa,
            files_to_parse: VecDeque::new(),
        }
    }

    pub fn parse_all(&mut self) -> Vec<Arc<ast::File>> {
        // self.prepare_packages();

        // self.add_stdlib_package();
        // self.add_boots_package();
        self.add_program_package();
        // self.add_dependency_packages();

        let mut asts = vec![];
        let id_gen = Arc::from(NodeIdGenerator::new());

        while let Some(file_id) = self.files_to_parse.pop_front() {
            let file = self.sa.compilation.file(file_id);
            let ast = self.parse_file(id_gen.clone(), file);
            dump_file(&ast);
            asts.push(ast);
        }

        asts
    }

    /*fn prepare_packages(&mut self) {
        for (name, file) in &self.sa.args.packages {
            if self.packages.contains_key(name) {
                self.sa
                    .report_without_location(ErrorMessage::PackageAlreadyExists(name.clone()));
            } else {
                let result = self.packages.insert(name.into(), file.clone());
                assert!(result.is_none());
            }
        }
    }*/

    /*fn add_stdlib_package(&mut self) {
        let stdlib_name = "std";
        let stdlib_iname = self.sa.interner.intern(stdlib_name);
        let (package_id, module_id) = add_package(self.sa, PackageName::Stdlib, Some(stdlib_iname));
        self.sa
            .package_names
            .insert(stdlib_name.to_string(), package_id);
        self.sa.set_stdlib_module_id(module_id);
        self.sa.set_stdlib_package_id(package_id);

        if let Some(stdlib) = self.packages.remove(stdlib_name) {
            self.add_file_from_filesystem(package_id, module_id, PathBuf::from(stdlib));
        } else {
            let stdlib_file = format!("stdlib{}stdlib.dora", std::path::MAIN_SEPARATOR);
            let file_path = PathBuf::from(stdlib_file);
            let module_path = PathBuf::from(file_path.parent().expect("parent missing"));
            self.add_bundled_file(package_id, module_id, file_path, module_path);
        }
    }*/

    /*fn add_bundled_file(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_path: PathBuf,
        module_path: PathBuf,
    ) {
        let content = self.get_bundled_file(&file_path);
        self.add_file_from_string(
            package_id,
            module_id,
            file_path,
            content.into(),
            Some(module_path),
            FileLookup::Bundle,
        );
    }*/

    /*fn get_bundled_file(&self, path: &Path) -> &'static str {
        for (name, content) in STDLIB {
            if *name == path.to_string_lossy() {
                return *content;
            }
        }

        for (bundled_file_path, _) in STDLIB {
            eprintln!("\t{}", bundled_file_path);
        }
        panic!("can't find file {} in bundle.", path.display())
    }*/

    //fn add_boots_package(&mut self) {
    //    let boots_name: String = "boots".into();
    //    if let Some(boots_path) = self.packages.remove(&boots_name) {
    //        let interned_boots_name = self.sa.interner.intern(&boots_name);
    //        let (package_id, module_id) =
    //            add_package(self.sa, PackageName::Boots, Some(interned_boots_name));
    //        self.sa.package_names.insert(String::from(boots_name), package_id);
    //        self.sa.set_boots_module_id(module_id);
    //        self.sa.set_boots_package_id(package_id);
    //        self.add_file_from_filesystem(package_id, module_id, PathBuf::from(boots_path));
    //    }
    //}

    fn add_program_package(&mut self) {
        let module_id = add_module(self.sa, ModuleName::Program);
        self.sa.compilation.set_program_module_id(module_id);

        match self.sa.args.files.clone() {
            Some(files) => {
                for path in files.iter() {
                    self.add_file_from_filesystem(module_id, path.clone());
                }
            }
            None => {
                self.sa
                    .report_without_location(ErrorMessage::MissingFileArgument);
            }
        }
    }

    /*fn add_dependency_packages(&mut self) {
        let packages = std::mem::replace(&mut self.packages, HashMap::new());

        for (name, paths) in packages {
            let iname = self.sa.interner.intern(&name);
            let package_name = PackageName::External(name.clone());
            let (package_id, module_id) = add_package(self.sa, package_name, Some(iname));
            self.sa.compilation.package_names.insert(name, package_id);

            for path in paths {
                self.add_file_from_filesystem(package_id, module_id, path);
            }
        }
    }*/

    /*fn scan_file(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        module_path: Option<PathBuf>,
        ast: &ast::File,
    ) {
        let module_table = self
            .module_symtables
            .remove(&module_id)
            .unwrap_or(SymTable::new());

        let mut decl_discovery = TopLevelDeclaration {
            sa: self.sa,
            package_id,
            module_id,
            file_id,
            external_modules: Vec::new(),
            module_table,
            module_symtables: &mut self.module_symtables,
        };

        decl_discovery.visit_file(ast);

        let module_table = decl_discovery.module_table;

        if !decl_discovery.external_modules.is_empty() {
            for external_module_id in decl_discovery.external_modules {
                self.add_module_files(package_id, external_module_id, module_path.clone());
            }
        }

        self.module_symtables.insert(module_id, module_table);
    }*/

    /*fn add_module_files(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        module_path: Option<PathBuf>,
    ) {
        let module = self.sa.compilation.module(module_id);
        let node = module.ast.clone().unwrap();
        let file_id = module.file_id.expect("missing file_id");

        if let Some(ident) = &node.name {
            let module_path = module_path.expect("missing module_path");

            let mut file_path = module_path.clone();
            file_path.push(format!("{}.ark", ident.name_as_string));

            let mut module_path = module_path;
            module_path.push(&ident.name_as_string);

            self.add_file(
                package_id,
                module_id,
                file_path,
                Some(module_path),
                Some((file_id, node.span)),
            );
        }
    }*/

    fn add_file(&mut self, module_id: ModuleId, file_path: PathBuf, error_location: Option<Span>) {
        let result = file_as_string(&file_path);

        match result {
            Ok(content) => {
                let file_id = add_source_file(self.sa, module_id, file_path, Arc::new(content));
                self.files_to_parse.push_back(file_id);
            }

            Err(_) => {
                if let Some(span) = error_location {
                    self.sa.report(span, ErrorMessage::FileNoAccess(file_path));
                } else {
                    self.sa
                        .report_without_location(ErrorMessage::FileNoAccess(file_path));
                }
            }
        }
    }

    fn add_file_from_filesystem(&mut self, module_id: ModuleId, path: PathBuf) {
        if path.is_file() {
            let file_path = PathBuf::from(path);
            self.add_file(module_id, file_path, None);
        } else {
            self.sa
                .report_without_location(ErrorMessage::FileDoesNotExist(path));
        }
    }

    /*fn add_file_from_string(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_path: PathBuf,
        content: String,
        module_path: Option<PathBuf>,
    ) {
        let file_id = add_source_file(self.sa, package_id, module_id, file_path, Arc::new(content));
        self.files_to_parse.push_back((file_id, module_path));
    }*/

    fn parse_file(&self, id_gen: Arc<NodeIdGenerator>, file: &SourceFile) -> Arc<ast::File> {
        let parser = Parser::from_source(id_gen, file);

        let (ast, errors) = parser.parse();

        for error in errors {
            self.sa
                .report(error.span, ErrorMessage::Custom(error.error.message()));
        }
        //assert!(self
        //    .sa
        //    .compilation
        //    .file(file_id)
        //    .ast
        //    .set(ast.clone())
        //    .is_ok());

        ast
    }
}

/*struct TopLevelDeclaration<'x> {
    sa: &'x mut Sema,
    package_id: PackageDefinitionId,
    file_id: SourceFileId,
    module_id: ModuleDefinitionId,
    external_modules: Vec<ModuleDefinitionId>,
    module_table: SymTable,
    module_symtables: &'x mut HashMap<ModuleDefinitionId, SymTable>,
}

impl<'x> visit::Visitor for TopLevelDeclaration<'x> {
    fn visit_fct(&mut self, node: &Arc<ast::FnItem>) {
        let fct = FnDefinition {
            id: None,
            name: ensure_name(self.sa, &node.name),
            package_id: self.package_id,
            module_id: self.module_id,
            file_id: self.file_id,
            ast: node.clone(),
        };

        let fct_id = self.sa.compilation.functions.alloc(fct);
        self.sa.compilation.functions[fct_id].id = Some(fct_id);
        let sym = SymbolKind::Fn(fct_id);

        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }
    }
}*/

/*impl<'x> TopLevelDeclaration<'x> {
    fn insert(&mut self, name: Name, sym: SymbolKind) -> Option<Symbol> {
        self.module_table.insert(name, sym)
    }

    fn insert_optional(
        &mut self,
        ident: &Option<ast::Ident>,
        sym: SymbolKind,
    ) -> Option<(Name, Symbol)> {
        if let Some(ident) = ident {
            let name = self.sa.interner.intern(&ident.name_as_string);
            self.insert(name, sym).map(|sym| (name, sym))
        } else {
            None
        }
    }
}*/

fn file_as_string(path: &PathBuf) -> Result<String, Error> {
    let mut content = String::new();
    let mut file = fs::File::open(&path)?;
    file.read_to_string(&mut content)?;
    Ok(content)
}

pub fn add_source_file(
    sa: &mut Sema,
    module_id: ModuleId,
    path: PathBuf,
    content: Arc<String>,
) -> SourceFileId {
    let file_id = sa
        .compilation
        .source_files
        .alloc(SourceFile::new_shared(path, content));
    assert!(sa.compilation.file(file_id).id.set(file_id).is_ok());
    file_id
}

fn add_module(sa: &mut Sema, module_name: ModuleName) -> ModuleId {
    let module = Module::new(module_name);
    let module_id = sa.compilation.modules.alloc(module);
    module_id
}
