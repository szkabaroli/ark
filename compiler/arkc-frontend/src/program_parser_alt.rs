use std::cell::OnceCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::io::{Error, Read};
use std::path::PathBuf;
use std::sync::Arc;

use crate::error::msg::ErrorMessage;
use crate::hir::{
    FnDefinition, ModuleDefinition, ModuleDefinitionId, PackageDefinition, PackageDefinitionId,
    PackageName, SourceFile, SourceFileId,
};
use crate::interner::Name;
use crate::report_sym_shadow_span;
use crate::sema::Sema;
use crate::sym::{SymTable, Symbol, SymbolKind};

use parser::ast::{self, visit};
use parser::parser::Parser;
use parser::{compute_line_starts, Span};

pub struct ProgramParser<'a> {
    sa: &'a mut Sema,
    files_to_parse: VecDeque<(SourceFileId, Option<PathBuf>)>,
    packages: HashMap<String, Vec<PathBuf>>,
    //pub module_symtables: HashMap<ModuleDefinitionId, SymTable>,
}

impl<'a> ProgramParser<'a> {
    pub fn new(sa: &mut Sema) -> ProgramParser {
        ProgramParser {
            sa,
            files_to_parse: VecDeque::new(),
            packages: HashMap::new(),
            //module_symtables: HashMap::new(),
        }
    }

    pub fn parse_all(&mut self) {
        self.prepare_packages();
        self.add_all_packages();

        while let Some((file_id, module_path)) = self.files_to_parse.pop_front() {
            let file = self.sa.compilation.file(file_id);
            let package_id = file.package_id;
            let module_id = file.module_id;
            self.parse_file(file, file_id);
            //self.scan_file(package_id, module_id, file_id, module_path, file.ast.get_mut().unwrap());
        }
    }

    fn prepare_packages(&mut self) {
        for (name, file) in &self.sa.args.packages {
            if self.packages.contains_key(name) {
                self.sa
                    .report_without_location(ErrorMessage::PackageAlreadyExists(name.clone()));
            } else {
                let result = self.packages.insert(name.into(), file.clone());
                assert!(result.is_none());
            }
        }
    }

    fn add_all_packages(&mut self) {
        // self.add_stdlib_package();
        // self.add_boots_package();
        self.add_program_package();
        self.add_dependency_packages();
    }

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
        let (package_id, module_id) = add_package(self.sa, PackageName::Program, None);
        self.sa.set_program_module_id(module_id);
        self.sa.set_program_package_id(package_id);

        if self.sa.args.arg_file.is_none() {
            self.sa
                .report_without_location(ErrorMessage::MissingFileArgument);
        } else {
            let arg_file = self.sa.args.arg_file.as_ref().expect("argument expected");
            let arg_file = arg_file.clone();
            let path = PathBuf::from(&arg_file);

            self.add_file_from_filesystem(package_id, module_id, path);
        }
    }

    fn add_dependency_packages(&mut self) {
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
    }

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

    fn add_module_files(
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
    }

    fn add_file(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        path: PathBuf,
        module_path: Option<PathBuf>,
        error_location: Option<(SourceFileId, Span)>,
    ) {
        let result = file_as_string(&path);

        match result {
            Ok(content) => {
                self.add_file_from_string(package_id, module_id, path, content, module_path);
            }

            Err(_) => {
                if let Some((file_id, span)) = error_location {
                    self.sa
                        .report(file_id, span, ErrorMessage::FileNoAccess(path));
                } else {
                    self.sa
                        .report_without_location(ErrorMessage::FileNoAccess(path));
                }
            }
        }
    }

    fn add_file_from_filesystem(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        path: PathBuf,
    ) {
        if path.is_file() {
            let file_path = PathBuf::from(path);
            let module_path = PathBuf::from(file_path.parent().expect("parent missing"));
            self.add_file(package_id, module_id, file_path, Some(module_path), None);
        } else {
            self.sa
                .report_without_location(ErrorMessage::FileDoesNotExist(path));
        }
    }

    fn add_file_from_string(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_path: PathBuf,
        content: String,
        module_path: Option<PathBuf>,
    ) {
        let file_id = add_source_file(self.sa, package_id, module_id, file_path, Arc::new(content));
        self.files_to_parse.push_back((file_id, module_path));
    }

    fn parse_file(&self, file: &SourceFile, file_id: SourceFileId) {
        let contenline_startst = file.content.clone();

        let parser = Parser::from_shared_string(content);

        let (ast, errors) = parser.parse();

        for error in errors {
            self.sa.report(
                file_id,
                error.span,
                ErrorMessage::Custom(error.error.message()),
            );
        }

        assert!(self.sa.compilation.file(file_id).ast.set(ast.clone()).is_ok());
    }
}

fn file_as_string(path: &PathBuf) -> Result<String, Error> {
    let mut content = String::new();
    let mut file = fs::File::open(&path)?;
    file.read_to_string(&mut content)?;
    Ok(content)
}

struct TopLevelDeclaration<'x> {
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
}

fn ensure_name(sa: &Sema, ident: &Option<ast::Ident>) -> Name {
    if let Some(ident) = ident {
        sa.interner.intern(&ident.name_as_string)
    } else {
        sa.interner.intern("<missing name>")
    }
}

impl<'x> TopLevelDeclaration<'x> {
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
}

fn check_if_symbol_exists(
    sa: &Sema,
    file_id: SourceFileId,
    used_names: &mut HashSet<Name>,
    name: Name,
    span: Span,
) {
    if !used_names.insert(name) {
        let name = sa.interner.str(name).to_string();
        sa.report(file_id, span, ErrorMessage::ShadowField(name));
    }
}

pub fn add_source_file(
    sa: &mut Sema,
    package_id: PackageDefinitionId,
    module_id: ModuleDefinitionId,
    path: PathBuf,
    content: Arc<String>,
) -> SourceFileId {
    let line_starts = compute_line_starts(&content);
    let file_id = sa.compilation.source_files.alloc(SourceFile {
        id: OnceCell::new(),
        package_id,
        path,
        content,
        module_id,
        line_starts,
        ast: OnceCell::new(),
    });
    assert!(sa.compilation.file(file_id).id.set(file_id).is_ok());
    file_id
}

fn add_package(
    sa: &mut Sema,
    package_name: PackageName,
    module_name: Option<Name>,
) -> (PackageDefinitionId, ModuleDefinitionId) {
    let module = ModuleDefinition::new_top_level(module_name);
    let module_id = sa.compilation.modules.alloc(module);

    let package = PackageDefinition::new(package_name, module_id);
    let package_id = sa.compilation.packages.alloc(package);

    sa.compilation.modules[module_id].package_id = Some(package_id);

    (package_id, module_id)
}
