mod reader;
mod writer;
mod data;
mod program;
mod ty;
mod dumper;
mod builder;

#[cfg(test)]
mod tests;

pub use data::*;
pub use program::{
    AliasData, AliasId, ClassData, ClassField, ClassId, EnumData, EnumId, EnumVariant,
    ExtensionData, ExtensionId, FunctionData, FunctionId, FunctionKind, GlobalData, GlobalId,
    ImplData, ImplId, Intrinsic, ModuleData, ModuleId, NativeFunction, PackageData, PackageId,
    Program, SourceFileData, SourceFileId, StructData, StructField, StructId, TraitData, TraitId,
    TypeParamBound, TypeParamData,
};
pub use reader::*;
pub use writer::*;
pub use builder::*;
pub use ty::*;
pub use dumper::*;