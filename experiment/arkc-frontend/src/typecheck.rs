mod call;
mod control;
mod expr;
mod function;
mod lookup;
mod stmt;

use call::check_expr_call;
use control::check_expr_return;
use expr::check_expr;
use function::{add_local, check_lit_float, check_lit_int, TypeCheck, VarManager};
use stmt::check_stmt;

use crate::{
    sema::{AnalysisData, FctDefinition, FctParent, Sema},
    sym::ModuleSymTable,
};

pub fn check(sa: &mut Sema) {
    //let mut lazy_context_class_creation = Vec::new();
    //let mut lazy_lambda_creation = Vec::new();

    for (_id, fct) in sa.functions.iter() {
        if fct.has_body() {
            check_function(
                sa,
                fct,
                //&mut lazy_context_class_creation,
                //&mut lazy_lambda_creation,
            );
        }
    }
}

fn check_function(
    sa: &Sema,
    fct: &FctDefinition,
    // lazy_context_class_creation: &mut Vec<LazyContextClassCreationData>,
    // lazy_lambda_creation: &mut Vec<LazyLambdaCreationData>,
) {
    let mut analysis = AnalysisData::new();
    let mut symtable = ModuleSymTable::new(sa, fct.module_id);
    let mut vars = VarManager::new();
    let mut context_classes = Vec::new();

    let self_ty = match fct.parent {
        FctParent::None => None,
        //FctParent::Extension(id) => Some(sa.extension(id).ty().clone()),
        //FctParent::Impl(id) => Some(sa.impl_(id).extended_ty()),
        //FctParent::Trait(..) => Some(SourceType::This),
        FctParent::Function => unreachable!(),
    };

    let mut typeck = TypeCheck {
        sa,
        //type_param_defs: fct.type_params(),
        package_id: fct.package_id,
        module_id: fct.module_id,
        file_id: fct.file_id,
        analysis: &mut analysis,
        symtable: &mut symtable,
        param_types: fct.params_with_self().to_owned(),
        return_type: Some(fct.return_type()),
        in_loop: false,
        has_hidden_self_argument: fct.has_hidden_self_argument(),
        is_self_available: fct.has_hidden_self_argument(),
        self_ty,
        is_lambda: false,
        vars: &mut vars,
        //lazy_context_class_creation,
        //lazy_lambda_creation,
        context_classes: &mut context_classes,
        start_context_id: 0,
        needs_context_slot_in_lambda_object: false,
    };

    typeck.check_fct(&fct.ast);
    assert!(fct.analysis.set(analysis).is_ok());
}
