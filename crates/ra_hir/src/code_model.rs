use std::sync::Arc;

use ra_db::SourceRootId;

use crate::db::HirDatabase;

// We need to bootstrap somehow...
// Let's pick the first module.
pub fn hir_model<H: HirDatabase + 'static>(db: H) -> cm::Module {
    let source_root_id = SourceRootId(0);
    let module_tree = db.module_tree(source_root_id).unwrap();
    let module_id = module_tree.modules().next().unwrap();
    let hir_model = HirModel(db);
    let hir_model = Arc::new(hir_model);
    <HirModel<H> as cm::CodeModelProvider>::mk_module(hir_model, module_id.idx)
}

struct HirModel<T>(T);

impl<T: HirDatabase> cm::CodeModelProvider for HirModel<T> {
    fn module_name(&self, id: u32) -> String {
        let source_root_id = SourceRootId(0);
        let module_id = crate::arena::Id::new(id);
        let m = crate::Module::new(&self.0, source_root_id, module_id).unwrap();

        let name = m.name().unwrap();
        name.to_string()
    }
    fn module_parent(&self, id: u32) -> Option<u32> {
        let source_root_id = SourceRootId(0);
        let module_id = crate::arena::Id::new(id);
        let m = crate::Module::new(&self.0, source_root_id, module_id).unwrap();
        m.parent().map(|it| it.module_id.idx)
    }
    fn module_defined_functions(&self, id: u32) -> Vec<u32> {
        let source_root_id = SourceRootId(0);
        let module_id = crate::arena::Id::new(id);
        let m = crate::Module::new(&self.0, source_root_id, module_id).unwrap();
        m.scope(&self.0).unwrap().entries()
            .filter_map(|(_, res)| res.def_id.take_values())
            .map(|def_id| def_id.0)
            .collect()
    }
    fn function_name(&self, id: u32) -> String {
        let def_id = crate::DefId(id);
        let fn_def = crate::Function::new(def_id);
        fn_def.signature_info(&self.0).unwrap().name
    }
}
