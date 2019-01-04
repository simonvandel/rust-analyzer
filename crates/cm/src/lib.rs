use std::sync::Arc;

pub trait CodeModelProvider {
    fn mk_module(this: Arc<Self>, id: u32) -> Module
        where Self: Sized + 'static
    {
        Module { provider: this, id }
    }

    fn module_name(&self, id: u32) -> String;
    fn module_parent(&self, id: u32) -> Option<u32>;
    fn module_defined_functions(&self, id: u32) -> Vec<u32>;
    fn function_name(&self, id: u32) -> String;
}

type Provider = Arc<dyn CodeModelProvider>;

pub struct Module {
    provider: Provider,
    id: u32,
}

pub struct Function {
    provider: Provider,
    id: u32,
}

impl Module {
    fn name(&self) -> String {
        self.provider.module_name(self.id)
    }
    fn parent(&self) -> Option<Module> {
        self.provider.module_parent(self.id)
            .map(|id| Module { provider: self.provider.clone(), id })
    }
    fn defined_functions(&self) -> Vec<Function> {
        self.provider.module_defined_functions(self.id)
            .into_iter()
            .map(|id| Function { provider: self.provider.clone(), id })
            .collect()
    }
}

impl Function {
    fn name(&self) -> String {
        self.provider.function_name(self.id)
    }
}
