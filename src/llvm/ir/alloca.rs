use super::*;

struct FunctionAlloca<'ctx> {
    entry: BasicBlock<'ctx>,
    locals: HashMap<usize, PointerValue<'ctx>>,
}

impl<'ctx> FunctionAlloca<'ctx> {
    fn new(entry: BasicBlock<'ctx>) -> FunctionAlloca<'ctx> {
        FunctionAlloca { entry, locals: HashMap::new() }
    }

    fn get_local(&self, num: usize) -> Option<&PointerValue<'ctx>> {
        self.locals.get(&num)
    }

    fn store_local<T>(
        &mut self,
        builder: &Builder<'ctx>,
        r#type: T,
        num: usize
    ) -> PointerValue<'ctx>
    where
        T: BasicType<'ctx>
    {
        let Some(builder_pos) = builder.get_insert_block() else { panic!() };
        
        if let Some(first) = self.entry.get_first_instruction() {
            builder.position_before(&first);
        } else {
            builder.position_at_end(self.entry);
        }

        let alloca = builder.build_alloca(r#type, &format!("local{}", num)).unwrap();
        self.locals.insert(num, alloca);

        builder.position_at_end(builder_pos);

        alloca
    }
}

pub struct AllocaStore<'ctx> {
    alloca_map: HashMap<FunctionValue<'ctx>, FunctionAlloca<'ctx>>,
}

impl<'ctx> AllocaStore<'ctx> {
    pub fn new() -> AllocaStore<'ctx> {
        AllocaStore { alloca_map: HashMap::new() }
    }

    pub fn store_fn(
        &mut self,
        function: FunctionValue<'ctx>,
        entry: BasicBlock<'ctx>
    ) {
        self.alloca_map.insert(function, FunctionAlloca::new(entry));
    }

    pub fn get_local(&self, function: FunctionValue<'ctx>, num: usize)
    -> Option<&PointerValue<'ctx>> {
        if let Some(fn_alloca) = self.alloca_map.get(&function) {
            fn_alloca.get_local(num)
        } else {
            panic!("alloca store attempted to access unstored function")
        }
    }

    pub fn store_local<T>(
        &mut self,
        builder: &Builder<'ctx>,
        function: FunctionValue<'ctx>,
        r#type: T,
        num: usize
    ) -> PointerValue<'ctx>
    where
        T: BasicType<'ctx> 
    {
        if let Some(fn_alloca) = self.alloca_map.get_mut(&function) {
            fn_alloca.store_local(builder, r#type, num)
        } else {
            panic!("alloca store attempted to access unstored function")
        }
    }
}
