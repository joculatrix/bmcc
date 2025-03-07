use super::*;

/// A type for accessing the local variable bindings of a function. Accessed
/// through the [`AllocaStore`].
struct FunctionAlloca<'ctx> {
    /// Points to a [`BasicBlock`] at the beginning of the function's body;
    /// memory allocations for local variables are placed here. This is only
    /// stored internally for the [`Builder`] to use.
    entry: BasicBlock<'ctx>,
    /// Maps the ordinal number given to each of a function's locals during
    /// [`name resolution`] to a [`PointerValue`] pointing to a local.
    ///
    /// [`name resolution`]: crate::symbol
    locals: HashMap<usize, PointerValue<'ctx>>,
}

impl<'ctx> FunctionAlloca<'ctx> {
    fn new(entry: BasicBlock<'ctx>) -> FunctionAlloca<'ctx> {
        FunctionAlloca { entry, locals: HashMap::new() }
    }

    fn get_local(&self, num: usize) -> Option<&PointerValue<'ctx>> {
        self.locals.get(&num)
    }

    /// Positions the [`Builder`] at the function's entry block, builds an
    /// allocation instruction, return the builder to its previous location,
    /// and returns a pointer to the allocated value.
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

/// A type for managing variable allocations. It stores a hash map of [`FunctionValue`]s
/// mapped to [`FunctionAlloca`]s. Essentially, each function gets its own
/// object to manage memory allocations for its local values.
pub struct AllocaStore<'ctx> {
    alloca_map: HashMap<FunctionValue<'ctx>, FunctionAlloca<'ctx>>,
}

impl<'ctx> AllocaStore<'ctx> {
    pub fn new() -> AllocaStore<'ctx> {
        AllocaStore { alloca_map: HashMap::new() }
    }

    /// Enter a [`FunctionValue`] and the position of its entry block so the
    /// AllocaStore can manage that function's local variable allocations.
    pub fn store_fn(
        &mut self,
        function: FunctionValue<'ctx>,
        entry: BasicBlock<'ctx>
    ) {
        self.alloca_map.insert(function, FunctionAlloca::new(entry));
    }

    /// Get a pointer to a specific local from a specific function.
    ///
    /// Panics if the function doesn't exist in the AllocaStore.
    pub fn get_local(&self, function: FunctionValue<'ctx>, num: usize)
    -> Option<&PointerValue<'ctx>> {
        if let Some(fn_alloca) = self.alloca_map.get(&function) {
            fn_alloca.get_local(num)
        } else {
            panic!("alloca store attempted to access unstored function")
        }
    }

    /// Store a local in a particular function, and receive a pointer to its
    /// location.
    ///
    /// Panics if the function doesn't exist in the AllocaStore.
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
