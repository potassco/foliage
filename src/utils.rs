pub trait FindFunctionDeclaration
{
	fn find_function_declaration(&self, name: &str, arity: usize)
		-> std::rc::Rc<crate::FunctionDeclaration>;
}

pub trait FindPredicateDeclaration
{
	fn find_predicate_declaration(&self, name: &str, arity: usize)
		-> std::rc::Rc<crate::PredicateDeclaration>;
}

pub trait FindVariableDeclaration
{
	fn find_variable_declaration(&self, name: &str) -> std::rc::Rc<crate::VariableDeclaration>;
}

pub struct FreeVariableDeclarations
{
	variable_declarations: std::cell::RefCell<crate::VariableDeclarations>,
}

impl FreeVariableDeclarations
{
	pub fn new() -> Self
	{
		Self
		{
			variable_declarations: std::cell::RefCell::new(vec![]),
		}
	}
}

pub struct BoundVariableDeclarations<'p>
{
	parent: &'p VariableDeclarationStackLayer<'p>,
	variable_declarations: std::rc::Rc<crate::VariableDeclarations>,
}

impl<'p> BoundVariableDeclarations<'p>
{
	pub fn new(parent: &'p VariableDeclarationStackLayer<'p>,
		variable_declarations: std::rc::Rc<crate::VariableDeclarations>) -> Self
	{
		Self
		{
			parent,
			variable_declarations,
		}
	}
}

pub enum VariableDeclarationStackLayer<'p>
{
	Free(FreeVariableDeclarations),
	Bound(BoundVariableDeclarations<'p>),
}

impl<'p> VariableDeclarationStackLayer<'p>
{
	pub fn free() -> Self
	{
		Self::Free(FreeVariableDeclarations::new())
	}

	pub fn bound(parent: &'p VariableDeclarationStackLayer<'p>,
		variable_declarations: std::rc::Rc<crate::VariableDeclarations>) -> Self
	{
		Self::Bound(BoundVariableDeclarations::new(parent, variable_declarations))
	}

	pub fn find(&self, variable_name: &str) -> Option<std::rc::Rc<crate::VariableDeclaration>>
	{
		match self
		{
			VariableDeclarationStackLayer::Free(free) =>
			{
				if let Some(variable_declaration) = free.variable_declarations.borrow().iter()
					.find(|x| x.name == variable_name)
				{
					return Some(std::rc::Rc::clone(&variable_declaration));
				}

				None
			},
			VariableDeclarationStackLayer::Bound(bound) =>
			{
				if let Some(variable_declaration) = bound.variable_declarations.iter()
					.find(|x| x.name == variable_name)
				{
					return Some(std::rc::Rc::clone(&variable_declaration));
				}

				bound.parent.find(variable_name)
			},
		}
	}

	pub fn find_or_create(&self, variable_name: &str) -> std::rc::Rc<crate::VariableDeclaration>
	{
		match self
		{
			VariableDeclarationStackLayer::Free(free) =>
			{
				if let Some(variable_declaration) = free.variable_declarations.borrow().iter()
					.find(|x| x.name == variable_name)
				{
					return std::rc::Rc::clone(&variable_declaration);
				}

				let variable_declaration = crate::VariableDeclaration
				{
					name: variable_name.to_owned(),
				};
				let variable_declaration = std::rc::Rc::new(variable_declaration);

				free.variable_declarations.borrow_mut()
					.push(std::rc::Rc::clone(&variable_declaration));

				variable_declaration
			},
			VariableDeclarationStackLayer::Bound(bound) =>
			{
				if let Some(variable_declaration) = bound.variable_declarations.iter()
					.find(|x| x.name == variable_name)
				{
					return std::rc::Rc::clone(&variable_declaration);
				}

				bound.parent.find_or_create(variable_name)
			},
		}
	}

	#[cfg(test)]
	pub fn free_variable_declarations_do<F, G>(&self, f: F) -> G
	where
		F: Fn(&crate::VariableDeclarations) -> G
	{
		match self
		{
			VariableDeclarationStackLayer::Free(free) => f(&free.variable_declarations.borrow()),
			VariableDeclarationStackLayer::Bound(bound)
				=> bound.parent.free_variable_declarations_do(f),
		}
	}
}
