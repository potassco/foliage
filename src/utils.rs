pub trait FindOrCreateFunctionDeclaration
{
	fn find_or_create_function_declaration(&self, name: &str, arity: usize)
		-> std::rc::Rc<crate::FunctionDeclaration>;
}

pub trait FindOrCreatePredicateDeclaration
{
	fn find_or_create_predicate_declaration(&self, name: &str, arity: usize)
		-> std::rc::Rc<crate::PredicateDeclaration>;
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
	Free(std::cell::RefCell<crate::VariableDeclarations>),
	Bound(BoundVariableDeclarations<'p>),
}

impl<'p> VariableDeclarationStackLayer<'p>
{
	pub fn free() -> Self
	{
		Self::Free(std::cell::RefCell::new(vec![]))
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
			VariableDeclarationStackLayer::Free(free_variable_declarations) =>
			{
				if let Some(variable_declaration) = free_variable_declarations.borrow().iter()
					.find(|x| x.name == variable_name)
				{
					return Some(std::rc::Rc::clone(&variable_declaration));
				}

				None
			},
			VariableDeclarationStackLayer::Bound(bound_variable_declarations) =>
			{
				if let Some(variable_declaration) = bound_variable_declarations
					.variable_declarations.iter()
					.find(|x| x.name == variable_name)
				{
					return Some(std::rc::Rc::clone(&variable_declaration));
				}

				bound_variable_declarations.parent.find(variable_name)
			},
		}
	}

	pub fn find_or_create(&self, variable_name: &str) -> std::rc::Rc<crate::VariableDeclaration>
	{
		match self
		{
			VariableDeclarationStackLayer::Free(free_variable_declarations) =>
			{
				if let Some(variable_declaration) = free_variable_declarations.borrow().iter()
					.find(|x| x.name == variable_name)
				{
					return std::rc::Rc::clone(&variable_declaration);
				}

				let variable_declaration = crate::VariableDeclaration
				{
					name: variable_name.to_owned(),
				};
				let variable_declaration = std::rc::Rc::new(variable_declaration);

				free_variable_declarations.borrow_mut()
					.push(std::rc::Rc::clone(&variable_declaration));

				variable_declaration
			},
			VariableDeclarationStackLayer::Bound(bound_variable_declarations) =>
			{
				if let Some(variable_declaration) = bound_variable_declarations
					.variable_declarations.iter()
					.find(|x| x.name == variable_name)
				{
					return std::rc::Rc::clone(&variable_declaration);
				}

				bound_variable_declarations.parent.find_or_create(variable_name)
			},
		}
	}

	pub fn free_variable_declarations_do_mut<F, G>(&self, f: F) -> G
	where
		F: Fn(&mut crate::VariableDeclarations) -> G
	{
		match self
		{
			VariableDeclarationStackLayer::Free(free_variable_declarations)
				=> f(&mut free_variable_declarations.borrow_mut()),
			VariableDeclarationStackLayer::Bound(bound_variable_declarations)
				=> bound_variable_declarations.parent.free_variable_declarations_do_mut(f),
		}
	}

	pub fn free_variable_declarations_do<F, G>(&self, f: F) -> G
	where
		F: Fn(&crate::VariableDeclarations) -> G
	{
		match self
		{
			VariableDeclarationStackLayer::Free(free_variable_declarations)
				=> f(&free_variable_declarations.borrow()),
			VariableDeclarationStackLayer::Bound(bound_variable_declarations)
				=> bound_variable_declarations.parent.free_variable_declarations_do(f),
		}
	}
}

pub struct Declarations
{
	function_declarations: std::cell::RefCell<crate::FunctionDeclarations>,
	predicate_declarations: std::cell::RefCell<crate::PredicateDeclarations>,
}

impl Declarations
{
	pub fn new() -> Self
	{
		Self
		{
			function_declarations: std::cell::RefCell::new(crate::FunctionDeclarations::new()),
			predicate_declarations: std::cell::RefCell::new(crate::PredicateDeclarations::new()),
		}
	}
}

impl FindOrCreateFunctionDeclaration for Declarations
{
	fn find_or_create_function_declaration(&self, name: &str, arity: usize)
		-> std::rc::Rc<crate::FunctionDeclaration>
	{
		let mut function_declarations = self.function_declarations.borrow_mut();

		match function_declarations.iter().find(|x| x.name == name && x.arity == arity)
		{
			Some(declaration) => std::rc::Rc::clone(&declaration),
			None =>
			{
				let declaration = crate::FunctionDeclaration
				{
					name: name.to_string(),
					arity,
				};
				let declaration = std::rc::Rc::new(declaration);

				function_declarations.insert(std::rc::Rc::clone(&declaration));

				declaration
			},
		}
	}
}

impl FindOrCreatePredicateDeclaration for Declarations
{
	fn find_or_create_predicate_declaration(&self, name: &str, arity: usize)
		-> std::rc::Rc<crate::PredicateDeclaration>
	{
		let mut predicate_declarations = self.predicate_declarations.borrow_mut();

		match predicate_declarations.iter().find(|x| x.name == name && x.arity == arity)
		{
			Some(declaration) => std::rc::Rc::clone(&declaration),
			None =>
			{
				let declaration = crate::PredicateDeclaration
				{
					name: name.to_string(),
					arity,
				};
				let declaration = std::rc::Rc::new(declaration);

				predicate_declarations.insert(std::rc::Rc::clone(&declaration));

				declaration
			},
		}
	}
}
