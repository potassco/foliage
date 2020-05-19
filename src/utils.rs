use crate::flavor::{FunctionDeclaration as _, PredicateDeclaration as _, VariableDeclaration as _};

// Group with implementations
pub trait FindOrCreateFunctionDeclaration<F>
where
	F: crate::flavor::Flavor,
{
	fn find_or_create_function_declaration(&self, name: &str, arity: usize)
		-> std::rc::Rc<F::FunctionDeclaration>;
}

pub trait FindOrCreatePredicateDeclaration<F>
where
	F: crate::flavor::Flavor,
{
	fn find_or_create_predicate_declaration(&self, name: &str, arity: usize)
		-> std::rc::Rc<F::PredicateDeclaration>;
}

pub struct BoundVariableDeclarations<'p, F>
where
	F: crate::flavor::Flavor,
{
	parent: &'p VariableDeclarationStackLayer<'p, F>,
	variable_declarations: std::rc::Rc<crate::VariableDeclarations<F>>,
}

impl<'p, F> BoundVariableDeclarations<'p, F>
where
	F: crate::flavor::Flavor,
{
	pub fn new(parent: &'p VariableDeclarationStackLayer<'p, F>,
		variable_declarations: std::rc::Rc<crate::VariableDeclarations<F>>) -> Self
	{
		Self
		{
			parent,
			variable_declarations,
		}
	}
}

pub enum VariableDeclarationStackLayer<'p, F>
where
	F: crate::flavor::Flavor,
{
	Free(std::cell::RefCell<crate::VariableDeclarations<F>>),
	Bound(BoundVariableDeclarations<'p, F>),
}

impl<'p, F> VariableDeclarationStackLayer<'p, F>
where
	F: crate::flavor::Flavor,
{
	pub fn free() -> Self
	{
		Self::Free(std::cell::RefCell::new(vec![]))
	}

	pub fn bound(parent: &'p VariableDeclarationStackLayer<'p, F>,
		variable_declarations: std::rc::Rc<crate::VariableDeclarations<F>>) -> Self
	{
		Self::Bound(BoundVariableDeclarations::new(parent, variable_declarations))
	}

	pub fn find(&self, variable_name: &str) -> Option<std::rc::Rc<F::VariableDeclaration>>
	{
		match self
		{
			VariableDeclarationStackLayer::Free(free_variable_declarations) =>
			{
				if let Some(variable_declaration) = free_variable_declarations.borrow().iter()
					.find(|x| x.name() == variable_name)
				{
					return Some(std::rc::Rc::clone(&variable_declaration));
				}

				None
			},
			VariableDeclarationStackLayer::Bound(bound_variable_declarations) =>
			{
				if let Some(variable_declaration) = bound_variable_declarations
					.variable_declarations.iter()
					.find(|x| x.name() == variable_name)
				{
					return Some(std::rc::Rc::clone(&variable_declaration));
				}

				bound_variable_declarations.parent.find(variable_name)
			},
		}
	}

	pub fn find_or_create(&self, variable_name: &str) -> std::rc::Rc<F::VariableDeclaration>
	{
		match self
		{
			VariableDeclarationStackLayer::Free(free_variable_declarations) =>
			{
				if let Some(variable_declaration) = free_variable_declarations.borrow().iter()
					.find(|x| x.name() == variable_name)
				{
					return std::rc::Rc::clone(&variable_declaration);
				}

				let variable_declaration = F::VariableDeclaration::new(variable_name.to_owned());
				let variable_declaration = std::rc::Rc::new(variable_declaration);

				free_variable_declarations.borrow_mut()
					.push(std::rc::Rc::clone(&variable_declaration));

				variable_declaration
			},
			VariableDeclarationStackLayer::Bound(bound_variable_declarations) =>
			{
				if let Some(variable_declaration) = bound_variable_declarations
					.variable_declarations.iter()
					.find(|x| x.name() == variable_name)
				{
					return std::rc::Rc::clone(&variable_declaration);
				}

				bound_variable_declarations.parent.find_or_create(variable_name)
			},
		}
	}

	pub fn free_variable_declarations_do_mut<F1, F2>(&self, f: F1) -> F2
	where
		F1: Fn(&mut crate::VariableDeclarations<F>) -> F2,
	{
		match self
		{
			VariableDeclarationStackLayer::Free(free_variable_declarations)
				=> f(&mut free_variable_declarations.borrow_mut()),
			VariableDeclarationStackLayer::Bound(bound_variable_declarations)
				=> bound_variable_declarations.parent.free_variable_declarations_do_mut(f),
		}
	}

	pub fn free_variable_declarations_do<F1, F2>(&self, f: F1) -> F2
	where
		F1: Fn(&crate::VariableDeclarations<F>) -> F2,
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

pub struct Declarations<F>
where
	F: crate::flavor::Flavor,
{
	function_declarations: std::cell::RefCell<crate::FunctionDeclarations<F>>,
	predicate_declarations: std::cell::RefCell<crate::PredicateDeclarations<F>>,
}

impl<F> Declarations<F>
where
	F: crate::flavor::Flavor,
{
	pub fn new() -> Self
	{
		Self
		{
			function_declarations: std::cell::RefCell::new(crate::FunctionDeclarations::<F>::new()),
			predicate_declarations:
				std::cell::RefCell::new(crate::PredicateDeclarations::<F>::new()),
		}
	}
}

impl<F> FindOrCreateFunctionDeclaration<F> for Declarations<F>
where
	F: crate::flavor::Flavor,
{
	fn find_or_create_function_declaration(&self, name: &str, arity: usize)
		-> std::rc::Rc<F::FunctionDeclaration>
	{
		let mut function_declarations = self.function_declarations.borrow_mut();

		match function_declarations.iter().find(|x| x.name() == name && x.arity() == arity)
		{
			Some(declaration) => std::rc::Rc::clone(&declaration),
			None =>
			{
				let declaration = F::FunctionDeclaration::new(name.to_string(), arity);
				let declaration = std::rc::Rc::new(declaration);

				function_declarations.insert(std::rc::Rc::clone(&declaration));

				declaration
			},
		}
	}
}

impl<F> FindOrCreatePredicateDeclaration<F> for Declarations<F>
where
	F: crate::flavor::Flavor,
{
	fn find_or_create_predicate_declaration(&self, name: &str, arity: usize)
		-> std::rc::Rc<F::PredicateDeclaration>
	{
		let mut predicate_declarations = self.predicate_declarations.borrow_mut();

		match predicate_declarations.iter().find(|x| x.name() == name && x.arity() == arity)
		{
			Some(declaration) => std::rc::Rc::clone(&declaration),
			None =>
			{
				let declaration = F::PredicateDeclaration::new(name.to_string(), arity);
				let declaration = std::rc::Rc::new(declaration);

				predicate_declarations.insert(std::rc::Rc::clone(&declaration));

				declaration
			},
		}
	}
}
