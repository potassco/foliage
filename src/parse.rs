pub mod error;
pub mod formulas;
pub mod terms;
pub mod tokens;

pub use error::Error;

use crate::flavor::{FunctionDeclaration as _, PredicateDeclaration as _, VariableDeclaration as _};

pub trait Parser: Sized
{
	type Flavor: crate::flavor::Flavor;

	fn new_function_declaration(name: String, arity: usize)
		-> <Self::Flavor as crate::flavor::Flavor>::FunctionDeclaration;
	fn new_predicate_declaration(name: String, arity: usize)
		-> <Self::Flavor as crate::flavor::Flavor>::PredicateDeclaration;
	fn new_variable_declaration(name: String)
		-> <Self::Flavor as crate::flavor::Flavor>::VariableDeclaration;

	fn find_or_create_function_declaration(&self, name: &str, arity: usize)
		-> std::rc::Rc<<Self::Flavor as crate::flavor::Flavor>::FunctionDeclaration>;
	fn find_or_create_predicate_declaration(&self, name: &str, arity: usize)
		-> std::rc::Rc<<Self::Flavor as crate::flavor::Flavor>::PredicateDeclaration>;
	fn find_or_create_variable_declaration(
		variable_declaration_stack_layer: &crate::VariableDeclarationStackLayer<Self::Flavor>,
		variable_name: &str)
		-> std::rc::Rc<<Self::Flavor as crate::flavor::Flavor>::VariableDeclaration>
	{
		match variable_declaration_stack_layer
		{
			crate::VariableDeclarationStackLayer::Free(free_variable_declarations) =>
			{
				if let Some(variable_declaration) = free_variable_declarations.borrow().iter()
					.find(|x| x.matches_name(variable_name))
				{
					return std::rc::Rc::clone(&variable_declaration);
				}

				let variable_declaration = Self::new_variable_declaration(variable_name.to_owned());
				let variable_declaration = std::rc::Rc::new(variable_declaration);

				free_variable_declarations.borrow_mut()
					.push(std::rc::Rc::clone(&variable_declaration));

				variable_declaration
			},
			crate::VariableDeclarationStackLayer::Bound(bound_variable_declarations) =>
			{
				if let Some(variable_declaration) = bound_variable_declarations
					.variable_declarations.iter()
					.find(|x| x.matches_name(variable_name))
				{
					return std::rc::Rc::clone(&variable_declaration);
				}

				Self::find_or_create_variable_declaration(bound_variable_declarations.parent,
					variable_name)
			},
		}
	}

	fn parse_formula(&self, input: &str)
		-> Result<crate::OpenFormula<Self::Flavor>, crate::parse::Error>
	{
		formulas::formula(input, self)
	}
}

pub struct DefaultParser
{
	function_declarations:
		std::cell::RefCell<crate::FunctionDeclarations<<Self as Parser>::Flavor>>,
	predicate_declarations:
		std::cell::RefCell<crate::PredicateDeclarations<<Self as Parser>::Flavor>>,
}

impl DefaultParser
{
	pub fn new() -> Self
	{
		Self
		{
			function_declarations: std::cell::RefCell::new(
				crate::FunctionDeclarations::<<Self as Parser>::Flavor>::new()),
			predicate_declarations: std::cell::RefCell::new(
				crate::PredicateDeclarations::<<Self as Parser>::Flavor>::new()),
		}
	}
}

impl Parser for DefaultParser
{
	type Flavor = crate::flavor::DefaultFlavor;

	fn new_function_declaration(name: String, arity: usize)
		-> <Self::Flavor as crate::flavor::Flavor>::FunctionDeclaration
	{
		crate::FunctionDeclaration
		{
			name,
			arity,
		}
	}

	fn new_predicate_declaration(name: String, arity: usize)
		-> <Self::Flavor as crate::flavor::Flavor>::PredicateDeclaration
	{
		crate::PredicateDeclaration
		{
			name,
			arity,
		}
	}

	fn new_variable_declaration(name: String)
		-> <Self::Flavor as crate::flavor::Flavor>::VariableDeclaration
	{
		crate::VariableDeclaration
		{
			name,
		}
	}

	fn find_or_create_function_declaration(&self, name: &str, arity: usize)
		-> std::rc::Rc<<Self::Flavor as crate::flavor::Flavor>::FunctionDeclaration>
	{
		let mut function_declarations = self.function_declarations.borrow_mut();

		match function_declarations.iter().find(|x| x.matches_signature(name, arity))
		{
			Some(declaration) => std::rc::Rc::clone(&declaration),
			None =>
			{
				let declaration = Self::new_function_declaration(name.to_string(), arity);
				let declaration = std::rc::Rc::new(declaration);

				function_declarations.insert(std::rc::Rc::clone(&declaration));

				declaration
			},
		}
	}

	fn find_or_create_predicate_declaration(&self, name: &str, arity: usize)
		-> std::rc::Rc<<Self::Flavor as crate::flavor::Flavor>::PredicateDeclaration>
	{
		let mut predicate_declarations = self.predicate_declarations.borrow_mut();

		match predicate_declarations.iter().find(|x| x.matches_signature(name, arity))
		{
			Some(declaration) => std::rc::Rc::clone(&declaration),
			None =>
			{
				let declaration = Self::new_predicate_declaration(name.to_string(), arity);
				let declaration = std::rc::Rc::new(declaration);

				predicate_declarations.insert(std::rc::Rc::clone(&declaration));

				declaration
			},
		}
	}
}
