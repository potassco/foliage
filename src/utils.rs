use crate::flavor::VariableDeclaration as _;

pub struct BoundVariableDeclarations<'p, F>
where
	F: crate::Flavor,
{
	pub parent: &'p VariableDeclarationStackLayer<'p, F>,
	pub variable_declarations: std::rc::Rc<crate::VariableDeclarations<F>>,
}

impl<'p, F> BoundVariableDeclarations<'p, F>
where
	F: crate::Flavor,
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
	F: crate::Flavor,
{
	Free(std::cell::RefCell<crate::VariableDeclarations<F>>),
	Bound(BoundVariableDeclarations<'p, F>),
}

impl<'p, F> VariableDeclarationStackLayer<'p, F>
where
	F: crate::Flavor,
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
					.find(|x| x.matches_name(variable_name))
				{
					return Some(std::rc::Rc::clone(&variable_declaration));
				}

				None
			},
			VariableDeclarationStackLayer::Bound(bound_variable_declarations) =>
			{
				if let Some(variable_declaration) = bound_variable_declarations
					.variable_declarations.iter()
					.find(|x| x.matches_name(variable_name))
				{
					return Some(std::rc::Rc::clone(&variable_declaration));
				}

				bound_variable_declarations.parent.find(variable_name)
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
