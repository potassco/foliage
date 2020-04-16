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

pub struct VariableDeclarationStack
{
	pub free_variable_declarations: crate::VariableDeclarations,
	bound_variable_declaration_stack: Vec<std::rc::Rc<crate::VariableDeclarations>>,
}

impl VariableDeclarationStack
{
	pub fn new() -> Self
	{
		Self
		{
			free_variable_declarations: crate::VariableDeclarations::new(),
			bound_variable_declaration_stack: vec![],
		}
	}

	pub fn find(&self, variable_name: &str) -> Option<std::rc::Rc<crate::VariableDeclaration>>
	{
		for variable_declarations in self.bound_variable_declaration_stack.iter().rev()
		{
			if let Some(variable_declaration) = variable_declarations.iter()
				.find(|x| x.name == variable_name)
			{
				return Some(std::rc::Rc::clone(&variable_declaration));
			}
		}

		if let Some(variable_declaration) = self.free_variable_declarations.iter()
			.find(|x| x.name == variable_name)
		{
			return Some(std::rc::Rc::clone(&variable_declaration));
		}

		None
	}

	pub fn find_or_create(&mut self, variable_name: &str) -> std::rc::Rc<crate::VariableDeclaration>
	{
		if let Some(variable_declaration) = self.find(variable_name)
		{
			return variable_declaration;
		}

		let variable_declaration = crate::VariableDeclaration
		{
			name: variable_name.to_owned(),
		};
		let variable_declaration = std::rc::Rc::new(variable_declaration);

		self.free_variable_declarations.push(std::rc::Rc::clone(&variable_declaration));

		variable_declaration
	}

	pub fn is_empty(&self) -> bool
	{
		self.free_variable_declarations.is_empty()
			&& self.bound_variable_declaration_stack.is_empty()
	}

	pub fn push<'v>(variable_declaration_stack: &'v std::cell::RefCell<VariableDeclarationStack>,
		bound_variable_declarations: std::rc::Rc<crate::VariableDeclarations>)
		-> VariableDeclarationStackGuard
	{
		variable_declaration_stack.borrow_mut()
			.bound_variable_declaration_stack.push(bound_variable_declarations);

		VariableDeclarationStackGuard
		{
			variable_declaration_stack: variable_declaration_stack,
		}
	}

	pub(self) fn pop(&mut self)
	{
		if let None = self.bound_variable_declaration_stack.pop()
		{
			unreachable!()
		}
	}
}

pub struct VariableDeclarationStackGuard<'v>
{
	variable_declaration_stack: &'v std::cell::RefCell<VariableDeclarationStack>,
}

impl<'v> Drop for VariableDeclarationStackGuard<'v>
{
	fn drop(&mut self)
	{
		self.variable_declaration_stack.borrow_mut().pop();
	}
}
