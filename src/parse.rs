mod formulas;
mod helpers;
mod literals;
mod names;
mod terms;

pub(crate) use helpers::word_boundary;
pub(crate) use literals::{boolean, integer, special_integer, string};
pub(crate) use names::{function_or_predicate_name, variable_name};
pub use terms::term;
pub use formulas::formula;

pub struct Declarations
{
	function_declarations: std::cell::RefCell<crate::FunctionDeclarations>,
	predicate_declarations: std::cell::RefCell<crate::PredicateDeclarations>,
	variable_declaration_stack: std::cell::RefCell<crate::VariableDeclarationStack>,
}

impl Declarations
{
	pub fn new() -> Self
	{
		Self
		{
			function_declarations: std::cell::RefCell::new(crate::FunctionDeclarations::new()),
			predicate_declarations: std::cell::RefCell::new(crate::PredicateDeclarations::new()),
			variable_declaration_stack:
				std::cell::RefCell::new(crate::VariableDeclarationStack::new()),
		}
	}
}
