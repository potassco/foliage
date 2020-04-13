mod formulas;
mod terms;

pub trait Format
{
	fn display_variable_declaration(&self, formatter: &mut std::fmt::Formatter,
		variable_declaration: &std::rc::Rc<crate::VariableDeclaration>)
		-> std::fmt::Result;
}

pub struct DefaultFormat;

impl Format for DefaultFormat
{
	fn display_variable_declaration(&self, formatter: &mut std::fmt::Formatter,
		variable_declaration: &std::rc::Rc<crate::VariableDeclaration>)
		-> std::fmt::Result
	{
		write!(formatter, "{:?}", variable_declaration)
	}
}
