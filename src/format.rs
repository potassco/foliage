pub mod formulas;
pub mod terms;

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

pub fn display_term<'term, 'format, F>(term: &'term crate::Term, format: &'format F)
	-> terms::TermDisplay<'term, 'format, F>
where
	F: Format,
{
	terms::display_term(term, None, terms::TermPosition::Any, format)
}

pub fn display_formula<'formula, 'format, F>(formula: &'formula crate::Formula, format: &'format F)
	-> formulas::FormulaDisplay<'formula, 'format, F>
where
	F: Format,
{
	formulas::display_formula(formula, None, formulas::FormulaPosition::Any, format)
}
