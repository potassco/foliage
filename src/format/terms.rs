use crate::flavor::{FunctionDeclaration as _, VariableDeclaration as _};

impl std::fmt::Debug for crate::SpecialInteger
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match &self
		{
			Self::Infimum => write!(formatter, "#inf"),
			Self::Supremum => write!(formatter, "#sup"),
		}
	}
}

impl std::fmt::Display for crate::SpecialInteger
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(formatter, "{:?}", &self)
	}
}

impl std::fmt::Debug for crate::FunctionDeclaration
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(formatter, "{}/{}", &self.name, self.arity)
	}
}

impl std::fmt::Display for crate::FunctionDeclaration
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(formatter, "{:?}", &self)
	}
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub(crate) enum TermPosition
{
	Any,
	Left,
	Right,
}

pub struct TermDisplay<'term, F>
where
	F: crate::flavor::Flavor,
{
	term: &'term crate::Term<F>,
	parent_term: Option<&'term crate::Term<F>>,
	position: TermPosition,
}

impl<'term, F> TermDisplay<'term, F>
where
	F: crate::flavor::Flavor,
{
	fn requires_parentheses(&self) -> bool
	{
		use crate::Term;

		let parent_term = match self.parent_term
		{
			Some(parent_term) => parent_term,
			None => return false,
		};

		// The absolute value operation never requires parentheses, as it has its own parentheses
		if let Term::UnaryOperation(
			crate::UnaryOperation{operator: crate::UnaryOperator::AbsoluteValue, ..}) = parent_term
		{
			return false;
		}

		match self.term
		{
			Term::Boolean(_)
			| Term::SpecialInteger(_)
			| Term::Integer(_)
			| Term::String(_)
			| Term::Variable(_)
			| Term::Function(_)
			| Term::UnaryOperation(_)
				=> false,
			Term::BinaryOperation(binary_operation) =>
			{
				let parent_binary_operation = match parent_term
				{
					Term::BinaryOperation(parent_binary_operation) => parent_binary_operation,
					// Binary operations nested in the negative operation always require parentheses
					Term::UnaryOperation(
						crate::UnaryOperation{operator: crate::UnaryOperator::Negative, ..})
						=> return true,
					_ => return false,
				};

				match binary_operation.operator
				{
					crate::BinaryOperator::Exponentiate =>
						parent_binary_operation.operator == crate::BinaryOperator::Exponentiate
							&& self.position == TermPosition::Left,
					crate::BinaryOperator::Multiply
					| crate::BinaryOperator::Divide
						=> match parent_binary_operation.operator
					{
						crate::BinaryOperator::Exponentiate => true,
						crate::BinaryOperator::Divide
						| crate::BinaryOperator::Modulo
							=> self.position == TermPosition::Right,
						_ => false,
					},
					crate::BinaryOperator::Modulo => match parent_binary_operation.operator
					{
						crate::BinaryOperator::Exponentiate => true,
						crate::BinaryOperator::Multiply
						| crate::BinaryOperator::Divide
						| crate::BinaryOperator::Modulo
							=> self.position == TermPosition::Right,
						_ => false,
					},
					crate::BinaryOperator::Add
					| crate::BinaryOperator::Subtract
						=> match parent_binary_operation.operator
					{
						crate::BinaryOperator::Exponentiate
						| crate::BinaryOperator::Multiply
						| crate::BinaryOperator::Divide
						| crate::BinaryOperator::Modulo
							=> true,
						crate::BinaryOperator::Subtract
							=> self.position == TermPosition::Right,
						_ => false,
					},
				}
			},
		}
	}
}

pub(crate) fn display_term<'term, F>(term: &'term crate::Term<F>,
	parent_term: Option<&'term crate::Term<F>>, position: TermPosition)
	-> TermDisplay<'term, F>
where
	F: crate::flavor::Flavor,
{
	TermDisplay
	{
		term,
		parent_term,
		position,
	}
}

impl<'term, F> std::fmt::Debug for TermDisplay<'term, F>
where
	F: crate::flavor::Flavor,
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		let requires_parentheses = self.requires_parentheses();

		if requires_parentheses
		{
			write!(formatter, "(")?;
		}

		match &self.term
		{
			crate::Term::Boolean(true) => write!(formatter, "true")?,
			crate::Term::Boolean(false) => write!(formatter, "false")?,
			crate::Term::SpecialInteger(value) => write!(formatter, "{:?}", value)?,
			crate::Term::Integer(value) => write!(formatter, "{}", value)?,
			crate::Term::String(value) => write!(formatter, "\"{}\"",
				value.replace("\\", "\\\\").replace("\n", "\\n").replace("\t", "\\t"))?,
			crate::Term::Variable(variable) => variable.declaration.display_name(formatter)?,
			crate::Term::Function(function) =>
			{
				function.declaration.display_name(formatter)?;

				assert!(function.declaration.arity() == function.arguments.len(),
					"number of function arguments differs from declaration (expected {}, got {})",
					function.declaration.arity(), function.arguments.len());

				if !function.arguments.is_empty()
				{
					write!(formatter, "(")?;

					let mut separator = "";

					for argument in &function.arguments
					{
						write!(formatter, "{}{:?}", separator,
							display_term(&argument, Some(self.term), TermPosition::Any))?;

						separator = ", ";
					}

					write!(formatter, ")")?;
				}
			},
			crate::Term::BinaryOperation(binary_operation) =>
			{
				let operator_string = match binary_operation.operator
				{
					crate::BinaryOperator::Add => "+",
					crate::BinaryOperator::Subtract => "-",
					crate::BinaryOperator::Multiply => "*",
					crate::BinaryOperator::Divide => "/",
					crate::BinaryOperator::Modulo => "%",
					crate::BinaryOperator::Exponentiate => "**",
				};

				write!(formatter, "{:?} {} {:?}",
					display_term(&binary_operation.left, Some(self.term), TermPosition::Left),
					operator_string,
					display_term(&binary_operation.right, Some(self.term), TermPosition::Right))?;
			},
			crate::Term::UnaryOperation(
				crate::UnaryOperation{operator: crate::UnaryOperator::Negative, argument})
				=> write!(formatter, "-{:?}",
					display_term(argument, Some(self.term), TermPosition::Any))?,
			crate::Term::UnaryOperation(
				crate::UnaryOperation{operator: crate::UnaryOperator::AbsoluteValue, argument})
				=> write!(formatter, "|{:?}|",
					display_term(argument, Some(self.term), TermPosition::Any))?,
		}

		if requires_parentheses
		{
			write!(formatter, ")")?;
		}

		Ok(())
	}
}

impl<'term, F> std::fmt::Display for TermDisplay<'term, F>
where
	F: crate::flavor::Flavor,
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(formatter, "{:?}", self)
	}
}

impl<F> std::fmt::Debug for crate::Term<F>
where
	F: crate::flavor::Flavor,
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(formatter, "{:?}", display_term(&self, None, TermPosition::Any))
	}
}

impl<F> std::fmt::Display for crate::Term<F>
where
	F: crate::flavor::Flavor,
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(formatter, "{}", display_term(&self, None, TermPosition::Any))
	}
}

#[cfg(test)]
pub(crate) mod tests
{
	use crate::*;
	type Term = crate::Term<flavor::DefaultFlavor>;

	macro_rules! assert
	{
		($term:expr, $output:expr) =>
		{
			assert_eq!(format($term), $output);
		};
	}

	fn format(term: Box<Term>) -> String
	{
		format!("{}", term)
	}

	pub(crate) fn absolute_value(argument: Box<Term>) -> Box<Term>
	{
		Box::new(Term::absolute_value(argument))
	}

	pub(crate) fn add(left: Box<Term>, right: Box<Term>) -> Box<Term>
	{
		Box::new(Term::add(left, right))
	}

	pub(crate) fn constant(name: &str) -> Box<Term>
	{
		Box::new(Term::function(function_declaration(name, 0), vec![]))
	}

	pub(crate) fn divide(left: Box<Term>, right: Box<Term>) -> Box<Term>
	{
		Box::new(Term::divide(left, right))
	}

	pub(crate) fn exponentiate(left: Box<Term>, right: Box<Term>) -> Box<Term>
	{
		Box::new(Term::exponentiate(left, right))
	}

	pub(crate) fn false_() -> Box<Term>
	{
		Box::new(Term::false_())
	}

	pub(crate) fn function(name: &str, arguments: Vec<Box<Term>>) -> Box<Term>
	{
		Box::new(Term::function(function_declaration(name, arguments.len()),
			arguments.into_iter().map(|x| *x).collect()))
	}

	pub(crate) fn function_declaration(name: &str, arity: usize) -> std::rc::Rc<FunctionDeclaration>
	{
		std::rc::Rc::new(FunctionDeclaration::new(name.to_string(), arity))
	}

	pub(crate) fn infimum() -> Box<Term>
	{
		Box::new(Term::infimum())
	}

	pub(crate) fn integer(value: i32) -> Box<Term>
	{
		Box::new(Term::integer(value))
	}

	pub(crate) fn modulo(left: Box<Term>, right: Box<Term>) -> Box<Term>
	{
		Box::new(Term::modulo(left, right))
	}

	pub(crate) fn multiply(left: Box<Term>, right: Box<Term>) -> Box<Term>
	{
		Box::new(Term::multiply(left, right))
	}

	pub(crate) fn negative(argument: Box<Term>) -> Box<Term>
	{
		Box::new(Term::negative(argument))
	}

	pub(crate) fn subtract(left: Box<Term>, right: Box<Term>) -> Box<Term>
	{
		Box::new(Term::subtract(left, right))
	}

	pub(crate) fn supremum() -> Box<Term>
	{
		Box::new(Term::supremum())
	}

	pub(crate) fn string(value: &str) -> Box<Term>
	{
		Box::new(Term::string(value.to_string()))
	}

	pub(crate) fn true_() -> Box<Term>
	{
		Box::new(Term::true_())
	}

	pub(crate) fn variable(name: &str) -> Box<Term>
	{
		Box::new(Term::variable(variable_declaration(name)))
	}

	pub(crate) fn variable_declaration(name: &str) -> std::rc::Rc<VariableDeclaration>
	{
		std::rc::Rc::new(VariableDeclaration::new(name.to_string()))
	}

	pub(crate) fn a() -> Box<Term>
	{
		constant("a")
	}

	pub(crate) fn b() -> Box<Term>
	{
		constant("b")
	}

	pub(crate) fn c() -> Box<Term>
	{
		constant("c")
	}

	pub(crate) fn d() -> Box<Term>
	{
		constant("d")
	}

	pub(crate) fn e() -> Box<Term>
	{
		constant("e")
	}

	pub(crate) fn abc() -> Vec<Box<Term>>
	{
		vec![a(), b(), c()]
	}

	pub(crate) fn a1b1c1() -> Vec<Box<Term>>
	{
		vec![constant("a1"), constant("b1"), constant("c1")]
	}

	pub(crate) fn a2b2c2() -> Vec<Box<Term>>
	{
		vec![constant("a2"), constant("b2"), constant("c2")]
	}

	pub(crate) fn x() -> Box<Term>
	{
		variable("X")
	}

	pub(crate) fn y() -> Box<Term>
	{
		variable("Y")
	}

	pub(crate) fn z() -> Box<Term>
	{
		variable("Z")
	}

	pub(crate) fn xyz() -> Vec<Box<Term>>
	{
		vec![x(), y(), z()]
	}

	#[test]
	fn format_binary_operation()
	{
		assert!(add(a(), constant("b")), "a + b");
		assert!(subtract(a(), constant("b")), "a - b");
		assert!(multiply(a(), constant("b")), "a * b");
		assert!(divide(a(), constant("b")), "a / b");
		assert!(modulo(a(), constant("b")), "a % b");
		assert!(exponentiate(a(), constant("b")), "a ** b");
	}

	#[test]
	fn format_boolean()
	{
		assert!(true_(), "true");
		assert!(false_(), "false");
	}

	#[test]
	fn format_special_integer()
	{
		assert!(infimum(), "#inf");
		assert!(supremum(), "#sup");
	}

	#[test]
	fn format_integer()
	{
		assert!(integer(0), "0");
		assert!(integer(1), "1");
		assert!(integer(10000), "10000");
		assert!(integer(-1), "-1");
		assert!(integer(-10000), "-10000");
	}

	#[test]
	fn format_string()
	{
		assert!(string(""), "\"\"");
		assert!(string(" "), "\" \"");
		assert!(string("     "), "\"     \"");
		assert!(string("a"), "\"a\"");
		assert!(string("test test"), "\"test test\"");
		assert!(string("123 123"), "\"123 123\"");
		assert!(string("\ntest\n123\n"), "\"\\ntest\\n123\\n\"");
		assert!(string("\ttest\t123\t"), "\"\\ttest\\t123\\t\"");
		assert!(string("\\test\\123\\"), "\"\\\\test\\\\123\\\\\"");
		assert!(string("🙂test🙂123🙂"), "\"🙂test🙂123🙂\"");
	}

	#[test]
	fn format_function()
	{
		assert!(a(), "a");
		assert!(constant("constant"), "constant");
		assert!(function("f", vec![a()]), "f(a)");
		assert!(function("f", abc()), "f(a, b, c)");
		assert!(function("function", abc()), "function(a, b, c)");

		assert!(function("function", vec![
			exponentiate(absolute_value(multiply(a(), integer(-20))), integer(2)),
			string("test"),
			function("f", vec![multiply(add(b(), c()), subtract(b(), c())), infimum(), x()])]),
			"function(|a * -20| ** 2, \"test\", f((b + c) * (b - c), #inf, X))");

		// TODO: escape functions that start with capital letters or that conflict with keywords
	}

	#[test]
	fn format_function_declaration()
	{
		assert_eq!(format!("{}", function_declaration("a", 0)), "a/0");
		assert_eq!(format!("{}", function_declaration("constant", 0)), "constant/0");
		assert_eq!(format!("{}", function_declaration("f", 1)), "f/1");
		assert_eq!(format!("{}", function_declaration("f", 3)), "f/3");
		assert_eq!(format!("{}", function_declaration("function", 3)), "function/3");
	}

	#[test]
	fn format_variable()
	{
		assert!(variable("X"), "X");
		assert!(variable("Variable"), "Variable");
	}

	#[test]
	fn format_combinations_boolean()
	{
		// Function + Boolean
		assert!(function("f", vec![true_()]), "f(true)");
		assert!(function("f", vec![false_()]), "f(false)");
		assert!(function("f", vec![true_(), true_(), true_()]), "f(true, true, true)");
		assert!(function("f", vec![false_(), false_(), false_()]), "f(false, false, false)");

		// Absolute value + Boolean
		assert!(absolute_value(true_()), "|true|");
		assert!(absolute_value(false_()), "|false|");

		// Negative + Boolean
		assert!(negative(true_()), "-true");
		assert!(negative(false_()), "-false");

		// Exponentiate + Boolean
		assert!(exponentiate(true_(), true_()), "true ** true");
		assert!(exponentiate(false_(), false_()), "false ** false");

		// Multiply + Boolean
		assert!(multiply(true_(), true_()), "true * true");
		assert!(multiply(false_(), false_()), "false * false");

		// Divide + Boolean
		assert!(divide(true_(), true_()), "true / true");
		assert!(divide(false_(), false_()), "false / false");

		// Modulo + Boolean
		assert!(modulo(true_(), true_()), "true % true");
		assert!(modulo(false_(), false_()), "false % false");

		// Add + Boolean
		assert!(add(true_(), true_()), "true + true");
		assert!(add(false_(), false_()), "false + false");

		// Subtract + Boolean
		assert!(subtract(true_(), true_()), "true - true");
		assert!(subtract(false_(), false_()), "false - false");
	}

	#[test]
	fn format_combinations_special_integer()
	{
		// Function + special integer
		assert!(function("f", vec![infimum()]), "f(#inf)");
		assert!(function("f", vec![supremum()]), "f(#sup)");
		assert!(function("f", vec![infimum(), infimum(), infimum()]), "f(#inf, #inf, #inf)");
		assert!(function("f", vec![supremum(), supremum(), supremum()]), "f(#sup, #sup, #sup)");

		// Absolute value + special integer
		assert!(absolute_value(infimum()), "|#inf|");
		assert!(absolute_value(supremum()), "|#sup|");

		// Negative + special integer
		assert!(negative(infimum()), "-#inf");
		assert!(negative(supremum()), "-#sup");

		// Exponentiate + special integer
		assert!(exponentiate(infimum(), infimum()), "#inf ** #inf");
		assert!(exponentiate(supremum(), supremum()), "#sup ** #sup");

		// Multiply + special integer
		assert!(multiply(infimum(), infimum()), "#inf * #inf");
		assert!(multiply(supremum(), supremum()), "#sup * #sup");

		// Divide + special integer
		assert!(divide(infimum(), infimum()), "#inf / #inf");
		assert!(divide(supremum(), supremum()), "#sup / #sup");

		// Modulo + special integer
		assert!(modulo(infimum(), infimum()), "#inf % #inf");
		assert!(modulo(supremum(), supremum()), "#sup % #sup");

		// Add + special integer
		assert!(add(infimum(), infimum()), "#inf + #inf");
		assert!(add(supremum(), supremum()), "#sup + #sup");

		// Subtract + special integer
		assert!(subtract(infimum(), infimum()), "#inf - #inf");
		assert!(subtract(supremum(), supremum()), "#sup - #sup");
	}

	#[test]
	fn format_combinations_integer()
	{
		// Function + integer
		assert!(function("f", vec![integer(0)]), "f(0)");
		assert!(function("f", vec![integer(10000)]), "f(10000)");
		assert!(function("f", vec![integer(-10000)]), "f(-10000)");
		assert!(function("f", vec![integer(0), integer(0), integer(0)]), "f(0, 0, 0)");
		assert!(function("f", vec![integer(10000), integer(10000), integer(10000)]),
			"f(10000, 10000, 10000)");
		assert!(function("f", vec![integer(-10000), integer(-10000), integer(-10000)]),
			"f(-10000, -10000, -10000)");

		// Absolute value + integer
		assert!(absolute_value(integer(0)), "|0|");
		assert!(absolute_value(integer(10000)), "|10000|");
		assert!(absolute_value(integer(-10000)), "|-10000|");

		// Negative + integer
		assert!(negative(integer(0)), "-0");
		assert!(negative(integer(10000)), "-10000");
		assert!(negative(integer(-10000)), "--10000");

		// Exponentiate + integer
		assert!(exponentiate(integer(0), integer(0)), "0 ** 0");
		assert!(exponentiate(integer(10000), integer(10000)), "10000 ** 10000");
		assert!(exponentiate(integer(-10000), integer(-10000)), "-10000 ** -10000");

		// Multiply + integer
		assert!(multiply(integer(0), integer(0)), "0 * 0");
		assert!(multiply(integer(10000), integer(10000)), "10000 * 10000");
		assert!(multiply(integer(-10000), integer(-10000)), "-10000 * -10000");

		// Divide + integer
		assert!(divide(integer(0), integer(0)), "0 / 0");
		assert!(divide(integer(10000), integer(10000)), "10000 / 10000");
		assert!(divide(integer(-10000), integer(-10000)), "-10000 / -10000");

		// Modulo + integer
		assert!(modulo(integer(0), integer(0)), "0 % 0");
		assert!(modulo(integer(10000), integer(10000)), "10000 % 10000");
		assert!(modulo(integer(-10000), integer(-10000)), "-10000 % -10000");

		// Add + integer
		assert!(add(integer(0), integer(0)), "0 + 0");
		assert!(add(integer(10000), integer(10000)), "10000 + 10000");
		assert!(add(integer(-10000), integer(-10000)), "-10000 + -10000");

		// Subtract + integer
		assert!(subtract(integer(0), integer(0)), "0 - 0");
		assert!(subtract(integer(10000), integer(10000)), "10000 - 10000");
		assert!(subtract(integer(-10000), integer(-10000)), "-10000 - -10000");
	}

	#[test]
	fn format_combinations_string()
	{
		// Function + string
		assert!(function("f", vec![string("")]), "f(\"\")");
		assert!(function("f", vec![string("test 123")]), "f(\"test 123\")");
		assert!(function("f", vec![string("\\a\nb🙂c\t")]), "f(\"\\\\a\\nb🙂c\\t\")");
		assert!(function("f", vec![string(""), string(""), string("")]), "f(\"\", \"\", \"\")");
		assert!(function("f", vec![string("test 123"), string("test 123"), string("test 123")]),
			"f(\"test 123\", \"test 123\", \"test 123\")");
		assert!(function("f", vec![string("\\a\nb🙂c\t"), string("\\a\nb🙂c\t"),
			string("\\a\nb🙂c\t")]),
			"f(\"\\\\a\\nb🙂c\\t\", \"\\\\a\\nb🙂c\\t\", \"\\\\a\\nb🙂c\\t\")");

		// Absolute value + string
		assert!(absolute_value(string("")), "|\"\"|");
		assert!(absolute_value(string("test 123")), "|\"test 123\"|");
		assert!(absolute_value(string("\\a\nb🙂c\t")), "|\"\\\\a\\nb🙂c\\t\"|");

		// Negative + string
		assert!(negative(string("")), "-\"\"");
		assert!(negative(string("test 123")), "-\"test 123\"");
		assert!(negative(string("\\a\nb🙂c\t")), "-\"\\\\a\\nb🙂c\\t\"");

		// Exponentiate + string
		assert!(exponentiate(string(""), string("")), "\"\" ** \"\"");
		assert!(exponentiate(string("test 123"), string("test 123")),
			"\"test 123\" ** \"test 123\"");
		assert!(exponentiate(string("\\a\nb🙂c\t"), string("\\a\nb🙂c\t")),
			"\"\\\\a\\nb🙂c\\t\" ** \"\\\\a\\nb🙂c\\t\"");

		// Multiply + string
		assert!(multiply(string(""), string("")), "\"\" * \"\"");
		assert!(multiply(string("test 123"), string("test 123")), "\"test 123\" * \"test 123\"");
		assert!(multiply(string("\\a\nb🙂c\t"), string("\\a\nb🙂c\t")),
			"\"\\\\a\\nb🙂c\\t\" * \"\\\\a\\nb🙂c\\t\"");

		// Divide + string
		assert!(divide(string(""), string("")), "\"\" / \"\"");
		assert!(divide(string("test 123"), string("test 123")), "\"test 123\" / \"test 123\"");
		assert!(divide(string("\\a\nb🙂c\t"), string("\\a\nb🙂c\t")),
			"\"\\\\a\\nb🙂c\\t\" / \"\\\\a\\nb🙂c\\t\"");

		// Modulo + string
		assert!(modulo(string(""), string("")), "\"\" % \"\"");
		assert!(modulo(string("test 123"), string("test 123")), "\"test 123\" % \"test 123\"");
		assert!(modulo(string("\\a\nb🙂c\t"), string("\\a\nb🙂c\t")),
			"\"\\\\a\\nb🙂c\\t\" % \"\\\\a\\nb🙂c\\t\"");

		// Add + string
		assert!(add(string(""), string("")), "\"\" + \"\"");
		assert!(add(string("test 123"), string("test 123")), "\"test 123\" + \"test 123\"");
		assert!(add(string("\\a\nb🙂c\t"), string("\\a\nb🙂c\t")),
			"\"\\\\a\\nb🙂c\\t\" + \"\\\\a\\nb🙂c\\t\"");

		// Subtract + string
		assert!(subtract(string(""), string("")), "\"\" - \"\"");
		assert!(subtract(string("test 123"), string("test 123")), "\"test 123\" - \"test 123\"");
		assert!(subtract(string("\\a\nb🙂c\t"), string("\\a\nb🙂c\t")),
			"\"\\\\a\\nb🙂c\\t\" - \"\\\\a\\nb🙂c\\t\"");
	}

	#[test]
	fn format_combinations_variable()
	{
		let variable = || variable("Variable");

		// Function + variable
		assert!(function("f", vec![x()]), "f(X)");
		assert!(function("f", vec![variable()]), "f(Variable)");
		assert!(function("f", xyz()), "f(X, Y, Z)");
		assert!(function("f", vec![variable(), variable(), variable()]),
			"f(Variable, Variable, Variable)");

		// Absolute value + variable
		assert!(absolute_value(x()), "|X|");
		assert!(absolute_value(variable()), "|Variable|");

		// Negative + variable
		assert!(negative(x()), "-X");
		assert!(negative(variable()), "-Variable");

		// Exponentiate + variable
		assert!(exponentiate(x(), y()), "X ** Y");
		assert!(exponentiate(variable(), variable()), "Variable ** Variable");

		// Multiply + variable
		assert!(multiply(x(), y()), "X * Y");
		assert!(multiply(variable(), variable()), "Variable * Variable");

		// Divide + variable
		assert!(divide(x(), y()), "X / Y");
		assert!(divide(variable(), variable()), "Variable / Variable");

		// Modulo + variable
		assert!(modulo(x(), y()), "X % Y");
		assert!(modulo(variable(), variable()), "Variable % Variable");

		// Add + variable
		assert!(add(x(), y()), "X + Y");
		assert!(add(variable(), variable()), "Variable + Variable");

		// Subtract + variable
		assert!(subtract(x(), y()), "X - Y");
		assert!(subtract(variable(), variable()), "Variable - Variable");
	}

	#[test]
	fn format_combinations_function()
	{
		let f_a = || function("f", vec![a()]);
		let g_b = || function("g", vec![b()]);
		let f_abc = || function("f", abc());
		let f_a1b1c1 = || function("f", a1b1c1());
		let g_a2b2c2 = || function("g", a2b2c2());

		// Function + function
		// TODO

		// Absolute value + function
		assert!(absolute_value(a()), "|a|");
		assert!(absolute_value(f_a()), "|f(a)|");
		assert!(absolute_value(f_abc()), "|f(a, b, c)|");

		// Negative + function
		assert!(negative(a()), "-a");
		assert!(negative(f_a()), "-f(a)");
		assert!(negative(f_abc()), "-f(a, b, c)");

		// Exponentiate + function
		assert!(exponentiate(a(), b()), "a ** b");
		assert!(exponentiate(f_a(), g_b()), "f(a) ** g(b)");
		assert!(exponentiate(f_a1b1c1(), g_a2b2c2()), "f(a1, b1, c1) ** g(a2, b2, c2)");

		// Multiply + function
		assert!(multiply(a(), b()), "a * b");
		assert!(multiply(f_a(), g_b()), "f(a) * g(b)");
		assert!(multiply(f_a1b1c1(), g_a2b2c2()), "f(a1, b1, c1) * g(a2, b2, c2)");

		// Divide + function
		assert!(divide(a(), b()), "a / b");
		assert!(divide(f_a(), g_b()), "f(a) / g(b)");
		assert!(divide(f_a1b1c1(), g_a2b2c2()), "f(a1, b1, c1) / g(a2, b2, c2)");

		// Modulo + function
		assert!(modulo(a(), b()), "a % b");
		assert!(modulo(f_a(), g_b()), "f(a) % g(b)");
		assert!(modulo(f_a1b1c1(), g_a2b2c2()), "f(a1, b1, c1) % g(a2, b2, c2)");

		// Add + function
		assert!(add(a(), b()), "a + b");
		assert!(add(f_a(), g_b()), "f(a) + g(b)");
		assert!(add(f_a1b1c1(), g_a2b2c2()), "f(a1, b1, c1) + g(a2, b2, c2)");

		// Subtract + function
		assert!(subtract(a(), b()), "a - b");
		assert!(subtract(f_a(), g_b()), "f(a) - g(b)");
		assert!(subtract(f_a1b1c1(), g_a2b2c2()), "f(a1, b1, c1) - g(a2, b2, c2)");
	}

	#[test]
	fn format_combinations_absolute_value()
	{
		// Function + absolute value
		assert!(function("f", vec![absolute_value(a())]), "f(|a|)");
		assert!(function("f", vec![absolute_value(a()), absolute_value(b()), absolute_value(c())]),
			"f(|a|, |b|, |c|)");

		// Absolute value + absolute value
		assert!(absolute_value(absolute_value(a())), "||a||");

		// Negative + absolute value
		assert!(negative(absolute_value(a())), "-|a|");

		// Exponentiate + absolute value
		assert!(exponentiate(absolute_value(a()), absolute_value(b())), "|a| ** |b|");

		// Multiply + absolute value
		assert!(multiply(absolute_value(a()), absolute_value(b())), "|a| * |b|");

		// Divide + absolute value
		assert!(divide(absolute_value(a()), absolute_value(b())), "|a| / |b|");

		// Modulo + absolute value
		assert!(modulo(absolute_value(a()), absolute_value(b())), "|a| % |b|");

		// Add + absolute value
		assert!(add(absolute_value(a()), absolute_value(b())), "|a| + |b|");

		// Subtract + absolute value
		assert!(subtract(absolute_value(a()), absolute_value(b())), "|a| - |b|");
	}

	#[test]
	fn format_combinations_negative()
	{
		// Function + negative
		assert!(function("f", vec![negative(a())]), "f(-a)");
		assert!(function("f", vec![negative(a()), negative(b()), negative(c())]), "f(-a, -b, -c)");

		// Absolute value + negative
		assert!(absolute_value(negative(a())), "|-a|");

		// Negative + negative
		assert!(negative(negative(a())), "--a");

		// Exponentiate + negative
		assert!(exponentiate(negative(a()), negative(b())), "-a ** -b");

		// Multiply + negative
		assert!(multiply(negative(a()), negative(b())), "-a * -b");

		// Divide + negative
		assert!(divide(negative(a()), negative(b())), "-a / -b");

		// Modulo + negative
		assert!(modulo(negative(a()), negative(b())), "-a % -b");

		// Add + negative
		assert!(add(negative(a()), negative(b())), "-a + -b");

		// Subtract + negative
		assert!(subtract(negative(a()), negative(b())), "-a - -b");
	}

	#[test]
	fn format_combinations_exponentiate()
	{
		// Function + exponentiate
		assert!(function("f", vec![exponentiate(a(), b())]), "f(a ** b)");
		assert!(function("f", vec![exponentiate(a(), b()), exponentiate(c(), d()),
			exponentiate(e(), a())]),
			"f(a ** b, c ** d, e ** a)");

		// Absolute value + exponentiate
		assert!(absolute_value(exponentiate(a(), b())), "|a ** b|");

		// Negative + exponentiate
		assert!(negative(exponentiate(a(), b())), "-(a ** b)");

		// Exponentiate + exponentiate
		assert!(exponentiate(exponentiate(a(), b()), exponentiate(c(), d())), "(a ** b) ** c ** d");

		// Multiply + exponentiate
		assert!(multiply(exponentiate(a(), b()), exponentiate(c(), d())), "a ** b * c ** d");

		// Divide + exponentiate
		assert!(divide(exponentiate(a(), b()), exponentiate(c(), d())), "a ** b / c ** d");

		// Modulo + exponentiate
		assert!(modulo(exponentiate(a(), b()), exponentiate(c(), d())), "a ** b % c ** d");

		// Add + exponentiate
		assert!(add(exponentiate(a(), b()), exponentiate(c(), d())), "a ** b + c ** d");

		// Subtract + exponentiate
		assert!(subtract(exponentiate(a(), b()), exponentiate(c(), d())), "a ** b - c ** d");
	}

	#[test]
	fn format_combinations_multiply()
	{
		// Function + multiply
		assert!(function("f", vec![multiply(a(), b())]), "f(a * b)");
		assert!(function("f", vec![multiply(a(), b()), multiply(c(), d()), multiply(e(), a())]),
			"f(a * b, c * d, e * a)");

		// Absolute value + multiply
		assert!(absolute_value(multiply(a(), b())), "|a * b|");

		// Negative + multiply
		assert!(negative(multiply(a(), b())), "-(a * b)");

		// Exponentiate + multiply
		assert!(exponentiate(multiply(a(), b()), multiply(c(), d())), "(a * b) ** (c * d)");

		// Multiply + multiply
		assert!(multiply(multiply(a(), b()), multiply(c(), d())), "a * b * c * d");

		// Divide + multiply
		assert!(divide(multiply(a(), b()), multiply(c(), d())), "a * b / (c * d)");

		// Modulo + multiply
		assert!(modulo(multiply(a(), b()), multiply(c(), d())), "a * b % (c * d)");

		// Add + multiply
		assert!(add(multiply(a(), b()), multiply(c(), d())), "a * b + c * d");

		// Subtract + multiply
		assert!(subtract(multiply(a(), b()), multiply(c(), d())), "a * b - c * d");
	}

	#[test]
	fn format_combinations_divide()
	{
		// Function + divide
		assert!(function("f", vec![divide(a(), b())]), "f(a / b)");
		assert!(function("f", vec![divide(a(), b()), divide(c(), d()), divide(e(), a())]),
			"f(a / b, c / d, e / a)");

		// Absolute value + divide
		assert!(absolute_value(divide(a(), b())), "|a / b|");

		// Negative + divide
		assert!(negative(divide(a(), b())), "-(a / b)");

		// Exponentiate + divide
		assert!(exponentiate(divide(a(), b()), divide(c(), d())), "(a / b) ** (c / d)");

		// Multiply + divide
		assert!(multiply(divide(a(), b()), divide(c(), d())), "a / b * c / d");

		// Divide + divide
		assert!(divide(divide(a(), b()), divide(c(), d())), "a / b / (c / d)");

		// Modulo + divide
		assert!(modulo(divide(a(), b()), divide(c(), d())), "a / b % (c / d)");

		// Add + divide
		assert!(add(divide(a(), b()), divide(c(), d())), "a / b + c / d");

		// Subtract + divide
		assert!(subtract(divide(a(), b()), divide(c(), d())), "a / b - c / d");
	}

	#[test]
	fn format_combinations_modulo()
	{
		// Function + modulo
		assert!(function("f", vec![modulo(a(), b())]), "f(a % b)");
		assert!(function("f", vec![modulo(a(), b()), modulo(c(), d()), modulo(e(), a())]),
			"f(a % b, c % d, e % a)");

		// Absolute value + modulo
		assert!(absolute_value(modulo(a(), b())), "|a % b|");

		// Negative + modulo
		assert!(negative(modulo(a(), b())), "-(a % b)");

		// Exponentiate + modulo
		assert!(exponentiate(modulo(a(), b()), modulo(c(), d())), "(a % b) ** (c % d)");

		// Multiply + modulo
		assert!(multiply(modulo(a(), b()), modulo(c(), d())), "a % b * (c % d)");

		// Divide + modulo
		assert!(divide(modulo(a(), b()), modulo(c(), d())), "a % b / (c % d)");

		// Modulo + modulo
		assert!(modulo(modulo(a(), b()), modulo(c(), d())), "a % b % (c % d)");

		// Add + modulo
		assert!(add(modulo(a(), b()), modulo(c(), d())), "a % b + c % d");

		// Subtract + modulo
		assert!(subtract(modulo(a(), b()), modulo(c(), d())), "a % b - c % d");
	}

	#[test]
	fn format_combinations_add()
	{
		// Function + add
		assert!(function("f", vec![add(a(), b())]), "f(a + b)");
		assert!(function("f", vec![add(a(), b()), add(c(), d()), add(e(), a())]),
			"f(a + b, c + d, e + a)");

		// Absolute value + add
		assert!(absolute_value(add(a(), b())), "|a + b|");

		// Negative + add
		assert!(negative(add(a(), b())), "-(a + b)");

		// Exponentiate + add
		assert!(exponentiate(add(a(), b()), add(c(), d())), "(a + b) ** (c + d)");

		// Multiply + add
		assert!(multiply(add(a(), b()), add(c(), d())), "(a + b) * (c + d)");

		// Divide + add
		assert!(divide(add(a(), b()), add(c(), d())), "(a + b) / (c + d)");

		// Modulo + add
		assert!(modulo(add(a(), b()), add(c(), d())), "(a + b) % (c + d)");

		// Add + add
		assert!(add(add(a(), b()), add(c(), d())), "a + b + c + d");

		// Subtract + add
		assert!(subtract(add(a(), b()), add(c(), d())), "a + b - (c + d)");
	}

	#[test]
	fn format_combinations_subtract()
	{
		// Function + subtract
		assert!(function("f", vec![subtract(a(), b())]), "f(a - b)");
		assert!(function("f", vec![subtract(a(), b()), subtract(c(), d()), subtract(e(), a())]),
			"f(a - b, c - d, e - a)");

		// Absolute value + subtract
		assert!(absolute_value(subtract(a(), b())), "|a - b|");

		// Negative + subtract
		assert!(negative(subtract(a(), b())), "-(a - b)");

		// Exponentiate + subtract
		assert!(exponentiate(subtract(a(), b()), subtract(c(), d())), "(a - b) ** (c - d)");

		// Multiply + subtract
		assert!(multiply(subtract(a(), b()), subtract(c(), d())), "(a - b) * (c - d)");

		// Divide + subtract
		assert!(divide(subtract(a(), b()), subtract(c(), d())), "(a - b) / (c - d)");

		// Modulo + subtract
		assert!(modulo(subtract(a(), b()), subtract(c(), d())), "(a - b) % (c - d)");

		// Add + subtract
		assert!(add(subtract(a(), b()), subtract(c(), d())), "a - b + c - d");

		// Subtract + subtract
		assert!(subtract(subtract(a(), b()), subtract(c(), d())), "a - b - (c - d)");
	}
}
