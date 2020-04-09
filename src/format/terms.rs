use super::Precedence;

impl Precedence for crate::Term
{
	fn precedence(&self) -> i32
	{
		match &self
		{
			Self::Boolean(_)
			| Self::Function(_)
			| Self::SpecialInteger(_)
			| Self::Integer(_)
			| Self::String(_)
			| Self::Variable(_)
			| Self::UnaryOperation(
				crate::UnaryOperation{operator: crate::UnaryOperator::AbsoluteValue, ..})
				=> 0,
			Self::UnaryOperation(
				crate::UnaryOperation{operator: crate::UnaryOperator::Negative, ..})
				=> 1,
			Self::BinaryOperation(
				crate::BinaryOperation{operator: crate::BinaryOperator::Exponentiate, ..})
				=> 2,
			Self::BinaryOperation(
				crate::BinaryOperation{operator: crate::BinaryOperator::Multiply, ..})
			| Self::BinaryOperation(
				crate::BinaryOperation{operator: crate::BinaryOperator::Divide, ..})
			| Self::BinaryOperation(
				crate::BinaryOperation{operator: crate::BinaryOperator::Modulo, ..})
				=> 3,
			Self::BinaryOperation(crate::BinaryOperation{operator: crate::BinaryOperator::Add, ..})
			| Self::BinaryOperation(
				crate::BinaryOperation{operator: crate::BinaryOperator::Subtract, ..})
				=> 4,
		}
	}
}

impl std::fmt::Debug for crate::SpecialInteger
{
	fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match &self
		{
			Self::Infimum => write!(format, "#inf"),
			Self::Supremum => write!(format, "#sup"),
		}
	}
}

impl std::fmt::Display for crate::SpecialInteger
{
	fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(format, "{:?}", &self)
	}
}

impl std::fmt::Debug for crate::FunctionDeclaration
{
	fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(format, "{}/{}", &self.name, self.arity)
	}
}

impl std::fmt::Display for crate::FunctionDeclaration
{
	fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(format, "{:?}", &self)
	}
}

impl std::fmt::Debug for crate::VariableDeclaration
{
	fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(format, "{}", &self.name)
	}
}

impl std::fmt::Display for crate::VariableDeclaration
{
	fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(format, "{:?}", &self)
	}
}

pub(crate) struct TermDisplay<'term>
{
	parent_precedence: Option<i32>,
	term: &'term crate::Term,
}

pub(crate) fn display_term(term: &crate::Term, parent_precedence: Option<i32>) -> TermDisplay
{
	TermDisplay
	{
		parent_precedence,
		term,
	}
}

impl<'term> std::fmt::Debug for TermDisplay<'term>
{
	fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		let precedence = self.term.precedence();
		let requires_parentheses = match self.parent_precedence
		{
			Some(parent_precedence) => precedence > parent_precedence,
			None => false,
		};

		if requires_parentheses
		{
			write!(format, "(")?;
		}

		match &self.term
		{
			crate::Term::Boolean(true) => write!(format, "true"),
			crate::Term::Boolean(false) => write!(format, "false"),
			crate::Term::SpecialInteger(value) => write!(format, "{:?}", value),
			crate::Term::Integer(value) => write!(format, "{}", value),
			crate::Term::String(value) => write!(format, "\"{}\"", value),
			crate::Term::Variable(variable) => write!(format, "{:?}", variable.declaration),
			crate::Term::Function(function) =>
			{
				write!(format, "{}", function.declaration.name)?;

				assert!(function.declaration.arity == function.arguments.len(),
					"number of function arguments differs from declaration (expected {}, got {})",
					function.declaration.arity, function.arguments.len());

				if !function.arguments.is_empty()
				{
					write!(format, "(")?;

					let mut separator = "";

					for argument in &function.arguments
					{
						write!(format, "{}{:?}", separator, display_term(&argument, None))?;

						separator = ", ";
					}

					write!(format, ")")?;
				}

				Ok(())
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

				let left_requires_parentheses = binary_operation.left.precedence() == precedence
					// Exponentiation is right-associative and thus requires parentheses when
					// nested on the left side
					&& binary_operation.operator == crate::BinaryOperator::Exponentiate;

				// The subtraction, division, and modulo operators require parentheses around the
				// right argument even if it has the same precedence
				let operator_requires_right_priority = match binary_operation.operator
				{
					crate::BinaryOperator::Subtract
					| crate::BinaryOperator::Divide
					| crate::BinaryOperator::Modulo
						=> true,
					_ => false,
				};

				// Additionally, modulo operations nested to the right of another multiplicative
				// operation always require parentheses
				let right_requires_priority = match *binary_operation.right
				{
					crate::Term::BinaryOperation(
						crate::BinaryOperation{operator: crate::BinaryOperator::Modulo, ..})
						=> true,
					_ => false,
				};

				let right_requires_parentheses = binary_operation.right.precedence() == precedence
					&& (operator_requires_right_priority || right_requires_priority);

				if left_requires_parentheses
				{
					write!(format, "(")?;
				}

				write!(format, "{:?}", display_term(&binary_operation.left, Some(precedence)))?;

				if left_requires_parentheses
				{
					write!(format, ")")?;
				}

				write!(format, " {} ", operator_string)?;

				if right_requires_parentheses
				{
					write!(format, "(")?;
				}

				write!(format, "{:?}", display_term(&binary_operation.right, Some(precedence)))?;

				if right_requires_parentheses
				{
					write!(format, ")")?;
				}

				Ok(())
			},
			crate::Term::UnaryOperation(
				crate::UnaryOperation{operator: crate::UnaryOperator::Negative, argument})
				=> write!(format, "-{:?}", display_term(argument, Some(precedence))),
			crate::Term::UnaryOperation(
				crate::UnaryOperation{operator: crate::UnaryOperator::AbsoluteValue, argument})
				=> write!(format, "|{:?}|", display_term(argument, None)),
		}?;

		if requires_parentheses
		{
			write!(format, ")")?;
		}

		Ok(())
	}
}

impl<'term> std::fmt::Display for TermDisplay<'term>
{
	fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(format, "{:?}", self)
	}
}

impl std::fmt::Debug for crate::Term
{
	fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(format, "{:?}", display_term(&self, None))
	}
}

impl std::fmt::Display for crate::Term
{
	fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(format, "{}", display_term(&self, None))
	}
}
