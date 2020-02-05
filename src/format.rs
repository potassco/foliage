trait Precedence
{
	fn precedence(&self) -> i32;
}

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
			Self::UnaryOperation(
				crate::UnaryOperation{operator: crate::UnaryOperator::AbsoluteValue, ..})
				=> 5,
		}
	}
}

impl Precedence for crate::Formula
{
	fn precedence(&self) -> i32
	{
		match &self
		{
			Self::Predicate(_)
			| Self::Boolean(_)
			| Self::Compare(_)
				=> 0,
			Self::Exists(_)
			| Self::ForAll(_)
				=> 1,
			Self::Not(_)
				=> 2,
			Self::And(_)
				=> 3,
			Self::Or(_)
				=> 4,
			Self::Implies(_)
				=> 5,
			Self::IfAndOnlyIf(_)
				=> 6,
		}
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

impl std::fmt::Debug for crate::PredicateDeclaration
{
	fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(format, "{}/{}", &self.name, self.arity)
	}
}

impl std::fmt::Display for crate::PredicateDeclaration
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

struct TermDisplay<'term>
{
	parent_precedence: Option<i32>,
	term: &'term crate::Term,
}

fn display_term<'term>(term: &'term crate::Term, parent_precedence: Option<i32>)
	-> TermDisplay<'term>
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
		let precedence = Some(precedence);

		if requires_parentheses
		{
			write!(format, "(")?;
		}

		match &self.term
		{
			crate::Term::Boolean(true) => write!(format, "true"),
			crate::Term::Boolean(false) => write!(format, "false"),
			crate::Term::SpecialInteger(crate::SpecialInteger::Infimum) => write!(format, "#inf"),
			crate::Term::SpecialInteger(crate::SpecialInteger::Supremum) => write!(format, "#sup"),
			crate::Term::Integer(value) => write!(format, "{}", value),
			crate::Term::String(value) => write!(format, "\"{}\"", value),
			crate::Term::Variable(variable) => write!(format, "{:?}", variable.declaration),
			crate::Term::Function(function) =>
			{
				write!(format, "{}", function.declaration.name)?;

				assert!(function.declaration.arity == function.arguments.len(),
					"number of function arguments differs from declaration (expected {}, got {})",
					function.declaration.arity, function.arguments.len());

				if function.arguments.len() > 0
				{
					write!(format, "{}(", function.declaration.name)?;

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
			crate::Term::BinaryOperation(
				crate::BinaryOperation{operator: crate::BinaryOperator::Add, left, right})
				=> write!(format, "{:?} + {:?}", display_term(left, precedence),
					display_term(right, precedence)),
			crate::Term::BinaryOperation(
				crate::BinaryOperation{operator: crate::BinaryOperator::Subtract, left, right})
				=> write!(format, "{:?} - {:?}", display_term(left, precedence),
					display_term(right, precedence)),
			crate::Term::BinaryOperation(
				crate::BinaryOperation{operator: crate::BinaryOperator::Multiply, left, right})
				=> write!(format, "{:?} * {:?}", display_term(left, precedence),
					display_term(right, precedence)),
			crate::Term::BinaryOperation(
				crate::BinaryOperation{operator: crate::BinaryOperator::Divide, left, right})
				=> write!(format, "{:?} / {:?}", display_term(left, precedence),
					display_term(right, precedence)),
			crate::Term::BinaryOperation(
				crate::BinaryOperation{operator: crate::BinaryOperator::Modulo, left, right})
				=> write!(format, "{:?} % {:?}", display_term(left, precedence),
					display_term(right, precedence)),
			crate::Term::BinaryOperation(
				crate::BinaryOperation{operator: crate::BinaryOperator::Exponentiate, left, right})
				=> write!(format, "{:?} ** {:?}", display_term(left, precedence),
					display_term(right, precedence)),
			crate::Term::UnaryOperation(
				crate::UnaryOperation{operator: crate::UnaryOperator::Negative, argument})
				=> write!(format, "-{:?}", display_term(argument, precedence)),
			crate::Term::UnaryOperation(
				crate::UnaryOperation{operator: crate::UnaryOperator::AbsoluteValue, argument})
				=> write!(format, "|{:?}|", display_term(argument, precedence)),
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

struct FormulaDisplay<'formula>
{
	parent_precedence: Option<i32>,
	formula: &'formula crate::Formula,
}

fn display_formula<'formula>(formula: &'formula crate::Formula, parent_precedence: Option<i32>)
	-> FormulaDisplay<'formula>
{
	FormulaDisplay
	{
		parent_precedence,
		formula,
	}
}

impl<'formula> std::fmt::Debug for FormulaDisplay<'formula>
{
	fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		let precedence = self.formula.precedence();
		let requires_parentheses = match self.parent_precedence
		{
			Some(parent_precedence) => precedence > parent_precedence,
			None => false,
		};
		let precedence = Some(precedence);

		if requires_parentheses
		{
			write!(format, "(")?;
		}

		match &self.formula
		{
			crate::Formula::Exists(exists) =>
			{
				assert!(!exists.parameters.is_empty());

				write!(format, "exists")?;

				let mut separator = " ";

				for parameter in exists.parameters.iter()
				{
					write!(format, "{}{:?}", separator, parameter)?;

					separator = ", "
				}

				write!(format, " {:?}", display_formula(&exists.argument, precedence))?;
			},
			crate::Formula::ForAll(for_all) =>
			{
				assert!(!for_all.parameters.is_empty());

				write!(format, "forall")?;

				let mut separator = " ";

				for parameter in for_all.parameters.iter()
				{
					write!(format, "{}{:?}", separator, parameter)?;

					separator = ", "
				}

				write!(format, " {:?}", display_formula(&for_all.argument, precedence))?;
			},
			crate::Formula::Not(argument) => write!(format, "not {:?}",
				display_formula(argument, precedence))?,
			crate::Formula::And(arguments) =>
			{
				let mut separator = "";

				assert!(!arguments.is_empty());

				for argument in arguments
				{
					write!(format, "{}{:?}", separator, display_formula(argument, precedence))?;

					separator = " and "
				}
			},
			crate::Formula::Or(arguments) =>
			{
				let mut separator = "";

				assert!(!arguments.is_empty());

				for argument in arguments
				{
					write!(format, "{}{:?}", separator, display_formula(argument, precedence))?;

					separator = " or "
				}
			},
			crate::Formula::Implies(crate::Implies{antecedent, implication})
				=> write!(format, "{:?} -> {:?}", display_formula(antecedent, precedence),
					display_formula(implication, precedence))?,
			crate::Formula::IfAndOnlyIf(crate::IfAndOnlyIf{left, right})
				=> write!(format, "{:?} <-> {:?}", display_formula(left, precedence),
					display_formula(right, precedence))?,
			crate::Formula::Compare(
				crate::Compare{operator: crate::ComparisonOperator::Less, left, right})
				=> write!(format, "{:?} < {:?}", display_term(left, None),
					display_term(right, None))?,
			crate::Formula::Compare(
				crate::Compare{operator: crate::ComparisonOperator::LessOrEqual, left, right})
				=> write!(format, "{:?} <= {:?}", display_term(left, None),
					display_term(right, None))?,
			crate::Formula::Compare(
				crate::Compare{operator: crate::ComparisonOperator::Greater, left, right})
				=> write!(format, "{:?} > {:?}", display_term(left, None),
					display_term(right, None))?,
			crate::Formula::Compare(
				crate::Compare{operator: crate::ComparisonOperator::GreaterOrEqual, left, right})
				=> write!(format, "{:?} >= {:?}", display_term(left, None),
					display_term(right, None))?,
			crate::Formula::Compare(
				crate::Compare{operator: crate::ComparisonOperator::Equal, left, right})
				=> write!(format, "{:?} = {:?}", display_term(left, None),
					display_term(right, None))?,
			crate::Formula::Compare(
				crate::Compare{operator: crate::ComparisonOperator::NotEqual, left, right})
				=> write!(format, "{:?} != {:?}", display_term(left, None),
					display_term(right, None))?,
			crate::Formula::Boolean(true) => write!(format, "#true")?,
			crate::Formula::Boolean(false) => write!(format, "#false")?,
			crate::Formula::Predicate(predicate) =>
			{
				write!(format, "{}", predicate.declaration.name)?;

				if !predicate.arguments.is_empty()
				{
					write!(format, "(")?;

					let mut separator = "";

					for argument in &predicate.arguments
					{
						write!(format, "{}{:?}", separator, display_term(argument, None))?;

						separator = ", "
					}

					write!(format, ")")?;
				}
			},
		}

		if requires_parentheses
		{
			write!(format, ")")?;
		}

		Ok(())
	}
}

impl<'formula> std::fmt::Display for FormulaDisplay<'formula>
{
	fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(format, "{:?}", self)
	}
}

impl std::fmt::Debug for crate::Formula
{
	fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(format, "{:?}", display_formula(&self, None))
	}
}

impl std::fmt::Display for crate::Formula
{
	fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(format, "{}", display_formula(&self, None))
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
