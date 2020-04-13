use super::Precedence;
use super::terms::*;

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

impl std::fmt::Debug for crate::ImplicationDirection
{
	fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match &self
		{
			Self::LeftToRight => write!(format, "left to right"),
			Self::RightToLeft => write!(format, "right to left"),
		}
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

struct FormulaDisplay<'formula>
{
	parent_precedence: Option<i32>,
	formula: &'formula crate::Formula,
}

fn display_formula(formula: &crate::Formula, parent_precedence: Option<i32>) -> FormulaDisplay
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
			crate::Formula::Implies(crate::Implies{
				direction: crate::ImplicationDirection::LeftToRight, antecedent, implication})
				=> write!(format, "{:?} -> {:?}", display_formula(antecedent, precedence),
					display_formula(implication, precedence))?,
			crate::Formula::Implies(crate::Implies{
				direction: crate::ImplicationDirection::RightToLeft, antecedent, implication})
				=> write!(format, "{:?} <- {:?}", display_formula(implication, precedence),
					display_formula(antecedent, precedence))?,
			crate::Formula::IfAndOnlyIf(arguments) =>
			{
				let mut separator = "";

				assert!(!arguments.is_empty());

				for argument in arguments
				{
					write!(format, "{}{:?}", separator, display_formula(argument, precedence))?;

					separator = " <-> "
				}
			},
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
			crate::Formula::Boolean(true) => write!(format, "true")?,
			crate::Formula::Boolean(false) => write!(format, "false")?,
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
