use crate::flavor::{PredicateDeclaration as _, VariableDeclaration as _};
use super::terms::*;

impl std::fmt::Debug for crate::ComparisonOperator
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		let operator_symbol = match self
		{
			Self::Less => "<",
			Self::LessOrEqual => "<=",
			Self::Greater => ">",
			Self::GreaterOrEqual => ">=",
			Self::Equal => "=",
			Self::NotEqual => "!=",
		};

		write!(formatter, "{}", operator_symbol)
	}
}

impl std::fmt::Debug for crate::ImplicationDirection
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match &self
		{
			Self::LeftToRight => write!(formatter, "left to right"),
			Self::RightToLeft => write!(formatter, "right to left"),
		}
	}
}

impl std::fmt::Debug for crate::PredicateDeclaration
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(formatter, "{}/{}", &self.name, self.arity)
	}
}

impl std::fmt::Display for crate::PredicateDeclaration
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(formatter, "{:?}", &self)
	}
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub(crate) enum FormulaPosition
{
	Any,
	ImpliesAntecedent,
}

pub struct FormulaDisplay<'formula, F>
where
	F: crate::Flavor,
{
	formula: &'formula crate::Formula<F>,
	parent_formula: Option<&'formula crate::Formula<F>>,
	position: FormulaPosition,
	//declarations: &'d D,
}

impl<'formula, F> FormulaDisplay<'formula, F>
where
	F: crate::Flavor,
{
	fn requires_parentheses(&self) -> bool
	{
		use crate::Formula;

		let parent_formula = match self.parent_formula
		{
			Some(parent_formula) => parent_formula,
			None => return false,
		};

		match self.formula
		{
			Formula::Predicate(_)
			| Formula::Boolean(_)
			| Formula::Compare(_)
			| Formula::Not(_)
			| Formula::Exists(_)
			| Formula::ForAll(_)
				=> false,
			Formula::And(formulas)
			| Formula::Or(formulas)
			| Formula::IfAndOnlyIf(formulas) if formulas.len() <= 1
				=> false,
			Formula::And(_) => match *parent_formula
			{
				Formula::Not(_)
				| Formula::Exists(_)
				| Formula::ForAll(_)
					=> true,
				_ => false,
			},
			Formula::Or(_) => match *parent_formula
			{
				Formula::Not(_)
				| Formula::Exists(_)
				| Formula::ForAll(_)
				| Formula::And(_)
					=> true,
				_ => false,
			},
			Formula::Implies(crate::Implies{direction, ..}) => match &*parent_formula
			{
				Formula::Not(_)
				| Formula::Exists(_)
				| Formula::ForAll(_)
				| Formula::And(_)
				| Formula::Or(_)
					=> true,
				Formula::Implies(crate::Implies{direction: parent_direction, ..}) =>
					if direction == parent_direction
					{
						// Implications with the same direction nested on the antecedent side
						// require parentheses because implication is considered right-associative
						self.position == FormulaPosition::ImpliesAntecedent
					}
					else
					{
						// Nested implications with opposite direction always require parentheses
						// because the order of formulas like p <- q -> r would be ambiguous
						true
					},
				_ => false,
			},
			Formula::IfAndOnlyIf(_) => true,
		}
	}
}

pub(crate) fn display_formula<'formula, F>(formula: &'formula crate::Formula<F>,
	parent_formula: Option<&'formula crate::Formula<F>>, position: FormulaPosition)
	-> FormulaDisplay<'formula, F>
where
	F: crate::Flavor,
{
	FormulaDisplay
	{
		formula,
		parent_formula,
		position,
	}
}

impl<'formula, F> std::fmt::Debug for FormulaDisplay<'formula, F>
where
	F: crate::Flavor,
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		let requires_parentheses = self.requires_parentheses();

		if requires_parentheses
		{
			write!(formatter, "(")?;
		}

		match &self.formula
		{
			crate::Formula::Exists(exists) =>
			{
				if exists.parameters.is_empty()
				{
					write!(formatter, "{:?}", display_formula::<F>(&exists.argument,
						self.parent_formula, self.position))?;
				}
				else
				{
					write!(formatter, "exists")?;

					let mut separator = " ";

					for parameter in exists.parameters.iter()
					{
						write!(formatter, "{}", separator)?;
						parameter.display_name(formatter)?;

						separator = ", "
					}

					write!(formatter, " {:?}", display_formula(&exists.argument, Some(self.formula),
						FormulaPosition::Any))?;
				}
			},
			crate::Formula::ForAll(for_all) =>
			{
				if for_all.parameters.is_empty()
				{
					write!(formatter, "{:?}", display_formula(&for_all.argument,
						self.parent_formula, self.position))?;
				}
				else
				{
					write!(formatter, "forall")?;

					let mut separator = " ";

					for parameter in for_all.parameters.iter()
					{
						write!(formatter, "{}", separator)?;
						parameter.display_name(formatter)?;

						separator = ", "
					}

					write!(formatter, " {:?}", display_formula(&for_all.argument,
						Some(self.formula), FormulaPosition::Any))?;
				}
			},
			crate::Formula::Not(argument) => write!(formatter, "not {:?}",
				display_formula(argument, Some(self.formula), FormulaPosition::Any))?,
			crate::Formula::And(arguments) =>
			{
				if arguments.is_empty()
				{
					write!(formatter, "true")?;
				}
				else
				{
					let (parent_formula, position) = match arguments.len()
					{
						1 => (self.parent_formula, self.position),
						_ => (Some(self.formula), FormulaPosition::Any),
					};

					let mut separator = "";

					for argument in arguments
					{
						write!(formatter, "{}{:?}", separator,
							display_formula(argument, parent_formula, position))?;

						separator = " and "
					}
				}
			},
			crate::Formula::Or(arguments) =>
			{
				if arguments.is_empty()
				{
					write!(formatter, "false")?;
				}
				else
				{
					let (parent_formula, position) = match arguments.len()
					{
						1 => (self.parent_formula, self.position),
						_ => (Some(self.formula), FormulaPosition::Any),
					};

					let mut separator = "";

					for argument in arguments
					{
						write!(formatter, "{}{:?}", separator,
							display_formula(argument, parent_formula, position))?;

						separator = " or "
					}
				}
			},
			crate::Formula::Implies(crate::Implies{direction, antecedent, implication}) =>
			{
				let format_antecedent = |formatter: &mut std::fmt::Formatter| -> Result<_, _>
				{
					write!(formatter, "{:?}",
						display_formula(antecedent, Some(self.formula),
							FormulaPosition::ImpliesAntecedent))
				};

				let format_implication = |formatter: &mut std::fmt::Formatter| -> Result<_, _>
				{
					write!(formatter, "{:?}",
						display_formula(implication, Some(self.formula), FormulaPosition::Any))
				};

				match direction
				{
					crate::ImplicationDirection::LeftToRight =>
					{
						format_antecedent(formatter)?;
						write!(formatter, " -> ")?;
						format_implication(formatter)?;
					},
					crate::ImplicationDirection::RightToLeft =>
					{
						format_implication(formatter)?;
						write!(formatter, " <- ")?;
						format_antecedent(formatter)?;
					},
				}
			},
			crate::Formula::IfAndOnlyIf(arguments) =>
			{
				if arguments.is_empty()
				{
					write!(formatter, "true")?;
				}
				else
				{
					let (parent_formula, position) = match arguments.len()
					{
						1 => (self.parent_formula, self.position),
						_ => (Some(self.formula), FormulaPosition::Any),
					};

					let mut separator = "";

					for argument in arguments
					{
						write!(formatter, "{}{:?}", separator,
							display_formula(argument, parent_formula, position))?;

						separator = " <-> "
					}
				}
			},
			crate::Formula::Compare(compare) =>
			{
				let operator_string = match compare.operator
				{
					crate::ComparisonOperator::Less => "<",
					crate::ComparisonOperator::LessOrEqual => "<=",
					crate::ComparisonOperator::Greater => ">",
					crate::ComparisonOperator::GreaterOrEqual => ">=",
					crate::ComparisonOperator::Equal => "=",
					crate::ComparisonOperator::NotEqual => "!=",
				};

				write!(formatter, "{:?} {} {:?}",
					display_term(&compare.left, None, TermPosition::Any), operator_string,
					display_term(&compare.right, None, TermPosition::Any))?;
			},
			crate::Formula::Boolean(true) => write!(formatter, "true")?,
			crate::Formula::Boolean(false) => write!(formatter, "false")?,
			crate::Formula::Predicate(predicate) =>
			{
				predicate.declaration.display_name(formatter)?;

				if !predicate.arguments.is_empty()
				{
					write!(formatter, "(")?;

					let mut separator = "";

					for argument in &predicate.arguments
					{
						write!(formatter, "{}{:?}", separator, display_term(argument, None,
							TermPosition::Any))?;

						separator = ", "
					}

					write!(formatter, ")")?;
				}
			},
		}

		if requires_parentheses
		{
			write!(formatter, ")")?;
		}

		Ok(())
	}
}

impl<'formula, F> std::fmt::Display for FormulaDisplay<'formula, F>
where
	F: crate::Flavor,
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(formatter, "{:?}", self)
	}
}

impl<F> std::fmt::Debug for crate::Formula<F>
where
	F: crate::Flavor,
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(formatter, "{:?}", display_formula(&self, None, FormulaPosition::Any))
	}
}

impl<F> std::fmt::Display for crate::Formula<F>
where
	F: crate::Flavor,
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(formatter, "{}", display_formula(&self, None, FormulaPosition::Any))
	}
}

#[cfg(test)]
mod tests
{
	use crate::*;
	use format::terms::tests::*;
	type Formula = crate::Formula<DefaultFlavor>;
	type Term = crate::Term<DefaultFlavor>;
	type VariableDeclarations = crate::VariableDeclarations<DefaultFlavor>;

	macro_rules! assert
	{
		($formula:expr, $output:expr) =>
		{
			assert_eq!(format($formula), $output);
		};
	}

	// Tests all neutral intermediates (such as 1-ary conjunction)
	macro_rules! assert_all
	{
		($intermediate:ident, $formula:expr, $output:expr) =>
		{
			let $intermediate = |f: Box<Formula>| f;
			assert!($formula, $output);

			let $intermediate = |f: Box<Formula>| exists(vec![], f);
			assert!($formula, $output);

			let $intermediate = |f: Box<Formula>| for_all(vec![], f);
			assert!($formula, $output);

			let $intermediate = |f: Box<Formula>| and(vec![f]);
			assert!($formula, $output);

			let $intermediate = |f: Box<Formula>| or(vec![f]);
			assert!($formula, $output);

			let $intermediate = |f: Box<Formula>| if_and_only_if(vec![f]);
			assert!($formula, $output);
		};
	}

	fn format(formula: Box<Formula>) -> String
	{
		format!("{}", formula)
	}

	fn and(arguments: Vec<Box<Formula>>) -> Box<Formula>
	{
		Box::new(Formula::and(arguments.into_iter().map(|x| *x).collect()))
	}

	fn equal(left: Box<Term>, right: Box<Term>) -> Box<Formula>
	{
		Box::new(Formula::equal(left, right))
	}

	fn exists(parameters: VariableDeclarations, argument: Box<Formula>) -> Box<Formula>
	{
		Box::new(Formula::exists(std::rc::Rc::new(parameters), argument))
	}

	fn false_() -> Box<Formula>
	{
		Box::new(Formula::false_())
	}

	fn for_all(parameters: VariableDeclarations, argument: Box<Formula>) -> Box<Formula>
	{
		Box::new(Formula::for_all(std::rc::Rc::new(parameters), argument))
	}

	fn greater(left: Box<Term>, right: Box<Term>) -> Box<Formula>
	{
		Box::new(Formula::greater(left, right))
	}

	fn greater_or_equal(left: Box<Term>, right: Box<Term>) -> Box<Formula>
	{
		Box::new(Formula::greater_or_equal(left, right))
	}

	fn if_and_only_if(arguments: Vec<Box<Formula>>) -> Box<Formula>
	{
		Box::new(Formula::if_and_only_if(arguments.into_iter().map(|x| *x).collect()))
	}

	fn implies(direction: ImplicationDirection, antecedent: Box<Formula>, implication: Box<Formula>)
		-> Box<Formula>
	{
		Box::new(Formula::implies(direction, antecedent, implication))
	}

	fn less(left: Box<Term>, right: Box<Term>) -> Box<Formula>
	{
		Box::new(Formula::less(left, right))
	}

	fn less_or_equal(left: Box<Term>, right: Box<Term>) -> Box<Formula>
	{
		Box::new(Formula::less_or_equal(left, right))
	}

	fn not(argument: Box<Formula>) -> Box<Formula>
	{
		Box::new(Formula::not(argument))
	}

	fn not_equal(left: Box<Term>, right: Box<Term>) -> Box<Formula>
	{
		Box::new(Formula::not_equal(left, right))
	}

	fn or(arguments: Vec<Box<Formula>>) -> Box<Formula>
	{
		Box::new(Formula::or(arguments.into_iter().map(|x| *x).collect()))
	}

	fn predicate(name: &str, arguments: Vec<Box<Term>>) -> Box<Formula>
	{
		Box::new(Formula::predicate(predicate_declaration(name, arguments.len()),
			arguments.into_iter().map(|x| *x).collect()))
	}

	fn predicate_declaration(name: &str, arity: usize) -> std::rc::Rc<PredicateDeclaration>
	{
		std::rc::Rc::new(PredicateDeclaration::new(name.to_string(), arity))
	}

	fn true_() -> Box<Formula>
	{
		Box::new(Formula::true_())
	}

	fn x() -> std::rc::Rc<VariableDeclaration>
	{
		variable_declaration("X")
	}

	fn y() -> std::rc::Rc<VariableDeclaration>
	{
		variable_declaration("Y")
	}

	fn z() -> std::rc::Rc<VariableDeclaration>
	{
		variable_declaration("Z")
	}

	fn xyz() -> VariableDeclarations
	{
		vec![x(), y(), z()]
	}

	fn x1y1z1() -> VariableDeclarations
	{
		vec![variable_declaration("X1"), variable_declaration("Y1"), variable_declaration("Z1")]
	}

	fn x2y2z2() -> VariableDeclarations
	{
		vec![variable_declaration("X2"), variable_declaration("Y2"), variable_declaration("Z2")]
	}

	fn x3y3z3() -> VariableDeclarations
	{
		vec![variable_declaration("X3"), variable_declaration("Y3"), variable_declaration("Z3")]
	}

	fn p() -> Box<Formula>
	{
		predicate("p", vec![])
	}

	fn q() -> Box<Formula>
	{
		predicate("q", vec![])
	}

	fn p1() -> Box<Formula>
	{
		predicate("p1", vec![])
	}

	fn q1() -> Box<Formula>
	{
		predicate("q1", vec![])
	}

	fn p2() -> Box<Formula>
	{
		predicate("p2", vec![])
	}

	fn q2() -> Box<Formula>
	{
		predicate("q2", vec![])
	}

	fn p3() -> Box<Formula>
	{
		predicate("p3", vec![])
	}

	fn q3() -> Box<Formula>
	{
		predicate("q3", vec![])
	}

	fn r() -> Box<Formula>
	{
		predicate("r", vec![])
	}

	fn pqr() -> Vec<Box<Formula>>
	{
		vec![p(), q(), r()]
	}

	fn p1q1r1() -> Vec<Box<Formula>>
	{
		vec![p1(), q1(), predicate("r1", vec![])]
	}

	fn p2q2r2() -> Vec<Box<Formula>>
	{
		vec![p2(), q2(), predicate("r2", vec![])]
	}

	fn p3q3r3() -> Vec<Box<Formula>>
	{
		vec![p3(), q3(), predicate("r3", vec![])]
	}

	fn implies_right(antecedent: Box<Formula>, implication: Box<Formula>) -> Box<Formula>
	{
		implies(ImplicationDirection::LeftToRight, antecedent, implication)
	}

	fn implies_left(antecedent: Box<Formula>, implication: Box<Formula>) -> Box<Formula>
	{
		implies(ImplicationDirection::RightToLeft, antecedent, implication)
	}

	#[test]
	fn format_boolean()
	{
		assert!(true_(), "true");
		assert!(false_(), "false");
	}

	#[test]
	fn format_compare()
	{
		assert!(greater(a(), b()), "a > b");
		assert!(less(a(), b()), "a < b");
		assert!(less_or_equal(a(), b()), "a <= b");
		assert!(greater_or_equal(a(), b()), "a >= b");
		assert!(equal(a(), b()), "a = b");
		assert!(not_equal(a(), b()), "a != b");

		assert!(greater(multiply(add(a(), b()), c()), absolute_value(subtract(d(), e()))),
			"(a + b) * c > |d - e|");
		assert!(less(multiply(add(a(), b()), c()), absolute_value(subtract(d(), e()))),
			"(a + b) * c < |d - e|");
		assert!(less_or_equal(multiply(add(a(), b()), c()), absolute_value(subtract(d(), e()))),
			"(a + b) * c <= |d - e|");
		assert!(greater_or_equal(multiply(add(a(), b()), c()), absolute_value(subtract(d(), e()))),
			"(a + b) * c >= |d - e|");
		assert!(equal(multiply(add(a(), b()), c()), absolute_value(subtract(d(), e()))),
			"(a + b) * c = |d - e|");
		assert!(not_equal(multiply(add(a(), b()), c()), absolute_value(subtract(d(), e()))),
			"(a + b) * c != |d - e|");
	}

	#[test]
	fn format_predicate()
	{
		assert!(p(), "p");
		assert!(predicate("predicate", vec![]), "predicate");
		assert!(predicate("q", vec![a()]), "q(a)");
		assert!(predicate("q", abc()), "q(a, b, c)");
		assert!(predicate("predicate", abc()), "predicate(a, b, c)");

		assert!(predicate("predicate", vec![
			exponentiate(absolute_value(multiply(a(), integer(-20))), integer(2)),
			string("test"),
			function("f", vec![multiply(add(b(), c()), subtract(b(), c())), infimum(),
				variable("X")])]),
			"predicate(|a * -20| ** 2, \"test\", f((b + c) * (b - c), #inf, X))");

		// TODO: escape predicates that start with capital letters or that conflict with keywords
	}

	#[test]
	fn format_predicate_declaration()
	{
		assert_eq!(format!("{}", predicate_declaration("p", 0)), "p/0");
		assert_eq!(format!("{}", predicate_declaration("predicate", 0)), "predicate/0");
		assert_eq!(format!("{}", predicate_declaration("q", 1)), "q/1");
		assert_eq!(format!("{}", predicate_declaration("q", 3)), "q/3");
		assert_eq!(format!("{}", predicate_declaration("predicate", 3)), "predicate/3");
	}

	#[test]
	fn format_exists()
	{
		assert!(exists(vec![], p()), "p");
		assert!(exists(vec![x()], p()), "exists X p");
		assert!(exists(xyz(), p()), "exists X, Y, Z p");
	}

	#[test]
	fn format_for_all()
	{
		assert!(for_all(vec![], p()), "p");
		assert!(for_all(vec![x()], p()), "forall X p");
		assert!(for_all(xyz(), p()), "forall X, Y, Z p");
	}

	#[test]
	fn format_not()
	{
		assert!(not(p()), "not p");
	}

	#[test]
	fn format_and()
	{
		assert!(and(vec![]), "true");
		assert!(and(vec![p()]), "p");
		assert!(and(pqr()), "p and q and r");
	}

	#[test]
	fn format_or()
	{
		assert!(or(vec![]), "false");
		assert!(or(vec![p()]), "p");
		assert!(or(pqr()), "p or q or r");
	}

	#[test]
	fn format_implies()
	{
		assert!(implies_right(p(), q()), "p -> q");
		assert!(implies_left(p(), q()), "q <- p");
	}

	#[test]
	fn format_if_and_only_if()
	{
		assert!(if_and_only_if(vec![]), "true");
		assert!(if_and_only_if(vec![p()]), "p");
		assert!(if_and_only_if(vec![p(), q()]), "p <-> q");
		assert!(if_and_only_if(pqr()), "p <-> q <-> r");
	}

	#[test]
	fn format_combinations_boolean()
	{
		// Not + Boolean
		assert!(not(true_()), "not true");
		assert!(not(false_()), "not false");

		// Quantified formula + Boolean
		assert!(exists(vec![], true_()), "true");
		assert!(exists(vec![], false_()), "false");
		assert!(exists(vec![x()], true_()), "exists X true");
		assert!(exists(vec![x()], false_()), "exists X false");
		assert!(exists(xyz(), true_()), "exists X, Y, Z true");
		assert!(exists(xyz(), false_()), "exists X, Y, Z false");
		assert!(for_all(vec![], true_()), "true");
		assert!(for_all(vec![], false_()), "false");
		assert!(for_all(vec![x()], true_()), "forall X true");
		assert!(for_all(vec![x()], false_()), "forall X false");
		assert!(for_all(xyz(), true_()), "forall X, Y, Z true");
		assert!(for_all(xyz(), false_()), "forall X, Y, Z false");

		// And + Boolean
		assert!(and(vec![true_()]), "true");
		assert!(and(vec![true_(), true_(), true_()]), "true and true and true");
		assert!(and(vec![false_()]), "false");
		assert!(and(vec![false_(), false_(), false_()]), "false and false and false");

		// Or + Boolean
		assert!(or(vec![true_()]), "true");
		assert!(or(vec![true_(), true_(), true_()]), "true or true or true");
		assert!(or(vec![false_()]), "false");
		assert!(or(vec![false_(), false_(), false_()]), "false or false or false");

		// Implies + Boolean
		assert!(implies_right(true_(), true_()), "true -> true");
		assert!(implies_left(true_(), true_()), "true <- true");
		assert!(implies_right(false_(), false_()), "false -> false");
		assert!(implies_left(false_(), false_()), "false <- false");

		// If and only if + Boolean
		assert!(if_and_only_if(vec![true_()]), "true");
		assert!(if_and_only_if(vec![true_(), true_(), true_()]), "true <-> true <-> true");
		assert!(if_and_only_if(vec![false_()]), "false");
		assert!(if_and_only_if(vec![false_(), false_(), false_()]), "false <-> false <-> false");
	}

	#[test]
	fn format_combinations_compare()
	{
		let term_1 = || multiply(add(a(), b()), c());
		let term_2 = || absolute_value(subtract(d(), e()));
		let term_3 = || exponentiate(a(), exponentiate(b(), c()));
		let term_4 = || negative(function("f", vec![integer(5), add(variable("X"), integer(3))]));

		// Not + compare
		assert!(not(greater(term_1(), term_2())), "not (a + b) * c > |d - e|");
		assert!(not(less(term_1(), term_2())), "not (a + b) * c < |d - e|");
		assert!(not(less_or_equal(term_1(), term_2())), "not (a + b) * c <= |d - e|");
		assert!(not(greater_or_equal(term_1(), term_2())), "not (a + b) * c >= |d - e|");
		assert!(not(equal(term_1(), term_2())), "not (a + b) * c = |d - e|");
		assert!(not(not_equal(term_1(), term_2())), "not (a + b) * c != |d - e|");

		// Quantified formula + compare
		assert!(exists(vec![], greater(term_1(), term_2())), "(a + b) * c > |d - e|");
		assert!(exists(vec![], less(term_1(), term_2())), "(a + b) * c < |d - e|");
		assert!(exists(vec![], less_or_equal(term_1(), term_2())), "(a + b) * c <= |d - e|");
		assert!(exists(vec![], greater_or_equal(term_1(), term_2())), "(a + b) * c >= |d - e|");
		assert!(exists(vec![], equal(term_1(), term_2())), "(a + b) * c = |d - e|");
		assert!(exists(vec![], not_equal(term_1(), term_2())), "(a + b) * c != |d - e|");
		assert!(for_all(vec![], greater(term_1(), term_2())), "(a + b) * c > |d - e|");
		assert!(for_all(vec![], less(term_1(), term_2())), "(a + b) * c < |d - e|");
		assert!(for_all(vec![], less_or_equal(term_1(), term_2())), "(a + b) * c <= |d - e|");
		assert!(for_all(vec![], greater_or_equal(term_1(), term_2())), "(a + b) * c >= |d - e|");
		assert!(for_all(vec![], equal(term_1(), term_2())), "(a + b) * c = |d - e|");
		assert!(for_all(vec![], not_equal(term_1(), term_2())), "(a + b) * c != |d - e|");
		assert!(exists(vec![x()], greater(term_1(), term_2())), "exists X (a + b) * c > |d - e|");
		assert!(exists(vec![x()], less(term_1(), term_2())), "exists X (a + b) * c < |d - e|");
		assert!(exists(vec![x()], less_or_equal(term_1(), term_2())),
			"exists X (a + b) * c <= |d - e|");
		assert!(exists(vec![x()], greater_or_equal(term_1(), term_2())),
			"exists X (a + b) * c >= |d - e|");
		assert!(exists(vec![x()], equal(term_1(), term_2())), "exists X (a + b) * c = |d - e|");
		assert!(exists(vec![x()], not_equal(term_1(), term_2())),
			"exists X (a + b) * c != |d - e|");
		assert!(for_all(vec![x()], greater(term_1(), term_2())), "forall X (a + b) * c > |d - e|");
		assert!(for_all(vec![x()], less(term_1(), term_2())), "forall X (a + b) * c < |d - e|");
		assert!(for_all(vec![x()], less_or_equal(term_1(), term_2())),
			"forall X (a + b) * c <= |d - e|");
		assert!(for_all(vec![x()], greater_or_equal(term_1(), term_2())),
			"forall X (a + b) * c >= |d - e|");
		assert!(for_all(vec![x()], equal(term_1(), term_2())), "forall X (a + b) * c = |d - e|");
		assert!(for_all(vec![x()], not_equal(term_1(), term_2())),
			"forall X (a + b) * c != |d - e|");
		assert!(exists(xyz(), greater(term_1(), term_2())), "exists X, Y, Z (a + b) * c > |d - e|");
		assert!(exists(xyz(), less(term_1(), term_2())), "exists X, Y, Z (a + b) * c < |d - e|");
		assert!(exists(xyz(), less_or_equal(term_1(), term_2())),
			"exists X, Y, Z (a + b) * c <= |d - e|");
		assert!(exists(xyz(), greater_or_equal(term_1(), term_2())),
			"exists X, Y, Z (a + b) * c >= |d - e|");
		assert!(exists(xyz(), equal(term_1(), term_2())), "exists X, Y, Z (a + b) * c = |d - e|");
		assert!(exists(xyz(), not_equal(term_1(), term_2())),
			"exists X, Y, Z (a + b) * c != |d - e|");
		assert!(for_all(xyz(), greater(term_1(), term_2())),
			"forall X, Y, Z (a + b) * c > |d - e|");
		assert!(for_all(xyz(), less(term_1(), term_2())), "forall X, Y, Z (a + b) * c < |d - e|");
		assert!(for_all(xyz(), less_or_equal(term_1(), term_2())),
			"forall X, Y, Z (a + b) * c <= |d - e|");
		assert!(for_all(xyz(), greater_or_equal(term_1(), term_2())),
			"forall X, Y, Z (a + b) * c >= |d - e|");
		assert!(for_all(xyz(), equal(term_1(), term_2())), "forall X, Y, Z (a + b) * c = |d - e|");
		assert!(for_all(xyz(), not_equal(term_1(), term_2())),
			"forall X, Y, Z (a + b) * c != |d - e|");

		// And + compare
		assert!(and(vec![greater(term_1(), term_2()), greater(term_3(), term_4()),
			greater(term_2(), term_4())]),
			"(a + b) * c > |d - e| and a ** b ** c > -f(5, X + 3) and |d - e| > -f(5, X + 3)");
		assert!(and(vec![less(term_1(), term_2()), less(term_3(), term_4()),
			less(term_2(), term_4())]),
			"(a + b) * c < |d - e| and a ** b ** c < -f(5, X + 3) and |d - e| < -f(5, X + 3)");
		assert!(and(vec![less_or_equal(term_1(), term_2()), less_or_equal(term_3(), term_4()),
			less_or_equal(term_2(), term_4())]),
			"(a + b) * c <= |d - e| and a ** b ** c <= -f(5, X + 3) and |d - e| <= -f(5, X + 3)");
		assert!(and(vec![greater_or_equal(term_1(), term_2()), greater_or_equal(term_3(), term_4()),
			greater_or_equal(term_2(), term_4())]),
			"(a + b) * c >= |d - e| and a ** b ** c >= -f(5, X + 3) and |d - e| >= -f(5, X + 3)");
		assert!(and(vec![equal(term_1(), term_2()), equal(term_3(), term_4()),
			equal(term_2(), term_4())]),
			"(a + b) * c = |d - e| and a ** b ** c = -f(5, X + 3) and |d - e| = -f(5, X + 3)");
		assert!(and(vec![not_equal(term_1(), term_2()), not_equal(term_3(), term_4()),
			not_equal(term_2(), term_4())]),
			"(a + b) * c != |d - e| and a ** b ** c != -f(5, X + 3) and |d - e| != -f(5, X + 3)");

		// Or + compare
		assert!(or(vec![greater(term_1(), term_2()), greater(term_3(), term_4()),
			greater(term_2(), term_4())]),
			"(a + b) * c > |d - e| or a ** b ** c > -f(5, X + 3) or |d - e| > -f(5, X + 3)");
		assert!(or(vec![less(term_1(), term_2()), less(term_3(), term_4()),
			less(term_2(), term_4())]),
			"(a + b) * c < |d - e| or a ** b ** c < -f(5, X + 3) or |d - e| < -f(5, X + 3)");
		assert!(or(vec![less_or_equal(term_1(), term_2()), less_or_equal(term_3(), term_4()),
			less_or_equal(term_2(), term_4())]),
			"(a + b) * c <= |d - e| or a ** b ** c <= -f(5, X + 3) or |d - e| <= -f(5, X + 3)");
		assert!(or(vec![greater_or_equal(term_1(), term_2()), greater_or_equal(term_3(), term_4()),
			greater_or_equal(term_2(), term_4())]),
			"(a + b) * c >= |d - e| or a ** b ** c >= -f(5, X + 3) or |d - e| >= -f(5, X + 3)");
		assert!(or(vec![equal(term_1(), term_2()), equal(term_3(), term_4()),
			equal(term_2(), term_4())]),
			"(a + b) * c = |d - e| or a ** b ** c = -f(5, X + 3) or |d - e| = -f(5, X + 3)");
		assert!(or(vec![not_equal(term_1(), term_2()), not_equal(term_3(), term_4()),
			not_equal(term_2(), term_4())]),
			"(a + b) * c != |d - e| or a ** b ** c != -f(5, X + 3) or |d - e| != -f(5, X + 3)");

		// Implies + compare
		assert!(implies_right(greater(term_1(), term_2()), greater(term_3(), term_4())),
			"(a + b) * c > |d - e| -> a ** b ** c > -f(5, X + 3)");
		assert!(implies_right(less(term_1(), term_2()), less(term_3(), term_4())),
			"(a + b) * c < |d - e| -> a ** b ** c < -f(5, X + 3)");
		assert!(implies_right(less_or_equal(term_1(), term_2()), less_or_equal(term_3(), term_4())),
			"(a + b) * c <= |d - e| -> a ** b ** c <= -f(5, X + 3)");
		assert!(implies_right(greater_or_equal(term_1(), term_2()),
			greater_or_equal(term_3(), term_4())),
			"(a + b) * c >= |d - e| -> a ** b ** c >= -f(5, X + 3)");
		assert!(implies_right(equal(term_1(), term_2()), equal(term_3(), term_4())),
			"(a + b) * c = |d - e| -> a ** b ** c = -f(5, X + 3)");
		assert!(implies_right(not_equal(term_1(), term_2()), not_equal(term_3(), term_4())),
			"(a + b) * c != |d - e| -> a ** b ** c != -f(5, X + 3)");
		assert!(implies_left(greater(term_1(), term_2()), greater(term_3(), term_4())),
			"a ** b ** c > -f(5, X + 3) <- (a + b) * c > |d - e|");
		assert!(implies_left(less(term_1(), term_2()), less(term_3(), term_4())),
			"a ** b ** c < -f(5, X + 3) <- (a + b) * c < |d - e|");
		assert!(implies_left(less_or_equal(term_1(), term_2()), less_or_equal(term_3(), term_4())),
			"a ** b ** c <= -f(5, X + 3) <- (a + b) * c <= |d - e|");
		assert!(implies_left(greater_or_equal(term_1(), term_2()),
			greater_or_equal(term_3(), term_4())),
			"a ** b ** c >= -f(5, X + 3) <- (a + b) * c >= |d - e|");
		assert!(implies_left(equal(term_1(), term_2()), equal(term_3(), term_4())),
			"a ** b ** c = -f(5, X + 3) <- (a + b) * c = |d - e|");
		assert!(implies_left(not_equal(term_1(), term_2()), not_equal(term_3(), term_4())),
			"a ** b ** c != -f(5, X + 3) <- (a + b) * c != |d - e|");

		// If and only if + compare
		assert!(if_and_only_if(vec![greater(term_1(), term_2()), greater(term_3(), term_4()),
			greater(term_2(), term_4())]),
			"(a + b) * c > |d - e| <-> a ** b ** c > -f(5, X + 3) <-> |d - e| > -f(5, X + 3)");
		assert!(if_and_only_if(vec![less(term_1(), term_2()), less(term_3(), term_4()),
			less(term_2(), term_4())]),
			"(a + b) * c < |d - e| <-> a ** b ** c < -f(5, X + 3) <-> |d - e| < -f(5, X + 3)");
		assert!(if_and_only_if(vec![less_or_equal(term_1(), term_2()),
			less_or_equal(term_3(), term_4()), less_or_equal(term_2(), term_4())]),
			"(a + b) * c <= |d - e| <-> a ** b ** c <= -f(5, X + 3) <-> |d - e| <= -f(5, X + 3)");
		assert!(if_and_only_if(vec![greater_or_equal(term_1(), term_2()),
			greater_or_equal(term_3(), term_4()), greater_or_equal(term_2(), term_4())]),
			"(a + b) * c >= |d - e| <-> a ** b ** c >= -f(5, X + 3) <-> |d - e| >= -f(5, X + 3)");
		assert!(if_and_only_if(vec![equal(term_1(), term_2()), equal(term_3(), term_4()),
			equal(term_2(), term_4())]),
			"(a + b) * c = |d - e| <-> a ** b ** c = -f(5, X + 3) <-> |d - e| = -f(5, X + 3)");
		assert!(if_and_only_if(vec![not_equal(term_1(), term_2()), not_equal(term_3(), term_4()),
			not_equal(term_2(), term_4())]),
			"(a + b) * c != |d - e| <-> a ** b ** c != -f(5, X + 3) <-> |d - e| != -f(5, X + 3)");
	}

	#[test]
	fn format_combinations_not()
	{
		// Not + not
		assert!(not(not(p())), "not not p");

		// Quantified formulas + not
		assert_all!(i, exists(vec![x()], i(not(p()))), "exists X not p");
		assert_all!(i, for_all(vec![x()], i(not(p()))), "forall X not p");
		assert_all!(i, exists(xyz(), i(not(p()))), "exists X, Y, Z not p");
		assert_all!(i, for_all(xyz(), i(not(p()))), "forall X, Y, Z not p");

		// And + not
		assert_all!(i, and(vec![i(not(p()))]), "not p");
		assert_all!(i, and(vec![i(not(p())), i(not(q())), i(not(r()))]),
			"not p and not q and not r");

		// Or + not
		assert_all!(i, or(vec![i(not(p()))]), "not p");
		assert_all!(i, or(vec![i(not(p())), i(not(q())), i(not(r()))]), "not p or not q or not r");

		// Implies + not
		assert_all!(i, implies_right(i(not(p())), i(not(q()))), "not p -> not q");
		assert_all!(i, implies_left(i(not(p())), i(not(q()))), "not q <- not p");

		// If and only if + not
		assert_all!(i, if_and_only_if(vec![i(not(p()))]), "not p");
		assert_all!(i, if_and_only_if(vec![i(not(p())), i(not(q())), i(not(r()))]),
			"not p <-> not q <-> not r");
	}

	#[test]
	fn format_combinations_quantified_formula()
	{
		// Not + quantified formula
		assert_all!(i, not(exists(xyz(), i(p()))), "not exists X, Y, Z p");
		assert_all!(i, not(for_all(xyz(), i(p()))), "not forall X, Y, Z p");

		// Quantified formula + quantified formula
		assert_all!(i, exists(vec![x()], i(exists(vec![y()], p()))), "exists X exists Y p");
		assert_all!(i, exists(vec![x()], i(for_all(vec![y()], p()))), "exists X forall Y p");
		assert_all!(i, for_all(vec![x()], i(exists(vec![y()], p()))), "forall X exists Y p");
		assert_all!(i, for_all(vec![x()], i(for_all(vec![y()], p()))), "forall X forall Y p");
		assert_all!(i, exists(x1y1z1(), i(exists(x2y2z2(), p()))),
			"exists X1, Y1, Z1 exists X2, Y2, Z2 p");
		assert_all!(i, exists(x1y1z1(), i(for_all(x2y2z2(), p()))),
			"exists X1, Y1, Z1 forall X2, Y2, Z2 p");
		assert_all!(i, for_all(x1y1z1(), i(exists(x2y2z2(), p()))),
			"forall X1, Y1, Z1 exists X2, Y2, Z2 p");
		assert_all!(i, for_all(x1y1z1(), i(for_all(x2y2z2(), p()))),
			"forall X1, Y1, Z1 forall X2, Y2, Z2 p");

		// And + quantified formula
		assert_all!(i, and(vec![i(exists(xyz(), p()))]), "exists X, Y, Z p");
		assert_all!(i, and(vec![i(for_all(xyz(), p()))]), "forall X, Y, Z p");
		assert_all!(i, and(vec![i(exists(x1y1z1(), p())), i(exists(x2y2z2(), q())),
			i(exists(x3y3z3(), r()))]),
			"exists X1, Y1, Z1 p and exists X2, Y2, Z2 q and exists X3, Y3, Z3 r");
		assert_all!(i, and(vec![i(for_all(x1y1z1(), p())), i(for_all(x2y2z2(), q())),
			i(for_all(x3y3z3(), r()))]),
			"forall X1, Y1, Z1 p and forall X2, Y2, Z2 q and forall X3, Y3, Z3 r");

		// Or + quantified formula
		assert_all!(i, or(vec![i(exists(xyz(), p()))]), "exists X, Y, Z p");
		assert_all!(i, or(vec![i(for_all(xyz(), p()))]), "forall X, Y, Z p");
		assert_all!(i, or(vec![i(exists(x1y1z1(), p())), i(exists(x2y2z2(), q())),
			i(exists(x3y3z3(), r()))]),
			"exists X1, Y1, Z1 p or exists X2, Y2, Z2 q or exists X3, Y3, Z3 r");
		assert_all!(i, or(vec![i(for_all(x1y1z1(), p())), i(for_all(x2y2z2(), q())),
			i(for_all(x3y3z3(), r()))]),
			"forall X1, Y1, Z1 p or forall X2, Y2, Z2 q or forall X3, Y3, Z3 r");

		// Implies + quantified formula
		assert_all!(i, implies_right(i(exists(x1y1z1(), p())), i(exists(x2y2z2(), q()))),
			"exists X1, Y1, Z1 p -> exists X2, Y2, Z2 q");
		assert_all!(i, implies_left(i(exists(x1y1z1(), p())), i(exists(x2y2z2(), q()))),
			"exists X2, Y2, Z2 q <- exists X1, Y1, Z1 p");
		assert_all!(i, implies_right(i(for_all(x1y1z1(), p())), i(for_all(x2y2z2(), q()))),
			"forall X1, Y1, Z1 p -> forall X2, Y2, Z2 q");
		assert_all!(i, implies_left(i(for_all(x1y1z1(), p())), i(for_all(x2y2z2(), q()))),
			"forall X2, Y2, Z2 q <- forall X1, Y1, Z1 p");

		// If and only if + quantified formula
		assert_all!(i, if_and_only_if(vec![i(exists(x1y1z1(), p()))]), "exists X1, Y1, Z1 p");
		assert_all!(i, if_and_only_if(vec![i(for_all(x1y1z1(), p()))]), "forall X1, Y1, Z1 p");
		assert_all!(i, if_and_only_if(vec![i(exists(x1y1z1(), p())), i(exists(x2y2z2(), q())),
			i(exists(x3y3z3(), r()))]),
			"exists X1, Y1, Z1 p <-> exists X2, Y2, Z2 q <-> exists X3, Y3, Z3 r");
		assert_all!(i, if_and_only_if(vec![i(for_all(x1y1z1(), p())), i(for_all(x2y2z2(), q())),
			i(for_all(x3y3z3(), r()))]),
			"forall X1, Y1, Z1 p <-> forall X2, Y2, Z2 q <-> forall X3, Y3, Z3 r");
	}

	#[test]
	fn format_combinations_and()
	{
		// Not + and
		assert_all!(i, not(i(and(vec![p()]))), "not p");
		assert_all!(i, not(i(and(pqr()))), "not (p and q and r)");

		// Quantified formula + and
		assert_all!(i, exists(vec![x()], i(and(vec![p()]))), "exists X p");
		assert_all!(i, for_all(vec![x()], i(and(vec![p()]))), "forall X p");
		assert_all!(i, exists(vec![x()], i(and(pqr()))), "exists X (p and q and r)");
		assert_all!(i, for_all(vec![x()], i(and(pqr()))), "forall X (p and q and r)");
		assert_all!(i, exists(xyz(), i(and(vec![p()]))), "exists X, Y, Z p");
		assert_all!(i, for_all(xyz(), i(and(vec![p()]))), "forall X, Y, Z p");
		assert_all!(i, exists(xyz(), i(and(pqr()))), "exists X, Y, Z (p and q and r)");
		assert_all!(i, for_all(xyz(), i(and(pqr()))), "forall X, Y, Z (p and q and r)");

		// And + and
		assert_all!(i, and(vec![i(and(vec![p()]))]), "p");
		assert_all!(i, and(vec![i(and(pqr()))]), "p and q and r");
		assert_all!(i, and(vec![i(and(vec![p()])), i(and(vec![q()])), i(and(vec![r()]))]),
			"p and q and r");
		assert_all!(i, and(vec![i(and(p1q1r1())), i(and(p2q2r2())), i(and(p3q3r3()))]),
			"p1 and q1 and r1 and p2 and q2 and r2 and p3 and q3 and r3");

		// Or + and
		assert_all!(i, or(vec![i(and(vec![p()]))]), "p");
		assert_all!(i, or(vec![i(and(pqr()))]), "p and q and r");
		assert_all!(i, or(vec![i(and(vec![p()])), i(and(vec![q()])), i(and(vec![r()]))]),
			"p or q or r");
		assert_all!(i, or(vec![i(and(p1q1r1())), i(and(p2q2r2())), i(and(p3q3r3()))]),
			"p1 and q1 and r1 or p2 and q2 and r2 or p3 and q3 and r3");

		// Implies + and
		assert_all!(i, implies_right(i(and(vec![p()])), i(and(vec![q()]))), "p -> q");
		assert_all!(i, implies_left(i(and(vec![p()])), i(and(vec![q()]))), "q <- p");
		assert_all!(i, implies_right(i(and(p1q1r1())), i(and(p2q2r2()))),
			"p1 and q1 and r1 -> p2 and q2 and r2");
		assert_all!(i, implies_left(i(and(p1q1r1())), i(and(p2q2r2()))),
			"p2 and q2 and r2 <- p1 and q1 and r1");

		// If and only if + and
		assert_all!(i, if_and_only_if(vec![i(and(vec![p()]))]), "p");
		assert_all!(i, if_and_only_if(vec![i(and(pqr()))]), "p and q and r");
		assert_all!(i, if_and_only_if(vec![i(and(vec![p()])), i(and(vec![q()])),
			i(and(vec![r()]))]),
			"p <-> q <-> r");
		assert_all!(i, if_and_only_if(vec![i(and(p1q1r1())), i(and(p2q2r2())), i(and(p3q3r3()))]),
			"p1 and q1 and r1 <-> p2 and q2 and r2 <-> p3 and q3 and r3");
	}

	#[test]
	fn format_combinations_or()
	{
		// Not + or
		assert_all!(i, not(i(or(vec![p()]))), "not p");
		assert_all!(i, not(i(or(pqr()))), "not (p or q or r)");

		// Quantified formula + or
		assert_all!(i, exists(vec![x()], i(or(vec![p()]))), "exists X p");
		assert_all!(i, for_all(vec![x()], i(or(vec![p()]))), "forall X p");
		assert_all!(i, exists(vec![x()], i(or(pqr()))), "exists X (p or q or r)");
		assert_all!(i, for_all(vec![x()], i(or(pqr()))), "forall X (p or q or r)");
		assert_all!(i, exists(xyz(), i(or(vec![p()]))), "exists X, Y, Z p");
		assert_all!(i, for_all(xyz(), i(or(vec![p()]))), "forall X, Y, Z p");
		assert_all!(i, exists(xyz(), i(or(pqr()))), "exists X, Y, Z (p or q or r)");
		assert_all!(i, for_all(xyz(), i(or(pqr()))), "forall X, Y, Z (p or q or r)");

		// And + or
		assert_all!(i, and(vec![i(or(vec![p()]))]), "p");
		assert_all!(i, and(vec![i(or(pqr()))]), "p or q or r");
		assert_all!(i, and(vec![i(or(vec![p()])), i(or(vec![q()])), i(or(vec![r()]))]),
			"p and q and r");
		assert_all!(i, and(vec![i(or(p1q1r1())), i(or(p2q2r2())), i(or(p3q3r3()))]),
			"(p1 or q1 or r1) and (p2 or q2 or r2) and (p3 or q3 or r3)");

		// Or + or
		assert_all!(i, or(vec![i(or(vec![p()]))]), "p");
		assert_all!(i, or(vec![i(or(pqr()))]), "p or q or r");
		assert_all!(i, or(vec![i(or(vec![p()])), i(or(vec![q()])), i(or(vec![r()]))]),
			"p or q or r");
		assert_all!(i, or(vec![i(or(p1q1r1())), i(or(p2q2r2())), i(or(p3q3r3()))]),
			"p1 or q1 or r1 or p2 or q2 or r2 or p3 or q3 or r3");

		// Implies + or
		assert_all!(i, implies_right(i(or(vec![p()])), i(or(vec![q()]))), "p -> q");
		assert_all!(i, implies_left(i(or(vec![p()])), i(or(vec![q()]))), "q <- p");
		assert_all!(i, implies_right(i(or(p1q1r1())), i(or(p2q2r2()))),
			"p1 or q1 or r1 -> p2 or q2 or r2");
		assert_all!(i, implies_left(i(or(p1q1r1())), i(or(p2q2r2()))),
			"p2 or q2 or r2 <- p1 or q1 or r1");

		// If and only if + or
		assert_all!(i, if_and_only_if(vec![i(or(vec![p()]))]), "p");
		assert_all!(i, if_and_only_if(vec![i(or(pqr()))]), "p or q or r");
		assert_all!(i, if_and_only_if(vec![i(or(vec![p()])), i(or(vec![q()])), i(or(vec![r()]))]),
			"p <-> q <-> r");
		assert_all!(i, if_and_only_if(vec![i(or(p1q1r1())), i(or(p2q2r2())), i(or(p3q3r3()))]),
			"p1 or q1 or r1 <-> p2 or q2 or r2 <-> p3 or q3 or r3");
	}

	#[test]
	fn format_combinations_implies()
	{
		// Not + implies
		assert_all!(i, not(i(implies_right(p(), q()))), "not (p -> q)");
		assert_all!(i, not(i(implies_left(p(), q()))), "not (q <- p)");

		// Quantified formula + implies
		assert_all!(i, exists(vec![x()], i(implies_right(p(), q()))), "exists X (p -> q)");
		assert_all!(i, exists(vec![x()], i(implies_left(p(), q()))), "exists X (q <- p)");
		assert_all!(i, for_all(vec![x()], i(implies_right(p(), q()))), "forall X (p -> q)");
		assert_all!(i, for_all(vec![x()], i(implies_left(p(), q()))), "forall X (q <- p)");
		assert_all!(i, exists(xyz(), i(implies_right(p(), q()))), "exists X, Y, Z (p -> q)");
		assert_all!(i, exists(xyz(), i(implies_left(p(), q()))), "exists X, Y, Z (q <- p)");
		assert_all!(i, for_all(xyz(), i(implies_right(p(), q()))), "forall X, Y, Z (p -> q)");
		assert_all!(i, for_all(xyz(), i(implies_left(p(), q()))), "forall X, Y, Z (q <- p)");

		// And + implies
		assert_all!(i, and(vec![i(implies_right(p(), q()))]), "p -> q");
		assert_all!(i, and(vec![i(implies_left(p(), q()))]), "q <- p");
		assert_all!(i, and(vec![i(implies_right(p1(), q1())), i(implies_right(p2(), q2())),
			i(implies_right(p3(), q3()))]),
			"(p1 -> q1) and (p2 -> q2) and (p3 -> q3)");
		assert_all!(i, and(vec![i(implies_left(p1(), q1())), i(implies_left(p2(), q2())),
			i(implies_left(p3(), q3()))]),
			"(q1 <- p1) and (q2 <- p2) and (q3 <- p3)");

		// Or + implies
		assert_all!(i, or(vec![i(implies_right(p(), q()))]), "p -> q");
		assert_all!(i, or(vec![i(implies_left(p(), q()))]), "q <- p");
		assert_all!(i, or(vec![i(implies_right(p1(), q1())), i(implies_right(p2(), q2())),
			i(implies_right(p3(), q3()))]),
			"(p1 -> q1) or (p2 -> q2) or (p3 -> q3)");
		assert_all!(i, or(vec![i(implies_left(p1(), q1())), i(implies_left(p2(), q2())),
			i(implies_left(p3(), q3()))]),
			"(q1 <- p1) or (q2 <- p2) or (q3 <- p3)");

		// Implies + implies
		assert_all!(i, implies_right(i(implies_right(p1(), q1())), i(implies_right(p2(), q2()))),
			"(p1 -> q1) -> p2 -> q2");
		assert_all!(i, implies_right(i(implies_left(p1(), q1())), i(implies_left(p2(), q2()))),
			"(q1 <- p1) -> (q2 <- p2)");
		assert_all!(i, implies_left(i(implies_right(p1(), q1())), i(implies_right(p2(), q2()))),
			"(p2 -> q2) <- (p1 -> q1)");
		assert_all!(i, implies_left(i(implies_left(p1(), q1())), i(implies_left(p2(), q2()))),
			"q2 <- p2 <- (q1 <- p1)");

		// If and only if + implies
		assert_all!(i, if_and_only_if(vec![i(implies_right(p(), q()))]), "p -> q");
		assert_all!(i, if_and_only_if(vec![i(implies_left(p(), q()))]), "q <- p");
		assert_all!(i, if_and_only_if(vec![i(implies_right(p1(), q1())),
			i(implies_right(p2(), q2())), i(implies_right(p3(), q3()))]),
			"p1 -> q1 <-> p2 -> q2 <-> p3 -> q3");
		assert_all!(i, if_and_only_if(vec![i(implies_left(p1(), q1())), i(implies_left(p2(), q2())),
			i(implies_left(p3(), q3()))]),
			"q1 <- p1 <-> q2 <- p2 <-> q3 <- p3");
	}

	#[test]
	fn format_combinations_if_and_only_if()
	{
		// Not + if and only if
		assert_all!(i, not(i(if_and_only_if(vec![p()]))), "not p");
		assert_all!(i, not(i(if_and_only_if(pqr()))), "not (p <-> q <-> r)");

		// Quantified formula + if and only if
		assert_all!(i, exists(vec![x()], i(if_and_only_if(vec![p()]))), "exists X p");
		assert_all!(i, for_all(vec![x()], i(if_and_only_if(vec![p()]))), "forall X p");
		assert_all!(i, exists(vec![x()], i(if_and_only_if(pqr()))), "exists X (p <-> q <-> r)");
		assert_all!(i, for_all(vec![x()], i(if_and_only_if(pqr()))), "forall X (p <-> q <-> r)");
		assert_all!(i, exists(xyz(), i(if_and_only_if(vec![p()]))), "exists X, Y, Z p");
		assert_all!(i, for_all(xyz(), i(if_and_only_if(vec![p()]))), "forall X, Y, Z p");
		assert_all!(i, exists(xyz(), i(if_and_only_if(pqr()))), "exists X, Y, Z (p <-> q <-> r)");
		assert_all!(i, for_all(xyz(), i(if_and_only_if(pqr()))), "forall X, Y, Z (p <-> q <-> r)");

		// And + if and only if
		assert_all!(i, and(vec![i(if_and_only_if(vec![p()]))]), "p");
		assert_all!(i, and(vec![i(if_and_only_if(pqr()))]), "p <-> q <-> r");
		assert_all!(i, and(vec![i(if_and_only_if(vec![p()])), i(if_and_only_if(vec![q()])),
			i(if_and_only_if(vec![r()]))]),
			"p and q and r");
		assert_all!(i, and(vec![i(if_and_only_if(p1q1r1())), i(if_and_only_if(p2q2r2())),
			i(if_and_only_if(p3q3r3()))]),
			"(p1 <-> q1 <-> r1) and (p2 <-> q2 <-> r2) and (p3 <-> q3 <-> r3)");

		// Or + if and only if
		assert_all!(i, or(vec![i(if_and_only_if(vec![p()]))]), "p");
		assert_all!(i, or(vec![i(if_and_only_if(pqr()))]), "p <-> q <-> r");
		assert_all!(i, or(vec![i(if_and_only_if(vec![p()])), i(if_and_only_if(vec![q()])),
			i(if_and_only_if(vec![r()]))]),
			"p or q or r");
		assert_all!(i, or(vec![i(if_and_only_if(p1q1r1())), i(if_and_only_if(p2q2r2())),
			i(if_and_only_if(p3q3r3()))]),
			"(p1 <-> q1 <-> r1) or (p2 <-> q2 <-> r2) or (p3 <-> q3 <-> r3)");

		// Implies + if and only if
		assert_all!(i, implies_right(i(if_and_only_if(vec![p()])), i(if_and_only_if(vec![q()]))),
			"p -> q");
		assert_all!(i, implies_left(i(if_and_only_if(vec![p()])), i(if_and_only_if(vec![q()]))),
			"q <- p");
		assert_all!(i, implies_right(i(if_and_only_if(p1q1r1())), i(if_and_only_if(p2q2r2()))),
			"(p1 <-> q1 <-> r1) -> (p2 <-> q2 <-> r2)");
		assert_all!(i, implies_left(i(if_and_only_if(p1q1r1())), i(if_and_only_if(p2q2r2()))),
			"(p2 <-> q2 <-> r2) <- (p1 <-> q1 <-> r1)");

		// If and only if + if and only if
		assert_all!(i, if_and_only_if(vec![i(if_and_only_if(vec![p()]))]), "p");
		assert_all!(i, if_and_only_if(vec![i(if_and_only_if(pqr()))]), "p <-> q <-> r");
		assert_all!(i, if_and_only_if(vec![i(if_and_only_if(vec![p()])),
			i(if_and_only_if(vec![q()])), i(if_and_only_if(vec![r()]))]),
			"p <-> q <-> r");
		assert_all!(i, if_and_only_if(vec![i(if_and_only_if(p1q1r1())), i(if_and_only_if(p2q2r2())),
			i(if_and_only_if(p3q3r3()))]),
			"(p1 <-> q1 <-> r1) <-> (p2 <-> q2 <-> r2) <-> (p3 <-> q3 <-> r3)");
	}
}
