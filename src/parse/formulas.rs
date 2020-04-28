use super::terms::*;
use super::tokens::*;

pub fn formula(input: &str) -> Result<crate::Formula, crate::parse::Error>
{
	let formula_str = FormulaStr::new(input);
	formula_str.parse(0)?;

	// TODO: implement correctly
	Ok(crate::Formula::true_())
}

pub(crate) fn predicate_name(identifier: &str) -> Option<(&str, &str)>
{
	function_name(identifier)
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum LogicalConnective
{
	And,
	IfAndOnlyIf,
	ImpliesLeftToRight,
	ImpliesRightToLeft,
	Or,
}

// TODO: rename to logic infix connective
impl LogicalConnective
{
	fn level(&self) -> usize
	{
		match self
		{
			Self::And => 1,
			Self::Or => 2,
			Self::ImpliesLeftToRight
			| Self::ImpliesRightToLeft => 3,
			Self::IfAndOnlyIf => 4,
		}
	}
}

impl std::fmt::Debug for LogicalConnective
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match &self
		{
			Self::And => write!(formatter, "and"),
			Self::IfAndOnlyIf => write!(formatter, "<->"),
			Self::ImpliesLeftToRight => write!(formatter, "->"),
			Self::ImpliesRightToLeft => write!(formatter, "<-"),
			Self::Or => write!(formatter, "or"),
		}
	}
}

struct FormulaStr<'i>
{
	input: &'i str,
}

impl<'i> FormulaStr<'i>
{
	pub fn new(input: &'i str) -> Self
	{
		Self
		{
			input,
		}
	}

	fn tokens(&self) -> Tokens<'i, impl FnMut(Token<'i>) -> Option<Token<'i>>>
	{
		Tokens::new_iter(self.input)
	}

	fn logical_connectives(&self) -> Tokens<'i, impl FnMut(Token<'i>) -> Option<LogicalConnective>>
	{
		let functor = |token| match token
		{
			Token::Identifier(ref identifier) => match *identifier
			{
				"and" => Some(LogicalConnective::And),
				"or" => Some(LogicalConnective::Or),
				_ => None,
			},
			Token::Symbol(ref symbol) => match symbol
			{
				Symbol::ArrowLeft => Some(LogicalConnective::ImpliesRightToLeft),
				Symbol::ArrowLeftAndRight => Some(LogicalConnective::IfAndOnlyIf),
				Symbol::ArrowRight => Some(LogicalConnective::ImpliesLeftToRight),
				_ => None,
			},
			_ => None,
		};

		Tokens::new_filter_map(self.input, functor)
	}

	fn split_at_logical_connective(&self, logical_connective: LogicalConnective)
		-> TokenSplit<Tokens<'i, impl FnMut(Token<'i>) -> Option<Token<'i>>>>
	{
		let predicate = move |token: &_| match token
		{
			Token::Identifier(ref identifier) => match *identifier
			{
				"and" => logical_connective == LogicalConnective::And,
				"or" => logical_connective == LogicalConnective::Or,
				_ => false,
			},
			Token::Symbol(ref symbol) => match symbol
			{
				Symbol::ArrowLeft => logical_connective == LogicalConnective::ImpliesRightToLeft,
				Symbol::ArrowLeftAndRight => logical_connective == LogicalConnective::IfAndOnlyIf,
				Symbol::ArrowRight => logical_connective == LogicalConnective::ImpliesLeftToRight,
				_ => false,
			},
			_ => false,
		};

		Tokens::new_filter(self.input, predicate).split()
	}

	pub fn top_level_logical_connective(&self)
		-> Result<Option<LogicalConnective>, crate::parse::Error>
	{
		let mut top_level_logical_connective = None;

		for logical_connective in self.logical_connectives()
		{
			let (_, logical_connective) = logical_connective?;

			top_level_logical_connective = match top_level_logical_connective
			{
				None => Some(logical_connective),
				Some(top_level_logical_connective) =>
				{
					if (logical_connective == LogicalConnective::ImpliesLeftToRight
							&& top_level_logical_connective == LogicalConnective::ImpliesRightToLeft)
						|| (logical_connective == LogicalConnective::ImpliesRightToLeft
							&& top_level_logical_connective == LogicalConnective::ImpliesLeftToRight)
					{
						return Err(crate::parse::Error::new_mixed_implication_directions(
							crate::parse::error::Location::new(0, Some(0)),
							crate::parse::error::Location::new(0, Some(0))));
					}

					if logical_connective.level() > top_level_logical_connective.level()
					{
						Some(logical_connective)
					}
					else
					{
						Some(top_level_logical_connective)
					}
				},
			}
		}

		Ok(top_level_logical_connective)
	}

	fn comparison_operators(&self) -> Tokens<'i, impl FnMut(Token<'i>)
		-> Option<crate::ComparisonOperator>>
	{
		let functor = |token| match token
		{
			Token::Symbol(symbol) => match symbol
			{
				Symbol::Greater => Some(crate::ComparisonOperator::Greater),
				Symbol::GreaterOrEqual => Some(crate::ComparisonOperator::GreaterOrEqual),
				Symbol::Less => Some(crate::ComparisonOperator::Less),
				Symbol::LessOrEqual => Some(crate::ComparisonOperator::LessOrEqual),
				Symbol::Equal => Some(crate::ComparisonOperator::Equal),
				Symbol::NotEqual => Some(crate::ComparisonOperator::NotEqual),
				_ => None,
			},
			_ => None,
		};

		Tokens::new_filter_map(self.input, functor)
	}

	pub fn parse(&self, level: usize) -> Result<crate::Formula, crate::parse::Error>
	{
		let indentation = "  ".repeat(level);
		let input = self.input.trim_start();

		println!("{}- parsing formula: {}", indentation, input);

		match input.chars().next()
		{
			Some(')') => return Err(crate::parse::Error::new_unmatched_parenthesis(
				crate::parse::error::Location::new(0, Some(0)))),
			None => return Err(crate::parse::Error::new_empty_input(
				crate::parse::error::Location::new(0, Some(0)))),
			_ => (),
		}

		// Parse logical infix connectives
		if let Some(top_level_logical_connective) = self.top_level_logical_connective()?
		{
			println!("{}  parsing “{:?}” infix formula", indentation, top_level_logical_connective);

			// Parse arguments of n-ary logical infix connectives
			let arguments_n_ary = ||
			{
				// TODO: improve error handling if the formulas between the operators are invalid
				self.split_at_logical_connective(top_level_logical_connective)
					.map(|subformula| FormulaStr::new(subformula?).parse(level + 1))
					.collect::<Result<Vec<_>, _>>()
			};

			match top_level_logical_connective
			{
				LogicalConnective::And => return Ok(crate::Formula::and(arguments_n_ary()?)),
				LogicalConnective::Or => return Ok(crate::Formula::or(arguments_n_ary()?)),
				LogicalConnective::IfAndOnlyIf =>
					return Ok(crate::Formula::if_and_only_if(arguments_n_ary()?)),
				LogicalConnective::ImpliesLeftToRight =>
					return implication_left_to_right(
						self.split_at_logical_connective(top_level_logical_connective), level + 1),
				/*LogicalConnective::ImpliesRightToLeft => unimplemented!(),*/
				_ =>
				{
					println!("{}  TODO: parse implication", indentation);

					// TODO: implement correctly
					return Ok(crate::Formula::true_());
				}
			}
		}

		// Parse quantified formulas
		if let Some((identifier, input)) = identifier(input)
		{
			let quantifier = match identifier
			{
				"exists" => Some(Quantifier::Existential),
				"forall" => Some(Quantifier::Universal),
				_ => None,
			};

			if let Some(quantifier) = quantifier
			{
				let input = input.trim_start();
				println!("{}  parsing “{:?}” formula body: {}", indentation, quantifier, input);

				return quantified_formula(input, quantifier, level + 1);
			}
		}

		let mut comparison_operators = self.comparison_operators();

		// Parse comparisons
		if let Some(comparison_operator) = comparison_operators.next()
		{
			let (_, comparison_operator) = comparison_operator?;

			// Comparisons with more than one comparison operator aren’t supported
			if let Some(next_comparison_operator) = comparison_operators.next()
			{
				let (_, next_comparison_operator) = next_comparison_operator?;

				return Err(crate::parse::Error::new_multiple_comparison_operators(
					comparison_operator, next_comparison_operator,
					crate::parse::error::Location::new(0, Some(0))));
			}

			println!("{}  parsing “{:?}” comparison: {}", indentation, comparison_operator, input);

			let mut comparison_operator_split = self.comparison_operators().split();

			// There’s exactly one comparison operator in this formula, as we have verified above.
			// Hence, the split is guaranteed to generate exactly these two elements
			let input_left = comparison_operator_split.next().unwrap()?;
			let input_right = comparison_operator_split.next().unwrap()?;

			let argument_left = TermStr::new(input_left).parse(level + 1)?;
			let argument_right = TermStr::new(input_right).parse(level + 1)?;

			return Ok(crate::Formula::compare(comparison_operator, Box::new(argument_left),
				Box::new(argument_right)));
		}

		// Parse predicates
		if let Some((predicate_name, input)) = predicate_name(input)
		{
			println!("{}  TODO: parse predicate {}", indentation, predicate_name);

			let input = input.trim_start();

			// Parse arguments if there are any
			/*let arguments = match parenthesized_expression(input)?
			{
				Some((parenthesized_expression, remaining_input)) =>
				{
					unimplemented!();
				}
				None => unimplemented!(),
			};*/

			// TODO: implement correctly
			return Ok(crate::Formula::true_());
		}

		// Parse parenthesized formulas
		if let Some('(') = input.chars().next()
		{
			match parenthesized_expression(input)?
			{
				Some((parenthesized_expression, remaining_input)) =>
				{
					if !remaining_input.trim().is_empty()
					{
						return Err(crate::parse::Error::new_unexpected_token(
							crate::parse::error::Location::new(0, Some(0))));
					}

					return FormulaStr::new(parenthesized_expression).parse(level);
				},
				None => unreachable!(),
			}
		};

		println!("{}  can’t break down formula any further: {}", indentation, input);

		// TODO: implement correctly
		Ok(crate::Formula::true_())
	}
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub(crate) enum Quantifier
{
	Existential,
	Universal,
}

impl std::fmt::Debug for Quantifier
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match &self
		{
			Self::Existential => write!(formatter, "exists"),
			Self::Universal => write!(formatter, "forall"),
		}
	}
}

// TODO: refactor
fn implication_left_to_right_inner<'i, T>(mut argument_iterator: T, level: usize)
	-> Result<Option<crate::Formula>, crate::parse::Error>
where
	T: std::iter::Iterator<Item = Result<&'i str, crate::parse::Error>>
{
	match argument_iterator.next()
	{
		Some(argument) =>
		{
			// TODO: improve error handling if antecedent cannot be parsed
			let argument = FormulaStr::new(argument?).parse(level)?;
			match implication_left_to_right_inner(argument_iterator, level)?
			{
				Some(next_argument) => Ok(Some(crate::Formula::implies(
					crate::ImplicationDirection::LeftToRight, Box::new(argument),
					Box::new(next_argument)))),
				None => Ok(Some(argument)),
			}
		},
		None => Ok(None),
	}
}

fn implication_left_to_right<'i, T>(mut argument_iterator: T, level: usize)
	-> Result<crate::Formula, crate::parse::Error>
where
	T: std::iter::Iterator<Item = Result<&'i str, crate::parse::Error>>
{
	match argument_iterator.next()
	{
		Some(argument) =>
		{
			// TODO: improve error handling if antecedent cannot be parsed
			let argument = FormulaStr::new(argument?).parse(level)?;
			match implication_left_to_right_inner(argument_iterator, level)?
			{
				Some(next_argument) => Ok(crate::Formula::implies(
					crate::ImplicationDirection::LeftToRight, Box::new(argument),
					Box::new(next_argument))),
				None => Err(crate::parse::Error::new_expected_logical_connective_argument(
					"left-to-right implication".to_string(),
					crate::parse::error::Location::new(0, Some(0)))),
			}
		},
		None => Err(crate::parse::Error::new_expected_logical_connective_argument(
			"left-to-right implication".to_string(),
			crate::parse::error::Location::new(0, Some(0)))),
	}
}

fn quantified_formula(input: &str, quantifier: Quantifier, level: usize)
	-> Result<crate::Formula, crate::parse::Error>
{
	let (parameters, input) = match variable_declarations(input)?
	{
		Some(variable_declarations) => variable_declarations,
		None => return Err(crate::parse::Error::new_expected_variable_declaration(
			crate::parse::error::Location::new(0, Some(0)))),
	};
	let parameters = std::rc::Rc::new(parameters);

	let formula_str = FormulaStr::new(input.trim());
	let formula = Box::new(formula_str.parse(level)?);

	// TODO: push variable stack layer
	let formula = match quantifier
	{
		Quantifier::Existential => crate::Formula::exists(parameters, formula),
		Quantifier::Universal => crate::Formula::for_all(parameters, formula),
	};

	Ok(formula)
}

#[cfg(test)]
mod tests
{
	use super::*;

	#[test]
	fn tokenize_formula_infix_operators()
	{
		let f = FormulaStr::new("((forall X exists Y (p(X) -> q(Y)) and false) or p) -> false");
		assert_eq!(f.top_level_infix_operator().unwrap(),
			Some(FormulaInfixOperator::ImpliesLeftToRight));
		let mut i = f.iter_infix_operators();
		assert_eq!(i.next().unwrap().unwrap().2, FormulaInfixOperator::ImpliesLeftToRight);
		assert!(i.next().is_none());

		let f = FormulaStr::new("forall X exists Y (p(X) -> q(Y)) and false or p -> false");
		assert_eq!(f.top_level_infix_operator().unwrap(),
			Some(FormulaInfixOperator::ImpliesLeftToRight));
		let mut i = f.iter_infix_operators();
		assert_eq!(i.next().unwrap().unwrap().2, FormulaInfixOperator::And);
		assert_eq!(i.next().unwrap().unwrap().2, FormulaInfixOperator::Or);
		assert_eq!(i.next().unwrap().unwrap().2, FormulaInfixOperator::ImpliesLeftToRight);
		assert!(i.next().is_none());

		let f = FormulaStr::new("  p -> forall X exists Y (p(X) -> q(Y)) and false or p -> false  ");
		assert_eq!(f.top_level_infix_operator().unwrap(),
			Some(FormulaInfixOperator::ImpliesLeftToRight));
		let mut i = f.split_at_infix_operator(FormulaInfixOperator::ImpliesLeftToRight);
		assert_eq!(i.next().unwrap().unwrap(), "p");
		assert_eq!(i.next().unwrap().unwrap(), "forall X exists Y (p(X) -> q(Y)) and false or p");
		assert_eq!(i.next().unwrap().unwrap(), "false");
		assert!(i.next().is_none());

		let f = FormulaStr::new("  p -> forall X exists Y (p(X) -> q(Y)) and false or p -> false  ");
		assert_eq!(f.top_level_infix_operator().unwrap(),
			Some(FormulaInfixOperator::ImpliesLeftToRight));
		let mut i = f.split_at_infix_operator(FormulaInfixOperator::And);
		assert_eq!(i.next().unwrap().unwrap(), "p -> forall X exists Y (p(X) -> q(Y))");
		assert_eq!(i.next().unwrap().unwrap(), "false or p -> false");
		assert!(i.next().is_none());

		let f = FormulaStr::new("  p and forall X exists Y (p(X) -> q(Y)) and false or p or false  ");
		assert_eq!(f.top_level_infix_operator().unwrap(), Some(FormulaInfixOperator::Or));
		let mut i = f.split_at_infix_operator(FormulaInfixOperator::Or);
		assert_eq!(i.next().unwrap().unwrap(), "p and forall X exists Y (p(X) -> q(Y)) and false");
		assert_eq!(i.next().unwrap().unwrap(), "p");
		assert_eq!(i.next().unwrap().unwrap(), "false");
		assert!(i.next().is_none());

		let f = FormulaStr::new(" (p and q) ");
		assert!(f.top_level_infix_operator().unwrap().is_none());
		let mut i = f.split_at_infix_operator(FormulaInfixOperator::And);
		assert_eq!(i.next().unwrap().unwrap(), "(p and q)");
		assert!(i.next().is_none());

		assert!(FormulaStr::new(" a -> b -> c ").parse(0).is_ok());
		assert!(FormulaStr::new(" a -> b <- c ").parse(0).is_err());

		assert!(!FormulaStr::new("  p -> forall X exists Y (p(X) -> q(Y)) and false or p -> false  ")
			.parse(0).is_ok());
	}
}
