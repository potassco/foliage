use super::terms::*;
use super::tokens::*;

pub fn formula<D, F>(input: &str, declarations: &D)
	-> Result<crate::OpenFormula<F>, crate::parse::Error>
where
	F: crate::flavor::Flavor,
	D: crate::FindOrCreateFunctionDeclaration<F> + crate::FindOrCreatePredicateDeclaration<F>,
{
	let variable_declaration_stack = crate::VariableDeclarationStackLayer::free();

	let formula_str = FormulaStr::new(input, declarations, &variable_declaration_stack);
	let formula = formula_str.parse(0)?;

	let free_variable_declarations = match variable_declaration_stack
	{
		crate::VariableDeclarationStackLayer::Free(free_variable_declarations) =>
			std::rc::Rc::new(free_variable_declarations.into_inner()),
		_ => unreachable!(),
	};

	Ok(crate::OpenFormula
	{
		formula,
		free_variable_declarations,
	})
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

struct FormulaStr<'i, 'd, 'p, 'v, F, D>
where
	F: crate::flavor::Flavor,
{
	input: &'i str,
	declarations: &'d D,
	variable_declaration_stack: &'v crate::VariableDeclarationStackLayer<'p, F>,
}

impl<'i, 'd, 'p, 'v, F, D> FormulaStr<'i, 'd, 'p, 'v, F, D>
where
	F: crate::flavor::Flavor,
	D: crate::FindOrCreateFunctionDeclaration<F> + crate::FindOrCreatePredicateDeclaration<F>,
{
	pub fn new(input: &'i str, declarations: &'d D,
		variable_declaration_stack: &'v crate::VariableDeclarationStackLayer<'p, F>)
		-> Self
	{
		Self
		{
			input,
			declarations,
			variable_declaration_stack,
		}
	}

	fn logical_connectives(&self) -> Tokens<'i, impl FnMut(Token<'i>) -> Option<LogicalConnective>>
	{
		let functor = |token| match token
		{
			Token::Identifier("and") => Some(LogicalConnective::And),
			Token::Identifier("or") => Some(LogicalConnective::Or),
			Token::Symbol(Symbol::ArrowLeft) => Some(LogicalConnective::ImpliesRightToLeft),
			Token::Symbol(Symbol::ArrowLeftAndRight) => Some(LogicalConnective::IfAndOnlyIf),
			Token::Symbol(Symbol::ArrowRight) => Some(LogicalConnective::ImpliesLeftToRight),
			_ => None,
		};

		Tokens::new_filter_map(self.input, functor)
	}

	fn split_at_logical_connective(&self, logical_connective: LogicalConnective)
		-> TokenSplit<Tokens<'i, impl FnMut(Token<'i>) -> Option<Token<'i>>>>
	{
		let predicate = move |token: &_| match token
		{
			Token::Identifier("and") => logical_connective == LogicalConnective::And,
			Token::Identifier("or") => logical_connective == LogicalConnective::Or,
			Token::Symbol(Symbol::ArrowLeft) =>
				logical_connective == LogicalConnective::ImpliesRightToLeft,
			Token::Symbol(Symbol::ArrowLeftAndRight) =>
				logical_connective == LogicalConnective::IfAndOnlyIf,
			Token::Symbol(Symbol::ArrowRight) =>
				logical_connective == LogicalConnective::ImpliesLeftToRight,
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
					let implication_directions_are_mixed =
						logical_connective == LogicalConnective::ImpliesLeftToRight
							&& top_level_logical_connective == LogicalConnective::ImpliesRightToLeft
						|| logical_connective == LogicalConnective::ImpliesRightToLeft
							&& top_level_logical_connective == LogicalConnective::ImpliesLeftToRight;

					if implication_directions_are_mixed
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

	pub fn parse(&self, level: usize) -> Result<crate::Formula<F>, crate::parse::Error>
	{
		let indentation = "  ".repeat(level);
		let input = trim_start(self.input);

		log::trace!("{}- parsing formula: {}", indentation, input);

		match input.chars().next()
		{
			Some(')') => return Err(crate::parse::Error::new_unmatched_parenthesis(
				crate::parse::error::Location::new(0, Some(0)))),
			None => return Err(crate::parse::Error::new_empty_expression(
				crate::parse::error::Location::new(0, Some(0)))),
			_ => (),
		}

		// Parse logical infix connectives
		if let Some(top_level_logical_connective) = self.top_level_logical_connective()?
		{
			log::trace!("{}  parsing “{:?}” logical connective", indentation,
				top_level_logical_connective);

			// Parse arguments of n-ary logical infix connectives
			let arguments_n_ary = ||
			{
				// TODO: improve error handling if the formulas between the operators are invalid
				self.split_at_logical_connective(top_level_logical_connective)
					.map(|argument| FormulaStr::new(argument?, self.declarations, self.variable_declaration_stack).parse(level + 1))
					.collect::<Result<Vec<_>, _>>()
			};

			match top_level_logical_connective
			{
				LogicalConnective::And => return Ok(crate::Formula::and(arguments_n_ary()?)),
				LogicalConnective::Or => return Ok(crate::Formula::or(arguments_n_ary()?)),
				LogicalConnective::IfAndOnlyIf =>
					return Ok(crate::Formula::if_and_only_if(arguments_n_ary()?)),
				LogicalConnective::ImpliesLeftToRight =>
					return self.implication_left_to_right(
						self.split_at_logical_connective(top_level_logical_connective), level + 1),
				LogicalConnective::ImpliesRightToLeft =>
				{
					let mut argument_iterator =
						self.split_at_logical_connective(top_level_logical_connective);
					let first_argument = argument_iterator.next().ok_or_else(||
						crate::parse::Error::new_expected_logical_connective_argument(
							"right-to-left implication".to_string(),
							crate::parse::error::Location::new(0, Some(0))))?;
					let first_argument = FormulaStr::new(first_argument?, self.declarations, self.variable_declaration_stack).parse(level + 1)?;

					return argument_iterator.try_fold(first_argument,
						|accumulator, argument|
						{
							let argument = FormulaStr::new(argument?, self.declarations, self.variable_declaration_stack).parse(level + 1)?;

							Ok(crate::Formula::implies(crate::ImplicationDirection::RightToLeft,
								Box::new(argument), Box::new(accumulator)))
						});
				},
			}
		}

		// Parse quantified formulas
		if let Some((identifier, input)) = identifier(input)
		{
			match identifier
			{
				"not" =>
				{
					let input = trim_start(input);
					log::trace!("{}  parsing “not” formula body: {}", indentation, input);

					let argument = FormulaStr::new(input, self.declarations, self.variable_declaration_stack).parse(level + 1)?;

					return Ok(crate::Formula::not(Box::new(argument)));
				},
				"true" =>
				{
					if !input.trim().is_empty()
					{
						return Err(crate::parse::Error::new_unexpected_token(
							crate::parse::error::Location::new(0, Some(0))))
					}

					return Ok(crate::Formula::true_());
				},
				"false" =>
				{
					if !input.trim().is_empty()
					{
						return Err(crate::parse::Error::new_unexpected_token(
							crate::parse::error::Location::new(0, Some(0))))
					}

					return Ok(crate::Formula::false_());
				},
				_ => (),
			}

			let quantifier = match identifier
			{
				"exists" => Some(Quantifier::Existential),
				"forall" => Some(Quantifier::Universal),
				_ => None,
			};

			if let Some(quantifier) = quantifier
			{
				let input = trim_start(input);
				log::trace!("{}  parsing “{:?}” formula body: {}", indentation, quantifier, input);

				return self.quantified_formula(input, quantifier, level + 1);
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

			log::trace!("{}  parsing “{:?}” comparison: {}", indentation, comparison_operator, input);

			let mut comparison_operator_split = self.comparison_operators().split();

			// There’s exactly one comparison operator in this formula, as we have verified above.
			// Hence, the split is guaranteed to generate exactly these two elements
			let input_left = comparison_operator_split.next().unwrap()?;
			let input_right = comparison_operator_split.next().unwrap()?;
			assert!(comparison_operator_split.next().is_none());

			let argument_left =
				TermStr::new(input_left, self.declarations, self.variable_declaration_stack)
					.parse(level + 1)?;
			let argument_right =
				TermStr::new(input_right, self.declarations, self.variable_declaration_stack)
					.parse(level + 1)?;

			return Ok(crate::Formula::compare(comparison_operator, Box::new(argument_left),
				Box::new(argument_right)));
		}

		// Parse predicates
		if let Some((predicate_name, input)) = predicate_name(input)
		{
			log::trace!("{}  parsing predicate {}", indentation, predicate_name);

			let input = trim_start(input);

			// Parse arguments if there are any
			let (arguments, input) = match parenthesized_expression(input)?
			{
				Some((parenthesized_expression, input)) =>
				{
					let functor = |token: &_| *token == Token::Symbol(Symbol::Comma);
					let arguments = Tokens::new_filter(parenthesized_expression, functor).split()
						.map(|argument| TermStr::new(argument?, self.declarations,
							self.variable_declaration_stack)
								.parse(level + 1))
						.collect::<Result<_, _>>()?;

					(arguments, input)
				}
				None => (vec![], input),
			};

			if !input.trim().is_empty()
			{
				return Err(crate::parse::Error::new_unexpected_token(
					crate::parse::error::Location::new(0, Some(0))))
			}

			let declaration = self.declarations.find_or_create_predicate_declaration(predicate_name,
				arguments.len());
			return Ok(crate::Formula::predicate(declaration, arguments));
		}

		// Parse parenthesized formulas
		if let Some((parenthesized_expression, input)) = parenthesized_expression(input)?
		{
			if !input.trim().is_empty()
			{
				return Err(crate::parse::Error::new_unexpected_token(
					crate::parse::error::Location::new(0, Some(0))));
			}

			return FormulaStr::new(parenthesized_expression, self.declarations, self.variable_declaration_stack).parse(level + 1);
		}

		Err(crate::parse::Error::new_unexpected_token(
			crate::parse::error::Location::new(0, Some(0))))
	}

	// TODO: refactor
	fn implication_left_to_right_inner<T>(&self, mut argument_iterator: T, level: usize)
		-> Result<Option<crate::Formula<F>>, crate::parse::Error>
	where
		T: std::iter::Iterator<Item = Result<&'i str, crate::parse::Error>>
	{
		match argument_iterator.next()
		{
			Some(argument) =>
			{
				// TODO: improve error handling if antecedent cannot be parsed
				let argument = FormulaStr::new(argument?, self.declarations, self.variable_declaration_stack).parse(level)?;
				match self.implication_left_to_right_inner(argument_iterator, level)?
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

	fn implication_left_to_right<T>(&self, mut argument_iterator: T, level: usize)
		-> Result<crate::Formula<F>, crate::parse::Error>
	where
		T: std::iter::Iterator<Item = Result<&'i str, crate::parse::Error>>
	{
		match argument_iterator.next()
		{
			Some(argument) =>
			{
				// TODO: improve error handling if antecedent cannot be parsed
				let argument = FormulaStr::new(argument?, self.declarations, self.variable_declaration_stack).parse(level)?;
				match self.implication_left_to_right_inner(argument_iterator, level)?
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

	// TODO: refactor without input argument
	fn quantified_formula(&self, input: &str, quantifier: Quantifier, level: usize)
		-> Result<crate::Formula<F>, crate::parse::Error>
	{
		let (parameters, input) = match variable_declarations::<F>(input)?
		{
			Some(variable_declarations) => variable_declarations,
			None => return Err(crate::parse::Error::new_expected_variable_declaration(
				crate::parse::error::Location::new(0, Some(0)))),
		};
		let parameters = std::rc::Rc::new(parameters);

		let variable_declaration_stack = crate::VariableDeclarationStackLayer::bound(
			self.variable_declaration_stack, std::rc::Rc::clone(&parameters));

		let formula_str =
			FormulaStr::new(input.trim(), self.declarations, &variable_declaration_stack);
		let formula = Box::new(formula_str.parse(level)?);

		let formula = match quantifier
		{
			Quantifier::Existential => crate::Formula::exists(parameters, formula),
			Quantifier::Universal => crate::Formula::for_all(parameters, formula),
		};

		Ok(formula)
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

#[cfg(test)]
mod tests
{
	use super::*;

	#[test]
	fn tokenize_formula_logical_connectives()
	{
		let declarations = crate::Declarations::<crate::flavor::DefaultFlavor>::new();
		let variable_declaration_stack = crate::VariableDeclarationStackLayer::free();

		let formula_str = |input| FormulaStr::new(input, &declarations,
			&variable_declaration_stack);

		let f = formula_str("((forall X exists Y (p(X) -> q(Y)) and false) or p) -> false");
		assert_eq!(f.top_level_logical_connective().unwrap(),
			Some(LogicalConnective::ImpliesLeftToRight));
		let mut i = f.logical_connectives();
		assert_eq!(i.next().unwrap().unwrap().1, LogicalConnective::ImpliesLeftToRight);
		assert!(i.next().is_none());

		let f = formula_str("forall X exists Y (p(X) -> q(Y)) and false or p -> false");
		assert_eq!(f.top_level_logical_connective().unwrap(),
			Some(LogicalConnective::ImpliesLeftToRight));
		let mut i = f.logical_connectives();
		assert_eq!(i.next().unwrap().unwrap().1, LogicalConnective::And);
		assert_eq!(i.next().unwrap().unwrap().1, LogicalConnective::Or);
		assert_eq!(i.next().unwrap().unwrap().1, LogicalConnective::ImpliesLeftToRight);
		assert!(i.next().is_none());

		let f = formula_str("  p -> forall X exists Y (p(X) -> q(Y)) and false or p -> false  ");
		assert_eq!(f.top_level_logical_connective().unwrap(),
			Some(LogicalConnective::ImpliesLeftToRight));
		let mut i = f.split_at_logical_connective(LogicalConnective::ImpliesLeftToRight);
		assert_eq!(i.next().unwrap().unwrap(), "p");
		assert_eq!(i.next().unwrap().unwrap(), "forall X exists Y (p(X) -> q(Y)) and false or p");
		assert_eq!(i.next().unwrap().unwrap(), "false");
		assert!(i.next().is_none());

		let f = formula_str("  p -> forall X exists Y (p(X) -> q(Y)) and false or p -> false  ");
		assert_eq!(f.top_level_logical_connective().unwrap(),
			Some(LogicalConnective::ImpliesLeftToRight));
		let mut i = f.split_at_logical_connective(LogicalConnective::And);
		assert_eq!(i.next().unwrap().unwrap(), "p -> forall X exists Y (p(X) -> q(Y))");
		assert_eq!(i.next().unwrap().unwrap(), "false or p -> false");
		assert!(i.next().is_none());

		let f = formula_str("  p and forall X exists Y (p(X) -> q(Y)) and false or p or false  ");
		assert_eq!(f.top_level_logical_connective().unwrap(), Some(LogicalConnective::Or));
		let mut i = f.split_at_logical_connective(LogicalConnective::Or);
		assert_eq!(i.next().unwrap().unwrap(), "p and forall X exists Y (p(X) -> q(Y)) and false");
		assert_eq!(i.next().unwrap().unwrap(), "p");
		assert_eq!(i.next().unwrap().unwrap(), "false");
		assert!(i.next().is_none());

		let f = formula_str(" (p and q) ");
		assert!(f.top_level_logical_connective().unwrap().is_none());
		let mut i = f.split_at_logical_connective(LogicalConnective::And);
		assert_eq!(i.next().unwrap().unwrap(), "(p and q)");
		assert!(i.next().is_none());

		assert!(formula_str(" a -> b -> c ").parse(0).is_ok());
		assert!(formula_str(" a -> b <- c ").parse(0).is_err());

		assert!(formula_str("  p -> forall X exists Y (p(X) -> q(Y)) and false or p -> false  ")
			.parse(0).is_ok());
	}
}
