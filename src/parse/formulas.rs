use super::tokens::*;

pub fn parse_formula(input: &str) -> Result<crate::Formula, crate::parse::Error>
{
	let formula_str = FormulaStr::new(input);
	formula_str.parse(0)?;

	// TODO: implement correctly
	Ok(crate::Formula::true_())
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum FormulaInfixOperator
{
	And,
	IfAndOnlyIf,
	ImpliesLeftToRight,
	ImpliesRightToLeft,
	Or,
}

impl FormulaInfixOperator
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

impl std::fmt::Debug for FormulaInfixOperator
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

	pub fn top_level_infix_operator(&self)
		-> Result<Option<FormulaInfixOperator>, crate::parse::Error>
	{
		let mut top_level_infix_operator = None;

		for infix_operator in self.iter_infix_operators()
		{
			let (_, _, infix_operator) = infix_operator?;

			top_level_infix_operator = match top_level_infix_operator
			{
				None => Some(infix_operator),
				Some(top_level_infix_operator) =>
				{
					if (infix_operator == FormulaInfixOperator::ImpliesLeftToRight
							&& top_level_infix_operator == FormulaInfixOperator::ImpliesRightToLeft)
						|| (infix_operator == FormulaInfixOperator::ImpliesRightToLeft
							&& top_level_infix_operator == FormulaInfixOperator::ImpliesLeftToRight)
					{
						return Err(crate::parse::Error::new_mixed_implication_directions(
							crate::parse::error::Location::new(0, Some(0)),
							crate::parse::error::Location::new(0, Some(0))));
					}

					if infix_operator.level() > top_level_infix_operator.level()
					{
						Some(infix_operator)
					}
					else
					{
						Some(top_level_infix_operator)
					}
				},
			}
		}

		Ok(top_level_infix_operator)
	}

	pub fn iter_infix_operators(&self) -> FormulaInfixOperatorIterator<'i>
	{
		FormulaInfixOperatorIterator::new(self.input)
	}

	pub fn split_at_infix_operator(&self, infix_operator: FormulaInfixOperator)
		-> SplitFormulaAtInfixOperator<'i>
	{
		SplitFormulaAtInfixOperator::new(self, infix_operator)
	}

	pub fn parse(&self, level: usize) -> Result<(), crate::parse::Error>
	{
		let indentation = "  ".repeat(level);
		println!("{}- parsing: {}", indentation, self.input);

		let input = self.input.trim_start();

		match self.top_level_infix_operator()?
		{
			None =>
			{
				if let Some((identifier, _)) = identifier(input)
				{
					match identifier
					{
						"exists" => println!("{}  parsing “exists” expression from: {}", indentation, input),
						"forall" => println!("{}  parsing “forall” expression from: {}", indentation, input),
						_ => (),
					}
				}

				println!("{}  can’t break down any further: {}", indentation, input)
			},
			Some(top_level_infix_operator) =>
			{
				println!("{}  parsing “{:?}” expression from: {}", indentation,
					top_level_infix_operator, input);

				for subformula in self.split_at_infix_operator(top_level_infix_operator)
				{
					FormulaStr::new(subformula?).parse(level + 1)?;
				}
			},
		}

		Ok(())
	}
}

struct FormulaInfixOperatorIterator<'i>
{
	original_input: &'i str,
	input: &'i str,
}

impl<'i> FormulaInfixOperatorIterator<'i>
{
	pub fn new(input: &'i str) -> Self
	{
		Self
		{
			original_input: input,
			input,
		}
	}
}

impl<'i> std::iter::Iterator for FormulaInfixOperatorIterator<'i>
{
	type Item = Result<(&'i str, &'i str, FormulaInfixOperator), crate::parse::Error>;

	fn next(&mut self) -> Option<Self::Item>
	{
		loop
		{
			self.input = self.input.trim_start();

			let first_character = match self.input.chars().next()
			{
				None => return None,
				Some(first_character) => first_character,
			};

			if self.input.starts_with(")")
			{
				return Some(Err(crate::parse::Error::new_unmatched_parenthesis(
					crate::parse::error::Location::new(0, Some(1)))));
			}

			match parenthesized_expression(self.input)
			{
				Ok(Some((_, remaining_input))) =>
				{
					self.input = remaining_input;
					continue;
				},
				Ok(None) => (),
				Err(error) => return Some(Err(error)),
			}

			match number(self.input)
			{
				Ok(Some((_, remaining_input))) =>
				{
					self.input = remaining_input;
					continue;
				}
				Ok(None) => (),
				Err(error) => return Some(Err(error)),
			}

			let index_left = self.input.as_ptr() as usize - self.original_input.as_ptr() as usize;
			let input_left = self.original_input.split_at(index_left).0.trim_end();

			if let Some((identifier, remaining_input)) = identifier(self.input)
			{
				self.input = remaining_input;

				match identifier
				{
					"and" =>
						return Some(Ok((input_left, remaining_input, FormulaInfixOperator::And))),
					"or" =>
						return Some(Ok((input_left, remaining_input, FormulaInfixOperator::Or))),
					_ => continue,
				}
			}

			if let Some((symbol, remaining_input)) = symbol(self.input)
			{
				self.input = remaining_input;

				match symbol
				{
					Symbol::ArrowLeft => return Some(Ok((input_left, remaining_input,
						FormulaInfixOperator::ImpliesRightToLeft))),
					Symbol::ArrowLeftAndRight => return Some(Ok((input_left, remaining_input,
						FormulaInfixOperator::IfAndOnlyIf))),
					Symbol::ArrowRight => return Some(Ok((input_left, remaining_input,
						FormulaInfixOperator::ImpliesLeftToRight))),
					_ => continue,
				}
			}

			return Some(Err(crate::parse::Error::new_character_not_allowed(first_character,
				crate::parse::error::Location::new(0, Some(0)))));
		}
	}
}

struct SplitFormulaAtInfixOperator<'i>
{
	infix_operator_iterator: FormulaInfixOperatorIterator<'i>,
	infix_operator: FormulaInfixOperator,
	previous_index: usize,
}

impl<'i> SplitFormulaAtInfixOperator<'i>
{
	pub fn new(input: &FormulaStr<'i>, infix_operator: FormulaInfixOperator)
		-> Self
	{
		Self
		{
			infix_operator_iterator: input.iter_infix_operators(),
			infix_operator,
			previous_index: 0,
		}
	}
}

impl<'i> std::iter::Iterator for SplitFormulaAtInfixOperator<'i>
{
	type Item = Result<&'i str, crate::parse::Error>;

	fn next(&mut self) -> Option<Self::Item>
	{
		loop
		{
			let (input_left, input_right, infix_operator) =
				match self.infix_operator_iterator.next()
			{
				Some(Err(error)) => return Some(Err(error)),
				Some(Ok(infix_operator_iterator_next)) => infix_operator_iterator_next,
				None => break,
			};

			if infix_operator == self.infix_operator
			{
				// TODO: refactor
				let index = input_left.as_ptr() as usize
					+ input_left.len()
					- self.infix_operator_iterator.original_input.as_ptr() as usize;
				let split_input = &self.infix_operator_iterator
					.original_input[self.previous_index..index].trim();
				self.previous_index = input_right.as_ptr() as usize
					- self.infix_operator_iterator.original_input.as_ptr() as usize;

				return Some(Ok(split_input));
			}
		}

		let remaining_input = self.infix_operator_iterator
			.original_input[self.previous_index..].trim();

		if remaining_input.is_empty()
		{
			None
		}
		else
		{
			self.previous_index = self.infix_operator_iterator.original_input.len();

			Some(Ok(remaining_input))
		}
	}
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
