use super::tokens::*;

pub fn parse_term(input: &str) -> Result<crate::Term, crate::parse::Error>
{
	let term_str = TermStr::new(input);
	term_str.parse(0)?;

	// TODO: implement correctly
	Ok(crate::Term::true_())
}

pub(crate) fn function_name(input: &str) -> Option<(&str, &str)>
{
	let (identifier, remaining_input) = identifier(input)?;

	if is_keyword(identifier)
	{
		return None;
	}

	let mut characters = identifier.chars();

	while let Some(character) = characters.next()
	{
		match character
		{
			'_' => continue,
			_ if character.is_ascii_lowercase() => return Some((identifier, remaining_input)),
			_ => return None,
		}
	}

	None
}

fn variable_name(input: &str) -> Option<(&str, &str)>
{
	let (identifier, remaining_input) = identifier(input)?;

	let mut characters = identifier.chars();

	while let Some(character) = characters.next()
	{
		match character
		{
			'_' => continue,
			_ if character.is_ascii_uppercase() => return Some((identifier, remaining_input)),
			_ => return None,
		}
	}

	None
}

pub(crate) fn variable_declaration(input: &str) -> Option<(crate::VariableDeclaration, &str)>
{
	variable_name(input)
		.map(|(variable_name, remaining_input)|
			(crate::VariableDeclaration::new(variable_name.to_string()), remaining_input))
}

pub(crate) fn variable_declarations(input: &str)
	-> Result<Option<(crate::VariableDeclarations, &str)>, crate::parse::Error>
{
	let mut variable_declarations = vec![];

	let (first_variable_declaration, mut input) = match variable_declaration(input)
	{
		Some(first_variable_declaration) => first_variable_declaration,
		None => return Ok(None),
	};

	variable_declarations.push(std::rc::Rc::new(first_variable_declaration));

	loop
	{
		input = input.trim_start();

		input = match symbol(input)
		{
			Some((Symbol::Comma, input)) => input,
			// TODO: detect redeclarations, such as in “exists X, Y, X”
			_ => return Ok(Some((variable_declarations, input))),
		};

		input = input.trim_start();

		let (variable_declaration, remaining_input) = match variable_declaration(input)
		{
			Some(variable_declaration) => variable_declaration,
			None => return Err(crate::parse::Error::new_expected_variable_declaration(
				crate::parse::error::Location::new(0, Some(0)))),
		};

		input = remaining_input;

		variable_declarations.push(std::rc::Rc::new(variable_declaration));
	}
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub(crate) enum TermInfixOperator
{
	Add,
	Divide,
	Exponentiate,
	Modulo,
	Multiply,
	Subtract,
}

impl TermInfixOperator
{
	fn level(&self) -> usize
	{
		match self
		{
			Self::Exponentiate => 1,
			Self::Multiply
			| Self::Divide
			| Self::Modulo => 2,
			Self::Add
			| Self::Subtract => 3,
		}
	}
}

impl std::fmt::Debug for TermInfixOperator
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match &self
		{
			Self::Add => write!(formatter, "+"),
			Self::Divide => write!(formatter, "/"),
			Self::Exponentiate => write!(formatter, "**"),
			// TODO: conflicts with single-line comments
			Self::Modulo => write!(formatter, "%"),
			Self::Multiply => write!(formatter, "*"),
			Self::Subtract => write!(formatter, "-"),
		}
	}
}

pub(crate) struct TermStr<'i>
{
	input: &'i str,
}

impl<'i> TermStr<'i>
{
	pub fn new(input: &'i str) -> Self
	{
		Self
		{
			input,
		}
	}

	pub fn iter_infix_operators(&self) -> TermInfixOperatorIterator<'i>
	{
		TermInfixOperatorIterator::new(self.input)
	}

	pub fn split_at_infix_operator(&self, infix_operator: TermInfixOperator)
		-> SplitTermAtInfixOperator<'i>
	{
		SplitTermAtInfixOperator::new(self, infix_operator)
	}

	pub fn parse(&self, level: usize) -> Result<crate::Term, crate::parse::Error>
	{
		let indentation = "  ".repeat(level);
		println!("{}- parsing term: {}", indentation, self.input);

		let input = self.input.trim_start();

		// TODO: implement
		Ok(crate::Term::true_())
	}
}

pub(crate) struct TermInfixOperatorIterator<'i>
{
	original_input: &'i str,
	input: &'i str,
}

impl<'i> TermInfixOperatorIterator<'i>
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

impl<'i> std::iter::Iterator for TermInfixOperatorIterator<'i>
{
	type Item = Result<(&'i str, &'i str, TermInfixOperator), crate::parse::Error>;

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

			// TODO: implement
			if self.input.starts_with("|")
			{
				unimplemented!();
			}

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

			if let Some((_, remaining_input)) = identifier(self.input)
			{
				self.input = remaining_input;
				continue;
			}

			if let Some((symbol, remaining_input)) = symbol(self.input)
			{
				self.input = remaining_input;

				match symbol
				{
					Symbol::Division => return Some(Ok((input_left, remaining_input,
						TermInfixOperator::Divide))),
					Symbol::Exponentiation => return Some(Ok((input_left, remaining_input,
						TermInfixOperator::Exponentiate))),
					Symbol::Minus => return Some(Ok((input_left, remaining_input,
						TermInfixOperator::Subtract))),
					Symbol::Multiplication => return Some(Ok((input_left, remaining_input,
						TermInfixOperator::Multiply))),
					Symbol::Percent => return Some(Ok((input_left, remaining_input,
						TermInfixOperator::Modulo))),
					Symbol::Plus => return Some(Ok((input_left, remaining_input,
						TermInfixOperator::Add))),
					_ => continue,
				}
			}

			return Some(Err(crate::parse::Error::new_character_not_allowed(first_character,
				crate::parse::error::Location::new(0, Some(0)))));
		}
	}
}

pub(crate) struct SplitTermAtInfixOperator<'i>
{
	infix_operator_iterator: TermInfixOperatorIterator<'i>,
	infix_operator: TermInfixOperator,
	previous_index: usize,
}

impl<'i> SplitTermAtInfixOperator<'i>
{
	pub fn new(input: &TermStr<'i>, infix_operator: TermInfixOperator)
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

impl<'i> std::iter::Iterator for SplitTermAtInfixOperator<'i>
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
	fn parse_variable_name()
	{
		assert_eq!(variable_name("X").unwrap(), ("X", ""));
		assert_eq!(variable_name("_X").unwrap(), ("_X", ""));
		assert_eq!(variable_name("__X").unwrap(), ("__X", ""));
		assert_eq!(variable_name("Variable").unwrap(), ("Variable", ""));
		assert_eq!(variable_name("_Variable").unwrap(), ("_Variable", ""));
		assert_eq!(variable_name("__Variable").unwrap(), ("__Variable", ""));
		assert_eq!(variable_name("X,").unwrap(), ("X", ","));
		assert_eq!(variable_name("_X,").unwrap(), ("_X", ","));
		assert_eq!(variable_name("__X,").unwrap(), ("__X", ","));
		assert_eq!(variable_name("Variable,").unwrap(), ("Variable", ","));
		assert_eq!(variable_name("_Variable,").unwrap(), ("_Variable", ","));
		assert_eq!(variable_name("__Variable,").unwrap(), ("__Variable", ","));
	}

	#[test]
	fn parse_variable_declaration()
	{
		let v = variable_declaration("X").unwrap();
		assert_eq!((v.0.name.as_str(), v.1), ("X", ""));
		let v = variable_declaration("_X").unwrap();
		assert_eq!((v.0.name.as_str(), v.1), ("_X", ""));
		let v = variable_declaration("__X").unwrap();
		assert_eq!((v.0.name.as_str(), v.1), ("__X", ""));
		let v = variable_declaration("Variable").unwrap();
		assert_eq!((v.0.name.as_str(), v.1), ("Variable", ""));
		let v = variable_declaration("_Variable").unwrap();
		assert_eq!((v.0.name.as_str(), v.1), ("_Variable", ""));
		let v = variable_declaration("__Variable").unwrap();
		assert_eq!((v.0.name.as_str(), v.1), ("__Variable", ""));
		let v = variable_declaration("X,").unwrap();
		assert_eq!((v.0.name.as_str(), v.1), ("X", ","));
		let v = variable_declaration("_X,").unwrap();
		assert_eq!((v.0.name.as_str(), v.1), ("_X", ","));
		let v = variable_declaration("__X,").unwrap();
		assert_eq!((v.0.name.as_str(), v.1), ("__X", ","));
		let v = variable_declaration("Variable,").unwrap();
		assert_eq!((v.0.name.as_str(), v.1), ("Variable", ","));
		let v = variable_declaration("_Variable,").unwrap();
		assert_eq!((v.0.name.as_str(), v.1), ("_Variable", ","));
		let v = variable_declaration("__Variable,").unwrap();
		assert_eq!((v.0.name.as_str(), v.1), ("__Variable", ","));
	}

	#[test]
	fn parse_variable_declarations()
	{
		let v = variable_declarations("X.").unwrap().unwrap();
		assert_eq!(v.0.len(), 1);
		assert_eq!(v.0[0].name.as_str(), "X");
		assert_eq!(v.1, ".");

		let v = variable_declarations("X,Y,Z.").unwrap().unwrap();
		assert_eq!(v.0.len(), 3);
		assert_eq!(v.0[0].name.as_str(), "X");
		assert_eq!(v.0[1].name.as_str(), "Y");
		assert_eq!(v.0[2].name.as_str(), "Z");
		assert_eq!(v.1, ".");

		let v = variable_declarations("X, Y, Z.").unwrap().unwrap();
		assert_eq!(v.0.len(), 3);
		assert_eq!(v.0[0].name.as_str(), "X");
		assert_eq!(v.0[1].name.as_str(), "Y");
		assert_eq!(v.0[2].name.as_str(), "Z");
		assert_eq!(v.1, ".");

		let v = variable_declarations("X , Y , Z.").unwrap().unwrap();
		assert_eq!(v.0.len(), 3);
		assert_eq!(v.0[0].name.as_str(), "X");
		assert_eq!(v.0[1].name.as_str(), "Y");
		assert_eq!(v.0[2].name.as_str(), "Z");
		assert_eq!(v.1, ".");

		assert!(variable_declarations("test").unwrap().is_none());
		assert!(variable_declarations("X, test").is_err());
		assert!(variable_declarations("X ,test").is_err());
		assert!(variable_declarations("X,Y,Z, test").is_err());
		assert!(variable_declarations("X,Y,Z ,test").is_err());
	}
}
