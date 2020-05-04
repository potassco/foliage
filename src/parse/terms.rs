use super::tokens::*;

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

pub fn is_function_name(identifier: &str) -> bool
{
	if is_keyword(identifier)
	{
		return false;
	}

	let mut characters = identifier.chars();

	while let Some(character) = characters.next()
	{
		match character
		{
			'_' => continue,
			_ if character.is_ascii_lowercase() => return true,
			_ => return false,
		}
	}

	false
}

fn is_variable_name(identifier: &str) -> bool
{
	let mut characters = identifier.chars();

	while let Some(character) = characters.next()
	{
		match character
		{
			'_' => continue,
			_ if character.is_ascii_uppercase() => return true,
			_ => return false,
		}
	}

	false
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
pub(crate) enum ArithmeticOperatorClass
{
	Exponential,
	Multiplicative,
	Additive,
}

impl ArithmeticOperatorClass
{
	fn level(&self) -> usize
	{
		match self
		{
			Self::Exponential => 1,
			Self::Multiplicative => 2,
			Self::Additive => 3,
		}
	}
}

impl std::fmt::Debug for ArithmeticOperatorClass
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match &self
		{
			Self::Exponential => write!(formatter, "exponential"),
			Self::Multiplicative => write!(formatter, "multiplicative"),
			Self::Additive => write!(formatter, "additive"),
		}
	}
}

pub(crate) struct TermStr<'i, 'd, 'v, 'p, D>
{
	input: &'i str,
	declarations: &'d D,
	variable_declaration_stack: &'v crate::VariableDeclarationStackLayer<'p>,
}

impl<'i, 'd, 'v, 'p, D> TermStr<'i, 'd, 'v, 'p, D>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration,
{
	pub fn new(input: &'i str, declarations: &'d D,
		variable_declaration_stack: &'v crate::VariableDeclarationStackLayer<'p>)
		-> Self
	{
		Self
		{
			input,
			declarations,
			variable_declaration_stack,
		}
	}

	fn arithmetic_operator_classes(&self) -> Tokens<'i, impl FnMut(Token<'i>)
		-> Option<ArithmeticOperatorClass>>
	{
		let functor = |token| match token
		{
			Token::Symbol(Symbol::Exponentiation) => Some(ArithmeticOperatorClass::Exponential),
			Token::Symbol(Symbol::Multiplication) => Some(ArithmeticOperatorClass::Multiplicative),
			Token::Symbol(Symbol::Division) => Some(ArithmeticOperatorClass::Multiplicative),
			Token::Symbol(Symbol::Percent) => Some(ArithmeticOperatorClass::Multiplicative),
			Token::Symbol(Symbol::Plus) => Some(ArithmeticOperatorClass::Additive),
			Token::Symbol(Symbol::Minus) => Some(ArithmeticOperatorClass::Additive),
			_ => None,
		};

		// TODO: refactor so that self.input is always set correctly
		Tokens::new_filter_map(self.input, functor)
	}

	fn filter_by_arithmetic_operator_class(&self,
		arithmetic_operator_class: ArithmeticOperatorClass)
		-> Tokens<'i, impl FnMut(Token<'i>) -> Option<crate::BinaryOperator>>
	{
		let functor = move |token| match token
		{
			Token::Symbol(Symbol::Exponentiation) =>
				if arithmetic_operator_class == ArithmeticOperatorClass::Exponential
				{
					Some(crate::BinaryOperator::Exponentiate)
				}
				else
				{
					None
				},
			Token::Symbol(Symbol::Multiplication) =>
				if arithmetic_operator_class == ArithmeticOperatorClass::Multiplicative
				{
					Some(crate::BinaryOperator::Multiply)
				}
				else
				{
					None
				},
			Token::Symbol(Symbol::Division) =>
				if arithmetic_operator_class == ArithmeticOperatorClass::Multiplicative
				{
					Some(crate::BinaryOperator::Divide)
				}
				else
				{
					None
				},
			Token::Symbol(Symbol::Percent) =>
				if arithmetic_operator_class == ArithmeticOperatorClass::Multiplicative
				{
					Some(crate::BinaryOperator::Modulo)
				}
				else
				{
					None
				},
			Token::Symbol(Symbol::Plus) =>
				if arithmetic_operator_class == ArithmeticOperatorClass::Additive
				{
					Some(crate::BinaryOperator::Add)
				}
				else
				{
					None
				},
			Token::Symbol(Symbol::Minus) =>
				if arithmetic_operator_class == ArithmeticOperatorClass::Additive
				{
					Some(crate::BinaryOperator::Subtract)
				}
				else
				{
					None
				},
			_ => None,
		};

		Tokens::new_filter_map(self.input, functor)
	}

	pub fn top_level_arithmetic_operator_class(&self)
		-> Result<Option<ArithmeticOperatorClass>, crate::parse::Error>
	{
		let mut top_level_arithmetic_operator_class = None;

		for arithmetic_operator_class in self.arithmetic_operator_classes()
		{
			let (_, arithmetic_operator_class) = arithmetic_operator_class?;

			top_level_arithmetic_operator_class = match top_level_arithmetic_operator_class
			{
				None => Some(arithmetic_operator_class),
				Some(top_level_arithmetic_operator_class) =>
				{
					if arithmetic_operator_class.level()
						> top_level_arithmetic_operator_class.level()
					{
						Some(arithmetic_operator_class)
					}
					else
					{
						Some(top_level_arithmetic_operator_class)
					}
				},
			}
		}

		Ok(top_level_arithmetic_operator_class)
	}

	pub fn parse(&self, level: usize) -> Result<crate::Term, crate::parse::Error>
	{
		let indentation = "  ".repeat(level);
		println!("{}- parsing term: {}", indentation, self.input);

		let input = self.input.trim_start();

		match input.chars().next()
		{
			Some(')') => return Err(crate::parse::Error::new_unmatched_parenthesis(
				crate::parse::error::Location::new(0, Some(0)))),
			// TODO: implement absolute value function
			Some('|') => unimplemented!(),
			None => return Err(crate::parse::Error::new_empty_expression(
				crate::parse::error::Location::new(0, Some(0)))),
			_ => (),
		}

		// Parse arithmetic infix operations
		if let Some(top_level_arithmetic_operator_class) =
			self.top_level_arithmetic_operator_class()?
		{
			println!("{}  parsing {:?} arithmetic term", indentation,
				top_level_arithmetic_operator_class);

			if top_level_arithmetic_operator_class == ArithmeticOperatorClass::Exponential
			{
				return self.exponentiate(
					self.filter_by_arithmetic_operator_class(top_level_arithmetic_operator_class)
						.split(), level + 1);
			}

			// Parse arguments of arithmetic infix operations
			let mut argument_iterator =
				self.filter_by_arithmetic_operator_class(top_level_arithmetic_operator_class);

			let (first_argument, first_binary_operator) = argument_iterator.next().ok_or_else(||
				crate::parse::Error::new_expected_term(
					crate::parse::error::Location::new(0, Some(0))))??;
			let first_argument =
				TermStr::new(first_argument, self.declarations, self.variable_declaration_stack)
					.parse(level + 1)?;
			// TODO: improve error handling if the terms between the operators are invalid

			let (accumulator, last_binary_operator) =
				argument_iterator.try_fold((first_argument, first_binary_operator),
					|(accumulator, binary_operator), argument|
					{
						let (argument, next_binary_operator) = argument?;
						let argument = TermStr::new(argument, self.declarations,
							self.variable_declaration_stack)
								.parse(level + 1)?;
						let binary_operation =
							crate::BinaryOperation::new(binary_operator, Box::new(accumulator),
								Box::new(argument));
						let formula = crate::Term::BinaryOperation(binary_operation);

						Ok((formula, next_binary_operator))
					})?;

			// The last item hasn’t been consumed yet, so it’s safe to unwrap it
			let last_argument = argument_iterator.remaining_input().unwrap();
			let last_argument =
				TermStr::new(last_argument, self.declarations, self.variable_declaration_stack)
					.parse(level + 1)?;
			let last_binary_operation =
				crate::BinaryOperation::new(last_binary_operator, Box::new(accumulator),
					Box::new(last_argument));

			return Ok(crate::Term::BinaryOperation(last_binary_operation));
		}

		if let Some((number, input)) = number(input)?
		{
			if !input.trim().is_empty()
			{
				return Err(crate::parse::Error::new_unexpected_token(
					crate::parse::error::Location::new(0, Some(0))));
			}

			return Ok(crate::Term::integer(number as i32));
		}

		if let Some((identifier, input)) = identifier(input)
		{
			match identifier
			{
				"inf" =>
				{
					if !input.trim().is_empty()
					{
						return Err(crate::parse::Error::new_unexpected_token(
							crate::parse::error::Location::new(0, Some(0))))
					}

					return Ok(crate::Term::infimum());
				},
				"sup" =>
				{
					if !input.trim().is_empty()
					{
						return Err(crate::parse::Error::new_unexpected_token(
							crate::parse::error::Location::new(0, Some(0))))
					}

					return Ok(crate::Term::supremum());
				},
				"true" =>
				{
					if !input.trim().is_empty()
					{
						return Err(crate::parse::Error::new_unexpected_token(
							crate::parse::error::Location::new(0, Some(0))))
					}

					return Ok(crate::Term::true_());
				},
				"false" =>
				{
					if !input.trim().is_empty()
					{
						return Err(crate::parse::Error::new_unexpected_token(
							crate::parse::error::Location::new(0, Some(0))))
					}

					return Ok(crate::Term::false_());
				},
				_ if is_variable_name(identifier) =>
				{
					if !input.trim().is_empty()
					{
						return Err(crate::parse::Error::new_unexpected_token(
							crate::parse::error::Location::new(0, Some(0))))
					}

					let declaration = self.variable_declaration_stack.find_or_create(identifier);
					return Ok(crate::Term::variable(declaration));
				},
				_ if is_function_name(identifier) =>
				{
					let function_name = identifier;
					println!("{}  parsing function {}", indentation, function_name);

					let input = input.trim_start();

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

					let declaration = self.declarations.find_or_create_function_declaration(
						function_name, arguments.len());
					return Ok(crate::Term::function(declaration, arguments));
				},
				_ => (),
			}
		}

		// TODO: parse negative value

		// Parse parenthesized terms
		if let Some((parenthesized_expression, input)) = parenthesized_expression(input)?
		{
			if !input.trim().is_empty()
			{
				return Err(crate::parse::Error::new_unexpected_token(
					crate::parse::error::Location::new(0, Some(0))));
			}

			return TermStr::new(parenthesized_expression, self.declarations,
				self.variable_declaration_stack)
					.parse(level + 1);
		}

		Err(crate::parse::Error::new_unexpected_token(
			crate::parse::error::Location::new(0, Some(0))))
	}

	// TODO: refactor
	fn exponentiate_inner<T>(&self, mut argument_iterator: T, level: usize)
		-> Result<Option<crate::Term>, crate::parse::Error>
	where
		T: std::iter::Iterator<Item = Result<&'i str, crate::parse::Error>>
	{
		match argument_iterator.next()
		{
			Some(argument) =>
			{
				// TODO: improve error handling if antecedent cannot be parsed
				let argument =
					TermStr::new(argument?, self.declarations, self.variable_declaration_stack)
						.parse(level)?;
				match self.exponentiate_inner(argument_iterator, level)?
				{
					Some(next_argument) => Ok(Some(crate::Term::exponentiate(Box::new(argument),
						Box::new(next_argument)))),
					None => Ok(Some(argument)),
				}
			},
			None => Ok(None),
		}
	}

	fn exponentiate<T>(&self, mut argument_iterator: T, level: usize)
		-> Result<crate::Term, crate::parse::Error>
	where
		T: std::iter::Iterator<Item = Result<&'i str, crate::parse::Error>>
	{
		match argument_iterator.next()
		{
			Some(argument) =>
			{
				// TODO: improve error handling if antecedent cannot be parsed
				let argument =
					TermStr::new(argument?, self.declarations, self.variable_declaration_stack)
						.parse(level)?;
				match self.exponentiate_inner(argument_iterator, level)?
				{
					Some(next_argument) =>
						Ok(crate::Term::exponentiate(Box::new(argument), Box::new(next_argument))),
					None => Err(crate::parse::Error::new_expected_term(
						crate::parse::error::Location::new(0, Some(0)))),
				}
			},
			None => Err(crate::parse::Error::new_expected_term(
				crate::parse::error::Location::new(0, Some(0)))),
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
