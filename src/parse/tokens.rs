fn substring_offset(substring: &str, string: &str) -> usize
{
	substring.as_ptr() as usize - string.as_ptr() as usize
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub(crate) enum Keyword
{
	And,
	Exists,
	False,
	ForAll,
	Not,
	Or,
	True,
}

impl std::fmt::Debug for Keyword
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match &self
		{
			Self::And => write!(formatter, "and"),
			Self::Exists => write!(formatter, "exists"),
			Self::False => write!(formatter, "false"),
			Self::ForAll => write!(formatter, "forall"),
			Self::Not => write!(formatter, "not"),
			Self::Or => write!(formatter, "or"),
			Self::True => write!(formatter, "true"),
		}
	}
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub(crate) enum Symbol
{
	ArrowLeft,
	ArrowLeftAndRight,
	ArrowRight,
	Comma,
	Division,
	Equal,
	Exponentiation,
	Greater,
	GreaterOrEqual,
	Less,
	LessOrEqual,
	Minus,
	Multiplication,
	NotEqual,
	Percent,
	Plus,
	VerticalBar,
}

impl std::fmt::Debug for Symbol
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match &self
		{
			Self::ArrowLeft => write!(formatter, "<-"),
			Self::ArrowLeftAndRight => write!(formatter, "<->"),
			Self::ArrowRight => write!(formatter, "->"),
			Self::Comma => write!(formatter, ","),
			Self::Division => write!(formatter, "/"),
			Self::Equal => write!(formatter, "="),
			Self::Exponentiation => write!(formatter, "**"),
			Self::Greater => write!(formatter, ">"),
			Self::GreaterOrEqual => write!(formatter, ">="),
			Self::Less => write!(formatter, "<"),
			Self::LessOrEqual => write!(formatter, "<="),
			Self::Minus => write!(formatter, "-"),
			Self::Multiplication => write!(formatter, "*"),
			Self::NotEqual => write!(formatter, "!="),
			Self::Percent => write!(formatter, "%"),
			Self::Plus => write!(formatter, "+"),
			Self::VerticalBar => write!(formatter, "|"),
		}
	}
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub(crate) enum Token<'i>
{
	Identifier(&'i str),
	Number(usize),
	ParenthesizedExpression(&'i str),
	Symbol(Symbol),
}

fn is_identifier_start_character(character: char) -> bool
{
	character.is_ascii_alphabetic()
}

fn is_identifier_body_character(character: char) -> bool
{
	match character
	{
		'_' => true,
		_ if character.is_ascii_alphanumeric() => true,
		_ => false,
	}
}

pub(crate) fn identifier(input: &str) -> Option<(&str, &str)>
{
	let mut characters = input.char_indices();

	let first_character = loop
	{
		match characters.next()
		{
			Some((_, '_')) => continue,
			Some((_, character)) => break Some(character),
			None => break None,
		}
	}?;

	if !is_identifier_start_character(first_character)
	{
		return None;
	}

	loop
	{
		match characters.next()
		{
			None => return Some((input, characters.as_str())),
			Some((character_index, character)) =>
			{
				if !is_identifier_body_character(character)
				{
					return Some(input.split_at(character_index));
				}
			},
		}
	}
}

pub(crate) fn is_keyword(identifier: &str) -> bool
{
	match identifier
	{
		"and"
		| "exists"
		| "false"
		| "forall"
		| "not"
		| "or"
		| "true" => true,
		_ => false,
	}
}

fn number_string(input: &str) -> Option<(&str, &str)>
{
	let mut characters = input.char_indices();

	let (_, character) = match characters.next()
	{
		Some(characters_next) => characters_next,
		None => return None,
	};

	if !character.is_ascii_digit()
	{
		return None;
	}

	loop
	{
		match characters.next()
		{
			None => return Some((input, characters.as_str())),
			Some((character_index, character)) =>
			{
				if !character.is_ascii_digit()
				{
					return Some(input.split_at(character_index));
				}
			},
		}
	}
}

pub(crate) fn number(input: &str) -> Result<Option<(usize, &str)>, crate::parse::Error>
{
	let (number_string, remaining_input) = match number_string(input)
	{
		Some(number_string) => number_string,
		None => return Ok(None),
	};

	let number = number_string.parse()
		.map_err(|error| crate::parse::Error::new_parse_number(input,
			crate::parse::error::Location::new(0, Some(0)), error))?;

	Ok(Some((number, remaining_input)))
}

pub(crate) fn symbol(input: &str) -> Option<(Symbol, &str)>
{
	let mut characters = input.char_indices();

	let (_, character) = match characters.next()
	{
		Some(characters_next) => characters_next,
		None => return None,
	};

	let remaining_input = characters.as_str();

	match character
	{
		',' => Some((Symbol::Comma, remaining_input)),
		// <->, <-, <=, <
		'=' => Some((Symbol::Equal, remaining_input)),
		// !=
		'!' => match characters.next()
		{
			Some((_, '=')) => Some((Symbol::NotEqual, characters.as_str())),
			_ => None,
		},
		'<' => match characters.next()
		{
			Some((_, '-')) =>
			{
				let remaining_input = characters.as_str();

				match characters.next()
				{
					Some((_, '>')) => Some((Symbol::ArrowLeftAndRight, characters.as_str())),
					_ => Some((Symbol::ArrowLeft, remaining_input)),
				}
			},
			Some((_, '=')) => Some((Symbol::LessOrEqual, characters.as_str())),
			_ => Some((Symbol::Less, remaining_input)),
		},
		// >=, >
		'>' => match characters.next()
		{
			Some((_, '=')) => Some((Symbol::GreaterOrEqual, characters.as_str())),
			_ => Some((Symbol::Greater, remaining_input)),
		},
		'+' => Some((Symbol::Plus, remaining_input)),
		// ->, -
		'-' => match characters.next()
		{
			Some((_, '>')) => Some((Symbol::ArrowRight, characters.as_str())),
			_ => Some((Symbol::Minus, remaining_input)),
		},
		// **, *
		'*' => match characters.next()
		{
			Some((_, '*')) => Some((Symbol::Exponentiation, characters.as_str())),
			_ => Some((Symbol::Multiplication, remaining_input)),
		},
		'/' => Some((Symbol::Division, remaining_input)),
		'%' => Some((Symbol::Percent, remaining_input)),
		'|' => Some((Symbol::VerticalBar, remaining_input)),
		_ => None,
	}
}

pub(crate) fn parenthesized_expression(input: &str)
	-> Result<Option<(&str, &str)>, crate::parse::Error>
{
	let mut characters = input.chars();

	let (first_character, remaining_input) = match characters.next()
	{
		Some(first_character) => (first_character, characters.as_str()),
		None => return Ok(None),
	};

	if first_character != '('
	{
		return Ok(None);
	}

	let mut characters = remaining_input.char_indices();
	let mut number_of_open_parentheses = 1;

	while let Some((character_index, character)) = characters.next()
	{
		match character
		{
			'(' => number_of_open_parentheses += 1,
			')' => number_of_open_parentheses -= 1,
			_ => (),
		}

		if number_of_open_parentheses == 0
		{
			let position_of_closing_parenthesis = character_index;
			let (parenthesized_expression, _) =
				remaining_input.split_at(position_of_closing_parenthesis);
			let remaining_input = characters.as_str();

			return Ok(Some((parenthesized_expression, remaining_input)));
		}
	}

	Err(crate::parse::Error::new_unmatched_parenthesis(
		crate::parse::error::Location::new(0, Some(1))))
}

pub(crate) struct Tokens<'i, F>
{
	original_input: &'i str,
	input: &'i str,
	previous_index: usize,
	reached_end_of_stream: bool,
	functor: F,
}

impl<'i> Tokens<'i, ()>
{
	pub fn new_iter(input: &'i str) -> Tokens<'i, impl FnMut(Token<'i>) -> Option<Token<'i>>>
	{
		Tokens::new_filter_map(input, |x| Some(x))
	}

	pub fn new_filter<P>(input: &'i str, mut predicate: P)
		-> Tokens<'i, impl FnMut(Token<'i>) -> Option<Token<'i>>>
	where
		P: FnMut(&Token<'i>) -> bool,
	{
		Tokens::new_filter_map(input,
			move |x|
			{
				if predicate(&x)
				{
					Some(x)
				}
				else
				{
					None
				}
			})
	}
}

impl<'i, F> Tokens<'i, F>
{
	pub fn new_filter_map(input: &'i str, functor: F) -> Self
	{
		Self
		{
			original_input: input,
			input,
			previous_index: 0,
			reached_end_of_stream: false,
			functor,
		}
	}

	fn next_token(&mut self) -> Option<Result<(usize, usize, Token<'i>), crate::parse::Error>>
	{
		self.input = self.input.trim_start();
		let index_left = substring_offset(self.input, self.original_input);

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
			Ok(Some((parenthesized_expression, remaining_input))) =>
			{
				self.input = remaining_input;
				let index_right = substring_offset(self.input, self.original_input);

				return Some(Ok((index_left, index_right,
					Token::ParenthesizedExpression(parenthesized_expression))));
			},
			Ok(None) => (),
			Err(error) => return Some(Err(error)),
		}

		match number(self.input)
		{
			Ok(Some((number, remaining_input))) =>
			{
				self.input = remaining_input;
				let index_right = substring_offset(self.input, self.original_input);

				return Some(Ok((index_left, index_right, Token::Number(number))));
			},
			Ok(None) => (),
			Err(error) => return Some(Err(error)),
		}

		if let Some((identifier, remaining_input)) = identifier(self.input)
		{
			self.input = remaining_input;
			let index_right = substring_offset(self.input, self.original_input);

			return Some(Ok((index_left, index_right, Token::Identifier(identifier))));
		}

		if let Some((symbol, remaining_input)) = symbol(self.input)
		{
			self.input = remaining_input;
			let index_right = substring_offset(self.input, self.original_input);

			return Some(Ok((index_left, index_right, Token::Symbol(symbol))));
		}

		return Some(Err(crate::parse::Error::new_character_not_allowed(first_character,
			crate::parse::error::Location::new(0, Some(0)))));
	}

	pub fn remaining_input(&mut self) -> Option<&'i str>
	{
		if self.reached_end_of_stream
		{
			return None;
		}

		let remaining_input = self.original_input[self.previous_index..].trim();
		self.reached_end_of_stream = true;

		Some(remaining_input)
	}

	pub fn split(self) -> TokenSplit<Self>
	{
		TokenSplit::new(self)
	}
}

impl<'i, F, G> std::iter::Iterator for Tokens<'i, F>
where
	F: FnMut(Token<'i>) -> Option<G>,
{
	type Item = Result<(&'i str, G), crate::parse::Error>;

	fn next(&mut self) -> Option<Self::Item>
	{
		if self.previous_index == self.original_input.len()
		{
			return None;
		}

		loop
		{
			match self.next_token()
			{
				Some(Ok((index_left, index_right, token))) =>
				{
					let token = match (self.functor)(token)
					{
						None => continue,
						Some(token) => token,
					};

					let input_left = self.original_input[self.previous_index..index_left].trim();
					assert!(!input_left.is_empty());

					self.previous_index = index_right;

					return Some(Ok((input_left, token)));
				},
				Some(Err(error)) => return Some(Err(error)),
				None => return None,
			}
		}
	}
}

pub(crate) struct TokenSplit<T>
{
	tokens: T,
}

impl TokenSplit<()>
{
	pub fn new<T>(tokens: T) -> TokenSplit<T>
	{
		TokenSplit
		{
			tokens,
		}
	}
}

impl<'i, F, G> std::iter::Iterator for TokenSplit<Tokens<'i, F>>
where
	F: FnMut(Token<'i>) -> Option<G>,
{
	type Item = Result<&'i str, crate::parse::Error>;

	fn next(&mut self) -> Option<Self::Item>
	{
		match self.tokens.next()
		{
			Some(Ok((input_before, _))) => Some(Ok(input_before)),
			Some(Err(error)) => Some(Err(error)),
			None => match self.tokens.remaining_input()
			{
				Some(remaining_input) => Some(Ok(remaining_input)),
				None => None,
			},
		}
	}
}

#[cfg(test)]
mod tests
{
	use super::*;

	#[test]
	fn tokenize_identifier()
	{
		assert_eq!(identifier("test").unwrap(), ("test", ""));
		assert_eq!(identifier("test2").unwrap(), ("test2", ""));
		assert_eq!(identifier("Test").unwrap(), ("Test", ""));
		assert_eq!(identifier("Test2").unwrap(), ("Test2", ""));
		assert_eq!(identifier("_test").unwrap(), ("_test", ""));
		assert_eq!(identifier("_test2").unwrap(), ("_test2", ""));
		assert_eq!(identifier("__test").unwrap(), ("__test", ""));
		assert_eq!(identifier("__test2").unwrap(), ("__test2", ""));
		assert_eq!(identifier("test, test").unwrap(), ("test", ", test"));
		assert_eq!(identifier("test2, test").unwrap(), ("test2", ", test"));
		assert_eq!(identifier("Test, Test").unwrap(), ("Test", ", Test"));
		assert_eq!(identifier("Test2, Test").unwrap(), ("Test2", ", Test"));
		assert_eq!(identifier("_test, _test").unwrap(), ("_test", ", _test"));
		assert_eq!(identifier("_test2, _test").unwrap(), ("_test2", ", _test"));
		assert_eq!(identifier("__test, __test").unwrap(), ("__test", ", __test"));
		assert_eq!(identifier("__test2, __test").unwrap(), ("__test2", ", __test"));
		assert!(identifier("2test, test").is_none());
		assert!(identifier("#test, test").is_none());
		assert!(identifier("$test, test").is_none());
		assert!(identifier(",test, test").is_none());
	}

	#[test]
	fn tokenize_primitives()
	{
		assert_eq!(parenthesized_expression("(foo bar baz) test").unwrap(),
			Some(("foo bar baz", " test")));
		assert!(parenthesized_expression("( | asd#0231(asd|asd) test").is_err());
		assert_eq!(parenthesized_expression("( | asd#0231(asd|asd) ) test").unwrap(),
			Some((" | asd#0231(asd|asd) ", " test")));
		assert_eq!(parenthesized_expression("( | a)sd#0231(asd|asd)  test").unwrap(),
			Some((" | a", "sd#0231(asd|asd)  test")));

		assert_eq!(number("1234, ").unwrap(), Some((1234, ", ")));
		assert_eq!(number("1234.5, ").unwrap(), Some((1234, ".5, ")));
		assert_eq!(number("-1234, ").unwrap(), None);
		assert_eq!(number("a1234, ").unwrap(), None);

		assert_eq!(symbol("<-"), Some((Symbol::ArrowLeft, "")));
		assert_eq!(symbol("<->"), Some((Symbol::ArrowLeftAndRight, "")));
		assert_eq!(symbol("->"), Some((Symbol::ArrowRight, "")));
		assert_eq!(symbol(","), Some((Symbol::Comma, "")));
		assert_eq!(symbol("/"), Some((Symbol::Division, "")));
		assert_eq!(symbol("="), Some((Symbol::Equal, "")));
		assert_eq!(symbol("**"), Some((Symbol::Exponentiation, "")));
		assert_eq!(symbol(">"), Some((Symbol::Greater, "")));
		assert_eq!(symbol(">="), Some((Symbol::GreaterOrEqual, "")));
		assert_eq!(symbol("<"), Some((Symbol::Less, "")));
		assert_eq!(symbol("<="), Some((Symbol::LessOrEqual, "")));
		assert_eq!(symbol("-"), Some((Symbol::Minus, "")));
		assert_eq!(symbol("*"), Some((Symbol::Multiplication, "")));
		assert_eq!(symbol("!="), Some((Symbol::NotEqual, "")));
		assert_eq!(symbol("+"), Some((Symbol::Plus, "")));
		assert_eq!(symbol("|"), Some((Symbol::VerticalBar, "")));

		assert_eq!(symbol("<-a"), Some((Symbol::ArrowLeft, "a")));
		assert_eq!(symbol("<->a"), Some((Symbol::ArrowLeftAndRight, "a")));
		assert_eq!(symbol("->a"), Some((Symbol::ArrowRight, "a")));
		assert_eq!(symbol(",a"), Some((Symbol::Comma, "a")));
		assert_eq!(symbol("/a"), Some((Symbol::Division, "a")));
		assert_eq!(symbol("=a"), Some((Symbol::Equal, "a")));
		assert_eq!(symbol("**a"), Some((Symbol::Exponentiation, "a")));
		assert_eq!(symbol(">a"), Some((Symbol::Greater, "a")));
		assert_eq!(symbol(">=a"), Some((Symbol::GreaterOrEqual, "a")));
		assert_eq!(symbol("<a"), Some((Symbol::Less, "a")));
		assert_eq!(symbol("<=a"), Some((Symbol::LessOrEqual, "a")));
		assert_eq!(symbol("-a"), Some((Symbol::Minus, "a")));
		assert_eq!(symbol("*a"), Some((Symbol::Multiplication, "a")));
		assert_eq!(symbol("!=a"), Some((Symbol::NotEqual, "a")));
		assert_eq!(symbol("+a"), Some((Symbol::Plus, "a")));
		assert_eq!(symbol("|a"), Some((Symbol::VerticalBar, "a")));
	}
}
