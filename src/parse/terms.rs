use nom::
{
	IResult,
	branch::alt,
	bytes::complete::tag,
	character::complete::multispace0,
	combinator::{map, opt},
	multi::{many1, separated_list},
	sequence::{delimited, pair, preceded, terminated},
};

use super::{boolean, function_or_predicate_name, integer, special_integer, string, variable_name};

fn negative<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Term>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	map
	(
		preceded
		(
			terminated
			(
				tag("-"),
				multispace0,
			),
			|i| term_precedence_1(i, d, v),
		),
		|x| match x
		{
			crate::Term::Integer(value) => crate::Term::integer(-value),
			crate::Term::UnaryOperation(
				crate::UnaryOperation{operator: crate::UnaryOperator::Negative, argument})
				=> *argument,
			_ => crate::Term::negative(Box::new(x)),
		}
	)(i)
}

fn absolute_value<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Term>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	map
	(
		delimited
		(
			terminated
			(
				tag("|"),
				multispace0,
			),
			|i| term(i, d, v),
			preceded
			(
				multispace0,
				tag("|"),
			),
		),
		|x| crate::Term::absolute_value(Box::new(x)),
	)(i)
}

pub(crate) fn function_or_predicate<'i, 'v, D>(i: &'i str, d: &D,
	v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, (&'i str, Option<crate::Terms>)>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	pair
	(
		function_or_predicate_name,
		opt
		(
			delimited
			(
				delimited
				(
					multispace0,
					tag("("),
					multispace0,
				),
				separated_list
				(
					delimited
					(
						multispace0,
						tag(","),
						multispace0,
					),
					|i| term(i, d, v),
				),
				preceded
				(
					multispace0,
					tag(")"),
				),
			)
		),
	)(i)
}

fn function<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Function>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	map
	(
		|i| function_or_predicate(i, d, v),
		|(name, arguments)|
		{
			let arguments = match arguments
			{
				Some(arguments) => arguments,
				None => vec![],
			};

			let declaration = d.find_or_create_function_declaration(name, arguments.len());

			crate::Function::new(declaration, arguments)
		},
	)(i)
}

pub(crate) fn variable_declaration(i: &str) -> IResult<&str, crate::VariableDeclaration>
{
	map
	(
		variable_name,
		|name| crate::VariableDeclaration::new(name.to_string())
	)(i)
}

fn variable<'i, 'v>(i: &'i str, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Variable>
{
	map
	(
		variable_name,
		|name|
		{
			let declaration = v.find_or_create(name);

			crate::Variable::new(declaration)
		},
	)(i)
}

fn term_parenthesized<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Term>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	delimited
	(
		terminated
		(
			tag("("),
			multispace0,
		),
		|i| term(i, d, v),
		preceded
		(
			multispace0,
			tag(")"),
		),
	)(i)
}

fn term_precedence_0<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Term>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	alt
	((
		map
		(
			boolean,
			crate::Term::Boolean,
		),
		map
		(
			special_integer,
			crate::Term::SpecialInteger,
		),
		map
		(
			integer,
			crate::Term::Integer,
		),
		map
		(
			|i| function(i, d, v),
			crate::Term::Function,
		),
		map
		(
			string,
			crate::Term::String,
		),
		map
		(
			|i| variable(i, v),
			crate::Term::Variable,
		),
		|i| absolute_value(i, d, v),
		|i| term_parenthesized(i, d, v),
	))(i)
}

fn term_precedence_1<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Term>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	alt
	((
		|i| negative(i, d, v),
		|i| term_precedence_0(i, d, v),
	))(i)
}

fn term_precedence_2<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Term>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	alt
	((
		map
		(
			pair
			(
				many1
				(
					terminated
					(
						|i| term_precedence_1(i, d, v),
						delimited
						(
							multispace0,
							tag("**"),
							multispace0,
						),
					)
				),
				|i| term_precedence_1(i, d, v),
			),
			|(arguments, last_argument)| arguments.into_iter().rev().fold(last_argument,
				|accumulator, argument|
					crate::Term::exponentiate(Box::new(argument), Box::new(accumulator))),
		),
		|i| term_precedence_1(i, d, v),
	))(i)
}

fn term_precedence_3<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Term>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	alt
	((
		map
		(
			pair
			(
				|i| term_precedence_2(i, d, v),
				many1
				(
					pair
					(
						delimited
						(
							multispace0,
							alt
							((
								tag("*"),
								tag("/"),
								tag("%"),
							)),
							multispace0,
						),
						|i| term_precedence_2(i, d, v),
					)
				),
			),
			|(first_argument, arguments)| arguments.into_iter().fold(first_argument,
				|accumulator, (operator, argument)|
				match operator
				{
					"*" => crate::Term::multiply(Box::new(accumulator), Box::new(argument)),
					"/" => crate::Term::divide(Box::new(accumulator), Box::new(argument)),
					"%" => crate::Term::modulo(Box::new(accumulator), Box::new(argument)),
					// TODO: handle appropriately
					_ => panic!("test"),
				})
		),
		|i| term_precedence_2(i, d, v),
	))(i)
}

fn term_precedence_4<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Term>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	alt
	((
		map
		(
			pair
			(
				|i| term_precedence_3(i, d, v),
				many1
				(
					pair
					(
						delimited
						(
							multispace0,
							alt
							((
								tag("+"),
								tag("-"),
							)),
							multispace0,
						),
						|i| term_precedence_3(i, d, v),
					)
				),
			),
			|(first_argument, arguments)| arguments.into_iter().fold(first_argument,
				|accumulator, (operator, argument)|
				match operator
				{
					"+" => crate::Term::add(Box::new(accumulator), Box::new(argument)),
					"-" => crate::Term::subtract(Box::new(accumulator), Box::new(argument)),
					// TODO: handle appropriately
					_ => panic!("test"),
				})
		),
		|i| term_precedence_3(i, d, v),
	))(i)
}

pub fn term<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Term>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	term_precedence_4(i, d, v)
}

#[cfg(test)]
mod tests
{
	use crate::{Term, VariableDeclaration, VariableDeclarationStackLayer};
	use crate::parse::terms as original;
	use crate::utils::*;

	fn term(i: &str) -> Term
	{
		original::term(i, &Declarations::new(), &VariableDeclarationStackLayer::free()).unwrap().1
	}

	fn format_term(i: &str) -> String
	{
		format!("{}", term(i))
	}

	#[test]
	fn parse_parenthesized()
	{
		assert_eq!(format_term("(1)"), format_term("1"));
		assert_eq!(format_term("((1))"), format_term("1"));
		assert_eq!(format_term("(-1)"), format_term("-1"));
		assert_eq!(format_term("((-1))"), format_term("-1"));
		assert_eq!(format_term("(-(1))"), format_term("-1"));
		assert_eq!(format_term("-((1))"), format_term("-1"));
		assert_eq!(format_term("(-(-1))"), format_term("1"));
		assert_eq!(format_term("-((-1))"), format_term("1"));
		assert_eq!(format_term("-(-(1))"), format_term("1"));
		assert_eq!(format_term("-(-(-1))"), format_term("-1"));
		assert_eq!(format_term("(a)"), format_term("a"));
		assert_eq!(format_term("((a))"), format_term("a"));
		assert_eq!(format_term("(X)"), format_term("X"));
		assert_eq!(format_term("((X))"), format_term("X"));
		assert_eq!(format_term("(\"test\")"), format_term("\"test\""));
		assert_eq!(format_term("((\"test\"))"), format_term("\"test\""));
		assert_eq!(format_term("(a ** b)"), format_term("a ** b"));
		assert_eq!(format_term("(a * b)"), format_term("a * b"));
		assert_eq!(format_term("(a / b)"), format_term("a / b"));
		assert_eq!(format_term("(a % b)"), format_term("a % b"));
		assert_eq!(format_term("(a + b)"), format_term("a + b"));
		assert_eq!(format_term("(a - b)"), format_term("a - b"));
		assert_eq!(format_term("((a ** b))"), format_term("a ** b"));
		assert_eq!(format_term("((a * b))"), format_term("a * b"));
		assert_eq!(format_term("((a / b))"), format_term("a / b"));
		assert_eq!(format_term("((a % b))"), format_term("a % b"));
		assert_eq!(format_term("((a + b))"), format_term("a + b"));
		assert_eq!(format_term("((a - b))"), format_term("a - b"));
		assert_eq!(format_term("(f(a, b))"), format_term("f(a, b)"));
		assert_eq!(format_term("((f(a, b)))"), format_term("f(a, b)"));
		assert_eq!(format_term("f((a), (b))"), format_term("f(a, b)"));
		assert_eq!(format_term("f(|-a|)"), format_term("f(|-a|)"));
		assert_eq!(format_term("f((|-a|))"), format_term("f(|-a|)"));
		assert_eq!(format_term("f((-a))"), format_term("f(-a)"));
		assert_eq!(format_term("f(((-a)))"), format_term("f(-a)"));
		assert_eq!(format_term("f((a ** b))"), format_term("f(a ** b)"));
		assert_eq!(format_term("f((a * b))"), format_term("f(a * b)"));
		assert_eq!(format_term("f((a / b))"), format_term("f(a / b)"));
		assert_eq!(format_term("f((a % b))"), format_term("f(a % b)"));
		assert_eq!(format_term("f((a + b))"), format_term("f(a + b)"));
		assert_eq!(format_term("f((a - b))"), format_term("f(a - b)"));
		assert_eq!(format_term("f(((a ** b)))"), format_term("f(a ** b)"));
		assert_eq!(format_term("f(((a * b)))"), format_term("f(a * b)"));
		assert_eq!(format_term("f(((a / b)))"), format_term("f(a / b)"));
		assert_eq!(format_term("f(((a % b)))"), format_term("f(a % b)"));
		assert_eq!(format_term("f(((a + b)))"), format_term("f(a + b)"));
		assert_eq!(format_term("f(((a - b)))"), format_term("f(a - b)"));
		assert_eq!(format_term("(|a ** b|)"), format_term("|a ** b|"));
		assert_eq!(format_term("|(a ** b)|"), format_term("|a ** b|"));
		assert_eq!(format_term("(|(a ** b)|)"), format_term("|a ** b|"));
		assert_eq!(format_term("(|a * b|)"), format_term("|a * b|"));
		assert_eq!(format_term("|(a * b)|"), format_term("|a * b|"));
		assert_eq!(format_term("(|(a * b)|)"), format_term("|a * b|"));
		assert_eq!(format_term("(|a / b|)"), format_term("|a / b|"));
		assert_eq!(format_term("|(a / b)|"), format_term("|a / b|"));
		assert_eq!(format_term("(|(a / b)|)"), format_term("|a / b|"));
		assert_eq!(format_term("(|a % b|)"), format_term("|a % b|"));
		assert_eq!(format_term("|(a % b)|"), format_term("|a % b|"));
		assert_eq!(format_term("(|(a % b)|)"), format_term("|a % b|"));
	}

	#[test]
	fn parse_boolean()
	{
		assert_eq!(term("true"), Term::true_());
		assert_eq!(term("false"), Term::false_());
	}

	#[test]
	fn parse_integer()
	{
		assert_eq!(term("0"), Term::integer(0));
		assert_eq!(term("10000"), Term::integer(10000));
		assert_eq!(term("+10000"), Term::integer(10000));
		assert_eq!(term("-10000"), Term::integer(-10000));
	}

	#[test]
	fn parse_special_integer()
	{
		assert_eq!(term("#inf"), Term::infimum());
		assert_eq!(term("#sup"), Term::supremum());
	}

	#[test]
	fn parse_string()
	{
		// TODO: fix
		//assert_eq!(term("\"\""), Term::string("".to_string()));
		assert_eq!(term("\"a\""), Term::string("a".to_string()));
		assert_eq!(term("\"#\""), Term::string("#".to_string()));
		assert_eq!(term("\" \""), Term::string(" ".to_string()));
		assert_eq!(term("\"     \""), Term::string("     ".to_string()));
		assert_eq!(term("\"test test\""), Term::string("test test".to_string()));
		assert_eq!(term("\"123 456\""), Term::string("123 456".to_string()));
		assert_eq!(term("\"-#? -#?\""), Term::string("-#? -#?".to_string()));
		assert_eq!(term("\"\\ntest\\n123\\n\""), Term::string("\ntest\n123\n".to_string()));
		assert_eq!(term("\"\\ttest\\t123\\t\""), Term::string("\ttest\t123\t".to_string()));
		assert_eq!(term("\"\\\\test\\\\123\\\\\""), Term::string("\\test\\123\\".to_string()));
		assert_eq!(term("\"🙂test🙂123🙂\""), Term::string("🙂test🙂123🙂".to_string()));
	}

	#[test]
	fn parse_function()
	{
		let term_as_function = |i| match term(i)
		{
			Term::Function(function) => function,
			_ => panic!("expected function"),
		};

		assert_eq!(term_as_function("s").declaration.name, "s");
		assert_eq!(term_as_function("s").declaration.arity, 0);
		assert_eq!(term_as_function("s()").declaration.name, "s");
		assert_eq!(term_as_function("s").declaration.arity, 0);
		assert_eq!(term_as_function("s(1, 2, 3)").declaration.name, "s");
		assert_eq!(term_as_function("s(1, 2, 3)").declaration.arity, 3);
		assert_eq!(term_as_function("s(1, 2, 3)").arguments.remove(0), Term::integer(1));
		assert_eq!(term_as_function("s(1, 2, 3)").arguments.remove(2), Term::integer(3));
	}

	#[test]
	fn parse_variable()
	{
		let term_as_variable = |i| match term(i)
		{
			Term::Variable(variable) => variable,
			_ => panic!("expected variable"),
		};

		assert_eq!(term_as_variable("X").declaration.name, "X");
		assert_eq!(term_as_variable("Variable_123").declaration.name, "Variable_123");
	}

	#[test]
	fn parse_unary()
	{
		assert_eq!(format_term("|a|"), "|a|");
		assert_eq!(format_term("||a||"), "||a||");
		assert_eq!(format_term("|a - b|"), "|a - b|");
		assert_eq!(format_term("|a| - b"), "|a| - b");
		assert_eq!(format_term("a - |b|"), "a - |b|");
		assert_eq!(format_term("||a| - b|"), "||a| - b|");
		assert_eq!(format_term("|a - |b||"), "|a - |b||");
		assert_eq!(format_term("||a| - |b||"), "||a| - |b||");
		assert_eq!(format_term("||a| - |b| - |c||"), "||a| - |b| - |c||");
		assert_eq!(format_term("||a - b| - |c - d||"), "||a - b| - |c - d||");
		assert_eq!(format_term("-a"), "-a");
		assert_eq!(format_term("--a"), "a");
		assert_eq!(format_term("---a"), "-a");
		assert_eq!(format_term("-(a + b)"), "-(a + b)");
		assert_eq!(format_term("-|a + b|"), "-|a + b|");
		assert_eq!(format_term("--|a + b|"), "|a + b|");
		assert_eq!(format_term("---|a + b|"), "-|a + b|");
		assert_eq!(term("5"), Term::integer(5));
		assert_eq!(term("-5"), Term::integer(-5));
		assert_eq!(term("--5"), Term::integer(5));
		assert_eq!(term("---5"), Term::integer(-5));
		assert_eq!(term("0"), Term::integer(0));
		assert_eq!(term("-0"), Term::integer(0));
		assert_eq!(term("--0"), Term::integer(0));
		assert_eq!(term("---0"), Term::integer(0));
	}

	#[test]
	fn parse_exponentiate()
	{
		assert_eq!(term("1 ** (2 ** (3 ** (4 ** 5)))"), term("1 ** 2 ** 3 ** 4 ** 5"));
		assert_eq!(format_term("1 ** 2 ** 3 ** 4 ** 5"), "1 ** 2 ** 3 ** 4 ** 5");
		assert_eq!(term("1 ** (2 ** (3 ** (4 ** 5)))"), term("1 ** 2 ** 3 ** 4 ** 5"));
		// As exponentiation is right-associative, these parentheses cannot be omitted
		assert_ne!(term("(((1 ** 2) ** 3) ** 4) ** 5"), term("1 ** 2 ** 3 ** 4 ** 5"));
		assert_eq!(format_term("(((1 ** 2) ** 3) ** 4) ** 5"), "(((1 ** 2) ** 3) ** 4) ** 5");
	}

	#[test]
	fn parse_multiplicative()
	{
		assert_eq!(format_term("(a * b) * (c * d)"), "a * b * c * d");
		assert_eq!(format_term("(a * b) * (c / d)"), "a * b * c / d");
		assert_eq!(format_term("(a * b) * (c % d)"), "a * b * (c % d)");
		assert_eq!(format_term("(a / b) * (c * d)"), "a / b * c * d");
		assert_eq!(format_term("(a / b) * (c / d)"), "a / b * c / d");
		assert_eq!(format_term("(a / b) * (c % d)"), "a / b * (c % d)");
		assert_eq!(format_term("(a % b) * (c * d)"), "a % b * c * d");
		assert_eq!(format_term("(a % b) * (c / d)"), "a % b * c / d");
		assert_eq!(format_term("(a % b) * (c % d)"), "a % b * (c % d)");
		assert_eq!(format_term("(a * b) / (c * d)"), "a * b / (c * d)");
		assert_eq!(format_term("(a * b) / (c / d)"), "a * b / (c / d)");
		assert_eq!(format_term("(a * b) / (c % d)"), "a * b / (c % d)");
		assert_eq!(format_term("(a / b) / (c * d)"), "a / b / (c * d)");
		assert_eq!(format_term("(a / b) / (c / d)"), "a / b / (c / d)");
		assert_eq!(format_term("(a / b) / (c % d)"), "a / b / (c % d)");
		assert_eq!(format_term("(a % b) / (c * d)"), "a % b / (c * d)");
		assert_eq!(format_term("(a % b) / (c / d)"), "a % b / (c / d)");
		assert_eq!(format_term("(a % b) / (c % d)"), "a % b / (c % d)");
		assert_eq!(format_term("(a * b) % (c * d)"), "a * b % (c * d)");
		assert_eq!(format_term("(a * b) % (c / d)"), "a * b % (c / d)");
		assert_eq!(format_term("(a * b) % (c % d)"), "a * b % (c % d)");
		assert_eq!(format_term("(a / b) % (c * d)"), "a / b % (c * d)");
		assert_eq!(format_term("(a / b) % (c / d)"), "a / b % (c / d)");
		assert_eq!(format_term("(a / b) % (c % d)"), "a / b % (c % d)");
		assert_eq!(format_term("(a % b) % (c * d)"), "a % b % (c * d)");
		assert_eq!(format_term("(a % b) % (c / d)"), "a % b % (c / d)");
		assert_eq!(format_term("(a % b) % (c % d)"), "a % b % (c % d)");

		// TODO: test malformed expressions
	}

	#[test]
	fn parse_additive()
	{
		assert_eq!(format_term("(a + b) + (c + d)"), "a + b + c + d");
		assert_eq!(format_term("(a + b) + (c - d)"), "a + b + c - d");
		assert_eq!(format_term("(a - b) + (c + d)"), "a - b + c + d");
		assert_eq!(format_term("(a - b) + (c - d)"), "a - b + c - d");
		assert_eq!(format_term("(a + b) - (c + d)"), "a + b - (c + d)");
		assert_eq!(format_term("(a + b) - (c - d)"), "a + b - (c - d)");
		assert_eq!(format_term("(a - b) - (c + d)"), "a - b - (c + d)");
		assert_eq!(format_term("(a - b) - (c - d)"), "a - b - (c - d)");
	}

	#[test]
	fn parse_precedence()
	{
		assert_eq!(term("-a + b"), term("(-a) + b"));
		assert_eq!(term("-a + -b"), term("(-a) + (-b)"));
		assert_eq!(term("-a + -b"), term("-(a) + -(b)"));
		assert_eq!(format_term("-(a + b)"), "-(a + b)");
		assert_eq!(term("-a - b"), term("(-a) - b"));
		assert_eq!(term("-a - -b"), term("(-a) - (-b)"));
		assert_eq!(term("-a - -b"), term("-(a) - -(b)"));
		assert_eq!(term("-a * b"), term("(-a) * b"));
		assert_eq!(term("-a * -b"), term("(-a) * (-b)"));
		assert_eq!(term("-a * -b"), term("-(a) * -(b)"));
		assert_eq!(term("-a / b"), term("(-a) / b"));
		assert_eq!(term("-a / -b"), term("(-a) / (-b)"));
		assert_eq!(term("-a / -b"), term("-(a) / -(b)"));
		assert_eq!(term("-a % b"), term("(-a) % b"));
		assert_eq!(term("-a % -b"), term("(-a) % (-b)"));
		assert_eq!(term("-a % -b"), term("-(a) % -(b)"));
		assert_eq!(term("-a ** b"), term("(-a) ** b"));
		assert_eq!(term("-a ** -b"), term("(-a) ** (-b)"));
		assert_eq!(term("-a ** -b"), term("-(a) ** -(b)"));
		assert_eq!(format_term("-(a + b)"), "-(a + b)");
		assert_eq!(format_term("-(a + b)"), "-(a + b)");
		assert_eq!(format_term("-(a + b)"), "-(a + b)");
		assert_eq!(format_term("-(a + b)"), "-(a + b)");
		assert_eq!(format_term("-(a + b)"), "-(a + b)");
		assert_eq!(term("a + (b * c) + d"), term("(a + (b * c)) + d"));
		assert_eq!(term("a + (b / c) + d"), term("(a + (b / c)) + d"));
		assert_eq!(term("a + (b % c) + d"), term("(a + (b % c)) + d"));
		assert_eq!(term("a - (b * c) - d"), term("(a - (b * c)) - d"));
		assert_eq!(term("a - (b / c) - d"), term("(a - (b / c)) - d"));
		assert_eq!(term("a - (b % c) - d"), term("(a - (b % c)) - d"));
		assert_eq!(format_term("(a + b) * (c + d)"), "(a + b) * (c + d)");
		assert_eq!(format_term("(a + b) / (c + d)"), "(a + b) / (c + d)");
		assert_eq!(format_term("(a + b) % (c + d)"), "(a + b) % (c + d)");
		assert_eq!(format_term("(a - b) * (c - d)"), "(a - b) * (c - d)");
		assert_eq!(format_term("(a - b) / (c - d)"), "(a - b) / (c - d)");
		assert_eq!(format_term("(a - b) % (c - d)"), "(a - b) % (c - d)");
		assert_eq!(term("a ** b ** c + d ** e ** f"), term("(a ** b ** c) + (d ** e ** f)"));
		assert_eq!(term("a ** (b ** c + d) ** e ** f"), term("a ** ((b ** c + d) ** (e ** f))"));
		assert_eq!(term("a ** b ** (c + d) ** e ** f"), term("a ** (b ** ((c + d) ** (e ** f)))"));
		assert_eq!(term("a ** b ** (c + d ** e) ** f"), term("a ** (b ** ((c + d ** e) ** f))"));
		assert_eq!(term("a ** b ** c - d ** e ** f"), term("(a ** b ** c) - (d ** e ** f)"));
		assert_eq!(term("a ** (b ** c - d) ** e ** f"), term("a ** ((b ** c - d) ** (e ** f))"));
		assert_eq!(term("a ** b ** (c - d) ** e ** f"), term("a ** (b ** ((c - d) ** (e ** f)))"));
		assert_eq!(term("a ** b ** (c - d ** e) ** f"), term("a ** (b ** ((c - d ** e) ** f))"));
		assert_eq!(term("a ** b ** c * d ** e ** f"), term("(a ** b ** c) * (d ** e ** f)"));
		assert_eq!(term("a ** (b ** c * d) ** e ** f"), term("a ** ((b ** c * d) ** (e ** f))"));
		assert_eq!(term("a ** b ** (c * d) ** e ** f"), term("a ** (b ** ((c * d) ** (e ** f)))"));
		assert_eq!(term("a ** b ** (c * d ** e) ** f"), term("a ** (b ** ((c * d ** e) ** f))"));
		assert_eq!(term("a ** b ** c / d ** e ** f"), term("(a ** b ** c) / (d ** e ** f)"));
		assert_eq!(term("a ** (b ** c / d) ** e ** f"), term("a ** ((b ** c / d) ** (e ** f))"));
		assert_eq!(term("a ** b ** (c / d) ** e ** f"), term("a ** (b ** ((c / d) ** (e ** f)))"));
		assert_eq!(term("a ** b ** (c / d ** e) ** f"), term("a ** (b ** ((c / d ** e) ** f))"));
		assert_eq!(term("a ** b ** c % d ** e ** f"), term("(a ** b ** c) % (d ** e ** f)"));
		assert_eq!(term("a ** (b ** c % d) ** e ** f"), term("a ** ((b ** c % d) ** (e ** f))"));
		assert_eq!(term("a ** b ** (c % d) ** e ** f"), term("a ** (b ** ((c % d) ** (e ** f)))"));
		assert_eq!(term("a ** b ** (c % d ** e) ** f"), term("a ** (b ** ((c % d ** e) ** f))"));
	}

	#[test]
	fn parse_bounds()
	{
		let term = |i| original::term(i, &Declarations::new(),
			&VariableDeclarationStackLayer::free()).unwrap().0;

		assert_eq!(term("1 ** 2 ** 3, rest"), ", rest");
		assert_eq!(term("1 * 2 * 3, rest"), ", rest");
		assert_eq!(term("1 / 2 / 3, rest"), ", rest");
		assert_eq!(term("1 % 2 % 3, rest"), ", rest");
		assert_eq!(term("1 + 2 + 3, rest"), ", rest");
		assert_eq!(term("1 - 2 - 3, rest"), ", rest");
		assert_eq!(term("1, rest"), ", rest");
		assert_eq!(term("-1, rest"), ", rest");
		assert_eq!(term("--1, rest"), ", rest");
		assert_eq!(term("|1|, rest"), ", rest");
		assert_eq!(term("|1| + |-2|, rest"), ", rest");
		assert_eq!(term("||-2||, rest"), ", rest");
		assert_eq!(term("|-|-2||, rest"), ", rest");
		assert_eq!(term("(1), rest"), ", rest");
		assert_eq!(term("a, rest"), ", rest");
		assert_eq!(term("1, rest"), ", rest");
		assert_eq!(term("true, rest"), ", rest");
		assert_eq!(term("false, rest"), ", rest");
		assert_eq!(term("#inf, rest"), ", rest");
		assert_eq!(term("#sup, rest"), ", rest");
		assert_eq!(term("f(1, 2), rest"), ", rest");
		assert_eq!(term("g(1 ** 2, 3 * 4, #inf), rest"), ", rest");
		assert_eq!(term("\"test\", rest"), ", rest");
		assert_eq!(term("X, rest"), ", rest");
		assert_eq!(term("Variable, rest"), ", rest");
		assert_eq!(term("f(\"test\", Variable), rest"), ", rest");
	}

	#[test]
	fn parse_whitespace()
	{
		assert_eq!(format_term("(a+b)*(c+d)"), "(a + b) * (c + d)");
		assert_eq!(format_term("( a + b ) * ( c + d )"), "(a + b) * (c + d)");
		assert_eq!(format_term("(  a  +  b  )  *  (  c  +  d  )"), "(a + b) * (c + d)");
		assert_eq!(format_term("(\ta\t+\tb\t)\t*\t(\tc\t+\td\t)"), "(a + b) * (c + d)");
		assert_eq!(format_term("(\na\n+\nb\n)\n*\n(\nc\n+\nd\n)"), "(a + b) * (c + d)");
		assert_eq!(format_term("( \t a \t + \t b \t ) \t * \t ( \t c \t + \t d \t )"), "(a + b) * (c + d)");
		assert_eq!(format_term("( \n a \n + \n b \n ) \n * \n ( \n c \n + \n d \n )"), "(a + b) * (c + d)");
		assert_eq!(format_term("f(\ta\t+\tb\t,\tc\t+\td\t)"), "f(a + b, c + d)");
		assert_eq!(format_term("f(\na\n+\nb\n,\nc\n+\nd\n)"), "f(a + b, c + d)");
		assert_eq!(format_term("f( \t a \t + \t b \t , \t c \t + \t d \t)"), "f(a + b, c + d)");
		assert_eq!(format_term("f( \n a \n + \n b \n , \n c \n + \n d \n)"), "f(a + b, c + d)");
		// TODO: test other operators
	}

	#[test]
	fn parse_function_primitive()
	{
		let function = |i| original::function(i, &Declarations::new(),
			&VariableDeclarationStackLayer::free()).unwrap().1;
		let function_remainder = |i| original::function(i, &Declarations::new(),
			&VariableDeclarationStackLayer::free()).unwrap().0;

		assert_eq!(function("s").declaration.name, "s");
		assert_eq!(function("s").declaration.arity, 0);
		assert_eq!(function_remainder("s"), "");
		assert_eq!(function("s ( )").declaration.name, "s");
		assert_eq!(function("s ( )").declaration.arity, 0);
		assert_eq!(function_remainder("s ( )"), "");
		assert_eq!(function("s ( 1 , 2 , 3 )").declaration.name, "s");
		assert_eq!(function("s ( 1 , 2 , 3 )").declaration.arity, 3);
		assert_eq!(function("s ( 1 , 2 , 3 )").arguments.remove(0), Term::integer(1));
		assert_eq!(function("s ( 1 , 2 , 3 )").arguments.remove(1), Term::integer(2));
		assert_eq!(function("s ( 1 , 2 , 3 )").arguments.remove(2), Term::integer(3));
		assert_eq!(function_remainder("s ( 1 , 2 , 3 )"), "");
		assert_eq!(function("s ( ), rest").declaration.name, "s");
		assert_eq!(function("s ( ), rest").declaration.arity, 0);
		assert_eq!(function_remainder("s ( ), rest"), ", rest");
		assert_eq!(function("s ( 1 , 2 , 3 ), rest").declaration.name, "s");
		assert_eq!(function("s ( 1 , 2 , 3 ), rest").declaration.arity, 3);
		assert_eq!(function_remainder("s ( 1 , 2 , 3 ), rest"), ", rest");
	}

	#[test]
	fn parse_variable_declaration()
	{
		let variable_declaration = |i| original::variable_declaration(i).unwrap().1;
		let variable_declaration_remainder = |i| original::variable_declaration(i).unwrap().0;

		assert_eq!(variable_declaration("X Rest").name, "X");
		assert_eq!(variable_declaration_remainder("X Rest"), " Rest");
		assert_eq!(variable_declaration("X, Rest").name, "X");
		assert_eq!(variable_declaration_remainder("X, Rest"), ", Rest");
		// Variable declarations parsed at different locations should not be considered equal
		assert_ne!(variable_declaration("X"), variable_declaration("X"));
		assert_eq!(variable_declaration("Variable_123 Rest").name, "Variable_123");
		assert_eq!(variable_declaration_remainder("Variable_123 Rest"), " Rest");

		let variable_declaration = original::variable_declaration;

		assert!(variable_declaration("0 Rest").is_err());
		assert!(variable_declaration("123_Asd Rest").is_err());
		assert!(variable_declaration("x Rest").is_err());
		assert!(variable_declaration("variable_123 Rest").is_err());
		assert!(variable_declaration("_ Rest").is_err());
		assert!(variable_declaration("_variable_123 Rest").is_err());
		assert!(variable_declaration(" ").is_err());
	}

	#[test]
	fn parse_variable_primitive()
	{
		let variable = |i| original::variable(i, &VariableDeclarationStackLayer::free()).unwrap().1;
		let variable_remainder = |i| original::variable(i,
			&VariableDeclarationStackLayer::free()).unwrap().0;

		assert_eq!(variable("X Rest").declaration.name, "X");
		assert_eq!(variable_remainder("X Rest"), " Rest");
		assert_eq!(variable("X, Rest").declaration.name, "X");
		assert_eq!(variable_remainder("X, Rest"), ", Rest");
		assert_eq!(variable("Variable_123 Rest").declaration.name, "Variable_123");
		assert_eq!(variable_remainder("Variable_123 Rest"), " Rest");

		let variable = |i| original::variable(i, &VariableDeclarationStackLayer::free());

		assert!(variable("0 Rest").is_err());
		assert!(variable("123_Asd Rest").is_err());
		assert!(variable("x Rest").is_err());
		assert!(variable("variable_123 Rest").is_err());
		assert!(variable("_ Rest").is_err());
		assert!(variable("_variable_123 Rest").is_err());
		assert!(variable(" ").is_err());

		let new_variable_declarations = |names: &[&str]| std::rc::Rc::new(names.iter()
			.map(|name| std::rc::Rc::new(VariableDeclaration::new(name.to_string())))
			.collect());

		let layer_1 = new_variable_declarations(&["A", "B", "X"]);
		let layer_2 = new_variable_declarations(&["C", "D", "X"]);
		let layer_3 = new_variable_declarations(&["E", "F", "Y"]);
		let layer_4 = new_variable_declarations(&["G", "H", "X"]);

		let v_0 = VariableDeclarationStackLayer::free();
		let variable = |i| original::variable(i, &v_0).unwrap().1;
		let number_of_free_variable_declarations =
			|| v_0.free_variable_declarations_do(|x| x.len());

		let x1 = variable("X");
		assert_eq!(number_of_free_variable_declarations(), 1);
		let x2 = variable("X");
		assert_eq!(number_of_free_variable_declarations(), 1);
		assert_eq!(x1.declaration, x2.declaration);
		let y1 = variable("Y");
		assert_eq!(number_of_free_variable_declarations(), 2);
		assert_ne!(x1.declaration, y1.declaration);
		assert_ne!(x2.declaration, y1.declaration);

		let v_1 = VariableDeclarationStackLayer::bound(&v_0, layer_1);
		let variable = |i| original::variable(i, &v_1).unwrap().1;

		let x3 = variable("X");
		assert_eq!(number_of_free_variable_declarations(), 2);
		assert_ne!(x1.declaration, x3.declaration);
		let x4 = variable("X");
		assert_eq!(number_of_free_variable_declarations(), 2);
		assert_eq!(x3.declaration, x4.declaration);
		let a1 = variable("A");
		assert_eq!(number_of_free_variable_declarations(), 2);
		assert_ne!(x3.declaration, a1.declaration);
		let y2 = variable("Y");
		assert_eq!(number_of_free_variable_declarations(), 2);
		assert_eq!(y1.declaration, y2.declaration);

		let v_2 = VariableDeclarationStackLayer::bound(&v_1, layer_2);
		let variable = |i| original::variable(i, &v_2).unwrap().1;

		let x5 = variable("X");
		assert_eq!(number_of_free_variable_declarations(), 2);
		assert_ne!(x1.declaration, x5.declaration);
		assert_ne!(x3.declaration, x5.declaration);
		let x6 = variable("X");
		assert_eq!(number_of_free_variable_declarations(), 2);
		assert_eq!(x5.declaration, x6.declaration);
		let a2 = variable("A");
		assert_eq!(number_of_free_variable_declarations(), 2);
		assert_eq!(a1.declaration, a2.declaration);

		let v_3 = VariableDeclarationStackLayer::bound(&v_2, layer_3);
		let variable = |i| original::variable(i, &v_3).unwrap().1;

		let x7 = variable("X");
		assert_eq!(number_of_free_variable_declarations(), 2);
		assert_eq!(x5.declaration, x7.declaration);
		let y3 = variable("Y");
		assert_eq!(number_of_free_variable_declarations(), 2);
		assert_ne!(y2.declaration, y3.declaration);

		let v_4 = VariableDeclarationStackLayer::bound(&v_3, layer_4);
		let variable = |i| original::variable(i, &v_4).unwrap().1;

		let x8 = variable("X");
		assert_eq!(number_of_free_variable_declarations(), 2);
		assert_ne!(x7.declaration, x8.declaration);
		let y4 = variable("Y");
		assert_eq!(number_of_free_variable_declarations(), 2);
		assert_eq!(y3.declaration, y4.declaration);
		let _ = variable("I");
		assert_eq!(number_of_free_variable_declarations(), 3);
	}
}
