use nom::
{
	IResult,
	branch::alt,
	bytes::complete::tag,
	character::complete::multispace0,
	combinator::{cut, map, map_res},
	multi::{many1, separated_list},
	sequence::{delimited, pair, preceded, terminated, tuple},
};

use super::{Declarations, boolean, word_boundary};

pub fn predicate<'i>(i: &'i str, d: &Declarations) -> IResult<&'i str, crate::Predicate>
{
	map
	(
		|i| crate::parse::terms::function_or_predicate(i, d),
		|(name, arguments)|
		{
			let arguments = match arguments
			{
				Some(arguments) => arguments,
				None => vec![],
			};

			let mut predicate_declarations = d.predicate_declarations.borrow_mut();

			let declaration = match predicate_declarations.iter()
				.find(|x| x.name == name && x.arity == arguments.len())
			{
				Some(declaration) => std::rc::Rc::clone(&declaration),
				None =>
				{
					let declaration = crate::PredicateDeclaration
					{
						name: name.to_string(),
						arity: arguments.len(),
					};
					let declaration = std::rc::Rc::new(declaration);

					predicate_declarations.insert(std::rc::Rc::clone(&declaration));

					declaration
				},
			};

			crate::Predicate::new(declaration, arguments)
		},
	)(i)
}

fn not<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::Formula>
{
	map
	(
		preceded
		(
			terminated
			(
				tag("not"),
				multispace0,
			),
			|i| formula_precedence_2(i, d),
		),
		|x| crate::Formula::not(Box::new(x)),
	)(i)
}

fn and<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::Formulas>
{
	map_res
	(
		separated_list
		(
			delimited
			(
				multispace0,
				terminated
				(
					tag("and"),
					word_boundary,
				),
				multispace0,
			),
			|i| formula_precedence_2(i, d),
		),
		|arguments| -> Result<_, (_, _)>
		{
			if arguments.len() >= 2
			{
				Ok(arguments.into_iter().collect())
			}
			else
			{
				Err(nom::error::make_error(i, nom::error::ErrorKind::Many1))
			}
		}
	)(i)
}

fn or<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::Formulas>
{
	map_res
	(
		separated_list
		(
			delimited
			(
				multispace0,
				terminated
				(
					tag("or"),
					word_boundary,
				),
				multispace0,
			),
			|i| formula_precedence_3(i, d),
		),
		|arguments| -> Result<_, (_, _)>
		{
			if arguments.len() >= 2
			{
				Ok(arguments.into_iter().collect())
			}
			else
			{
				Err(nom::error::make_error(i, nom::error::ErrorKind::Many1))
			}
		}
	)(i)
}

fn implies_left_to_right<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::Formula>
{
	map
	(
		pair
		(
			many1
			(
				terminated
				(
					|i| formula_precedence_4(i, d),
					delimited
					(
						multispace0,
						tag("->"),
						multispace0,
					),
				)
			),
			|i| formula_precedence_4(i, d),
		),
		|(arguments, last_argument)| arguments.into_iter().rev().fold(last_argument,
			|accumulator, argument|
				crate::Formula::implies(crate::ImplicationDirection::LeftToRight,
					Box::new(argument), Box::new(accumulator)))
	)(i)
}

fn implies_right_to_left<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::Formula>
{
	map
	(
		pair
		(
			|i| formula_precedence_4(i, d),
			many1
			(
				preceded
				(
					delimited
					(
						multispace0,
						tag("<-"),
						multispace0,
					),
					|i| formula_precedence_4(i, d),
				)
			),
		),
		|(first_argument, arguments)| arguments.into_iter().fold(first_argument,
			|accumulator, argument|
				crate::Formula::implies(crate::ImplicationDirection::RightToLeft,
					Box::new(argument), Box::new(accumulator)))
	)(i)
}

fn if_and_only_if<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::Formulas>
{
	map_res
	(
		separated_list
		(
			delimited
			(
				multispace0,
				tag("<->"),
				multispace0,
			),
			|i| formula_precedence_5(i, d),
		),
		|arguments| -> Result<_, (_, _)>
		{
			if arguments.len() >= 2
			{
				Ok(arguments.into_iter().collect())
			}
			else
			{
				Err(nom::error::make_error(i, nom::error::ErrorKind::Many1))
			}
		}
	)(i)
}

fn quantified_formula<'a, 'b>(i: &'a str, d: &Declarations, keyword: &'b str)
	-> IResult<&'a str, crate::QuantifiedFormula>
{
	preceded
	(
		terminated
		(
			tag(keyword),
			word_boundary,
		),
		cut
		(
			|i|
			{
				let (i, variable_declarations) =
					map
					(
						delimited
						(
							multispace0,
							separated_list
							(
								delimited
								(
									multispace0,
									tag(","),
									multispace0,
								),
								map
								(
									crate::parse::terms::variable_declaration,
									std::rc::Rc::new,
								),
							),
							multispace0,
						),
						std::rc::Rc::new,
					)(i)?;

				if variable_declarations.is_empty()
				{
					return Err(nom::Err::Failure((i, nom::error::ErrorKind::Many1)));
				}

				let _guard = crate::VariableDeclarationStack::push(&d.variable_declaration_stack,
					std::rc::Rc::clone(&variable_declarations));

				let (i, argument) = formula_precedence_0(i, d)?;

				Ok((i, crate::QuantifiedFormula::new(variable_declarations, Box::new(argument))))
			}
		),
	)(i)
}

fn compare<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::Compare>
{
	map
	(
		tuple
		((
			|i| crate::parse::term(i, d),
			delimited
			(
				multispace0,
				alt
				((
					map
					(
						tag(">"),
						|_| crate::ComparisonOperator::Greater,
					),
					map
					(
						tag("<"),
						|_| crate::ComparisonOperator::Less,
					),
					map
					(
						tag("<="),
						|_| crate::ComparisonOperator::LessOrEqual,
					),
					map
					(
						tag(">="),
						|_| crate::ComparisonOperator::GreaterOrEqual,
					),
					map
					(
						tag("!="),
						|_| crate::ComparisonOperator::NotEqual,
					),
					map
					(
						tag("="),
						|_| crate::ComparisonOperator::Equal,
					),
				)),
				multispace0,
			),
			|i| crate::parse::term(i, d),
		)),
		|(left, operator, right)|
		{
			crate::Compare::new(operator, Box::new(left), Box::new(right))
		}
	)(i)
}

fn exists<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::QuantifiedFormula>
{
	quantified_formula(i, d, "exists")
}

fn for_all<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::QuantifiedFormula>
{
	quantified_formula(i, d, "forall")
}

fn formula_parenthesized<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::Formula>
{
	delimited
	(
		terminated
		(
			tag("("),
			multispace0,
		),
		|i| formula(i, d),
		preceded
		(
			multispace0,
			tag(")"),
		),
	)(i)
}

fn formula_precedence_0<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::Formula>
{
	alt
	((
		map
		(
			boolean,
			crate::Formula::Boolean,
		),
		map
		(
			|i| predicate(i, d),
			crate::Formula::Predicate,
		),
		map
		(
			|i| compare(i, d),
			crate::Formula::Compare,
		),
		|i| formula_parenthesized(i, d),
	))(i)
}

fn formula_precedence_1<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::Formula>
{
	alt
	((
		map
		(
			|i| exists(i, d),
			crate::Formula::Exists,
		),
		map
		(
			|i| for_all(i, d),
			crate::Formula::ForAll,
		),
		|i| formula_precedence_0(i, d),
	))(i)
}

fn formula_precedence_2<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::Formula>
{
	alt
	((
		|i| not(i, d),
		|i| formula_precedence_1(i, d),
	))(i)
}

fn formula_precedence_3<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::Formula>
{
	alt
	((
		map
		(
			|i| and(i, d),
			crate::Formula::And,
		),
		|i| formula_precedence_2(i, d),
	))(i)
}

fn formula_precedence_4<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::Formula>
{
	alt
	((
		map
		(
			|i| or(i, d),
			crate::Formula::Or,
		),
		|i| formula_precedence_3(i, d),
	))(i)
}

fn formula_precedence_5<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::Formula>
{
	alt
	((
		|i| implies_left_to_right(i, d),
		|i| implies_right_to_left(i, d),
		|i| formula_precedence_4(i, d),
	))(i)
}

fn formula_precedence_6<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::Formula>
{
	alt
	((
		map
		(
			|i| if_and_only_if(i, d),
			crate::Formula::IfAndOnlyIf,
		),
		|i| formula_precedence_5(i, d),
	))(i)
}

pub fn formula<'a>(i: &'a str, d: &Declarations) -> IResult<&'a str, crate::Formula>
{
	formula_precedence_6(i, d)
}

#[cfg(test)]
mod tests
{
	use crate::parse::formulas::*;
	use crate::parse::formulas as original;
	use crate::{Formula, Term};

	fn formula(i: &str) -> Formula
	{
		original::formula(i, &Declarations::new()).unwrap().1
	}

	fn formula_remainder(i: &str) -> &str
	{
		original::formula(i, &Declarations::new()).unwrap().0
	}

	fn format_formula(i: &str) -> String
	{
		format!("{}", formula(i))
	}

	#[test]
	fn parse_boolean()
	{
		assert_eq!(formula("true"), Formula::true_());
		assert_eq!(formula("false"), Formula::false_());
	}

	#[test]
	fn parse_precedence()
	{
		assert_eq!(format_formula("a -> b -> c <-> d -> e -> f"), "a -> b -> c <-> d -> e -> f");
		assert_eq!(format_formula("(a -> b -> c) <-> (d -> e -> f)"), "a -> b -> c <-> d -> e -> f");
		assert_eq!(format_formula("a <- b <- c <-> d <- e <- f"), "a <- b <- c <-> d <- e <- f");
		assert_eq!(format_formula("(a <- b <- c) <-> (d <- e <- f)"), "a <- b <- c <-> d <- e <- f");
	}

	#[test]
	fn parse_exists()
	{
		let formula_as_exists = |i| match formula(i)
		{
			Formula::Exists(exists) => exists,
			_ => panic!("expected existentially quantified formula"),
		};

		let as_predicate = |x| match x
		{
			Formula::Predicate(arguments) => arguments,
			_ => panic!("expected predicate"),
		};

		assert_eq!(format_formula("exists  X , Y , Z  ( p )"), "exists X, Y, Z p");
		assert_eq!(formula_as_exists("exists  X , Y , Z  ( p )").parameters.len(), 3);
		assert_eq!(as_predicate(*formula_as_exists("exists  X , Y , Z  ( p )").argument)
			.declaration.name, "p");
	}

	#[test]
	fn parse_and()
	{
		let formula_as_and = |i| match formula(i)
		{
			Formula::And(arguments) => arguments,
			_ => panic!("expected conjunction"),
		};

		let as_predicate = |x| match x
		{
			Formula::Predicate(arguments) => arguments,
			_ => panic!("expected predicate"),
		};

		assert_eq!(format_formula("(true  and  false)"), "true and false");
		assert_eq!(formula_as_and("(true and false)").len(), 2);
		assert_eq!(formula_as_and("(true and false)").remove(0), Formula::true_());
		assert_eq!(formula_as_and("(true and false)").remove(1), Formula::false_());
		// The order of elements is retained
		assert_ne!(formula("(true and false)"), formula("false and true"));
		assert_eq!(format_formula("(p  and  q  and  r  and  s)"), "p and q and r and s");
		assert_eq!(
			as_predicate(formula_as_and("(p and q and r and s)").remove(0)).declaration.name, "p");
		assert_eq!(
			as_predicate(formula_as_and("(p and q and r and s)").remove(3)).declaration.name, "s");

		let formula = |i| original::formula(i, &Declarations::new());

		// Malformed formulas shouldn’t be accepted
		assert!(formula("and").is_err());
		assert!(formula("and p").is_err());
		assert_eq!(formula_remainder("p and"), " and");
		assert_eq!(formula_remainder("p andq"), " andq");
		assert_eq!(formula_remainder("p and q and"), " and");
		assert_eq!(formula_remainder("p and q andq"), " andq");
		assert!(formula("(p and) q").is_err());
		assert_eq!(formula_remainder("p (and q)"), " (and q)");
	}

	#[test]
	fn parse_or()
	{
		let formula_as_or = |i| match formula(i)
		{
			Formula::Or(arguments) => arguments,
			_ => panic!("expected disjunction"),
		};

		let as_predicate = |x| match x
		{
			Formula::Predicate(arguments) => arguments,
			_ => panic!("expected predicate"),
		};

		assert_eq!(format_formula("(true  or  false)"), "true or false");
		assert_eq!(formula_as_or("(true or false)").len(), 2);
		assert_eq!(formula_as_or("(true or false)").remove(0), Formula::true_());
		assert_eq!(formula_as_or("(true or false)").remove(1), Formula::false_());
		// The order of elements is retained
		assert_ne!(formula("(true or false)"), formula("false or true)"));
		assert_eq!(format_formula("(p  or  q  or  r  or  s)"), "p or q or r or s");
		assert_eq!(
			as_predicate(formula_as_or("(p or q or r or s)").remove(0)).declaration.name, "p");
		assert_eq!(
			as_predicate(formula_as_or("(p or q or r or s)").remove(3)).declaration.name, "s");

		let formula = |i| original::formula(i, &Declarations::new());

		// Malformed formulas shouldn’t be accepted
		assert!(formula("or").is_err());
		assert!(formula("or p").is_err());
		assert_eq!(formula_remainder("p or"), " or");
		assert_eq!(formula_remainder("p orq"), " orq");
		assert_eq!(formula_remainder("p or q or"), " or");
		assert_eq!(formula_remainder("p or q orq"), " orq");
		assert!(formula("(p or) q").is_err());
		assert_eq!(formula_remainder("p (or q)"), " (or q)");
	}

	#[test]
	fn parse_implies()
	{
		let formula_as_implies = |i| match formula(i)
		{
			Formula::Implies(implies) => implies,
			_ => panic!("expected implication"),
		};

		let as_predicate = |x| match x
		{
			Formula::Predicate(arguments) => arguments,
			_ => panic!("expected predicate"),
		};

		assert_eq!(as_predicate(*formula_as_implies("a -> b").antecedent).declaration.name, "a");
		assert_eq!(as_predicate(*formula_as_implies("a -> b").implication).declaration.name, "b");
		assert_eq!(formula_as_implies("a -> b").direction,
			crate::ImplicationDirection::LeftToRight);

		assert_eq!(as_predicate(*formula_as_implies("a <- b").antecedent).declaration.name, "b");
		assert_eq!(as_predicate(*formula_as_implies("a <- b").implication).declaration.name, "a");
		assert_eq!(formula_as_implies("a <- b").direction,
			crate::ImplicationDirection::RightToLeft);

		assert_eq!(format_formula("(a -> b -> c)"), "a -> b -> c");
		assert_eq!(format_formula("(a -> (b -> c))"), "a -> b -> c");
		assert_eq!(format_formula("((a -> b) -> c)"), "(a -> b) -> c");
	}

	#[test]
	fn parse_predicate()
	{
		let predicate = |i| original::predicate(i, &Declarations::new()).unwrap().1;
		let predicate_remainder = |i| original::predicate(i, &Declarations::new()).unwrap().0;

		assert_eq!(predicate("s").declaration.name, "s");
		assert_eq!(predicate("s").declaration.arity, 0);
		assert_eq!(predicate_remainder("s"), "");
		assert_eq!(predicate("s ( )").declaration.name, "s");
		assert_eq!(predicate("s ( )").declaration.arity, 0);
		assert_eq!(predicate_remainder("s ( )"), "");
		assert_eq!(predicate("s ( 1 , 2 , 3 )").declaration.name, "s");
		assert_eq!(predicate("s ( 1 , 2 , 3 )").declaration.arity, 3);
		assert_eq!(predicate("s ( 1 , 2 , 3 )").arguments.remove(0), Term::integer(1));
		assert_eq!(predicate("s ( 1 , 2 , 3 )").arguments.remove(1), Term::integer(2));
		assert_eq!(predicate("s ( 1 , 2 , 3 )").arguments.remove(2), Term::integer(3));
		assert_eq!(predicate_remainder("s ( 1 , 2 , 3 )"), "");
		assert_eq!(predicate_remainder("s ( 1 , 2 , 3 )"), "");
		assert_eq!(predicate("s ( ), rest").declaration.name, "s");
		assert_eq!(predicate("s ( ), rest").declaration.arity, 0);
		assert_eq!(predicate_remainder("s ( ), rest"), ", rest");
		assert_eq!(predicate("s ( 1 , 2 , 3 ), rest").declaration.name, "s");
		assert_eq!(predicate("s ( 1 , 2 , 3 ), rest").declaration.arity, 3);
		assert_eq!(predicate_remainder("s ( 1 , 2 , 3 ), rest"), ", rest");
	}

	#[test]
	fn parse_exists_primitive()
	{
		assert_eq!(exists("exists X (p(X, Y, X, Y)), rest", &Declarations::new())
			.map(|(i, x)| (i, x.parameters.len())),
			Ok((", rest", 1)));
		assert_eq!(exists("exists X p(X, Y, X, Y), rest", &Declarations::new())
			.map(|(i, x)| (i, x.parameters.len())),
			Ok((", rest", 1)));
		assert!(exists("exists (p(X, Y, X, Y)), rest", &Declarations::new()).is_err());
		assert!(exists("exists X, rest", &Declarations::new()).is_err());
		assert!(exists("exists X (), rest", &Declarations::new()).is_err());
		assert!(exists("exists X (, true), rest", &Declarations::new()).is_err());
		assert!(exists("exists X (true, ), rest", &Declarations::new()).is_err());
		assert!(exists("exists X (true false), rest", &Declarations::new()).is_err());
		assert!(exists("exists X (true), rest", &Declarations::new()).is_ok());
		assert!(exists("exists X p(X), rest", &Declarations::new()).is_ok());
	}

	#[test]
	fn parse_formula()
	{
		// TODO: refactor
		formula("exists X, Y (p(X, Y, X, Y) and X < Y and q(X) <-> r(X)), rest");
	}
}
