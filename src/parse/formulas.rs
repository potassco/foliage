use nom::
{
	IResult,
	branch::alt,
	bytes::complete::tag,
	character::complete::multispace0,
	combinator::{cut, map, map_res},
	multi::{many1, separated_list1},
	sequence::{delimited, pair, preceded, terminated, tuple},
};

use super::{boolean, word_boundary};

pub fn predicate<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Predicate>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	map
	(
		|i| crate::parse::terms::function_or_predicate(i, d, v),
		|(name, arguments)|
		{
			let arguments = match arguments
			{
				Some(arguments) => arguments,
				None => vec![],
			};

			let declaration = d.find_or_create_predicate_declaration(name, arguments.len());

			crate::Predicate::new(declaration, arguments)
		},
	)(i)
}

fn not<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Formula>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
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
			|i| formula_precedence_2(i, d, v),
		),
		|x| crate::Formula::not(Box::new(x)),
	)(i)
}

fn and<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Formulas>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	map_res
	(
		separated_list1
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
			|i| formula_precedence_2(i, d, v),
		),
		|arguments| -> Result<_, (_, _)>
		{
			if arguments.len() >= 2
			{
				Ok(arguments.into_iter().collect())
			}
			else
			{
				// TODO: return more appropriate error type
				Err(nom::error::make_error(i, nom::error::ErrorKind::Many1))
			}
		}
	)(i)
}

fn or<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Formulas>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	map_res
	(
		separated_list1
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
			|i| formula_precedence_3(i, d, v),
		),
		|arguments| -> Result<_, (_, _)>
		{
			if arguments.len() >= 2
			{
				Ok(arguments.into_iter().collect())
			}
			else
			{
				// TODO: return more appropriate error type
				Err(nom::error::make_error(i, nom::error::ErrorKind::Many1))
			}
		}
	)(i)
}

fn implies_left_to_right<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Formula>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	map
	(
		pair
		(
			many1
			(
				terminated
				(
					|i| formula_precedence_4(i, d, v),
					delimited
					(
						multispace0,
						tag("->"),
						multispace0,
					),
				)
			),
			|i| formula_precedence_4(i, d, v),
		),
		|(arguments, last_argument)| arguments.into_iter().rev().fold(last_argument,
			|accumulator, argument|
				crate::Formula::implies(crate::ImplicationDirection::LeftToRight,
					Box::new(argument), Box::new(accumulator)))
	)(i)
}

fn implies_right_to_left<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Formula>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	map
	(
		pair
		(
			|i| formula_precedence_4(i, d, v),
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
					|i| formula_precedence_4(i, d, v),
				)
			),
		),
		|(first_argument, arguments)| arguments.into_iter().fold(first_argument,
			|accumulator, argument|
				crate::Formula::implies(crate::ImplicationDirection::RightToLeft,
					Box::new(argument), Box::new(accumulator)))
	)(i)
}

fn if_and_only_if<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Formulas>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	map_res
	(
		separated_list1
		(
			delimited
			(
				multispace0,
				tag("<->"),
				multispace0,
			),
			|i| formula_precedence_5(i, d, v),
		),
		|arguments| -> Result<_, (_, _)>
		{
			if arguments.len() >= 2
			{
				Ok(arguments.into_iter().collect())
			}
			else
			{
				// TODO: return more appropriate error type
				Err(nom::error::make_error(i, nom::error::ErrorKind::Many1))
			}
		}
	)(i)
}

fn quantified_formula<'i, 'b, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer,
	keyword: &'b str)
	-> IResult<&'i str, crate::QuantifiedFormula>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
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
							separated_list1
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

				let bound_variable_declarations = crate::VariableDeclarationStackLayer::bound(v,
					std::rc::Rc::clone(&variable_declarations));

				let (i, argument) = formula_precedence_0(i, d, &bound_variable_declarations)?;

				Ok((i, crate::QuantifiedFormula::new(variable_declarations, Box::new(argument))))
			}
		),
	)(i)
}

fn compare<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Compare>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	map
	(
		tuple
		((
			|i| crate::parse::term(i, d, v),
			delimited
			(
				multispace0,
				alt
				((
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
			|i| crate::parse::term(i, d, v),
		)),
		|(left, operator, right)|
		{
			crate::Compare::new(operator, Box::new(left), Box::new(right))
		}
	)(i)
}

fn exists<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::QuantifiedFormula>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	quantified_formula(i, d, v, "exists")
}

fn for_all<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::QuantifiedFormula>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	quantified_formula(i, d, v, "forall")
}

fn formula_parenthesized<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Formula>
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
		|i| formula(i, d, v),
		preceded
		(
			multispace0,
			tag(")"),
		),
	)(i)
}

fn formula_precedence_0<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Formula>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
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
			|i| predicate(i, d, v),
			crate::Formula::Predicate,
		),
		map
		(
			|i| compare(i, d, v),
			crate::Formula::Compare,
		),
		|i| formula_parenthesized(i, d, v),
	))(i)
}

fn formula_precedence_1<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Formula>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	alt
	((
		map
		(
			|i| exists(i, d, v),
			crate::Formula::Exists,
		),
		map
		(
			|i| for_all(i, d, v),
			crate::Formula::ForAll,
		),
		|i| formula_precedence_0(i, d, v),
	))(i)
}

fn formula_precedence_2<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Formula>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	alt
	((
		|i| not(i, d, v),
		|i| formula_precedence_1(i, d, v),
	))(i)
}

fn formula_precedence_3<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Formula>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	alt
	((
		map
		(
			|i| and(i, d, v),
			crate::Formula::And,
		),
		|i| formula_precedence_2(i, d, v),
	))(i)
}

fn formula_precedence_4<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Formula>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	alt
	((
		map
		(
			|i| or(i, d, v),
			crate::Formula::Or,
		),
		|i| formula_precedence_3(i, d, v),
	))(i)
}

fn formula_precedence_5<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Formula>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	alt
	((
		|i| implies_left_to_right(i, d, v),
		|i| implies_right_to_left(i, d, v),
		|i| formula_precedence_4(i, d, v),
	))(i)
}

fn formula_precedence_6<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Formula>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	alt
	((
		map
		(
			|i| if_and_only_if(i, d, v),
			crate::Formula::IfAndOnlyIf,
		),
		|i| formula_precedence_5(i, d, v),
	))(i)
}

pub fn formula<'i, 'v, D>(i: &'i str, d: &D, v: &'v crate::VariableDeclarationStackLayer)
	-> IResult<&'i str, crate::Formula>
where
	D: crate::FindOrCreateFunctionDeclaration + crate::FindOrCreatePredicateDeclaration
{
	formula_precedence_6(i, d, v)
}

#[cfg(test)]
mod tests
{
	use crate::{Formula, ImplicationDirection, Term, VariableDeclarationStackLayer};
	use crate::parse::formulas as original;
	use crate::utils::*;

	fn formula(i: &str) -> Formula
	{
		original::formula(i, &Declarations::new(), &VariableDeclarationStackLayer::free())
			.unwrap().1
	}

	fn formula_remainder(i: &str) -> &str
	{
		original::formula(i, &Declarations::new(), &VariableDeclarationStackLayer::free())
			.unwrap().0
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

		let formula = |i| original::formula(i, &Declarations::new(),
			&VariableDeclarationStackLayer::free());

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

		let formula = |i| original::formula(i, &Declarations::new(),
			&VariableDeclarationStackLayer::free());

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
		assert_eq!(formula_as_implies("a -> b").direction, ImplicationDirection::LeftToRight);

		assert_eq!(as_predicate(*formula_as_implies("a <- b").antecedent).declaration.name, "b");
		assert_eq!(as_predicate(*formula_as_implies("a <- b").implication).declaration.name, "a");
		assert_eq!(formula_as_implies("a <- b").direction, ImplicationDirection::RightToLeft);

		assert_eq!(format_formula("(a -> b -> c)"), "a -> b -> c");
		assert_eq!(format_formula("(a -> (b -> c))"), "a -> b -> c");
		assert_eq!(format_formula("((a -> b) -> c)"), "(a -> b) -> c");
	}

	#[test]
	fn parse_predicate()
	{
		let predicate = |i| original::predicate(i, &Declarations::new(),
			&VariableDeclarationStackLayer::free()).unwrap().1;
		let predicate_remainder = |i| original::predicate(i, &Declarations::new(),
			&VariableDeclarationStackLayer::free()).unwrap().0;

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
		let exists = |i| original::exists(i, &Declarations::new(),
			&VariableDeclarationStackLayer::free());

		assert_eq!(exists("exists X (p(X, Y, X, Y)), rest")
			.map(|(i, x)| (i, x.parameters.len())),
			Ok((", rest", 1)));
		assert_eq!(exists("exists X p(X, Y, X, Y), rest")
			.map(|(i, x)| (i, x.parameters.len())),
			Ok((", rest", 1)));
		assert!(exists("exists (p(X, Y, X, Y)), rest").is_err());
		assert!(exists("exists X, rest").is_err());
		assert!(exists("exists X (), rest").is_err());
		assert!(exists("exists X (, true), rest").is_err());
		assert!(exists("exists X (true, ), rest").is_err());
		assert!(exists("exists X (true false), rest").is_err());
		assert!(exists("exists X (true), rest").is_ok());
		assert!(exists("exists X p(X), rest").is_ok());
	}

	#[test]
	fn parse_formula()
	{
		// TODO: refactor
		formula("exists X, Y (p(X, Y, X, Y) and X < Y and q(X) <-> r(X)), rest");
	}
}
