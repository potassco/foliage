// Operators

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum BinaryOperator
{
	Add,
	Subtract,
	Multiply,
	Divide,
	Modulo,
	Exponentiate,
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum ComparisonOperator
{
	Greater,
	Less,
	LessOrEqual,
	GreaterOrEqual,
	NotEqual,
	Equal,
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum UnaryOperator
{
	AbsoluteValue,
	Negative,
}

// ImplicationDirection

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ImplicationDirection
{
	LeftToRight,
	RightToLeft,
}

// Primitives

#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FunctionDeclaration
{
	pub name: String,
	pub arity: usize,
}

impl FunctionDeclaration
{
	pub fn new(name: String, arity: usize) -> Self
	{
		Self
		{
			name,
			arity,
		}
	}
}

pub type FunctionDeclarations = std::collections::BTreeSet<std::rc::Rc<FunctionDeclaration>>;

#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct PredicateDeclaration
{
	pub name: String,
	pub arity: usize,
}

impl PredicateDeclaration
{
	pub fn new(name: String, arity: usize) -> Self
	{
		Self
		{
			name,
			arity,
		}
	}
}

pub type PredicateDeclarations = std::collections::BTreeSet<std::rc::Rc<PredicateDeclaration>>;

pub struct VariableDeclaration
{
	pub name: String,
}

impl std::cmp::PartialEq for VariableDeclaration
{
	#[inline(always)]
	fn eq(&self, other: &VariableDeclaration) -> bool
	{
		let l = self as *const VariableDeclaration;
		let r = other as *const VariableDeclaration;

		l.eq(&r)
	}
}

impl std::cmp::Eq for VariableDeclaration
{
}

impl std::cmp::PartialOrd for VariableDeclaration
{
	#[inline(always)]
	fn partial_cmp(&self, other: &VariableDeclaration) -> Option<std::cmp::Ordering>
	{
		let l = self as *const VariableDeclaration;
		let r = other as *const VariableDeclaration;

		l.partial_cmp(&r)
	}
}

impl std::cmp::Ord for VariableDeclaration
{
	#[inline(always)]
	fn cmp(&self, other: &VariableDeclaration) -> std::cmp::Ordering
	{
		let l = self as *const VariableDeclaration;
		let r = other as *const VariableDeclaration;

		l.cmp(&r)
	}
}

impl VariableDeclaration
{
	pub fn new(name: String) -> Self
	{
		Self
		{
			name,
		}
	}
}

pub type VariableDeclarations = Vec<std::rc::Rc<VariableDeclaration>>;

// Terms

pub struct BinaryOperation
{
	pub operator: BinaryOperator,
	pub left: Box<Term>,
	pub right: Box<Term>,
}

impl BinaryOperation
{
	pub fn new(operator: BinaryOperator, left: Box<Term>, right: Box<Term>) -> Self
	{
		Self
		{
			operator,
			left,
			right,
		}
	}
}

pub struct Function
{
	pub declaration: std::rc::Rc<FunctionDeclaration>,
	pub arguments: Terms,
}

impl Function
{
	pub fn new(declaration: std::rc::Rc<FunctionDeclaration>, arguments: Terms) -> Self
	{
		assert_eq!(declaration.arity, arguments.len(),
			"function has a different number of arguments then declared");

		Self
		{
			declaration,
			arguments,
		}
	}
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum SpecialInteger
{
	Infimum,
	Supremum,
}

pub struct UnaryOperation
{
	pub operator: UnaryOperator,
	pub argument: Box<Term>,
}

impl UnaryOperation
{
	pub fn new(operator: UnaryOperator, argument: Box<Term>) -> Self
	{
		Self
		{
			operator,
			argument,
		}
	}
}

pub struct Variable
{
	pub declaration: std::rc::Rc<VariableDeclaration>,
}

impl Variable
{
	pub fn new(declaration: std::rc::Rc<VariableDeclaration>) -> Self
	{
		Self
		{
			declaration,
		}
	}
}

// Formulas

pub struct Compare
{
	pub operator: ComparisonOperator,
	pub left: Box<Term>,
	pub right: Box<Term>,
}

impl Compare
{
	pub fn new(operator: ComparisonOperator, left: Box<Term>, right: Box<Term>) -> Self
	{
		Self
		{
			operator,
			left,
			right,
		}
	}
}

pub struct QuantifiedFormula
{
	pub parameters: std::rc::Rc<VariableDeclarations>,
	pub argument: Box<Formula>,
}

impl QuantifiedFormula
{
	pub fn new(parameters: std::rc::Rc<VariableDeclarations>, argument: Box<Formula>) -> Self
	{
		Self
		{
			parameters,
			argument,
		}
	}
}

pub struct Implies
{
	pub direction: ImplicationDirection,
	pub antecedent: Box<Formula>,
	pub implication: Box<Formula>,
}

impl Implies
{
	pub fn new(direction: ImplicationDirection, antecedent: Box<Formula>, implication: Box<Formula>)
		-> Self
	{
		Self
		{
			direction,
			antecedent,
			implication,
		}
	}
}

pub struct Predicate
{
	pub declaration: std::rc::Rc<PredicateDeclaration>,
	pub arguments: Terms,
}

impl Predicate
{
	pub fn new(declaration: std::rc::Rc<PredicateDeclaration>, arguments: Terms) -> Self
	{
		assert_eq!(declaration.arity, arguments.len(),
			"predicate has a different number of arguments then declared");

		Self
		{
			declaration,
			arguments,
		}
	}
}

// Variants

pub enum Term
{
	BinaryOperation(BinaryOperation),
	Boolean(bool),
	Function(Function),
	Integer(i32),
	SpecialInteger(SpecialInteger),
	String(String),
	UnaryOperation(UnaryOperation),
	Variable(Variable),
}

pub type Terms = Vec<Term>;

impl Term
{
	pub fn absolute_value(argument: Box<Term>) -> Self
	{
		Self::unary_operation(UnaryOperator::AbsoluteValue, argument)
	}

	pub fn add(left: Box<Term>, right: Box<Term>) -> Self
	{
		Self::binary_operation(BinaryOperator::Add, left, right)
	}

	pub fn binary_operation(operator: BinaryOperator, left: Box<Term>, right: Box<Term>) -> Self
	{
		Self::BinaryOperation(BinaryOperation::new(operator, left, right))
	}

	pub fn boolean(value: bool) -> Self
	{
		Self::Boolean(value)
	}

	pub fn divide(left: Box<Term>, right: Box<Term>) -> Self
	{
		Self::binary_operation(BinaryOperator::Divide, left, right)
	}

	pub fn exponentiate(left: Box<Term>, right: Box<Term>) -> Self
	{
		Self::binary_operation(BinaryOperator::Exponentiate, left, right)
	}

	pub fn false_() -> Self
	{
		Self::boolean(false)
	}

	pub fn function(declaration: std::rc::Rc<FunctionDeclaration>, arguments: Terms) -> Self
	{
		Self::Function(Function::new(declaration, arguments))
	}

	pub fn infimum() -> Self
	{
		Self::special_integer(SpecialInteger::Infimum)
	}

	pub fn integer(value: i32) -> Self
	{
		Self::Integer(value)
	}

	pub fn modulo(left: Box<Term>, right: Box<Term>) -> Self
	{
		Self::binary_operation(BinaryOperator::Modulo, left, right)
	}

	pub fn multiply(left: Box<Term>, right: Box<Term>) -> Self
	{
		Self::binary_operation(BinaryOperator::Multiply, left, right)
	}

	pub fn negative(argument: Box<Term>) -> Self
	{
		Self::unary_operation(UnaryOperator::Negative, argument)
	}

	pub fn special_integer(value: SpecialInteger) -> Self
	{
		Self::SpecialInteger(value)
	}

	pub fn string(value: String) -> Self
	{
		Self::String(value)
	}

	pub fn subtract(left: Box<Term>, right: Box<Term>) -> Self
	{
		Self::binary_operation(BinaryOperator::Subtract, left, right)
	}

	pub fn supremum() -> Self
	{
		Self::special_integer(SpecialInteger::Supremum)
	}

	pub fn true_() -> Self
	{
		Self::boolean(true)
	}

	pub fn unary_operation(operator: UnaryOperator, argument: Box<Term>) -> Self
	{
		Self::UnaryOperation(UnaryOperation::new(operator, argument))
	}

	pub fn variable(declaration: std::rc::Rc<VariableDeclaration>) -> Self
	{
		Self::Variable(Variable::new(declaration))
	}
}

pub enum Formula
{
	And(Formulas),
	Boolean(bool),
	Compare(Compare),
	Exists(QuantifiedFormula),
	ForAll(QuantifiedFormula),
	IfAndOnlyIf(Formulas),
	Implies(Implies),
	Not(Box<Formula>),
	Or(Formulas),
	Predicate(Predicate),
}

pub type Formulas = Vec<Formula>;

impl Formula
{
	pub fn and(arguments: Formulas) -> Self
	{
		Self::And(arguments)
	}

	pub fn boolean(value: bool) -> Self
	{
		Self::Boolean(value)
	}

	pub fn compare(operator: ComparisonOperator, left: Box<Term>, right: Box<Term>) -> Self
	{
		Self::Compare(Compare::new(operator, left, right))
	}

	pub fn exists(parameters: std::rc::Rc<VariableDeclarations>, argument: Box<Formula>) -> Self
	{
		Self::Exists(QuantifiedFormula::new(parameters, argument))
	}

	pub fn equal(left: Box<Term>, right: Box<Term>) -> Self
	{
		Self::compare(ComparisonOperator::Equal, left, right)
	}

	pub fn false_() -> Self
	{
		Self::boolean(false)
	}

	pub fn for_all(parameters: std::rc::Rc<VariableDeclarations>, argument: Box<Formula>) -> Self
	{
		Self::ForAll(QuantifiedFormula::new(parameters, argument))
	}

	pub fn greater(left: Box<Term>, right: Box<Term>) -> Self
	{
		Self::compare(ComparisonOperator::Greater, left, right)
	}

	pub fn greater_or_equal(left: Box<Term>, right: Box<Term>) -> Self
	{
		Self::compare(ComparisonOperator::GreaterOrEqual, left, right)
	}

	pub fn if_and_only_if(arguments: Formulas) -> Self
	{
		Self::IfAndOnlyIf(arguments)
	}

	pub fn implies(direction: ImplicationDirection, antecedent: Box<Formula>,
		consequent: Box<Formula>) -> Self
	{
		Self::Implies(Implies::new(direction, antecedent, consequent))
	}

	pub fn less(left: Box<Term>, right: Box<Term>) -> Self
	{
		Self::compare(ComparisonOperator::Less, left, right)
	}

	pub fn less_or_equal(left: Box<Term>, right: Box<Term>) -> Self
	{
		Self::compare(ComparisonOperator::LessOrEqual, left, right)
	}

	pub fn not(argument: Box<Formula>) -> Self
	{
		Self::Not(argument)
	}

	pub fn not_equal(left: Box<Term>, right: Box<Term>) -> Self
	{
		Self::compare(ComparisonOperator::NotEqual, left, right)
	}

	pub fn or(arguments: Formulas) -> Self
	{
		Self::Or(arguments)
	}

	pub fn predicate(declaration: std::rc::Rc<PredicateDeclaration>, arguments: Terms) -> Self
	{
		Self::Predicate(Predicate::new(declaration, arguments))
	}

	pub fn true_() -> Self
	{
		Self::boolean(true)
	}
}
