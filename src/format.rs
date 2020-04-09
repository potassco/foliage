mod formulas;
mod terms;

trait Precedence
{
	fn precedence(&self) -> i32;
}
