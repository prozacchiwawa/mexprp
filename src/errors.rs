/// Got an unexpected token
#[derive(Debug, Fail)]
#[fail(display = "Got unexpected token")]
pub struct UnexpectedToken {
	/// The token
	pub token: String,
}

/// Parenthesis of the expression didn't match
#[derive(Debug, Fail)]
#[fail(display = "Parenthesis didn't match")]
pub struct MismatchedParenthesis;

/// An error that can occur while evaluating an expression
#[derive(Debug, Fail)]
pub enum MathError {
	/// A variable that was not defined in the context was referenced
	#[fail(display = "Variable '{}' is not defined", name)]
	UndefinedVariable {
		/// The name of the variable
		name: String,
	},
	/// A function that was not defined in the context was referenced
	#[fail(display = "Function '{}' is not defined", name)]
	UndefinedFunction {
		/// The name of the function
		name: String,
	},
	/// A function was given arguments in an incorrect form
	#[fail(display = "A function was passed incorrect arguments")]
	IncorrectArguments,
	/// Attempted to divide by zero
	#[fail(display = "Attempted to divide by zero")]
	DivideByZero,
	/// A NaN value was used in a way that is not possible
	#[fail(display = "A NaN value was attempted to be used as an operand")]
	NaN,
}

/// Expected a token but was not met
#[derive(Debug, Fail)]
pub enum Expected {
	/// Expected an operator
	#[fail(display = "Expected another operator")]
	Operator,
	/// Expected an expression
	#[fail(display = "Expected another expression")]
	Expression,
	/// Expected a parenthesis
	#[fail(display = "Expected a parenthesis")]
	Paren,
	/// Expected a function
	#[fail(display = "Expected a function")]
	Function,
}