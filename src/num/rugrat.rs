use std::cmp::Ordering;

use rug::Rational;
use opers::Calculation;
use errors::MathError;
use answer::Answer;
use num::Num;
use context::Context;

impl Num for Rational {
	fn from_f64(t: f64, ctx: &Context<Self>) -> Calculation<Self> {
		Ok(Answer::Single(if let Some(r) = Rational::from_f64(t) {
			r
		} else {
			return Err(MathError::Other) // TODO make descriptive
		}))
	}
	
	fn from_f64_complex((r, _i): (f64, f64), ctx: &Context<Self>) -> Calculation<Self> {
		Ok(Answer::Single(if let Some(r) = Rational::from_f64(r) {
			r
		} else {
			return Err(MathError::Other) // TODO make descriptive
		}))
	}
	
	fn tryord(&self, other: &Self, ctx: &Context<Self>) -> Result<Ordering, MathError> {
		if let Some(ord) = self.partial_cmp(other) {
			Ok(ord)
		} else {
			Err(MathError::CmpError)
		}
	}
	
	fn add(&self, other: &Self, ctx: &Context<Self>) -> Calculation<Self> {
		let r = Rational::from(self + other);
		
		Ok(Answer::Single(r))
	}
	
	fn sub(&self, other: &Self, ctx: &Context<Self>) -> Calculation<Self> {
		let r = Rational::from(self - other);
		
		Ok(Answer::Single(r))
	}
	
	fn mul(&self, other: &Self, ctx: &Context<Self>) -> Calculation<Self> {
		let r = Rational::from(self * other);
		
		Ok(Answer::Single(r))
	}
	
	fn div(&self, other: &Self, ctx: &Context<Self>) -> Calculation<Self> {
		let r = Rational::from(self / other);
		
		Ok(Answer::Single(r))
	}
	
	fn pow(&self, other: &Self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn sqrt(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn nrt(&self, other: &Self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn abs(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn sin(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn cos(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn tan(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn asin(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn acos(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn atan(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn atan2(&self, other: &Self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn floor(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn ceil(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn round(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn log(&self, other: &Self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
}