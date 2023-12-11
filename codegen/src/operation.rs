use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::ir::Value;
use cranelift_frontend::FunctionBuilder;
use parser::Operator;
use std::iter::Iterator;

use crate::CompileErr;

pub trait ValueFuncUnary {
    fn value_func(builder: &mut FunctionBuilder, value: Value) -> Value;
}

pub trait ValueFunc {
    fn value_func(builder: &mut FunctionBuilder, value: Value, other: Value) -> Value;
}

pub trait ValueFuncIter {
    fn value_func<'a>(
        &self,
        builder: &mut FunctionBuilder,
        value: Value,
        others: impl Iterator<Item = &'a Value>,
    ) -> Result<Value, CompileErr>;
}

pub trait ValueAccumulator {
    fn value_func(builder: &mut FunctionBuilder, acc: Value, other: Value) -> Value;
    fn accumulate<'a>(
        builder: &mut FunctionBuilder,
        acc: Value,
        others: impl Iterator<Item = &'a Value>,
    ) -> Value {
        others.fold(acc, |acc, o| Self::value_func(builder, acc, *o))
    }
}

pub struct IAddAccumulator;
pub struct ISubAccumulator;
pub struct IMulAccumulator;
pub struct IDivAccumulator;
pub struct IModAccumulator;
pub struct INegAccumulator;

pub struct FAddAccumulator;
pub struct FSubAccumulator;
pub struct FMulAccumulator;
pub struct FDivAccumulator;
pub struct FNegAccumulator;

impl ValueAccumulator for IAddAccumulator {
    fn value_func(builder: &mut FunctionBuilder, acc: Value, other: Value) -> Value {
        builder.ins().iadd(acc, other)
    }
}
impl ValueAccumulator for ISubAccumulator {
    fn value_func(builder: &mut FunctionBuilder, acc: Value, other: Value) -> Value {
        builder.ins().isub(acc, other)
    }
}
impl ValueAccumulator for IMulAccumulator {
    fn value_func(builder: &mut FunctionBuilder, acc: Value, other: Value) -> Value {
        builder.ins().imul(acc, other)
    }
}
impl ValueAccumulator for IDivAccumulator {
    fn value_func(builder: &mut FunctionBuilder, acc: Value, other: Value) -> Value {
        builder.ins().sdiv(acc, other)
    }
}
impl ValueAccumulator for IModAccumulator {
    fn value_func(builder: &mut FunctionBuilder, acc: Value, other: Value) -> Value {
        builder.ins().srem(acc, other)
    }
}
impl ValueFuncUnary for INegAccumulator {
    fn value_func(builder: &mut FunctionBuilder, value: Value) -> Value {
        builder.ins().ineg(value)
    }
}

impl ValueAccumulator for FAddAccumulator {
    fn value_func(builder: &mut FunctionBuilder, acc: Value, other: Value) -> Value {
        builder.ins().fadd(acc, other)
    }
}
impl ValueAccumulator for FSubAccumulator {
    fn value_func(builder: &mut FunctionBuilder, acc: Value, other: Value) -> Value {
        builder.ins().fsub(acc, other)
    }
}
impl ValueAccumulator for FMulAccumulator {
    fn value_func(builder: &mut FunctionBuilder, acc: Value, other: Value) -> Value {
        builder.ins().fmul(acc, other)
    }
}
impl ValueAccumulator for FDivAccumulator {
    fn value_func(builder: &mut FunctionBuilder, acc: Value, other: Value) -> Value {
        builder.ins().fdiv(acc, other)
    }
}
impl ValueFuncUnary for FNegAccumulator {
    fn value_func(builder: &mut FunctionBuilder, value: Value) -> Value {
        builder.ins().fneg(value)
    }
}

pub struct IOpAccumulator<'a>(&'a Operator);
pub struct FOpAccumulator<'a>(&'a Operator);

impl<'b> ValueFuncIter for IOpAccumulator<'b> {
    fn value_func<'a>(
        &self,
        builder: &mut FunctionBuilder,
        value: Value,
        others: impl Iterator<Item = &'a Value>,
    ) -> Result<Value, CompileErr> {
        match self {
            IOpAccumulator(op) => match op {
                Operator::Add => Ok(IAddAccumulator::accumulate(builder, value, others)),
                Operator::Sub => Ok(ISubAccumulator::accumulate(builder, value, others)),
                Operator::Mul => Ok(IMulAccumulator::accumulate(builder, value, others)),
                Operator::Div => Ok(IDivAccumulator::accumulate(builder, value, others)),
                Operator::Mod => Ok(IModAccumulator::accumulate(builder, value, others)),
                _ => Err(CompileErr::InvalidOperation(format!(
                    "{op:?} on integer is not supported"
                ))),
            },
        }
    }
}
impl<'b> ValueFuncIter for FOpAccumulator<'b> {
    fn value_func<'a>(
        &self,
        builder: &mut FunctionBuilder,
        value: Value,
        others: impl Iterator<Item = &'a Value>,
    ) -> Result<Value, CompileErr> {
        match self {
            FOpAccumulator(op) => match op {
                Operator::Add => Ok(FAddAccumulator::accumulate(builder, value, others)),
                Operator::Sub => Ok(FSubAccumulator::accumulate(builder, value, others)),
                Operator::Mul => Ok(FMulAccumulator::accumulate(builder, value, others)),
                Operator::Div => Ok(FDivAccumulator::accumulate(builder, value, others)),
                _ => Err(CompileErr::InvalidOperation(format!(
                    "{op:?} on float is not supported"
                ))),
            },
        }
    }
}

impl<'a> From<&'a Operator> for IOpAccumulator<'a> {
    fn from(value: &'a Operator) -> Self {
        Self(value)
    }
}
impl<'a> From<&'a Operator> for FOpAccumulator<'a> {
    fn from(value: &'a Operator) -> Self {
        Self(value)
    }
}
