use cranelift::{
    codegen::ir::{immediates::Offset32, StackSlot},
    prelude::*,
};
use cranelift_module::{DataId, FuncId};

#[derive(Debug, Clone)]
pub enum DefinedValue {
    None,
    Value(Value),
    Variable(Variable),
    Function(FuncId),
    Data(DataId),
    Stack {
        slot: StackSlot,
        items: Vec<(Type, Offset32)>,
    },
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub val_type: Type,
    pub value: DefinedValue,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Symbol(usize);

impl From<usize> for Symbol {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl Definition {
    pub fn new(val_type: Type, value: DefinedValue) -> Self {
        Self { val_type, value }
    }

    pub fn null() -> Self {
        Self {
            val_type: types::INVALID,
            value: DefinedValue::None,
        }
    }

    pub fn symbol(&self) -> Symbol {
        let index = match self.value {
            DefinedValue::None => 0,
            DefinedValue::Value(value) => value.index(),
            DefinedValue::Variable(var) => var.index(),
            DefinedValue::Data(data_id) => data_id.index(),
            DefinedValue::Function(func_id) => func_id.index(),
            DefinedValue::Stack { slot, .. } => slot.index(),
        };

        Symbol(index)
    }

    pub fn bits(&self) -> u32 {
        self.val_type.bits()
    }

    pub fn bytes(&self) -> u32 {
        self.val_type.bytes()
    }
}

impl Default for Definition {
    fn default() -> Self {
        Self::null()
    }
}
impl From<(Type, Value)> for Definition {
    fn from(value: (Type, Value)) -> Self {
        Self::new(value.0, DefinedValue::Value(value.1))
    }
}
impl From<(Type, FuncId)> for Definition {
    fn from(value: (Type, FuncId)) -> Self {
        Self::new(value.0, DefinedValue::Function(value.1))
    }
}
impl From<(Type, DataId)> for Definition {
    fn from(value: (Type, DataId)) -> Self {
        Self::new(value.0, DefinedValue::Data(value.1))
    }
}
impl From<(Type, Variable)> for Definition {
    fn from(value: (Type, Variable)) -> Self {
        Self::new(value.0, DefinedValue::Variable(value.1))
    }
}
