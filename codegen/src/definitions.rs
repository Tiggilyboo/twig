use cranelift_codegen::ir::types;
use cranelift_codegen::ir::{immediates::Offset32, StackSlot, Type, Value};
use cranelift_frontend::Variable;
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
    List(Vec<(Type, DefinedValue)>),
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub symbol: Symbol,
    pub val_type: Type,
    pub value: DefinedValue,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Symbol(usize);

impl Default for Symbol {
    fn default() -> Self {
        Symbol(0)
    }
}

impl From<usize> for Symbol {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl Definition {
    pub fn new(symbol: Symbol, val_type: Type, value: DefinedValue) -> Self {
        Self {
            symbol,
            val_type,
            value,
        }
    }

    pub fn null() -> Self {
        Self {
            symbol: Symbol::default(),
            val_type: types::INVALID,
            value: DefinedValue::None,
        }
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
