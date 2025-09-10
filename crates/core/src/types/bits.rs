// use crate::interpreter::value::Value;
// use std::fmt;
// use std::fmt::Formatter;

// #[derive(Debug, Clone)]
// pub enum BitVal {
//     One,
//     Zero,
// }

// #[derive(Debug, Clone)]
// pub struct Bit {
//     bit: BitVal,
// }

// impl Bit {
//     pub fn new(bit: BitVal) -> Self {
//         Self { bit }
//     }
//     pub fn value(&self) -> BitVal {
//         self.bit.clone()
//     }

//     pub fn as_bool_value(&self) -> Value {
//         match self.bit {
//             BitVal::One => Value::Boolean(true),
//             BitVal::Zero => Value::Boolean(false),
//         }
//     }
// }

// impl fmt::Display for BitVal {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         match self {
//             BitVal::One => write!(f, "1"),
//             BitVal::Zero => write!(f, "0"),
//         }
//     }
// }
