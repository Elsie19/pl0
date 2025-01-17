use std::{
    collections::HashMap,
    ops::{Add, AddAssign, Sub},
};

use qbe::{Type, Value};

#[derive(Default, Clone)]
pub struct VariableTable {
    pub table: HashMap<String, Value>,
    pub tmp: TmpVariableTable,
}

impl Into<Vec<(Type<'_>, Value)>> for VariableTable {
    fn into(self) -> Vec<(Type<'static>, Value)> {
        self.table
            .iter()
            .map(|var| (Type::Long, Value::Temporary(var.0.to_string())))
            .collect()
    }
}

#[derive(Default, Clone)]
pub struct TmpVariableTable {
    pub table: usize,
}

impl Into<Value> for TmpVariableTable {
    fn into(self) -> Value {
        Value::Temporary(format!(".{}", self.table))
    }
}

impl AddAssign for TmpVariableTable {
    fn add_assign(&mut self, rhs: Self) {
        self.table += rhs.table
    }
}

impl AddAssign<usize> for TmpVariableTable {
    fn add_assign(&mut self, rhs: usize) {
        self.table += rhs
    }
}

impl Add for TmpVariableTable {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            table: self.table + rhs.table,
        }
    }
}

impl Sub for TmpVariableTable {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            table: self.table - rhs.table,
        }
    }
}

impl ToString for TmpVariableTable {
    fn to_string(&self) -> String {
        format!(".{}", self.table)
    }
}
