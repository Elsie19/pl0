use core::fmt;

use qbe::{DataDef, DataItem, Function, Instr, Linkage, Module, Type, Value};

use crate::parse::grammar::Operand::*;
use crate::parse::grammar::{Block, Const, Do, Expression, Output, Statement, Var};

use super::variables::VariableTable;

fn format_var<S: AsRef<str>>(var: S) -> String {
    format!(".{}", var.as_ref())
}

#[derive(Debug)]
pub enum EvaluateError {
    UninitializedVariable,
    InvalidExpression,
}

impl fmt::Display for EvaluateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UninitializedVariable => write!(f, "Variable is uninitialized"),
            Self::InvalidExpression => write!(f, "Invalid expression"),
        }
    }
}

impl std::error::Error for EvaluateError {}

#[macro_export]
macro_rules! binary_op {
    ($expr:path, $left:expr, $right:expr, $func:expr, $variable_table:expr) => {{
        let left_side = evaluate(&$left, $func, $variable_table)?.1;
        let right_side = evaluate(&$right, $func, $variable_table)?.1;
        $func.assign_instr(
            $variable_table.tmp.clone().into(),
            Type::Word,
            $expr(left_side, right_side),
        );
        let ret = $variable_table.tmp.clone().into();
        $variable_table.tmp += 1;
        Ok((Type::Word, ret))
    }};
}

fn evaluate<'a>(
    expr: &Expression,
    func: &'a mut Function,
    variable_table: &'a mut VariableTable,
) -> Result<(Type<'a>, Value), EvaluateError> {
    match expr {
        Expression::Number(num) => Ok((Type::Word, Value::Const(*num as u64))),
        Expression::Ident(id) => match variable_table.table.get(id) {
            Some(id) => Ok((Type::Word, id.clone())),
            None => Err(EvaluateError::UninitializedVariable),
        },
        Expression::BinaryOp { op, left, right } => match op {
            Add => binary_op!(Instr::Add, left, right, func, variable_table),
            Sub => binary_op!(Instr::Sub, left, right, func, variable_table),
            Mul => binary_op!(Instr::Mul, left, right, func, variable_table),
            Div => binary_op!(Instr::Div, left, right, func, variable_table),
        },
        Expression::UnaryOp { op: _, expr: _ } => todo!(),
        Expression::Group(group) => evaluate(&group, func, variable_table),
    }
}

fn add_consts(constants: &[Const], module: &mut Module, variable_table: &mut VariableTable) {
    for con in constants {
        let items = vec![(Type::Word, DataItem::Const(con.number as u64))];

        let data = DataDef::new(Linkage::public(), con.ident.as_str(), Some(4), items);
        module.add_data(data);
        variable_table
            .table
            .insert(con.ident.clone(), Value::Global(format_var(&con.ident)));
    }
}

fn initialize_vars(vars: &[Var], func: &mut Function, variable_table: &mut VariableTable) {
    for var in vars {
        // All variables will be initialized to `0`.
        func.assign_instr(
            Value::Temporary(format_var(&var.ident)),
            Type::Long,
            Instr::Alloc4(4),
        );
        func.add_instr(Instr::Store(
            Type::Word,
            Value::Const(0),
            Value::Temporary(format_var(&var.ident)),
        ));
        variable_table
            .table
            .insert(var.ident.clone(), Value::Temporary(format_var(&var.ident)));
    }
}

fn output_var<'a>(
    output: &Output,
    func: &'a mut Function<'a>,
    variable_table: &'a mut VariableTable,
) {
    let eval_result =
        evaluate(&output.0, func, variable_table).expect("Could not parse evaluation.");

    func.add_instr(Instr::Call("puts".into(), vec![eval_result], None));
}

fn compile_ir<'a>(
    ir: &Block,
    variable_table: &'a mut VariableTable,
    module: &mut Module,
    func: &'a mut Function<'a>,
) {
    match &ir.0 {
        Do::Const(ref constants) => add_consts(&constants, module, variable_table),
        Do::Var(ref vars) => initialize_vars(&vars, func, variable_table),
        Do::Procedure(ref procedures) => {
            for constant in procedures {
                let mut inner_func = Function::new(
                    Linkage::private(),
                    &constant.ident,
                    variable_table.clone().into(),
                    None,
                );
                compile_ir(ir, variable_table, module, &mut inner_func);
            }
        }
        Do::Statement(stmt) => match stmt {
            Statement::Output(output) => output_var(output, func, variable_table),
            _ => todo!(),
        },
    }
}

/// Compile PL/0 IR into a [`qbe::Module`].
pub fn compile(ir: &[Block]) -> String {
    let mut module = Module::new();
    let mut variables = VariableTable::default();

    let mut func = Function::new(Linkage::public(), "main", vec![], Some(Type::Word));

    func.add_block("start");
    for i in ir {
        compile_ir(i, &mut variables, &mut module, &mut func);
    }

    func.add_instr(Instr::Ret(Some(Value::Const(0))));

    module.add_function(func);

    module.to_string()
}
