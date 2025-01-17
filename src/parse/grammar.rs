use pest_consume::match_nodes;
use pest_consume::Error;
use pest_consume::Parser;

#[derive(Parser)]
#[grammar = "parse/pl0.pest"]
pub struct PL0Parser;

#[derive(Debug)]
pub struct Call(pub String);
#[derive(Debug)]
pub struct Input(pub String);
#[derive(Debug)]
pub struct Output(pub Expression);
#[derive(Debug)]
pub struct Block(pub Do);

#[derive(Debug)]
pub struct Const {
    pub ident: String,
    pub number: usize,
}

#[derive(Debug)]
pub struct Var {
    pub ident: String,
    pub number: Option<Expression>,
}

#[derive(Debug)]
pub struct Procedure {
    pub ident: String,
    pub block: Box<Do>,
}

#[derive(Debug)]
pub enum Condition {
    Odd(Expression),
    Comparison(Comparison),
}

#[derive(Debug)]
pub struct Conditional {
    pub condition: Condition,
    pub statement: Box<Statement>,
}

#[derive(Debug)]
pub struct Loop {
    pub condition: Condition,
    pub statement: Box<Statement>,
}

#[derive(Debug)]
pub struct Comparison {
    pub left: Expression,
    pub comparator: Comparator,
    pub right: Expression,
}

#[derive(Debug)]
pub enum Comparator {
    Eq,
    Ineq,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug)]
pub enum Operand {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum Expression {
    Number(usize),
    Ident(String),
    UnaryOp {
        op: Operand,
        expr: Box<Expression>,
    },
    BinaryOp {
        op: Operand,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Group(Box<Expression>),
}

#[derive(Debug)]
pub struct CompoundStatement(Vec<Statement>);

#[derive(Debug)]
pub enum Statement {
    Assignment(Var),
    ProcedureCall(Call),
    Input(Input),
    Output(Output),
    CompoundStatement(CompoundStatement),
    ConditionalStatement(Conditional),
    LoopStatement(Loop),
}

#[derive(Debug)]
pub enum Do {
    Const(Vec<Const>),
    Var(Vec<Var>),
    Procedure(Vec<Procedure>),
    Statement(Statement),
}

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[pest_consume::parser]
impl PL0Parser {
    pub fn program(input: Node) -> Result<Vec<Block>> {
        Ok(match_nodes!(input.into_children();
            [block(block).., EOI(())] => block.map(|b| Block(b)).collect(),
        ))
    }

    fn block(input: Node) -> Result<Do> {
        Ok(match_nodes!(input.into_children();
            [const_declaration(co)] => Do::Const(co),
            [var_declaration(var)] => Do::Var(var),
            [procedure_declaration(proc)..] => Do::Procedure(proc.collect()),
            [statement(stmt)] => Do::Statement(stmt),
        ))
    }

    fn statement(input: Node) -> Result<Statement> {
        Ok(match_nodes!(input.into_children();
            [assignment(assign)] => Statement::Assignment(assign),
            [procedure_call(call)] => Statement::ProcedureCall(call),
            [input(input)] => Statement::Input(input),
            [output(output)] => Statement::Output(output),
            [compound_statement(stmt)] => Statement::CompoundStatement(stmt),
            [conditional_statement(stmt)] => Statement::ConditionalStatement(stmt),
            [loop_statement(stmt)] => Statement::LoopStatement(stmt),
        ))
    }

    fn conditional_statement(input: Node) -> Result<Conditional> {
        Ok(match_nodes!(input.into_children();
            [condition(condition), statement(stmt)] => Conditional { condition, statement: Box::new(stmt) },
        ))
    }

    fn loop_statement(input: Node) -> Result<Loop> {
        Ok(match_nodes!(input.into_children();
            [condition(condition), statement(stmt)] => Loop { condition, statement: Box::new(stmt) },
        ))
    }

    fn compound_statement(input: Node) -> Result<CompoundStatement> {
        Ok(match_nodes!(input.into_children();
            [statement(stmt)..] => CompoundStatement(stmt.collect()),
        ))
    }

    fn assignment(input: Node) -> Result<Var> {
        Ok(match_nodes!(input.into_children();
            [ident(ident), expression(expr)] => Var { ident, number: Some(expr) },
        ))
    }

    fn procedure_declaration(input: Node) -> Result<Procedure> {
        Ok(match_nodes!(input.into_children();
            [ident(ident), block(block)] => Procedure { ident, block: Box::new(block) },
        ))
    }

    fn const_declaration(input: Node) -> Result<Vec<Const>> {
        Ok(match_nodes!(input.into_children();
            [const_assignment(constant)..] => constant.collect(),
        ))
    }

    fn const_assignment(input: Node) -> Result<Const> {
        Ok(match_nodes!(input.into_children();
            [ident(ident), number(number)] => Const { ident, number },
        ))
    }

    fn var_declaration(input: Node) -> Result<Vec<Var>> {
        Ok(match_nodes!(input.into_children();
            [ident(ident)..] => ident.map(|id| Var { ident: id, number: None }).collect(),
        ))
    }

    fn number(input: Node) -> Result<usize> {
        input.as_str().parse::<usize>().map_err(|e| input.error(e))
    }

    fn ident(input: Node) -> Result<String> {
        Ok(input.as_str().to_string())
    }

    fn expression(input: Node) -> Result<Expression> {
        Ok(match_nodes!(input.into_children();
            [term(term)] => term,
            [term(left), op(op), term(right)] => Expression::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            },
        ))
    }

    fn term(input: Node) -> Result<Expression> {
        Ok(match_nodes!(input.into_children();
            [factor(factor)] => factor,
            [factor(left), op(op), factor(right)] => Expression::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            },
        ))
    }

    fn factor(input: Node) -> Result<Expression> {
        Ok(match_nodes!(input.into_children();
            [number(number)] => Expression::Number(number),
            [ident(ident)] => Expression::Ident(ident),
            [expression(expr)] => Expression::Group(Box::new(expr)),
        ))
    }

    fn op(input: Node) -> Result<Operand> {
        Ok(match input.as_str() {
            "+" => Operand::Add,
            "-" => Operand::Sub,
            "/" => Operand::Div,
            "*" => Operand::Mul,
            _ => unimplemented!("Operator not implemented yet"),
        })
    }

    fn condition(input: Node) -> Result<Condition> {
        Ok(match_nodes!(input.into_children();
            [expression(expr)] => Condition::Odd(expr),
            [expression(left), comparison_op(comparator), expression(right)] => Condition::Comparison(Comparison { left, comparator, right }),
        ))
    }

    fn comparison_op(input: Node) -> Result<Comparator> {
        Ok(match input.as_str() {
            "=" => Comparator::Eq,
            "#" => Comparator::Ineq,
            "<" => Comparator::Lt,
            "<=" => Comparator::Le,
            ">" => Comparator::Gt,
            ">=" => Comparator::Ge,
            _ => unimplemented!("Operator not implemented yet"),
        })
    }

    fn procedure_call(input: Node) -> Result<Call> {
        Ok(match_nodes!(input.into_children();
            [ident(func)] => Call(func),
        ))
    }

    fn input(input: Node) -> Result<Input> {
        Ok(match_nodes!(input.into_children();
            [ident(func)] => Input(func),
        ))
    }

    fn output(input: Node) -> Result<Output> {
        Ok(match_nodes!(input.into_children();
            [expression(expr)] => Output(expr),
        ))
    }

    fn EOI(_input: Node) -> Result<()> {
        Ok(())
    }
}
