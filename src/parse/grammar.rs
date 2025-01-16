use pest_consume::match_nodes;
use pest_consume::Error;
use pest_consume::Parser;

#[derive(Parser)]
#[grammar = "parse/pl0.pest"]
pub struct PL0Parser;

pub struct Call(String);
pub struct Input(String);
pub struct Block(Do);

pub struct Const {
    pub ident: String,
    pub number: usize,
}

pub struct Var {
    pub ident: String,
    pub number: Option<usize>,
}

pub struct Procedure {
    pub ident: String,
    pub block: Block,
}

pub enum Do {
    Const(Vec<Const>),
    Var(Vec<Var>),
    Procedure(Procedure),
    Statement(Statement),
}

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[pest_consume::parser]
impl PL0Parser {
    pub fn program(input: Node) -> Result<Block> {
        Ok(match_nodes!(input.into_children();
            [block(block)] => Block(block),
        ))
    }

    fn block(input: Node) -> Result<Do> {
        Ok(match_nodes!(input.into_children();
            [const_declaration(co)] => Do::Const(co),
            [var_declaration(var)] => Do::Var(var),
            [procedure_declaration(proc)] => Do::Procedure(proc),
            [statement(stmt)] => Do::Statement(stmt),
        ))
    }

    fn procedure_declaration(input: Node) -> Result<Procedure> {
        Ok(match_nodes!(input.into_children();
            [ident(ident), block(block)] => constant.collect(),
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
        Ok(input.to_string())
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
}
