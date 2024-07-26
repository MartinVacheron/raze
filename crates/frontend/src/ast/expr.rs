use std::fmt::Display;
use ecow::EcoString;

use tools::results::{Loc, RevReport, RevResult};

use crate::lexer::Token;


#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary(BinaryExpr),
    Grouping(GroupingExpr),
    IntLiteral(IntLiteralExpr),
    FloatLiteral(FloatLiteralExpr),
    StrLiteral(StrLiteralExpr),
    Identifier(IdentifierExpr),
    Unary(UnaryExpr),
    Assign(AssignExpr),
    Logical(LogicalExpr),
    Call(CallExpr),
    Get(GetExpr),
    Set(SetExpr),
    Selff(SelfExpr),
    Is(IsExpr),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(e) => write!(f, "{} {} {}", e.left, e.operator, e.right),
            Expr::Grouping(e) => write!(f, "{}", e.expr),
            Expr::IntLiteral(e) => write!(f, "{}", e.value),
            Expr::FloatLiteral(e) => write!(f, "{}", e.value),
            Expr::StrLiteral(e) => write!(f, "{}", e.value),
            Expr::Identifier(e) => write!(f, "{}", e.name),
            Expr::Unary(e) => write!(f, "{} {}", e.operator, e.right),
            Expr::Assign(e) => write!(f, "{} {}", e.name, e.value),
            Expr::Logical(e) => write!(f, "{} {} {}", e.left, e.operator, e.right),
            Expr::Call(e) => write!(f, "{}: {:?}", e.callee, e.args),
            Expr::Get(e) => write!(f, "{}: {}", e.object, e.name),
            Expr::Set(e) => write!(f, "{}: {} {}", e.object, e.name, e.value),
            Expr::Selff(_) => write!(f, "self"),
            Expr::Is(e) => write!(f, "{} is {}", e.left, e.typ.value),
        }
    }
}

impl Expr {
    pub fn get_loc(&self) -> Loc {
        match self {
            Self::Binary(b) => b.get_loc(),
            Self::Grouping(g) => {
                let mut loc = g.expr.get_loc();
                loc.start -= 1;
                loc.end += 1;
                loc
            },
            Self::IntLiteral(i) => i.loc.clone(),
            Self::FloatLiteral(r) => r.loc.clone(),
            Self::StrLiteral(s) => s.loc.clone(),
            Self::Identifier(i) => i.loc.clone(),
            Self::Unary(u) => u.right.get_loc(),
            Self::Assign(a) => a.loc.clone(),
            Self::Logical(l) => l.loc.clone(),
            Self::Call(c) => c.loc.clone(),
            Self::Get(g) => g.object.get_loc(),
            Self::Set(s) => s.loc.clone(),
            Self::Selff(s) => s.loc.clone(),
            Self::Is(i) => i.loc.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

impl BinaryExpr {
    pub fn get_loc(&self) -> Loc {
        let l = self.left.get_loc();
        let r = self.right.get_loc();

        Loc::new(l.start, r.end)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct GroupingExpr {
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntLiteralExpr {
    pub value: i64,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FloatLiteralExpr {
    pub value: f64,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StrLiteralExpr {
    pub value: EcoString,
    pub loc: Loc,
}


#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierExpr {
    pub name: EcoString,
    pub loc: Loc,
}


#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>
}


#[derive(Debug, PartialEq, Clone)]
pub struct AssignExpr {
    pub name: EcoString,
    pub value: Box<Expr>,
    pub loc: Loc,
}


#[derive(Debug, PartialEq, Clone)]
pub struct LogicalExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct GetExpr {
    pub object: Box<Expr>,
    pub name: EcoString,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SetExpr {
    pub object: Box<Expr>,
    pub name: EcoString,
    pub value: Box<Expr>,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SelfExpr {
    pub name: EcoString,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IsExpr {
    pub left: Box<Expr>,
    pub typ: Token,
    pub loc: Loc,
}


impl Expr {
    pub fn accept<T, U: RevReport>(
        &self,
        visitor: &mut impl VisitExpr<T, U>,
    ) -> Result<T, RevResult<U>> {
        match self {
            Expr::Binary(e) => visitor.visit_binary_expr(e),
            Expr::Grouping(e) => visitor.visit_grouping_expr(e),
            Expr::IntLiteral(e) => visitor.visit_int_literal_expr(e),
            Expr::FloatLiteral(e) => visitor.visit_float_literal_expr(e),
            Expr::StrLiteral(e) => visitor.visit_str_literal_expr(e),
            Expr::Identifier(e) => visitor.visit_identifier_expr(e),
            Expr::Unary(e) => visitor.visit_unary_expr(e),
            Expr::Assign(e) => visitor.visit_assign_expr(e),
            Expr::Logical(e) => visitor.visit_logical_expr(e),
            Expr::Call(e) => visitor.visit_call_expr(e),
            Expr::Get(e) => visitor.visit_get_expr(e),
            Expr::Set(e) => visitor.visit_set_expr(e),
            Expr::Selff(e) => visitor.visit_self_expr(e),
            Expr::Is(e) => visitor.visit_is_expr(e),
        }
    }
}

pub trait VisitExpr<T, U: RevReport> {
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Result<T, RevResult<U>>;
    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> Result<T, RevResult<U>>;
    fn visit_int_literal_expr(&mut self, expr: &IntLiteralExpr) -> Result<T, RevResult<U>>;
    fn visit_float_literal_expr(&mut self, expr: &FloatLiteralExpr) -> Result<T, RevResult<U>>;
    fn visit_str_literal_expr(&mut self, expr: &StrLiteralExpr) -> Result<T, RevResult<U>>;
    fn visit_identifier_expr(&mut self, expr: &IdentifierExpr) -> Result<T, RevResult<U>>;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Result<T, RevResult<U>>;
    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Result<T, RevResult<U>>;
    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> Result<T, RevResult<U>>;
    fn visit_call_expr(&mut self, expr: &CallExpr) -> Result<T, RevResult<U>>;
    fn visit_get_expr(&mut self, expr: &GetExpr) -> Result<T, RevResult<U>>;
    fn visit_set_expr(&mut self, expr: &SetExpr) -> Result<T, RevResult<U>>;
    fn visit_self_expr(&mut self, expr: &SelfExpr) -> Result<T, RevResult<U>>;
    fn visit_is_expr(&mut self, expr: &IsExpr) -> Result<T, RevResult<U>>;
}

// Into
impl From<&IdentifierExpr> for Expr {
    fn from(value: &IdentifierExpr) -> Self {
        Self::Identifier(IdentifierExpr {
            name: value.name.clone(),
            loc: value.loc.clone(),
        })
    }
}
