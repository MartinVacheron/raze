use ecow::EcoString;

use crate::ast::{
    expr::{
        AssignExpr, BinaryExpr, CallExpr, GetExpr, GroupingExpr, IdentifierExpr, IntLiteralExpr, LogicalExpr, RealLiteralExpr, StrLiteralExpr, UnaryExpr, VisitExpr
    },
    stmt::{
        BlockStmt, ExprStmt, FnDeclStmt, ForStmt, IfStmt, PrintStmt, ReturnStmt, Stmt, StructStmt, VarDeclStmt, VisitStmt, WhileStmt
    },
};

use tools::results::{PhyReport, PhyResult, Loc};

#[derive(Debug)]
pub enum ParserTestErr {}

impl PhyReport for ParserTestErr {
    fn get_err_msg(&self) -> String {
        String::from("")
    }
}

type PhyResParserTestErr = PhyResult<ParserTestErr>;

// ------
//  Stmt
// ------
#[derive(Default, Debug, PartialEq, Clone)]
pub struct StmtInfos {
    pub expr: ExprInfos,
    pub print: Vec<String>,
    pub var_decl: Vec<(EcoString, Option<ExprInfos>)>,
    pub block: Vec<StmtInfos>,
    pub if_stmt: Vec<IfInfos>,
    pub while_stmt: Vec<WhileInfos>,
    pub for_stmt: Vec<ForInfos>,
    pub fn_decl: Vec<FnDeclInfos>,
    pub return_stmt: Vec<Option<ExprInfos>>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct IfInfos {
    pub condition: ExprInfos,
    pub then_branch: Option<StmtInfos>,
    pub else_branch: Option<StmtInfos>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct WhileInfos {
    pub condition: ExprInfos,
    pub body: StmtInfos,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct ForInfos {
    pub placeholder: EcoString,
    pub range: (i64, Option<i64>),
    pub body: StmtInfos,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct FnDeclInfos {
    pub name: EcoString,
    pub params: Vec<EcoString>,
    pub body: Vec<StmtInfos>,
}

impl StmtInfos {
    fn concat(&mut self, other: &mut StmtInfos) {
        self.expr.concat(&mut other.expr);
        self.print.append(&mut other.print);
        self.var_decl.append(&mut other.var_decl);
        self.block.append(&mut other.block);
        self.if_stmt.append(&mut other.if_stmt);
        self.while_stmt.append(&mut other.while_stmt);
        self.for_stmt.append(&mut other.for_stmt);
        self.fn_decl.append(&mut other.fn_decl);
        self.return_stmt.append(&mut other.return_stmt);
    }
}

impl VisitStmt<StmtInfos, ParserTestErr> for TestParser {
    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Result<StmtInfos, PhyResult<ParserTestErr>> {
        let expr = stmt.expr.accept(self)?;
        Ok(StmtInfos {
            expr,
            ..Default::default()
        })
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> Result<StmtInfos, PhyResult<ParserTestErr>> {
        let expr = stmt.expr.accept(self)?;
        let mut infos = StmtInfos::default();

        // Only one field cannot be empty
        if let Some(v) = expr.get_int_values().first() {
            infos.print = vec![format!("{}", v)];
        } else if let Some(v) = expr.get_real_values().first() {
            infos.print = vec![format!("{}", v)];
        } else if let Some(v) = expr.get_str_values().first() {
            infos.print = vec![format!("{}", v)];
        } else if let Some(v) = expr.get_ident_values().first() {
            infos.print = vec![format!("{}", v)];
        }

        Ok(infos)
    }

    fn visit_var_decl_stmt(
        &mut self,
        stmt: &VarDeclStmt,
    ) -> Result<StmtInfos, PhyResult<ParserTestErr>> {
        let mut infos = StmtInfos::default();

        let val = if let Some(v) = &stmt.value {
            Some(v.accept(self)?)
        } else {
            None
        };

        infos.var_decl = vec![(stmt.name.clone(), val)];
        Ok(infos)
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Result<StmtInfos, PhyResult<ParserTestErr>> {
        let mut all_infos: StmtInfos = StmtInfos::default();

        for s in &stmt.stmts {
            all_infos.concat(&mut s.accept(self)?);
        }

        Ok(StmtInfos {
            block: vec![all_infos],
            ..Default::default()
        })
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Result<StmtInfos, PhyResult<ParserTestErr>> {
        let condition = stmt.condition.accept(self)?;

        let mut then_branch = None;
        if let Some(e) = &stmt.then_branch {
            then_branch = Some(e.accept(self)?);
        }

        let mut else_branch = None;

        if let Some(e) = &stmt.else_branch {
            else_branch = Some(e.accept(self)?);
        }

        Ok(StmtInfos {
            if_stmt: vec![IfInfos {
                condition,
                then_branch,
                else_branch,
            }],
            ..Default::default()
        })
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Result<StmtInfos, PhyResult<ParserTestErr>> {
        let condition = stmt.condition.accept(self)?;
        let body = stmt.body.accept(self)?;

        Ok(StmtInfos {
            while_stmt: vec![WhileInfos { condition, body }],
            ..Default::default()
        })
    }

    fn visit_for_stmt(&mut self, stmt: &ForStmt) -> Result<StmtInfos, PhyResult<ParserTestErr>> {
        let placeholder = stmt.placeholder.name.clone();
        let range = (stmt.range.start, stmt.range.end);
        let body = stmt.body.accept(self)?;

        Ok(StmtInfos {
            for_stmt: vec![ForInfos {
                placeholder,
                range,
                body,
            }],
            ..Default::default()
        })
    }

    fn visit_fn_decl_stmt(&mut self, stmt: &FnDeclStmt) -> Result<StmtInfos, PhyResult<ParserTestErr>> {
        let name = stmt.name.clone();

        let mut body: Vec<StmtInfos> = vec![];
        for s in &*stmt.body {
            body.push(s.accept(self)?);
        }

        Ok(StmtInfos {
            fn_decl: vec![FnDeclInfos {
                name,
                params: stmt.params.clone().to_vec(),
                body,
            }],
            ..Default::default()
        })
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Result<StmtInfos, PhyResult<ParserTestErr>> {
        let value = match &stmt.value {
            Some(v) => Some(v.accept(self)?),
            None => None,
        };

        Ok(StmtInfos { return_stmt: vec![value], ..Default::default() })
    }
    
    fn visit_struct_stmt(&mut self, _stmt: &StructStmt) -> Result<StmtInfos, PhyResult<ParserTestErr>> {
        todo!()
    }
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct ExprInfos {
    pub int: Vec<IntInfo>,
    pub real: Vec<RealInfo>,
    pub str: Vec<StrInfo>,
    pub bool: Vec<BoolInfo>,
    pub binop: Vec<BinopInfo>,
    pub grouping: Vec<GroupingInfo>,
    pub ident: Vec<IdentifierInfo>,
    pub unary: Vec<UnaryInfo>,
    pub assign: Vec<AssignInfo>,
    pub logical: Vec<LogicalInfo>,
    pub call: Vec<CallInfo>,
}

impl ExprInfos {
    pub fn get_int_values(&self) -> Vec<&i64> {
        self.int.iter().map(|i| &i.value).collect()
    }

    pub fn get_real_values(&self) -> Vec<&f64> {
        self.real.iter().map(|i| &i.value).collect()
    }

    pub fn get_str_values(&self) -> Vec<EcoString> {
        self.str.iter().map(|i| i.value.clone()).collect()
    }

    pub fn get_grp_values(&self) -> Vec<&ExprInfos> {
        self.grouping.iter().map(|g| &g.expr).collect()
    }

    pub fn get_ident_values(&self) -> Vec<EcoString> {
        self.ident.iter().map(|i| i.name.clone()).collect()
    }

    pub fn get_binop_values(&self) -> Vec<(&ExprInfos, EcoString, &ExprInfos)> {
        self.binop
            .iter()
            .map(|b| (&b.left, b.op.clone(), &b.right))
            .collect()
    }

    pub fn get_assign_values(&self) -> Vec<(EcoString, &ExprInfos)> {
        self.assign
            .iter()
            .map(|a| (a.name.clone(), &a.expr))
            .collect()
    }

    pub fn get_locations(&self) -> Vec<&Loc> {
        let mut locs: Vec<&Loc> = vec![];
        self.int.iter().for_each(|i| locs.push(&i.loc));
        self.real.iter().for_each(|r| locs.push(&r.loc));
        self.binop.iter().for_each(|b| locs.push(&b.loc));
        self.unary.iter().for_each(|u| locs.push(&u.loc));
        self.grouping.iter().for_each(|g| locs.push(&g.loc));

        locs
    }

    fn concat(&mut self, other: &mut ExprInfos) {
        self.int.append(&mut other.int);
        self.real.append(&mut other.real);
        self.str.append(&mut other.str);
        self.binop.append(&mut other.binop);
        self.grouping.append(&mut other.grouping);
        self.unary.append(&mut other.unary);
        self.assign.append(&mut other.assign);
        self.logical.append(&mut other.logical);
        self.call.append(&mut other.call);
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinopInfo {
    pub left: ExprInfos,
    pub op: EcoString,
    pub right: ExprInfos,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntInfo {
    pub value: i64,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RealInfo {
    pub value: f64,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StrInfo {
    pub value: EcoString,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BoolInfo {
    pub value: bool,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct GroupingInfo {
    pub expr: ExprInfos,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierInfo {
    pub name: EcoString,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryInfo {
    pub expr: ExprInfos,
    pub op: EcoString,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AssignInfo {
    pub name: EcoString,
    pub expr: ExprInfos,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LogicalInfo {
    pub left: ExprInfos,
    pub op: EcoString,
    pub right: ExprInfos,
    pub loc: Loc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallInfo {
    pub callee: ExprInfos,
    pub args: Vec<ExprInfos>,
    pub loc: Loc,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct TestParser {
    pub infos: StmtInfos,
}

impl TestParser {
    pub fn get_all_infos(&mut self, nodes: &[Stmt]) -> Result<&StmtInfos, PhyResParserTestErr> {
        for node in nodes {
            let mut res = node.accept(self).unwrap();
            self.infos.concat(&mut res);
        }

        Ok(&self.infos)
    }
}

impl VisitExpr<ExprInfos, ParserTestErr> for TestParser {
    fn visit_int_literal_expr(
        &mut self,
        expr: &IntLiteralExpr,
    ) -> Result<ExprInfos, PhyResParserTestErr> {
        let mut infos = ExprInfos::default();
        let int_infos = IntInfo {
            value: expr.value,
            loc: expr.loc.clone(),
        };
        infos.int.push(int_infos);

        Ok(infos)
    }

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Result<ExprInfos, PhyResParserTestErr> {
        let mut infos = ExprInfos::default();
        let binop_infos = BinopInfo {
            left: expr.left.accept(self).unwrap(),
            op: expr.operator.clone(),
            right: expr.right.accept(self).unwrap(),
            loc: expr.loc.clone(),
        };
        infos.binop.push(binop_infos);

        Ok(infos)
    }

    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> Result<ExprInfos, PhyResParserTestErr> {
        let mut infos = ExprInfos::default();
        let grouping_info = GroupingInfo {
            expr: expr.expr.accept(self).unwrap(),
            loc: expr.loc.clone(),
        };
        infos.grouping.push(grouping_info);

        Ok(infos)
    }

    fn visit_real_literal_expr(
        &mut self,
        expr: &RealLiteralExpr,
    ) -> Result<ExprInfos, PhyResParserTestErr> {
        let mut infos = ExprInfos::default();
        let real_infos = RealInfo {
            value: expr.value,
            loc: expr.loc.clone(),
        };
        infos.real.push(real_infos);

        Ok(infos)
    }

    fn visit_str_literal_expr(
        &mut self,
        expr: &StrLiteralExpr,
    ) -> Result<ExprInfos, PhyResParserTestErr> {
        let mut infos = ExprInfos::default();
        let str_infos = StrInfo {
            value: expr.value.clone(),
            loc: expr.loc.clone(),
        };
        infos.str.push(str_infos);

        Ok(infos)
    }

    fn visit_identifier_expr(
        &mut self,
        expr: &IdentifierExpr,
    ) -> Result<ExprInfos, PhyResParserTestErr> {
        let mut infos = ExprInfos::default();
        let ident_info = IdentifierInfo {
            name: expr.name.clone(),
        };
        infos.ident.push(ident_info);

        Ok(infos)
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Result<ExprInfos, PhyResParserTestErr> {
        let mut infos = ExprInfos::default();
        let unary_info = UnaryInfo {
            expr: expr.right.accept(self).unwrap(),
            op: expr.operator.clone(),
            loc: expr.loc.clone(),
        };
        infos.unary.push(unary_info);

        Ok(infos)
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Result<ExprInfos, PhyResult<ParserTestErr>> {
        let mut infos = ExprInfos::default();
        let assign_infos = AssignInfo {
            name: expr.name.clone(),
            expr: expr.value.accept(self)?,
            loc: expr.loc.clone(),
        };
        infos.assign.push(assign_infos);
        Ok(infos)
    }

    fn visit_logical_expr(
        &mut self,
        expr: &LogicalExpr,
    ) -> Result<ExprInfos, PhyResult<ParserTestErr>> {
        let mut infos = ExprInfos::default();
        let logical_infos = LogicalInfo {
            left: expr.left.accept(self).unwrap(),
            op: expr.operator.clone(),
            right: expr.right.accept(self).unwrap(),
            loc: expr.loc.clone(),
        };
        infos.logical.push(logical_infos);

        Ok(infos)
    }

    fn visit_call_expr(&mut self, expr: &CallExpr) -> Result<ExprInfos, PhyResult<ParserTestErr>> {
        let mut infos = ExprInfos::default();
        let callee = expr.callee.accept(self)?;
        let mut args: Vec<ExprInfos> = vec![];

        for a in &expr.args {
            args.push(a.accept(self)?);
        }

        infos.call.push(CallInfo {
            callee,
            args,
            loc: expr.loc.clone(),
        });

        Ok(infos)
    }
    
    fn visit_get_expr(&mut self, expr: &GetExpr) -> Result<ExprInfos, PhyResult<ParserTestErr>> {
        todo!()
    }
}
