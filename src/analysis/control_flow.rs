//! Module for analyzing the control flow of a program.
//!
//! Currently, the analysis here does not truly build a full control-flow graph,
//! largely because there aren't plans to do extensive optimizations outside of
//! passes in LLVM, which is able to handle control flow on its own. Instead,
//! function bodies are traversed in reverse for the sole purpose of detecting
//! a) that every branch of every function returns and b) dead code the user
//! can be warned about.
use chumsky::span::SimpleSpan;

use crate::AstVisitor;

use super::*;

/// Type for verifying function return validity with regards to control flow.
pub struct ControlFlowVisitor<'a> {
    /// Spans of code that are dead/unreachable due to a preceding return.
    dead: Vec<SimpleSpan>,
    errs: Vec<ControlFlowErr<'a>>,
}

impl<'a> AstVisitor<'a, Vec<SimpleSpan>, ControlFlowErr<'a>> for ControlFlowVisitor<'a> {
    fn visit(
        self,
        ast: &Vec<Decl<'a>>
    ) -> Result<Vec<SimpleSpan>, Vec<ControlFlowErr<'a>>> {
        let (dead, errs) = self.resolve(ast);
        if errs.is_empty() {
            Ok(dead)
        } else {
            Err(errs)
        }
    }
}

impl<'a> ControlFlowVisitor<'a> {
    pub fn new() -> ControlFlowVisitor<'a> {
        ControlFlowVisitor {
            dead: vec![],
            errs: vec![],
        }
    }

    /// Run control flow verification on the AST, ensuring every function
    /// returns in every branch (ignoring functions that return void).
    ///
    /// This function consumes the `ControlFlowVisitor` and returns a tuple
    /// containing a vector of [`chumsky::SimpleSpan`]s warning of any dead /
    /// unreachable code and a vector of [`ControlFlowErr`]s for any functions
    /// that can't be guaranteed to return properly.
    fn resolve(
        mut self,
        ast: &Vec<Decl<'a>>
    ) -> (Vec<SimpleSpan>, Vec<ControlFlowErr<'a>>) {
        for decl in ast {
            let Decl::Function(function) = decl else { continue };

            // ignore return verification if function returns void:
            let Type::Function(fn_type) = &*function.r#type else { panic!() };
            if let Type::Atomic(Atomic::Void, ..) = *fn_type.return_type { continue };

            let Some(body) = &function.body else { continue; };
            
            let returned = match body {
                Stmt::Block(stmts, ..) => self.visit_block(&stmts),
                Stmt::Return(..) => true,
                Stmt::If(if_stmt) => self.visit_if(&if_stmt),
                Stmt::For(for_stmt) => self.visit_for(&for_stmt),
                Stmt::While(while_stmt) => self.visit_while(&while_stmt),
                _ => false,
            };

            if !returned {
                self.errs.push(
                    ControlFlowErr {
                        fn_name: function.name,
                        fn_span: function.span,
                    }
                );
            }
        }

        (self.dead, self.errs)
    }

    /// Traverses a block of code in reverse order to verify a function returns
    /// in every branch. Returns a bool representing whether this condition is met.
    ///
    /// The process is as follows:
    ///
    /// * Start at the last statement of a block.
    /// * If the statement is a return statement, return true.
    /// * If the statement is an if statement, and it has both an if body and
    ///   an else body, then execute this process on both. If both return true,
    ///   then return true.
    /// * If the process hasn't returned, move to the previous statement.
    /// * Any time true is returned, mark the span of code after that point
    ///   as dead/unreachable.
    fn visit_block(&mut self, stmts: &Vec<Stmt<'a>>) -> bool {
        for (i, stmt) in stmts.iter().enumerate().rev() {
            let returned = match stmt {
                Stmt::Block(stmts, ..) => self.visit_block(stmts),
                Stmt::Return(..) => true,
                Stmt::If(if_stmt) => self.visit_if(if_stmt),
                Stmt::For(for_stmt) => self.visit_for(for_stmt),
                Stmt::While(while_stmt) => self.visit_while(while_stmt),
                _ => false,
            };

            if returned && i != stmts.len() - 1 {
                self.dead.push(
                    SimpleSpan::new(
                        stmts[i + 1].get_span().start,
                        stmts.last()
                            .expect("shouldn't be empty inside loop")
                            .get_span()
                            .end,
                    )
                );
                return true;
            } else if returned {
                return true;
            }
        }
        false
    }

    /// An if statement can only be guaranteed to return if it has both an if
    /// body and an else body, and if both of those bodies return.
    fn visit_if(&mut self, if_stmt: &stmt::IfStmt<'a>) -> bool {
        let (if_body, else_body) = (&if_stmt.body, &if_stmt.else_body);

        if let Some(else_body) = else_body {
            let returned_if = match &**if_body {
                Stmt::Block(stmts, ..) => self.visit_block(&stmts),
                Stmt::Return(..) => true,
                Stmt::If(if_stmt) => self.visit_if(if_stmt),
                Stmt::For(for_stmt) => self.visit_for(for_stmt),
                Stmt::While(while_stmt) => self.visit_while(while_stmt),
                _ => false,
            };

            let returned_else = match &**else_body {
                Stmt::Block(stmts, ..) => self.visit_block(&stmts),
                Stmt::Return(..) => true,
                Stmt::If(if_stmt) => self.visit_if(if_stmt),
                Stmt::For(for_stmt) => self.visit_for(for_stmt),
                Stmt::While(while_stmt) => self.visit_while(while_stmt),
                _ => false,
            };

            returned_if && returned_else
        } else {
            false
        }
    }

    fn visit_for(&mut self, for_stmt: &stmt::ForStmt<'a>) -> bool {
        match &*for_stmt.body {
            Stmt::Block(stmts, ..) => self.visit_block(&stmts),
            Stmt::Return(..) => true,
            Stmt::If(if_stmt) => self.visit_if(if_stmt),
            Stmt::For(for_stmt) => self.visit_for(for_stmt),
            Stmt::While(while_stmt) => self.visit_while(while_stmt),
            _ => false,
        }
    }

    fn visit_while(&mut self, while_stmt: &stmt::WhileStmt<'a>) -> bool {
        match &*while_stmt.body {
            Stmt::Block(stmts, ..) => self.visit_block(&stmts),
            Stmt::Return(..) => true,
            Stmt::If(if_stmt) => self.visit_if(if_stmt),
            Stmt::For(for_stmt) => self.visit_for(for_stmt),
            Stmt::While(while_stmt) => self.visit_while(while_stmt),
            _ => false,
        }
    }
}

/// Errors pointing to functions that can't be guaranteed to return in every
/// branch of execution.
pub struct ControlFlowErr<'a> {
    pub fn_name: &'a str,
    pub fn_span: SimpleSpan,
}
