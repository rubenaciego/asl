//////////////////////////////////////////////////////////////////////
//
//    TypeCheckVisitor - Walk the parser tree to do the semantic
//                       typecheck for the Asl programming language
//
//    Copyright (C) 2017-2023  Universitat Politecnica de Catalunya
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU General Public License
//    as published by the Free Software Foundation; either version 3
//    of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Affero General Public License for more details.
//
//    You should have received a copy of the GNU Affero General Public
//    License along with this library; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
//
//    contact: José Miguel Rivero (rivero@cs.upc.edu)
//             Computer Science Department
//             Universitat Politecnica de Catalunya
//             despatx Omega.110 - Campus Nord UPC
//             08034 Barcelona.  SPAIN
//
//////////////////////////////////////////////////////////////////////

#include "TypeCheckVisitor.h"
#include "antlr4-runtime.h"

#include "../common/TypesMgr.h"
#include "../common/SymTable.h"
#include "../common/TreeDecoration.h"
#include "../common/SemErrors.h"

#include <iostream>
#include <string>

// uncomment the following line to enable debugging messages with DEBUG*
// #define DEBUG_BUILD
#include "../common/debug.h"

// using namespace std;


// Constructor
TypeCheckVisitor::TypeCheckVisitor(TypesMgr       & Types,
                                   SymTable       & Symbols,
                                   TreeDecoration & Decorations,
                                   SemErrors      & Errors) :
  Types{Types},
  Symbols{Symbols},
  Decorations{Decorations},
  Errors{Errors} {
}

// Accessor/Mutator to the attribute currFunctionType
TypesMgr::TypeId TypeCheckVisitor::getCurrentFunctionTy() const {
  return currFunctionType;
}

void TypeCheckVisitor::setCurrentFunctionTy(TypesMgr::TypeId type) {
  currFunctionType = type;
}

// Methods to visit each kind of node:
//
antlrcpp::Any TypeCheckVisitor::visitProgram(AslParser::ProgramContext *ctx) {
  DEBUG_ENTER();
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  for (auto ctxFunc : ctx->function()) { 
    visit(ctxFunc);
  }
  if (Symbols.noMainProperlyDeclared())
    Errors.noMainProperlyDeclared(ctx);
  Symbols.popScope();
  Errors.print();
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitFunction(AslParser::FunctionContext *ctx) {
  DEBUG_ENTER();
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  // Symbols.print();

  std::vector<TypesMgr::TypeId> lParamsTy;
  for (auto param : ctx->params)
  {
    TypesMgr::TypeId t = getTypeDecor(param->type());
    lParamsTy.push_back(t);
  }
  TypesMgr::TypeId tRet = ctx->basic_type() ? getTypeDecor(ctx->basic_type()) : Types.createVoidTy();
  TypesMgr::TypeId tFunc = Types.createFunctionTy(lParamsTy, tRet);
  setCurrentFunctionTy(tFunc);

  visit(ctx->statements());
  Symbols.popScope();
  DEBUG_EXIT();
  return 0;
}

// antlrcpp::Any TypeCheckVisitor::visitDeclarations(AslParser::DeclarationsContext *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any TypeCheckVisitor::visitVariable_decl(AslParser::Variable_declContext *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// antlrcpp::Any TypeCheckVisitor::visitType(AslParser::TypeContext *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

antlrcpp::Any TypeCheckVisitor::visitStatements(AslParser::StatementsContext *ctx) {
  DEBUG_ENTER();
  visitChildren(ctx);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitAssignStmt(AslParser::AssignStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->left_expr());
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->left_expr());
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr());

  if ((not Types.isErrorTy(t1)) and (not Types.isErrorTy(t2)) and
      (not Types.copyableTypes(t1, t2)))
    Errors.incompatibleAssignment(ctx->ASSIGN());
  if ((not Types.isErrorTy(t1)) and (not getIsLValueDecor(ctx->left_expr())))
    Errors.nonReferenceableLeftExpr(ctx->left_expr());

  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitIfStmt(AslParser::IfStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isBooleanTy(t1)))
    Errors.booleanRequired(ctx);
  visit(ctx->ifstmt);
  if (ctx->elsestmt) visit(ctx->elsestmt);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitWhileStmt(AslParser::WhileStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isBooleanTy(t1)))
    Errors.booleanRequired(ctx);
  visit(ctx->statements());
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitReturn(AslParser::ReturnContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId t;
  TypesMgr::TypeId fType = getCurrentFunctionTy();
  TypesMgr::TypeId retType = Types.getFuncReturnType(fType);
  if (ctx->expr())
  {
    visit(ctx->expr());
    t = getTypeDecor(ctx->expr());
  }
  else
    t = Types.createVoidTy();

  if ((not Types.isErrorTy(t)) and (not Types.isErrorTy(retType)) and
      (not Types.copyableTypes(retType, t)))
    Errors.incompatibleReturn(ctx->RETURN());
  
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitProcCall(AslParser::ProcCallContext *ctx) {
  DEBUG_ENTER();
  visitChildren(ctx);
  TypesMgr::TypeId t1 = getTypeDecor(ctx->ident());
  if (Types.isErrorTy(t1)) {
    ;
  } else if (not Types.isFunctionTy(t1)) {
    Errors.isNotCallable(ctx->ident());
  }
  else {
    visitParameters(ctx->expr(), t1, ctx, ctx->ident());
  }
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitFunCall(AslParser::FunCallContext *ctx)
{
  DEBUG_ENTER();
  visitChildren(ctx);
  TypesMgr::TypeId t1 = getTypeDecor(ctx->ident());
  TypesMgr::TypeId t = Types.createErrorTy();
  if (Types.isErrorTy(t1)) {
    ;
  } else if (not Types.isFunctionTy(t1)) {
    Errors.isNotCallable(ctx->ident());
  }
  else {
    if (Types.isVoidFunction(t1))
    {
      t = Types.createErrorTy();
      Errors.isNotFunction(ctx->ident());
    }
    else t = Types.getFuncReturnType(t1);

    visitParameters(ctx->expr(), t1, ctx, ctx->ident());
  }

  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, false);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitParameters(const std::vector<AslParser::ExprContext*>& params,
  TypesMgr::TypeId ftype, antlr4::ParserRuleContext* ctx, AslParser::IdentContext* ident)
{
  DEBUG_ENTER();
  const std::vector<TypesMgr::TypeId>& paramTypes = Types.getFuncParamsTypes(ftype);

  if (paramTypes.size() != params.size())
    Errors.numberOfParameters(ident);
  else
  {
    for (size_t i = 0; i < params.size(); ++i)
    {
      TypesMgr::TypeId expected = paramTypes[i];
      TypesMgr::TypeId actual = getTypeDecor(params[i]);

      if (!Types.isErrorTy(expected) && ! Types.isErrorTy(actual) && !Types.copyableTypes(expected, actual))
        Errors.incompatibleParameter(params[i], i + 1, ctx);
    }
  }

  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitReadStmt(AslParser::ReadStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->left_expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->left_expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isPrimitiveTy(t1)) and
      (not Types.isFunctionTy(t1)))
    Errors.readWriteRequireBasic(ctx);
  if ((not Types.isErrorTy(t1)) and (not getIsLValueDecor(ctx->left_expr())))
    Errors.nonReferenceableExpression(ctx);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitWriteExpr(AslParser::WriteExprContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isPrimitiveTy(t1)))
    Errors.readWriteRequireBasic(ctx);
  DEBUG_EXIT();
  return 0;
}

// antlrcpp::Any TypeCheckVisitor::visitWriteString(AslParser::WriteStringContext *ctx) {
//   DEBUG_ENTER();
//   antlrcpp::Any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

antlrcpp::Any TypeCheckVisitor::visitLeftIdent(AslParser::LeftIdentContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->ident());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->ident());
  bool lvalue = getIsLValueDecor(ctx->ident());
  putTypeDecor(ctx, t1);
  putIsLValueDecor(ctx, lvalue);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitLeftIndexing(AslParser::LeftIndexingContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->left_expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->left_expr());
  visit(ctx->expr());
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr());
  TypesMgr::TypeId t;

  if ((not Types.isErrorTy(t2)) and (not Types.isIntegerTy(t2)))
    Errors.nonIntegerIndexInArrayAccess(ctx->expr());

  if ((not Types.isErrorTy(t1)) and (not Types.isArrayTy(t1))) {
    Errors.nonArrayInArrayAccess(ctx);
    t = Types.createErrorTy();
  }
  else if (Types.isArrayTy(t1))
    t = Types.getArrayElemType(t1);
  else
    t = Types.createErrorTy();
  
  bool lvalue = getIsLValueDecor(ctx->left_expr());
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, lvalue);

  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitLeftNested(AslParser::LeftNestedContext *ctx)
{
  DEBUG_ENTER();
  visit(ctx->left_expr());
  TypesMgr::TypeId t = getTypeDecor(ctx->left_expr());
  bool lvalue = getIsLValueDecor(ctx->left_expr());
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, lvalue);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitIndexing(AslParser::IndexingContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr(0));
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  visit(ctx->expr(1));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  TypesMgr::TypeId t;

  if ((not Types.isErrorTy(t2)) and (not Types.isIntegerTy(t2)))
    Errors.nonIntegerIndexInArrayAccess(ctx->expr(1));

  if ((not Types.isErrorTy(t1)) and (not Types.isArrayTy(t1))) {
    Errors.nonArrayInArrayAccess(ctx);
    t = Types.createErrorTy();
  }
  else if (Types.isArrayTy(t1))
    t = Types.getArrayElemType(t1);
  else
    t = Types.createErrorTy();
  
  bool lvalue = getIsLValueDecor(ctx->expr(0));
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, lvalue);

  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitUnary(AslParser::UnaryContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t = getTypeDecor(ctx->expr());

  if (not Types.isErrorTy(t) and ((ctx->NOT() and not Types.isBooleanTy(t)) or
                                  (not ctx->NOT() and not Types.isNumericTy(t))))
  {
    Errors.incompatibleOperator(ctx->op);
    t = Types.createErrorTy();
  }
  
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, false);
  DEBUG_EXIT();
  return 0;  
}

antlrcpp::Any TypeCheckVisitor::visitArithmetic(AslParser::ArithmeticContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr(0));
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  visit(ctx->expr(1));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  TypesMgr::TypeId t = Types.createIntegerTy();

  if (ctx->MOD()) {
    if (((not Types.isErrorTy(t1)) and (not Types.isIntegerTy(t1))) or
       ((not Types.isErrorTy(t2)) and (not Types.isIntegerTy(t2))))
      Errors.incompatibleOperator(ctx->op);
  }
  else
  {
    if (((not Types.isErrorTy(t1)) and (not Types.isNumericTy(t1))) or
        ((not Types.isErrorTy(t2)) and (not Types.isNumericTy(t2))))
      Errors.incompatibleOperator(ctx->op);
    if (Types.isFloatTy(t1) or Types.isFloatTy(t2)) t = Types.createFloatTy();
  }

  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, false);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitRelational(AslParser::RelationalContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr(0));
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  visit(ctx->expr(1));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  std::string oper = ctx->op->getText();

  if ((not Types.isErrorTy(t1)) and (not Types.isErrorTy(t2)) and
      (not Types.comparableTypes(t1, t2, oper)))
    Errors.incompatibleOperator(ctx->op);
  
  TypesMgr::TypeId t = Types.createBooleanTy();
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, false);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitLogical(AslParser::LogicalContext *ctx)
{
  DEBUG_ENTER();
  visit(ctx->expr(0));
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  visit(ctx->expr(1));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));

  if (((not Types.isErrorTy(t1)) and (not Types.isBooleanTy(t1))) or
      ((not Types.isErrorTy(t2)) and (not Types.isBooleanTy(t2))))
    Errors.incompatibleOperator(ctx->op);

  TypesMgr::TypeId t = Types.createBooleanTy();
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, false);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitNested(AslParser::NestedContext *ctx)
{
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t = getTypeDecor(ctx->expr());
  bool lvalue = getIsLValueDecor(ctx->expr());
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, lvalue);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitValue(AslParser::ValueContext *ctx) {
  DEBUG_ENTER();
  if (ctx->INTVAL()) {
    TypesMgr::TypeId t = Types.createIntegerTy();
    putTypeDecor(ctx, t);
  }
  else if (ctx->FLOATVAL()) {
    TypesMgr::TypeId t = Types.createFloatTy();
    putTypeDecor(ctx, t);
  }
  else if (ctx->BOOLVAL()) {
    TypesMgr::TypeId t = Types.createBooleanTy();
    putTypeDecor(ctx, t);
  }
  else if (ctx->CHARVAL()) {
    TypesMgr::TypeId t = Types.createCharacterTy();
    putTypeDecor(ctx, t);
  }

  putIsLValueDecor(ctx, false);

  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitExprIdent(AslParser::ExprIdentContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->ident());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->ident());
  bool lvalue = getIsLValueDecor(ctx->ident());
  putTypeDecor(ctx, t1);
  putIsLValueDecor(ctx, lvalue);
  DEBUG_EXIT();
  return 0;
}

antlrcpp::Any TypeCheckVisitor::visitIdent(AslParser::IdentContext *ctx) {
  DEBUG_ENTER();
  std::string ident = ctx->getText();
  if (Symbols.findInStack(ident) == -1) {
    Errors.undeclaredIdent(ctx->ID());
    TypesMgr::TypeId te = Types.createErrorTy();
    putTypeDecor(ctx, te);
    putIsLValueDecor(ctx, true);
  }
  else {
    TypesMgr::TypeId t1 = Symbols.getType(ident);
    putTypeDecor(ctx, t1);
    if (Symbols.isFunctionClass(ident))
      putIsLValueDecor(ctx, false);
    else
      putIsLValueDecor(ctx, true);
  }

  DEBUG_EXIT();
  return 0;
}

// Getters for the necessary tree node atributes:
//   Scope, Type ans IsLValue
SymTable::ScopeId TypeCheckVisitor::getScopeDecor(antlr4::ParserRuleContext *ctx) {
  return Decorations.getScope(ctx);
}
TypesMgr::TypeId TypeCheckVisitor::getTypeDecor(antlr4::ParserRuleContext *ctx) {
  return Decorations.getType(ctx);
}
bool TypeCheckVisitor::getIsLValueDecor(antlr4::ParserRuleContext *ctx) {
  return Decorations.getIsLValue(ctx);
}

// Setters for the necessary tree node attributes:
//   Scope, Type ans IsLValue
void TypeCheckVisitor::putScopeDecor(antlr4::ParserRuleContext *ctx, SymTable::ScopeId s) {
  Decorations.putScope(ctx, s);
}
void TypeCheckVisitor::putTypeDecor(antlr4::ParserRuleContext *ctx, TypesMgr::TypeId t) {
  Decorations.putType(ctx, t);
}
void TypeCheckVisitor::putIsLValueDecor(antlr4::ParserRuleContext *ctx, bool b) {
  Decorations.putIsLValue(ctx, b);
}
