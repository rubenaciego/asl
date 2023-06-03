//////////////////////////////////////////////////////////////////////
//
//    CodeGenVisitor - Walk the parser tree to do
//                     the generation of code
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

#include "CodeGenVisitor.h"
#include "antlr4-runtime.h"

#include "../common/TypesMgr.h"
#include "../common/SymTable.h"
#include "../common/TreeDecoration.h"
#include "../common/code.h"

#include <string>
#include <cstddef>    // std::size_t

#include <typeinfo>

// uncomment the following line to enable debugging messages with DEBUG*
// #define DEBUG_BUILD
#include "../common/debug.h"

// using namespace std;


// Constructor
CodeGenVisitor::CodeGenVisitor(TypesMgr       & Types,
                               SymTable       & Symbols,
                               TreeDecoration & Decorations) :
  Types{Types},
  Symbols{Symbols},
  Decorations{Decorations} {
}

// Accessor/Mutator to the attribute currFunctionType
TypesMgr::TypeId CodeGenVisitor::getCurrentFunctionTy() const {
  return currFunctionType;
}

void CodeGenVisitor::setCurrentFunctionTy(TypesMgr::TypeId type) {
  currFunctionType = type;
}

// Methods to visit each kind of node:
//
antlrcpp::Any CodeGenVisitor::visitProgram(AslParser::ProgramContext *ctx) {
  DEBUG_ENTER();
  code my_code;
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  for (auto ctxFunc : ctx->function()) { 
    subroutine subr = visit(ctxFunc);
    my_code.add_subroutine(subr);
  }
  Symbols.popScope();
  DEBUG_EXIT();
  return my_code;
}

antlrcpp::Any CodeGenVisitor::visitFunction(AslParser::FunctionContext *ctx) {
  DEBUG_ENTER();
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  subroutine subr(ctx->ID()->getText());
  codeCounters.reset();

  if (ctx->basic_type())
  {
    TypesMgr::TypeId retType = getTypeDecor(ctx->basic_type());
    subr.add_param("_result", Types.to_string_basic(retType), Types.isArrayTy(retType));
  }

  for (auto param : ctx->param_decl())
  {
    TypesMgr::TypeId pType = getTypeDecor(param->type());
    var&& p = visit(param);
    subr.add_param(p.name, Types.to_string_basic(pType), Types.isArrayTy(pType));
  }

  std::vector<var> && lvars = visit(ctx->declarations());
  for (auto & onevar : lvars) {
    subr.add_var(onevar);
  }
  instructionList && code = visit(ctx->statements());
  code = code || instruction(instruction::RETURN());
  subr.set_instructions(code);
  Symbols.popScope();
  DEBUG_EXIT();
  return subr;
}

antlrcpp::Any CodeGenVisitor::visitDeclarations(AslParser::DeclarationsContext *ctx) {
  DEBUG_ENTER();
  std::vector<var> lvars;
  for (auto & varDeclCtx : ctx->variable_decl()) {
    std::vector<var>&& vardecl = visit(varDeclCtx);
    lvars.insert(lvars.end(), vardecl.begin(), vardecl.end());
  }
  DEBUG_EXIT();
  return lvars;
}

antlrcpp::Any CodeGenVisitor::visitVariable_decl(AslParser::Variable_declContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId   t1 = getTypeDecor(ctx->type());
  std::size_t      size = Types.getSizeOfType(t1);
  std::vector<var> lvars;
  for (size_t i = 0; i < ctx->ID().size(); ++i)
    lvars.emplace_back(ctx->ID(i)->getText(), Types.to_string_basic(t1), size);
  DEBUG_EXIT();
  return lvars;
}

antlrcpp::Any CodeGenVisitor::visitParam_decl(AslParser::Param_declContext *ctx)
{
  DEBUG_ENTER();
  TypesMgr::TypeId   t1 = getTypeDecor(ctx->type());
  std::size_t      size = Types.getSizeOfType(t1);
  var v(ctx->ID()->getText(), Types.to_string(t1), size);
  DEBUG_EXIT();
  return v;
}

antlrcpp::Any CodeGenVisitor::visitStatements(AslParser::StatementsContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  for (auto stCtx : ctx->statement()) {
    instructionList && codeS = visit(stCtx);
    code = code || codeS;
  }
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitAssignStmt(AslParser::AssignStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  
  CodeAttribs &&        codAtsE1 = visit(ctx->left_expr());
  std::string           addr1 = codAtsE1.addr;
  std::string           offs1 = codAtsE1.offs;
  instructionList &     code1 = codAtsE1.code;
  TypesMgr::TypeId t1 = getTypeDecor(ctx->left_expr());
  
  CodeAttribs &&        codAtsE2 = visit(ctx->expr());
  std::string           addr2 = codAtsE2.addr;
  instructionList &     code2 = codAtsE2.code;
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr());
  
  code = code1 || code2;
  
  if (Types.isArrayTy(t1) && Types.isArrayTy(t2)) {
	  std::string tempI = "%"+codeCounters.newTEMP();
	  std::string tempL = "%"+codeCounters.newTEMP();
	  std::string tempV = "%"+codeCounters.newTEMP();
	  std::string tempIncr = "%"+codeCounters.newTEMP();
	  std::string tempCmp = "%"+codeCounters.newTEMP();
	  
	  std::string label = codeCounters.newLabelWHILE();
	  std::string labelWhile = "while"+label;
	  std::string labelEndWhile = "endwhile"+label;
	  
	  code = code
		|| instruction::ILOAD(tempI, "0")
		|| instruction::ILOAD(tempL, std::to_string(Types.getArraySize(t1)))
		|| instruction::ILOAD(tempIncr, "1")
		
		|| instruction::LABEL(labelWhile)
		|| instruction::LT(tempCmp, tempI, tempL)
		|| instruction::FJUMP(tempCmp, labelEndWhile)
		
		|| instruction::LOADX(tempV, addr2, tempI)
		|| instruction::XLOAD(addr1, tempI, tempV)
		|| instruction::ADD(tempI, tempI, tempIncr)
		
		|| instruction::UJUMP(labelWhile)
		|| instruction::LABEL(labelEndWhile);
  }
  else {
	  std::string temp = "%"+codeCounters.newTEMP();
	  if (Types.isFloatTy(t1) && Types.isIntegerTy(t2))
		  code = code || instruction::FLOAT(temp, addr2);
	  else temp = addr2;
	  
	  if (offs1 != "") code = code || instruction::XLOAD(addr1, offs1, temp);
	  else code = code || instruction::LOAD(addr1, temp);
  }

  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitIfStmt(AslParser::IfStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs     && codAtsE = visit(ctx->expr());
  std::string          addr1 = codAtsE.addr;
  instructionList &    code1 = codAtsE.code;
  instructionList &&   code2 = visit(ctx->ifstmt);
  std::string label = codeCounters.newLabelIF();
  std::string labelEndIf = "endif"+label;

  if (ctx->elsestmt)
  {
    instructionList && codeElse = visit(ctx->elsestmt);
    std::string labelElse = "else"+label;
    code = code1 || instruction::FJUMP(addr1, labelElse) ||
           code2 || instruction::UJUMP(labelEndIf) ||
           instruction::LABEL(labelElse) ||
           codeElse || instruction::LABEL(labelEndIf);
  
  }
  else
  {
    code = code1 || instruction::FJUMP(addr1, labelEndIf) ||
          code2 || instruction::LABEL(labelEndIf);
  }
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitReturn(AslParser::ReturnContext *ctx)
{
  DEBUG_ENTER();
  instructionList code;
  if (ctx->expr()) {
    CodeAttribs     && codAt1 = visit(ctx->expr());
    std::string         addr1 = codAt1.addr;
    instructionList &   code1 = codAt1.code;
    code = code1 || instruction::LOAD("_result", addr1);
  }
  code = code || instruction::RETURN();
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWhileStmt(AslParser::WhileStmtContext *ctx)
{
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs     && codAtsE = visit(ctx->expr());
  std::string          addr1 = codAtsE.addr;
  instructionList &    code1 = codAtsE.code;
  instructionList &&   code2 = visit(ctx->statements());
  std::string label = codeCounters.newLabelWHILE();
  std::string labelWhile = "while"+label;
  std::string labelEndWhile = "endwhile"+label;
  code = instruction::LABEL(labelWhile) ||
         code1 || instruction::FJUMP(addr1, labelEndWhile) ||
         code2 || instruction::UJUMP(labelWhile) ||
         instruction::LABEL(labelEndWhile);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitProcCall(AslParser::ProcCallContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->ident());
  instructionList & code = codAts.code;
  TypesMgr::TypeId t = getTypeDecor(ctx->ident());
  
  if (!Types.isVoidFunction(t)) code = code || instruction::PUSH();
  
  size_t i = 0;
  for (auto expr : ctx->expr()) {
    CodeAttribs && codAts2 = visit(expr);
    code = code || codAts2.code;
    std::string addr = codAts2.addr;

    TypesMgr::TypeId p = Types.getParameterType(t, i);
    TypesMgr::TypeId e = getTypeDecor(expr);
    
    if (Types.isFloatTy(p) && !Types.isFloatTy(e)) {
      std::string temp = "%"+codeCounters.newTEMP();
      code = code || instruction::FLOAT(temp, addr);
      addr = temp;
    }
    else if (Types.isArrayTy(p) && !Symbols.isParameterClass(ctx->expr(i)->getText())) {
      std::string temp = "%"+codeCounters.newTEMP();
      code = code || instruction::ALOAD(temp, addr);
      addr = temp;
    }

    code = code || instruction::PUSH(addr);
    ++i;
  }
  
  std::string name = ctx->ident()->getText();
  code = code || instruction::CALL(name); 

  for (i = 0; i < ctx->expr().size(); ++i)
    code = code || instruction::POP();
  
  if (!Types.isVoidFunction(t)) code = code || instruction::POP();
  
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitFunCall(AslParser::FunCallContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->ident());
  instructionList & code = codAts.code;
  TypesMgr::TypeId t = getTypeDecor(ctx->ident());
  
  code = code || instruction::PUSH();
  
  size_t i = 0;
  for (auto expr : ctx->expr()) {
    CodeAttribs && codAts2 = visit(expr);
    code = code || codAts2.code;
    std::string addr = codAts2.addr;

    TypesMgr::TypeId p = Types.getParameterType(t, i);
    TypesMgr::TypeId e = getTypeDecor(expr);
    
    if (Types.isFloatTy(p) && !Types.isFloatTy(e)) {
      std::string temp = "%"+codeCounters.newTEMP();
      code = code || instruction::FLOAT(temp, addr);
      addr = temp;
    }
    else if (Types.isArrayTy(p) && !Symbols.isParameterClass(ctx->expr(i)->getText())) {
      std::string temp = "%"+codeCounters.newTEMP();
      code = code || instruction::ALOAD(temp, addr);
      addr = temp;
    }

    code = code || instruction::PUSH(addr);
    ++i;
  }
  
  std::string name = ctx->ident()->getText();
  code = code || instruction::CALL(name); 

  for (i = 0; i < ctx->expr().size(); ++i)
    code = code || instruction::POP();
  
  std::string temp = "%"+codeCounters.newTEMP();
  code = code || instruction::POP(temp);
  codAts.addr = temp;
  
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitReadStmt(AslParser::ReadStmtContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAtsE = visit(ctx->left_expr());
  std::string          addr1 = codAtsE.addr;
  std::string          offs1 = codAtsE.offs;
  instructionList &    code1 = codAtsE.code;
  instructionList &     code = code1;
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->left_expr());
  std::string temp = (offs1 != "") ? ("%"+codeCounters.newTEMP()) : addr1;

  if (Types.isFloatTy(tid1)) code = code1 || instruction::READF(temp);
  else if (Types.isCharacterTy(tid1)) code = code1 || instruction::READC(temp);
  else if (Types.isIntegerTy(tid1)) code = code1 || instruction::READI(temp);
  else if (Types.isBooleanTy(tid1))
  {
    std::string t0 = "%"+codeCounters.newTEMP();
    std::string t1 = "%"+codeCounters.newTEMP();
    std::string t2 = "%"+codeCounters.newTEMP();
    code = code1 || instruction::READI(t1) ||
           instruction::LOAD(t0, "0") ||
           instruction::EQ(t2, t1, t0) ||
           instruction::NOT(temp, t2);
  }
  
  if (offs1 != "")
    code = code || instruction::XLOAD(addr1, offs1, temp);

  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWriteExpr(AslParser::WriteExprContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr());
  std::string         addr1 = codAt1.addr;
  // std::string         offs1 = codAt1.offs;
  instructionList &   code1 = codAt1.code;
  instructionList &    code = code1;
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->expr());

  if (Types.isFloatTy(tid1)) code = code1 || instruction::WRITEF(addr1);
  else if (Types.isCharacterTy(tid1)) code = code1 || instruction::WRITEC(addr1);
  else code = code1 || instruction::WRITEI(addr1);

  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWriteString(AslParser::WriteStringContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string s = ctx->STRING()->getText();
  code = code || instruction::WRITES(s);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitLeftIdent(AslParser::LeftIdentContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->ident());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitNested(AslParser::NestedContext *ctx)
{
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->expr());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitLeftNested(AslParser::LeftNestedContext *ctx)
{
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->left_expr());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitLeftIndexing(AslParser::LeftIndexingContext *ctx)
{
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->left_expr());
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;  
  CodeAttribs     && codAt2 = visit(ctx->expr());
  std::string         addr2 = codAt2.addr;
  instructionList &   code2 = codAt2.code;
  instructionList &&   code = code1 || code2;

  CodeAttribs codAt(addr1, addr2, code);
  DEBUG_EXIT();
  return codAt;
}

antlrcpp::Any CodeGenVisitor::visitIndexing(AslParser::IndexingContext *ctx)
{
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr(0));
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;  
  CodeAttribs     && codAt2 = visit(ctx->expr(1));
  std::string         addr2 = codAt2.addr;
  instructionList &   code2 = codAt2.code;
  instructionList &&   code = code1 || code2;

  std::string temp = "%"+codeCounters.newTEMP();
  code = code || instruction::LOADX(temp, addr1, addr2);
  CodeAttribs codAt(temp, "", code);
  DEBUG_EXIT();
  return codAt;
}

antlrcpp::Any CodeGenVisitor::visitArithmetic(AslParser::ArithmeticContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr(0));
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;  
  CodeAttribs     && codAt2 = visit(ctx->expr(1));
  std::string         addr2 = codAt2.addr;
  instructionList &   code2 = codAt2.code;
  instructionList &&   code = code1 || code2;
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  TypesMgr::TypeId  t = getTypeDecor(ctx);

  std::string temp = "%"+codeCounters.newTEMP();

  if (Types.isFloatTy(t))
  {
    if (Types.isIntegerTy(t1))
    {
      std::string addrTemp = addr1;
      addr1 = "%"+codeCounters.newTEMP();
      code = code || instruction::FLOAT(addr1, addrTemp);
    }
    if (Types.isIntegerTy(t2))
    {
      std::string addrTemp = addr2;
      addr2 = "%"+codeCounters.newTEMP();
      code = code || instruction::FLOAT(addr2, addrTemp);
    }

    if (ctx->MUL())
      code = code || instruction::FMUL(temp, addr1, addr2);
    else if (ctx->DIV())
      code = code || instruction::FDIV(temp, addr1, addr2);
    else if (ctx->PLUS())
      code = code || instruction::FADD(temp, addr1, addr2);
    else if (ctx->SUB())
      code = code || instruction::FSUB(temp, addr1, addr2);
  }
  else
  {
    if (ctx->MUL())
      code = code || instruction::MUL(temp, addr1, addr2);
    else if (ctx->DIV())
      code = code || instruction::DIV(temp, addr1, addr2);
    else if (ctx->MOD())
    {
      std::string tempDiv = "%"+codeCounters.newTEMP();
      std::string tempMul = "%"+codeCounters.newTEMP();
      code = code || instruction::DIV(tempDiv, addr1, addr2)
		              || instruction::MUL(tempMul, tempDiv, addr2)
		              || instruction::SUB(temp, addr1, tempMul);
    }
    else if (ctx->PLUS())
      code = code || instruction::ADD(temp, addr1, addr2);
    else if (ctx->SUB())
      code = code || instruction::SUB(temp, addr1, addr2);
  }
  
  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitUnary(AslParser::UnaryContext *ctx)
{
  DEBUG_ENTER();
  CodeAttribs&& codeAt = visit(ctx->expr());

  if (ctx->NOT())
  {
    std::string temp = "%"+codeCounters.newTEMP();
    codeAt.code = codeAt.code || instruction::NOT(temp, codeAt.addr);
    codeAt.addr = temp;
  }
  else if (ctx->SUB())
  {
    TypesMgr::TypeId t = getTypeDecor(ctx->expr());
    std::string temp = "%"+codeCounters.newTEMP();
    if (Types.isFloatTy(t)) codeAt.code = codeAt.code || instruction::FNEG(temp, codeAt.addr);
    else codeAt.code = codeAt.code || instruction::NEG(temp, codeAt.addr);
    codeAt.addr = temp;
  }

  DEBUG_EXIT();
  return codeAt;
}

antlrcpp::Any CodeGenVisitor::visitRelational(AslParser::RelationalContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr(0));
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));

  CodeAttribs     && codAt2 = visit(ctx->expr(1));
  std::string         addr2 = codAt2.addr;
  instructionList &   code2 = codAt2.code;
  instructionList &&   code = code1 || code2;
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));

  std::string temp = "%"+codeCounters.newTEMP();

  if (Types.isFloatTy(t1) || Types.isFloatTy(t2))
  {
    if (!Types.isFloatTy(t1)) {
      std::string tempf1 = "%"+codeCounters.newTEMP();
      code = code || instruction::FLOAT(tempf1, addr1);
      addr1 = tempf1;
    }
    else if (!Types.isFloatTy(t2)) {
      std::string tempf2 = "%"+codeCounters.newTEMP();
      code = code || instruction::FLOAT(tempf2, addr2);
      addr2 = tempf2;
    }

    if (ctx->EQUAL())
      code = code || instruction::FEQ(temp, addr1, addr2);
    else if (ctx->NEQUAL()) {
      code = code || instruction::FEQ(temp, addr1, addr2);
      code = code || instruction::NOT(temp, temp);
    }
    else if (ctx->LOWER())
      code = code || instruction::FLT(temp, addr1, addr2);
    else if (ctx->GREATER())
      code = code || instruction::FLT(temp, addr2, addr1);
    else if (ctx->LOWERERQ())
      code = code || instruction::FLE(temp, addr1, addr2);
    else if (ctx->GREATEREQ())
      code = code || instruction::FLE(temp, addr2, addr1);
  }
  else {
    if (ctx->EQUAL())
      code = code || instruction::EQ(temp, addr1, addr2);
    else if (ctx->NEQUAL()) {
      code = code || instruction::EQ(temp, addr1, addr2);
      code = code || instruction::NOT(temp, temp);
    }
    else if (ctx->LOWER())
      code = code || instruction::LT(temp, addr1, addr2);
    else if (ctx->GREATER())
      code = code || instruction::LT(temp, addr2, addr1);
    else if (ctx->LOWERERQ())
      code = code || instruction::LE(temp, addr1, addr2);
    else if (ctx->GREATEREQ())
      code = code || instruction::LE(temp, addr2, addr1);
  }
  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
  }

  antlrcpp::Any CodeGenVisitor::visitLogical(AslParser::LogicalContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = visit(ctx->expr(0));
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;
  CodeAttribs     && codAt2 = visit(ctx->expr(1));
  std::string         addr2 = codAt2.addr;
  instructionList &   code2 = codAt2.code;
  instructionList &&   code = code1 || code2;
  std::string temp = "%"+codeCounters.newTEMP();
  if (ctx->AND()) code = code || instruction::AND(temp, addr1, addr2);
  else code = code || instruction::OR(temp, addr1, addr2);
  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitValue(AslParser::ValueContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string temp = "%"+codeCounters.newTEMP();
  
  if (ctx->CHARVAL()) code = instruction::CHLOAD(temp, ctx->getText().substr(1, ctx->getText().size() - 2));
  else if (ctx->FLOATVAL()) code = instruction::FLOAD(temp, ctx->getText());
  else if (ctx->BOOLVAL()) {
    if (ctx->getText() == "true") code = instruction::ILOAD(temp, "1");
    else code = instruction::ILOAD(temp, "0");
  }
  else code = instruction::ILOAD(temp, ctx->getText());

  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitExprIdent(AslParser::ExprIdentContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->ident());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitIdent(AslParser::IdentContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs codAts(ctx->ID()->getText(), "", instructionList());
  DEBUG_EXIT();
  return codAts;
}


// Getters for the necessary tree node atributes:
//   Scope and Type
SymTable::ScopeId CodeGenVisitor::getScopeDecor(antlr4::ParserRuleContext *ctx) const {
  return Decorations.getScope(ctx);
}
TypesMgr::TypeId CodeGenVisitor::getTypeDecor(antlr4::ParserRuleContext *ctx) const {
  return Decorations.getType(ctx);
}


// Constructors of the class CodeAttribs:
//
CodeGenVisitor::CodeAttribs::CodeAttribs(const std::string & addr,
                                         const std::string & offs,
                                         instructionList & code) :
  addr{addr}, offs{offs}, code{code} {
}

CodeGenVisitor::CodeAttribs::CodeAttribs(const std::string & addr,
                                         const std::string & offs,
                                         instructionList && code) :
  addr{addr}, offs{offs}, code{code} {
}
