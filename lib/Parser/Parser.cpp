#include "chocopy-llvm/Parser/Parser.h"
#include "chocopy-llvm/AST/ASTContext.h"
#include "chocopy-llvm/Basic/Diagnostic.h"
#include "chocopy-llvm/Sema/Scope.h"
#include "chocopy-llvm/Sema/Sema.h"

#include <llvm/ADT/APInt.h>

#include <stack>

namespace chocopy {
Parser::Parser(ASTContext &C, Lexer &Lex, Sema &Acts)
    : Diags(Lex.getDiagnostics()), Context(C), TheLexer(Lex) {}

Program *Parser::parse() {
  Program *P = parseProgramDemo();
  return P;
}

bool Parser::consumeToken(tok::TokenKind ExpectedTok) {
  if (Tok.is(ExpectedTok)) {
    TheLexer.lex(Tok);
    return true;
  }
  return false;
}

bool Parser::consumeToken() {
  TheLexer.lex(Tok);
  return true;
}

bool Parser::expect(tok::TokenKind ExpectedTok) {
  if (Tok.is(ExpectedTok))
    return true;
  Diags.emitError(Tok.getLocation().Start, diag::err_near_token) << Tok;
  Diags.emitError(Tok.getLocation().Start, diag::err_expected)
      << tok::getTokenName(ExpectedTok);
  return false;
}

bool Parser::expectAndConsume(tok::TokenKind ExpectedTok) {
  return expect(ExpectedTok) && consumeToken();
}

void Parser::skipToNextLine() {
  while (!Tok.is(tok::eof) && !consumeToken(tok::NEWLINE))
    consumeToken();
}

void Parser::emitUnexpected() {
  Diags.emitError(Tok.getLocation().Start, diag::err_unexpected) << Tok;
}

const Token &Parser::getLookAheadToken(int N) {
  assert(N);
  return TheLexer.LookAhead(N - 1);
}

Program *Parser::parseProgramDemo() {
  DeclList Declarations;
  StmtList Statements;

  auto IsVarDef = [this](Token &Tok) {
    return Tok.is(tok::identifier) &&
           TheLexer.LookAhead(0).is(tok::colon);
  };

  auto IsFuncDef = [this](Token &Tok) {
    return Tok.is(tok::kw_def) &&
           TheLexer.LookAhead(0).is(tok::identifier);
  };

  auto IsClassDef = [this](Token &Tok) {
    return Tok.is(tok::kw_class) &&
           TheLexer.LookAhead(0).is(tok::identifier);
  };

  consumeToken();
  while (IsVarDef(Tok) || IsFuncDef(Tok) || IsClassDef(Tok)) { 
    if (IsVarDef(Tok))
      Declarations.push_back(parseVarDef());
    else if (IsFuncDef(Tok))
      Declarations.push_back(parseFuncDef());
    else if (IsClassDef(Tok))
      Declarations.push_back(parseClassDef());
    else
      skipToNextLine();
  }

  while (Tok.isNot(tok::eof)) {
    if (Stmt *S = parseStmtDemo())
      Statements.push_back(S);
    else
      skipToNextLine();
  }

  return Context.createProgram(Declarations, Statements);
}

StmtList Parser::parseBlockStmt() {
  StmtList Block;
  if (!expectAndConsume(tok::NEWLINE))
    return Block;

  if (!expectAndConsume(tok::INDENT))
    return Block;

  Stmt *S = nullptr;
  do {
    S = parseStmtDemo();
    if (!S)
      break;
    Block.push_back(S);
  } while (Tok.isNot(tok::DEDENT));

  if (!expectAndConsume(tok::DEDENT))
    return Block;

  return Block;
}

Stmt *Parser::parseForStmt() {
  SMLoc SLoc = Tok.getLocation().Start;

  consumeToken();

  if (!expect(tok::identifier))
    return nullptr;

  SymbolInfo *Name = Tok.getSymbolInfo();
  SMRange NameLoc = Tok.getLocation();

  consumeToken();

  if (!expectAndConsume(tok::kw_in))
    return nullptr;

  Expr *E = parseExprDemo();
  if (!E)
    return nullptr;

  if (!expectAndConsume(tok::colon))
    return nullptr;

  StmtList Block = parseBlockStmt();

  if (Block.empty()) {
    return nullptr;
  }

  SMLoc ELoc = Block.back()->getLocation().End;
  SMRange Loc(SLoc, ELoc);
  DeclRef *V = Context.createDeclRef(NameLoc, Name);

  return Context.createForStmt(Loc, V, E, Block);
}

Stmt *Parser::parseWhileStmt() {
  SMLoc SLoc = Tok.getLocation().Start;

  consumeToken();

  Expr *E = parseExprDemo();
  if (!E)
    return nullptr;

  if (!expectAndConsume(tok::colon))
    return nullptr;

  StmtList Block = parseBlockStmt();

  if (Block.empty())
    return nullptr;

  SMLoc ELoc = Block.back()->getLocation().End;;
  

  SMRange Loc(SLoc, ELoc);

  return Context.createWhileStmt(Loc, E, Block);
}

Stmt *Parser::parseIfStmt() {
  SMLoc SLoc = Tok.getLocation().Start;

  consumeToken();

  Expr *E = parseExprDemo();
  if (!E)
    return nullptr;

  if (!expectAndConsume(tok::colon))
    return nullptr;

  StmtList Block = parseBlockStmt();

  if (Block.empty())
    return nullptr;

  SMLoc LastLoc = Block.back()->getLocation().End;
  
  while (Tok.is(tok::kw_elif)) {
    SMLoc ElifLoc = Tok.getLocation().Start;
    consumeToken();

    Expr *Cond = parseExprDemo();
    if (!Cond)
      return nullptr;

    if (!expectAndConsume(tok::colon)) {
      return nullptr;
    }
    
    StmtList ElifBlock = parseBlockStmt();

    SMLoc CondLoc;
    if (!ElifBlock.empty()) {
      CondLoc = ElifBlock.back()->getLocation().End;
    } else {
      return nullptr;
    }

    SMRange Loc(ElifLoc, CondLoc);
    LastLoc = CondLoc;

    StmtList ElseBlock;

    Block.push_back(Context.createIfStmt(Loc, Cond, ElifBlock, ElseBlock));
  }

  StmtList ElseBlock;

  if (Tok.is(tok::kw_else)) {
    consumeToken();
    if (!expectAndConsume(tok::colon))
      return nullptr;

    ElseBlock = parseBlockStmt();

    if (ElseBlock.empty()) {
      return nullptr;
    }
  }

  SMLoc ELoc;
  if (!ElseBlock.empty()) {
    ELoc = ElseBlock.back()->getLocation().End;
  } else {
    ELoc = LastLoc;
  }

  SMRange Loc(SLoc, ELoc);

  return Context.createIfStmt(Loc, E, Block, ElseBlock);
}

Stmt *Parser::parseStmtDemo() {
  if (Tok.is(tok::kw_for)) { // for_stmt
    return parseForStmt();
  }

  if (Tok.is(tok::kw_while)) { // while_stmt
    return parseWhileStmt();
  }

  if (Tok.is(tok::kw_if)) { // if_stmt
    return parseIfStmt();
  }

  if (Stmt *S = parseSimpleStmt()) // simple_stmt
    if (expectAndConsume(tok::NEWLINE))
      return S;

  return nullptr;
}

Stmt *Parser::parseReturnStmt() {
  SMLoc SLoc = Tok.getLocation().Start;

  consumeToken();

  Expr *E = nullptr;

  if (!Tok.is(tok::NEWLINE)) {
    E = parseExprDemo();
    if (!E)
      return nullptr;
  }

  SMLoc ELoc = Tok.getLocation().End;
  SMRange Loc(SLoc, ELoc);
    
  return Context.createReturnStmt(Loc, E);
}

Stmt *Parser::parseSimpleStmt() {
  if (Tok.is(tok::kw_return)) { // return_stmt
    return parseReturnStmt();
  }

  if (Tok.is(tok::kw_pass)) { // pass
    consumeToken();
    return nullptr;
  }

  if (Stmt *S = parseAssignOrExprDemo()) { // others
    return S;
  }

  return nullptr;
}

Stmt *Parser::parseAssignOrExprDemo() {
  auto IsTarget = [](const Expr *E) { return llvm::isa<DeclRef>(E); }; // TODO: look at this

  SMLoc SLoc = Tok.getLocation().Start;
  ExprList Targets;
  Expr *E = nullptr;
  do {
    if (E)
      Targets.push_back(E);

    E = parseExprDemo();
    if (!E)
      return nullptr;
  } while (IsTarget(E) && consumeToken(tok::equal));

  if (!expect(tok::NEWLINE))
    return nullptr;

  SMLoc ELoc = Tok.getLocation().Start;
  SMRange Loc(SLoc, ELoc);
  if (!Targets.empty())
    return Context.createAssignStmt(Loc, Targets, E);

  return Context.createExprStmt(Loc, E);
}

Expr *Parser::parseProjectionExpr() {
  SMLoc SLoc = Tok.getLocation().Start;

  Expr *E = parseBinaryAddOrSubDemo();
  if (!E)
    return nullptr;
  
  auto IsContinue = [](Token &Tok) {
    return Tok.is(tok::less) || Tok.is(tok::lessequal)
    || Tok.is(tok::greater) || Tok.is(tok::greaterequal)
    || Tok.is(tok::equalequal) || Tok.is(tok::exclaimequal);
  };

  while (IsContinue(Tok)) {
    BinaryExpr::OpKind Kind;
    tok::TokenKind k = Tok.getKind();
    switch (k) {
      case tok::less:
        Kind = BinaryExpr::OpKind::LCmp;
        break;
      case tok::lessequal:
        Kind = BinaryExpr::OpKind::LEqCmp;
        break;
      case tok::greater:
        Kind = BinaryExpr::OpKind::GCmp;
        break;
      case tok::greaterequal:
        Kind = BinaryExpr::OpKind::GEqCmp;
        break;
      case tok::equalequal:
        Kind = BinaryExpr::OpKind::EqCmp;
        break;
      case tok::exclaimequal:
        Kind = BinaryExpr::OpKind::NEqCmp;
        break;
      default:
        return nullptr;
    }

    consumeToken();
    Expr *I = parseBinaryAddOrSubDemo();
    if (!I)
      return nullptr;
    
    SMLoc ELoc = I->getLocation().End;
    SMRange Loc(SLoc, ELoc);
    
    E = Context.createBinaryExpr(Loc, E, Kind, I);
  }

  return E;
}

Expr *Parser::parseLogicalAndIfExpr() {
  SMLoc SLoc = Tok.getLocation().Start;

  if (Tok.is(tok::kw_not)) {
    consumeToken();
    Expr *E = parseExprDemo();
    if (!E)
      return nullptr;

    SMLoc ELoc = Tok.getLocation().End;
    SMRange Loc(SLoc, ELoc);
    return Context.createUnaryExpr(Loc, UnaryExpr::OpKind::Not, E);
  }

  Expr *E = parseProjectionExpr();
  if (!E)
    return nullptr;

  if (Tok.is(tok::kw_and) || Tok.is(tok::kw_or)) {
    BinaryExpr::OpKind Kind = Tok.is(tok::kw_and) ? BinaryExpr::OpKind::And : BinaryExpr::OpKind::Or;

    consumeToken();

    Expr *R = parseExprDemo();
    if (!R)
      return nullptr;

    SMLoc ELoc = Tok.getLocation().End;
    SMRange Loc(SLoc, ELoc);

    return Context.createBinaryExpr(Loc, E, Kind, R);
  }

  if (Tok.is(tok::kw_if)) {

    Expr *Cond = parseExprDemo();
    if (!Cond)
      return nullptr;

    if (!expectAndConsume(tok::kw_else)) {
      return nullptr;
    }

    Expr *Else = parseExprDemo();
    if (!Else)
      return nullptr;

    SMLoc ELoc = Tok.getLocation().End;
    SMRange Loc(SLoc, ELoc);

    return Context.createIfExpr(Loc, E, Cond, Else);
  }

  return E;
}

Expr *Parser::parseCExpr() {
  SMLoc SLoc = Tok.getLocation().Start;

  auto IsFnCallExpr = [this](Token &Tok) {
    return Tok.is(tok::identifier) &&
           TheLexer.LookAhead(0).is(tok::l_paren);
  };

  if (Tok.is(tok::identifier)) { // ID or ID(...) 
    if (IsFnCallExpr(Tok)) // ID(...)
      return parseFnCallDemo();

    return parseAtomicExprDemo();
  }

  if (Tok.is(tok::l_paren)) { // (expr)
    consumeToken();

    Expr *E = parseExprDemo();
    if (!E)
      return nullptr;

    if (!expectAndConsume(tok::r_paren))
      return nullptr;

    return E;
  }

  if (Tok.is(tok::l_square)) { // [ [[expr [[, expr]]*]]? ]
    consumeToken();

    ExprList Exprs;

    Expr *E = parseExprDemo();
    if (!E) {
      if (!expectAndConsume(tok::r_square))
        return nullptr;
      
      SMLoc ELoc = Tok.getLocation().End;
      SMRange Loc(SLoc, ELoc);
      return Context.createListExpr(Loc, Exprs);
    }
    
    Exprs.push_back(E);

    while (Tok.is(tok::comma)) {
      consumeToken();

      Expr *E = parseExprDemo();
      if (!E)
        return nullptr;

      Exprs.push_back(E);
    }

    if (!expectAndConsume(tok::r_paren))
      return nullptr;

    SMLoc ELoc = Tok.getLocation().End;
    SMRange Loc(SLoc, ELoc);
    return Context.createListExpr(Loc, Exprs);
  }

  if (Tok.is(tok::minus)) { // - expr
    SMLoc SLoc = Tok.getLocation().Start;
    consumeToken();

    Expr *E = parseCExpr();
    if (!E)
      return nullptr;

    SMLoc ELoc = Tok.getLocation().End;
    SMRange Loc(SLoc, ELoc);

    return Context.createUnaryExpr(Loc, UnaryExpr::OpKind::Minus, E);
  }

  if (Literal *L = parseOnlyLiteral()) { // literal
    return L;
  }

  Expr *E = parseBinaryAddOrSubDemo();
  if (E)
    return E;

  /* E = parseCExpr(); // cexpr
  if (!E)
    return nullptr; */

  if (Tok.is(tok::period)) { // member_expr
    consumeToken();

    if (!expect(tok::identifier))
      return nullptr;

    SMLoc ELoc = Tok.getLocation().End;
    SMRange Loc(SLoc, ELoc);
    DeclRef *DR = Context.createDeclRef(Loc, Tok.getSymbolInfo());
    consumeToken();

    MemberExpr *ME = Context.createMemberExpr(Loc, E, DR);

    if (Tok.is(tok::l_paren)) { // member_expr(...)
      consumeToken();

      ExprList Exprs;

      Expr *I = parseExprDemo();
      if (!I) {
        if (!expectAndConsume(tok::r_square))
          return nullptr;
        
        SMLoc ELoc = Tok.getLocation().End;
        SMRange Loc(SLoc, ELoc);
        return Context.createListExpr(Loc, Exprs);
      }
      
      Exprs.push_back(I);

      while (Tok.is(tok::comma)) {
        consumeToken();

        I = parseExprDemo();
        if (!I)
          return nullptr;

        Exprs.push_back(I);
      }

      if (!expectAndConsume(tok::r_paren))
        return nullptr;

      ELoc = Tok.getLocation().End;
      SMRange Loc(SLoc, ELoc);

      return Context.createMethodCallExpr(Loc, ME, Exprs);
    }

    return ME;
  }

  if (Tok.is(tok::l_square)) { // expr[expr]
    consumeToken();

    Expr *I = parseExprDemo();
    if (!I)
      return nullptr;

    if (!expectAndConsume(tok::r_square))
      return nullptr;
    
    SMLoc ELoc = Tok.getLocation().End;
    SMRange Loc(SLoc, ELoc);

    return Context.createIndexExpr(Loc, E, I);
  }

  emitUnexpected();
  return nullptr;
}

Expr *Parser::parseExprDemo() {
  /* if (Expr *E = parseLogicalAndIfExpr()) {
    return E;
  } else if (Expr *E = parseBinaryAddOrSubDemo()) {
    return E;
  } else if (Expr *E = parseCExpr()) {
    return E;
  } */



  return parseLogicalAndIfExpr();
}

Expr *Parser::parseBinaryAddOrSubDemo() {
  Expr *L = parseBinaryMulOrDivOrModDemo();
  if (!L)
    return nullptr;

  auto IsContinue = [](const Token &T) {
    return T.isOneOf(tok::plus, tok::minus);
  };

  while (IsContinue(Tok)) {
    SMRange Loc = Tok.getLocation();
    BinaryExpr::OpKind K =
        Tok.is(tok::plus) ? BinaryExpr::OpKind::Add : BinaryExpr::OpKind::Sub;
    consumeToken();

    Expr *R = parseBinaryMulOrDivOrModDemo();
    if (!R)
      return nullptr;

    L = Context.createBinaryExpr(Loc, L, K, R);
  }
  return L;
}

Expr *Parser::parseBinaryMulOrDivOrModDemo() {
  Expr *L = parseFnCallDemo();
  if (!L)
    return nullptr;

  auto IsContinue = [](const Token &T) {
    return T.isOneOf(tok::star, tok::slashslash, tok::percent);
  };

  while (IsContinue(Tok)) {
    BinaryExpr::OpKind K = BinaryExpr::OpKind::Mul;
    if (Tok.is(tok::slashslash))
      K = BinaryExpr::OpKind::FloorDiv;
    else if (Tok.is(tok::percent))
      K = BinaryExpr::OpKind::Mod;
    consumeToken();

    Expr *R = parseFnCallDemo();
    if (!R)
      return nullptr;
    L = Context.createBinaryExpr(Tok.getLocation(), L, K, R);
  }
  return L;
}

Expr *Parser::parseFnCallDemo() {
  Expr *E = parseAtomicExprDemo();
  if (!E)
    return nullptr;

  auto IsContinue = [](const Token &T) {
    return !T.is(tok::eof) && T.is(tok::l_paren);
  };

  auto ParseArgs = [this](ExprList &Args) {
    do {
      Expr *A = parseExprDemo();
      if (!A)
        return false;
      Args.push_back(A);
    } while (consumeToken(tok::comma));
    return true;
  };

  if (DeclRef *DR = dyn_cast<DeclRef>(E)) {
    // parse func(a)(b)(c)...()
    while (IsContinue(Tok)) {
      ExprList Args;
      consumeToken();
      if (Tok.isNot(tok::r_paren))
        if (!ParseArgs(Args))
          return nullptr;

      if (!expectAndConsume(tok::r_paren))
        return nullptr;

      SMRange Loc(DR->getLocation().Start, Tok.getLocation().Start);
      E = Context.createCallExpr(Loc, DR, Args);
    }
  }
  return E;
}

Expr *Parser::parseAtomicExprDemo() {
  SMRange Loc = Tok.getLocation();

  if (Tok.is(tok::identifier)) {
    Expr *E = Context.createDeclRef(Loc, Tok.getSymbolInfo());
    consumeToken();
    return E;
  }

  if (Tok.is(tok::l_paren)) {
    consumeToken();
    Expr *E = parseExprDemo();
    if (!E || !expectAndConsume(tok::r_paren))
      return nullptr;
    return E;
  }

  if (Tok.is(tok::l_square)) {
    consumeToken();
    Expr *E = parseExprDemo();
    if (!E || !expectAndConsume(tok::r_square))
      return nullptr;
    return E;
  }

  if (Expr *E = parseOnlyLiteral()) {
    return E;
  }

  return nullptr;
}

/// type = ID | IDSTRING | '[' type ']'
TypeAnnotation *Parser::parseType() {
  SMRange Loc = Tok.getLocation();
  switch (Tok.getKind()) {
  case tok::identifier: {
    StringRef Name = Tok.getSymbolInfo()->getName();
    consumeToken();
    return Context.createClassType(Loc, Name);
  }
  case tok::idstring: {
    StringRef Name = Tok.getLiteralData();
    consumeToken();
    return Context.createClassType(Loc, Name);
  }
  case tok::l_square: {
    consumeToken();
    if (TypeAnnotation *T = parseType()) {
      if (expectAndConsume(tok::r_square)) {
        Loc = SMRange(Loc.Start, Tok.getLocation().End);
        return Context.createListType(Loc, T);
      }
    }
    return nullptr;
  }
  default:
    return nullptr;
  }
}

/// var_def = typed_var '=' literal NEWLINE
VarDef *Parser::parseVarDef() {

  if (!expect(tok::identifier))
    return nullptr;

  SymbolInfo *Name = Tok.getSymbolInfo();
  SMRange NameLoc = Tok.getLocation();
  consumeToken();

  if (!expectAndConsume(tok::colon))
    return nullptr;

  TypeAnnotation *T = parseType();
  if (!expectAndConsume(tok::equal))
    return nullptr;

  if (Literal *L = parseLiteral()) {
    if (expectAndConsume(tok::NEWLINE)) {
      SMLoc ELoc = L->getLocation().End;
      SMRange Loc(NameLoc.Start, ELoc);
      Identifier *V = Context.createIdentifier(NameLoc, Name);
      return Context.createVarDef(Loc, V, T, L);
    }
  }

  return nullptr;
}

Literal *Parser::parseOnlyLiteral() {
  SMRange Loc = Tok.getLocation();
  if (consumeToken(tok::kw_None)) { // literal
    return Context.createNoneLiteral(Loc);
  } else if (consumeToken(tok::kw_True)) {
    return Context.createBooleanLiteral(Loc, true);
  } else if (consumeToken(tok::kw_False)) {
    return Context.createBooleanLiteral(Loc, false);
  } else if (Tok.is(tok::integer_literal)) {
    llvm::APInt Value(32, Tok.getLiteralData(), 10);
    consumeToken();
    return Context.createIntegerLiteral(Loc, Value.getSExtValue());
  } else if (Tok.isOneOf(tok::idstring, tok::string_literal)) {
    StringRef Str = Tok.getLiteralData();
    consumeToken();
    return Context.createStringLiteral(Loc, Str);
  }

  return nullptr;
}

Literal *Parser::parseLiteral() {
  if (Literal *L = parseOnlyLiteral())
    return L;

  Diags.emitError(Tok.getLocation().Start, diag::err_near_token) << Tok;
  return nullptr;
}

std::pair<Identifier *, TypeAnnotation *> Parser::parseTypedVar() {
  if (!expect(tok::identifier)) // id
    return std::make_pair(nullptr, nullptr);

  SymbolInfo *ArgName = Tok.getSymbolInfo();
  SMRange ArgNameLoc = Tok.getLocation();
  consumeToken();

  if (!expectAndConsume(tok::colon)) // ':'
    return std::make_pair(nullptr, nullptr);

  TypeAnnotation *T;
  if (!(T = parseType())) // type
    return std::make_pair(nullptr, nullptr);

  Identifier *Arg = Context.createIdentifier(ArgNameLoc, ArgName);
  return std::make_pair(Arg, T);
}

/// func_def = 'def' id '(' [typed_var (',' typed_var)* ] ')' ':' type

FuncDef *Parser::parseFuncDef() {
  SMRange funcLoc = Tok.getLocation();

  if (!expectAndConsume(tok::kw_def)) // 'def'
    return nullptr;

  if (!expect(tok::identifier)) // id
    return nullptr;

  SymbolInfo *Name = Tok.getSymbolInfo();
  SMRange NameLoc = Tok.getLocation();
  consumeToken();
  
  if (!expectAndConsume(tok::l_paren)) // '('
    return nullptr;

  ParamDeclList ArgList;
  bool flagFirst = true;

  while (flagFirst || Tok.is(tok::comma)) {
    if (!flagFirst)
      consumeToken();
    flagFirst = false;

    std::pair<Identifier *, TypeAnnotation *> Arg = parseTypedVar();

    if (!Arg.first)
      continue;

    SMRange Loc(Arg.first->getLocation().Start, Arg.second->getLocation().End);

    ParamDecl *temp = Context.createParamDecl(Loc, Arg.first, Arg.second);
    ArgList.push_back(temp);
  }

  if (!expectAndConsume(tok::r_paren)) // ')'
    return nullptr;

  TypeAnnotation *retType;

  if (Tok.is(tok::arrow)) {
    consumeToken();
    retType = parseType();
    if (!retType) 
      return nullptr;
  } /* else {
    retType = ASTContext::getNoneTy(); // TODO return None
  } */

  if (!expectAndConsume(tok::colon)) // ':'
    return nullptr;

  if (!expectAndConsume(tok::NEWLINE) && !expectAndConsume(tok::INDENT)) // NEWLINE, INDENT
    return nullptr;

  DeclList bodyDecl;
  StmtList bodyStmt;
  auto temp = parseFuncBody(); // TODO: func_body
  bodyDecl = temp.first;
  bodyStmt = temp.second;

  if (!expectAndConsume(tok::DEDENT)) // DEDENT
    return nullptr;

  if (bodyStmt.empty()) {
    return nullptr;
  }
  
  SMLoc ELoc = bodyStmt.back()->getLocation().End;

  SMRange Loc(funcLoc.Start, ELoc);
  Identifier *V = Context.createIdentifier(NameLoc, Name);
  return Context.createFuncDef(Loc, V, ArgList, retType, bodyDecl, bodyStmt);
}

/// func_body = [global_decl | nonlocal_decl | var_def | func_def]* [stmt]+
GlobalDecl *Parser::parseGlobalDecl() {
  SMRange globalLoc = Tok.getLocation();

  if (!expect(tok::kw_global)) // 'global'
    return nullptr;
  
  consumeToken();

  if (!expect(tok::identifier)) // id
    return nullptr;

  SymbolInfo *Name = Tok.getSymbolInfo();
  SMRange NameLoc = Tok.getLocation();
  consumeToken();

  if (!expectAndConsume(tok::NEWLINE)) // NEWLINE
    return nullptr;

  Identifier *V = Context.createIdentifier(NameLoc, Name);
  SMRange Loc(globalLoc.Start, NameLoc.End);
  return Context.createGlobalDecl(Loc, V);
}

NonLocalDecl *Parser::parseNonLocalDecl() {
  SMRange nonLocalLoc = Tok.getLocation();

  if (!expect(tok::kw_nonlocal)) // 'nonlocal'
    return nullptr;
  
  consumeToken();

  if (!expect(tok::identifier)) // id
    return nullptr;

  SymbolInfo *Name = Tok.getSymbolInfo();
  SMRange NameLoc = Tok.getLocation();
  consumeToken();

  if (!expectAndConsume(tok::NEWLINE)) // NEWLINE
    return nullptr;

  Identifier *V = Context.createIdentifier(NameLoc, Name);
  SMRange Loc(nonLocalLoc.Start, NameLoc.End);
  return Context.createNonLocalDecl(Loc, V);
}

std::pair<DeclList, StmtList> Parser::parseFuncBody() {
  DeclList VarDeclList;
  StmtList StmtLst;

  auto isVarDef = [this](Token &Tok) {
    return Tok.is(tok::identifier) &&
           TheLexer.LookAhead(0).is(tok::colon);
  };

  auto IsFuncDef = [this](Token &Tok) {
    return Tok.is(tok::kw_def) &&
           TheLexer.LookAhead(0).is(tok::identifier);
  };

  auto isGlobalDecl = [](Token &Tok) {
    return Tok.is(tok::kw_global);
  };

  auto isNonLocalDecl = [](Token &Tok) {
    return Tok.is(tok::kw_nonlocal);
  };

  while (isVarDef(Tok) || IsFuncDef(Tok) || isGlobalDecl(Tok) || isNonLocalDecl(Tok)) {
    if (isVarDef(Tok)) {
      VarDef *temp = parseVarDef();
      VarDeclList.push_back(temp);
    } else if (IsFuncDef(Tok)) {
      FuncDef *temp = parseFuncDef();
      VarDeclList.push_back(temp);
    } else if (isGlobalDecl(Tok)) {
      GlobalDecl *temp = parseGlobalDecl();
      VarDeclList.push_back(temp);
    } else if (isNonLocalDecl(Tok)) {
      NonLocalDecl *temp = parseNonLocalDecl();
      VarDeclList.push_back(temp);
    }
  }

  do {
    StmtLst.push_back(parseStmtDemo());
  } while (!Tok.is(tok::DEDENT));

  return std::pair<DeclList, StmtList>(VarDeclList, StmtLst);
}

/// class_body = pass NEWLINE | [var_def | func_def]+
DeclList Parser::parseClassBody() {
  DeclList declList;

  if (Tok.is(tok::kw_pass)) {
    consumeToken();
    return declList;
  }

  auto IsVarDef = [this](Token &Tok) {
    return Tok.is(tok::identifier) &&
          TheLexer.LookAhead(0).is(tok::colon);
  };

  auto IsFuncDef = [this](Token &Tok) {
    return Tok.is(tok::kw_def) &&
           TheLexer.LookAhead(0).is(tok::identifier);
  };

  do {
    if (IsVarDef(Tok)) {
      declList.push_back(parseVarDef());
    } else if (IsFuncDef(Tok)) {
      declList.push_back(parseFuncDef());
    }
  } while (IsVarDef(Tok) || IsFuncDef(Tok));

  return declList;
}

ClassDef *Parser::parseClassDef() {
  SMRange classLoc = Tok.getLocation();

  if (!expectAndConsume(tok::kw_class)) // 'class'
    return nullptr;

  if (!expect(tok::identifier)) // id
    return nullptr;

  SymbolInfo *Name = Tok.getSymbolInfo();
  SMRange NameLoc = Tok.getLocation();
  consumeToken();

  if (!expectAndConsume(tok::l_paren)) // '('
    return nullptr;

  if (!expect(tok::identifier)) // id
    return nullptr;

  SymbolInfo *SuperName = Tok.getSymbolInfo();
  SMRange SuperNameLoc = Tok.getLocation();
  consumeToken();

  if (!expectAndConsume(tok::r_paren)) // ')'
    return nullptr;

  if (!expectAndConsume(tok::colon)) // ':'
    return nullptr;
  
  if (!expectAndConsume(tok::NEWLINE) && !expectAndConsume(tok::INDENT)) // NEWLINE, INDENT
    return nullptr;

  auto bodyClass = parseClassBody();
  if (!expectAndConsume(tok::DEDENT)) // DEDENT
    return nullptr;

  if (bodyClass.empty()) {
    return nullptr;
  }

  SMLoc ELoc = bodyClass.back()->getLocation().End;
  SMRange Loc(classLoc.Start, ELoc);
  Identifier *V = Context.createIdentifier(NameLoc, Name);
  Identifier *SuperV = Context.createIdentifier(SuperNameLoc, SuperName);
  return Context.createClassDef(Loc, V, SuperV, bodyClass);
}

} // namespace chocopy
