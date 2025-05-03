#ifndef CHOCOPY_LLVM_PARSER_PARSER_H
#define CHOCOPY_LLVM_PARSER_PARSER_H

#include "chocopy-llvm/AST/AST.h"
#include "chocopy-llvm/Lexer/Lexer.h"

namespace chocopy {
class Sema;
class Scope;

class Parser {
  class ParseScope;

public:
  Parser(ASTContext &C, Lexer &Lex, Sema &Acts);

  Program *parse();

private:
  bool consumeToken(tok::TokenKind ExpectedTok);
  bool consumeToken();

  bool expect(tok::TokenKind ExpectedTok);
  bool expectAndConsume(tok::TokenKind ExpectedTok);

  void skipToNextLine();

  void emitUnexpected();

  const Token &getLookAheadToken(int N);

  Program *parseProgramDemo();
  Stmt *parseStmtDemo();
  Stmt *parseAssignOrExprDemo();
  Expr *parseExprDemo();
  Expr *parseBinaryAddOrSubDemo();
  Expr *parseBinaryMulOrDivOrModDemo();
  Expr *parseFnCallDemo();
  Expr *parseAtomicExprDemo();

  TypeAnnotation *parseType();
  VarDef *parseVarDef();
  Literal *parseLiteral();

  // own
  FuncDef *parseFuncDef();
  std::pair<DeclList, StmtList> parseFuncBody();
  ClassDef *parseClassDef();
  DeclList parseClassBody();
  std::pair<Identifier *, TypeAnnotation *> parseTypedVar();
  GlobalDecl *parseGlobalDecl();
  NonLocalDecl *parseNonLocalDecl();
  StmtList parseBlockStmt();
  Stmt *parseForStmt();
  Stmt *parseWhileStmt();
  Stmt *parseIfStmt();
  Stmt *parseSimpleStmt();
  Stmt *parseReturnStmt();
  Expr *parseProjectionExpr();
  Expr *parseLogicalAndIfExpr();
  Expr *parseCExpr();
  Literal *parseOnlyLiteral();

private:
  DiagnosticsEngine &Diags;
  ASTContext &Context;
  Lexer &TheLexer;
  Token Tok;
};
} // namespace chocopy
#endif // CHOCOPY_LLVM_PARSER_PARSER_H