#ifndef MYCCPILER_PARSER_H
#define MYCCPILER_PARSER_H

#include <fstream>
#include <memory>
#include <sema/ast.h>
#include <lex/lex.h>
#include <sema/symbol_tables.h>
#include <sema/operator.h>

class NotAInfixOpException {};
class ParserException : public std::exception {
 public:
  ParserException(std::string error, const Token &token);
  const char *what() const noexcept override;
 private:
  const Token &token;
  std::string error;
};

class Parser {
 public:
  Parser(std::ifstream &ifstream, SymbolTables &symbolTables);
  nt<TranslationUnitAST> parseTranslationUnit();
 private:
  std::ifstream &in;
  Lex lex;
  SymbolTables &symbolTables;
  SymbolTable *table;
  const Token *mStartToken = nullptr;
  InfixOp isInfixOp(TokenKind kind);
  const std::string &accept(TokenKind kind);
  bool expect(TokenKind kind);
  ParserException parseError(const std::string &msg);
  ParserException parseError(const std::string &, const Token &token);
  int precedence(InfixOp op);
  template<typename T, typename... Args>
  std::unique_ptr<T> make_ast(Args &&... args) {
    auto ptr = std::unique_ptr<T>(new T(std::forward<Args>(args)...));
    static_cast<AST *>(ptr.get())->mLeftMost = mStartToken;
    static_cast<AST *>(ptr.get())->mRightMost = &lex.peek(-1);
    return ptr;
  }
 public:
  nt<ExternalDeclarationAST> parseExternalDeclaration();
//  nt<FunctionDefinitionAST> parseFunctionDefinition();
  nt<DeclarationSpecifiersAST> parseDeclarationSpecifiers();
  StorageSpecifier parseStorageClassSpecifier();
  nt<TypeSpecifierAST> parseTypeSpecifier();
  nt<StructOrUnionSpecifierAST> parseStructOrUnionSpecifier();
  nt<StructDeclarationAST> parseStructDeclaration();
  nt<SpecifierQualifierAST> parseSpecifierQualifiers();
  nt<StructDeclaratorListAST> parseStructDeclaratorList();
  nt<StructDeclaratorAST> parseStructDeclarator();
  nt<DeclaratorAST> parseDeclarator();
  nt<PointerAST> parsePointer();
  nt<TypeQualifierAST> parseTypeQualifier();
  nt<DirectDeclaratorAST> parseDirectDeclarator();
  nt<ConstantExpressionAST> parseConstantExpression();
  nt<ConditionalExpressionAST> parseConditionalExpression();
  nt<IBinaryOperationAST> parseBinaryOperationAST(int calling_prec = 0);
  nt<CastExpressionAST> parseCastExpression();
  nt<UnaryExpressionAST> parseUnaryExpression();
  nt<PostfixExpressionAST> parsePostfixExpression();
  nt<PrimaryExpressionAST> parsePrimaryExpression();
  nt<ExpressionAST> parseExpression();
  nt<AssignmentExpressionAST> parseAssignmentExpression();
  AssignmentOp parseAssignmentOperator();
  Terminal<UnaryOp> parseUnaryOperator();
  nt<TypeNameAST> parseTypeName();
  nt<ParameterListAST> parseParameterLists();
  nt<ParameterDeclarationAST> parseParameterDeclaration();
  nt<EnumSpecifierAST> parseEnumSpecifier();
  nt<EnumeratorListAST> parseEnumeratorList();
  nt<EnumeratorAST> parseEnumerator();
  nt<TypedefNameAST> parseTypedefName();
  nts<DeclarationAST> parseDeclarations();
  InitDeclarators parseInitDeclarators();
  nt<InitializerAST> parseInitializer();
  nt<InitializerListAST> parseInitializerList();
  nt<CompoundStatementAST> parseCompoundStatement();
  nt<StatementAST> parseStatement();
  nt<LabeledStatementAST> parseLabeledStatement();
  nt<ExpressionStatementAST> parseExpressionStatement();
  nt<SelectionStatementAST> parseSelectionStatement();
  nt<IterationStatementAST> parseIterationStatement();
  nt<JumpStatementAST> parseJumpStatement();
  nt<IdentifierAST> parseIdentifier();
};

#endif //MYCCPILER_PARSER_H
