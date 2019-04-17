#ifndef MYCCPILER_PARSER_H
#define MYCCPILER_PARSER_H

#include <fstream>
#include <memory>
#include <ast/ast.h>
#include <lex/lex.h>
#include <sema/SymbolTable.h>
#include <sema/operator.h>

namespace mycc {

class NotAInfixOpException {};

class Parser {
 public:
  Parser(std::ifstream &ifstream, std::vector<SymbolTable> &tables);
  nt<TranslationUnitAST> parseTranslationUnit();
 private:
  std::ifstream &in;
  Lex lex;
  std::vector<SymbolTable> &tables;
  SymbolTable *pTable;
  nt<ExternalDeclarationAST> parseExternalDeclaration();
  nt<FunctionDefinitionAST> parseFunctionDefinition();
  nt<DeclarationSpecifierAST> parseDeclarationSpecifier();
  nt<StorageClassSpecifierAST> parseStorageClassSpecifier();
  nt<TypeSpecifierAST> parseTypeSpecifier();
  nt<StructOrUnionSpecifierAST> parseStructOrUnionSpecifier();
  nt<StructOrUnionAST> parseStructOrUnion();
  nt<StructDeclarationAST> parseStructDeclaration();
  nt<SpecifierQualifierAST> parseSpecifierQualifier();
  nt<StructDeclaratorListAST> parseStructDeclaratorList();
  nt<StructDeclaratorAST> parseStructDeclarator();
  nt<DeclaratorAST> parseDeclarator();
  nt<PointerAST> parsePointer();
  nt<TypeQualifierAST> parseTypeQualifier();
  nt<DirectDeclaratorAST> parseDirectDeclarator();
  nt<ConstantExpressionAST> parseConstantExpression();
  nt<ConditionalExpressionAST> parseConditionalExpression();
  nt<LogicalOrExpressionAST> parseLogicalOrExpression(int calling_prec = 0);
  nt<LogicalAndExpressionAST> parseLogicalAndExpression();
  nt<InclusiveOrExpressionAST> parseInclusiveOrExpression();
  nt<ExclusiveOrExpressionAST> parseExclusiveOrExpression();
  nt<AndExpressionAST> parseAndExpression();
  nt<EqualityExpressionAST> parseEqualityExpression();
  nt<RelationalExpressionAST> parseRelationalExpression();
  nt<ShiftExpressionAST> parseShiftExpression();
  nt<AdditiveExpressionAST> parseAdditiveExpression();
  nt<MultiplicativeExpressionAST> parseMultiplicativeExpression();
  nt<CastExpressionAST> parseCastExpression();
  nt<UnaryExpressionAST> parseUnaryExpression();
  nt<PostfixExpressionAST> parsePostfixExpression();
  nt<PrimaryExpressionAST> parsePrimaryExpression();
  nt<ConstantAST> parseConstant();
  nt<ExpressionAST> parseExpression();
  nt<AssignmentExpressionAST> parseAssignmentExpression();
  nt<AssignmentOperatorAST> parseAssignmentOperator();
  nt<UnaryOperatorAST> parseUnaryOperator();
  nt<TypeNameAST> parseTypeName();
  nt<ParameterTypeListAST> parseParameterTypeList();
  nt<ParameterListAST> parseParameterList();
  nt<ParameterDeclarationAST> parseParameterDeclaration();
  nt<AbstractDeclaratorAST> parseAbstractDeclarator();
  nt<DirectAbstractDeclaratorAST> parseDirectAbstractDeclarator();
  nt<EnumSpecifierAST> parseEnumSpecifier();
  nt<EnumeratorListAST> parseEnumeratorList();
  nt<EnumeratorAST> parseEnumerator();
  nt<TypedefNameAST> parseTypedefName();
  nt<DeclarationAST> parseDeclaration();
  nt<InitDeclaratorAST> parseInitDeclarator();
  nt<InitializerAST> parseInitializer();
  nt<InitializerListAST> parseInitializerList();
  nt<CompoundStatementAST> parseCompoundStatement();
  nt<StatementAST> parseStatement();
  nt<LabeledStatementAST> parseLabeledStatement();
  nt<ExpressionStatementAST> parseExpressionStatement();
  nt<SelectionStatementAST> parseSelectionStatement();
  nt<IterationStatementAST> parseIterationStatement();
  nt<JumpStatementAST> parseJumpStatement();
  nt<IdentifierAST> parseIdentifer();
  InfixOp isInfixOp(TokenKind kind);

  const std::string &accept(TokenKind kind);
  bool expect(TokenKind kind);
  std::runtime_error parseError(const std::string msg);
  int precedence(InfixOp op);
};

#endif //MYCCPILER_PARSER_H
} //namespace mycc
