#ifndef MYCCPILER_SEMA_H
#define MYCCPILER_SEMA_H

#include "symbol_tables.h"
class Sema {
 public:
  Sema(nt<TranslationUnitAST> &&root) : root(std::move(root)), table(&this->root->table) {}
  void analyze();
 private:
  nt<TranslationUnitAST> root;
  SymbolTable *table;
  void analyzeTranslationUnitAST(const TranslationUnitAST *ast);
  void analyzeExternalDeclaration(const ExternalDeclarationAST *ast);
  void analyzeFunctionDefinition(const FunctionDefinitionAST *ast);
  void analyzeDeclarationSpecifiers(const DeclarationSpecifiersAST *ast);
  void analyzeStorageClassSpecifier(const StorageClassSpecifierAST *ast);
  void analyzeTypeSpecifier(const TypeSpecifierAST *ast);
  void analyzeStructOrUnionSpecifier(const StructOrUnionSpecifierAST *ast);
  void analyzeStructDeclaration(const StructDeclarationAST *ast);
  void analyzeSpecifierQualifier(const SpecifierQualifierAST *ast);
  void analyzeStructDeclaratorList(const StructDeclaratorListAST *ast);
  void analyzeStructDeclarator(const StructDeclaratorAST *ast);
  void analyzeDeclarator(const DeclaratorAST *ast);
  void analyzePointer(const PointerAST *ast);
  void analyzeTypeQualifier(const TypeQualifierAST *ast);
  void analyzeDirectDeclarator(const DirectDeclaratorAST *ast);
  void analyzeConstantExpression(const ConstantExpressionAST *ast);
  void analyzeConditionalExpression(const ConditionalExpressionAST *ast);
  void analyzeLogicalOrExpression(const LogicalOrExpressionAST *ast);
  void analyzeCastExpression(const CastExpressionAST *ast);
  void analyzeUnaryExpression(const UnaryExpressionAST *ast);
  void analyzePostfixExpression(const PostfixExpressionAST *ast);
  void analyzePrimaryExpression(const PrimaryExpressionAST *ast);
  void analyzeExpression(const ExpressionAST *ast);
  void analyzeAssignmentExpression(const AssignmentExpressionAST *ast);
  void analyzeTypeName(const TypeNameAST *ast);
  void analyzeParameterTypeList(const ParameterTypeListAST *ast);
  void analyzeParameterList(const ParameterListAST *ast);
  void analyzeParameterDeclaration(const ParameterDeclarationAST *ast);
  void analyzeEnumSpecifier(const EnumSpecifierAST *ast);
  void analyzeEnumeratorList(const EnumeratorListAST *ast);
  void analyzeEnumerator(const EnumeratorAST *ast);
  void analyzeTypedefName(const TypedefNameAST *ast);
  void analyzeDeclarations(const nts<DeclarationAST> *declarations);
  void analyzeInitDeclarators(const InitDeclarators *ast);
  void analyzeInitializer(const InitializerAST *ast);
  void analyzeInitializerList(const InitializerListAST *ast);
  void analyzeCompoundStatement(const CompoundStatementAST *ast);
  void analyzeStatement(const StatementAST *ast);
  void analyzeLabeledStatement(const LabeledStatementAST *ast);
  void analyzeExpressionStatement(const ExpressionStatementAST *ast);
  void analyzeSelectionStatement(const SelectionStatementAST *ast);
  void analyzeIterationStatement(const IterationStatementAST *ast);
  void analyzeJumpStatement(const JumpStatementAST *ast);
  void analyzeIdentifier(const IdentifierAST *ast);
};

#endif //MYCCPILER_SEMA_H
