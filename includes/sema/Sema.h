#ifndef MYCCPILER_SEMA_H
#define MYCCPILER_SEMA_H

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "symbol_tables.h"
class Sema {
 public:
  Sema(nt<TranslationUnitAST> &&root, llvm::Module &module, llvm::IRBuilder<> &builder)
      : root(std::move(root)), table(&this->root->table), module(module), builder(builder) {}
  void analyze();
 private:
  nt<TranslationUnitAST> root;
  SymbolTable *table;
  llvm::Module &module;
  llvm::IRBuilder<> &builder;
  void analyzeTranslationUnitAST(TranslationUnitAST *ast);
  void analyzeExternalDeclaration(ExternalDeclarationAST *ast);
  void analyzeFunctionDefinition(FunctionDefinitionAST *ast);
  void analyzeDeclarationSpecifiers(DeclarationSpecifiersAST *ast);
  void analyzeStorageClassSpecifier(StorageClassSpecifierAST *ast);
  void analyzeTypeSpecifier(TypeSpecifierAST *ast);
  void analyzeStructOrUnionSpecifier(StructOrUnionSpecifierAST *ast);
  void analyzeStructDeclaration(StructDeclarationAST *ast);
  void analyzeSpecifierQualifier(SpecifierQualifierAST *ast);
  void analyzeStructDeclaratorList(StructDeclaratorListAST *ast);
  void analyzeStructDeclarator(StructDeclaratorAST *ast);
  void analyzeDeclarator(DeclaratorAST *ast);
  void analyzePointer(PointerAST *ast);
  void analyzeTypeQualifier(TypeQualifierAST *ast);
  void analyzeDirectDeclarator(DirectDeclaratorAST *ast);
  void analyzeConstantExpression(ConstantExpressionAST *ast);
  void analyzeConditionalExpression(ConditionalExpressionAST *ast);
  void analyzeLogicalOrExpression(LogicalOrExpressionAST *ast);
  void analyzeCastExpression(CastExpressionAST *ast);
  void analyzeUnaryExpression(UnaryExpressionAST *ast);
  void analyzePostfixExpression(PostfixExpressionAST *ast);
  const Type * analyzePrimaryExpression(PrimaryExpressionAST *ast);
  void analyzeExpression(ExpressionAST *ast);
  void analyzeAssignmentExpression(AssignmentExpressionAST *ast);
  void analyzeTypeName(TypeNameAST *ast);
  void analyzeParameterTypeList(ParameterTypeListAST *ast);
  void analyzeParameterList(ParameterListAST *ast);
  void analyzeParameterDeclaration(ParameterDeclarationAST *ast);
  void analyzeEnumSpecifier(EnumSpecifierAST *ast);
  void analyzeEnumeratorList(EnumeratorListAST *ast);
  void analyzeEnumerator(EnumeratorAST *ast);
  void analyzeTypedefName(TypedefNameAST *ast);
  void analyzeDeclaration(DeclarationAST *declaration);
  void analyzeInitDeclarators(InitDeclarators *ast);
  void analyzeInitializer(InitializerAST *ast);
  void analyzeInitializerList(InitializerListAST *ast);
  void analyzeCompoundStatement(CompoundStatementAST *ast);
  void analyzeStatement(StatementAST *ast);
  void analyzeLabeledStatement(LabeledStatementAST *ast);
  void analyzeExpressionStatement(ExpressionStatementAST *ast);
  void analyzeSelectionStatement(SelectionStatementAST *ast);
  void analyzeIterationStatement(IterationStatementAST *ast);
  void analyzeJumpStatement(JumpStatementAST *ast);
  void analyzeIdentifier(IdentifierAST *ast);
  void analyzeString(StringAST *ast);
};

#endif //MYCCPILER_SEMA_H
