#include <sema/Sema.h>
void Sema::analyze() {
  analyzeTranslationUnitAST(root.get());
}
void Sema::analyzeTranslationUnitAST(const TranslationUnitAST *ast) {
  for (const auto &ds : ast->external_declarations) {
    analyzeExternalDeclaration(ds.get());
  }
}
void Sema::analyzeExternalDeclaration(const ExternalDeclarationAST *ast) {
  if (ast->def->getKind() == AST::Kind::FUNCTION_DEFINITION) {
    analyzeFunctionDefinition(static_cast<FunctionDefinitionAST *>(ast->def.get()));
  } else {
    analyzeDeclaration(static_cast<DeclarationAST *>(ast->def.get()));
  }
}
void Sema::analyzeFunctionDefinition(const FunctionDefinitionAST *ast) {
  for(const auto& specifier : ast->declaration_spcifiers->storage_specifiers) {
    if (specifier.type != StorageSpecifier::kEXTERN && specifier.type != StorageSpecifier::kSTATIC) {
      throw SemaException("declaration specifiers shall be either extern or static", specifier.token);
    }
  }
}
void Sema::analyzeIdentifier(const IdentifierAST *ast) {

}
void Sema::analyzeDeclarationSpecifiers(const DeclarationSpecifiersAST *ast) {

}
void Sema::analyzeStorageClassSpecifier(const StorageClassSpecifierAST *ast) {

}
void Sema::analyzeTypeSpecifier(const TypeSpecifierAST *ast) {

}
void Sema::analyzeStructOrUnionSpecifier(const StructOrUnionSpecifierAST *ast) {

}
void Sema::analyzeStructDeclaration(const StructDeclarationAST *ast) {

}
void Sema::analyzeSpecifierQualifier(const SpecifierQualifierAST *ast) {

}
void Sema::analyzeStructDeclaratorList(const StructDeclaratorListAST *ast) {

}
void Sema::analyzeStructDeclarator(const StructDeclaratorAST *ast) {

}
void Sema::analyzeDeclarator(const DeclaratorAST *ast) {

}
void Sema::analyzePointer(const PointerAST *ast) {

}
void Sema::analyzeTypeQualifier(const TypeQualifierAST *ast) {

}
void Sema::analyzeDirectDeclarator(const DirectDeclaratorAST *ast) {

}
void Sema::analyzeConstantExpression(const ConstantExpressionAST *ast) {

}
void Sema::analyzeConditionalExpression(const ConditionalExpressionAST *ast) {

}
void Sema::analyzeLogicalOrExpression(const LogicalOrExpressionAST *ast) {

}
void Sema::analyzeCastExpression(const CastExpressionAST *ast) {

}
void Sema::analyzeUnaryExpression(const UnaryExpressionAST *ast) {

}
void Sema::analyzePostfixExpression(const PostfixExpressionAST *ast) {

}
void Sema::analyzePrimaryExpression(const PrimaryExpressionAST *ast) {

}
void Sema::analyzeExpression(const ExpressionAST *ast) {

}
void Sema::analyzeAssignmentExpression(const AssignmentExpressionAST *ast) {

}
void Sema::analyzeTypeName(const TypeNameAST *ast) {

}
void Sema::analyzeParameterTypeList(const ParameterTypeListAST *ast) {

}
void Sema::analyzeParameterList(const ParameterListAST *ast) {

}
void Sema::analyzeParameterDeclaration(const ParameterDeclarationAST *ast) {

}
void Sema::analyzeEnumSpecifier(const EnumSpecifierAST *ast) {

}
void Sema::analyzeEnumeratorList(const EnumeratorListAST *ast) {

}
void Sema::analyzeEnumerator(const EnumeratorAST *ast) {

}
void Sema::analyzeTypedefName(const TypedefNameAST *ast) {

}
void Sema::analyzeDeclaration(const DeclarationAST *declaration) {

}
void Sema::analyzeInitDeclarators(const InitDeclarators *ast) {

}
void Sema::analyzeInitializer(const InitializerAST *ast) {

}
void Sema::analyzeInitializerList(const InitializerListAST *ast) {

}
void Sema::analyzeCompoundStatement(const CompoundStatementAST *ast) {

}
void Sema::analyzeStatement(const StatementAST *ast) {

}
void Sema::analyzeLabeledStatement(const LabeledStatementAST *ast) {

}
void Sema::analyzeExpressionStatement(const ExpressionStatementAST *ast) {

}
void Sema::analyzeSelectionStatement(const SelectionStatementAST *ast) {

}
void Sema::analyzeIterationStatement(const IterationStatementAST *ast) {

}
void Sema::analyzeJumpStatement(const JumpStatementAST *ast) {

}
