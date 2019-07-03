#include <sema/Sema.h>
void Sema::analyze() {
  analyzeTranslationUnitAST(root.get());
}
void Sema::analyzeTranslationUnitAST(TranslationUnitAST *ast) {
  for (const auto &ds : ast->external_declarations) {
    analyzeExternalDeclaration(ds.get());
  }
}
void Sema::analyzeExternalDeclaration(ExternalDeclarationAST *ast) {
  if (ast->def->getKind() == AST::Kind::FUNCTION_DEFINITION) {
    analyzeFunctionDefinition(static_cast<FunctionDefinitionAST *>(ast->def.get()));
  } else {
    analyzeDeclaration(static_cast<DeclarationAST *>(ast->def.get()));
  }
}
void Sema::analyzeFunctionDefinition(FunctionDefinitionAST *ast) {
  for (const auto &specifier : ast->declaration_spcifiers->storage_specifiers) {
    if (specifier.type != StorageSpecifier::kEXTERN && specifier.type != StorageSpecifier::kSTATIC) {
      throw SemaException("declaration specifiers shall be either extern or static", specifier.token);
    }
  }

}
void Sema::analyzeIdentifier(IdentifierAST *ast) {

}
void Sema::analyzeDeclarationSpecifiers(DeclarationSpecifiersAST *ast) {

}
void Sema::analyzeStorageClassSpecifier(StorageClassSpecifierAST *ast) {

}
void Sema::analyzeTypeSpecifier(TypeSpecifierAST *ast) {

}
void Sema::analyzeStructOrUnionSpecifier(StructOrUnionSpecifierAST *ast) {

}
void Sema::analyzeStructDeclaration(StructDeclarationAST *ast) {

}
void Sema::analyzeSpecifierQualifier(SpecifierQualifierAST *ast) {

}
void Sema::analyzeStructDeclaratorList(StructDeclaratorListAST *ast) {

}
void Sema::analyzeStructDeclarator(StructDeclaratorAST *ast) {

}
void Sema::analyzeDeclarator(DeclaratorAST *ast) {

}
void Sema::analyzePointer(PointerAST *ast) {

}
void Sema::analyzeTypeQualifier(TypeQualifierAST *ast) {

}
void Sema::analyzeDirectDeclarator(DirectDeclaratorAST *ast) {

}
void Sema::analyzeConstantExpression(ConstantExpressionAST *ast) {

}
void Sema::analyzeConditionalExpression(ConditionalExpressionAST *ast) {

}
void Sema::analyzeLogicalOrExpression(LogicalOrExpressionAST *ast) {

}
void Sema::analyzeCastExpression(CastExpressionAST *ast) {

}
void Sema::analyzeUnaryExpression(UnaryExpressionAST *ast) {

}
void Sema::analyzePostfixExpression(PostfixExpressionAST *ast) {

}
void Sema::analyzePrimaryExpression(PrimaryExpressionAST *ast) {
  switch (ast->getProduction()) {
    case 0: { // identifier
      auto *identifierAST = static_cast<IdentifierAST *>(ast->ast.get());
      auto *symbol = table->lookup(identifierAST->token);
      SymbolKind kind = symbol->getKind();
      if (kind == SymbolKind::OBJECT) {
        ast->type = static_cast<ObjectSymbol *>(symbol)->getType();
      } else if (kind == SymbolKind::FUNCTION) {
        ast->type = static_cast<FunctionSymbol *>(symbol)->getType();
      } else if (kind == SymbolKind::ENUMERATION_CONSTANT) {
        ast->type = static_cast<EnumConstSymbol *>(symbol)->getType();
      } else {
        throw SemaException("identifier as a primary expression must be object or function or enumeration const",
                            identifierAST->token);
      }
      return;
    }
    case 1: {
      auto *integerConstAST = static_cast<IntegerConstantAST *>(ast->ast.get());
    }
    case 2:
    case 3:
    case 4:
    case 5:
    default:break;
  }
}
void Sema::analyzeExpression(ExpressionAST *ast) {

}
void Sema::analyzeAssignmentExpression(AssignmentExpressionAST *ast) {

}
void Sema::analyzeTypeName(TypeNameAST *ast) {

}
void Sema::analyzeParameterTypeList(ParameterTypeListAST *ast) {

}
void Sema::analyzeParameterList(ParameterListAST *ast) {

}
void Sema::analyzeParameterDeclaration(ParameterDeclarationAST *ast) {

}
void Sema::analyzeEnumSpecifier(EnumSpecifierAST *ast) {

}
void Sema::analyzeEnumeratorList(EnumeratorListAST *ast) {

}
void Sema::analyzeEnumerator(EnumeratorAST *ast) {

}
void Sema::analyzeTypedefName(TypedefNameAST *ast) {

}
void Sema::analyzeDeclaration(DeclarationAST *declaration) {

}
void Sema::analyzeInitDeclarators(InitDeclarators *ast) {

}
void Sema::analyzeInitializer(InitializerAST *ast) {

}
void Sema::analyzeInitializerList(InitializerListAST *ast) {

}
void Sema::analyzeCompoundStatement(CompoundStatementAST *ast) {

}
void Sema::analyzeStatement(StatementAST *ast) {

}
void Sema::analyzeLabeledStatement(LabeledStatementAST *ast) {

}
void Sema::analyzeExpressionStatement(ExpressionStatementAST *ast) {

}
void Sema::analyzeSelectionStatement(SelectionStatementAST *ast) {

}
void Sema::analyzeIterationStatement(IterationStatementAST *ast) {

}
void Sema::analyzeJumpStatement(JumpStatementAST *ast) {

}
void Sema::analyzeString(StringAST *ast) {

}
