#include <sema/Sema.h>
void Sema::analyze() {
  analyzeTranslationUnitAST(root.get());
}
void Sema::analyzeTranslationUnitAST(const TranslationUnitAST *ast) {
  for (const auto &ds : ast.external_declarations) {
    analyzeExternalDeclaration(*ds);
  }
}
void Sema::analyzeExternalDeclaration(const ExternalDeclarationAST &ast) {
  if (ast.def->getKind() == AST::Kind::FUNCTION_DEFINITION) {
    analyzeFunctionDefinition(static_cast<FunctionDefinitionAST>(ast));
  } else {

  }
}
