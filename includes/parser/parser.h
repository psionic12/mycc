#ifndef MYCCPILER_PARSER_H
#define MYCCPILER_PARSER_H

#include <fstream>
#include <memory>
#include <ast/ast.h>
#include <lex/lex.h>
#include <sema/SymbolTable.h>

namespace mycc {
class Parser {
 public:
  Parser(std::ifstream& ifstream);
  nt<TranslationUnitAST> parseTranslationUnit();
 private:
  std::ifstream &in;
  Lex lex;
  nt<ExternalDeclarationAST> parseExternalDeclaration();
  nt<FunctionDefinitionAST> parseFunctionDefinition();
  nt<DeclarationAST> parseDeclaration();
};

#endif //MYCCPILER_PARSER_H
} //namespace mycc
