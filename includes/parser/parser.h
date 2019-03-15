#ifndef MYCCPILER_PARSER_H
#define MYCCPILER_PARSER_H

#include <fstream>
#include <memory>
#include <ast/ast.h>
#include <lex/lex.h>

namespace mycc {
class Parser {
 public:
  Parser(std::ifstream& ifstream);
  std::unique_ptr<TranslationUnitAST> parseTranslationUnit();
 private:
  std::ifstream &in;
  Lex lex;
  std::unique_ptr<ExternalDeclarationAST> parseExternalDeclaration();
};

#endif //MYCCPILER_PARSER_H
} //namespace mycc
