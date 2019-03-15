#include <memory>

#include <parser/parser.h>
#include <tokens/token.h>
mycc::Parser::Parser(std::ifstream& ifstream) : in(ifstream), lex(ifstream) {

}

/// <translation-unit> ::= {<external-declaration>}*
std::unique_ptr<mycc::TranslationUnitAST> mycc::Parser::parseTranslationUnit() {
  std::vector<std::unique_ptr<ExternalDeclarationAST>> declarations;
  while(lex.peek()->getKind() != TokenKind::TOKEN_EOF) {
    declarations.push_back(parseExternalDeclaration());
  }
  return std::make_unique<TranslationUnitAST>(std::move(declarations));
}

///<external-declaration> ::= <function-definition>
///                         | <declaration>
std::unique_ptr<mycc::ExternalDeclarationAST> mycc::Parser::parseExternalDeclaration() {

}
