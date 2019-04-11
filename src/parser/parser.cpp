#include <memory>
<<<<<<< HEAD
=======

#include <memory>

>>>>>>> fd9357080aa517249c7062a1c2578683b7eac34f
#include <parser/parser.h>
#include <tokens/token.h>
mycc::Parser::Parser(std::ifstream &ifstream) : in(ifstream), lex(ifstream) {}

/// <translation-unit> ::= {<external-declaration>}*
mycc::nt<mycc::TranslationUnitAST>
mycc::Parser::parseTranslationUnit() {
<<<<<<< HEAD
  
=======
>>>>>>> fd9357080aa517249c7062a1c2578683b7eac34f
  mycc::nts<ExternalDeclarationAST> declarations;
  while (lex.peek() != TokenKind::TOKEN_EOF) {
    declarations.push_back(parseExternalDeclaration());
  }
  return std::make_unique<TranslationUnitAST>(std::move(declarations), table);
}

///<external-declaration> ::= <function-definition>
///                         | <declaration>
mycc::nt<mycc::ExternalDeclarationAST> mycc::Parser::parseExternalDeclaration() {
  // first sets are conflicted
  // we can not figure out which production to use until we see a '{' or ';'
  // or there's no <declaration-specifier> in <function-definition>
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_LPAREN:
    case TokenKind::TOKEN_STAR:
      return std::make_unique<mycc::ExternalDeclarationAST>(parseFunctionDefinition());
    case TokenKind::TOKEN_AUTO:
    case TokenKind::TOKEN_CHAR:
    case TokenKind::TOKEN_CONST:
    case TokenKind::TOKEN_DOUBLE:
    case TokenKind::TOKEN_ENUM:
    case TokenKind::TOKEN_EXTERN:
    case TokenKind::TOKEN_FLOAT:
    case TokenKind::TOKEN_INT:
    case TokenKind::TOKEN_LONG:
    case TokenKind::TOKEN_REGISTER:
    case TokenKind::TOKEN_SHORT:
    case TokenKind::TOKEN_SIGNED:
    case TokenKind::TOKEN_STATIC:
    case TokenKind::TOKEN_STRUCT:
    case TokenKind::TOKEN_TYPEDEF:
    case TokenKind::TOKEN_UNION:
    case TokenKind::TOKEN_UNSIGNED:
    case TokenKind::TOKEN_VOID:
<<<<<<< HEAD
    case TokenKind::TOKEN_VOLATILE: {
=======
    case TokenKind::TOKEN_VOLATILE:
label_lookup:
>>>>>>> fd9357080aa517249c7062a1c2578683b7eac34f
      TokenKind kind = lex.lookupTokens({TokenKind::TOKEN_LBRACE, TokenKind::TOKEN_SEMI});
      if (kind == TokenKind::TOKEN_LBRACE) {
        return std::make_unique<mycc::ExternalDeclarationAST>(parseFunctionDefinition());
      } else if (kind == TokenKind::TOKEN_SEMI) {
        return std::make_unique<mycc::ExternalDeclarationAST>(parseDeclaration());
      } else {
        //TODO error report
      }
      break;
<<<<<<< HEAD
    }
=======
>>>>>>> fd9357080aa517249c7062a1c2578683b7eac34f
    case TokenKind::TOKEN_IDENTIFIER:
      //TODO
      // Type -> search '{' or ';';
      // Var -> function
      break;
    default:
      //TODO char no in first set;
      break;

  }

}
mycc::nt<mycc::FunctionDefinitionAST>
mycc::Parser::parseFunctionDefinition() {
  return std::unique_ptr<mycc::FunctionDefinitionAST>();
}
mycc::nt<mycc::DeclarationAST>
mycc::Parser::parseDeclaration() {
  return mycc::nt<mycc::DeclarationAST>();
}
