#include <memory>
#include <parser/parser.h>
#include <tokens/token.h>
mycc::Parser::Parser(std::ifstream &ifstream, std::vector<SymbolTable> &tables)
    : in(ifstream), lex(ifstream), tables(tables) {}
bool mycc::Parser::expect(mycc::TokenKind kind) {
  if (lex.peek() == kind) {
    lex.consumeToken();
    return true;
  } else {
    return false;
  }
}
const std::string &mycc::Parser::accept(mycc::TokenKind kind) {
  if (lex.peek() != kind) {
    throw parseError(std::string("except ").append(enumToString(kind)));
  } else {
    const std::string &name = lex.peek().getValue();
    lex.consumeToken();
    return name;
  }
}
std::runtime_error mycc::Parser::parseError(const std::string msg) {
  //TODO finish this
  return std::runtime_error(msg);
}
int mycc::Parser::precedence(mycc::TokenKind kind) {
  switch (kind) {
    case TokenKind::TOKEN_BARBAR:return 1;
    case TokenKind::TOKEN_AMPAMP:return 2;
    case TokenKind::TOKEN_BAR:return 3;
    case TokenKind::TOKEN_CARET:return 4;
    case TokenKind::TOKEN_AMP:return 5;
    case TokenKind::TOKEN_EQEQ:
    case TokenKind::TOKEN_BANGEQ:return 6;
    case TokenKind::TOKEN_LT:
    case TokenKind::TOKEN_GT:
    case TokenKind::TOKEN_LTEQ:
    case TokenKind::TOKEN_GTEQ:return 7;
    case TokenKind::TOKEN_LTLT:
    case TokenKind::TOKEN_GTGT:return 8;
    case TokenKind::TOKEN_PLUS:
    case TokenKind::TOKEN_SUB:return 9;
    case TokenKind::TOKEN_STAR:
    case TokenKind::TOKEN_SLASH:
    case TokenKind::TOKEN_PERCENT:return 10;
    default:throw parseError(std::string("precedence for illigel token: ").append(enumToString(kind)));
  }
}

/// <translation-unit> ::= {<external-declaration>}*
mycc::nt<mycc::TranslationUnitAST>
mycc::Parser::parseTranslationUnit() {
  tables.emplace_back("TranslationUnit");
  pTable = &tables.back();
  mycc::nts<ExternalDeclarationAST> declarations;
  while (lex.peek() != TokenKind::TOKEN_EOF) {
    declarations.push_back(parseExternalDeclaration());
  }
  return std::make_unique<TranslationUnitAST>(std::move(declarations), *pTable);
}

///<external-declaration> ::= <function-definition>
///                         | <declaration>
mycc::nt<mycc::ExternalDeclarationAST> mycc::Parser::parseExternalDeclaration() {
  // first sets are conflicted
  // we can not figure out which production to use until we see a '{' or ';'
  // or there's no <declaration-specifier> in <function-definition>
  TokenKind kind = lex.lookupTokens({TokenKind::TOKEN_LBRACE, TokenKind::TOKEN_SEMI});
  if (kind == TokenKind::TOKEN_LBRACE) {
    return std::make_unique<mycc::ExternalDeclarationAST>(parseFunctionDefinition());
  } else if (kind == TokenKind::TOKEN_SEMI) {
    return std::make_unique<mycc::ExternalDeclarationAST>(parseDeclaration());
  } else {
    throw parseError("expected { or ;");
  }
}

/// <typedef-name> ::= <identifier>
mycc::nt<mycc::TypedefNameAST> mycc::Parser::parseTypedefName() {
  return std::make_unique<mycc::TypedefNameAST>(parseIdentifer());
}
mycc::nt<mycc::IdentifierAST> mycc::Parser::parseIdentifer() {
  return std::make_unique<mycc::IdentifierAST>(accept(TokenKind::TOKEN_IDENTIFIER));
}

///<unary-operator> ::= &
///                   | *
///                   | +
///                   | -
///                   | ~
///                   | !
mycc::nt<mycc::UnaryOperatorAST> mycc::Parser::parseUnaryOperator() {
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_AMP:
    case TokenKind::TOKEN_STAR:
    case TokenKind::TOKEN_PLUS:
    case TokenKind::TOKEN_SUB:
    case TokenKind::TOKEN_TILDE:
    case TokenKind::TOKEN_BANG:lex.consumeToken();
      return std::make_unique<mycc::UnaryOperatorAST>(lex.peek().getKind());
    default:throw parseError("unary operator expected '&', '*', '+', '-', '~', '!'");
  }

}

///<assignment-operator> ::= =
///                        | /=
///                        | %=
///                        | +=
///                        | -=
///                        | <<=
///                        | >>=
///                        | &=
///                        | ^=
///                        | |=
mycc::nt<mycc::AssignmentOperatorAST> mycc::Parser::parseAssignmentOperator() {
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_EQ:
    case TokenKind::TOKEN_STAREQ:
    case TokenKind::TOKEN_SLASHEQ:
    case TokenKind::TOKEN_PERCENTEQ:
    case TokenKind::TOKEN_PLUSEQ:
    case TokenKind::TOKEN_SUBEQ:
    case TokenKind::TOKEN_LTLTEQ:
    case TokenKind::TOKEN_GTGTEQ:
    case TokenKind::TOKEN_AMPEQ:
    case TokenKind::TOKEN_CARETEQ:
    case TokenKind::TOKEN_BAREQ:lex.consumeToken();
      return std::make_unique<mycc::AssignmentOperatorAST>(lex.peek().getKind());
    default:
      throw parseError("assignment operator expected '=', '*=', '/=', '%=', '+=', '-=', '<<=', '>>=', '&=', '^=', '|='");
  }
}

///<type-qualifier> ::= const
///                   | volatile

/// const volatile
mycc::nt<mycc::TypeQualifierAST> mycc::Parser::parseTypeQualifier() {
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_CONST:
    case TokenKind::TOKEN_VOLATILE:lex.consumeToken();
      return std::make_unique<mycc::TypeQualifierAST>(lex.peek().getKind());
    default:throw parseError(R"(type qualifier expected "const" or "volatile")");
  }
}


///<struct-or-union> ::= struct
///                    | union

/// struct union
mycc::nt<mycc::StructOrUnionAST> mycc::Parser::parseStructOrUnion() {
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_STRUCT:
    case TokenKind::TOKEN_UNION:lex.consumeToken();
      return std::make_unique<mycc::StructOrUnionAST>(lex.peek().getKind());
    default:throw parseError(R"(expected "struct" or "union")");
  }
}

///<enumerator> ::= <identifier>
///               | <identifier> = <constant-expression>

/// id
mycc::nt<mycc::EnumeratorAST> mycc::Parser::parseEnumerator() {
  auto id = parseIdentifer();
  if (expect(TokenKind::TOKEN_EQ)) {
    //TODO <constant-expression>
  } else {
    return std::make_unique<mycc::EnumeratorAST>(parseIdentifer());
  }
}

///<enumerator-list> ::= <enumerator>
///                    | <enumerator-list> , <enumerator>

/// id
mycc::nt<mycc::EnumeratorListAST> mycc::Parser::parseEnumeratorList() {
  nts<EnumeratorAST> list;
  do {
    list.emplace_back(parseEnumerator());
  } while (expect(TokenKind::TOKEN_COMMA));
  return std::make_unique<mycc::EnumeratorListAST>(std::move(list));
}

///<enum-specifier> ::= enum <identifier> { <enumerator-list> }
///                   | enum { <enumerator-list> }
///                   | enum <identifier>

/// enum
mycc::nt<mycc::EnumSpecifierAST> mycc::Parser::parseEnumSpecifier() {
  accept(TokenKind::TOKEN_ENUM);
  if (lex.peek() == TokenKind::TOKEN_IDENTIFIER) {
    auto id = parseIdentifer();
    if (expect(TokenKind::TOKEN_LBRACE)) {
      auto p = std::make_unique<mycc::EnumSpecifierAST>(parseEnumeratorList());
      accept(TokenKind::TOKEN_RBRACE);
      return p;
    } else {
      return std::make_unique<mycc::EnumSpecifierAST>(std::move(id));
    }
  } else if (expect(TokenKind::TOKEN_LBRACE)) {
    auto p = std::make_unique<mycc::EnumSpecifierAST>(parseEnumeratorList());
    accept(TokenKind::TOKEN_RBRACE);
    return p;
  } else {
    throw parseError("enum specifier expected an identifer or '{'");
  }
}

///<primary-expression> ::= <identifier>
///                       | <constant>
///                       | <string>
///                       | ( <expression> )
///<constant> ::= <integer-constant>
///             | <character-constant>
///             | <floating-constant>
///             | <enumeration-constant>

/// id int-const float-const char-const string (


// we don't distinguish whether an identifier is a enumeraition constant or not, we put this on the sema phrase.
mycc::nt<mycc::PrimaryExpressionAST> mycc::Parser::parsePrimaryExpression() {
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_IDENTIFIER:return std::make_unique<PrimaryExpressionAST>(parseIdentifer());
    case TokenKind::TOKEN_INT_CONSTANT: {
      auto c = std::make_unique<PrimaryExpressionAST>(std::make_unique<IntegerConstantAST>(lex.peek().getValue()));
      lex.consumeToken();
      return c;
    }
    case TokenKind::TOKEN_FLOAT_CONSTANT: {
      auto c = std::make_unique<PrimaryExpressionAST>(std::make_unique<FloatingConstantAST>(lex.peek().getValue()));
      lex.consumeToken();
      return c;
    }
    case TokenKind::TOKEN_CHARLITERAL: {
      auto c = std::make_unique<PrimaryExpressionAST>(std::make_unique<CharacterConstantAST>(lex.peek().getValue()));
      lex.consumeToken();
      return c;
    }
    case TokenKind::TOKEN_STRINGLITERAL: {
      auto c = std::make_unique<PrimaryExpressionAST>(std::make_unique<StringAST>(lex.peek().getValue()));
      lex.consumeToken();
      return c;
    }
    case TokenKind::TOKEN_LPAREN: {
      lex.consumeToken();
      auto c = std::make_unique<PrimaryExpressionAST>(parseExpression());
      accept(TokenKind::TOKEN_RPAREN);
      return c;
    }
    default:throw parseError("expects identifier/string/char/int/float or a '(' for constant type");
  }
}

///<expression> ::= <assignment-expression>
///               | <expression> , <assignment-expression>
mycc::nt<mycc::ExpressionAST> mycc::Parser::parseExpression() {
  nts<AssignmentExpressionAST> list;
  do {
    list.emplace_back(parseAssignmentExpression());
  } while (expect(TokenKind::TOKEN_COMMA));
  return std::make_unique<ExpressionAST>(std::move(list));
}

///<assignment-expression> ::= <conditional-expression>
///                          | <unary-expression> <assignment-operator> <assignment-expression>
mycc::nt<mycc::AssignmentExpressionAST> mycc::Parser::parseAssignmentExpression() {
  // considers <unary-expression> as <conditional-expression> for consistency,
  // check whether the LHS is l-value on sema phrase.
  auto LHS = parseConditionalExpression();
  TokenKind op = lex.peek().getKind();
  switch (op) {
    case TokenKind::TOKEN_EQ:
    case TokenKind::TOKEN_STAREQ:
    case TokenKind::TOKEN_SLASHEQ:
    case TokenKind::TOKEN_PERCENTEQ:
    case TokenKind::TOKEN_PLUSEQ:
    case TokenKind::TOKEN_SUBEQ:
    case TokenKind::TOKEN_LTLTEQ:
    case TokenKind::TOKEN_GTGTEQ:
    case TokenKind::TOKEN_AMPEQ:
    case TokenKind::TOKEN_CARETEQ:
    case TokenKind::TOKEN_BAREQ:
      return std::make_unique<AssignmentExpressionAST>(LHS,
                                                       parseAssignmentOperator(),
                                                       parseAssignmentExpression());
    default:return std::make_unique<AssignmentExpressionAST>(LHS);
  }
}

///<conditional-expression> ::= <logical-or-expression>
///                           | <logical-or-expression> ? <expression> : <conditional-expression>

mycc::nt<mycc::ConditionalExpressionAST> mycc::Parser::parseConditionalExpression() {
  auto LHS = parseLogicalOrExpression();
  if (expect(TokenKind::TOKEN_QUES)) {
    auto exp = parseExpression();
    accept(TokenKind::TOKEN_COLON);
    auto conditional = parseConditionalExpression();
    return std::make_unique<ConditionalExpressionAST>(LHS, exp, conditional);
  } else {
    return std::make_unique<ConditionalExpressionAST>(LHS);
  }
}

// Traditional top down parsing will not work in parsing logical expressions.
// All logical expressions can be abstract as <logical-expression> <infixop> <logical-expression>
mycc::nt<mycc::LogicalOrExpressionAST> mycc::Parser::parseLogicalOrExpression() {
  return mycc::nt<mycc::LogicalOrExpressionAST>();
}
