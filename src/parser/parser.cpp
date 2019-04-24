#include <memory>
#include <parser/parser.h>
#include <tokens/token.h>
mycc::Parser::Parser(std::ifstream &ifstream)
    : in(ifstream), lex(ifstream) {}
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
int mycc::Parser::precedence(mycc::InfixOp op) {
  switch (op) {
    case InfixOp::BARBAR:return 1;
    case InfixOp::AMPAMP:return 2;
    case InfixOp::BAR:return 3;
    case InfixOp::CARET:return 4;
    case InfixOp::AMP:return 5;
    case InfixOp::EQEQ:
    case InfixOp::BANGEQ:return 6;
    case InfixOp::LT:
    case InfixOp::GT:
    case InfixOp::LTEQ:
    case InfixOp::GTEQ:return 7;
    case InfixOp::LTLT:
    case InfixOp::GTGT:return 8;
    case InfixOp::PLUS:
    case InfixOp::SUB:return 9;
    case InfixOp::STAR:
    case InfixOp::SLASH:
    case InfixOp::PERCENT:return 10;
  }
}
mycc::InfixOp mycc::Parser::isInfixOp(TokenKind kind) {
  InfixOp op;
  switch (kind) {
    case TokenKind::TOKEN_BARBAR:op = InfixOp::BARBAR;
      break;
    case TokenKind::TOKEN_AMPAMP:op = InfixOp::AMPAMP;
      break;
    case TokenKind::TOKEN_BAR:op = InfixOp::BAR;
      break;
    case TokenKind::TOKEN_CARET:op = InfixOp::CARET;
      break;
    case TokenKind::TOKEN_AMP:op = InfixOp::AMP;
      break;
    case TokenKind::TOKEN_EQEQ:op = InfixOp::EQEQ;
      break;
    case TokenKind::TOKEN_BANGEQ:op = InfixOp::BANGEQ;
      break;
    case TokenKind::TOKEN_LT:op = InfixOp::LT;
      break;
    case TokenKind::TOKEN_GT:op = InfixOp::GT;
      break;
    case TokenKind::TOKEN_LTEQ:op = InfixOp::LTEQ;
      break;
    case TokenKind::TOKEN_GTEQ:op = InfixOp::GTEQ;
      break;
    case TokenKind::TOKEN_LTLT:op = InfixOp::LTLT;
      break;
    case TokenKind::TOKEN_GTGT:op = InfixOp::GTGT;
      break;
    case TokenKind::TOKEN_PLUS:op = InfixOp::PLUS;
      break;
    case TokenKind::TOKEN_SUB:op = InfixOp::SUB;
      break;
    case TokenKind::TOKEN_STAR:op = InfixOp::STAR;
      break;
    case TokenKind::TOKEN_SLASH:op = InfixOp::SLASH;
      break;
    case TokenKind::TOKEN_PERCENT:op = InfixOp::PERCENT;
      break;
    default:throw NotAInfixOpException{};
  }
  return op;
}

bool mycc::Parser::isIdentiferAType(const std::string &name) {
  try {
    return pTable->lookup(name).isType();
  } catch (SymbolNotFoundException) {
    return false;
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
mycc::AssignmentOp mycc::Parser::parseAssignmentOperator() {
  AssignmentOp op;
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_EQ:op = AssignmentOp::EQ;
      break;
    case TokenKind::TOKEN_STAREQ:op = AssignmentOp::STAREQ;
      break;
    case TokenKind::TOKEN_SLASHEQ:op = AssignmentOp::SLASHEQ;
      break;
    case TokenKind::TOKEN_PERCENTEQ:op = AssignmentOp::PERCENTEQ;
      break;
    case TokenKind::TOKEN_PLUSEQ:op = AssignmentOp::PLUSEQ;
      break;
    case TokenKind::TOKEN_SUBEQ:op = AssignmentOp::SUBEQ;
      break;
    case TokenKind::TOKEN_LTLTEQ:op = AssignmentOp::LTLTEQ;
      break;
    case TokenKind::TOKEN_GTGTEQ:op = AssignmentOp::GTGTEQ;
      break;
    case TokenKind::TOKEN_AMPEQ:op = AssignmentOp::AMPEQ;
      break;
    case TokenKind::TOKEN_CARETEQ:op = AssignmentOp::CARETEQ;
      break;
    case TokenKind::TOKEN_BAREQ:op = AssignmentOp::BAREQ;
      break;
    default:
      throw parseError("assignment operator expected '=', '*=', '/=', '%=', '+=', '-=', '<<=', '>>=', '&=', '^=', '|='");
  }
  lex.consumeToken();
  return op;
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
      return std::make_unique<AssignmentExpressionAST>(std::move(LHS),
                                                       parseAssignmentOperator(),
                                                       parseAssignmentExpression());
    default:return std::make_unique<AssignmentExpressionAST>(std::move(LHS));
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
    return std::make_unique<ConditionalExpressionAST>(std::move(LHS), std::move(exp), std::move(conditional));
  } else {
    return std::make_unique<ConditionalExpressionAST>(std::move(LHS));
  }
}

// Traditional top down parsing will not work in parsing logical expressions.
// All logical expressions can be abstract as <term> <infixop> <term>
mycc::nt<mycc::LogicalOrExpressionAST> mycc::Parser::parseLogicalOrExpression(int calling_prec) {
  auto term1 = std::make_unique<LogicalOrExpressionAST>(parseCastExpression());
  while (true) {
    InfixOp op;
    try {
      op = isInfixOp(lex.peek().getKind());
    } catch (const NotAInfixOpException &e) {
      break;
    }
    auto prec = precedence(op);
    if (prec <= calling_prec) {
      return term1;
    } else {
      lex.consumeToken();
      auto term2 = parseLogicalOrExpression(prec);
      term1 = std::make_unique<LogicalOrExpressionAST>(std::move(term1), op, std::move(term2));
    }

  }
  return term1;
}

/// <declaration> ::=  {<declaration-specifier>}+ {<init-declarator>}* ;
mycc::nt<mycc::DeclarationAST> mycc::Parser::parseDeclaration() {
  nts<DeclarationSpecifierAST> specifiers;
  do {
    specifiers.push_back(parseDeclarationSpecifier());
    switch (lex.peek().getKind()) {
      case TokenKind::TOKEN_IDENTIFIER:if (!isIdentiferAType(lex.peek().getValue())) break;
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
      case TokenKind::TOKEN_VOLATILE:continue;
      default:break;
    }
    break;
  } while (true);

  nts<InitDeclaratorAST> init_decls;
  while (true) {
    switch (lex.peek().getKind()) {
      case TokenKind::TOKEN_IDENTIFIER:
      case TokenKind::TOKEN_LPAREN:
      case TokenKind::TOKEN_STAR:init_decls.push_back(parseInitDeclarator());
        continue;
      default:break;
    }
    break;
  }
  accept(TokenKind::TOKEN_SEMI);
  return std::make_unique<DeclarationAST>(std::move(specifiers), std::move(init_decls));
}

///<declaration-specifier> ::= <storage-class-specifier>
///                          | <type-specifier>
///                          | <type-qualifier>
mycc::nt<mycc::DeclarationSpecifierAST> mycc::Parser::parseDeclarationSpecifier() {
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_AUTO:
    case TokenKind::TOKEN_EXTERN:
    case TokenKind::TOKEN_REGISTER:
    case TokenKind::TOKEN_STATIC:
    case TokenKind::TOKEN_TYPEDEF:return std::make_unique<DeclarationSpecifierAST>(parseStorageClassSpecifier());
    case TokenKind::TOKEN_CHAR:
    case TokenKind::TOKEN_DOUBLE:
    case TokenKind::TOKEN_ENUM:
    case TokenKind::TOKEN_FLOAT:
    case TokenKind::TOKEN_INT:
    case TokenKind::TOKEN_LONG:
    case TokenKind::TOKEN_SHORT:
    case TokenKind::TOKEN_SIGNED:
    case TokenKind::TOKEN_STRUCT:
    case TokenKind::TOKEN_IDENTIFIER:
      if (!isIdentiferAType(lex.peek().getValue())) {
        throw parseError("except declaration specifier");
      }
    case TokenKind::TOKEN_UNION:
    case TokenKind::TOKEN_UNSIGNED:
    case TokenKind::TOKEN_VOID:return std::make_unique<DeclarationSpecifierAST>(parseTypeSpecifier());
    case TokenKind::TOKEN_CONST:
    case TokenKind::TOKEN_VOLATILE:return std::make_unique<DeclarationSpecifierAST>(parseTypeQualifier());
    default:throw parseError("except declaration specifier");
  }
}

///<init-declarator> ::= <declarator>
///                    | <declarator> = <initializer>
mycc::nt<mycc::InitDeclaratorAST> mycc::Parser::parseInitDeclarator() {
  auto decl = parseDeclarator(false);
  if (expect(TokenKind::TOKEN_EQ)) {
    return std::make_unique<InitDeclaratorAST>(std::move(decl), parseInitializer());
  } else {
    return std::make_unique<InitDeclaratorAST>(std::move(decl));
  }
}

///<declarator>          ::= {<pointer>}? <direct-declarator>
///<abstract-declarator> ::= <pointer>
///                        | <pointer>    <direct-abstract-declarator>
///                        |              <direct-abstract-declarator>
mycc::nt<mycc::DeclaratorAST> mycc::Parser::parseDeclarator() {
  nt<PointerAST> pointer = nullptr;
  if (lex.peek() == TokenKind::TOKEN_STAR) {
    pointer = parsePointer();
  }
  nt<DirectDeclaratorAST> dd = nullptr;
  if (lex.peek() == TokenKind::TOKEN_IDENTIFIER || lex.peek() == TokenKind::TOKEN_LPAREN) {
    return std::make_unique<DeclaratorAST>(pointer, parseDirectDeclarator());
  }
}

///<pointer> ::= * {<type-qualifier>}* {<pointer>}?
mycc::nt<mycc::PointerAST> mycc::Parser::parsePointer() {
  accept(TokenKind::TOKEN_STAR);
  nts<TypeQualifierAST> type_qualifiers;
  while (true) {
    if (lex.peek() == TokenKind::TOKEN_CONST || lex.peek() == TokenKind::TOKEN_VOLATILE) {
      type_qualifiers.push_back(parseTypeQualifier());
      continue;
    } else {
      break;
    }
  }
  nt<PointerAST> pointer;
  if (lex.peek() == TokenKind::TOKEN_STAR) {
    pointer = parsePointer();
  } else {
    pointer = nullptr;
  }
  return std::make_unique<PointerAST>(std::move(type_qualifiers), std::move(pointer));
}

///<direct-declarator> ::= <identifier>
///                      | ( <declarator> )
///                      | <direct-declarator> [ {<constant-expression>}? ]
///                      | <direct-declarator> ( <parameter-type-list> )
///                      | <direct-declarator> ( {<identifier>}* )

///<direct-abstract-declarator> ::=  ( <abstract-declarator> )
///       | {<direct-abstract-declarator>}? [ {<constant-expression>}? ]
///       | {<direct-abstract-declarator>}? ( {<parameter-type-list>}? )

mycc::nt<mycc::DirectDeclaratorAST> mycc::Parser::parseDirectDeclarator() {
  nt<AST> term1;
  if (lex.peek() == TokenKind::TOKEN_IDENTIFIER) {
    term1 = parseIdentifer();
  } else if (lex.peek() == TokenKind::TOKEN_LPAREN
      && (lex.peek(1) == TokenKind::TOKEN_LPAREN
          || lex.peek(1) == TokenKind::TOKEN_STAR
          || lex.peek(1) == TokenKind::TOKEN_LBRACKET
          || (lex.peek(1) == TokenKind::TOKEN_IDENTIFIER
              && !isIdentiferAType(lex.peek(1).getValue())))) {
    lex.consumeToken();
    term1 = parseDeclarator();
    accept(TokenKind::TOKEN_RPAREN);
  } else {
    term1 = nullptr;
  }

  std::vector<std::pair<DirectDeclaratorAST::Term2, nt<AST>>> term2s;
  while (lex.peek() == TokenKind::TOKEN_LBRACKET || lex.peek() == TokenKind::TOKEN_LPAREN) {
    if (expect(TokenKind::TOKEN_LBRACKET)) {
      switch (lex.peek().getKind()) {
        case TokenKind::TOKEN_BANG:
        case TokenKind::TOKEN_AMP:
        case TokenKind::TOKEN_LPAREN:
        case TokenKind::TOKEN_STAR:
        case TokenKind::TOKEN_PLUS:
        case TokenKind::TOKEN_PLUSPLUS:
        case TokenKind::TOKEN_SUB:
        case TokenKind::TOKEN_SUBSUB:
        case TokenKind::TOKEN_CHAR:
        case TokenKind::TOKEN_STRINGLITERAL:
        case TokenKind::TOKEN_IDENTIFIER:
        case TokenKind::TOKEN_FLOAT_CONSTANT:
        case TokenKind::TOKEN_INT_CONSTANT:
        case TokenKind::TOKEN_SIZEOF:
        case TokenKind::TOKEN_TILDE:
          term2s.emplace_back(DirectDeclaratorAST::Term2::CONST_EXPR,
                              parseConstantExpression());
          break;
        default:term2s.emplace_back(DirectDeclaratorAST::Term2::CONST_EXPR, nullptr);
      }
      accept(TokenKind::TOKEN_RBRACKET);
    } else {
      accept(TokenKind::TOKEN_LPAREN);
      switch (lex.peek().getKind()) {
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
        case TokenKind::TOKEN_VOLATILE:
          term2s.emplace_back(DirectDeclaratorAST::Term2::PARA_LIST,
                              parseParameterTypeList());
          break;
        case TokenKind::TOKEN_IDENTIFIER:
          if (isIdentiferAType(lex.peek().getValue())) {
            term2s.emplace_back(DirectDeclaratorAST::Term2::PARA_LIST, parseParameterTypeList());
          } else {
            do {
              term2s.emplace_back(DirectDeclaratorAST::Term2::ID, parseIdentifer());
            } while (lex.peek() == TokenKind::TOKEN_IDENTIFIER);
          }
          break;
        default:break;
      }
      accept(TokenKind::TOKEN_RPAREN);
    }
  }
  return std::make_unique<DirectDeclaratorAST>(std::move(term1), std::move(term2s));
}

///<parameter-type-list> ::= <parameter-list>
///                        | <parameter-list> , ...
mycc::nt<mycc::ParameterTypeListAST> mycc::Parser::parseParameterTypeList() {
  auto para_list = parseParameterList();
  bool has_multiple = false;
  if (expect(TokenKind::TOKEN_COMMA)) {
    accept(TokenKind::TOKEN_ELLIPSIS);
    has_multiple = true;
  }
  return std::make_unique<ParameterTypeListAST>(std::move(para_list), has_multiple);
}

///<parameter-list> ::= <parameter-declaration>
///                   | <parameter-list> , <parameter-declaration>
mycc::nt<mycc::ParameterListAST> mycc::Parser::parseParameterList() {
  nts<ParameterDeclarationAST> decls;
  do {
    decls.emplace_back(parseParameterDeclaration());
    if (!expect(TokenKind::TOKEN_COMMA)) {
      break;
    }
  } while (true);
  return std::make_unique<ParameterListAST>(decls);
}

///<parameter-declaration> ::= {<declaration-specifier>}+ <declarator>
///                          | {<declaration-specifier>}+ <abstract-declarator>
///                          | {<declaration-specifier>}+
mycc::nt<mycc::ParameterDeclarationAST> mycc::Parser::parseParameterDeclaration() {
  nts<DeclarationSpecifierAST> ds;
  do {
    ds.emplace_back(parseDeclarationSpecifier());
    switch (lex.peek().getKind()) {
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
      case TokenKind::TOKEN_IDENTIFIER:if (!isIdentiferAType(lex.peek().getValue())) break;
      case TokenKind::TOKEN_TYPEDEF:
      case TokenKind::TOKEN_UNION:
      case TokenKind::TOKEN_UNSIGNED:
      case TokenKind::TOKEN_VOID:
      case TokenKind::TOKEN_VOLATILE:continue;
    }
    break;
  } while (true);

  nt<DeclaratorAST> declarator;
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_LPAREN:
    case TokenKind::TOKEN_STAR:
    case TokenKind::TOKEN_LBRACKET:declarator = parseDeclarator();
    default:declarator = nullptr;
  }
  std::make_unique<ParameterDeclarationAST>(std::move(ds), std::move(declarator));
}

///<storage-class-specifier> ::= auto
///                            | register
///                            | static
///                            | extern
///                            | typedef
mycc::StorageSpecifier mycc::Parser::parseStorageClassSpecifier() {
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_AUTO:return StorageSpecifier::kAUTO;
    case TokenKind::TOKEN_REGISTER:return StorageSpecifier::kREGISTER;
    case TokenKind::TOKEN_STATIC:return StorageSpecifier::kSTATIC;
    case TokenKind::TOKEN_EXTERN:return StorageSpecifier::kEXTERN;
    case TokenKind::TOKEN_TYPEDEF:return StorageSpecifier::kTYPEDEF;
    default:throw parseError("expected storage class specifier");
  }
}

///<type-specifier> ::= void
//                   | char
//                   | short
//                   | int
//                   | long
//                   | float
//                   | double
//                   | signed
//                   | unsigned
//                   | <struct-or-union-specifier>
//                   | <enum-specifier>
//                   | <typedef-name>
mycc::nt<mycc::TypeSpecifierAST> mycc::Parser::parseTypeSpecifier() {
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_VOID:lex.consumeToken();
      return std::make_unique<TypeSpecifierAST>(ProtoTypeSpecifier::kVOID);
    case TokenKind::TOKEN_CHAR:lex.consumeToken();
      return std::make_unique<TypeSpecifierAST>(ProtoTypeSpecifier::kCHAR);
    case TokenKind::TOKEN_SHORT:lex.consumeToken();
      return std::make_unique<TypeSpecifierAST>(ProtoTypeSpecifier::kSHORT);
    case TokenKind::TOKEN_INT:lex.consumeToken();
      return std::make_unique<TypeSpecifierAST>(ProtoTypeSpecifier::kINT);
    case TokenKind::TOKEN_LONG:lex.consumeToken();
      return std::make_unique<TypeSpecifierAST>(ProtoTypeSpecifier::kLONG);
    case TokenKind::TOKEN_FLOAT:lex.consumeToken();
      return std::make_unique<TypeSpecifierAST>(ProtoTypeSpecifier::kFLOAT);
    case TokenKind::TOKEN_DOUBLE:lex.consumeToken();
      return std::make_unique<TypeSpecifierAST>(ProtoTypeSpecifier::kDOUBLE);
    case TokenKind::TOKEN_SIGNED:lex.consumeToken();
      return std::make_unique<TypeSpecifierAST>(ProtoTypeSpecifier::kSIGNED);
    case TokenKind::TOKEN_UNSIGNED:lex.consumeToken();
      return std::make_unique<TypeSpecifierAST>(ProtoTypeSpecifier::kUNSIGNED);
    case TokenKind::TOKEN_STRUCT:
    case TokenKind::TOKEN_UNION:return std::make_unique<TypeSpecifierAST>(parseStructOrUnionSpecifier());
    case TokenKind::TOKEN_ENUM:return std::make_unique<TypeSpecifierAST>(parseEnumSpecifier());
    case TokenKind::TOKEN_IDENTIFIER:
      if (isIdentiferAType(lex.peek().getValue())) {
        return std::make_unique<TypeSpecifierAST>(parseTypedefName());
      }
    default:throw parseError("expected type specifier");
  }
}

///<struct-or-union-specifier> ::= <struct-or-union> <identifier> { {<struct-declaration>}+ }
///                              | <struct-or-union> { {<struct-declaration>}+ }
///                              | <struct-or-union> <identifier>
mycc::nt<mycc::StructOrUnionSpecifierAST> mycc::Parser::parseStructOrUnionSpecifier() {
  StructOrUnion type;
  if (lex.peek() == TokenKind::TOKEN_STRUCT) {
    type = StructOrUnion::kSTRUCT;
    lex.consumeToken();
  } else if (lex.peek() == TokenKind::TOKEN_UNION) {
    type = StructOrUnion::kUNION;
    lex.consumeToken();
  } else {
    throw parseError(R"(struct or union-specifier must start with "struct" or "union")");
  }
  nt<IdentifierAST> id = nullptr;
  if (lex.peek() == TokenKind::TOKEN_IDENTIFIER) {
    id = parseIdentifer();
  }
  accept(TokenKind::TOKEN_LBRACE);
  nts<StructDeclarationAST> declarations;
  do {
    declarations.push_back(parseStructDeclaration());
    switch (lex.peek().getKind()) {
      case TokenKind::TOKEN_LPAREN:
      case TokenKind::TOKEN_STAR:
      case TokenKind::TOKEN_COLON:
      case TokenKind::TOKEN_CHAR:
      case TokenKind::TOKEN_CONST:
      case TokenKind::TOKEN_DOUBLE:
      case TokenKind::TOKEN_ENUM:
      case TokenKind::TOKEN_FLOAT:
      case TokenKind::TOKEN_IDENTIFIER:
      case TokenKind::TOKEN_INT:
      case TokenKind::TOKEN_LONG:
      case TokenKind::TOKEN_SHORT:
      case TokenKind::TOKEN_SIGNED:
      case TokenKind::TOKEN_STRUCT:
      case TokenKind::TOKEN_UNION:
      case TokenKind::TOKEN_UNSIGNED:
      case TokenKind::TOKEN_VOID:
      case TokenKind::TOKEN_VOLATILE:continue;
      default:break;
    }
    break;
  } while (true);
  accept(TokenKind::TOKEN_RBRACE);
  return std::make_unique<StructOrUnionSpecifierAST>(type, std::move(id), std::move(declarations));
}

///<struct-declaration> ::= {<specifier-qualifier>}* <struct-declarator-list>
mycc::nt<mycc::StructDeclarationAST> mycc::Parser::parseStructDeclaration() {
  nts<SpecifierQualifierAST> qualifiers;
  while (true) {
    switch (lex.peek().getKind()) {
      case TokenKind::TOKEN_CHAR:
      case TokenKind::TOKEN_CONST:
      case TokenKind::TOKEN_DOUBLE:
      case TokenKind::TOKEN_ENUM:
      case TokenKind::TOKEN_FLOAT:
      case TokenKind::TOKEN_INT:
      case TokenKind::TOKEN_LONG:
      case TokenKind::TOKEN_SHORT:
      case TokenKind::TOKEN_SIGNED:
      case TokenKind::TOKEN_STRUCT:
      case TokenKind::TOKEN_IDENTIFIER:if (!isIdentiferAType(lex.peek().getValue())) break;
      case TokenKind::TOKEN_UNION:
      case TokenKind::TOKEN_UNSIGNED:
      case TokenKind::TOKEN_VOID:
      case TokenKind::TOKEN_VOLATILE:qualifiers.push_back(parseSpecifierQualifier());
        continue;
      default:break;
    }
    break;
  }
  return std::make_unique<StructDeclarationAST>(std::move(qualifiers), parseStructDeclaratorList());
}

///<specifier-qualifier> ::= <type-specifier>
///                        | <type-qualifier>
mycc::nt<mycc::SpecifierQualifierAST> mycc::Parser::parseSpecifierQualifier() {
  if (lex.peek() == TokenKind::TOKEN_CONST || lex.peek() == TokenKind::TOKEN_VOLATILE) {
    return std::make_unique<SpecifierQualifierAST>(parseTypeQualifier());
  } else {
    return std::make_unique<SpecifierQualifierAST>(parseTypeSpecifier());
  }
}

///<struct-declarator-list> ::= <struct-declarator>
///                           | <struct-declarator-list> , <struct-declarator>
mycc::nt<mycc::StructDeclaratorListAST> mycc::Parser::parseStructDeclaratorList() {
  nts<StructDeclaratorAST> declarators;
  do {
    declarators.push_back(parseStructDeclarator());
  } while (expect(TokenKind::TOKEN_COMMA));
  return std::make_unique<StructDeclaratorListAST>(declarators);
}

///<struct-declarator> ::= <declarator>
///                      | <declarator> : <constant-expression>
///                      | : <constant-expression>
mycc::nt<mycc::StructDeclaratorAST> mycc::Parser::parseStructDeclarator() {
  if (expect(TokenKind::TOKEN_COLON)) {
    return std::make_unique<StructDeclaratorAST>(parseConstantExpression());
  } else {
    auto declarator = parseDeclarator();
    if (expect(TokenKind::TOKEN_COLON)) {
      return std::make_unique<StructDeclaratorAST>(std::move(declarator), parseConstantExpression());
    } else {
      return std::make_unique<StructDeclaratorAST>(std::move(declarator));
    }
  }
}
