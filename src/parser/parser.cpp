#include <memory>
#include <parser/parser.h>
#include <tokens/token.h>
Parser::Parser(std::ifstream &ifstream, SymbolTables &symbolTables)
    : in(ifstream), lex(ifstream), symbolTables(symbolTables) {
  table = symbolTables.createTable(ScopeKind::FILE, nullptr);
}
bool Parser::expect(TokenKind kind) {
  TokenKind peekKind = lex.peek().getKind();
  if (peekKind == kind) {
    lex.consumeToken();
    return true;
  } else {
    return false;
  }
}
const std::string &Parser::accept(TokenKind kind) {
  if (lex.peek() != kind) {
    lex.consumeToken();
    throw parseError(std::string("except ").append(Token::enumToString(kind)));
  } else {
    const std::string &name = lex.peek().getValue();
    lex.consumeToken();
    return name;
  }
}
ParserException Parser::parseError(const std::string &msg) {
  return ParserException(msg, lex.peek(-1));
}
int Parser::precedence(InfixOp op) {
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
InfixOp Parser::isInfixOp(TokenKind kind) {
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

/// <translation-unit> ::= {<external-declaration>}*
nt<TranslationUnitAST>
Parser::parseTranslationUnit() {
  nts<ExternalDeclarationAST> declarations;
  while (lex.peek() != TokenKind::TOKEN_EOF) {
    declarations.push_back(parseExternalDeclaration());
  }
  return std::make_unique<TranslationUnitAST>(std::move(declarations));
}

///<external-declaration> ::= <function-definition>
///                         | <declaration>
nt<ExternalDeclarationAST> Parser::parseExternalDeclaration() {
  // conflict, we merge to productions.
  //<declaration> 		  ::= {<declaration-specifier>}+ {<init-declarator>}* ;
  //<function-definition> ::= {<declaration-specifier>}+ <declarator> 		  {<declaration>}* <compound-statement>
  auto specifiers = parseDeclarationSpecifiers();
  if (specifiers->empty()) {
    throw SemaException("at least one declaration specifier needed", lex.peek());
  }
  auto init_declarators = parseInitDeclarators();
  // no <init-declarator> or <init-declarator> is more than one or <init-declarator> has initializer, it's a declaration
  if (init_declarators.empty() || init_declarators.size() > 1 || init_declarators.back().second != nullptr) {
    accept(TokenKind::TOKEN_SEMI);
    auto declaration = std::make_unique<DeclarationAST>(std::move(specifiers), std::move(init_declarators), *table);
    return std::make_unique<ExternalDeclarationAST>(std::move(declaration));
  } else {
    // only one declarator, we don't know what's this, continue parsing
    auto declarations = parseDeclarations();
    // has <declaration> or next token is a '{', this is a function definition
    if (!declarations.empty() || lex.peek() == TokenKind::TOKEN_LBRACE) {
      auto declarator = std::move(init_declarators.back().first);
      auto function_definition = std::make_unique<FunctionDefinitionAST>(std::move(specifiers),
                                                                         std::move(declarator),
                                                                         std::move(declarations),
                                                                         parseCompoundStatement(true));
      return std::make_unique<ExternalDeclarationAST>(std::move(function_definition));
    } else {
      // this is a declaration
      accept(TokenKind::TOKEN_SEMI);
      auto declaration = std::make_unique<DeclarationAST>(std::move(specifiers), std::move(init_declarators), *table);
      return std::make_unique<ExternalDeclarationAST>(std::move(declaration));
    }
  }
}

/// <typedef-name> ::= <identifier>
nt<TypedefNameAST> Parser::parseTypedefName() {
  return std::make_unique<TypedefNameAST>(parseIdentifier());
}
nt<IdentifierAST> Parser::parseIdentifier() {
  if (lex.peek() != TokenKind::TOKEN_IDENTIFIER) {
    throw parseError("expected identifier");
  } else {
    const Token &token = lex.peek();
    lex.consumeToken();
    return std::make_unique<IdentifierAST>(token);
  }
}

///<unary-operator> ::= &
///                   | *
///                   | +
///                   | -
///                   | ~
///                   | !
UnaryOp Parser::parseUnaryOperator() {
  UnaryOp op;
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_AMP:op = UnaryOp::AMP;
      break;
    case TokenKind::TOKEN_STAR:op = UnaryOp::STAR;
      break;
    case TokenKind::TOKEN_PLUS:op = UnaryOp::PLUS;
      break;
    case TokenKind::TOKEN_SUB:op = UnaryOp::SUB;
      break;
    case TokenKind::TOKEN_TILDE:op = UnaryOp::TILDE;
      break;
    case TokenKind::TOKEN_BANG:op = UnaryOp::BANG;
      break;
    default:throw parseError("unary operator expected '&', '*', '+', '-', '~', '!'");
  }
  return op;
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
AssignmentOp Parser::parseAssignmentOperator() {
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
nt<TypeQualifierAST> Parser::parseTypeQualifier() {
  TypeQuailifier quailifier;
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_CONST:quailifier = TypeQuailifier::kCONST;
      break;
    case TokenKind::TOKEN_VOLATILE:quailifier = TypeQuailifier::kVOLATILE;
      break;
    default:throw parseError(R"(type qualifier expected "const" or "volatile")");
  }
  lex.consumeToken();
  return std::make_unique<TypeQualifierAST>(Terminal<TypeQuailifier>(quailifier, lex.peek()));
}

///<enumerator> ::= <identifier>
///               | <identifier> = <constant-expression>
nt<EnumeratorAST> Parser::parseEnumerator() {
  auto id = parseIdentifier();
  if (expect(TokenKind::TOKEN_EQ)) {
    return std::make_unique<EnumeratorAST>(std::move(id), parseConstantExpression());
  } else {
    return std::make_unique<EnumeratorAST>(std::move(id));
  }
}

///<enumerator-list> ::= <enumerator>
///                    | <enumerator-list> , <enumerator>
nt<EnumeratorListAST> Parser::parseEnumeratorList() {
  nts<EnumeratorAST> list;
  do {
    list.emplace_back(parseEnumerator());
  } while (expect(TokenKind::TOKEN_COMMA));
  return std::make_unique<EnumeratorListAST>(std::move(list));
}

///<enum-specifier> ::= enum <identifier> { <enumerator-list> }
///                   | enum { <enumerator-list> }
///                   | enum <identifier>
nt<EnumSpecifierAST> Parser::parseEnumSpecifier() {
  accept(TokenKind::TOKEN_ENUM);
  if (lex.peek() == TokenKind::TOKEN_IDENTIFIER) {
    auto id = parseIdentifier();
    if (expect(TokenKind::TOKEN_LBRACE)) {
      auto p = std::make_unique<EnumSpecifierAST>(parseEnumeratorList());
      accept(TokenKind::TOKEN_RBRACE);
      return p;
    } else {
      return std::make_unique<EnumSpecifierAST>(std::move(id));
    }
  } else if (expect(TokenKind::TOKEN_LBRACE)) {
    auto p = std::make_unique<EnumSpecifierAST>(parseEnumeratorList());
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
nt<PrimaryExpressionAST> Parser::parsePrimaryExpression() {
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_IDENTIFIER:return std::make_unique<PrimaryExpressionAST>(parseIdentifier());
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
nt<ExpressionAST> Parser::parseExpression() {
  nts<AssignmentExpressionAST> list;
  do {
    list.emplace_back(parseAssignmentExpression());
  } while (expect(TokenKind::TOKEN_COMMA));
  return std::make_unique<ExpressionAST>(std::move(list));
}

///<assignment-expression> ::= <conditional-expression>
///                          | <unary-expression> <assignment-operator> <assignment-expression>
nt<AssignmentExpressionAST> Parser::parseAssignmentExpression() {
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
                                                       Terminal<AssignmentOp>(parseAssignmentOperator(), lex.peek()),
                                                       parseAssignmentExpression());
    default:return std::make_unique<AssignmentExpressionAST>(std::move(LHS));
  }
}

///<conditional-expression> ::= <logical-or-expression>
///                           | <logical-or-expression> ? <expression> : <conditional-expression>

nt<ConditionalExpressionAST> Parser::parseConditionalExpression() {
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
nt<LogicalOrExpressionAST> Parser::parseLogicalOrExpression(int calling_prec) {
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
      term1 = std::make_unique<LogicalOrExpressionAST>(std::move(term1),
                                                       Terminal<InfixOp>(op, lex.peek()),
                                                       std::move(term2));
    }

  }
  return term1;
}

/// <declaration> ::=  {<declaration-specifier>}+ {<init-declarator>}* ;
nts<DeclarationAST> Parser::parseDeclarations() {
  nts<DeclarationAST> declarations;
  while (true) {
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
        declarations.push_back(std::make_unique<DeclarationAST>(parseDeclarationSpecifiers(),
                                                                parseInitDeclarators(), *table));
        accept(TokenKind::TOKEN_SEMI);
        continue;
      case TokenKind::TOKEN_IDENTIFIER:
        if (table->isTypedef(lex.peek())) {
          declarations.push_back(std::make_unique<DeclarationAST>(parseDeclarationSpecifiers(),
                                                                  parseInitDeclarators(), *table));
          continue;
        } else {
          break;
        }
      default:break;
    }
    break;
  }
  return declarations;
}

///<declaration-specifier> ::= <storage-class-specifier>
///                          | <type-specifier>
///                          | <type-qualifier>
nt<DeclarationSpecifiersAST> Parser::parseDeclarationSpecifiers() {
  ts<StorageSpecifier> storage_specifiers;
  nts<TypeSpecifierAST> type_specifiers;
  nts<TypeQualifierAST> type_qualifiers;
  while (true) {
    const auto &token = lex.peek();
    switch (token.getKind()) {
      case TokenKind::TOKEN_AUTO:
      case TokenKind::TOKEN_REGISTER:
      case TokenKind::TOKEN_EXTERN:
      case TokenKind::TOKEN_STATIC:
      case TokenKind::TOKEN_TYPEDEF:storage_specifiers.emplace_back(parseStorageClassSpecifier(), token);
        continue;
      case TokenKind::TOKEN_IDENTIFIER:
        if (!table->isTypedef(lex.peek())) {
          break;
        }
      case TokenKind::TOKEN_CHAR:
      case TokenKind::TOKEN_DOUBLE:
      case TokenKind::TOKEN_ENUM:
      case TokenKind::TOKEN_FLOAT:
      case TokenKind::TOKEN_INT:
      case TokenKind::TOKEN_LONG:
      case TokenKind::TOKEN_SHORT:
      case TokenKind::TOKEN_SIGNED:
      case TokenKind::TOKEN_STRUCT:
      case TokenKind::TOKEN_UNION:
      case TokenKind::TOKEN_UNSIGNED:
      case TokenKind::TOKEN_VOID:type_specifiers.push_back(parseTypeSpecifier());
        continue;
      case TokenKind::TOKEN_CONST:
      case TokenKind::TOKEN_VOLATILE:type_qualifiers.push_back(parseTypeQualifier());
        continue;
      default:break;
    }
    break;
  }
  return std::make_unique<DeclarationSpecifiersAST>(std::move(storage_specifiers),
                                                    std::move(type_specifiers),
                                                    std::move(type_qualifiers));
}

///<init-declarator> ::= <declarator>
///                    | <declarator> = <initializer>

/// init-declarator-list:
///                  init-declarator
///                  init-declarator-list , init-declarator
InitDeclarators Parser::parseInitDeclarators() {
  std::vector<std::pair<nt<DeclaratorAST>, nt<InitializerAST>>> init_declarators;
  while (true) {
    switch (lex.peek().getKind()) {
      case TokenKind::TOKEN_IDENTIFIER:
      case TokenKind::TOKEN_LPAREN:
      case TokenKind::TOKEN_STAR: {
        auto decl = parseDeclarator();
        if (expect(TokenKind::TOKEN_EQ)) {
          init_declarators.emplace_back(std::move(decl), parseInitializer());
        } else {
          init_declarators.emplace_back(std::move(decl), nullptr);
        }
        if (expect(TokenKind::TOKEN_COMMA)) continue;
        break;
      }
      default:return init_declarators;
    }
  }
}

///<declarator>          ::= {<pointer>}? <direct-declarator>
///<abstract-declarator> ::= <pointer>
///                        | <pointer>    <direct-abstract-declarator>
///                        |              <direct-abstract-declarator>
nt<DeclaratorAST> Parser::parseDeclarator() {
  nt<PointerAST> pointer = nullptr;
  if (lex.peek() == TokenKind::TOKEN_STAR) {
    pointer = parsePointer();
  }
  nt<DirectDeclaratorAST> dd = nullptr;
  if (lex.peek() == TokenKind::TOKEN_IDENTIFIER || lex.peek() == TokenKind::TOKEN_LPAREN) {
    dd = parseDirectDeclarator();
  }
  return std::make_unique<DeclaratorAST>(std::move(pointer), std::move(dd));
}

///<pointer> ::= * {<type-qualifier>}* {<pointer>}?
nt<PointerAST> Parser::parsePointer() {
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

nt<DirectDeclaratorAST> Parser::parseDirectDeclarator() {
  nt<AST> term1;
  if (lex.peek() == TokenKind::TOKEN_IDENTIFIER) {
    term1 = parseIdentifier();
  } else if (lex.peek() == TokenKind::TOKEN_LPAREN
      && (lex.peek(1) == TokenKind::TOKEN_LPAREN
          || lex.peek(1) == TokenKind::TOKEN_STAR
          || lex.peek(1) == TokenKind::TOKEN_LBRACKET
          || (lex.peek(1) == TokenKind::TOKEN_IDENTIFIER
              && !table->isTypedef(lex.peek(1))))) {
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
          lex.consumeToken();
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
          if (table->isTypedef(lex.peek())) {
            term2s.emplace_back(DirectDeclaratorAST::Term2::PARA_LIST, parseParameterTypeList());
          } else {
            do {
              term2s.emplace_back(DirectDeclaratorAST::Term2::ID, parseIdentifier());
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
nt<ParameterTypeListAST> Parser::parseParameterTypeList() {
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
nt<ParameterListAST> Parser::parseParameterList() {
  SymbolScope s(table, symbolTables.createTable(ScopeKind::FUNCTION_PROTOTYPE, table));
  nts<ParameterDeclarationAST> decls;
  do {
    decls.emplace_back(parseParameterDeclaration());
    if (lex.peek() != TokenKind::TOKEN_COMMA || lex.peek(1) == TokenKind::TOKEN_ELLIPSIS) {
      break;
    } else {
      lex.consumeToken();
    }
  } while (true);
  return std::make_unique<ParameterListAST>(std::move(decls), *table);
}

///<parameter-declaration> ::= {<declaration-specifier>}+ <declarator>
///                          | {<declaration-specifier>}+ <abstract-declarator>
///                          | {<declaration-specifier>}+
nt<ParameterDeclarationAST> Parser::parseParameterDeclaration() {
  auto ds = parseDeclarationSpecifiers();

  nt<DeclaratorAST> declarator;
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_LPAREN:
    case TokenKind::TOKEN_STAR:
    case TokenKind::TOKEN_IDENTIFIER:
    case TokenKind::TOKEN_LBRACKET:declarator = parseDeclarator();
      break;
    default:declarator = nullptr;
  }
  return std::make_unique<ParameterDeclarationAST>(std::move(ds), std::move(declarator));
}

///<storage-class-specifier> ::= auto
///                            | register
///                            | static
///                            | extern
///                            | typedef
StorageSpecifier Parser::parseStorageClassSpecifier() {
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_AUTO:lex.consumeToken();
      return StorageSpecifier::kAUTO;
    case TokenKind::TOKEN_REGISTER:lex.consumeToken();
      return StorageSpecifier::kREGISTER;
    case TokenKind::TOKEN_STATIC:lex.consumeToken();
      return StorageSpecifier::kSTATIC;
    case TokenKind::TOKEN_EXTERN:lex.consumeToken();
      return StorageSpecifier::kEXTERN;
    case TokenKind::TOKEN_TYPEDEF:lex.consumeToken();
      return StorageSpecifier::kTYPEDEF;
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
nt<TypeSpecifierAST> Parser::parseTypeSpecifier() {
  ProtoTypeSpecifier specifier;
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_VOID:specifier = (ProtoTypeSpecifier::kVOID);
      goto handle_proto_type;
    case TokenKind::TOKEN_CHAR:specifier = (ProtoTypeSpecifier::kCHAR);
      goto handle_proto_type;
    case TokenKind::TOKEN_SHORT:specifier = (ProtoTypeSpecifier::kSHORT);
      goto handle_proto_type;
    case TokenKind::TOKEN_INT:specifier = (ProtoTypeSpecifier::kINT);
      goto handle_proto_type;
    case TokenKind::TOKEN_LONG:specifier = (ProtoTypeSpecifier::kLONG);
      goto handle_proto_type;
    case TokenKind::TOKEN_FLOAT:specifier = (ProtoTypeSpecifier::kFLOAT);
      goto handle_proto_type;
    case TokenKind::TOKEN_DOUBLE:specifier = (ProtoTypeSpecifier::kDOUBLE);
      goto handle_proto_type;
    case TokenKind::TOKEN_SIGNED:specifier = (ProtoTypeSpecifier::kSIGNED);
      goto handle_proto_type;
    case TokenKind::TOKEN_UNSIGNED:specifier = (ProtoTypeSpecifier::kUNSIGNED);
      goto handle_proto_type;
    case TokenKind::TOKEN_STRUCT:
    case TokenKind::TOKEN_UNION:return std::make_unique<TypeSpecifierAST>(parseStructOrUnionSpecifier());
    case TokenKind::TOKEN_ENUM:return std::make_unique<TypeSpecifierAST>(parseEnumSpecifier());
    case TokenKind::TOKEN_IDENTIFIER:
      if (table->isTypedef(lex.peek())) {
        return std::make_unique<TypeSpecifierAST>(parseTypedefName());
      }
    default:throw parseError("expected type specifier");
  }
  handle_proto_type:
  auto ast = std::make_unique<TypeSpecifierAST>(Terminal<ProtoTypeSpecifier>(specifier, lex.peek()));
  lex.consumeToken();
  return std::move(ast);

}

///<struct-or-union-specifier> ::= <struct-or-union> <identifier> { {<struct-declaration>}+ }
///                              | <struct-or-union> { {<struct-declaration>}+ }
///                              | <struct-or-union> <identifier>
nt<StructOrUnionSpecifierAST> Parser::parseStructOrUnionSpecifier() {
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
    id = parseIdentifier();
  }
  nts<StructDeclarationAST> declarations;
  if(!expect(TokenKind::TOKEN_LBRACE)) {
    return std::make_unique<StructOrUnionSpecifierAST>(type, std::move(id), std::move(declarations));
  }

  do {
    declarations.push_back(parseStructDeclaration());
    TokenKind kind = lex.peek().getKind();
    switch (kind) {
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

///<struct-declaration> ::= {<specifier-qualifier>}* <struct-declarator-list> ;
nt<StructDeclarationAST> Parser::parseStructDeclaration() {
  nts<SpecifierQualifierAST> qualifiers;
  while (true) {
    switch (lex.peek().getKind()) {
      case TokenKind::TOKEN_IDENTIFIER:if (!table->isTypedef(lex.peek()))break;
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
      case TokenKind::TOKEN_UNION:
      case TokenKind::TOKEN_UNSIGNED:
      case TokenKind::TOKEN_VOID:
      case TokenKind::TOKEN_VOLATILE:qualifiers.push_back(parseSpecifierQualifier());
        continue;
      default:break;
    }
    break;
  }
  auto ast = std::make_unique<StructDeclarationAST>(std::move(qualifiers), parseStructDeclaratorList());
  accept(TokenKind::TOKEN_SEMI);
  return ast;
}

///<specifier-qualifier> ::= <type-specifier>
///                        | <type-qualifier>
nt<SpecifierQualifierAST> Parser::parseSpecifierQualifier() {
  if (lex.peek() == TokenKind::TOKEN_CONST || lex.peek() == TokenKind::TOKEN_VOLATILE) {
    return std::make_unique<SpecifierQualifierAST>(parseTypeQualifier());
  } else {
    return std::make_unique<SpecifierQualifierAST>(parseTypeSpecifier());
  }
}

///<struct-declarator-list> ::= <struct-declarator>
///                           | <struct-declarator-list> , <struct-declarator>
nt<StructDeclaratorListAST> Parser::parseStructDeclaratorList() {
  nts<StructDeclaratorAST> declarators;
  do {
    declarators.push_back(parseStructDeclarator());
  } while (expect(TokenKind::TOKEN_COMMA));
  return std::make_unique<StructDeclaratorListAST>(std::move(declarators));
}

///<struct-declarator> ::= <declarator>
///                      | <declarator> : <constant-expression>
///                      | : <constant-expression>
nt<StructDeclaratorAST> Parser::parseStructDeclarator() {
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

///<initializer> ::= <assignment-expression>
///                | { <initializer-list> }
///                | { <initializer-list> , }
nt<InitializerAST> Parser::parseInitializer() {
  if (expect(TokenKind::TOKEN_LBRACE)) {
    auto exp = parseInitializerList();
    expect(TokenKind::TOKEN_COMMA);
    accept(TokenKind::TOKEN_LBRACE);
    return std::make_unique<InitializerAST>(std::move(exp));
  } else {
    return std::make_unique<InitializerAST>(parseAssignmentExpression());
  }
}

///<initializer-list> ::= <initializer>
///                     | <initializer-list> , <initializer>
nt<InitializerListAST> Parser::parseInitializerList() {
  nts<InitializerAST> list;
  do {
    list.push_back(parseInitializer());
  } while (expect(TokenKind::TOKEN_COMMA));
  return std::make_unique<InitializerListAST>(std::move(list));
}

///<function-definition> ::= {<declaration-specifier>}* <declarator> {<declaration>}* <compound-statement>
// we use {<declaration-specifier>}+ here
nt<FunctionDefinitionAST> Parser::parseFunctionDefinition() {
  auto specifiers = parseDeclarationSpecifiers();
  auto declarator = parseDeclarator();
  auto declarations = parseDeclarations();
  auto compound = parseCompoundStatement(true);
  return std::make_unique<FunctionDefinitionAST>(std::move(specifiers),
                                                 std::move(declarator),
                                                 std::move(declarations),
                                                 std::move(compound));
}

///<compound-statement> ::= { {<declaration>}* {<statement>}* }
nt<CompoundStatementAST> Parser::parseCompoundStatement(bool function) {
  SymbolScope s(table, symbolTables.createTable(function ? ScopeKind::FUNCTION : ScopeKind::BLOCK, table));
  accept(TokenKind::TOKEN_LBRACE);
  auto declarations = parseDeclarations();

  nts<StatementAST> statements{};
  while (lex.peek() != TokenKind::TOKEN_RBRACE) {
    statements.push_back(parseStatement());
  }
  accept(TokenKind::TOKEN_RBRACE);
  return std::make_unique<CompoundStatementAST>(std::move(declarations), std::move(statements), *table);
}

///<statement> ::= <labeled-statement>
///              | <expression-statement>
///              | <compound-statement>
///              | <selection-statement>
///              | <iteration-statement>
///              | <jump-statement>
nt<StatementAST> Parser::parseStatement() {
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_CASE:
    case TokenKind::TOKEN_DEFAULT:
    case TokenKind::TOKEN_IDENTIFIER:
      if (lex.peek(1).getKind() == TokenKind::TOKEN_COLON) {
        return std::make_unique<StatementAST>(parseLabeledStatement());
      } else {
        return std::make_unique<StatementAST>(parseExpressionStatement());
      }
    case TokenKind::TOKEN_LBRACE:return std::make_unique<StatementAST>(parseCompoundStatement(false));
    case TokenKind::TOKEN_IF:
    case TokenKind::TOKEN_SWITCH:return std::make_unique<StatementAST>(parseSelectionStatement());
    case TokenKind::TOKEN_DO:
    case TokenKind::TOKEN_FOR:
    case TokenKind::TOKEN_WHILE:return std::make_unique<StatementAST>(parseIterationStatement());
    case TokenKind::TOKEN_BREAK:
    case TokenKind::TOKEN_CONTINUE:
    case TokenKind::TOKEN_GOTO:
    case TokenKind::TOKEN_RETURN:return std::make_unique<StatementAST>(parseJumpStatement());
    default:return std::make_unique<StatementAST>(parseExpressionStatement());
  }
}

///<labeled-statement> ::= <identifier> : <statement>
///                      | case <constant-expression> : <statement>
///                      | default : <statement>
nt<LabeledStatementAST> Parser::parseLabeledStatement() {
  if (lex.peek() == TokenKind::TOKEN_IDENTIFIER) {
    auto id = parseIdentifier();
    accept(TokenKind::TOKEN_COLON);
    return std::make_unique<LabeledStatementAST>(std::move(id), parseStatement());
  } else if (expect(TokenKind::TOKEN_CASE)) {
    auto exp = parseConstantExpression();
    accept(TokenKind::TOKEN_COLON);
    return std::make_unique<LabeledStatementAST>(std::move(exp), parseStatement());
  } else if (expect(TokenKind::TOKEN_DEFAULT)) {
    accept(TokenKind::TOKEN_COLON);
    return std::make_unique<LabeledStatementAST>(parseStatement());
  } else {
    throw parseError(R"(labeled statement expects "case", "default" or an identifer)");
  }
}

///<expression-statement> ::= {<expression>}? ;
nt<ExpressionStatementAST> Parser::parseExpressionStatement() {
  nt<ExpressionAST> exp = nullptr;
  if (!expect(TokenKind::TOKEN_SEMI)) {
    exp = parseExpression();
    accept(TokenKind::TOKEN_SEMI);
  }
  return std::make_unique<ExpressionStatementAST>(std::move(exp));
}

///<selection-statement> ::= if ( <expression> ) <statement>
///                        | if ( <expression> ) <statement> else <statement>
///                        | switch ( <expression> ) <statement>
nt<SelectionStatementAST> Parser::parseSelectionStatement() {
  if (expect(TokenKind::TOKEN_IF)) {
    accept(TokenKind::TOKEN_LPAREN);
    auto condition = parseExpression();
    accept(TokenKind::TOKEN_RPAREN);
    auto if_statement = parseStatement();
    if (expect(TokenKind::TOKEN_ELSE)) {
      return std::make_unique<SelectionStatementAST>(std::move(condition), std::move(if_statement), parseStatement());
    } else {
      return std::make_unique<SelectionStatementAST>(std::move(condition), std::move(if_statement), true);
    }
  } else {
    accept(TokenKind::TOKEN_SWITCH);
    accept(TokenKind::TOKEN_LPAREN);
    auto exp = parseExpression();
    accept(TokenKind::TOKEN_RPAREN);
    return std::make_unique<SelectionStatementAST>(std::move(exp), parseStatement(), false);
  }
}

///<iteration-statement> ::= while ( <expression> ) <statement>
///                        | do <statement> while ( <expression> ) ;
///                        | for ( {<expression>}? ; {<expression>}? ; {<expression>}? ) <statement>
nt<IterationStatementAST> Parser::parseIterationStatement() {
  if (expect(TokenKind::TOKEN_WHILE)) {
    accept(TokenKind::TOKEN_LPAREN);
    auto exp = parseExpression();
    accept(TokenKind::TOKEN_RPAREN);
    return std::make_unique<IterationStatementAST>(std::move(exp), parseStatement());
  } else if (expect(TokenKind::TOKEN_DO)) {
    auto statement = parseStatement();
    accept(TokenKind::TOKEN_WHILE);
    accept(TokenKind::TOKEN_LPAREN);
    auto exp = parseExpression();
    accept(TokenKind::TOKEN_RPAREN);
    accept(TokenKind::TOKEN_SEMI);
    return std::make_unique<IterationStatementAST>(std::move(statement), std::move(exp));
  } else {
    accept(TokenKind::TOKEN_FOR);
    accept(TokenKind::TOKEN_LPAREN);
    nt<ExpressionAST> term1 = nullptr;
    nt<ExpressionAST> term2 = nullptr;
    nt<ExpressionAST> term3 = nullptr;
    if (!expect(TokenKind::TOKEN_SEMI)) {
      term1 = parseExpression();
      accept(TokenKind::TOKEN_SEMI);
    }
    if (!expect(TokenKind::TOKEN_SEMI)) {
      term2 = parseExpression();
      accept(TokenKind::TOKEN_SEMI);
    }
    if (!expect(TokenKind::TOKEN_RPAREN)) {
      term3 = parseExpression();
      accept(TokenKind::TOKEN_RPAREN);
    }
    return std::make_unique<IterationStatementAST>(std::move(term1),
                                                   std::move(term2),
                                                   std::move(term3),
                                                   parseStatement());
  }
}

///<jump-statement> ::= goto <identifier> ;
///                   | continue ;
///                   | break ;
///                   | return {<expression>}? ;
nt<JumpStatementAST> Parser::parseJumpStatement() {
  if (expect(TokenKind::TOKEN_GOTO)) {
    auto id = parseIdentifier();
    accept(TokenKind::TOKEN_SEMI);
    return std::make_unique<JumpStatementAST>(std::move(id));
  } else if (expect(TokenKind::TOKEN_RETURN)) {
    nt<ExpressionAST> exp = nullptr;
    if (!expect(TokenKind::TOKEN_SEMI)) {
      exp = parseExpression();
      accept(TokenKind::TOKEN_SEMI);
    }
    return std::make_unique<JumpStatementAST>(std::move(exp));
  } else {
    bool is_continue;
    if (expect(TokenKind::TOKEN_CONTINUE)) {
      is_continue = true;
    } else {
      accept(TokenKind::TOKEN_BREAK);
      is_continue = false;
    }
    return std::make_unique<JumpStatementAST>(is_continue);
  }
}

///<constant-expression> ::= <conditional-expression>
nt<ConstantExpressionAST> Parser::parseConstantExpression() {
  return std::make_unique<ConstantExpressionAST>(parseConditionalExpression());
}

///<cast-expression> ::= <unary-expression>
///                    | ( <type-name> ) <cast-expression>
nt<CastExpressionAST> Parser::parseCastExpression() {
  if (lex.peek() == TokenKind::TOKEN_LPAREN
      && lex.peek(1) == TokenKind::TOKEN_IDENTIFIER
      && table->isTypedef(lex.peek(1))) {
    accept(TokenKind::TOKEN_LPAREN);
    auto type_name = parseTypeName();
    accept(TokenKind::TOKEN_RPAREN);
    return std::make_unique<CastExpressionAST>(std::move(type_name), parseCastExpression());
  } else {
    return std::make_unique<CastExpressionAST>(parseUnaryExpression());
  }
}

///<unary-expression> ::= <postfix-expression>
///                     | ++ <unary-expression>
///                     | -- <unary-expression>
///                     | <unary-operator> <cast-expression>
///                     | sizeof <unary-expression>
///                     | sizeof <type-name>
nt<UnaryExpressionAST> Parser::parseUnaryExpression() {
  if (expect(TokenKind::TOKEN_PLUSPLUS)) {
    return std::make_unique<UnaryExpressionAST>(parseUnaryExpression(), UnaryExpressionAST::PrefixType::PLUSPLUS);
  } else if (expect(TokenKind::TOKEN_SUBSUB)) {
    return std::make_unique<UnaryExpressionAST>(parseUnaryExpression(), UnaryExpressionAST::PrefixType::SUBSUB);
  } else if (expect(TokenKind::TOKEN_SIZEOF)) {
    return std::make_unique<UnaryExpressionAST>(parseUnaryExpression(), UnaryExpressionAST::PrefixType::SIZE_OF);
  } else if (lex.peek() == TokenKind::TOKEN_BANG
      || lex.peek() == TokenKind::TOKEN_AMP
      || lex.peek() == TokenKind::TOKEN_STAR
      || lex.peek() == TokenKind::TOKEN_PLUS
      || lex.peek() == TokenKind::TOKEN_SUB
      || lex.peek() == TokenKind::TOKEN_TILDE) {
    return std::make_unique<UnaryExpressionAST>(Terminal<UnaryOp>(parseUnaryOperator(), lex.peek()),
                                                parseCastExpression());
  } else {
    return std::make_unique<UnaryExpressionAST>(parsePostfixExpression());
  }
}

///<postfix-expression> ::= <primary-expression>
//                       | <postfix-expression> [ <expression> ]
//                       | <postfix-expression> ( {<assignment-expression>}* )
//                       | <postfix-expression> . <identifier>
//                       | <postfix-expression> -> <identifier>
//                       | <postfix-expression> ++
//                       | <postfix-expression> --
nt<PostfixExpressionAST> Parser::parsePostfixExpression() {
  auto primary = parsePrimaryExpression();
  std::vector<std::pair<int, nt<AST>>> terms;
  while (true) {
    switch (lex.peek().getKind()) {
      case TokenKind::TOKEN_LBRACKET:lex.consumeToken();
        terms.emplace_back(1, parseExpression());
        accept(TokenKind::TOKEN_RBRACKET);
        continue;
      case TokenKind::TOKEN_LPAREN:lex.consumeToken();
        while (!expect(TokenKind::TOKEN_RPAREN)) {
          terms.emplace_back(2, parseAssignmentExpression());
        }
        continue;
      case TokenKind::TOKEN_DOT:lex.consumeToken();
        terms.emplace_back(3, parseIdentifier());
        continue;
      case TokenKind::TOKEN_ARROW:lex.consumeToken();
        terms.emplace_back(4, parseIdentifier());
        continue;
      case TokenKind::TOKEN_PLUSPLUS:lex.consumeToken();
        terms.emplace_back(5, nullptr);
        continue;
      case TokenKind::TOKEN_SUBSUB:lex.consumeToken();
        terms.emplace_back(6, nullptr);
        continue;
      default:break;
    }
    break;
  }
  return std::make_unique<PostfixExpressionAST>(std::move(primary), std::move(terms));
}

///<type-name> ::= {<specifier-qualifier>}+ {<abstract-declarator>}?
nt<TypeNameAST> Parser::parseTypeName() {
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
      case TokenKind::TOKEN_IDENTIFIER:
        if (!table->isTypedef(lex.peek())) {
          break;
        }
      case TokenKind::TOKEN_UNION:
      case TokenKind::TOKEN_UNSIGNED:
      case TokenKind::TOKEN_VOID:
      case TokenKind::TOKEN_VOLATILE:qualifiers.push_back(parseSpecifierQualifier());
        continue;
      default:break;
    }
    break;
  }

  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_LPAREN:
    case TokenKind::TOKEN_STAR:
    case TokenKind::TOKEN_LBRACKET:return std::make_unique<TypeNameAST>(std::move(qualifiers), parseDeclarator());
    default:return std::make_unique<TypeNameAST>(std::move(qualifiers), nullptr);
  }
}
ParserException Parser::parseError(const std::string &msg, const Token &token) {
  return ParserException(msg, token);
}

ParserException::ParserException(std::string error, const Token &token) :
    token(token), error(std::move(error)) {
  this->error.append("\n").append(token.getTokenInLine());
}
const char *ParserException::what() const noexcept {
  return error.c_str();
}
