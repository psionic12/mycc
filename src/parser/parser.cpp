#include <memory>
#include <parser/parser.h>
#include <tokens/token.h>
#include <tokens/specifier_combination.h>
Parser::Parser(std::ifstream &ifstream)
    : in(ifstream), lex(ifstream), symbolTables(AST::getTables()) {
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
  mStartToken = &lex.peek();
  nts<ExternalDeclarationAST> declarations;
  table = symbolTables.createTable(ScopeKind::FILE);
  while (lex.peek() != TokenKind::TOKEN_EOF) {
    declarations.push_back(parseExternalDeclaration());
  }
  auto *tagTable = symbolTables.createTable(ScopeKind::FILE);
  //remove all typedef symbol's which is used only in parser
  symbolTables.clear();
  return make_ast<TranslationUnitAST>(std::move(declarations), *table, *tagTable);
}

///<external-declaration> ::= <function-definition>
///                         | <declaration>
nt<ExternalDeclarationAST> Parser::parseExternalDeclaration() {
  mStartToken = &lex.peek();
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
    auto declaration = make_ast<DeclarationAST>(std::move(specifiers), std::move(init_declarators), *table);
    return make_ast<ExternalDeclarationAST>(std::move(declaration));
  } else {
    // only one declarator, we don't know what's this, continue parsing
    auto declarations = parseDeclarations();
    // has <declaration> or next token is a '{', this is a function definition
    if (!declarations.empty() || lex.peek() == TokenKind::TOKEN_LBRACE) {
      auto declarator = std::move(init_declarators.back().first);
      auto *labelTable = symbolTables.createTable(ScopeKind::LABEL);
      auto function_definition = make_ast<FunctionDefinitionAST>(std::move(specifiers),
                                                                 std::move(declarator),
                                                                 std::move(declarations),
                                                                 parseCompoundStatement(),
                                                                 *labelTable);
      // common mistake
      if (expect(TokenKind::TOKEN_SEMI)) {
        throw SemaException("function definition should end without semi", lex.peek(-1));
      }
      return make_ast<ExternalDeclarationAST>(std::move(function_definition));
    } else {
      // this is a declaration
      accept(TokenKind::TOKEN_SEMI);
      auto declaration = make_ast<DeclarationAST>(std::move(specifiers), std::move(init_declarators), *table);
      return make_ast<ExternalDeclarationAST>(std::move(declaration));
    }
  }
}

/// <typedef-name> ::= <identifier>
nt<TypedefNameAST> Parser::parseTypedefName() {
  mStartToken = &lex.peek();
  return make_ast<TypedefNameAST>(parseIdentifier());
}
nt<IdentifierAST> Parser::parseIdentifier() {
  mStartToken = &lex.peek();
  if (lex.peek() != TokenKind::TOKEN_IDENTIFIER) {
    throw parseError("expected identifier");
  } else {
    const Token &token = lex.peek();
    lex.consumeToken();
    return make_ast<IdentifierAST>(token);
  }
}

///<unary-operator> ::= &
///                   | *
///                   | +
///                   | -
///                   | ~
///                   | !
Terminal<UnaryOp> Parser::parseUnaryOperator() {
  mStartToken = &lex.peek();
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
  auto terminal = Terminal<UnaryOp>(op, lex.peek());
  lex.consumeToken();
  return terminal;
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
  mStartToken = &lex.peek();
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
  mStartToken = &lex.peek();
  TypeQualifier quailifier;
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_CONST:quailifier = TypeQualifier::kCONST;
      break;
    case TokenKind::TOKEN_VOLATILE:quailifier = TypeQualifier::kVOLATILE;
      break;
    default:throw parseError(R"(type qualifier expected "const" or "volatile")");
  }
  lex.consumeToken();
  return make_ast<TypeQualifierAST>(Terminal<TypeQualifier>(quailifier, lex.peek()));
}

///<enumerator> ::= <identifier>
///               | <identifier> = <constant-expression>
nt<EnumeratorAST> Parser::parseEnumerator() {
  mStartToken = &lex.peek();
  auto id = parseIdentifier();
  if (expect(TokenKind::TOKEN_EQ)) {
    return make_ast<EnumeratorAST>(std::move(id), parseConstantExpression());
  } else {
    return make_ast<EnumeratorAST>(std::move(id));
  }
}

///<enumerator-list> ::= <enumerator>
///                    | <enumerator-list> , <enumerator>
nt<EnumeratorListAST> Parser::parseEnumeratorList() {
  mStartToken = &lex.peek();
  nts<EnumeratorAST> list;
  while (lex.peek() == TokenKind::TOKEN_IDENTIFIER) {
    list.emplace_back(parseEnumerator());
    expect(TokenKind::TOKEN_COMMA);
  }
  return make_ast<EnumeratorListAST>(std::move(list));
}

///<enum-specifier> ::= enum <identifier> { <enumerator-list> }
///                   | enum { <enumerator-list> }
///                   | enum <identifier>

///          enum-specifier:
//                enum identifieropt { enumerator-list }
//                enum identifieropt { enumerator-list , }
//                enum identifier
nt<EnumSpecifierAST> Parser::parseEnumSpecifier() {
  mStartToken = &lex.peek();
  accept(TokenKind::TOKEN_ENUM);
  if (lex.peek() == TokenKind::TOKEN_IDENTIFIER) {
    if (table->isTypedef(lex.peek())) {
      throw parseError(std::string("cannot combine type \"") + lex.peek().getValue() + "\" with enum");
    }
    auto id = parseIdentifier();
    if (expect(TokenKind::TOKEN_LBRACE)) {
      auto p = make_ast<EnumSpecifierAST>(std::move(id), parseEnumeratorList());
      accept(TokenKind::TOKEN_RBRACE);
      return p;
    } else {
      return make_ast<EnumSpecifierAST>(std::move(id));
    }
  } else if (expect(TokenKind::TOKEN_LBRACE)) {
    auto p = make_ast<EnumSpecifierAST>(parseEnumeratorList());
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
  mStartToken = &lex.peek();
  TokenKind kind = lex.peek().getKind();
  switch (kind) {
    case TokenKind::TOKEN_IDENTIFIER:return make_ast<IdentifierPrimaryExpressionAST>(parseIdentifier());
    case TokenKind::TOKEN_INT_CONSTANT: {
      auto c = make_ast<IntegerConstantPrimaryExpressionAST>(make_ast<IntegerConstantAST>(lex.peek()));
      lex.consumeToken();
      return c;
    }
    case TokenKind::TOKEN_FLOAT_CONSTANT: {
      auto c = make_ast<FloatingConstantPrimaryExpressionAST>(make_ast<FloatingConstantAST>(lex.peek()));
      lex.consumeToken();
      return c;
    }
    case TokenKind::TOKEN_CHARLITERAL: {
      auto c = make_ast<CharacterConstantPrimaryExpressionAST>(make_ast<CharacterConstantAST>(lex.peek()));
      lex.consumeToken();
      return c;
    }
    case TokenKind::TOKEN_STRINGLITERAL: {
      auto c = make_ast<StringPrimaryExpressionAST>(make_ast<StringAST>(lex.peek()));
      lex.consumeToken();
      return c;
    }
    case TokenKind::TOKEN_LPAREN: {
      lex.consumeToken();
      auto c = make_ast<ExpressionPrimaryExpressionAST>(parseExpression());
      accept(TokenKind::TOKEN_RPAREN);
      return c;
    }
    default:throw parseError("expects identifier/string/char/int/float or a '(' for constant type");
  }
}

///<expression> ::= <assignment-expression>
///               | <expression> , <assignment-expression>
nt<ExpressionAST> Parser::parseExpression() {
  mStartToken = &lex.peek();
  nt<ExpressionAST> ast = std::make_unique<ExpressionAST>(nullptr, parseAssignmentExpression());
  while (expect(TokenKind::TOKEN_COMMA)) {
    ast = std::make_unique<ExpressionAST>(std::move(ast), parseAssignmentExpression());
  };
  return ast;
}

///<assignment-expression> ::= <conditional-expression>
///                          | <unary-expression> <assignment-operator> <assignment-expression>
nt<AssignmentExpressionAST> Parser::parseAssignmentExpression() {
  mStartToken = &lex.peek();
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
    case TokenKind::TOKEN_BAREQ: {
      Terminal<AssignmentOp> terminalOp(parseAssignmentOperator(), lex.peek());
      return make_ast<AssignmentExpressionAST>(std::move(LHS), terminalOp, parseAssignmentExpression());
    }
    default:return make_ast<AssignmentExpressionAST>(std::move(LHS));
  }
}

///<conditional-expression> ::= <logical-or-expression>
///                           | <logical-or-expression> ? <expression> : <conditional-expression>

nt<ConditionalExpressionAST> Parser::parseConditionalExpression() {
  mStartToken = &lex.peek();
  auto LHS = parseBinaryOperationAST();
  if (expect(TokenKind::TOKEN_QUES)) {
    auto exp = parseExpression();
    accept(TokenKind::TOKEN_COLON);
    auto conditional = parseConditionalExpression();
    return make_ast<ConditionalExpressionAST>(std::move(LHS), std::move(exp), std::move(conditional));
  } else {
    return make_ast<ConditionalExpressionAST>(std::move(LHS));
  }
}

// Traditional top down parsing will not work in parsing logical expressions.
// All logical expressions can be abstract as <term> <infixop> <term>
nt<IBinaryOperationAST> Parser::parseBinaryOperationAST(int calling_prec) {
  mStartToken = &lex.peek();
  nt<IBinaryOperationAST> term1 = make_ast<SimpleBinaryOperatorAST>(parseCastExpression());
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
      auto term2 = parseBinaryOperationAST(prec);
      if (op == InfixOp::BARBAR || op == InfixOp::AMPAMP) {
        term1 = make_ast<LogicalBinaryOperatorAST>(std::move(term1),
                                                   Terminal<InfixOp>(op, lex.peek()),
                                                   std::move(term2));
      } else {
        term1 = make_ast<BinaryOperatorAST>(std::move(term1),
                                            Terminal<InfixOp>(op, lex.peek()),
                                            std::move(term2));
      }
    }

  }
  return term1;
}

/// <declaration> ::=  {<declaration-specifier>}+ {<init-declarator>}* ;
nts<DeclarationAST> Parser::parseDeclarations() {
  mStartToken = &lex.peek();
  nts<DeclarationAST> declarations;
  while (true) {
    switch (lex.peek().getKind()) {
      case TokenKind::TOKEN_IDENTIFIER:if (!table->isTypedef(lex.peek())) break;
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
      case TokenKind::TOKEN_VOLATILE: {
        auto ds = parseDeclarationSpecifiers();
        auto p = make_ast<DeclarationAST>(std::move(ds),
                                          parseInitDeclarators(),
                                          *table);
        declarations.push_back(std::move(p));
        accept(TokenKind::TOKEN_SEMI);
        continue;
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
  mStartToken = &lex.peek();
  ts<StorageSpecifier> storage_specifiers;
  nts<TypeSpecifierAST> type_specifiers;
  nts<TypeQualifierAST> type_qualifiers;
  TokenCombinations tc;
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
      case TokenKind::TOKEN_INT:
      case TokenKind::TOKEN_LONG:
      case TokenKind::TOKEN_SHORT:
      case TokenKind::TOKEN_SIGNED:
      case TokenKind::TOKEN_UNSIGNED:
      case TokenKind::TOKEN_VOID:
      case TokenKind::TOKEN_STRUCT:
      case TokenKind::TOKEN_UNION:
      case TokenKind::TOKEN_ENUM:
      case TokenKind::TOKEN_FLOAT:
        if (!tc.put(token.getKind())) {
          throw parseError(std::string("cannot combine ") + (lex.peek().getValue()) + (" with ")
                               + type_specifiers.back().get()->mLeftMost->getValue(), lex.peek());
        }
        type_specifiers.push_back(parseTypeSpecifier());
        continue;
      case TokenKind::TOKEN_CONST:
      case TokenKind::TOKEN_VOLATILE:type_qualifiers.push_back(parseTypeQualifier());
        continue;
      default:break;
    }
    break;
  }
  return make_ast<DeclarationSpecifiersAST>(std::move(storage_specifiers),
                                            make_ast<TypeSpecifiersAST>(tc.getType(), std::move(type_specifiers)),
                                            std::move(type_qualifiers));
}

///<init-declarator> ::= <declarator>
///                    | <declarator> = <initializer>

/// init-declarator-list:
///                  init-declarator
///                  init-declarator-list , init-declarator
InitDeclarators Parser::parseInitDeclarators() {
  mStartToken = &lex.peek();
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
        return init_declarators;
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
  mStartToken = &lex.peek();
  nt<PointerAST> pointer = nullptr;
  if (lex.peek() == TokenKind::TOKEN_STAR) {
    pointer = parsePointer();
  }
  nt<DirectDeclaratorAST> dd = parseDirectDeclarator();
  return make_ast<DeclaratorAST>(std::move(pointer), std::move(dd));
}

///<pointer> ::= * {<type-qualifier>}* {<pointer>}?
nt<PointerAST> Parser::parsePointer() {
  mStartToken = &lex.peek();
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
  return make_ast<PointerAST>(std::move(type_qualifiers), std::move(pointer));
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
  mStartToken = &lex.peek();
  nt<DirectDeclaratorAST> root;
  if (lex.peek() == TokenKind::TOKEN_IDENTIFIER) {
    root = make_ast<SimpleDirectDeclaratorAST>(parseIdentifier());
  } else if (lex.peek() == TokenKind::TOKEN_LPAREN) {
    if (lex.peek(1) == TokenKind::TOKEN_LPAREN
        || lex.peek(1) == TokenKind::TOKEN_STAR
        || lex.peek(1) == TokenKind::TOKEN_IDENTIFIER) {
      lex.consumeToken();
      auto declarator = parseDeclarator();
      accept(TokenKind::TOKEN_RPAREN);
      root = make_ast<ParenthesedDirectDeclaratorAST>(std::move(declarator));
    } else {
      root = make_ast<SimpleDirectDeclaratorAST>(nullptr);
    }
  } else {
    root = make_ast<SimpleDirectDeclaratorAST>(nullptr);
  }
  while (lex.peek() == TokenKind::TOKEN_LPAREN
      || lex.peek() == TokenKind::TOKEN_LBRACKET) {
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
        case TokenKind::TOKEN_TILDE:root = make_ast<ArrayDeclaratorAST>(std::move(root), parseConstantExpression());
          break;
        default:root = make_ast<ArrayDeclaratorAST>(std::move(root), nullptr);
          break;
      }
      accept(TokenKind::TOKEN_RBRACKET);
    } else if (expect(TokenKind::TOKEN_LPAREN)) {
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
        case TokenKind::TOKEN_VOLATILE:root = make_ast<FunctionDeclaratorAST>(std::move(root), parseParameterLists());
          break;
        case TokenKind::TOKEN_IDENTIFIER:
//          if (table->isTypedef(lex.peek())) {
//            term2s.emplace_back(DirectDeclaratorAST::Term2::PARA_LIST, parseParameterTypeList());
//          } else {
//            do {
//              term2s.emplace_back(DirectDeclaratorAST::Term2::ID, parseIdentifier());
//            } while (expect(TokenKind::TOKEN_COMMA));
//          }
//          break;
          //TODO implement identifier list
          throw parseError("do not support identifier list function declaration yet");
        default:root = make_ast<FunctionDeclaratorAST>(std::move(root), nullptr);
      }
      accept(TokenKind::TOKEN_RPAREN);
    } else {
      return make_ast<SimpleDirectDeclaratorAST>(nullptr);
    }
  }
  return root;
}

///<parameter-list> ::= <parameter-declaration>
///                   | <parameter-list> , <parameter-declaration>
nt<ParameterListAST> Parser::parseParameterLists() {
  mStartToken = &lex.peek();
  auto *temp_table = symbolTables.createTable(ScopeKind::FUNCTION_PROTOTYPE);
  SymbolScope s(table, temp_table);
  nts<ParameterDeclarationAST> decls;
  bool hasMutiple = false;
  do {
    const Token &token = lex.peek();
    if (token == TokenKind::TOKEN_ELLIPSIS) {
      hasMutiple = true;
      lex.consumeToken();
      break;
    } else {
      decls.emplace_back(parseParameterDeclaration());
    }
  } while (expect(TokenKind::TOKEN_COMMA));
  return make_ast<ParameterListAST>(std::move(decls), hasMutiple, *table);
}

///<parameter-declaration> ::= {<declaration-specifier>}+ <declarator>
///                          | {<declaration-specifier>}+ <abstract-declarator>
///                          | {<declaration-specifier>}+
nt<ParameterDeclarationAST> Parser::parseParameterDeclaration() {
  mStartToken = &lex.peek();
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
  return make_ast<ParameterDeclarationAST>(std::move(ds), std::move(declarator));
}

///<storage-class-specifier> ::= auto
///                            | register
///                            | static
///                            | extern
///                            | typedef
StorageSpecifier Parser::parseStorageClassSpecifier() {
  mStartToken = &lex.peek();
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
  mStartToken = &lex.peek();
  ProtoTypeSpecifierOp specifier;
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_VOID:specifier = (ProtoTypeSpecifierOp::kVOID);
      goto handle_proto_type;
    case TokenKind::TOKEN_CHAR:specifier = (ProtoTypeSpecifierOp::kCHAR);
      goto handle_proto_type;
    case TokenKind::TOKEN_SHORT:specifier = (ProtoTypeSpecifierOp::kSHORT);
      goto handle_proto_type;
    case TokenKind::TOKEN_INT:specifier = (ProtoTypeSpecifierOp::kINT);
      goto handle_proto_type;
    case TokenKind::TOKEN_LONG:specifier = (ProtoTypeSpecifierOp::kLONG);
      goto handle_proto_type;
    case TokenKind::TOKEN_FLOAT:specifier = (ProtoTypeSpecifierOp::kFLOAT);
      goto handle_proto_type;
    case TokenKind::TOKEN_DOUBLE:specifier = (ProtoTypeSpecifierOp::kDOUBLE);
      goto handle_proto_type;
    case TokenKind::TOKEN_SIGNED:specifier = (ProtoTypeSpecifierOp::kSIGNED);
      goto handle_proto_type;
    case TokenKind::TOKEN_UNSIGNED:specifier = (ProtoTypeSpecifierOp::kUNSIGNED);
      goto handle_proto_type;
    case TokenKind::TOKEN_STRUCT:
    case TokenKind::TOKEN_UNION:return make_ast<TypeSpecifierAST>(parseStructOrUnionSpecifier());
    case TokenKind::TOKEN_ENUM:return make_ast<TypeSpecifierAST>(parseEnumSpecifier());
    case TokenKind::TOKEN_IDENTIFIER:
      if (table->isTypedef(lex.peek())) {
        return make_ast<TypeSpecifierAST>(parseTypedefName());
      }
    default:throw parseError("expected type specifier");
  }
  handle_proto_type:
  auto ast = make_ast<TypeSpecifierAST>(Terminal<ProtoTypeSpecifierOp>(specifier, lex.peek()));
  lex.consumeToken();
  return std::move(ast);

}

///<struct-or-union-specifier> ::= <struct-or-union> <identifier> { {<struct-declaration>}+ }
///                              | <struct-or-union> { {<struct-declaration>}+ }
///                              | <struct-or-union> <identifier>
nt<StructOrUnionSpecifierAST> Parser::parseStructOrUnionSpecifier() {
  mStartToken = &lex.peek();
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
    if (table->isTypedef(lex.peek())) {
      throw parseError(std::string("cannot combine type \"") + lex.peek().getValue() + "\" with struct or union");
    }
    id = parseIdentifier();
  }
  nts<StructDeclarationAST> declarations;
  if (!expect(TokenKind::TOKEN_LBRACE)) {
    return make_ast<StructOrUnionSpecifierAST>(type, std::move(id), std::move(declarations));
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
  return make_ast<StructOrUnionSpecifierAST>(type, std::move(id), std::move(declarations));
}

///<struct-declaration> ::= {<specifier-qualifier>}* <struct-declarator-list> ;
nt<StructDeclarationAST> Parser::parseStructDeclaration() {
  mStartToken = &lex.peek();
  nt<SpecifierQualifierAST> qualifiers = parseSpecifierQualifiers();
  auto ast = make_ast<StructDeclarationAST>(std::move(qualifiers), parseStructDeclaratorList());
  accept(TokenKind::TOKEN_SEMI);
  return ast;
}

///<struct-declarator-list> ::= <struct-declarator>
///                           | <struct-declarator-list> , <struct-declarator>
nt<StructDeclaratorListAST> Parser::parseStructDeclaratorList() {
  mStartToken = &lex.peek();
  nts<StructDeclaratorAST> declarators;
  do {
    declarators.push_back(parseStructDeclarator());
  } while (expect(TokenKind::TOKEN_COMMA));
  return make_ast<StructDeclaratorListAST>(std::move(declarators));
}

///<struct-declarator> ::= <declarator>
///                      | <declarator> : <constant-expression>
///                      | : <constant-expression>
nt<StructDeclaratorAST> Parser::parseStructDeclarator() {
  mStartToken = &lex.peek();
  if (expect(TokenKind::TOKEN_COLON)) {
    return make_ast<StructDeclaratorAST>(parseConstantExpression());
  } else {
    auto declarator = parseDeclarator();
    if (expect(TokenKind::TOKEN_COLON)) {
      return make_ast<StructDeclaratorAST>(std::move(declarator), parseConstantExpression());
    } else {
      return make_ast<StructDeclaratorAST>(std::move(declarator));
    }
  }
}

///<initializer> ::= <assignment-expression>
///                | { <initializer-list> }
///                | { <initializer-list> , }
nt<InitializerAST> Parser::parseInitializer() {
  mStartToken = &lex.peek();
  if (expect(TokenKind::TOKEN_LBRACE)) {
    auto exp = parseInitializerList();
    expect(TokenKind::TOKEN_COMMA);
    accept(TokenKind::TOKEN_RBRACE);
    return make_ast<InitializerAST>(std::move(exp));
  } else {
    return make_ast<InitializerAST>(parseAssignmentExpression());
  }
}

///<initializer-list> ::= <initializer>
///                     | <initializer-list> , <initializer>
nt<InitializerListAST> Parser::parseInitializerList() {
  mStartToken = &lex.peek();
  nts<InitializerAST> list;
  do {
    list.push_back(parseInitializer());
  } while (expect(TokenKind::TOKEN_COMMA));
  return make_ast<InitializerListAST>(std::move(list));
}

///<function-definition> ::= {<declaration-specifier>}* <declarator> {<declaration>}* <compound-statement>
// we use {<declaration-specifier>}+ here
//nt<FunctionDefinitionAST> Parser::parseFunctionDefinition() {
//  mStartToken = &lex.peek();
//  auto specifiers = parseDeclarationSpecifiers();
//  auto declarator = parseDeclarator();
//  auto declarations = parseDeclarations();
//  auto compound = parseCompoundStatement();
//  return make_ast<FunctionDefinitionAST>(std::move(specifiers),
//                                         std::move(declarator),
//                                         std::move(declarations),
//                                         std::move(compound));
//}

///<compound-statement> ::= { {<declaration>}* {<statement>}* }
///          compound-statement:
//                { block-item-listopt }
//          block-item-list:
//                  block-item
//                  block-item-list block-item
//          block-item:
//                  declaration
//                  statement
nt<CompoundStatementAST> Parser::parseCompoundStatement() {
  mStartToken = &lex.peek();
  SymbolScope s(table, symbolTables.createTable(ScopeKind::BLOCK));
  accept(TokenKind::TOKEN_LBRACE);
//  auto declarations = parseDeclarations();
//
//  nts<StatementAST> statements{};
//  while (lex.peek() != TokenKind::TOKEN_RBRACE) {
//    statements.push_back(parseStatement());
//  }
  nts<AST> asts;
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
      case TokenKind::TOKEN_VOLATILE: {
        auto declarations = parseDeclarations();
        asts.insert(asts.end(),
                    std::make_move_iterator(declarations.begin()),
                    std::make_move_iterator(declarations.end()));
        continue;
      }
      case TokenKind::TOKEN_BANG:
      case TokenKind::TOKEN_AMP:
      case TokenKind::TOKEN_LPAREN:
      case TokenKind::TOKEN_STAR:
      case TokenKind::TOKEN_PLUS:
      case TokenKind::TOKEN_PLUSPLUS:
      case TokenKind::TOKEN_SUB:
      case TokenKind::TOKEN_SUBSUB:
      case TokenKind::TOKEN_SEMI:
      case TokenKind::TOKEN_BREAK:
      case TokenKind::TOKEN_CASE:
      case TokenKind::TOKEN_CHARLITERAL:
      case TokenKind::TOKEN_FLOAT_CONSTANT:
      case TokenKind::TOKEN_INT_CONSTANT:
      case TokenKind::TOKEN_CONTINUE:
      case TokenKind::TOKEN_DEFAULT:
      case TokenKind::TOKEN_DO:
      case TokenKind::TOKEN_FOR:
      case TokenKind::TOKEN_GOTO:
      case TokenKind::TOKEN_IF:
      case TokenKind::TOKEN_RETURN:
      case TokenKind::TOKEN_SIZEOF:
      case TokenKind::TOKEN_STRINGLITERAL:
      case TokenKind::TOKEN_SWITCH:
      case TokenKind::TOKEN_WHILE:
      case TokenKind::TOKEN_LBRACE:
      case TokenKind::TOKEN_TILDE:asts.push_back(parseStatement());
        continue;
      case TokenKind::TOKEN_IDENTIFIER:
        if (table->isTypedef(lex.peek())) {
          auto declarations = parseDeclarations();
          asts.insert(asts.end(),
                      std::make_move_iterator(declarations.begin()),
                      std::make_move_iterator(declarations.end()));
        } else {
          asts.push_back(parseStatement());
        }
        continue;
      default:break;
    }
    break;
  }
  accept(TokenKind::TOKEN_RBRACE);
  auto *tagTable = symbolTables.createTable(ScopeKind::BLOCK);
  return make_ast<CompoundStatementAST>(std::move(asts), *table, *tagTable);
}

///<statement> ::= <labeled-statement>
///              | <expression-statement>
///              | <compound-statement>
///              | <selection-statement>
///              | <iteration-statement>
///              | <jump-statement>
nt<StatementAST> Parser::parseStatement() {
  mStartToken = &lex.peek();
  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_CASE:
    case TokenKind::TOKEN_DEFAULT:return parseLabeledStatement();
    case TokenKind::TOKEN_IDENTIFIER:
      if (lex.peek(1).getKind() == TokenKind::TOKEN_COLON) {
        return parseLabeledStatement();
      } else {
        return parseExpressionStatement();
      }
    case TokenKind::TOKEN_LBRACE:return parseCompoundStatement();
    case TokenKind::TOKEN_IF:
    case TokenKind::TOKEN_SWITCH:return parseSelectionStatement();
    case TokenKind::TOKEN_DO:
    case TokenKind::TOKEN_FOR:
    case TokenKind::TOKEN_WHILE:return parseIterationStatement();
    case TokenKind::TOKEN_BREAK:
    case TokenKind::TOKEN_CONTINUE:
    case TokenKind::TOKEN_GOTO:
    case TokenKind::TOKEN_RETURN:return parseJumpStatement();
    default:return parseExpressionStatement();
  }
}

///<labeled-statement> ::= <identifier> : <statement>
///                      | case <constant-expression> : <statement>
///                      | default : <statement>
nt<LabeledStatementAST> Parser::parseLabeledStatement() {
  mStartToken = &lex.peek();
  if (lex.peek() == TokenKind::TOKEN_IDENTIFIER) {
    auto id = parseIdentifier();
    accept(TokenKind::TOKEN_COLON);
    return make_ast<IdentifierLabeledStatementAST>(std::move(id), parseStatement());
  } else if (expect(TokenKind::TOKEN_CASE)) {
    auto exp = parseConstantExpression();
    accept(TokenKind::TOKEN_COLON);
    return make_ast<CaseLabeledStatementAST>(std::move(exp), parseStatement());
  } else if (expect(TokenKind::TOKEN_DEFAULT)) {
    accept(TokenKind::TOKEN_COLON);
    return make_ast<DefaultLabeledStatementAST>(parseStatement());
  } else {
    throw parseError(R"(labeled statement expects "case", "default" or an identifer)");
  }
}

///<expression-statement> ::= {<expression>}? ;
nt<ExpressionStatementAST> Parser::parseExpressionStatement() {
  mStartToken = &lex.peek();
  nt<ExpressionAST> exp = nullptr;
  if (!expect(TokenKind::TOKEN_SEMI)) {
    exp = parseExpression();
    accept(TokenKind::TOKEN_SEMI);
  }
  return make_ast<ExpressionStatementAST>(std::move(exp));
}

///<selection-statement> ::= if ( <expression> ) <statement>
///                        | if ( <expression> ) <statement> else <statement>
///                        | switch ( <expression> ) <statement>
nt<SelectionStatementAST> Parser::parseSelectionStatement() {
  mStartToken = &lex.peek();
  if (expect(TokenKind::TOKEN_IF)) {
    accept(TokenKind::TOKEN_LPAREN);
    auto condition = parseExpression();
    accept(TokenKind::TOKEN_RPAREN);
    auto if_statement = parseStatement();
    if (expect(TokenKind::TOKEN_ELSE)) {
      return make_ast<IfSelectionStatementAST>(std::move(condition), std::move(if_statement), parseStatement());
    } else {
      return make_ast<IfSelectionStatementAST>(std::move(condition), std::move(if_statement), nullptr);
    }
  } else {
    accept(TokenKind::TOKEN_SWITCH);
    accept(TokenKind::TOKEN_LPAREN);
    auto exp = parseExpression();
    accept(TokenKind::TOKEN_RPAREN);
    return make_ast<SwitchSelectionStatementAST>(std::move(exp), parseStatement());
  }
}

///<iteration-statement> ::= while ( <expression> ) <statement>
///                        | do <statement> while ( <expression> ) ;
///                        | for ( {<expression>}? ; {<expression>}? ; {<expression>}? ) <statement>
nt<IterationStatementAST> Parser::parseIterationStatement() {
  mStartToken = &lex.peek();
  if (expect(TokenKind::TOKEN_WHILE)) {
    accept(TokenKind::TOKEN_LPAREN);
    auto exp = parseExpression();
    accept(TokenKind::TOKEN_RPAREN);
    return make_ast<WhileIterationStatementAST>(std::move(exp), parseStatement());
  } else if (expect(TokenKind::TOKEN_DO)) {
    auto statement = parseStatement();
    accept(TokenKind::TOKEN_WHILE);
    accept(TokenKind::TOKEN_LPAREN);
    auto exp = parseExpression();
    accept(TokenKind::TOKEN_RPAREN);
    accept(TokenKind::TOKEN_SEMI);
    return make_ast<DoIterationStatementAST>(std::move(statement), std::move(exp));
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
    return make_ast<ForIterationStatementAST>(std::move(term1), std::move(term2), std::move(term3), parseStatement());
  }
}

///<jump-statement> ::= goto <identifier> ;
///                   | continue ;
///                   | break ;
///                   | return {<expression>}? ;
nt<JumpStatementAST> Parser::parseJumpStatement() {
  mStartToken = &lex.peek();
  if (expect(TokenKind::TOKEN_GOTO)) {
    auto id = parseIdentifier();
    accept(TokenKind::TOKEN_SEMI);
    return make_ast<GotoJumpStatementAST>(std::move(id));
  } else if (expect(TokenKind::TOKEN_RETURN)) {
    nt<ExpressionAST> exp = nullptr;
    if (!expect(TokenKind::TOKEN_SEMI)) {
      exp = parseExpression();
      accept(TokenKind::TOKEN_SEMI);
    }
    return make_ast<ReturnJumpStatementAST>(std::move(exp));
  } else {
    if (expect(TokenKind::TOKEN_CONTINUE)) {
      return make_ast<ContinueJumpStatementAST>();
    } else {
      accept(TokenKind::TOKEN_BREAK);
      return make_ast<BreakJumpStatementAST>();
    }
  }
}

///<constant-expression> ::= <conditional-expression>
nt<ConstantExpressionAST> Parser::parseConstantExpression() {
  mStartToken = &lex.peek();
  return make_ast<ConstantExpressionAST>(parseConditionalExpression());
}

///<cast-expression> ::= <unary-expression>
///                    | ( <type-name> ) <cast-expression>
nt<CastExpressionAST> Parser::parseCastExpression() {
  mStartToken = &lex.peek();
  if (lex.peek() == TokenKind::TOKEN_LPAREN) {
    switch (lex.peek(1).getKind()) {
      case TokenKind::TOKEN_IDENTIFIER:if (!table->isTypedef(lex.peek(1))) break;
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
      case TokenKind::TOKEN_VOLATILE: {
        accept(TokenKind::TOKEN_LPAREN);
        auto type_name = parseTypeName();
        accept(TokenKind::TOKEN_RPAREN);
        return make_ast<RealCastExpressionAST>(std::move(type_name), parseCastExpression());
      }
      default:break;
    }
  }
  return make_ast<SimpleCastExpressionAST>(parseUnaryExpression());
}

///<unary-expression> ::= <postfix-expression>
///                     | ++ <unary-expression>
///                     | -- <unary-expression>
///                     | <unary-operator> <cast-expression>
///                     | sizeof <unary-expression>
///                     | sizeof ( <type-name> )
nt<UnaryExpressionAST> Parser::parseUnaryExpression() {
  mStartToken = &lex.peek();
  if (expect(TokenKind::TOKEN_PLUSPLUS)) {
    return make_ast<PrefixIncrementExpressionAST>(parseUnaryExpression());
  } else if (expect(TokenKind::TOKEN_SUBSUB)) {
    return make_ast<PrefixDecrementExpressionAST>(parseUnaryExpression());
  } else if (expect(TokenKind::TOKEN_SIZEOF)) {
    if (expect(TokenKind::TOKEN_LPAREN)) {
      nt<SizeofUnaryExpressionAST> ast;
      switch (lex.peek().getKind()) {
        case TokenKind::TOKEN_CHAR:
        case TokenKind::TOKEN_DOUBLE:
        case TokenKind::TOKEN_INT:
        case TokenKind::TOKEN_LONG:
        case TokenKind::TOKEN_SHORT:
        case TokenKind::TOKEN_SIGNED:
        case TokenKind::TOKEN_UNSIGNED:
        case TokenKind::TOKEN_VOID:
        case TokenKind::TOKEN_STRUCT:
        case TokenKind::TOKEN_UNION:
        case TokenKind::TOKEN_ENUM:
        case TokenKind::TOKEN_FLOAT:
        case TokenKind::TOKEN_CONST:
        case TokenKind::TOKEN_VOLATILE:ast = make_ast<SizeofUnaryExpressionAST>(parseTypeName());
          break;
        case TokenKind::TOKEN_IDENTIFIER:
          if (table->isTypedef(lex.peek())) {
            ast = make_ast<SizeofUnaryExpressionAST>(parseTypeName());
          }
        default:ast = make_ast<SizeofUnaryExpressionAST>(parseUnaryExpression());
      }
      accept(TokenKind::TOKEN_RPAREN);
      return ast;
    } else {
      return make_ast<SizeofUnaryExpressionAST>(parseUnaryExpression());
    }
  } else if (lex.peek() == TokenKind::TOKEN_BANG
      || lex.peek() == TokenKind::TOKEN_AMP
      || lex.peek() == TokenKind::TOKEN_STAR
      || lex.peek() == TokenKind::TOKEN_PLUS
      || lex.peek() == TokenKind::TOKEN_SUB
      || lex.peek() == TokenKind::TOKEN_TILDE) {
    auto uo = parseUnaryOperator();
    return make_ast<UnaryOperatorExpressionAST>(uo, parseCastExpression());
  } else {
    return make_ast<SimpleUnaryExpressionAST>(parsePostfixExpression());
  }
}

///<postfix-expression> ::= <primary-expression>
///                       | <postfix-expression> [ <expression> ]
///                       | <postfix-expression> ( {<assignment-expression>}* )
///                       | <postfix-expression> . <identifier>
///                       | <postfix-expression> -> <identifier>
///                       | <postfix-expression> ++
///                       | <postfix-expression> --
nt<PostfixExpressionAST> Parser::parsePostfixExpression() {
  mStartToken = &lex.peek();
  nt<PostfixExpressionAST> p = make_ast<SimplePostfixExpressionAST>(parsePrimaryExpression());
  while (true) {
    switch (lex.peek().getKind()) {
      case TokenKind::TOKEN_LBRACKET:lex.consumeToken();
        p = make_ast<ArrayPostfixExpressionAST>(std::move(p), parseExpression());
        accept(TokenKind::TOKEN_RBRACKET);
        continue;
      case TokenKind::TOKEN_LPAREN: {
        lex.consumeToken();
        nts<AssignmentExpressionAST> arguments;
        if (lex.peek().getKind() != TokenKind::TOKEN_RPAREN) {
          do {
            arguments.emplace_back(parseAssignmentExpression());
          } while (expect(TokenKind::TOKEN_COMMA));
        }
        p = make_ast<FunctionPostfixExpressionAST>(std::move(p),
                                                   make_ast<ArgumentExpressionList>(std::move(arguments)));
        accept(TokenKind::TOKEN_RPAREN);
        continue;
      }
      case TokenKind::TOKEN_DOT:lex.consumeToken();
        p = make_ast<MemberPostfixExpressionAST>(std::move(p), parseIdentifier());
        continue;
      case TokenKind::TOKEN_ARROW:lex.consumeToken();
        p = make_ast<PointerMemberPostfixExpressionAST>(std::move(p), parseIdentifier());
        continue;
      case TokenKind::TOKEN_PLUSPLUS:lex.consumeToken();
        p = make_ast<IncrementPostfixExpression>(std::move(p));
        continue;
      case TokenKind::TOKEN_SUBSUB:lex.consumeToken();
        p = make_ast<DecrementPostfixExpression>(std::move(p));
        continue;
      default:break;
    }
    break;
  }
  return std::move(p);
}

///<type-name> ::= {<specifier-qualifier>}+ {<abstract-declarator>}?
nt<TypeNameAST> Parser::parseTypeName() {
  mStartToken = &lex.peek();
  nt<SpecifierQualifierAST> qualifiers = parseSpecifierQualifiers();

  switch (lex.peek().getKind()) {
    case TokenKind::TOKEN_LPAREN:
    case TokenKind::TOKEN_STAR:
    case TokenKind::TOKEN_LBRACKET:
      return make_ast<TypeNameAST>(std::move(qualifiers),
                                   parseDeclarator());
    default:return make_ast<TypeNameAST>(std::move(qualifiers), nullptr);
  }
}
ParserException Parser::parseError(const std::string &msg, const Token &token) {
  return ParserException(msg, token);
}
///<specifier-qualifier> ::= <type-specifier>
///                        | <type-qualifier>
nt<SpecifierQualifierAST> Parser::parseSpecifierQualifiers() {
  mStartToken = &lex.peek();
  nts<TypeSpecifierAST> type_specifiers;
  nts<TypeQualifierAST> type_qualifiers;
  TokenCombinations tc;
  while (true) {
    switch (lex.peek().getKind()) {
      case TokenKind::TOKEN_IDENTIFIER:
        if (!table->isTypedef(lex.peek())) {
          break;
        }
      case TokenKind::TOKEN_CHAR:
      case TokenKind::TOKEN_DOUBLE:
      case TokenKind::TOKEN_INT:
      case TokenKind::TOKEN_LONG:
      case TokenKind::TOKEN_SHORT:
      case TokenKind::TOKEN_SIGNED:
      case TokenKind::TOKEN_UNSIGNED:
      case TokenKind::TOKEN_VOID:
      case TokenKind::TOKEN_STRUCT:
      case TokenKind::TOKEN_UNION:
      case TokenKind::TOKEN_ENUM:
      case TokenKind::TOKEN_FLOAT:
        if (!tc.put(lex.peek().getKind())) {
          throw parseError(std::string("cannot combine ") + (lex.peek().getValue()) + (" with ")
                               + type_specifiers.back().get()->mLeftMost->getValue(), lex.peek());
        }
        type_specifiers.push_back(parseTypeSpecifier());
        continue;
      case TokenKind::TOKEN_CONST:
      case TokenKind::TOKEN_VOLATILE:type_qualifiers.push_back(parseTypeQualifier());
        continue;
      default:break;
    }
    break;
  }
  return make_ast<SpecifierQualifierAST>(make_ast<TypeSpecifiersAST>(tc.getType(), std::move(type_specifiers)),
                                         std::move(type_qualifiers));
}

ParserException::ParserException(std::string
                                 error,
                                 const Token &token) :
    token(token), error(std::move(error)) {
  this->error.append("\n").append(token.getTokenInLine());
}
const char *ParserException::what() const noexcept {
  return error.c_str();
}
