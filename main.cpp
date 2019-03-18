#include <iostream>
#include <fstream>
#include <lex/lex.h>
#include "src/tools/first_set_generator.h"

const char *enumToString(mycc::TokenKind kind) {
  using namespace mycc;
  switch (kind) {
    case TokenKind::TOKEN_AUTO: return "AUTO";
    case TokenKind::TOKEN_REGISTER: return "REGISTER";
    case TokenKind::TOKEN_STATIC: return "STATIC";
    case TokenKind::TOKEN_EXTERN: return "EXTERN";
    case TokenKind::TOKEN_TYPEDEF: return "TYPEDEF";
    case TokenKind::TOKEN_VOID: return "VOID";
    case TokenKind::TOKEN_CHAR: return "CHAR";
    case TokenKind::TOKEN_SHORT: return "SHORT";
    case TokenKind::TOKEN_INT: return "INT";
    case TokenKind::TOKEN_LONG: return "LONG";
    case TokenKind::TOKEN_FLOAT: return "FLOAT";
    case TokenKind::TOKEN_DOUBLE: return "DOUBLE";
    case TokenKind::TOKEN_SIGNED: return "SIGNED";
    case TokenKind::TOKEN_UNSIGNED: return "UNSIGNED";
    case TokenKind::TOKEN_STRUCT: return "STRUCT";
    case TokenKind::TOKEN_UNION: return "UNION";
    case TokenKind::TOKEN_CONST: return "CONST";
    case TokenKind::TOKEN_VOLATILE: return "VOLATILE";
    case TokenKind::TOKEN_SIZEOF: return "SIZEOF";
    case TokenKind::TOKEN_ENUM: return "ENUM";
    case TokenKind::TOKEN_CASE: return "CASE";
    case TokenKind::TOKEN_DEFAULT: return "DEFAULT";
    case TokenKind::TOKEN_IF: return "IF";
    case TokenKind::TOKEN_SWITCH: return "SWITCH";
    case TokenKind::TOKEN_WHILE: return "WHILE";
    case TokenKind::TOKEN_DO: return "DO";
    case TokenKind::TOKEN_FOR: return "FOR";
    case TokenKind::TOKEN_GOTO: return "GOTO";
    case TokenKind::TOKEN_CONTINUE: return "CONTINUE";
    case TokenKind::TOKEN_BREAK: return "BREAK";
    case TokenKind::TOKEN_RETURN: return "RETURN";
    case TokenKind::TOKEN_ARROW: return "ARROW";
    case TokenKind::TOKEN_LPAREN: return "LPAREN";
    case TokenKind::TOKEN_RPAREN: return "RPAREN";
    case TokenKind::TOKEN_LBRACE: return "LBRACE";
    case TokenKind::TOKEN_RBRACE: return "RBRACE";
    case TokenKind::TOKEN_LBRACKET: return "LBRACKET";
    case TokenKind::TOKEN_RBRACKET: return "RBRACKET";
    case TokenKind::TOKEN_SEMI: return "SEMI";
    case TokenKind::TOKEN_COMMA: return "COMMA";
    case TokenKind::TOKEN_DOT: return "DOT";
    case TokenKind::TOKEN_ELLIPSIS: return "ELLIPSIS";
    case TokenKind::TOKEN_EQ: return "EQ";
    case TokenKind::TOKEN_GT: return "GT";
    case TokenKind::TOKEN_LT: return "LT";
    case TokenKind::TOKEN_BANG: return "BANG";
    case TokenKind::TOKEN_TILDE: return "TILDE";
    case TokenKind::TOKEN_QUES: return "QUES";
    case TokenKind::TOKEN_COLON: return "COLON";
    case TokenKind::TOKEN_COLONCOLON: return "COLONCOLON";
    case TokenKind::TOKEN_EQEQ: return "EQEQ";
    case TokenKind::TOKEN_LTEQ: return "LTEQ";
    case TokenKind::TOKEN_GTEQ: return "GTEQ";
    case TokenKind::TOKEN_BANGEQ: return "BANGEQ";
    case TokenKind::TOKEN_AMPAMP: return "AMPAMP";
    case TokenKind::TOKEN_BARBAR: return "BARBAR";
    case TokenKind::TOKEN_PLUSPLUS: return "PLUSPLUS";
    case TokenKind::TOKEN_SUBSUB: return "SUBSUB";
    case TokenKind::TOKEN_PLUS: return "PLUS";
    case TokenKind::TOKEN_SUB: return "SUB";
    case TokenKind::TOKEN_STAR: return "STAR";
    case TokenKind::TOKEN_SLASH: return "SLASH";
    case TokenKind::TOKEN_AMP: return "AMP";
    case TokenKind::TOKEN_BAR: return "BAR";
    case TokenKind::TOKEN_CARET: return "CARET";
    case TokenKind::TOKEN_PERCENT: return "PERCENT";
    case TokenKind::TOKEN_LTLT: return "LTLT";
    case TokenKind::TOKEN_GTGT: return "GTGT";
    case TokenKind::TOKEN_PLUSEQ: return "PLUSEQ";
    case TokenKind::TOKEN_SUBEQ: return "SUBEQ";
    case TokenKind::TOKEN_STAREQ: return "STAREQ";
    case TokenKind::TOKEN_SLASHEQ: return "SLASHEQ";
    case TokenKind::TOKEN_AMPEQ: return "AMPEQ";
    case TokenKind::TOKEN_BAREQ: return "BAREQ";
    case TokenKind::TOKEN_CARETEQ: return "CARETEQ";
    case TokenKind::TOKEN_PERCENTEQ: return "PERCENTEQ";
    case TokenKind::TOKEN_LTLTEQ: return "LTLTEQ";
    case TokenKind::TOKEN_GTGTEQ: return "GTGTEQ";
    case TokenKind::TOKEN_IDENTIFIER: return "IDENTIFIER";
    case TokenKind::TOKEN_NUMERIC_CONSTANT: return "NUMERIC_CONSTANT";
    case TokenKind::TOKEN_CHARLITERAL: return "CHARLITERAL";
    case TokenKind::TOKEN_STRINGLITERAL: return "STRINGLITERAL";
    case TokenKind::TOKEN_EOF: return "EOF";
    case TokenKind::TOKEN_UNKNOWN: return "UNKNOWN";
    default: return "unknown default";
  }
}

int main() {
//  std::ifstream testFile;
//  testFile.open("test.c");
//  mycc::Lex lex(testFile);
//  while (lex.peek()->getKind() != mycc::TokenKind::TOKEN_EOF) {
//    mycc::Token* token = lex.get();
//    std::cout << enumToString(token->getKind()) << "("<< token->getValue() << ")" << std::endl;
//  }

  std::ifstream BNF;
  BNF.open("BNF");
  std::unordered_map<std::string, mycc::NoneTerminalId> none_terminal_map;
  mycc::Productions productions = mycc::FirstSetGenerator::ToProductions(BNF, none_terminal_map);
//  for(auto p : productions) {
//    std::cout << p.first << " -> ";
//    for (auto symbol : p.second) {
//      if (symbol.isNone_terminal()) {
//        std::cout << symbol.getType();
//      } else {
//        std::cout << symbol.getName();
//      }
//      std::cout << ' ';
//    }
//    std::cout << std::endl;
//  }
  std::vector<std::set<std::string>> first_sets = mycc::FirstSetGenerator::getFirstSets(productions, none_terminal_map.size());
  for (auto k : none_terminal_map) {
    std::cout << k.first << " -> ";
    auto set = first_sets[k.second];
    for(const auto& string : set) {
      std::cout << string << " ";
    }
    std::cout << std::endl;
  }
}