#ifndef MYCCPILER_TOKEN_H
#define MYCCPILER_TOKEN_H
#include <string>
#include <unordered_map>
#include <fstream>
namespace mycc {
enum class TokenKind {
  TOKEN_AUTO,
  TOKEN_REGISTER,
  TOKEN_STATIC,
  TOKEN_EXTERN,
  TOKEN_TYPEDEF,
  TOKEN_VOID,
  TOKEN_CHAR,
  TOKEN_SHORT,
  TOKEN_INT,
  TOKEN_LONG,
  TOKEN_FLOAT,
  TOKEN_DOUBLE,
  TOKEN_SIGNED,
  TOKEN_UNSIGNED,
  TOKEN_STRUCT,
  TOKEN_UNION,
  TOKEN_CONST,
  TOKEN_VOLATILE,
  TOKEN_SIZEOF,
  TOKEN_ENUM,
  TOKEN_CASE,
  TOKEN_DEFAULT,
  TOKEN_IF,
  TOKEN_ELSE,
  TOKEN_SWITCH,
  TOKEN_WHILE,
  TOKEN_DO,
  TOKEN_FOR,
  TOKEN_GOTO,
  TOKEN_CONTINUE,
  TOKEN_BREAK,
  TOKEN_RETURN,
  TOKEN_ARROW,//"->"
  TOKEN_LPAREN,//"("
  TOKEN_RPAREN,//")"
  TOKEN_LBRACE,//"{"
  TOKEN_RBRACE,//"}"
  TOKEN_LBRACKET,//"["
  TOKEN_RBRACKET,//"]"
  TOKEN_SEMI,//";"
  TOKEN_COMMA,//","
  TOKEN_DOT,//"."
  TOKEN_ELLIPSIS,//"..."
  TOKEN_EQ,//"="
  TOKEN_GT,//">"
  TOKEN_LT,//"<"
  TOKEN_BANG,//"!"
  TOKEN_TILDE,//"~"
  TOKEN_QUES,//"?"
  TOKEN_COLON,//":"
  TOKEN_COLONCOLON,//"::"
  TOKEN_EQEQ,//"=="
  TOKEN_LTEQ,//"<="
  TOKEN_GTEQ,//">="
  TOKEN_BANGEQ,//"!="
  TOKEN_AMPAMP,//"&&"
  TOKEN_BARBAR,//"||"
  TOKEN_PLUSPLUS,//"++"
  TOKEN_SUBSUB,//"--"
  TOKEN_PLUS,//"+"
  TOKEN_SUB,//"-"
  TOKEN_STAR,//"*"
  TOKEN_SLASH,//"/"
  TOKEN_AMP,//"&"
  TOKEN_BAR,//"|"
  TOKEN_CARET,//"^"
  TOKEN_PERCENT,//"%"
  TOKEN_LTLT,//"<<"
  TOKEN_GTGT,//">>"
  TOKEN_PLUSEQ,//"+="
  TOKEN_SUBEQ,//"-="
  TOKEN_STAREQ,//"*="
  TOKEN_SLASHEQ,//"/="
  TOKEN_AMPEQ,//"&="
  TOKEN_BAREQ,//"|="
  TOKEN_CARETEQ,//"^="
  TOKEN_PERCENTEQ,//"%="
  TOKEN_LTLTEQ,//"<<="
  TOKEN_GTGTEQ,//">>="
  TOKEN_IDENTIFIER,
  TOKEN_INT_CONSTANT,
  TOKEN_FLOAT_CONSTANT,
  TOKEN_CHARLITERAL,
  TOKEN_STRINGLITERAL,
  TOKEN_EOF,
  TOKEN_UNKNOWN,
};

const std::unordered_map<std::string, TokenKind> keyword_map{
    {"auto", TokenKind::TOKEN_AUTO},
    {"register", TokenKind::TOKEN_REGISTER},
    {"static", TokenKind::TOKEN_STATIC},
    {"extern", TokenKind::TOKEN_EXTERN},
    {"typedef", TokenKind::TOKEN_TYPEDEF},
    {"void", TokenKind::TOKEN_VOID},
    {"char", TokenKind::TOKEN_CHAR},
    {"short", TokenKind::TOKEN_SHORT},
    {"int", TokenKind::TOKEN_INT},
    {"long", TokenKind::TOKEN_LONG},
    {"float", TokenKind::TOKEN_FLOAT},
    {"double", TokenKind::TOKEN_DOUBLE},
    {"signed", TokenKind::TOKEN_SIGNED},
    {"unsigned", TokenKind::TOKEN_UNSIGNED},
    {"struct", TokenKind::TOKEN_STRUCT},
    {"union", TokenKind::TOKEN_UNION},
    {"const", TokenKind::TOKEN_CONST},
    {"volatile", TokenKind::TOKEN_VOLATILE},
    {"sizeof", TokenKind::TOKEN_SIZEOF},
    {"enum", TokenKind::TOKEN_ENUM},
    {"case", TokenKind::TOKEN_CASE},
    {"default", TokenKind::TOKEN_DEFAULT},
    {"if", TokenKind::TOKEN_IF},
    {"else", TokenKind::TOKEN_ELSE},
    {"switch", TokenKind::TOKEN_SWITCH},
    {"while", TokenKind::TOKEN_WHILE},
    {"do", TokenKind::TOKEN_DO},
    {"for", TokenKind::TOKEN_FOR},
    {"goto", TokenKind::TOKEN_GOTO},
    {"continue", TokenKind::TOKEN_CONTINUE},
    {"break", TokenKind::TOKEN_BREAK},
    {"return", TokenKind::TOKEN_RETURN},
};

struct Position {
  long line_number;
  std::streampos line;
  std::streampos pos_start;
  std::streampos pos_end = pos_start;
};

class Token {
 public:
  Token(std::ifstream &ifstream, Position position, TokenKind kind, std::string value = "")
      : ifstream(ifstream), position(position), kind(kind), value(std::move(value)) {}

  const Position &getPosition() const {
    return position;
  }
  const TokenKind getKind() const {
    return kind;
  }

  const bool operator==(TokenKind kind) const {
    return this->kind == kind;
  }

  const bool operator!=(TokenKind kind) const {
    return this->kind != kind;
  }

  const std::string &getValue() const {
    return value;
  }
  std::string getTokenInLine() const {
    std::streampos current_pos = ifstream.tellg();
    ifstream.seekg(getPosition().line);
    std::string s1;
    std::string s2("\n");
    while (true) {
      char c = (char) ifstream.peek();
      if (c == '\r' || c == '\n' || c == EOF) {
        ifstream.seekg(current_pos);
        s1.append(s2);
        return s1;
      } else {
        s1.push_back(c);
        if (ifstream.tellg() >= getPosition().pos_start && ifstream.tellg() <= getPosition().pos_end) {
          s2.push_back('~');
        } else {
          s2.push_back(' ');
        }
        ifstream.ignore();
      }

    }
  }
  static const char *enumToString(mycc::TokenKind kind) {
    switch (kind) {
      case TokenKind::TOKEN_REGISTER: return "register";
      case TokenKind::TOKEN_AUTO: return "auto";
      case TokenKind::TOKEN_STATIC: return "static";
      case TokenKind::TOKEN_EXTERN: return "extern";
      case TokenKind::TOKEN_TYPEDEF: return "typedef";
      case TokenKind::TOKEN_VOID: return "void";
      case TokenKind::TOKEN_CHAR: return "char";
      case TokenKind::TOKEN_SHORT: return "short";
      case TokenKind::TOKEN_INT: return "int";
      case TokenKind::TOKEN_LONG: return "long";
      case TokenKind::TOKEN_FLOAT: return "float";
      case TokenKind::TOKEN_DOUBLE: return "double";
      case TokenKind::TOKEN_SIGNED: return "signed";
      case TokenKind::TOKEN_UNSIGNED: return "unsigned";
      case TokenKind::TOKEN_STRUCT: return "struct";
      case TokenKind::TOKEN_UNION: return "union";
      case TokenKind::TOKEN_CONST: return "const";
      case TokenKind::TOKEN_VOLATILE: return "volatile";
      case TokenKind::TOKEN_SIZEOF: return "sizeof";
      case TokenKind::TOKEN_ENUM: return "enum";
      case TokenKind::TOKEN_CASE: return "case";
      case TokenKind::TOKEN_DEFAULT: return "default";
      case TokenKind::TOKEN_IF: return "if";
      case TokenKind::TOKEN_ELSE: return "else";
      case TokenKind::TOKEN_SWITCH: return "switch";
      case TokenKind::TOKEN_WHILE: return "while";
      case TokenKind::TOKEN_DO: return "do";
      case TokenKind::TOKEN_FOR: return "for";
      case TokenKind::TOKEN_GOTO: return "goto";
      case TokenKind::TOKEN_CONTINUE: return "continue";
      case TokenKind::TOKEN_BREAK: return "break";
      case TokenKind::TOKEN_RETURN: return "return";
      case TokenKind::TOKEN_ARROW: return "arrow";
      case TokenKind::TOKEN_LPAREN: return "lparen";
      case TokenKind::TOKEN_RPAREN: return "rparen";
      case TokenKind::TOKEN_LBRACE: return "lbrace";
      case TokenKind::TOKEN_RBRACE: return "rbrace";
      case TokenKind::TOKEN_LBRACKET: return "lbracket";
      case TokenKind::TOKEN_RBRACKET: return "rbracket";
      case TokenKind::TOKEN_SEMI: return "semi";
      case TokenKind::TOKEN_COMMA: return "comma";
      case TokenKind::TOKEN_DOT: return "dot";
      case TokenKind::TOKEN_ELLIPSIS: return "ellipsis";
      case TokenKind::TOKEN_EQ: return "eq";
      case TokenKind::TOKEN_GT: return "gt";
      case TokenKind::TOKEN_LT: return "lt";
      case TokenKind::TOKEN_BANG: return "bang";
      case TokenKind::TOKEN_TILDE: return "tilde";
      case TokenKind::TOKEN_QUES: return "ques";
      case TokenKind::TOKEN_COLON: return "colon";
      case TokenKind::TOKEN_COLONCOLON: return "coloncolon";
      case TokenKind::TOKEN_EQEQ: return "eqeq";
      case TokenKind::TOKEN_LTEQ: return "lteq";
      case TokenKind::TOKEN_GTEQ: return "gteq";
      case TokenKind::TOKEN_BANGEQ: return "bangeq";
      case TokenKind::TOKEN_AMPAMP: return "ampamp";
      case TokenKind::TOKEN_BARBAR: return "barbar";
      case TokenKind::TOKEN_PLUSPLUS: return "plusplus";
      case TokenKind::TOKEN_SUBSUB: return "subsub";
      case TokenKind::TOKEN_PLUS: return "plus";
      case TokenKind::TOKEN_SUB: return "sub";
      case TokenKind::TOKEN_STAR: return "star";
      case TokenKind::TOKEN_SLASH: return "slash";
      case TokenKind::TOKEN_AMP: return "amp";
      case TokenKind::TOKEN_BAR: return "bar";
      case TokenKind::TOKEN_CARET: return "caret";
      case TokenKind::TOKEN_PERCENT: return "percent";
      case TokenKind::TOKEN_LTLT: return "ltlt";
      case TokenKind::TOKEN_GTGT: return "gtgt";
      case TokenKind::TOKEN_PLUSEQ: return "pluseq";
      case TokenKind::TOKEN_SUBEQ: return "subeq";
      case TokenKind::TOKEN_STAREQ: return "stareq";
      case TokenKind::TOKEN_SLASHEQ: return "slasheq";
      case TokenKind::TOKEN_AMPEQ: return "ampeq";
      case TokenKind::TOKEN_BAREQ: return "bareq";
      case TokenKind::TOKEN_CARETEQ: return "careteq";
      case TokenKind::TOKEN_PERCENTEQ: return "percenteq";
      case TokenKind::TOKEN_LTLTEQ: return "ltlteq";
      case TokenKind::TOKEN_GTGTEQ: return "gtgteq";
      case TokenKind::TOKEN_IDENTIFIER: return "identifier";
      case TokenKind::TOKEN_INT_CONSTANT: return "token_int_constant";
      case TokenKind::TOKEN_FLOAT_CONSTANT: return "TOKEN_FLOAT_CONSTANT";
      case TokenKind::TOKEN_CHARLITERAL: return "charliteral";
      case TokenKind::TOKEN_STRINGLITERAL: return "stringliteral";
      case TokenKind::TOKEN_EOF: return "eof";
      case TokenKind::TOKEN_UNKNOWN: return "Unknown";
      default: return "unknown default";
    }
  }
 private:
  const Position position;
  const TokenKind kind;
  const std::string value;
  std::ifstream &ifstream;
};
} //namespace myccompiler
#endif //MYCCPILER_TOKEN_H
