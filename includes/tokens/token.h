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
  TOKEN_NUMERIC_CONSTANT,
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
 private:
  const Position position;
  const TokenKind kind;
  const std::string value;
  std::ifstream &ifstream;
};
} //namespace myccompiler
#endif //MYCCPILER_TOKEN_H
