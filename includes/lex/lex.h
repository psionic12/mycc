#ifndef MYCCPILER_LEX_H
#define MYCCPILER_LEX_H
#include <fstream>
#include <tokens/token.h>
#include <vector>
namespace mycc {
class Lex {
 public:
  explicit Lex(std::ifstream &ifstream);
  Token *get();
  Token *peek();
  void consumeToken();
 private:
  Token getToken();
  std::ifstream &in;
  Token scanIdent();
  Token scanNumber();
  mycc::Token scanCharConstant();
  Token scanStringConstant();
  void skipLineComment();
  void skipBlockComment();
  std::vector<Token> tokens;
  Token *currentToken;
  long currentLineNumber;
  std::streampos currentLine;
  std::streampos tokenStart;
  std::streampos tokenEnd;
  Token makeToken(TokenKind kind, std::string value = "");
};

class TokenException : public std::exception {
 public:
  TokenException(Token token, std::string error);
  const char *what() const noexcept override;
 private:
  Token token;
  std::string error;
};
} //namespace mycc
#endif //MYCCPILER_LEX_H
