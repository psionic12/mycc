#ifndef MYCCPILER_LEX_H
#define MYCCPILER_LEX_H
#include <fstream>
#include <tokens/token.h>
#include <vector>
namespace mycc {
class Lex {
 public:
  Lex(std::ifstream &ifstream);
  const Token &peek(unsigned long offsite = 0);
  void consumeToken();
  bool endOfTokens();
  TokenKind lookupTokens(const std::initializer_list<TokenKind> &tokens);
 private:
  bool end_of_tokens;
  bool eof_consumed;
  void getToken();
  std::ifstream &in;
  void scanIdent();
  void scanNumber();
  void scanCharConstant();
  void scanStringConstant();
  void skipLineComment();
  void skipBlockComment();
  std::vector<Token> tokens;
  std::size_t current;
  long currentLineNumber;
  std::streampos currentLine;
  std::streampos tokenStart;
  std::streampos tokenEnd;
  void makeToken(TokenKind kind, std::string value = "");
  void throwLexError(std::string error, TokenKind kind);
};

class LexException : public std::exception {
 public:
  LexException(Token token, std::string error);
  const char *what() const noexcept override;
 private:
  Token token;
  std::string error;
};
} //namespace mycc
#endif //MYCCPILER_LEX_H
