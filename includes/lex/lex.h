#ifndef MYCCPILER_LEX_H
#define MYCCPILER_LEX_H
#include <fstream>
#include <tokens/token.h>
#include <vector>

class Lex {
 public:
  Lex(std::ifstream &ifstream);
  const Token &peek(long offsite = 0) const;
  void consumeToken();
  bool endOfTokens();
  TokenKind lookupTokens(const std::initializer_list<TokenKind> &tokens);
  void throwLexError(std::string error, TokenKind kind);
 private:
  bool end_of_tokens;
  bool eof_consumed;
  std::vector<Token> tokens;
  std::size_t current;
  long currentLineNumber;
  std::streampos currentLine;
  std::streampos tokenStart;
  std::streampos tokenEnd;
  std::ifstream &in;
  void getToken();
  void scanIdent();
  void scanNumber();
  void scanCharConstant();
  void scanStringConstant();
  void skipLineComment();
  void skipBlockComment();
  void makeToken(TokenKind kind, std::string value);
};

class LexException : public std::exception {
 public:
  LexException(Token token, std::string error);
  const char *what() const noexcept override;
 private:
  Token token;
  std::string error;
};
#endif //MYCCPILER_LEX_H
