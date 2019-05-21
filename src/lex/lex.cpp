#include <lex/lex.h>
#include <fstream>
Lex::Lex(std::ifstream &ifstream)
    : in(ifstream),
      currentLineNumber(1),
      currentLine(ifstream.tellg()),
      current(0),
      end_of_tokens(false),
      eof_consumed(false) {
  while (!eof_consumed) {
    getToken();
  }
}
void Lex::getToken() {
  while (!eof_consumed) {
    switch (in.peek()) {
      case ' ':
      case '\t':in.ignore();
        while (in.peek() == ' ' || in.peek() == '\t') {
          in.ignore();
        }
        break;
      case '\r':in.ignore();
        if (in.peek() == '\n') {
          in.ignore();
        }
        currentLine = in.tellg();
        ++currentLineNumber;
        break;
      case '\n':in.ignore();
        currentLine = in.tellg();
        ++currentLineNumber;
        break;
      case 'A':
      case 'B':
      case 'C':
      case 'D':
      case 'E':
      case 'F':
      case 'G':
      case 'H':
      case 'I':
      case 'J':
      case 'K':
      case 'L':
      case 'M':
      case 'N':
      case 'O':
      case 'P':
      case 'Q':
      case 'R':
      case 'S':
      case 'T':
      case 'U':
      case 'V':
      case 'W':
      case 'X':
      case 'Y':
      case 'Z':
      case 'a':
      case 'b':
      case 'c':
      case 'd':
      case 'e':
      case 'f':
      case 'g':
      case 'h':
      case 'i':
      case 'j':
      case 'k':
      case 'l':
      case 'm':
      case 'n':
      case 'o':
      case 'p':
      case 'q':
      case 'r':
      case 's':
      case 't':
      case 'u':
      case 'v':
      case 'w':
      case 'x':
      case 'y':
      case 'z':
      case '_':return scanIdent();
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':return scanNumber();
      case '.':in.ignore();
        if (in.peek() >= '0' && in.peek() <= '9') {
          in.putback('.');
          return scanNumber();
        } else {
          tokenStart = tokenEnd = in.tellg() -= 1;
          return makeToken(TokenKind::TOKEN_DOT);
        }
      case ',':tokenStart = tokenEnd = in.tellg();
        in.ignore();
        return makeToken(TokenKind::TOKEN_COMMA);
      case ';':tokenStart = tokenEnd = in.tellg();
        in.ignore();
        return makeToken(TokenKind::TOKEN_SEMI);
      case '(':tokenStart = tokenEnd = in.tellg();
        in.ignore();
        return makeToken(TokenKind::TOKEN_LPAREN);
      case ')':tokenStart = tokenEnd = in.tellg();
        in.ignore();
        return makeToken(TokenKind::TOKEN_RPAREN);
      case '[':tokenStart = tokenEnd = in.tellg();
        in.ignore();
        return makeToken(TokenKind::TOKEN_LBRACKET);
      case ']':tokenStart = tokenEnd = in.tellg();
        in.ignore();
        return makeToken(TokenKind::TOKEN_RBRACKET);
      case '{':tokenStart = tokenEnd = in.tellg();
        in.ignore();
        return makeToken(TokenKind::TOKEN_LBRACE);
      case '}':tokenStart = tokenEnd = in.tellg();
        in.ignore();
        return makeToken(TokenKind::TOKEN_RBRACE);
      case '?':tokenStart = tokenEnd = in.tellg();
        in.ignore();
        return makeToken(TokenKind::TOKEN_QUES);
      case '~':tokenStart = tokenEnd = in.tellg();
        in.ignore();
        return makeToken(TokenKind::TOKEN_TILDE);
      case '\'':return scanCharConstant();
      case '"':return scanStringConstant();
      case '&':tokenStart = in.tellg();
        in.ignore();
        tokenEnd = in.tellg();
        if (in.peek() == '&') {
          in.ignore();
          return makeToken(TokenKind::TOKEN_AMPAMP);
        } else if (in.peek() == '=') {
          in.ignore();
          return makeToken(TokenKind::TOKEN_AMPEQ);
        } else {
          tokenEnd = tokenStart;
          return makeToken(TokenKind::TOKEN_AMP);
        }
      case '*':tokenStart = in.tellg();
        in.ignore();
        if (in.peek() == '=') {
          tokenEnd = in.tellg();
          in.ignore();
          return makeToken(TokenKind::TOKEN_STAREQ);
        } else {
          tokenEnd = tokenStart;
          return makeToken(TokenKind::TOKEN_STAR);
        }
      case '+':tokenStart = in.tellg();
        in.ignore();
        tokenEnd = in.tellg();
        if (in.peek() == '+') {
          in.ignore();
          return makeToken(TokenKind::TOKEN_PLUSPLUS);
        } else if (in.peek() == '=') {
          in.ignore();
          return makeToken(TokenKind::TOKEN_PLUSEQ);
        } else {
          tokenEnd = tokenStart;
          return makeToken(TokenKind::TOKEN_PLUS);
        }
      case '-':tokenStart = in.tellg();
        in.ignore();
        tokenEnd = in.tellg();
        if (in.peek() == '-') {
          in.ignore();
          return makeToken(TokenKind::TOKEN_SUBSUB);
        } else if (in.peek() == '=') {
          in.ignore();
          return makeToken(TokenKind::TOKEN_SUBEQ);
        } else if (in.peek() == '>') {
          return makeToken(TokenKind::TOKEN_ARROW);
        } else {
          tokenEnd = tokenStart;
          return makeToken(TokenKind::TOKEN_SUB);
        }
      case '!':tokenStart = in.tellg();
        in.ignore();
        if (in.peek() == '=') {
          tokenEnd = in.tellg();
          in.ignore();
          return makeToken(TokenKind::TOKEN_BANGEQ);
        } else {
          tokenEnd = tokenStart;
          return makeToken(TokenKind::TOKEN_BANG);
        }
      case '/':tokenStart = in.tellg();
        in.ignore();
        if (in.peek() == '/') {
          skipLineComment();
          break;
        } else if (in.peek() == '*') {
          skipBlockComment();
          break;
        } else if (in.peek() == '=') {
          tokenEnd = in.tellg();
          in.ignore();
          return makeToken(TokenKind::TOKEN_SLASHEQ);
        } else {
          tokenEnd = tokenStart;
          return makeToken(TokenKind::TOKEN_SLASH);
        }
      case '%':tokenStart = in.tellg();
        in.ignore();
        if (in.peek() == '=') {
          tokenEnd = in.tellg();
          in.ignore();
          return makeToken(TokenKind::TOKEN_PERCENTEQ);
        } else {
          tokenEnd = tokenStart;
          return makeToken(TokenKind::TOKEN_PERCENT);
        }
      case '<':tokenStart = in.tellg();
        in.ignore();
        tokenEnd = in.tellg();
        if (in.peek() == '<') {
          in.ignore();
          if (in.peek() == '=') {
            tokenEnd = in.tellg();
            in.ignore();
            return makeToken(TokenKind::TOKEN_LTLTEQ);
          } else {
            return makeToken(TokenKind::TOKEN_LTLT);
          }
        } else if (in.peek() == '=') {
          in.ignore();
          return makeToken(TokenKind::TOKEN_LTEQ);
        } else {
          tokenEnd = tokenStart;
          return makeToken(TokenKind::TOKEN_LT);
        }
      case '>':tokenStart = in.tellg();
        in.ignore();
        tokenEnd = in.tellg();
        if (in.peek() == '>') {
          in.ignore();
          if (in.peek() == '=') {
            tokenEnd = in.tellg();
            in.ignore();
            return makeToken(TokenKind::TOKEN_GTGTEQ);
          } else {
            return makeToken(TokenKind::TOKEN_GTGT);
          }
        } else if (in.peek() == '=') {
          in.ignore();
          return makeToken(TokenKind::TOKEN_GTEQ);
        } else {
          tokenEnd = tokenStart;
          return makeToken(TokenKind::TOKEN_GT);
        }
      case '^':tokenStart = in.tellg();
        in.ignore();
        if (in.peek() == '=') {
          tokenEnd = in.tellg();
          in.ignore();
          return makeToken(TokenKind::TOKEN_CARETEQ);
        } else {
          tokenEnd = tokenStart;
          return makeToken(TokenKind::TOKEN_CARET);
        }
      case '|':tokenStart = in.tellg();
        in.ignore();
        tokenEnd = in.tellg();
        if (in.peek() == '|') {
          in.ignore();
          return makeToken(TokenKind::TOKEN_BARBAR);
        } else if (in.peek() == '=') {
          in.ignore();
          return makeToken(TokenKind::TOKEN_BAREQ);
        } else {
          tokenEnd = tokenStart;
          return makeToken(TokenKind::TOKEN_BAR);
        }
      case ':':tokenStart = in.tellg();
        in.ignore();
        if (in.peek() == ':') {
          tokenEnd = in.tellg();
          in.ignore();
          return makeToken(TokenKind::TOKEN_COLONCOLON);
        } else {
          tokenEnd = tokenStart;
          return makeToken(TokenKind::TOKEN_COLON);
        }
      case '=':tokenStart = in.tellg();
        in.ignore();
        if (in.peek() == '=') {
          tokenEnd = in.tellg();
          in.ignore();
          return makeToken(TokenKind::TOKEN_EQEQ);
        } else {
          tokenEnd = tokenStart;
          return makeToken(TokenKind::TOKEN_EQ);
        }
      case EOF:tokenStart = tokenEnd = in.tellg();
        in.ignore();
        makeToken(TokenKind::TOKEN_EOF);
        eof_consumed = true;
        return;
      default:tokenStart = tokenEnd = in.tellg();
        std::string error = std::string("unknown token:");
        error.push_back((char) in.peek());
        throwLexError(error, TokenKind::TOKEN_UNKNOWN);
    }
  }
}
void Lex::scanIdent() {
  tokenStart = in.tellg();
  std::string value;
  do {
    char c = in.peek();
    switch (c) {
      case 'A':
      case 'B':
      case 'C':
      case 'D':
      case 'E':
      case 'F':
      case 'G':
      case 'H':
      case 'I':
      case 'J':
      case 'K':
      case 'L':
      case 'M':
      case 'N':
      case 'O':
      case 'P':
      case 'Q':
      case 'R':
      case 'S':
      case 'T':
      case 'U':
      case 'V':
      case 'W':
      case 'X':
      case 'Y':
      case 'Z':
      case 'a':
      case 'b':
      case 'c':
      case 'd':
      case 'e':
      case 'f':
      case 'g':
      case 'h':
      case 'i':
      case 'j':
      case 'k':
      case 'l':
      case 'm':
      case 'n':
      case 'o':
      case 'p':
      case 'q':
      case 'r':
      case 's':
      case 't':
      case 'u':
      case 'v':
      case 'w':
      case 'x':
      case 'y':
      case 'z':
      case '_':
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':value += (char) in.get();
        tokenEnd = in.tellg();
        break;
      default:TokenKind kind;
        try {
          kind = keyword_map.at(value);
        } catch (std::out_of_range &) {
          kind = TokenKind::TOKEN_IDENTIFIER;
        }
        return makeToken(kind, std::move(value));
    }
  } while (true);
}
void Lex::scanNumber() {
  tokenStart = in.tellg();
  std::string value;
  TokenKind kind = TokenKind::TOKEN_INT_CONSTANT;
  do {
    switch (in.peek()) {
      case '.':
        // TODO right?
        kind = TokenKind::TOKEN_FLOAT_CONSTANT;
      case 'A':
      case 'B':
      case 'C':
      case 'D':
      case 'E':
      case 'F':
      case 'G':
      case 'H':
      case 'I':
      case 'J':
      case 'K':
      case 'L':
      case 'M':
      case 'N':
      case 'O':
      case 'P':
      case 'Q':
      case 'R':
      case 'S':
      case 'T':
      case 'U':
      case 'V':
      case 'W':
      case 'X':
      case 'Y':
      case 'Z':
      case 'a':
      case 'b':
      case 'c':
      case 'd':
      case 'e':
      case 'f':
      case 'g':
      case 'h':
      case 'i':
      case 'j':
      case 'k':
      case 'l':
      case 'm':
      case 'n':
      case 'o':
      case 'p':
      case 'q':
      case 'r':
      case 's':
      case 't':
      case 'u':
      case 'v':
      case 'w':
      case 'x':
      case 'y':
      case 'z':
      case '_':
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':value += (char) in.get();
        tokenEnd = in.tellg();
        break;
      default:return makeToken(kind, std::move(value));
    }
  } while (true);
}
void Lex::scanCharConstant() {
  tokenStart = in.tellg();
  char c;
  in.ignore();
  if (in.peek() == '\'') {
    throwLexError(std::string("empty character constant"), TokenKind::TOKEN_CHARLITERAL);
  }
  if (in.peek() == '\n' || in.peek() == '\r' || in.eof()) {
    throwLexError(std::string("unclosed.char.lit"), TokenKind::TOKEN_CHARLITERAL);
  }
  if (in.peek() == '\\') {
    in.ignore();
    switch (in.get()) {
      case 'b': c = '\b';
        break;
      case 't': c = '\t';
        break;
      case 'n': c = '\n';
        break;
      case 'f': c = '\f';
        break;
      case 'r': c = '\r';
        break;
      case '\'': c = '\'';
        break;
      case '\"': c = '\"';
        break;
      case '\\': c = '\\';
        break;
      default:throwLexError(std::string("illegal.esc.char"), TokenKind::TOKEN_CHARLITERAL);
    }
  } else {
    c = (char) in.get();
  }

  if (in.peek() == '\'') {
    tokenEnd = in.tellg();
    in.ignore();
    return makeToken(TokenKind::TOKEN_CHARLITERAL, std::string(1, c));
  } else {
    throwLexError(std::string("unclosed.char.lit"), TokenKind::TOKEN_CHARLITERAL);
  }
}
void Lex::consumeToken() {
  if (tokens[current] != TokenKind::TOKEN_EOF) {
    getToken();
    ++current;
  } else {
    end_of_tokens = true;
  }
}
const Token &
Lex::peek(long offsite) const {
  return tokens[current + offsite];
}
void Lex::makeToken(TokenKind kind, std::string value) {
  tokens.emplace_back(in,
                             Position{currentLineNumber, currentLine, tokenStart, tokenEnd},
                             kind,
                             std::move(value));
}
void Lex::scanStringConstant() {
  tokenStart = in.tellg();
  in.ignore();
  std::string value;
  while (in.peek() != '"') {
    if (in.peek() == '\n' || in.peek() == '\r' || in.eof()) {
      throwLexError(std::string("unclosed.char.lit"), TokenKind::TOKEN_CHARLITERAL);
    }
    if (in.peek() == '\\') {
      in.ignore();
      switch (in.get()) {
        case 'b':value.push_back('\b');
          continue;
        case 't':value.push_back('\t');
          continue;
        case 'n':value.push_back('\n');
          continue;
        case 'f':value.push_back('\f');
          continue;
        case 'r':value.push_back('\r');
          continue;
        case '\'':value.push_back('\'');
          continue;
        case '\"':value.push_back('\"');
          continue;
        case '\\':value.push_back('\\');
          continue;
        default:throwLexError(std::string("illegal.esc.char"), TokenKind::TOKEN_STRINGLITERAL);
      }
    } else {
      value.push_back((char) in.get());
    }
  }
  tokenEnd = in.tellg();
  in.ignore();
  return makeToken(TokenKind::TOKEN_STRINGLITERAL, std::move(value));
}
void Lex::skipLineComment() {
  in.ignore();
  while (in.peek() != '\r' && in.peek() != '\n' && in.peek() != in.eof()) {
    in.ignore();
  }
}
void Lex::skipBlockComment() {
  in.ignore();
  while (!in.eof()) {
    if (in.get() == '*') {
      if (in.get() == '/') {
        return;
      }
    }
  }
}
void Lex::throwLexError(std::string error, TokenKind kind) {
  Token token(in, Position{currentLineNumber, currentLine, tokenStart, tokenEnd}, kind);
  throw LexException(std::move(token), std::move(error));
}
bool Lex::endOfTokens() {
  return end_of_tokens;
}
TokenKind Lex::lookupTokens(const std::initializer_list<TokenKind> &tokens) {
  for (auto it = this->tokens.begin() + current; it != this->tokens.end(); ++it) {
    for (auto token : tokens) {
      if (*it == token) {
        return it->getKind();
      }
    }
  }
  return TokenKind::TOKEN_UNKNOWN;
}
const char *LexException::what() const noexcept {
  return error.c_str();
}
LexException::LexException(Token token, std::string error)
    : token(std::move(token)), error(std::move(error)) {
  this->error.append("\n").append(token.getTokenInLine());
}
