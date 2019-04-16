#include <iostream>
#include <fstream>
#include <lex/lex.h>
#include "src/tools/first_set_generator.h"



int main() {
  std::ifstream testFile;
  testFile.open("test.c");



  mycc::Lex lex(testFile);
  while (!lex.endOfTokens()) {
    mycc::TokenKind kind = lex.peek().getKind();
    std::string value = lex.peek().getValue();
    std::cout << enumToString(kind) << "("<< value << ")" << std::endl;
    lex.consumeToken();
  }
}