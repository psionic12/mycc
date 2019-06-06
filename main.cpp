#include <iostream>
#include <fstream>
#include <parser/parser.h>

int main() {
  std::ifstream testFile;
  testFile.open("test.c");

  SymbolTables tables;
  auto parser = Parser(testFile, tables);
  auto tr = parser.parseTranslationUnit();
  tr->print(0);
}