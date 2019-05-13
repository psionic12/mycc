#include <iostream>
#include <fstream>
#include <parser/parser.h>
#include "src/tools/first_set_generator.h"



int main() {
  std::ifstream testFile;
  testFile.open("test.c");

  auto parser = Parser(testFile);
  auto tr = parser.parseTranslationUnit();
}