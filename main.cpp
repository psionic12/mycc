#include <iostream>
#include <fstream>
#include <parser/parser.h>
#include <tokens/specifier_combination.h>

int main() {
  try {
    std::ifstream testFile;
    testFile.open("test.c");
    auto parser = Parser(testFile);
    auto tr = parser.parseTranslationUnit();
    tr->print(0);
    tr->codegen();
  } catch (const SemaException &e) {
    std::cerr << e.what() << std::endl;
  } catch (const ParserException &e) {
    std::cerr << e.what() << std::endl;
  }
}