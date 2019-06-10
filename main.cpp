#include <iostream>
#include <fstream>
#include <parser/parser.h>

int main() {
  std::ifstream testFile;
  testFile.open("test.c");
  llvm::LLVMContext TheContext;
  llvm::Module module("test", TheContext);
  SymbolTables tables(module);
  auto parser = Parser(testFile, tables);
  auto tr = parser.parseTranslationUnit();
  tr->print(0);
}