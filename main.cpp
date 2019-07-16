#include <iostream>
#include <fstream>
#include <parser/parser.h>
#include <sema/Sema.h>
#include <tokens/specifier_combination.h>

int main() {
  try {
    std::ifstream testFile;
    testFile.open("test.c");
    llvm::LLVMContext TheContext;
    llvm::Module module("test", TheContext);
    llvm::IRBuilder<> builder(TheContext);
    SymbolTables tables;
    auto parser = Parser(testFile, tables);
    auto tr = parser.parseTranslationUnit();
    tr->print(0);
    auto sema = Sema(std::move(tr), module, builder);
    sema.analyze();

  } catch (const SemaException &e) {
    std::cerr << e.what() << std::endl;
  } catch (const ParserException &e) {
    std::cerr << e.what() << std::endl;
  }
}