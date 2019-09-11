#include <iostream>
#include <fstream>
#include <parser/parser.h>
#include <tokens/specifier_combination.h>
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"

int main() {
//  try {
    std::ifstream testFile;
    testFile.open("test.c");
    auto parser = Parser(testFile);
    auto tr = parser.parseTranslationUnit();
//    tr->print(0);
    tr->codegen();
    AST::getModule().print(llvm::errs(), nullptr);
//  } catch (const SemaException &e) {
//    std::cerr << e.what() << std::endl;
//  } catch (const ParserException &e) {
//    std::cerr << e.what() << std::endl;
//  }
}