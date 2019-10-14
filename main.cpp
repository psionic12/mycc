#include <iostream>
#include <fstream>
#include <parser/parser.h>
#include <tokens/specifier_combination.h>
#include "llvm/Support/raw_ostream.h"
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include "llvm/IR/DataLayout.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Error.h"

int main() {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();


//  try {
  std::ifstream testFile;
  testFile.open("test.c");
  auto parser = Parser(testFile);
  auto tr = parser.parseTranslationUnit();
    tr->print(0);
  tr->codegen();
  AST::getModule()->print(llvm::outs(), nullptr);
//  } catch (const SemaException &e) {
//    std::cerr << e.what() << std::endl;
//  } catch (const ParserException &e) {
//    std::cerr << e.what() << std::endl;
//  }

  if (llvm::verifyModule(*AST::getModule(), &llvm::errs())) {
    return 0;
  } else {
    std::cout << "Verified success\n";
  }

  std::string error;
  auto engine = llvm::EngineBuilder(AST::takeModule())
      .setErrorStr(&error)
      .setOptLevel(llvm::CodeGenOpt::Aggressive)
      .setEngineKind(llvm::EngineKind::JIT)
      .create();
  if (!engine) {
    std::cerr << "EE Error: " << error << '\n';
    return 1;
  }

  engine->finalizeObject();
  typedef void (*Function)();
  Function f = reinterpret_cast<Function>(
      engine->getPointerToNamedFunction("main"));
  f();
}