#include <iostream>
#include <fstream>
#include <parser/parser.h>
#include <tokens/specifier_combination.h>
#include "llvm/Support/raw_ostream.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/Support/TargetSelect.h"

int main() {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();
  // create jit
  llvm::orc::ExecutionSession ES;
  llvm::orc::RTDyldObjectLinkingLayer ObjectLayer(ES,
                                                  []() { return std::make_unique<llvm::SectionMemoryManager>(); });
  auto JTMB = llvm::orc::JITTargetMachineBuilder::detectHost();
  auto DL = JTMB->getDefaultDataLayoutForTarget();
  llvm::orc::IRCompileLayer CompileLayer(ES, ObjectLayer, llvm::orc::ConcurrentIRCompiler(std::move(*JTMB)));
  llvm::orc::MangleAndInterner Mangle(ES, *DL);


//  try {
  std::ifstream testFile;
  testFile.open("test.c");
  auto parser = Parser(testFile);
  auto tr = parser.parseTranslationUnit();
//    tr->print(0);
  tr->codegen();
  AST::getModule()->print(llvm::errs(), nullptr);
//  } catch (const SemaException &e) {
//    std::cerr << e.what() << std::endl;
//  } catch (const ParserException &e) {
//    std::cerr << e.what() << std::endl;
//  }

  // run the generated IR code
  CompileLayer.add(ES.getMainJITDylib(),
                   llvm::orc::ThreadSafeModule(std::move(AST::takeModule()), AST::takeContext()));
  auto symbol = ES.lookup({&ES.getMainJITDylib()}, Mangle("main"));
  if (!symbol) {
    std::cerr << "main() function not found" << std::endl;
    return 0;
  }
  int (*entry)() = (decltype(entry))symbol->getAddress();
  std::cout << entry() << std::endl;
}