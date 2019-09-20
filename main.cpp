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
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Error.h"

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
  ES.getMainJITDylib().setGenerator(
      llvm::cantFail(llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(*DL)));


//  try {
  std::ifstream testFile;
  testFile.open("test.c");
  auto parser = Parser(testFile);
  auto tr = parser.parseTranslationUnit();
//    tr->print(0);
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

  // run the generated IR code
  llvm::cantFail(CompileLayer.add(ES.getMainJITDylib(),
                                  llvm::orc::ThreadSafeModule(std::move(AST::takeModule()),
                                                              AST::takeContext())));

  auto symbol = llvm::cantFail(ES.lookup({&ES.getMainJITDylib()}, Mangle("main")));

  int (*entry)() = (decltype(entry)) symbol.getAddress();
  std::cout << entry() << std::endl;
}