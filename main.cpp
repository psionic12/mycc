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
#include <boost/program_options.hpp>

namespace po = boost::program_options;

int main(int argc, char *argv[]) {
  std::string inputName;
  bool printAST = false;
  bool printIR = false;
  bool execute = false;
  unsigned int ac = 0;
  std::string av{};

  po::options_description desc("Allowed options");
  desc.add_options()
      ("help", "produce help message")
      ("print-ast", "print the abstract syntax tree")
      ("print-ir", "print the generated IR code")
      ("execute", "execute main function with parameters")
      ("input-file", po::value<std::vector<std::string> >(), "input file");
  po::positional_options_description p;
  p.add("input-file", -1);

  po::variables_map vm;
  po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
  po::notify(vm);

  if (vm.empty()) {
    std::cout << "no input files" << std::endl;
    return 0;
  }

  if (vm.count("help")) {
    std::cout << "Usage: mycc [options] file..." << std::endl;
    std::cout << desc << std::endl;
    return 0;
  }

  if (vm.count("input-file")) {
    auto v = vm["input-file"].as<std::vector<std::string>>();
    if (v.size() > 1) {
      std::cout << "multiple input files detected, currently do not support";
    }
    inputName = v[0];
  }

  if (vm.count("print-ast")) {
    printAST = true;
  }

  if (vm.count("print-ir")) {
    printIR = true;
  }

  if (vm.count("execute")) {
    execute = true;
  }

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  std::ifstream testFile;
  testFile.open(inputName.c_str());
  auto parser = Parser(testFile);
  auto tr = parser.parseTranslationUnit();
  if (printAST) {
    tr->print(0);
  }
  tr->codegen();
  if (printIR) {
    AST::getModule()->print(llvm::outs(), nullptr);
  }

  if (llvm::verifyModule(*AST::getModule(), &llvm::errs())) {
    return 0;
  }

  if (execute) {
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
    typedef int (*Function)();
    Function f = reinterpret_cast<Function>(
        engine->getPointerToNamedFunction("main"));
    f();
  }
  return 0;
}