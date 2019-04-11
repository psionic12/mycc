#ifndef MYCCPILER_SYMBOLTABLE_H
#define MYCCPILER_SYMBOLTABLE_H

#include <map>
#include <vector>
<<<<<<< HEAD

namespace mycc {
class DeclarationAST;
=======
#include <ast/ast.h>

namespace mycc {
>>>>>>> fd9357080aa517249c7062a1c2578683b7eac34f
class SymbolTable {
 public:
  SymbolTable(const std::string& name, const SymbolTable &father);
  SymbolTable(const std::string& name);
<<<<<<< HEAD
  const DeclarationAST& lookup(const std::string& name) const;
 private:
  const SymbolTable &father;
  const std::string name;
  std::map<std::string, const DeclarationAST&> map;
=======
  const DeclarationAST& lookup(const std::string& name);
 private:
  const SymbolTable &father;
  const std::string name;
  std::map<std::string, const mycc::DeclarationAST&> map;
>>>>>>> fd9357080aa517249c7062a1c2578683b7eac34f
};
} //namespace mycc
#endif //MYCCPILER_SYMBOLTABLE_H
