#ifndef MYCCPILER_SYMBOLTABLE_H
#define MYCCPILER_SYMBOLTABLE_H

#include <map>
#include <vector>

namespace mycc {
class DeclarationAST;
class SymbolTable {
 public:
  SymbolTable(const std::string& name, const SymbolTable &father);
  SymbolTable(const std::string& name);
  const DeclarationAST& lookup(const std::string& name) const;
 private:
  const SymbolTable &father;
  const std::string name;
  std::map<std::string, const DeclarationAST&> map;
};
} //namespace mycc
#endif //MYCCPILER_SYMBOLTABLE_H
