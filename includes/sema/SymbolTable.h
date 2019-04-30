#ifndef MYCCPILER_SYMBOLTABLE_H
#define MYCCPILER_SYMBOLTABLE_H

#include <map>
#include <vector>
#include <string>
#include <memory>

namespace mycc {
class DeclarationAST;
class SymbolNotFoundException {};
class SymbolTable {
 public:
  SymbolTable(const std::string &name, const SymbolTable &father);
  SymbolTable(const std::string &name);
  const DeclarationAST &lookup(const std::string &name) const;
 private:
  const SymbolTable &father;
  const std::string name;
  std::map<std::string, const DeclarationAST &> map;
};

enum class SymbolKind {
  OBEJECT,
  FUNCTION,
  TAG,
  TYPEDEF,
  LABEL,
  ENUMERATION_CONSTANT,
};

enum class ScopeKind {
  FILE,
  FUNCTION,
  BLOCK,
  FUNCTION_PROTOTYPE,
};



class SymbolStack {
 public:

};
} //namespace mycc
#endif //MYCCPILER_SYMBOLTABLE_H
