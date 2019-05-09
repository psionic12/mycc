#ifndef MYCCPILER_SYMBOLTABLE_H
#define MYCCPILER_SYMBOLTABLE_H

#include <map>
#include <vector>
#include <string>
#include <memory>
#include "types.h"

namespace mycc {
class SymbolNotFoundException {};
class ScopeNotFoundException {};
class SymbolExsitsException {};

enum class SymbolKind {
  OBEJECT,
  FUNCTION,
  TAG,
  MEMBER,
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

class Symbol {
 public:
  Symbol(SymbolKind kind, std::unique_ptr<Type> type, int value = 0);
 private:
  std::unique_ptr<Type> type;
  SymbolKind kind;
  int value;
 public:
  const Type *getType() const {
    return type.get();
  }
  SymbolKind getKind() const {
    return kind;
  }
  bool operator==(SymbolKind kind) const;
  bool operator!=(SymbolKind kind) const;
};

bool operator==(SymbolKind kind, const Symbol& symbol);
bool operator!=(SymbolKind kind, const Symbol& symbol);

class SymbolStack {
 private:
  typedef std::map<std::string, Symbol> SymbolTable;
 public:
  void enterScope(ScopeKind kind);
  void leaveScope(ScopeKind kind);
  void insert(std::string name, Symbol symbol);
  const Symbol &lookup(const std::string &name);
  SymbolKind lookupTest(const std::string &name);
 private:
  std::vector<std::pair<SymbolTable, ScopeKind>> stack;
};
} //namespace mycc
#endif //MYCCPILER_SYMBOLTABLE_H
