#include <sema/SymbolTable.h>
#include <sema/ast.h>
void mycc::SymbolStack::enterScope(ScopeKind kind) {
  stack.emplace_back(std::map<std::string, Symbol>(), kind);
}
void mycc::SymbolStack::leaveScope(ScopeKind kind) {
  stack.pop_back();
}
void mycc::SymbolStack::insert(std::string name, Symbol symbol) {
  SymbolTable *table;
  ScopeKind kind;
  // insert label to the nearest function scope
  if (symbol.getKind() == SymbolKind::LABEL) {
    for (auto it = stack.rbegin(); it != stack.rend(); ++it) {
      table = &it->first;
      kind = it->second;
      if (kind != ScopeKind::FUNCTION) {
        continue;
      } else {
        break;
      }
    }
    throw ScopeNotFoundException();
  } else {
    table = &stack.back().first;
  }
  try {
    table->at(name);
    throw SymbolExsitsException();
  } catch (const std::out_of_range &) {
    table->emplace(std::move(name), std::move(symbol));
  }

}
const mycc::Symbol &mycc::SymbolStack::lookup(const std::string &name) {
  for (auto it = stack.rbegin(); it != stack.rend(); ++it) {
    const SymbolTable &table = it->first;
    try {
      return table.at(name);
    } catch (const std::out_of_range &) {
      continue;
    }
  }
  throw SymbolNotFoundException();
}
mycc::SymbolKind mycc::SymbolStack::lookupTest(const std::string &name) {
  return std::isupper(name[0]) ? SymbolKind::TYPEDEF : SymbolKind::OBEJECT;
}
bool mycc::Symbol::operator==(mycc::SymbolKind kind) const {
  return this->kind == kind;
}
bool mycc::Symbol::operator!=(mycc::SymbolKind kind) const {
  return !operator==(kind);
}
mycc::Symbol::Symbol(mycc::SymbolKind kind, std::unique_ptr<mycc::Type> type, int value)
    : kind(kind), type(std::move(type)), value(value) {

}
bool mycc::operator==(mycc::SymbolKind kind, const mycc::Symbol &symbol) {
  return symbol.getKind() == kind;
}
bool mycc::operator!=(mycc::SymbolKind kind, const mycc::Symbol &symbol) {
  return !(kind == symbol);
}
