#include <sema/SymbolTable.h>
#include <sema/ast.h>
void SymbolStack::enterScope(ScopeKind kind) {
  stack.emplace_back(std::map<std::string, Symbol>(), kind);
}
void SymbolStack::leaveScope(ScopeKind kind) {
  stack.pop_back();
}
void SymbolStack::insert(std::string name, Symbol symbol) {
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
const Symbol &SymbolStack::lookup(const std::string &name) {
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
SymbolKind SymbolStack::lookupTest(const std::string &name) {
  return std::isupper(name[0]) ? SymbolKind::TYPEDEF : SymbolKind::OBEJECT;
}
bool Symbol::operator==(SymbolKind kind) const {
  return this->kind == kind;
}
bool Symbol::operator!=(SymbolKind kind) const {
  return !operator==(kind);
}
Symbol::Symbol(SymbolKind kind, std::unique_ptr<Type> type, int value)
    : kind(kind), type(std::move(type)), value(value) {

}
bool operator==(SymbolKind kind, const Symbol &symbol) {
  return symbol.getKind() == kind;
}
bool operator!=(SymbolKind kind, const Symbol &symbol) {
  return !(kind == symbol);
}
