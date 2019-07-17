#include <sema/symbol_tables.h>
#include <sema/ast.h>
ISymbol *SymbolTable::lookup(const Token &token) {
  try {
    return at(token.getValue()).get();
  } catch (const std::out_of_range &) {
    if (father) {
      return father->lookup(token);
    } else {
      return nullptr;
    }
  }
}
bool SymbolTable::isTypedef(const Token &token) {
  return lookup(token) != nullptr && *lookup(token) == SymbolKind::TYPEDEF;
}
ISymbol * SymbolTable::insert(const Token &token, std::unique_ptr<ISymbol> &&symbol) {
  try {
    at(token.getValue());
    throw SemaException(std::string("symbol \"") + token.getValue() + "\" already defined", token);
  } catch (const std::out_of_range &) {
    return emplace(token.getValue(), std::move(symbol)).first->second.get();
  }
}
void SymbolTable::setFather(SymbolTable *father) {
  SymbolTable::father = father;
}
SymbolTable *SymbolTable::getFather() const {
  return father;
}
ISymbol * SymbolTable::insert(std::string name, std::unique_ptr<ISymbol> &&symbol) {
  return emplace(std::move(name), std::move(symbol)).first->second.get();
}
std::string SymbolTable::getAnonymousName() {
  return std::string("$") + std::to_string(anonymousId++);
}
SymbolTable *SymbolTables::createTable(ScopeKind kind, SymbolTable *father) {
  tables.emplace_back(kind, father);
  return &tables.back();
}
