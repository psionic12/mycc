#include <sema/symbol_tables.h>
ISymbol * SymbolTable::lookup(const Token &token) {
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
SymbolTable *SymbolTables::createTable(ScopeKind kind, SymbolTable *father) {
  tables.emplace_back(kind, father, module);
  return &tables.back();
}
