#include <sema/symbol_tables.h>
#include <sema/ast.h>
#include <llvm/IR/IRBuilder.h>
ISymbol *SymbolTable::lookup(const Token &token) const {
  try {
    return at(token.getValue());
  } catch (const std::out_of_range &) {
    if (mFather) {
      return mFather->lookup(token);
    } else {
      return nullptr;
    }
  }
}
bool SymbolTable::isTypedef(const Token &token) {
  return lookup(token) != nullptr && *lookup(token) == SymbolKind::TYPEDEF;
}
ISymbol *SymbolTable::insert(const Token &token, ISymbol *symbol) {
  try {
    at(token.getValue());
    throw SemaException(std::string("symbol \"") + token.getValue() + "\" already defined", token);
  } catch (const std::out_of_range &) {
    return emplace(token.getValue(), symbol).first->second;
  }
}
ISymbol *SymbolTable::insert(ISymbol *symbol) {
  return emplace(std::string("$") + std::to_string(mAnonymousId++), symbol).first->second;
}
void SymbolTable::setFather(SymbolTable *father) {
  SymbolTable::mFather = father;
}
SymbolTable *SymbolTable::getFather() const {
  return mFather;
}
ScopeKind SymbolTable::getScopeKind() const {
  return mScopeKind;
}
SymbolTable *SymbolTables::createTable(ScopeKind kind) {
  tables.emplace_back(kind);
  return &tables.back();
}