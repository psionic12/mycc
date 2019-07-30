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
ISymbol *SymbolTable::insert(const Token &token, std::unique_ptr<ISymbol> &&symbol) {
  try {
    at(token.getValue());
    throw SemaException(std::string("symbol \"") + token.getValue() + "\" already defined", token);
  } catch (const std::out_of_range &) {
    return emplace(token.getValue(), std::move(symbol)).first->second.get();
  }
}
ISymbol *SymbolTable::insert(std::unique_ptr<ISymbol> &&symbol) {
  return emplace(std::string("$") + std::to_string(anonymousId++), std::move(symbol)).first->second.get();
}
void SymbolTable::setFather(SymbolTable *father) {
  SymbolTable::father = father;
}
SymbolTable *SymbolTable::getFather() const {
  return father;
}
ScopeKind SymbolTable::getScopeKind() const {
  return scope_kind;
}
void SymbolTable::setValue(const Token &token, ISymbol *symbol) {
  if (auto *s = dynamic_cast<ObjectSymbol *>(symbol)) {
    switch (scope_kind) {
      case ScopeKind::FILE: {
        mModule.getOrInsertGlobal(token.getValue(), s->getType()->getLLVMType(mModule));
        llvm::GlobalVariable *gVar = mModule.getNamedGlobal(token.getValue());
        Linkage linkage = s->getLinkage();
        if (linkage != Linkage::kNone) {
          gVar->setLinkage(linkage == Linkage::kExternal ? llvm::GlobalValue::LinkageTypes::ExternalLinkage
                                                         : llvm::GlobalVariable::LinkageTypes::InternalLinkage);
        }
        if (s->getQualifiers().find(TypeQualifier::kCONST) != s->getQualifiers().end()) {
          gVar->setConstant(true);
        }
      }
      case ScopeKind::FUNCTION:
      case ScopeKind::BLOCK:

      case ScopeKind::FUNCTION_PROTOTYPE:
    }
  }
}
SymbolTable *SymbolTables::createTable(ScopeKind kind, SymbolTable *father) {
  tables.emplace_back(kind, father);
  return &tables.back();
}
