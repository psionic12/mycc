#include <sema/symbol_tables.h>
const ISymbol *SymbolTable::lookup(const Token &token, SymbolKind symbol_kind, const std::string &name_space) const {
  const ISymbol *symbol = lookupInner(token, symbol_kind, name_space);
  if (symbol) return symbol;
  else throw SemaException(std::string(token.getValue()).append(" not defined"), token);
}
void SymbolTable::insert(const Token &token,
                         SymbolKind symbol_kind,
                         llvm::Type *type,
                         StorageSpecifier storage_specifier) {
  // scope check
  SymbolTable *table = this;
  if (symbol_kind == SymbolKind::LABEL) {
    // insert label to the nearest function scope
    do {
      if (table->scope_kind != ScopeKind::FUNCTION) {
        table = table->father;
      } else {
        break;
      }
    } while (table != nullptr);

    if (table == nullptr) {
      throw SemaException(std::string(token.getValue()).append(" must define in function"), token);
    }
  }

  Linkage linkage;
  const ISymbol *linkage_symbol = nullptr;
  if ((symbol_kind != SymbolKind::OBJECT && symbol_kind != SymbolKind::FUNCTION)
      || table->scope_kind == ScopeKind::FUNCTION_PROTOTYPE
      || (table->scope_kind == ScopeKind::BLOCK && symbol_kind == SymbolKind::OBJECT
          && storage_specifier != StorageSpecifier::kEXTERN)) {
    linkage = Linkage::kNone;
  } else if (table->scope_kind == ScopeKind::FILE
      && (symbol_kind == SymbolKind::OBJECT || symbol_kind == SymbolKind::FUNCTION)
      && storage_specifier == StorageSpecifier::kSTATIC) {
    linkage = Linkage::kInternal;
  } else if (storage_specifier == StorageSpecifier::kEXTERN || symbol_kind == SymbolKind::FUNCTION
      || (symbol_kind == SymbolKind::OBJECT && table->scope_kind == ScopeKind::FILE)) {
    linkage_symbol = lookupInner(token, SymbolKind::OBJECT);  // object and function follow the same rule
    if (linkage_symbol) {
      // type check
      if (linkage_symbol->getKind() != symbol_kind) {
        throw SemaException(std::string("Redefination of \'") + token.getValue() + "\' as different kind of symbol",
                            token);
      } else {
        llvm::Type *linkage_symbol_type = nullptr;
        if (linkage_symbol->getKind() == SymbolKind::OBJECT) {
          auto *object_symbol = static_cast<const ObjectSymbol *>(linkage_symbol);
          linkage_symbol_type = object_symbol->getValue()->getType();
        } else {
          auto *function_symbol = static_cast<const FunctionSymbol *>(linkage_symbol);
          linkage_symbol_type = function_symbol->getFunction()->getType();
        }
        if (linkage_symbol_type != type) {
          throw SemaException(std::string("redeclaration of \'") + token.getValue() + "\' with a different type",
                              token);
        }
      }

      if (linkage_symbol->linkage == Linkage::kExternal
          || linkage_symbol->linkage == Linkage::kInternal) {
        linkage = linkage_symbol->linkage;
      } else {
        linkage = Linkage::kExternal;
      }
    } else {
      linkage = Linkage::kExternal;
    }
  } else {
    // check to see if the standard misses some case...
    throw SemaException(
        std::string("don't know how to deal with ")
            + std::to_string(static_cast<int>(symbol_kind)) + " "
            + std::to_string(static_cast<int>(storage_specifier)), token);
  }

  if (linkage_symbol) {
    std::unique_ptr<ISymbol> symbol;
    switch (linkage_symbol->getKind()) {
      case SymbolKind::OBJECT:
        symbol = std::make_unique<ObjectSymbol>(*static_cast<const ObjectSymbol *>(linkage_symbol));
        break;
      case SymbolKind::FUNCTION:
        symbol = std::make_unique<FunctionSymbol>(*static_cast<const FunctionSymbol *>(linkage_symbol));
        break;
      case SymbolKind::TAG:symbol = std::make_unique<TagSymbol>(*static_cast<const TagSymbol *>(linkage_symbol));
        break;
      case SymbolKind::MEMBER:
        symbol = std::make_unique<MemberSymbol>(*static_cast<const MemberSymbol *>(linkage_symbol));
        break;
      case SymbolKind::TYPEDEF:
        symbol = std::make_unique<TypedefSymbol>(*static_cast<const TypedefSymbol *>(linkage_symbol));
        break;
      case SymbolKind::LABEL:
        symbol = std::make_unique<LabelSymbol>(*static_cast<const LabelSymbol *>(linkage_symbol));
        break;
      case SymbolKind::ENUMERATION_CONSTANT:
        symbol = std::make_unique<EnumConstSymbol>(*static_cast<const EnumConstSymbol *>(linkage_symbol));
        break;
    }
    auto entry = table->ordinary_table.emplace(token.getValue(), std::move(symbol));
  } else {
    switch (symbol_kind) {
      case SymbolKind::OBJECT:
      case SymbolKind::FUNCTION:
      case SymbolKind::TAG:
      case SymbolKind::MEMBER:
      case SymbolKind::ENUMERATION_CONSTANT:
      case SymbolKind::LABEL:
        // TODO create values
        break;
      case SymbolKind::TYPEDEF:
        this->ordinary_table.emplace(token.getValue(), std::make_unique<TypedefSymbol>());
    }
  }
}
const ISymbol *SymbolTable::lookupInner(const Token &token, SymbolKind symbol_kind, const std::string &name_space) const {
  try {
    switch (symbol_kind) {
      case SymbolKind::OBJECT:
      case SymbolKind::FUNCTION:
      case SymbolKind::TYPEDEF:
      case SymbolKind::ENUMERATION_CONSTANT: {
        const ISymbol *symbol = ordinary_table.at(token.getValue()).get();
        if (symbol->getKind() != symbol_kind) return nullptr;
        else return symbol;
      }
      case SymbolKind::TAG:return tag_table.at(token.getValue()).get();
      case SymbolKind::LABEL:return label_table.at(token.getValue()).get();
      case SymbolKind::MEMBER:return member_tables.at(name_space).at(token.getValue()).get();
    }
  } catch (const std::out_of_range &) {
    if (father) {
      return father->lookupInner(token, symbol_kind, name_space);
    } else {
      return nullptr;
    }
  }
}
bool SymbolTable::isTypedef(const Token &token) {
  const ISymbol *symbol = lookupInner(token, SymbolKind::TYPEDEF);
  return symbol != nullptr;
}
SymbolTable *SymbolTables::createTable(ScopeKind kind, SymbolTable *father) {
  tables.emplace_back(kind, father, module);
  return &tables.back();
}
