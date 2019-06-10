#ifndef MYCCPILER_SYMBOL_TABLES_H
#define MYCCPILER_SYMBOL_TABLES_H

#include <llvm/IR/Value.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <map>
#include <list>
#include <tokens/token.h>
#include "ast.h"

enum class Linkage {
  kExternal,
  kInternal,
  kNone,
};

enum class SymbolKind {
  OBJECT,
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

class ISymbol {
 public:
  friend class SymbolTable;
  virtual SymbolKind getKind() const = 0;
  virtual bool operator==(SymbolKind kind) const {
    return getKind() == kind;
  };
  virtual bool operator!=(SymbolKind kind) const {
    return !operator==(kind);
  };
 private:
  Linkage linkage = Linkage::kNone;
};

inline bool operator==(SymbolKind kind, const ISymbol *symbol) {
  return symbol->getKind() == kind;
};
inline bool operator!=(SymbolKind kind, const ISymbol *symbol) {
  return !(kind == symbol);
};

class ObjectSymbol : public ISymbol {
 public:
  ObjectSymbol(llvm::Value *value) : value(value) {}
  SymbolKind getKind() const override {
    return SymbolKind::OBJECT;
  }
 private:
  llvm::Value *value;
 public:
  llvm::Value *getValue() const {
    return value;
  }
};

class FunctionSymbol : public ISymbol {
 public:
  FunctionSymbol(llvm::Function *function) : function(function) {}
  SymbolKind getKind() const override {
    return SymbolKind::FUNCTION;
  }
 private:
  llvm::Function *function;
 public:
  llvm::Function *getFunction() const {
    return function;
  }
};

class TagSymbol : public ISymbol {
 public:
  TagSymbol(llvm::Type *type) : type(type) {}
  SymbolKind getKind() const override {
    return SymbolKind::TAG;
  }
 private:
  llvm::Type *type;
};

class MemberSymbol : public ISymbol {
 public:
  SymbolKind getKind() const override {
    return SymbolKind::MEMBER;
  }
 private:
  llvm::Value *value;
};

class TypedefSymbol : public ISymbol {
 public:
  SymbolKind getKind() const override {
    return SymbolKind::TYPEDEF;
  }
 private:
  llvm::Type *type;
};

class LabelSymbol : public ISymbol {
 public:
  SymbolKind getKind() const override {
    return SymbolKind::LABEL;
  }
 private:
  llvm::BasicBlock *basicBlock;
};

class EnumConstSymbol : public ISymbol {
 public:
  SymbolKind getKind() const override {
    return SymbolKind::ENUMERATION_CONSTANT;
  }
 private:
  llvm::Value *constFP;
};

class SymbolTable : public std::map<std::string, std::unique_ptr<ISymbol>> {
 public:
  SymbolTable(ScopeKind kind, SymbolTable *father, llvm::Module &module) : kind(kind), father(father), module(module) {}
  ScopeKind getKind() {
    return kind;
  }
  const ISymbol *lookup(const Token &token) const {
    const ISymbol *symbol = lookupInner(token);
    if (symbol) {
      return symbol;
    } else {
      throw SemaException(std::string(token.getValue()).append(" not defined"), token);
    }
  }
  const ISymbol *lookupInner(const Token &token) const {
    try {
      return std::map<std::string, std::unique_ptr<ISymbol>>::at(token.getValue()).get();
    } catch (const std::out_of_range &) {
      if (father) {
        return father->lookupInner(token);
      } else {
        return nullptr;
      }
    }
  }
  const SymbolKind lookupTest(const Token &token) const {
    return isupper(token.getValue()[0]) ? SymbolKind::TYPEDEF : SymbolKind::OBJECT;
  }
  ISymbol *insert(const Token &token, SymbolKind symbol_kind, llvm::Type *type, StorageSpecifier storage_specifier) {
    // scope check
    SymbolTable *table = this;
    if (symbol_kind == SymbolKind::LABEL) {
      // insert label to the nearest function scope
      do {
        if (table->getKind() != ScopeKind::FUNCTION) {
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
        || table->getKind() == ScopeKind::FUNCTION_PROTOTYPE
        || (table->getKind() == ScopeKind::BLOCK && symbol_kind == SymbolKind::OBJECT
            && storage_specifier != StorageSpecifier::kEXTERN)) {
      linkage = Linkage::kNone;
    } else if (table->getKind() == ScopeKind::FILE
        && (symbol_kind == SymbolKind::OBJECT || symbol_kind == SymbolKind::FUNCTION)
        && storage_specifier == StorageSpecifier::kSTATIC) {
      linkage = Linkage::kInternal;
    } else if (storage_specifier == StorageSpecifier::kEXTERN || symbol_kind == SymbolKind::FUNCTION
        || (symbol_kind == SymbolKind::OBJECT && table->getKind() == ScopeKind::FILE)) {
      linkage_symbol = lookupInner(token);
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
      auto entry = table->emplace(token.getValue(), std::move(symbol));
      return entry.first->second.get();
    } else {
      switch (symbol_kind) {
        case SymbolKind::OBJECT:
        case SymbolKind::FUNCTION:
        case SymbolKind::TAG:
        case SymbolKind::MEMBER:
        case SymbolKind::TYPEDEF:
        case SymbolKind::LABEL:
        case SymbolKind::ENUMERATION_CONSTANT:
          // TODO create values
          return nullptr;
      }
    }
  }
 private:
  SymbolTable *father;
 public:
  SymbolTable *getFather() {
    return father;
  }
 private:
  ScopeKind kind;
  llvm::Module &module;
};

class SymbolTables {
 public:
  SymbolTables(llvm::Module &module) : module(module) {

  }
  SymbolTable *createTable(ScopeKind kind, SymbolTable *father) {
    tables.emplace_back(kind, father, module);
    return &tables.back();
  }
 private:
  std::list<SymbolTable> tables;
  llvm::Module &module;
};

class SymbolScope {
 public:
  SymbolScope(SymbolTable *&outter, SymbolTable *inner) : outter(outter), outter_valule(outter) {
    outter = inner;
  }
  ~SymbolScope() {
    outter = outter_valule;
  }
 private:
  SymbolTable *&outter;
  SymbolTable *outter_valule;
};

#endif //MYCCPILER_SYMBOL_TABLES_H
