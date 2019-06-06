#ifndef MYCCPILER_SYMBOL_TABLES_H
#define MYCCPILER_SYMBOL_TABLES_H

#include <llvm/IR/Value.h>
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
  virtual SymbolKind getKind() const = 0;
  virtual Linkage getLinkage() const {
    return Linkage::kNone;
  }
  virtual bool operator==(SymbolKind kind) const {
    return getKind() == kind;
  };
  virtual bool operator!=(SymbolKind kind) const {
    return !operator==(kind);
  };
};

inline bool operator==(SymbolKind kind, const ISymbol *symbol) {
  return symbol->getKind() == kind;
};
inline bool operator!=(SymbolKind kind, const ISymbol *symbol) {
  return !(kind == symbol);
};

class ObjectSymbol : public ISymbol {
 public:
  ObjectSymbol(llvm::Value *value, Linkage linkage) : value(value), linkage(linkage) {}
  SymbolKind getKind() const override {
    return SymbolKind::OBJECT;
  }
 private:
  llvm::Value *value;
  Linkage linkage;
};

class FunctionSymbol : public ISymbol {
 public:
  FunctionSymbol(llvm::Function *function, Linkage linkage) : function(function), linkage(linkage) {}
  SymbolKind getKind() const override {
    return SymbolKind::FUNCTION;
  }
 private:
  llvm::Function *function;
  Linkage linkage;
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
  SymbolTable(ScopeKind kind, SymbolTable *father) : kind(kind), father(father) {}
  ScopeKind getKind() {
    return kind;
  }
  const ISymbol *lookup(const Token &token) const {
    try {
      return std::map<std::string, std::unique_ptr<ISymbol>>::at(token.getValue()).get();
    } catch (const std::out_of_range &) {
      if (father) {
        return father->lookup(token);
      } else {
        throw SemaException(std::string(token.getValue()).append(" not defined"), token);
      }
    }
  }
  const SymbolKind lookupTest(const Token &token) const {
    return isupper(token.getValue()[0]) ? SymbolKind::TYPEDEF : SymbolKind::OBJECT;
  }
  const ISymbol *insert(const Token &token, std::unique_ptr<ISymbol> symbol) {
    SymbolKind kind = symbol->getKind();
    SymbolTable *table = this;
    if (kind == SymbolKind::LABEL) {
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
    try {
      table->at(token.getValue());
      throw SemaException(std::string(token.getValue()).append(" has already defined"), token);
    } catch (const std::out_of_range &) {
      table->emplace(token.getValue(), std::move(symbol));
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
};

class SymbolTables {
 public:
  SymbolTables() = default;
  SymbolTable *createTable(ScopeKind kind, SymbolTable *father) {
    tables.emplace_back(kind, father);
    return &tables.back();
  }
 private:
  std::list<SymbolTable> tables;
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
