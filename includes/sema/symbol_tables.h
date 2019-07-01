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
  ObjectSymbol(std::unique_ptr<Type> &&type, bool is_const = false, bool is_volatile = false)
      : type_(std::move(type)), const_(is_const), volatile_(is_volatile) {}
  SymbolKind getKind() const override {
    return SymbolKind::OBJECT;
  }
  Type *getType() {
    return type_.get();
  }
 private:
  std::unique_ptr<Type> type_;
  bool const_;
  bool volatile_;
};

class FunctionSymbol : public ISymbol {
 public:
  FunctionSymbol(std::unique_ptr<FunctionType> &&type) : type_(std::move(type)) {}
  SymbolKind getKind() const override {
    return SymbolKind::FUNCTION;
  }
  FunctionType *getType() {
    return type_.get();
  }
 private:
  std::unique_ptr<FunctionType> type_;

};

class TagSymbol : public ISymbol {
 public:
  SymbolKind getKind() const override {
    return SymbolKind::TAG;
  }
};

class MemberSymbol : public ISymbol {
 public:
  SymbolKind getKind() const override {
    return SymbolKind::MEMBER;
  }
};

class TypedefSymbol : public ISymbol {
 public:
  SymbolKind getKind() const override {
    return SymbolKind::TYPEDEF;
  }
};

class LabelSymbol : public ISymbol {
 public:
  SymbolKind getKind() const override {
    return SymbolKind::LABEL;
  }
};

class EnumConstSymbol : public ISymbol {
 public:
  SymbolKind getKind() const override {
    return SymbolKind::ENUMERATION_CONSTANT;
  }
  EnumConstSymbol(EnumerationType *type_) : type_(type_) {}
  EnumerationType *getType() const {
    return type_;
  }
 private:
  EnumerationType *type_;
};

class SymbolTable : std::map<std::string, std::unique_ptr<ISymbol>> {
 public:
  SymbolTable(ScopeKind kind, SymbolTable *father, llvm::Module &module)
      : scope_kind(kind), father(father), module(module) {}
  ISymbol *lookup(const Token &token);
  ISymbol *insert(const Token &token, std::unique_ptr<ISymbol> &&symbol);

  // TODO move this to parser
  bool isTypedef(const Token &token);
 private:
  ScopeKind scope_kind;
  SymbolTable *father;
  llvm::Module &module;
};

class SymbolTables {
 public:
  SymbolTables(llvm::Module &module) : module(module) {

  }
  SymbolTable *createTable(ScopeKind kind, SymbolTable *father);
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
