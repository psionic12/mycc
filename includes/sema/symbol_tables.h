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
  ObjectSymbol(llvm::Value *value, bool is_const = false, bool is_volatile = false)
      : value(value), is_const(is_const), is_volatile(is_volatile) {}
  SymbolKind getKind() const override {
    return SymbolKind::OBJECT;
  }
  llvm::Value *getValue() const {
    return value;
  }
 private:
  llvm::Value *value;
  bool is_const;
  bool is_volatile;
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

class SymbolTable {
 public:
  SymbolTable(ScopeKind kind, SymbolTable *father, llvm::Module &module)
      : scope_kind(kind), father(father), module(module) {}
  const ISymbol *lookup(const Token &token, SymbolKind symbol_kind, const std::string &name_space = "") const;
  void insert(const Token &token, SymbolKind symbol_kind, llvm::Type *type, StorageSpecifier storage_specifier);

  void insertObject(const Token &token, llvm::Type * type, StorageSpecifier storage_specifier);
  void insertFunction(const Token &token, llvm::FunctionType * type, StorageSpecifier storage_specifier);
  void insertTag(const Token &token, llvm::StructType* type);
  void insertMember(const Token &token);
  void insertTypedef(const Token &token, llvm::Type* type);
  void insertLabel(const Token &token, llvm::BasicBlock* basicBlock);
  void insertEnumConst(const Token &token);

  bool isTypedef(const Token &token);
 private:
  ScopeKind scope_kind;
  SymbolTable *father;
  // 6.2.3 Name spaces of identifiers
  typedef std::map<std::string, std::unique_ptr<ISymbol>> SymbolTableImpl;
  SymbolTableImpl ordinary_table;
  SymbolTableImpl label_table;
  SymbolTableImpl tag_table;
  std::map<std::string, SymbolTableImpl> member_tables;
  llvm::Module &module;
  const ISymbol *lookupInner(const Token &token, SymbolKind symbol_kind, const std::string &name_space = "") const;
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
