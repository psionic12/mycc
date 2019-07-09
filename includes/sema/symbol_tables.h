#include <utility>

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
//  FUNCTION,
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
  ObjectSymbol(ObjectType *type, std::set<TypeQualifier> qualifiers)
      : mObjectType(type), mQualifiers(std::move(qualifiers)) {}
  SymbolKind getKind() const override {
    return SymbolKind::OBJECT;
  }
  ObjectType *getType() const {
    return mObjectType;
  }
  const std::set<TypeQualifier> &getQualifiers() const {
    return mQualifiers;
  }
 private:
  ObjectType *mObjectType;
  std::set<TypeQualifier> mQualifiers;
};

//class FunctionSymbol : public ISymbol {
// public:
//  FunctionSymbol(std::unique_ptr<FunctionType> &&type)
//      : mFunctionType(std::move(type)) {}
//  SymbolKind getKind() const override {
//    return SymbolKind::FUNCTION;
//  }
// private:
//  std::unique_ptr<FunctionType> mFunctionType;
//};

class TagSymbol : public ISymbol {
 public:
  TagSymbol(std::unique_ptr<CompoundType> &&mCompoundType) : mCompoundType(std::move(mCompoundType)) {}
  SymbolKind getKind() const override {
    return SymbolKind::TAG;
  }
  CompoundType *getTagType() {
    return mCompoundType.get();
  }
 private:
  std::unique_ptr<CompoundType> mCompoundType;
};

//class MemberSymbol : public ISymbol {
// public:
//  SymbolKind getKind() const override {
//    return SymbolKind::MEMBER;
//  }
//};

class TypedefSymbol : public ISymbol {
 public:
  SymbolKind getKind() const override {
    return SymbolKind::TYPEDEF;
  }
  TypedefSymbol(Type *mType) : mType(mType) {}
 private:
  Type *mType;
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
  EnumConstSymbol(EnumerationType *mEnumType) : mEnumType(mEnumType) {}
  EnumerationType *getType() const {
    return mEnumType;
  }
 private:
  EnumerationType *mEnumType;
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
