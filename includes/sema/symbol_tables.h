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
  ObjectSymbol(ObjectType *type, std::set<TypeQualifier> qualifiers)
      : mObjectType(type), mQualifiers(std::move(qualifiers)) {}
  SymbolKind getKind() const override {
    return SymbolKind::OBJECT;
  }
  QualifiedType getQualifiedType() {
    return QualifiedType(mObjectType, mQualifiers);
  }
 private:
  std::set<TypeQualifier> mQualifiers;
  ObjectType *mObjectType;
};

class FunctionSymbol : public ISymbol {
 public:
  FunctionSymbol(std::unique_ptr<FunctionType> &&type)
      : mFunctionType(std::move(type)), mPointer(std::make_unique<PointerType>(mFunctionType.get())) {}
  SymbolKind getKind() const override {
    return SymbolKind::FUNCTION;
  }
  QualifiedType convertToPointer() {
    return QualifiedType(mPointer.get(), std::set<TypeQualifier>{TypeQualifier::kNone});
  }
 private:
  std::unique_ptr<FunctionType> mFunctionType;
  std::unique_ptr<PointerType> mPointer; // this is used for conversion from function type to pointer to function type
};

class TagSymbol : public ISymbol {
 public:
  TagSymbol(std::unique_ptr<CompoundType> &&mCompoundType) : mCompoundType(std::move(mCompoundType)) {}
  SymbolKind getKind() const override {
    return SymbolKind::TAG;
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
  TypedefSymbol(Type *mType, const std::set<TypeQualifier> &mQualifiers) : mQualifiers(mQualifiers), mType(mType) {}
  QualifiedType convertToQualifiedType() {
    if (auto *p = dynamic_cast<FunctionType *>(mType)) {

    } else if (auto *p = dynamic_cast<ArrayType *>(mType)) {

    } else {
      return QualifiedType(mType, mQualifiers);
    }
  }
 private:
  std::set<TypeQualifier> mQualifiers;
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
  EnumConstSymbol(QualifiedType mType) : mType(std::move(mType)) {}
  const QualifiedType &getType() const {
    return mType;
  }
 private:
  QualifiedType mType;
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
