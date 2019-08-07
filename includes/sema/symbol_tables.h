#include <utility>

#ifndef MYCCPILER_SYMBOL_TABLES_H
#define MYCCPILER_SYMBOL_TABLES_H
#include <map>
#include <list>
#include <tokens/token.h>
#include <set>
#include <memory>
#include <llvm/IR/Value.h>
#include <llvm/IR/Function.h>
#include "operator.h"
#include "qualifiedType.h"

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
  // standard defined a function scope, but the only purpose of it is used for labels, so I change it name to LABEL
  // and other none label symbols are save in other three tables
      LABEL,
  // standard uses namespace to identify tag members, we just create a new symbol type
      TAG,
  BLOCK,
  FUNCTION_PROTOTYPE,
};

class ISymbol {
 public:
  ISymbol(const Token *token) : mToken(token) {}
  friend class SymbolTable;
  virtual SymbolKind getKind() const = 0;
  virtual bool operator==(SymbolKind kind) const {
    return getKind() == kind;
  };
  virtual bool operator!=(SymbolKind kind) const {
    return !operator==(kind);
  };
  virtual llvm::Value *getValue() = 0;
  void setLinkage(Linkage linkage);
  Linkage getLinkage() const;
  const Token *getToken() const {
    return mToken;
  }
 private:
  Linkage linkage = Linkage::kNone;
  const Token *mToken;
};

inline bool operator==(SymbolKind kind, const ISymbol *symbol) {
  return symbol->getKind() == kind;
};
inline bool operator!=(SymbolKind kind, const ISymbol *symbol) {
  return !(kind == symbol);
};

class ObjectSymbol : public ISymbol {
 public:
  ObjectSymbol(QualifiedType qualifiedType,
               llvm::Value *value,
               const Token *token)
      : ISymbol(token), mQualifiedType(qualifiedType), mValue(value) {
  }
  ObjectSymbol(QualifiedType qualifiedType,
               const Token *token)
      : ISymbol(token), mQualifiedType(qualifiedType), mValue(nullptr) {
  }
  SymbolKind getKind() const override {
    return SymbolKind::OBJECT;
  }
  llvm::Value *getValue() override {
    return mValue;
  }
  void setValue(llvm::Value *value) {
    mValue = value;
  }
  const QualifiedType &getQualifiedType() const {
    return mQualifiedType;
  }
 private:
  QualifiedType mQualifiedType;
  llvm::Value *mValue;
};

class FunctionSymbol : public ISymbol {
 public:
  FunctionSymbol(FunctionType *type, llvm::Function *value, const Token *token)
      : mFunctionType(type), mFunction(value), ISymbol(token) {}
  SymbolKind getKind() const override {
    return SymbolKind::FUNCTION;
  }
  const FunctionType *getType() const {
    return mFunctionType;
  }
  llvm::Function *getValue() override {
    return mFunction;
  }
  void setValue(llvm::Function *value) {
    mFunction = value;
  }
 private:
  FunctionType *mFunctionType;
  llvm::Function *mFunction;
};

class TagSymbol : public ISymbol {
 public:
  TagSymbol(std::unique_ptr<CompoundType> &&compoundType, const Token *token)
      : ISymbol(token), mCompoundType(std::move(compoundType)) {}
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
  TypedefSymbol(QualifiedType qualifiedType, const Token *token)
      : ISymbol(token), mQualifiedType(qualifiedType) {}
  // used in parser, just indicate the identifier is a typedef, the represent type will set in sema phrase
  TypedefSymbol(const Token *token) : ISymbol(token) {}
  QualifiedType getType() const {
    return mQualifiedType;
  }
  void setType(QualifiedType qualifiedType) {
    mQualifiedType = qualifiedType;
  }
 private:
  QualifiedType mQualifiedType;
};

class LabelSymbol : public ISymbol {
 public:
  LabelSymbol(const Token *token) : ISymbol(token) {}
  SymbolKind getKind() const override {
    return SymbolKind::LABEL;
  }
};

class EnumConstSymbol : public ISymbol {
 public:
  EnumConstSymbol(const Token *token, int index) : ISymbol(token), mIndex(index) {}
  SymbolKind getKind() const override {
    return SymbolKind::ENUMERATION_CONSTANT;
  }
  EnumConstSymbol(EnumerationType *mEnumType,
                  const Token *token)
      : ISymbol(token), mEnumType(mEnumType) {}
  EnumerationType *getType() const {
    return mEnumType;
  }
  int getIndex() const {
    return mIndex;
  }
 private:
  EnumerationType *mEnumType;
  int mIndex;
};

class SymbolTable : public std::map<std::string, ISymbol *> {
 public:
  SymbolTable(ScopeKind kind, llvm::BasicBlock *basicBlock = nullptr)
      : mScopeKind(kind), mBasicBlock(basicBlock) {}
  ISymbol *lookup(const Token &token);
  ISymbol *insert(const Token &token, ISymbol *symbol);
  ISymbol *insert(ISymbol *symbol);
  // TODO move this to parser
  bool isTypedef(const Token &token);
  void setFather(SymbolTable *father);
  SymbolTable *getFather() const;
  ScopeKind getScopeKind() const;
  llvm::BasicBlock *getBasicBlock() {
    return mBasicBlock;
  }
 protected:
  ScopeKind mScopeKind;
  SymbolTable *mFather;
  int mAnonymousId = 0;
  llvm::BasicBlock *mBasicBlock;
};
void ISymbol::setLinkage(Linkage linkage) {
  ISymbol::linkage = linkage;
}
Linkage ISymbol::getLinkage() const {
  return linkage;
}

class SymbolTables {
 public:
  SymbolTable *createTable(ScopeKind kind, SymbolTable *father);
  void clear() {
    for (auto &table: tables) {
      table.clear();
    }
  }
 private:
  std::list<SymbolTable> tables;
};

class SymbolScope {
 public:
  SymbolScope(SymbolTable *&outter, SymbolTable *inner) : outter(outter), outter_valule(outter) {
    outter = inner;
    if (!inner->getFather())
      inner->setFather(outter);
  }
  ~SymbolScope() {
    outter = outter_valule;
  }
 private:
  SymbolTable *&outter;
  SymbolTable *outter_valule;
};

#endif //MYCCPILER_SYMBOL_TABLES_H
