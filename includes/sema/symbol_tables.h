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
  // standard defined a function scope, but the only purpose of it is used for tags, so I change it name to TAG
  // and other none tag symbols are save in other three tables
      TAG,
  BLOCK,
  FUNCTION_PROTOTYPE,
};

class ISymbol {
 public:
  ISymbol(std::pair<const Token &, const Token &> involvedTokens) : mInvolvedTokens(std::move(involvedTokens)) {}
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
  std::pair<const Token &, const Token &> mInvolvedTokens;
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
  ObjectSymbol(QualifiedType qualifiedType,
               llvm::Value *value,
               std::pair<const Token &, const Token &> involvedTokens)
      : ISymbol(involvedTokens), mQualifiedType(qualifiedType), mValue(value) {
  }
  SymbolKind getKind() const override {
    return SymbolKind::OBJECT;
  }
  const Type *getType() const {
    return mQualifiedType.getType();
  }
  const std::set<TypeQualifier> &getQualifiers() const {
    return mQualifiedType.getQualifiers();
  }
  llvm::Value *getValue() override {
    return mValue;
  }
 private:
  QualifiedType mQualifiedType;
  llvm::Value *mValue;
};

class FunctionSymbol : public ISymbol {
 public:
  FunctionSymbol(FunctionType *type, llvm::Function *value, std::pair<const Token &, const Token &> involvedTokens)
      : mFunctionType(type), mFunction(value), ISymbol(involvedTokens) {}
  SymbolKind getKind() const override {
    return SymbolKind::FUNCTION;
  }
  const FunctionType *getType() const {
    return mFunctionType;
  }
  llvm::Function *getValue() override {
    return mFunction;
  }
 private:
  FunctionType *mFunctionType;
  llvm::Function *mFunction;
};

class TagSymbol : public ISymbol {
 public:
  TagSymbol(std::unique_ptr<CompoundType> &&compoundType,
            std::pair<const Token &, const Token &> involvedTokens)
      : ISymbol(involvedTokens), mCompoundType(std::move(compoundType)) {}
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
  TypedefSymbol(QualifiedType qualifiedType, std::pair<const Token &, const Token &> involvedTokens)
      : ISymbol(involvedTokens), mQualifiedType(qualifiedType) {}
  // used in parser, just indicate the identifier is a typedef, the represent type will set in sema phrase
  TypedefSymbol(std::pair<const Token &, const Token &> involvedTokens) : ISymbol(involvedTokens) {}
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
  LabelSymbol(const std::pair<const Token &, const Token &> &involvedTokens) : ISymbol(involvedTokens) {}
  SymbolKind getKind() const override {
    return SymbolKind::LABEL;
  }
};

class EnumConstSymbol : public ISymbol {
 public:
  SymbolKind getKind() const override {
    return SymbolKind::ENUMERATION_CONSTANT;
  }
  EnumConstSymbol(EnumerationType *mEnumType,
                  std::pair<const Token &, const Token &> involvedTokens)
      : ISymbol(involvedTokens), mEnumType(mEnumType) {}
  EnumerationType *getType() const {
    return mEnumType;
  }
 private:
  EnumerationType *mEnumType;
};

class SymbolTable : public std::map<std::string, std::unique_ptr<ISymbol>> {
 public:
  SymbolTable(ScopeKind kind, llvm::BasicBlock *basicBlock = nullptr)
      : mScopeKind(kind), mBasicBlock(basicBlock) {}
  ISymbol *lookup(const Token &token);
  ISymbol *insert(const Token &token, std::unique_ptr<ISymbol> &&symbol);
  ISymbol *insert(std::unique_ptr<ISymbol> &&symbol);
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
