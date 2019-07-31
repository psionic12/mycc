#include <utility>

#ifndef MYCCPILER_SYMBOL_TABLES_H
#define MYCCPILER_SYMBOL_TABLES_H
#include <map>
#include <list>
#include <tokens/token.h>
#include <set>
#include <memory>
#include <llvm/IR/Value.h>
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
  FUNCTION,
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
  void setLinkage(Linkage linkage);
  Linkage getLinkage() const;
  std::pair<const Token &, const Token &> mInvolvedTokens;
  friend class FileTable;
  friend class BlockTable;
  friend class ProtoTable;
 private:
  Linkage linkage = Linkage::kNone;
  llvm::Value *mValue;
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
               std::pair<const Token &, const Token &> involvedTokens)
      : ISymbol(involvedTokens), mQulifiedType(qualifiedType) {
  }
  SymbolKind getKind() const override {
    return SymbolKind::OBJECT;
  }
  const ObjectType *getType() const {
    return mQulifiedType.getType();
  }
  const std::set<TypeQualifier> &getQualifiers() const {
    return mQulifiedType.getQualifiers();
  }
 private:
  QualifiedType mQulifiedType;
};

class FunctionSymbol : public ISymbol {
 public:
  FunctionSymbol(FunctionType *type, std::pair<const Token &, const Token &> involvedTokens)
      : mFunctionType(type), ISymbol(involvedTokens) {}
  SymbolKind getKind() const override {
    return SymbolKind::FUNCTION;
  }
  const FunctionType *getType() const {
    return mFunctionType;
  }
 private:
  FunctionType *mFunctionType;
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
  TypedefSymbol(Type *mType, std::pair<const Token &, const Token &> involvedTokens)
      : ISymbol(involvedTokens), mType(mType) {}
  // used in parser, just indicate the identifier is a typedef, the represent type will set in sema phrase
  TypedefSymbol(std::pair<const Token &, const Token &> involvedTokens) : ISymbol(involvedTokens), mType(nullptr) {}
  Type *getMType() const {
    return mType;
  }
  void setMType(Type *mType) {
    TypedefSymbol::mType = mType;
  }
 private:
  Type *mType;
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
  SymbolTable(ScopeKind kind, llvm::Module &module, std::string name)
      : scope_kind(kind), mModule(module), mName(std::move(name)) {}
  ISymbol *lookup(const Token &token);
  ISymbol *insert(const Token &token, std::unique_ptr<ISymbol> &&symbol);
  ISymbol *insert(std::unique_ptr<ISymbol> &&symbol);
  // TODO move this to parser
  bool isTypedef(const Token &token);
  void setFather(SymbolTable *father);
  SymbolTable *getFather() const;
  ScopeKind getScopeKind() const;
 protected:
  virtual void setValue(const Token &token, ISymbol *symbol);
  ScopeKind scope_kind;
  SymbolTable *father;
  int anonymousId = 0;
  llvm::Module &mModule;
  std::string mName;

};

class FileTable : public SymbolTable {
 public:
  FileTable(llvm::Module &module);
 private:
  void setValue(const Token &token, ISymbol *symbol) override;
};

class BlockTable : public SymbolTable {
 public:
  BlockTable(llvm::Module &module, bool function, std::string name, llvm::BasicBlock *basicBlock);
 private:
  void setValue(const Token &token, ISymbol *symbol) override;
  llvm::BasicBlock *mBasicBlock;
};

class ProtoTable : public SymbolTable {
 public:
  ProtoTable(llvm::Module &module, std::string name, llvm::BasicBlock *basicBlock);
 private:
  void setValue(const Token &token, ISymbol *symbol) override;
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
