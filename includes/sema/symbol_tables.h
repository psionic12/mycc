#include <utility>

#ifndef MYCCPILER_SYMBOL_TABLES_H
#define MYCCPILER_SYMBOL_TABLES_H
#include <map>
#include <list>
#include <tokens/token.h>
#include <set>
#include <memory>
#include "operator.h"
class Type;

class ObjectType;

class IntegerType;

class FloatingType;

class VoidType;

class FunctionType;

class PointerType;

class ArrayType;

class CompoundType;

class StructType;

class UnionType;

class EnumerationType;

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
  ISymbol(std::pair<const Token &, const Token &> involvedTokens) : mInvolvedTokens(std::move(involvedTokens)) {}
  friend class SymbolTable;
  virtual SymbolKind getKind() const = 0;
  virtual bool operator==(SymbolKind kind) const {
    return getKind() == kind;
  };
  virtual bool operator!=(SymbolKind kind) const {
    return !operator==(kind);
  };
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
  ObjectSymbol(std::pair<const Token &, const Token &> involvedTokens,
                 ObjectType *type,
                 std::set<TypeQualifier> qualifiers)
      : ISymbol(involvedTokens), mObjectType(type), mQualifiers(std::move(qualifiers)) {}
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
  TagSymbol(std::pair<const Token &, const Token &> involvedTokens,
              std::unique_ptr<CompoundType> &&compoundType)
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
  TypedefSymbol(std::pair<const Token &, const Token &> involvedTokens, Type *mType)
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
  EnumConstSymbol(std::pair<const Token &, const Token &> involvedTokens,
                    EnumerationType *mEnumType) : ISymbol(involvedTokens), mEnumType(mEnumType) {}
  EnumerationType *getType() const {
    return mEnumType;
  }
 private:
  EnumerationType *mEnumType;
};

class SymbolTable : public std::map<std::string, std::unique_ptr<ISymbol>> {
 public:
  SymbolTable(ScopeKind kind, SymbolTable *father)
      : scope_kind(kind), father(father) {}
  ISymbol *lookup(const Token &token);
  ISymbol *insert(const Token &token, std::unique_ptr<ISymbol> &&symbol);
  ISymbol *insert(std::unique_ptr<ISymbol> &&symbol);
  // TODO move this to parser
  bool isTypedef(const Token &token);
  void setFather(SymbolTable *father);
  SymbolTable *getFather() const;
 private:
  ScopeKind scope_kind;
  SymbolTable *father;
  int anonymousId = 0;
};

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
