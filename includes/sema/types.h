#ifndef MYCCPILER_TYPES_H
#define MYCCPILER_TYPES_H

#include <set>
#include <vector>
#include <memory>
#include "operator.h"
class Type {
 private:
 public:
  Type(std::set<TypeQuailifier> quailifiers) : quailifiers(std::move(quailifiers)) {}
 private:
  std::set<TypeQuailifier> quailifiers;
};

class ObjectType : public Type {
 public:
  ObjectType(std::set<TypeQuailifier> quailifiers) : Type(std::move(quailifiers)) {}
  virtual bool complete() {
    return true;
  }
};

class IntegerType : public ObjectType {
 public:
  enum class Kind {
    kChar = 1,
    kShortInt,
    kInt,
    kLongInt,
    kLongLongInt,
  };
  IntegerType(std::set<TypeQuailifier> quailifiers, bool bSigned, Kind kind)
      : mSigned(bSigned), mKind(kind), ObjectType(std::move(quailifiers)) {}
  static IntegerType *getIntegerType(bool bSigned, bool bConst, bool bVolatile, Kind kind) {
    auto &ptr =
        types[static_cast<int>(bSigned)][static_cast<int>(bConst)][static_cast<int>(bVolatile)][static_cast<int>(kind)];
    if (!ptr) {
      std::set<TypeQuailifier> set;
      if (bConst) {
        set.emplace(TypeQuailifier::kCONST);
      }
      if (bVolatile) {
        set.emplace(TypeQuailifier::kVOLATILE);
      }
      ptr = std::make_unique<IntegerType>(std::move(set), bSigned, kind);
    }
    return ptr.get();
  }
 private:
  bool mSigned;
  Kind mKind;
  static std::unique_ptr<IntegerType>
      types[2]/*signed*/[2]/*const*/[2]/*volatile*/[static_cast<int>(Kind::kLongLongInt)]/*kind*/;
};

class FloatingType : public ObjectType {
 public:
  enum class Kind {
    kFloat = 1,
    kDouble,
    kLongDouble,
  };
  static FloatingType *getFloatingType(bool bConst, bool bVolatile, Kind kind) {
    auto &ptr =
        types[static_cast<int>(bConst)][static_cast<int>(bVolatile)][static_cast<int>(kind)];
    if (!ptr) {
      std::set<TypeQuailifier> set;
      if (bConst) {
        set.emplace(TypeQuailifier::kCONST);
      }
      if (bVolatile) {
        set.emplace(TypeQuailifier::kVOLATILE);
      }
      ptr = std::make_unique<FloatingType>(std::move(set), kind);
    }
    return ptr.get();
  }
  FloatingType(std::set<TypeQuailifier> quailifiers, Kind kind_) : kind_(kind_), ObjectType(std::move(quailifiers)) {}
 private:
  static std::unique_ptr<FloatingType>
      types[2]/*const*/[2]/*volatile*/[static_cast<int>(Kind::kLongDouble)]/*kind*/;
  Kind kind_;
};

class VoidType : public ObjectType {
 public:
  VoidType() : ObjectType(std::set<TypeQuailifier>()) {}
  bool complete() override {
    return false;
  }
};

class FunctionType : public Type {
 public:
  FunctionType(std::set<TypeQuailifier> quailifiers, ObjectType *returnType, std::vector<ObjectType *> &&parameters)
      : mReturnType(returnType), mParameters(parameters), Type(std::move(quailifiers)) {}
  ObjectType *getReturnType() const {
    return mReturnType;
  }
  const std::vector<ObjectType *> &getParameters() const {
    return mParameters;
  }
 private:
  ObjectType *mReturnType;
  std::vector<ObjectType *> mParameters;
};

//TODO
class EnumerationType : public Type {

};

class ArrayType : public ObjectType {
 public:
  ArrayType(std::set<TypeQuailifier> quailifiers, ObjectType *elementType)
      : mElementType(elementType), ObjectType(std::move(quailifiers)) {}
  ArrayType(std::set<TypeQuailifier> quailifiers, ObjectType *elementType, unsigned int size)
      : mElementType(elementType), mSize(size), ObjectType(std::move(quailifiers)) {}
  bool complete() override {
    return mSize > 0;
  }
  void setSize(unsigned int size) {
    mSize = size;
  }
 private:
  ObjectType *mElementType;
  //TODO is int enough?
  unsigned int mSize = 0;
};

//TODO
class StructType : public Type {

};

//TODO
class UnionType : public Type {

};

class PointerType : public ObjectType {
 public:
  PointerType(std::set<TypeQuailifier> quailifiers, Type *referencedType)
      : referencedType_(referencedType), ObjectType(std::move(quailifiers)) {}
 private:
  Type *referencedType_;
};
#endif //MYCCPILER_TYPES_H
