#ifndef MYCCPILER_TYPES_H
#define MYCCPILER_TYPES_H

#include <set>
#include <vector>
#include <memory>
#include "operator.h"
class Type {
 private:
 public:
  Type(std::set<TypeQuailifier> quailifiers);
  virtual ~Type() = default;
 private:
  std::set<TypeQuailifier> mQquailifiers;
};

class ObjectType : public Type {
 public:
  ObjectType(std::set<TypeQuailifier> quailifiers);
  virtual bool complete();
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
  IntegerType(std::set<TypeQuailifier> quailifiers, bool bSigned, Kind kind);
  static IntegerType *getIntegerType(bool bSigned, bool bConst, bool bVolatile, Kind kind);
  unsigned int getSizeInBits() const;
 private:
  bool mSigned;
  Kind mKind;
  unsigned int mSizeInBits;
  static std::unique_ptr<IntegerType>
      sTypes[2]/*signed*/[2]/*const*/[2]/*volatile*/[static_cast<int>(Kind::kLongLongInt)]/*kind*/;
};

class FloatingType : public ObjectType {
 public:
  enum class Kind {
    kFloat = 1,
    kDouble,
    kLongDouble,
  };
  static FloatingType *getFloatingType(bool bConst, bool bVolatile, Kind kind);
  FloatingType(std::set<TypeQuailifier> quailifiers, Kind kind);
 private:
  static std::unique_ptr<FloatingType>
      sTypes[2]/*const*/[2]/*volatile*/[static_cast<int>(Kind::kLongDouble)]/*kind*/;
  Kind mKind;
  unsigned int mSizeInBits;
};

class VoidType : public ObjectType {
 public:
  VoidType();
  bool complete() override;
};

class FunctionType : public Type {
 public:
  FunctionType(std::set<TypeQuailifier> quailifiers, ObjectType *returnType, std::vector<ObjectType *> &&parameters);
  ObjectType *getReturnType() const;
  const std::vector<ObjectType *> &getParameters() const;
 private:
  ObjectType *mReturnType;
  std::vector<ObjectType *> mParameters;
};

//TODO
class EnumerationType : public Type {

};

class ArrayType : public ObjectType {
 public:
  ArrayType(std::set<TypeQuailifier> quailifiers, ObjectType *elementType);
  ArrayType(std::set<TypeQuailifier> quailifiers, ObjectType *elementType, unsigned int size);
  bool complete() override;
  void setSize(unsigned int size);
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
  PointerType(std::set<TypeQuailifier> quailifiers, Type *referencedType);
 private:
  Type *referencedType_;
};
#endif //MYCCPILER_TYPES_H
