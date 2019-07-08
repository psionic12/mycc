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
  virtual bool compatible(Type *type);
  virtual bool complete();
 private:
  std::set<TypeQuailifier> mQquailifiers;
};

class ObjectType : public Type {
 public:
  ObjectType(std::set<TypeQuailifier> quailifiers);
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
  bool compatible(Type *type) override;
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
  bool compatible(Type *type) override;
 private:
  static std::unique_ptr<FloatingType>
      sTypes[2]/*const*/[2]/*volatile*/[static_cast<int>(Kind::kLongDouble)]/*kind*/;
  Kind mKind;
  unsigned int mSizeInBits;
};

class VoidType : public Type {
 public:
  static VoidType sVoidType;
  bool complete() override;
};

class FunctionType : public Type {
 public:
  FunctionType(std::set<TypeQuailifier> quailifiers, Type *returnType, std::vector<ObjectType *> &&parameters);
  Type *getReturnType() const;
  const std::vector<ObjectType *> &getParameters() const;
 private:
  Type *mReturnType;
  std::vector<ObjectType *> mParameters;
};

class PointerType : public ObjectType {
 public:
  PointerType(std::set<TypeQuailifier> quailifiers, Type *referencedType);
 private:
  Type *mReferencedType;
 public:
  Type *getReferencedType() const;
};

class ArrayType : public PointerType {
 public:
  ArrayType(std::set<TypeQuailifier> quailifiers, ObjectType *elementType);
  ArrayType(std::set<TypeQuailifier> quailifiers, ObjectType *elementType, unsigned int size);
  bool complete() override;
  void setSize(unsigned int size);
 private:
  //TODO is int enough?
  unsigned int mSize = 0;
};

class CompoundType : public ObjectType {
 public:
  CompoundType(std::set<TypeQuailifier> quailifiers,
              std::string tag,
              std::vector<std::pair<std::string, Type *>> members);
  bool isMember(const std::string& name);
  Type* getMember(const std::string& name);
 private:
  std::string mTag;
  std::vector<std::pair<std::string, Type*>> mMembers;
 public:
  const std::string &getTag() const;
};

//TODO
class StructType : public CompoundType {

};

//TODO
class UnionType : public CompoundType {

};


//TODO
class EnumerationType : public CompoundType {
};
#endif //MYCCPILER_TYPES_H

