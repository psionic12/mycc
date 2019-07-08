#ifndef MYCCPILER_TYPES_H
#define MYCCPILER_TYPES_H

#include <set>
#include <vector>
#include <memory>
#include "operator.h"
class Type {
 private:
 public:
  virtual ~Type() = default;
  virtual bool compatible(Type *type);
  virtual bool complete();
};

class ObjectType : public Type {
};

// Types saved as public static members
class IntegerType : public ObjectType {
 public:
  IntegerType(unsigned int mSizeInBits);
  unsigned int getSizeInBits() const;
  bool compatible(Type *type) override;
  static const IntegerType sCharType;
  static const IntegerType sShortIntType;
  static const IntegerType sIntType;
  static const IntegerType sLongIntType;
  static const IntegerType sLongLongIntType;
  static const IntegerType sUnsignedCharType;
  static const IntegerType sUnsignedShortIntType;
  static const IntegerType sUnsignedIntType;
  static const IntegerType sUnsignedLongIntType;
  static const IntegerType sUnsignedLongLongIntType;
 private:
  unsigned int mSizeInBits;
};

// Types saved as public static members
class FloatingType : public ObjectType {
 public:
  FloatingType(unsigned int mSizeInBits);
  bool compatible(Type *type) override;
  static const FloatingType sFloatType;
  static const FloatingType sDoubleType;
  static const FloatingType sLongDoubleType;
 private:
  unsigned int mSizeInBits;
};

// Types saved as public static members
class VoidType : public ObjectType {
 public:
  static VoidType sVoidType;
  bool complete() override;
};

// Types stored in symbol table
class FunctionType : public Type {
 public:
  FunctionType(Type *returnType, std::vector<ObjectType *> &&parameters);
  Type *getReturnType() const;
  const std::vector<ObjectType *> &getParameters() const;
 private:
  Type *mReturnType;
  std::vector<ObjectType *> mParameters;
};

// Types created dynamically
class PointerType : public ObjectType {
 public:
  PointerType(Type *referencedType);
 private:
  Type *mReferencedType;
 public:
  Type *getReferencedType() const;
};

// Types create dynamically
class ArrayType : public Type {
 public:
  ArrayType(ObjectType *elementType);
  ArrayType(ObjectType *elementType, unsigned int size);
  bool complete() override;
  void setSize(unsigned int size);
 private:
  //TODO is int enough?
  unsigned int mSize = 0;
  ObjectType *mElementType;
};

// Types stored in symbol table
class CompoundType : public ObjectType {
 public:
  CompoundType(std::string tag,
               std::vector<std::pair<std::string, Type *>> members);
  bool isMember(const std::string &name);
  Type *getMember(const std::string &name);
 private:
  std::string mTag;
  std::vector<std::pair<std::string, Type *>> mMembers;
 public:
  const std::string &getTag() const;
};

//TODO
class StructType : public CompoundType {
 public:
  StructType(const std::string &tag, const std::vector<std::pair<std::string, Type *>> &members);
};

//TODO
class UnionType : public CompoundType {
 public:
  UnionType(const std::string &tag, const std::vector<std::pair<std::string, Type *>> &members);
};

//TODO
class EnumerationType : public CompoundType {
 public:
  EnumerationType(const std::string &tag, const std::vector<std::pair<std::string, Type *>> &members);
};

class QualifiedType {
 private:
  ObjectType *mType;
  std::set<TypeQualifier> mQualifiers;
 public:
  Type *getType() const;
  const std::set<TypeQualifier> &getQualifiers() const;
  QualifiedType(ObjectType *type, std::set<TypeQualifier> qualifiers);
};
#endif //MYCCPILER_TYPES_H

