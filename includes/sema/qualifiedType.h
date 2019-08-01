#ifndef MYCCPILER_QUALIFIEDTYPE_H
#define MYCCPILER_QUALIFIEDTYPE_H

#include "operator.h"
#include <set>

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

class QualifiedType {
 private:
  const Type *mType;
  std::set<TypeQualifier> mQualifiers;
 public:
  QualifiedType() = default;
  const Type *getType() const;
  const std::set<TypeQualifier> &getQualifiers() const;
  QualifiedType(const Type *type, std::set<TypeQualifier> qualifiers);
  bool operator==(const QualifiedType &qualifiedType);
};
QualifiedType::QualifiedType(const Type *type, std::set<TypeQualifier> qualifiers)
    : mType(type), mQualifiers(move(qualifiers)) {}
const Type *QualifiedType::getType() const {
  return mType;
}
const std::set<TypeQualifier> &QualifiedType::getQualifiers() const {
  return mQualifiers;
}
bool QualifiedType::operator==(const QualifiedType &qualifiedType) {
  return mType == qualifiedType.mType && mQualifiers == qualifiedType.mQualifiers;
}
#endif //MYCCPILER_QUALIFIEDTYPE_H
