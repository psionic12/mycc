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

class EnumerationMemberType;

class QualifiedType {
 private:
  const Type *mType;
  std::set<TypeQualifier> mQualifiers;
 public:
  QualifiedType() = default;
  const Type *getType() const;
  const std::set<TypeQualifier> &getQualifiers() const;
  const bool contains(TypeQualifier qualifier) const;
  const bool isSub(const std::set<TypeQualifier>& set) const;
  void addQualifier(TypeQualifier typeQualifier);
  void addQualifiers(const std::set<TypeQualifier> &typeQualifers);
  bool isConst() const;
  bool isVolatile() const;
  QualifiedType(const Type *type, std::set<TypeQualifier> qualifiers);
  bool operator==(const QualifiedType &qualifiedType) const;
  bool operator!=(const QualifiedType &qualifiedType) const;
  bool compatible(const QualifiedType& qualifiedType) const;
};
#endif //MYCCPILER_QUALIFIEDTYPE_H
