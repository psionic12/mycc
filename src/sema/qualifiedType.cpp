#include <sema/types.h>
#include <sema/qualifiedType.h>
QualifiedType::QualifiedType(Type *type, std::set<TypeQualifier> qualifiers)
    : mType(type), mQualifiers(move(qualifiers)) {}
Type * QualifiedType::getType() {
  return mType;
}
const std::set<TypeQualifier> &QualifiedType::getQualifiers() const {
  return mQualifiers;
}
bool QualifiedType::operator==(const QualifiedType &qualifiedType) const {
  return mType == qualifiedType.mType && mQualifiers == qualifiedType.mQualifiers;
}
bool QualifiedType::operator!=(const QualifiedType &qualifiedType) const {
  return !operator==(qualifiedType);
}
void QualifiedType::addQualifier(TypeQualifier typeQualifier) {
  mQualifiers.emplace(typeQualifier);
}
void QualifiedType::addQualifiers(const std::set<TypeQualifier> &typeQualifers) {
  mQualifiers.insert(typeQualifers.begin(), typeQualifers.end());
}
const bool QualifiedType::contains(TypeQualifier qualifier) const {
  return mQualifiers.find(qualifier) != mQualifiers.end();
}
bool QualifiedType::isVolatile() const {
  return contains(TypeQualifier::kVOLATILE);
}
bool QualifiedType::isConst() const {
  return contains(TypeQualifier::kCONST);
}
bool QualifiedType::compatible(QualifiedType &qualifiedType) const {
  return qualifiedType.getType()->compatible(mType) && qualifiedType.getQualifiers() == mQualifiers;
}
const bool QualifiedType::isSub(const std::set<TypeQualifier> &set) const {
  auto i1 = mQualifiers.begin();
  auto i2 = set.begin();
  while (i2 != set.end() && i1 != mQualifiers.end()) {
    if (*i2 != *i1) {
      ++i1;
    } else {
      ++i1;
      ++i2;
    }
  }
  return i2 == set.end();
}
