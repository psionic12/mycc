#include <sema/types.h>
bool Type::compatible(Type *type) {
  return this == type;
}
bool Type::complete() {
  return true;
}
unsigned int IntegerType::getSizeInBits() const {
  return mSizeInBits;
}
const IntegerType IntegerType::sCharType(8);
const IntegerType IntegerType::sShortIntType(16);
const IntegerType IntegerType::sIntType(32);
const IntegerType IntegerType::sLongIntType(64);
const IntegerType IntegerType::sLongLongIntType(64);
const IntegerType IntegerType::sUnsignedCharType(8);
const IntegerType IntegerType::sUnsignedShortIntType(16);
const IntegerType IntegerType::sUnsignedIntType(32);
const IntegerType IntegerType::sUnsignedLongIntType(64);
const IntegerType IntegerType::sUnsignedLongLongIntType(64);
bool IntegerType::compatible(Type *type) {
  return dynamic_cast<IntegerType *>(type) || dynamic_cast<FloatingType *>(type);
}
IntegerType::IntegerType(unsigned int mSizeInBits) : mSizeInBits(mSizeInBits) {}
bool FloatingType::compatible(Type *type) {
  return dynamic_cast<IntegerType *>(type) || dynamic_cast<FloatingType *>(type);
}
const FloatingType FloatingType::sFloatType(32);
const FloatingType FloatingType::sDoubleType(64);
const FloatingType FloatingType::sLongDoubleType(128);
FloatingType::FloatingType(unsigned int mSizeInBits) : mSizeInBits(mSizeInBits) {}
FunctionType::FunctionType(Type *returnType,
                           std::vector<ObjectType *> &&parameters)
    : mReturnType(returnType), mParameters(parameters) {}
Type *FunctionType::getReturnType() const {
  return mReturnType;
}
const std::vector<ObjectType *> &FunctionType::getParameters() const {
  return mParameters;
}
ArrayType::ArrayType(ObjectType *elementType)
    : mElementType(elementType) {}
ArrayType::ArrayType(ObjectType *elementType, unsigned int size)
    : mSize(size), mElementType(elementType) {}
bool ArrayType::complete() {
  return mSize > 0;
}
void ArrayType::setSize(unsigned int size) {
  mSize = size;
}
PointerType::PointerType(Type *referencedType)
    : mReferencedType(referencedType) {}
Type *PointerType::getReferencedType() const {
  return mReferencedType;
}
bool VoidType::complete() {
  return false;
}
CompoundType::CompoundType(std::string tag,
                           std::vector<std::pair<std::string, Type *>> members)
    : mTag(std::move(tag)), mMembers(std::move(members)) {}
bool CompoundType::isMember(const std::string &name) {
  for (const auto &member : mMembers) {
    if (name == member.first) {
      return true;
    }
  }
  return false;
}
const std::string &CompoundType::getTag() const {
  return mTag;
}
Type *CompoundType::getMember(const std::string &name) {
  for (const auto &member : mMembers) {
    if (name == member.first) {
      return member.second;
    }
  }
  return nullptr;
}
StructType::StructType(const std::string &tag, const std::vector<std::pair<std::string, Type *>> &members)
    : CompoundType(tag, members) {}
UnionType::UnionType(const std::string &tag, const std::vector<std::pair<std::string, Type *>> &members)
    : CompoundType(tag, members) {}
EnumerationType::EnumerationType(const std::string &tag, const std::vector<std::pair<std::string, Type *>> &members)
    : CompoundType(tag, members) {}
QualifiedType::QualifiedType(ObjectType *type, std::set<TypeQualifier> qualifiers)
    : mType(type), mQualifiers(std::move(qualifiers)) {}
Type *QualifiedType::getType() const {
  return mType;
}
const std::set<TypeQualifier> &QualifiedType::getQualifiers() const {
  return mQualifiers;
}
