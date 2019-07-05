#include <sema/types.h>
Type::Type(std::set<TypeQuailifier> quailifiers) : mQquailifiers(std::move(quailifiers)) {}
bool Type::canCast(Type *type) {
  return this == type;
}
ObjectType::ObjectType(std::set<TypeQuailifier> quailifiers) : Type(std::move(quailifiers)) {}
IntegerType::IntegerType(std::set<TypeQuailifier> quailifiers, bool bSigned, IntegerType::Kind kind)
    : mSigned(bSigned), mKind(kind), ObjectType(std::move(quailifiers)) {
  // TODO decide integer type size by platform info
  switch (kind) {
    case Kind::kChar: mSizeInBits = 8;
      break;
    case Kind::kShortInt: mSizeInBits = 16;
      break;
    case Kind::kInt:mSizeInBits = 32;
      break;
    case Kind::kLongInt:
    case Kind::kLongLongInt: mSizeInBits = 64;
      break;
  }
}
IntegerType *IntegerType::getIntegerType(bool bSigned, bool bConst, bool bVolatile, IntegerType::Kind kind) {
  auto &ptr =
      sTypes[static_cast<int>(bSigned)][static_cast<int>(bConst)][static_cast<int>(bVolatile)][static_cast<int>(kind)];
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
unsigned int IntegerType::getSizeInBits() const {
  return mSizeInBits;
}
bool IntegerType::canCast(Type *type) {
  return dynamic_cast<IntegerType*>(type) || dynamic_cast<FloatingType*>(type);
}
std::unique_ptr<IntegerType>
    IntegerType::sTypes[2]/*signed*/[2]/*const*/[2]/*volatile*/[static_cast<int>(Kind::kLongLongInt)]/*kind*/;
FloatingType *FloatingType::getFloatingType(bool bConst, bool bVolatile, FloatingType::Kind kind) {
  auto &ptr =
      sTypes[static_cast<int>(bConst)][static_cast<int>(bVolatile)][static_cast<int>(kind)];
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
FloatingType::FloatingType(std::set<TypeQuailifier> quailifiers, FloatingType::Kind kind)
    : mKind(kind), ObjectType(std::move(quailifiers)) {
  switch (kind) {
    case Kind::kFloat:mSizeInBits = 32;
      break;
    case Kind::kDouble:mSizeInBits = 64;
      break;
    case Kind::kLongDouble:mSizeInBits = 128;
      break;
  }
}
bool FloatingType::canCast(Type *type) {
  return dynamic_cast<IntegerType*>(type) || dynamic_cast<FloatingType*>(type);
}
std::unique_ptr<FloatingType>
    FloatingType::sTypes[2]/*const*/[2]/*volatile*/[static_cast<int>(Kind::kLongDouble)]/*kind*/;

FunctionType::FunctionType(std::set<TypeQuailifier> quailifiers,
                           Type *returnType,
                           std::vector<ObjectType *> &&parameters)
    : mReturnType(returnType), mParameters(parameters), Type(std::move(quailifiers)) {}
Type * FunctionType::getReturnType() const {
  return mReturnType;
}
const std::vector<ObjectType *> &FunctionType::getParameters() const {
  return mParameters;
}
ArrayType::ArrayType(std::set<TypeQuailifier> quailifiers, ObjectType *elementType)
    : PointerType(std::move(quailifiers), elementType) {}
ArrayType::ArrayType(std::set<TypeQuailifier> quailifiers, ObjectType *elementType, unsigned int size)
    : mSize(size), PointerType(std::move(quailifiers), elementType) {}
bool ArrayType::complete() {
  return mSize > 0;
}
void ArrayType::setSize(unsigned int size) {
  mSize = size;
}
PointerType::PointerType(std::set<TypeQuailifier> quailifiers, Type *referencedType)
    : mReferencedType(referencedType), ObjectType(std::move(quailifiers)) {}
Type *PointerType::getReferencedType() const {
  return mReferencedType;
}
