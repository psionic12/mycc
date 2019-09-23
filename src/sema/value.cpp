#include <sema/value.h>
#include <sema/ast.h>
Value::Value(QualifiedType qualifiedType, bool lvalue, llvm::Value *value)
    : qualifiedType(std::move(qualifiedType)), mLValue(lvalue), mValue(value) {
}
llvm::Value *Value::getValue() {
  if (isLValue()) {
    return AST::getBuilder().CreateLoad(mValue, isVolatile());
  } else {
    return mValue;
  }
}
llvm::Value *Value::getPtr() {
  if (isLValue()) {
    return mValue;
  } else {
    throw std::runtime_error("WTF: get addr for rvalue");
  }
}
bool Value::modifiable() {
  if (mLValue) {
    if (dynamic_cast<ArrayType *>(qualifiedType.getType())) {
      return false;
    } else if (!qualifiedType.getType()->complete()) {
      return false;
    } else if (qualifiedType.isConst()) {
      return false;
    } else if (auto *type = dynamic_cast<CompoundType *>(qualifiedType.getType())) {
      for (auto &pair : type->mTable) {
        auto *symbol = pair.second;
        if (auto *obj = dynamic_cast<ObjectSymbol *>(symbol)) {
          if (obj->getQualifiedType().isConst()) {
            return false;
          }
        }
      }
      return true;
    } else {
      return true;
    }
  } else {
    return false;
  }
}
Type * Value::getType() {
  return qualifiedType.getType();
}
const std::set<TypeQualifier> &Value::getQualifiers() const {
  return qualifiedType.getQualifiers();
}
bool Value::isConst() const {
  return qualifiedType.isConst();
}
bool Value::isVolatile() const {
  return qualifiedType.isVolatile();
}
llvm::Constant *Value::isConatant() {
  if (mLValue) {
    throw std::runtime_error("WTF: constant llvm value is a lvalue");
  }
  return llvm::dyn_cast<llvm::Constant>(mValue);
}
llvm::GlobalVariable *Value::isGlobalVariable() {
  if (!mLValue) {
    throw std::runtime_error("WTF: global variable llvm value is not a lvalue");
  }
  return llvm::dyn_cast<llvm::GlobalVariable>(mValue);
}
bool Value::isLValue() const {
  return mLValue;
}
QualifiedType &Value::getQualifiedType() {
  return qualifiedType;
}
