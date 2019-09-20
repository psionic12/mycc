#include <sema/value.h>
#include <sema/ast.h>
Value::Value(QualifiedType qualifiedType, bool lvalue, llvm::Value *value)
    : qualifiedType(std::move(qualifiedType)), mLValue(lvalue), mValue(value) {
}
llvm::Value *Value::getValue() const {
  if (isLValue()) {
    return AST::getBuilder().CreateLoad(mValue, isVolatile());
  } else {
    return mValue;
  }
}
llvm::Value *Value::getPtr() const {
  if (isLValue()) {
    return mValue;
  } else {
    throw std::runtime_error("WTF: get addr for rvalue");
  }
}
bool Value::modifiable() const {
  if (mLValue) {
    if (dynamic_cast<const ArrayType *>(qualifiedType.getType())) {
      return false;
    } else if (!qualifiedType.getType()->complete()) {
      return false;
    } else if (qualifiedType.isConst()) {
      return false;
    } else if (const auto *type = dynamic_cast<const CompoundType *>(qualifiedType.getType())) {
      for (const auto &pair : type->mTable) {
        const auto *symbol = pair.second;
        if (const auto *obj = dynamic_cast<const ObjectSymbol *>(symbol)) {
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
const Type *Value::getType() const {
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
llvm::Constant *Value::isConatant() const {
  if (mLValue) {
    throw std::runtime_error("WTF: constant llvm value is a lvalue");
  }
  return llvm::dyn_cast<llvm::Constant>(mValue);
}
llvm::GlobalVariable *Value::isGlobalVariable() const {
  if (!mLValue) {
    throw std::runtime_error("WTF: global variable llvm value is not a lvalue");
  }
  return llvm::dyn_cast<llvm::GlobalVariable>(mValue);
}
bool Value::isLValue() const {
  return mLValue;
}
const QualifiedType &Value::getQualifiedType() const {
  return qualifiedType;
}
