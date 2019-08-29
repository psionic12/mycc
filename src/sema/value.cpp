#include <sema/value.h>
Value::Value(QualifiedType qualifiedType, bool lvalue, llvm::Value *value)
    : qualifiedType(std::move(qualifiedType)), lvalue(lvalue), mValue(value) {}
llvm::Value *Value::getValue() const {
  if (qualifiedType.getType()->getLLVMType() == mValue->getType()) {
    return mValue;
  } else if (auto *pointerType = llvm::dyn_cast<llvm::PointerType>(mValue->getType())) {
    if (pointerType->getElementType() == qualifiedType.getType()->getLLVMType()) {
      return AST::getBuilder().CreateLoad(mValue, qualifiedType.isVolatile());
    }
  }
  throw std::runtime_error("WTF: type and value do not match");
}
llvm::Value *Value::getPtr() const {
  if (lvalue) {
    return mValue;
  } else {
    throw std::runtime_error("WTF: get addr for rvalue");
  }
}
bool Value::modifiable() const {
  if (lvalue) {
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
