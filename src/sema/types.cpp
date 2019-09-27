#include <sema/types.h>
#include <llvm/IR/DerivedTypes.h>
#include <sema/ast.h>
#include <sema/qualifiedType.h>
bool Type::compatible(Type *type) {
  return this == type;
}
bool Type::complete() {
  return true;
}
llvm::Value *Type::castTo(Type *toType, llvm::Value *fromValue, const AST *ast) {
  if (toType != this) {
    throw SemaException("cannot cast type", ast->involvedTokens());
  } else {
    return fromValue;
  }

}
unsigned int IntegerType::getSizeInBits() {
  return mSizeInBits;
}

IntegerType IntegerType::sShortIntType(16, true);
IntegerType IntegerType::sIntType(32, true);
IntegerType IntegerType::sLongIntType(64, true);
IntegerType IntegerType::sLongLongIntType(64, true);
IntegerType IntegerType::sUnsignedCharType(8, false);
IntegerType &IntegerType::sCharType(sUnsignedCharType);
IntegerType IntegerType::sUnsignedShortIntType(16, false);
IntegerType IntegerType::sUnsignedIntType(32, false);
IntegerType IntegerType::sUnsignedLongIntType(64, false);
IntegerType IntegerType::sUnsignedLongLongIntType(64, false);
IntegerType IntegerType::sOneBitBoolIntType(1, false);
IntegerType::IntegerType(unsigned int mSizeInBits, bool bSigned) : mSizeInBits(mSizeInBits), mSigned(bSigned) {}
llvm::IntegerType *IntegerType::getLLVMType() {
  return llvm::IntegerType::get(AST::getContext(), mSizeInBits);
}
llvm::APInt IntegerType::getAPInt(uint64_t value) const {
  return llvm::APInt(mSizeInBits, value, mSigned);
}
bool IntegerType::isSigned() const {
  return mSigned;
}

llvm::Value *IntegerType::castTo(Type *toType, llvm::Value *fromValue, const AST *ast) {
  auto &builder = AST::getBuilder();
  auto *module = AST::getModule();
  if (getLLVMType() != fromValue->getType()) {
    throw std::runtime_error("WTF: casting: source type does not match value type");
  }

  auto *fromConstantInt = llvm::dyn_cast<llvm::ConstantInt>(fromValue);

  if (auto *toIntegerType = dynamic_cast<IntegerType *>(toType)) {
    if (fromConstantInt) {
      return llvm::ConstantInt::get(toType->getLLVMType(), fromConstantInt->getSExtValue());
    } else {
      if (toIntegerType->getSizeInBits() > getSizeInBits()) {
        if (toIntegerType->isSigned()) {
          return builder.CreateSExt(fromValue, toType->getLLVMType());
        } else {
          return builder.CreateZExt(fromValue, toType->getLLVMType());
        }
      } else if (toIntegerType->getSizeInBits() == getSizeInBits()) {
        if (toIntegerType->isSigned() != isSigned()) {
          if (toIntegerType->isSigned()) {
            return builder.CreateSExt(fromValue, toType->getLLVMType());
          } else {
            return builder.CreateZExt(fromValue, toType->getLLVMType());
          }
        } else {
          return fromValue;
        }
      } else {
        return builder.CreateTrunc(fromValue, toType->getLLVMType());
      }
    }
  } else if (dynamic_cast<FloatingType * >(toType)) {
    if (fromConstantInt) {
      return llvm::ConstantFP::get(toType->getLLVMType(), fromConstantInt->getSExtValue());
    } else {
      if (isSigned()) {
        return builder.CreateFPToSI(fromValue, toType->getLLVMType());
      } else {
        return builder.CreateFPToUI(fromValue, toType->getLLVMType());
      }
    }
  } else if (auto *pointerType = dynamic_cast<PointerType *>(toType)) {
    if (fromConstantInt && (fromConstantInt->getSExtValue() == 0)) {
      return pointerType->getDefaultValue();
    } else {
//      return builder.CreateIntToPtr(value, targetTy->getLLVMType());
      throw SemaException("you are setting an integer to a pointer directly, we usually don't do this",
                          ast->involvedTokens());
    }
  } else if (dynamic_cast<VoidType *>(toType)) {
    return nullptr;
  } else {
    throw SemaException("cannot cast form integer type", ast->involvedTokens());
  }
}
std::pair<ArithmeticType *, llvm::Value *> IntegerType::promote(llvm::Value *value, AST *ast) {
  if (mSizeInBits < sIntType.mSizeInBits) {
    return {this, castTo(&IntegerType::sIntType, value, ast)};
  } else {
    return {this, std::move(value)};
  }
}
llvm::Constant *IntegerType::getDefaultValue() {
  return llvm::ConstantInt::get(AST::getContext(), llvm::APInt(mSizeInBits, 0));
}
FloatingType FloatingType::sFloatType(32);
FloatingType FloatingType::sDoubleType(64);
FloatingType FloatingType::sLongDoubleType(128);
FloatingType::FloatingType(unsigned int mSizeInBits) : mSizeInBits(mSizeInBits) {}
llvm::Type *FloatingType::getLLVMType() {
  if (mSizeInBits <= 32) {
    return llvm::Type::getFloatTy(AST::getContext());
  } else if (mSizeInBits <= 64) {
    return llvm::Type::getDoubleTy(AST::getContext());
  } else {
    return llvm::Type::getFP128Ty(AST::getContext());
  }
}
unsigned int FloatingType::getSizeInBits() {
  return mSizeInBits;
}
llvm::APFloat FloatingType::getAPFloat(long double n) const {
  if (this == &sFloatType) {
    return llvm::APFloat(static_cast<float>(n));
  } else {
    return llvm::APFloat(static_cast<double>(n));
  }
  //TODO implement long double
}
llvm::Value *FloatingType::castTo(Type *toType, llvm::Value *fromValue, const AST *ast) {
  auto &builder = AST::getBuilder();
  auto *module = AST::getModule();
  if (getLLVMType() != fromValue->getType()) {
    throw std::runtime_error("WTF: casting: source type does not match value type");
  }

  auto *fromConstantFp = llvm::dyn_cast<llvm::ConstantFP>(fromValue);

  if (auto *toIntegerType = dynamic_cast<IntegerType *>(toType)) {
    if (fromConstantFp) {
      return llvm::ConstantInt::get(toType->getLLVMType(), fromConstantFp->getValueAPF().bitcastToAPInt());
    } else {
      if (toIntegerType->isSigned()) {
        return builder.CreateFPToSI(fromValue, toType->getLLVMType());
      } else {
        return builder.CreateFPToUI(fromValue, toType->getLLVMType());
      }
    }
  } else if (auto *toFloatType = dynamic_cast<FloatingType * >(toType)) {
    if (fromConstantFp) {
      if (toFloatType->getSizeInBits() > getSizeInBits()) {
        return llvm::ConstantExpr::getFPExtend(fromConstantFp, toFloatType->getLLVMType());
      } else {
        return llvm::ConstantExpr::getFPTrunc(fromConstantFp, toFloatType->getLLVMType());
      }
    } else {
      if (getSizeInBits() > toFloatType->getSizeInBits()) {
        return builder.CreateFPTrunc(fromValue, toType->getLLVMType());
      } else {
        return builder.CreateFPExt(fromValue, toType->getLLVMType());
      }
    }
  } else if (dynamic_cast<const VoidType *>(toType)) {
    return nullptr;
  } else {
    throw SemaException("cannot cast from float type", ast->involvedTokens());
  }
}
llvm::Constant *FloatingType::getDefaultValue() {
  return llvm::ConstantFP::get(getLLVMType(), 0.0);
}
std::pair<ArithmeticType *, llvm::Value *> FloatingType::promote(llvm::Value *value, AST *ast) {
  auto *v = this->castTo(&FloatingType::sDoubleType, value, ast);
  return {&FloatingType::sDoubleType, v};
}
FunctionType::FunctionType(QualifiedType returnType, std::vector<QualifiedType> &&parameters, bool varArg)
    : mReturnType(std::move(returnType)),
      mParameters(parameters),
      mVarArg(varArg),
      mReferencedQualifiedType{this, {}} {}
QualifiedType FunctionType::getReturnType() {
  return mReturnType;
}
std::vector<QualifiedType> &FunctionType::getParameters() {
  return mParameters;
}
llvm::FunctionType *FunctionType::getLLVMType() {
  std::vector<llvm::Type *> args;
  for (auto &paramter : mParameters) {
    args.push_back(paramter.getType()->getLLVMType());
  }
  return llvm::FunctionType::get(mReturnType.getType()->getLLVMType(), args, mVarArg);
}
bool FunctionType::compatible(Type *type) {
  if (Type::compatible(type)) {
    return true;
  } else if (auto *functionType = dynamic_cast<FunctionType *>(type)) {
    if (functionType->getReturnType().compatible(mReturnType)) {
      if (functionType->getParameters().size() == mParameters.size() && mVarArg == functionType->mVarArg) {
        auto si = mParameters.begin();
        auto ti = functionType->getParameters().begin();
        while (si != mParameters.end()) {
          if (!si->compatible(*ti)) {
            return false;
          } else {
            ++si;
            ++ti;
          }
        }
        return true;
      }
    }
  }
  return false;
}
bool FunctionType::hasVarArg() const {
  return mVarArg;
}
QualifiedType &FunctionType::getReferencedQualifiedType() {
  return mReferencedQualifiedType;
}
ArrayType::ArrayType(QualifiedType elementType, unsigned int size)
    : mSize(size), mElementType(elementType) {}
bool ArrayType::complete() {
  return mSize > 0;
}
void ArrayType::setSize(unsigned int size) {
  if (!complete()) {
    mSize = size;
  } else {
    throw std::runtime_error("WTF: set size to complete array");
  }
}
llvm::ArrayType *ArrayType::getLLVMType() {
  return llvm::ArrayType::get(mElementType.getType()->getLLVMType(), mSize);
}
bool ArrayType::compatible(Type *type) {
  if (Type::compatible(type)) {
    return true;
  } else if (auto *arrayType = dynamic_cast<ArrayType *>(type)) {
    return arrayType->getReferencedQualifiedType().compatible(mElementType) && mSize == arrayType->mSize;
  } else if (auto *pointerType = dynamic_cast<PointerType *> (type)) {
    return pointerType->getReferencedQualifiedType().compatible(mElementType);
  } else {
    return false;
  }
}
QualifiedType &ArrayType::getReferencedQualifiedType() {
  return mElementType;
}
Value ArrayType::initializerCodegen(InitializerAST *ast) {
  llvm::Value *result = nullptr;
  if (auto *list = dynamic_cast<InitializerListAST *>(ast->ast.get())) {
    const auto &initializers = list->initializers;
    bool isAllConstant = true;
    long difference = mSize - initializers.size();
    if (!complete()) {
      difference = 0;
    } else if (difference < 0) {
      throw SemaException("excess elements", ast->involvedTokens());
    }
    std::vector<llvm::Value *> values;
    for (const auto &initializer : initializers) {
      auto *v = static_cast<ObjectType *>(mElementType.getType())->initializerCodegen(initializer.get()).getValue();
      if (!llvm::dyn_cast<llvm::ConstantData>(v)) {
        isAllConstant = false;
      }
      values.push_back(v);
    }
    auto *constInt0 = llvm::ConstantInt::get(AST::getContext(), llvm::APInt(64, 0));
    auto *constInt1 = llvm::ConstantInt::get(AST::getContext(), llvm::APInt(64, 1));
    if (isAllConstant) {
      std::vector<llvm::Constant *> constants;
      constants.reserve(values.size());
      for (auto *value : values) {
        constants.push_back(static_cast<llvm::Constant *>(value));
      }
      for (int i = 0; i < difference; ++i) {
        constants.push_back(static_cast<ObjectType *>(mElementType.getType())->getDefaultValue());
      }
      result = llvm::ConstantArray::get(llvm::ArrayType::get(mElementType.getType()->getLLVMType(), values.size()),
                                        constants);
    } else {
      auto &builder = AST::getBuilder();
      auto *type = llvm::ArrayType::get(mElementType.getType()->getLLVMType(), values.size());
      auto *alloc = builder.CreateAlloca(type);
      auto *ptr = builder.CreateGEP(type, alloc, {constInt0, constInt0});
      for (auto *v :values) {
        builder.CreateStore(v, ptr);
        ptr = builder.CreateGEP(mElementType.getType()->getLLVMType(), ptr, {constInt0, constInt1});
      }
      result = alloc;
    }
    mSize = initializers.size();
  } else {
    throw SemaException("array initializer must be an initializer list", ast->involvedTokens());
  }
  return Value(QualifiedType(this, {}), false, result);
}
llvm::Constant *ArrayType::getDefaultValue() {
  if (!complete()) {
    throw std::runtime_error("WTF: cannot set default values to incomplete array");
  }
  std::vector<llvm::Constant *>
      values(mSize, static_cast<ObjectType *>(mElementType.getType())->getDefaultValue());
  return llvm::ConstantArray::get(getLLVMType(), values);
}
unsigned int ArrayType::getSizeInBits() {
  return mSize * static_cast<ObjectType *>(mElementType.getType())->getSizeInBits();
}
llvm::Value *ArrayType::castTo(Type *toType, llvm::Value *fromValue, const AST *ast) {
  if (this->compatible(toType)) {
    return fromValue;
  } else {
    throw SemaException("cannot cast array type to target type", ast->involvedTokens());
  }
}
Type *PointerType::getReferencedType() {
  return mReferencedQualifiedType.getType();
}
IntegerType *PointerType::sAddrType = &IntegerType::sUnsignedLongIntType;
llvm::PointerType *PointerType::getLLVMType() {
  return llvm::PointerType::get(mReferencedQualifiedType.getType()->getLLVMType(), 0);
}
unsigned int PointerType::getSizeInBits() {
  //TODO 32 or 64?
  return 64;
}
PointerType::PointerType(QualifiedType referencedQualifiedType) : mReferencedQualifiedType(std::move(
    referencedQualifiedType)) {}
QualifiedType &PointerType::getReferencedQualifiedType() {
  return mReferencedQualifiedType;
}
bool PointerType::complete() {
  return mReferencedQualifiedType.getType()->complete();
}
bool PointerType::compatible(Type *type) {
  if (Type::compatible(type)) {
    return true;
  } else if (auto *pointerType = dynamic_cast<PointerType *>(type)) {
    return pointerType->getReferencedQualifiedType().compatible(mReferencedQualifiedType);
  } else if (auto *arrayType = dynamic_cast<ArrayType *>(type)) {
    return arrayType->getReferencedQualifiedType().compatible(mReferencedQualifiedType);
  } else {
    return false;
  }
}
llvm::Value *PointerType::castTo(Type *toType, llvm::Value *fromValue, const AST *ast) {
  auto &builder = AST::getBuilder();
  auto *module = AST::getModule();
  if (dynamic_cast<IntegerType *>(toType)) {
    return builder.CreatePtrToInt(fromValue, toType->getLLVMType());
  } else if (dynamic_cast<PointerType *>(toType)) {
    return builder.CreateBitCast(fromValue, toType->getLLVMType());
  } else if (dynamic_cast<VoidType *>(toType)) {
    return nullptr;
  } else {
    throw SemaException("cannot cast to pointer type", ast->involvedTokens());
  }
}
llvm::Constant *PointerType::getDefaultValue() {
  return llvm::ConstantPointerNull::get(getLLVMType());
}
VoidType VoidType::sVoidType;
bool VoidType::complete() {
  return false;
}
llvm::Type *VoidType::getLLVMType() {
  return llvm::Type::getVoidTy(AST::getContext());
}
unsigned int VoidType::getSizeInBits() {
  return 0;
}
Value VoidType::initializerCodegen(InitializerAST *ast) {
  throw std::runtime_error("WTF: initializer to a void type?!");
}
llvm::Constant *VoidType::getDefaultValue() {
  throw std::runtime_error("WTF: get default value for void type");
}
CompoundType::CompoundType()
    : mSizeInBits(0), mComplete(false), mTable(ScopeKind::TAG) {}
CompoundType::CompoundType(std::string tagName)
    : mSizeInBits(0), mComplete(false), mTable(ScopeKind::TAG), mTagName(std::move(tagName)) {}
const std::string &CompoundType::getTagName() const {
  return mTagName;
}

StructType::StructType(const std::string &tag)
    : mLLVMType(llvm::StructType::create(AST::getContext(), tag)), CompoundType(tag) {}

llvm::StructType *StructType::getLLVMType() {
  return mLLVMType;
}
void StructType::setBody(SymbolTable &&table) {
  if (table.empty()) return;
  mTable = std::move(table);
  std::vector<llvm::Type *> fields(mTable.size());
  mOrderedFields.resize(mTable.size());
  for (const auto &pair : mTable) {
    if (auto *obj = dynamic_cast<ObjectSymbol *>(pair.second)) {
      if (auto *type = dynamic_cast<ObjectType *>(obj->getQualifiedType().getType())) {
        mOrderedFields.at(obj->getIndex()) = obj->getQualifiedType();
        fields.at(obj->getIndex()) = type->getLLVMType();
        mSizeInBits += type->getSizeInBits();
      }
    }
  }
  mLLVMType->setBody(fields);
  mComplete = true;
}
bool StructType::compatible(Type *type) {
  auto *st = dynamic_cast<StructType *>(type);
  if (!Type::compatible(type) || !st || st->mTagName.empty() || mTagName.empty() || st->mTagName != mTagName
      || !st->complete() || !complete() || mTable.size() != st->mTable.size()) {
    return false;
  } else {
    auto si = mTable.begin();
    auto ti = st->mTable.begin();
    while (si != mTable.end()) {
      if (auto *obj1 = dynamic_cast<ObjectSymbol *>(si->second)) {
        if (auto *obj2 = dynamic_cast<ObjectSymbol *>(si->second)) {
          if (si->first != ti->first
              || !obj1->getQualifiedType().getType()->compatible(obj2->getQualifiedType().getType())) {
            //TODO bit-fields
            return false;
          } else {
            ++si;
            ++ti;
          }
        }
      }
    }
    return true;
  }
}
Value StructType::initializerCodegen(InitializerAST *ast) {
  llvm::Value *result = nullptr;
  if (auto *list = dynamic_cast<InitializerListAST *>(ast->ast.get())) {
    const auto &initializers = list->initializers;
    if (mOrderedFields.size() < initializers.size()) {
      throw SemaException("excess elements", ast->involvedTokens());
    }
    bool isAllConstant = true;
    std::vector<llvm::Value *> values;
    auto iter = mOrderedFields.begin();
    for (const auto &initializer : initializers) {
      auto *v = static_cast<ObjectType *>(iter->getType())->initializerCodegen(initializer.get()).getValue();
      if (!llvm::dyn_cast<llvm::ConstantData>(v)) {
        isAllConstant = false;
      }
      values.push_back(v);
      ++iter;
    }
    if (isAllConstant) {
      std::vector<llvm::Constant *> constants;
      constants.reserve(values.size());
      for (auto *value : values) {
        constants.push_back(static_cast<llvm::Constant *>(value));
      }
      while (iter != mOrderedFields.end()) {
        constants.push_back(static_cast<ObjectType *>(iter++->getType())->getDefaultValue());
      }
      result = llvm::ConstantStruct::get(getLLVMType(), constants);
    } else {
      auto &builder = AST::getBuilder();
      auto *alloc = builder.CreateAlloca(getLLVMType());
      auto *constInt0 = llvm::ConstantInt::get(AST::getContext(), llvm::APInt(64, 0));
      for (int i = 0; i < values.size(); ++i) {
        auto
            *ptr = builder.CreateGEP(alloc, {constInt0, llvm::ConstantInt::get(AST::getContext(), llvm::APInt(64, i))});
        builder.CreateStore(values[i], ptr);
      }
      result = alloc;
    }
  } else {
    throw SemaException("array initializer must be an initializer list", ast->involvedTokens());
  }
  return Value(QualifiedType(this, {}), false, result);
}
bool StructType::complete() {
  return mComplete;
}
unsigned int StructType::getSizeInBits() {
  return mSizeInBits;
}
llvm::Constant *StructType::getDefaultValue() {
  if (!complete()) {
    throw std::runtime_error("WTF: cannot set default values to imcompleted array");
  }
  std::vector<llvm::Constant *> values;
  values.reserve(mOrderedFields.size());
  for (auto &qualifiedType : mOrderedFields) {
    values.push_back(static_cast<ObjectType *>(qualifiedType.getType())->getDefaultValue());
  }
  return llvm::ConstantStruct::get(getLLVMType(), values);
}
UnionType::UnionType(const std::string &tag)
    : mLLVMType(llvm::StructType::create(AST::getContext(), tag)), CompoundType(tag) {}

llvm::StructType *UnionType::getLLVMType() {
  return mLLVMType;
}
void UnionType::setBody(SymbolTable &&table) {
  if (table.empty()) return;
  mTable = std::move(table);
  mOrderedFields.resize(mTable.size());
  for (auto &pair : mTable) {
    if (auto *obj = dynamic_cast<ObjectSymbol *>(pair.second)) {
      if (auto *type = dynamic_cast<ObjectType *>(obj->getQualifiedType().getType())) {
        mOrderedFields.insert(mOrderedFields.begin() + obj->getIndex(), obj->getQualifiedType());
        auto size = type->getSizeInBits();
        if (size > mSizeInBits) {
          mSizeInBits = size;
          mBigestType = type;
        }
      }
    }
  }
  mLLVMType->setBody({mBigestType->getLLVMType()});
  mComplete = true;
}
bool UnionType::compatible(Type *type) {
  auto *st = dynamic_cast<UnionType *>(type);
  if (!Type::compatible(type) || !st || st->mTagName.empty() || mTagName.empty() || st->mTagName != mTagName
      || !st->complete() || !complete() || mTable.size() != st->mTable.size()) {
    return false;
  } else {
    auto si = mTable.begin();
    auto ti = st->mTable.begin();
    while (si != mTable.end()) {
      if (auto *obj1 = dynamic_cast<ObjectSymbol *>(si->second)) {
        if (auto *obj2 = dynamic_cast<ObjectSymbol *>(ti->second)) {
          if (si->first != ti->first
              || !obj1->getQualifiedType().getType()->compatible(obj2->getQualifiedType().getType())) {
            return false;
          } else {
            ++si;
            ++ti;
          }
        }
      }
    }
    return true;
  }
}
unsigned int UnionType::getSizeInBits() {
  return mSizeInBits;
}
Value UnionType::initializerCodegen(InitializerAST *ast) {
  llvm::Value *result = nullptr;
  if (auto *list = dynamic_cast<InitializerListAST *>(ast->ast.get())) {
    const auto &initializers = list->initializers;
    auto *firstType = static_cast<ObjectType *> (mOrderedFields[0].getType());
    if (initializers.size() > 1) {
      throw SemaException("excess elements", ast->involvedTokens());
    }
    if (initializers.empty()) {
      result = firstType->getDefaultValue();
    }
    if (initializers.size() == 1) {
      result = firstType->initializerCodegen(initializers[0].get()).getValue();
    }
    if (auto* constant = llvm::dyn_cast<llvm::Constant>(result)) {
      result = llvm::ConstantStruct::get(getLLVMType(), {constant});
    } else {
      auto& builder = AST::getBuilder();
      auto* alloca = builder.CreateAlloca(getLLVMType());
      auto* bitCast = builder.CreateBitCast(alloca, firstType->getLLVMType());
      builder.CreateStore(result, bitCast);
      result = alloca;
    }

    return Value(QualifiedType(this, {}), false, result);
  } else {
    throw SemaException("union initializer must be an initializer list", ast->involvedTokens());
  }
}
bool UnionType::complete() {
  return mComplete;
}
llvm::Constant *UnionType::getDefaultValue() {
  return llvm::ConstantStruct::get(getLLVMType(), {mBigestType->getDefaultValue()});
}
void EnumerationType::setBody(SymbolTable &&table) {
  mTable = std::move(table);
}
EnumerationType::EnumerationType(const std::string &tag)
    : CompoundType(tag), IntegerType(sIntType.getSizeInBits(), sIntType.isSigned()) {}
bool EnumerationType::complete() {
  return true;
}
llvm::Constant *EnumerationType::getDefaultValue() {
  throw std::runtime_error("WTF: getDefaultValue of enumeration type");
}
EnumerationType::EnumerationType() : IntegerType(sIntType.getSizeInBits(), sIntType.isSigned()) {

}

Value ScalarType::initializerCodegen(InitializerAST *ast) {
  llvm::Value *result = nullptr;
  if (auto *exp = dynamic_cast<AssignmentExpressionAST *>(ast->ast.get())) {
    auto v = exp->codegen();
    result = v.getType()->castTo(this, v.getValue(), ast);
  } else {
    const auto &initializers = static_cast<InitializerListAST *>(ast->ast.get())->initializers;
    if (initializers.size() > 1) {
      throw SemaException("excess elements", ast->involvedTokens());
    } else if (initializers.size() == 1) {
      result = initializerCodegen(initializers[0].get()).getValue();
    } else {
      result = getDefaultValue();
    };
  }
  return Value(QualifiedType(this, {}), false, result);
}
