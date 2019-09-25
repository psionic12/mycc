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
llvm::Value *Type::cast(Type *toType, llvm::Value *value, const AST *ast) {
  if (toType != this) {
    throw SemaException("cannot cast type", ast->involvedTokens());
  } else {
    return value;
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

llvm::Value *IntegerType::cast(Type *targetTy, llvm::Value *value, const AST *ast) {
  auto &builder = AST::getBuilder();
  auto *module = AST::getModule();
  if (auto *constantInt = llvm::dyn_cast<llvm::ConstantInt>(value)) {
    return llvm::ConstantInt::get(getLLVMType(), constantInt->getSExtValue());
  } else if (auto *constantFp = llvm::dyn_cast<llvm::ConstantFP>(value)) {
    return llvm::ConstantInt::get(getLLVMType(), constantFp->getValueAPF().bitcastToAPInt());
  } else if (auto *integerType = dynamic_cast<IntegerType *>(targetTy)) {
    if (integerType->mSizeInBits > mSizeInBits) {
      if (integerType->mSizeInBits) {
        return builder.CreateSExt(value, targetTy->getLLVMType());
      } else {
        return builder.CreateZExt(value, targetTy->getLLVMType());
      }
    } else if (mSizeInBits == integerType->mSizeInBits) {

      return value;
    } else {
      return builder.CreateTrunc(value, targetTy->getLLVMType());
    }
  } else if (dynamic_cast<FloatingType * >(targetTy)) {
    if (isSigned()) {
      return builder.CreateSIToFP(value, targetTy->getLLVMType());
    } else {
      return builder.CreateUIToFP(value, targetTy->getLLVMType());
    }
  } else if (auto *pointerType = dynamic_cast<PointerType *>(targetTy)) {

    if (auto *constInt = llvm::dyn_cast<llvm::ConstantInt>(value)) {
      if (constInt->getSExtValue() == 0) {
        return pointerType->getDefaultValue();
      }
    } else {
//      return builder.CreateIntToPtr(value, targetTy->getLLVMType());
      throw SemaException("you are setting an integer to a pointer directly, we usually don't do this",
                          ast->involvedTokens());
    }
    //TODO do I need extend the size?
  } else if (dynamic_cast<VoidType *>(targetTy)) {
    return nullptr;
  } else {
    throw SemaException("cannot cast to integer type", ast->involvedTokens());
  }
}
std::pair<IntegerType *, llvm::Value *> IntegerType::promote(llvm::Value *value, AST *ast) {
  if (mSizeInBits < sIntType.mSizeInBits) {
    return std::make_pair<IntegerType *, llvm::Value *>(this, cast(&IntegerType::sIntType, value, ast));
  } else {
    return std::make_pair<IntegerType *, llvm::Value *>(this, std::move(value));
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
llvm::Value *FloatingType::cast(Type *toType, llvm::Value *value, const AST *ast) {
  auto &builder = AST::getBuilder();
  auto *module = AST::getModule();
  if (auto *constantInt = llvm::dyn_cast<llvm::ConstantInt>(value)) {
    return llvm::ConstantFP::get(getLLVMType(), constantInt->getSExtValue());
  } else if (auto *constantFp = llvm::dyn_cast<llvm::ConstantFP>(value)) {
    return llvm::ConstantFP::get(getLLVMType(), constantFp->getValueAPF().convertToDouble());
  } else if (const auto *integerType = dynamic_cast<const IntegerType *>(toType)) {
    if (integerType->isSigned()) {
      return builder.CreateFPToSI(value, toType->getLLVMType());
    } else {
      return builder.CreateFPToUI(value, toType->getLLVMType());
    }
  } else if (const auto *floatType = dynamic_cast<const FloatingType * >(toType)) {
    if (mSizeInBits > floatType->mSizeInBits) {
      return builder.CreateFPTrunc(value, toType->getLLVMType());
    } else {
      return builder.CreateFPExt(value, toType->getLLVMType());
    }
  } else if (dynamic_cast<const VoidType *>(toType)) {
    return nullptr;
  } else {
    throw SemaException("cannot cast to float type", ast->involvedTokens());
  }
}
llvm::Constant *FloatingType::getDefaultValue() {
  return llvm::ConstantFP::get(getLLVMType(), 0.0);
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
llvm::Value *ArrayType::cast(Type *toType, llvm::Value *value, const AST *ast) {
  if (this->compatible(toType)) {
    return value;
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
llvm::Value *PointerType::cast(Type *toType, llvm::Value *value, const AST *ast) {
  auto &builder = AST::getBuilder();
  auto *module = AST::getModule();
  if (dynamic_cast<IntegerType *>(toType)) {
    return builder.CreatePtrToInt(value, toType->getLLVMType());
  } else if (dynamic_cast<PointerType *>(toType)) {
    return builder.CreateBitCast(value, toType->getLLVMType());
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
    if (initializers.size() == 1) {
      result = mBigestType->initializerCodegen(initializers[0].get()).getValue();
    } else if (initializers.size() > 1) {
      throw SemaException("excess elements", ast->involvedTokens());
    } else {
      result = llvm::ConstantStruct::get(getLLVMType(), {mBigestType->getDefaultValue()});
    }
  } else {
    throw SemaException("union initializer must be an initializer list", ast->involvedTokens());
  }
  return Value(QualifiedType(this, {}), false, result);
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
EnumerationType::EnumerationType(const std::string &tag) : CompoundType(tag) {}
llvm::Type *EnumerationType::getLLVMType() {
  return nullptr;
}
bool EnumerationType::complete() {
  return mComplete;
}
unsigned int EnumerationType::getSizeInBits() {
  throw std::runtime_error("WTF: get size in bits of enumeration type");
}
Value EnumerationType::initializerCodegen(InitializerAST *ast) {
  throw std::runtime_error("WTF: initializerCodegen of enumeration type");
}
llvm::Constant *EnumerationType::getDefaultValue() {
  throw std::runtime_error("WTF: getDefaultValue of enumeration type");
}

Value ScalarType::initializerCodegen(InitializerAST *ast) {
  llvm::Value *result = nullptr;
  if (auto *exp = dynamic_cast<AssignmentExpressionAST *>(ast->ast.get())) {
    auto v = exp->codegen();
    result = v.getType()->cast(this, v.getValue(), ast);
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
