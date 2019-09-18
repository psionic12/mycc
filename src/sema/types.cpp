#include <sema/types.h>
#include <llvm/IR/DerivedTypes.h>
#include <sema/ast.h>
#include <sema/qualifiedType.h>
bool Type::compatible(const Type *type) const {
  return this == type;
}
bool Type::complete() const {
  return true;
}
llvm::Value *Type::cast(const Type *type, llvm::Value *value, const AST *ast) const {
  if (type != this) {
    throw SemaException("cannot cast type", ast->involvedTokens());
  } else {
    return value;
  }

}
unsigned int IntegerType::getSizeInBits() const {
  return mSizeInBits;
}

const IntegerType IntegerType::sShortIntType(16, true);
const IntegerType IntegerType::sIntType(32, true);
const IntegerType IntegerType::sLongIntType(64, true);
const IntegerType IntegerType::sLongLongIntType(64, true);
const IntegerType IntegerType::sUnsignedCharType(8, false);
const IntegerType &IntegerType::sCharType(sUnsignedCharType);
const IntegerType IntegerType::sUnsignedShortIntType(16, false);
const IntegerType IntegerType::sUnsignedIntType(32, false);
const IntegerType IntegerType::sUnsignedLongIntType(64, false);
const IntegerType IntegerType::sUnsignedLongLongIntType(64, false);
const IntegerType IntegerType::sOneBitBoolIntType(1, false);
IntegerType::IntegerType(unsigned int mSizeInBits, bool bSigned) : mSizeInBits(mSizeInBits), mSigned(bSigned) {}
llvm::IntegerType *IntegerType::getLLVMType() const {
  return llvm::IntegerType::get(AST::getContext(), mSizeInBits);
}
llvm::APInt IntegerType::getAPInt(uint64_t value) const {
  return llvm::APInt(mSizeInBits, value, mSigned);
}
bool IntegerType::isSigned() const {
  return mSigned;
}

llvm::Value *IntegerType::cast(const Type *targetTy, llvm::Value *value, const AST *ast) const {
  auto &builder = AST::getBuilder();
  auto *module = AST::getModule();
  if (auto *constantInt = llvm::dyn_cast<llvm::ConstantInt>(value)) {
    return llvm::ConstantInt::get(getLLVMType(), constantInt->getSExtValue());
  } else if (auto *constantFp = llvm::dyn_cast<llvm::ConstantFP>(value)) {
    return llvm::ConstantInt::get(getLLVMType(), constantFp->getValueAPF().bitcastToAPInt());
  } else if (const auto *integerType = dynamic_cast<const IntegerType *>(targetTy)) {
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
  } else if (dynamic_cast<const FloatingType * >(targetTy)) {
    if (isSigned()) {
      return builder.CreateSIToFP(value, targetTy->getLLVMType());
    } else {
      return builder.CreateUIToFP(value, targetTy->getLLVMType());
    }
  } else if (dynamic_cast<const PointerType *>(targetTy)) {
    return builder.CreateIntToPtr(value, targetTy->getLLVMType());
    //TODO do I need extend the size?
  } else if (dynamic_cast<const VoidType *>(targetTy)) {
    return nullptr;
  } else {
    throw SemaException("cannot cast to integer type", ast->involvedTokens());
  }
}
std::pair<const IntegerType *, llvm::Value *> IntegerType::promote(llvm::Value *value, AST *ast) const {
  if (mSizeInBits < sIntType.mSizeInBits) {
    return std::make_pair<const IntegerType *, llvm::Value *>(this, cast(&IntegerType::sIntType, value, ast));
  } else {
    return std::make_pair<const IntegerType *, llvm::Value *>(this, std::move(value));
  }
}
llvm::Constant *IntegerType::getDefaultValue() const {
  return llvm::ConstantInt::get(AST::getContext(), llvm::APInt(mSizeInBits, 0));
}
const FloatingType FloatingType::sFloatType(32);
const FloatingType FloatingType::sDoubleType(64);
const FloatingType FloatingType::sLongDoubleType(128);
FloatingType::FloatingType(unsigned int mSizeInBits) : mSizeInBits(mSizeInBits) {}
llvm::Type *FloatingType::getLLVMType() const {
  if (mSizeInBits <= 32) {
    return llvm::Type::getFloatTy(AST::getContext());
  } else if (mSizeInBits <= 64) {
    return llvm::Type::getDoubleTy(AST::getContext());
  } else {
    return llvm::Type::getFP128Ty(AST::getContext());
  }
}
unsigned int FloatingType::getSizeInBits() const {
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
llvm::Value *FloatingType::cast(const Type *type, llvm::Value *value, const AST *ast) const {
  auto &builder = AST::getBuilder();
  auto *module = AST::getModule();
  if (auto *constantInt = llvm::dyn_cast<llvm::ConstantInt>(value)) {
    return llvm::ConstantFP::get(getLLVMType(), constantInt->getSExtValue());
  } else if (auto *constantFp = llvm::dyn_cast<llvm::ConstantFP>(value)) {
    return llvm::ConstantFP::get(getLLVMType(), constantFp->getValueAPF().convertToDouble());
  } else if (const auto *integerType = dynamic_cast<const IntegerType *>(type)) {
    if (integerType->isSigned()) {
      return builder.CreateFPToSI(value, type->getLLVMType());
    } else {
      return builder.CreateFPToUI(value, type->getLLVMType());
    }
  } else if (const auto *floatType = dynamic_cast<const FloatingType * >(type)) {
    if (mSizeInBits > floatType->mSizeInBits) {
      return builder.CreateFPTrunc(value, type->getLLVMType());
    } else {
      return builder.CreateFPExt(value, type->getLLVMType());
    }
  } else if (dynamic_cast<const VoidType *>(type)) {
    return nullptr;
  } else {
    throw SemaException("cannot cast to float type", ast->involvedTokens());
  }
}
llvm::Constant *FloatingType::getDefaultValue() const {
  return llvm::ConstantFP::get(getLLVMType(), 0.0);
}
FunctionType::FunctionType(QualifiedType returnType, std::vector<QualifiedType> &&parameters, bool varArg)
    : mReturnType(std::move(returnType)),
      mParameters(parameters),
      mVarArg(varArg),
      mPointerType(QualifiedType(this, {})) {}
QualifiedType FunctionType::getReturnType() const {
  return mReturnType;
}
const std::vector<QualifiedType> &FunctionType::getParameters() const {
  return mParameters;
}
llvm::FunctionType *FunctionType::getLLVMType() const {
  std::vector<llvm::Type *> args;
  for (auto &paramter : mParameters) {
    args.push_back(paramter.getType()->getLLVMType());
  }
  return llvm::FunctionType::get(mReturnType.getType()->getLLVMType(), args, mVarArg);
}
bool FunctionType::compatible(const Type *type) const {
  if (Type::compatible(type)) {
    return true;
  } else if (const auto *functionType = dynamic_cast<const FunctionType *>(type)) {
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
const PointerType *FunctionType::castToPointerType() const {
  return &mPointerType;
}
bool FunctionType::hasVarArg() const {
  return mVarArg;
}
ArrayType::ArrayType(const QualifiedType elementType, unsigned int size)
    : mSize(size), mElementType(elementType), mPointerType(elementType) {}
bool ArrayType::complete() const {
  return mSize > 0;
}
void ArrayType::setSize(unsigned int size) {
  if (!complete()) {
    mSize = size;
  } else {
    throw std::runtime_error("WTF: set size to complete array");
  }
}
llvm::ArrayType *ArrayType::getLLVMType() const {
  return llvm::ArrayType::get(mElementType.getType()->getLLVMType(), mSize);
}
bool ArrayType::compatible(const Type *type) const {
  if (Type::compatible(type)) {
    return true;
  } else if (const auto *arrayType = dynamic_cast<const ArrayType *>(type)) {
    return arrayType->getReferencedQualifiedType().compatible(mElementType) && mSize == arrayType->mSize;
  } else {
    return false;
  }
}
const QualifiedType &ArrayType::getReferencedQualifiedType() const {
  return mElementType;
}
llvm::Value *ArrayType::initializerCodegen(InitializerAST *ast) const {
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
      auto *v = static_cast<const ObjectType *>(mElementType.getType())->initializerCodegen(initializer.get());
      if (!llvm::dyn_cast<llvm::Constant>(v)) {
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
        constants.push_back(static_cast<const ObjectType *>(mElementType.getType())->getDefaultValue());
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
        ptr = builder.CreateGEP(mElementType.getType()->getLLVMType(), ptr, constInt1);
      }
      result = alloc;
    }
  } else {
    throw SemaException("array initializer must be an initializer list", ast->involvedTokens());
  }
  return result;
}
llvm::Constant *ArrayType::getDefaultValue() const {
  if (!complete()) {
    throw std::runtime_error("WTF: cannot set default values to imcompleted array");
  }
  std::vector<llvm::Constant *>
      values(mSize, static_cast<const ObjectType *>(mElementType.getType())->getDefaultValue());
  return llvm::ConstantArray::get(getLLVMType(), values);
}
unsigned int ArrayType::getSizeInBits() const {
  return mSize * static_cast<const ObjectType *>(mElementType.getType())->getSizeInBits();
}
const PointerType *ArrayType::castToPointerType() const {
  return &mPointerType;
}
const Type *PointerType::getReferencedType() const {
  return mReferencedQualifiedType.getType();
}
const IntegerType *const PointerType::sAddrType = &IntegerType::sUnsignedLongIntType;
llvm::PointerType *PointerType::getLLVMType() const {
  return llvm::PointerType::get(mReferencedQualifiedType.getType()->getLLVMType(), 0);
}
unsigned int PointerType::getSizeInBits() const {
  //TODO 32 or 64?
  return 64;
}
PointerType::PointerType(QualifiedType referencedQualifiedType) : mReferencedQualifiedType(std::move(
    referencedQualifiedType)) {}
const QualifiedType &PointerType::getReferencedQualifiedType() const {
  return mReferencedQualifiedType;
}
bool PointerType::complete() const {
  return mReferencedQualifiedType.getType()->complete();
}
bool PointerType::compatible(const Type *type) const {
  if (Type::compatible(type)) {
    return true;
  } else if (const auto *pointerType = dynamic_cast<const PointerType *>(type)) {
    return pointerType->getReferencedQualifiedType().compatible(mReferencedQualifiedType);
  } else {
    return false;
  }
}
llvm::Value *PointerType::cast(const Type *type, llvm::Value *value, const AST *ast) const {
  auto &builder = AST::getBuilder();
  auto *module = AST::getModule();
  if (dynamic_cast<const IntegerType *>(type)) {
    return builder.CreatePtrToInt(value, type->getLLVMType());
  } else if (dynamic_cast<const PointerType *>(type)) {
    return builder.CreateBitCast(value, type->getLLVMType());
  } else if (dynamic_cast<const VoidType *>(type)) {
    return nullptr;
  } else {
    throw SemaException("cannot cast to pointer type", ast->involvedTokens());
  }
}
llvm::Constant *PointerType::getDefaultValue() const {
  return llvm::ConstantPointerNull::get(getLLVMType());
}
const VoidType VoidType::sVoidType;
bool VoidType::complete() const {
  return false;
}
llvm::Type *VoidType::getLLVMType() const {
  return llvm::Type::getVoidTy(AST::getContext());
}
unsigned int VoidType::getSizeInBits() const {
  return 0;
}
llvm::Value *VoidType::initializerCodegen(InitializerAST *ast) const {
  throw std::runtime_error("WTF: initializer to a void type?!");
}
llvm::Constant *VoidType::getDefaultValue() const {
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

llvm::StructType *StructType::getLLVMType() const {
  return mLLVMType;
}
void StructType::setBody(SymbolTable &&table) {
  if (table.empty()) return;
  mTable = std::move(table);
  std::vector<llvm::Type *> fields(mTable.size());
  mOrderedFields.resize(mTable.size());
  for (const auto &pair : mTable) {
    if (const auto *obj = dynamic_cast<const ObjectSymbol *>(pair.second)) {
      if (const auto *type = dynamic_cast<const ObjectType *>(obj->getQualifiedType().getType())) {
        mOrderedFields.at(obj->getIndex()) = obj->getQualifiedType();
        fields.at(obj->getIndex()) = type->getLLVMType();
        mSizeInBits += type->getSizeInBits();
      }
    }
  }
  mLLVMType->setBody(fields);
  mComplete = true;
}
bool StructType::compatible(const Type *type) const {
  const auto *st = dynamic_cast<const StructType *>(type);
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
llvm::Value *StructType::initializerCodegen(InitializerAST *ast) const {
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
      auto *v = static_cast<const ObjectType *>(iter->getType())->initializerCodegen(initializer.get());
      if (!llvm::dyn_cast<llvm::Constant>(v)) {
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
        constants.push_back(static_cast<const ObjectType *>(iter++->getType())->getDefaultValue());
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
  return result;
}
bool StructType::complete() const {
  return mComplete;
}
unsigned int StructType::getSizeInBits() const {
  return mSizeInBits;
}
llvm::Constant *StructType::getDefaultValue() const {
  if (!complete()) {
    throw std::runtime_error("WTF: cannot set default values to imcompleted array");
  }
  std::vector<llvm::Constant *> values;
  values.reserve(mOrderedFields.size());
  for (const auto &qualifiedType : mOrderedFields) {
    values.push_back(static_cast<const ObjectType *>(qualifiedType.getType())->getDefaultValue());
  }
  return llvm::ConstantStruct::get(getLLVMType(), values);
}
UnionType::UnionType(const std::string &tag)
    : mLLVMType(llvm::StructType::create(AST::getContext(), tag)), CompoundType(tag) {}

llvm::StructType *UnionType::getLLVMType() const {
  return mLLVMType;
}
void UnionType::setBody(SymbolTable &&table) {
  if (table.empty()) return;
  mTable = std::move(table);
  mOrderedFields.resize(mTable.size());
  for (const auto &pair : mTable) {
    if (const auto *obj = dynamic_cast<const ObjectSymbol *>(pair.second)) {
      if (const auto *type = dynamic_cast<const ObjectType *>(obj->getQualifiedType().getType())) {
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
bool UnionType::compatible(const Type *type) const {
  const auto *st = dynamic_cast<const UnionType *>(type);
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
unsigned int UnionType::getSizeInBits() const {
  return mSizeInBits;
}
llvm::Value *UnionType::initializerCodegen(InitializerAST *ast) const {
  llvm::Value *result = nullptr;
  if (auto *list = dynamic_cast<InitializerListAST *>(ast->ast.get())) {
    const auto &initializers = list->initializers;
    if (initializers.size() == 1) {
      result = mBigestType->initializerCodegen(initializers[0].get());
    } else if (initializers.size() > 1) {
      throw SemaException("excess elements", ast->involvedTokens());
    } else {
      result = llvm::ConstantStruct::get(getLLVMType(), {mBigestType->getDefaultValue()});
    }
  } else {
    throw SemaException("union initializer must be an initializer list", ast->involvedTokens());
  }
  return result;
}
bool UnionType::complete() const {
  return mComplete;
}
llvm::Constant *UnionType::getDefaultValue() const {
  return llvm::ConstantStruct::get(getLLVMType(), {mBigestType->getDefaultValue()});
}
void EnumerationType::setBody(SymbolTable &&table) {
  mTable = std::move(table);
}
EnumerationType::EnumerationType(const std::string &tag) : CompoundType(tag) {}
llvm::Type *EnumerationType::getLLVMType() const {
  return nullptr;
}
bool EnumerationType::complete() const {
  return mComplete;
}
unsigned int EnumerationType::getSizeInBits() const {
  throw std::runtime_error("WTF: get size in bits of enumeration type");
}
llvm::Value *EnumerationType::initializerCodegen(InitializerAST *ast) const {
  throw std::runtime_error("WTF: initializerCodegen of enumeration type");
}
llvm::Constant *EnumerationType::getDefaultValue() const {
  throw std::runtime_error("WTF: getDefaultValue of enumeration type");
}

llvm::Value *ScalarType::initializerCodegen(InitializerAST *ast) const {
  llvm::Value *result = nullptr;
  if (auto *exp = dynamic_cast<AssignmentExpressionAST *>(ast->ast.get())) {
    auto v = exp->codegen();
    result = cast(v.qualifiedType.getType(), v.getValue(), ast);
  } else {
    const auto &initializers = static_cast<InitializerListAST *>(ast->ast.get())->initializers;
    if (initializers.size() > 1) {
      throw SemaException("excess elements", ast->involvedTokens());
    } else if (initializers.size() == 1) {
      result = initializerCodegen(initializers[0].get());
    } else {
      result = getDefaultValue();
    };
  }
  return result;
}
