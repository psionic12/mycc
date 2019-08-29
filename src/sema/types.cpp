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
const IntegerType IntegerType::sCharType(8, false);
const IntegerType IntegerType::sShortIntType(16, true);
const IntegerType IntegerType::sIntType(32, true);
const IntegerType IntegerType::sLongIntType(64, true);
const IntegerType IntegerType::sLongLongIntType(64, true);
const IntegerType IntegerType::sUnsignedCharType(8, false);
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

llvm::Value *IntegerType::cast(const Type *type, llvm::Value *value, const AST *ast) const {
  auto &builder = AST::getBuilder();
  auto &module = AST::getModule();
  if (const auto *integerType = dynamic_cast<const IntegerType *>(type)) {
    if (mSizeInBits > integerType->mSizeInBits) {
      if (integerType->mSizeInBits) {
        return builder.CreateSExt(value, type->getLLVMType());
      } else {
        return builder.CreateZExt(value, type->getLLVMType());
      }
    } else if (mSizeInBits == integerType->mSizeInBits) {

      return value;
    } else {
      return builder.CreateTrunc(value, type->getLLVMType());
    }
  } else if (dynamic_cast<const FloatingType * >(type)) {
    if (isSigned()) {
      return builder.CreateSIToFP(value, type->getLLVMType());
    } else {
      return builder.CreateUIToFP(value, type->getLLVMType());
    }
  } else if (dynamic_cast<const PointerType *>(type)) {
    return builder.CreateIntToPtr(value, type->getLLVMType());
    //TODO do I need extend the size?
  } else if (dynamic_cast<const VoidType *>(type)) {
    return nullptr;
  } else {
    throw SemaException("cannot cast to integer type", ast->involvedTokens());
  }
}
std::pair<const IntegerType *, llvm::Value *> IntegerType::promote(llvm::Value *value) const {
  if (mSizeInBits < sIntType.mSizeInBits) {
    return std::make_pair<const IntegerType *, llvm::Value *>(this,
                                                              AST::getBuilder().CreateSExt(value,
                                                                                           sIntType.getLLVMType()));
  } else {
    return std::make_pair<const IntegerType *, llvm::Value *>(this, std::move(value));
  }
}
llvm::Value *IntegerType::initializerCodegen(InitializerAST *initializer) {
  if (auto *exp = dynamic_cast<AssignmentExpressionAST *>(initializer->ast.get())) {
    auto v = exp->codegen();
  }
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
  auto &module = AST::getModule();
  if (const auto *integerType = dynamic_cast<const IntegerType *>(type)) {
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
ArrayType::ArrayType(const QualifiedType elementType, unsigned int size)
    : mSize(size), mElementType(elementType), mPointerType(QualifiedType(this, {})) {}
bool ArrayType::complete() const {
  return mSize > 0;
}
void ArrayType::setSize(unsigned int size) {
  mSize = size;
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
  auto &module = AST::getModule();
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
CompoundType::CompoundType()
    : mSizeInBits(0), mComplete(false), mTable(ScopeKind::TAG) {}
bool CompoundType::complete() const {
  return mComplete;
}
unsigned int CompoundType::getSizeInBits() const {
  return mSizeInBits;
}
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
  mTable = std::move(table);
  std::vector<llvm::Type *> fields(mTable.size());
  for (const auto &pair : mTable) {
    if (const auto *obj = dynamic_cast<const ObjectSymbol *>(pair.second)) {
      if (const auto *type = dynamic_cast<const ObjectType *>(obj->getQualifiedType().getType())) {
        fields[obj->getIndex()] = (obj->getQualifiedType().getType()->getLLVMType());
        mSizeInBits += type->getSizeInBits();
      }
    }
  }
  mLLVMType->setBody(fields);
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
UnionType::UnionType(const std::string &tag)
    : mLLVMType(llvm::StructType::create(AST::getContext(), tag)), CompoundType(tag) {}

llvm::StructType *UnionType::getLLVMType() const {
  return mLLVMType;
}
void UnionType::setBody(SymbolTable &&table) {
  mTable = std::move(table);
  std::vector<llvm::Type *> fields;
  for (const auto &pair : mTable) {
    if (const auto *obj = dynamic_cast<const ObjectSymbol *>(pair.second)) {
      if (const auto *type = dynamic_cast<const ObjectType *>(obj->getQualifiedType().getType())) {
        fields.push_back(obj->getQualifiedType().getType()->getLLVMType());
        auto size = type->getSizeInBits();
        mSizeInBits = mSizeInBits > size ? size : mSizeInBits;
      }
    }
  }
  mLLVMType->setBody(fields);
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
        if (auto *obj2 = dynamic_cast<ObjectSymbol *>(si->second)) {
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
void EnumerationType::setBody(SymbolTable &&table) {
  mTable = std::move(table);
}
EnumerationType::EnumerationType(const std::string &tag) : CompoundType(tag) {}
llvm::Type *EnumerationType::getLLVMType() const {
  return nullptr;
}


