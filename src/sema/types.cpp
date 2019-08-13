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
bool IntegerType::compatible(const Type *type) const {
  return dynamic_cast<const IntegerType *>(type) || dynamic_cast<const FloatingType *>(type);
}
IntegerType::IntegerType(unsigned int mSizeInBits, bool bSigned) : mSizeInBits(mSizeInBits), mSigned(bSigned) {}
llvm::IntegerType *IntegerType::getLLVMType(llvm::Module &module) const {
  return llvm::IntegerType::get(module.getContext(), mSizeInBits);
}
llvm::APInt IntegerType::getAPInt(uint64_t value) const {
  return llvm::APInt(mSizeInBits, value, mSigned);
}
bool FloatingType::compatible(const Type *type) const {
  return dynamic_cast<const IntegerType *>(type) || dynamic_cast<const FloatingType *>(type);
}
const FloatingType FloatingType::sFloatType(32);
const FloatingType FloatingType::sDoubleType(64);
const FloatingType FloatingType::sLongDoubleType(128);
FloatingType::FloatingType(unsigned int mSizeInBits) : mSizeInBits(mSizeInBits) {}
llvm::Type *FloatingType::getLLVMType(llvm::Module &module) const {
  if (mSizeInBits <= 32) {
    return llvm::Type::getFloatTy(module.getContext());
  } else if (mSizeInBits <= 64) {
    return llvm::Type::getDoubleTy(module.getContext());
  } else {
    return llvm::Type::getFP128Ty(module.getContext());
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
llvm::FunctionType *FunctionType::getLLVMType(llvm::Module &module) const {
  std::vector<llvm::Type *> args;
  for (auto &paramter : mParameters) {
    args.push_back(paramter.getType()->getLLVMType(module));
  }
  return llvm::FunctionType::get(mReturnType.getType()->getLLVMType(module), args, mVarArg);
}
FunctionType::operator const PointerType *() const {
  return &mPointerType;
}
ArrayType::ArrayType(const QualifiedType elementType, unsigned int size)
    : mSize(size), mElementType(elementType), mPointerType(QualifiedType(this, {})) {}
bool ArrayType::complete() const {
  return mSize > 0;
}
void ArrayType::setSize(unsigned int size) {
  mSize = size;
}
llvm::ArrayType *ArrayType::getLLVMType(llvm::Module &module) const {
  return llvm::ArrayType::get(mElementType.getType()->getLLVMType(module), mSize);
}
ArrayType::operator const PointerType *() const {
  return &mPointerType;
}
const Type *PointerType::getReferencedType() const {
  return mReferencedQualifiedType.getType();
}
llvm::PointerType *PointerType::getLLVMType(llvm::Module &module) const {
  return llvm::PointerType::get(mReferencedQualifiedType.getType()->getLLVMType(module), 0);
}
unsigned int PointerType::getSizeInBits() const {
  //TODO 32 or 64?
  return 64;
}
PointerType::PointerType(QualifiedType referencedQualifiedType) : mReferencedQualifiedType(std::move(referencedQualifiedType)) {}
const QualifiedType &PointerType::getReferencedQualifiedType() const {
  return mReferencedQualifiedType;
}
const VoidType VoidType::sVoidType;
bool VoidType::complete() const {
  return false;
}
llvm::Type *VoidType::getLLVMType(llvm::Module &module) const {
  return llvm::Type::getVoidTy(module.getContext());
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

StructType::StructType(const std::string &tag, llvm::Module &module)
    : mLLVMType(llvm::StructType::create(module.getContext(), tag)) {}

StructType::StructType(llvm::Module &module) : mLLVMType(llvm::StructType::create(module.getContext())) {}
llvm::StructType *StructType::getLLVMType(llvm::Module &module) const {
  return mLLVMType;
}
void StructType::setBody(SymbolTable &&table, llvm::Module &module) {
  mTable = std::move(table);
  std::vector<llvm::Type *> fields;
  for (const auto &pair : mTable) {
    if (const auto *obj = dynamic_cast<const ObjectSymbol *>(pair.second)) {
      if (const auto *type = dynamic_cast<const ObjectType *>(obj->getQualifiedType().getType())) {
        fields.push_back(obj->getQualifiedType().getType()->getLLVMType(module));
        mSizeInBits += type->getSizeInBits();
      }
    }
  }
  mLLVMType->setBody(fields);
}
UnionType::UnionType(const std::string &tag, llvm::Module &module)
    : mLLVMType(llvm::StructType::create(module.getContext(), tag)) {}

UnionType::UnionType(llvm::Module &module) : mLLVMType(llvm::StructType::create(module.getContext())) {}
llvm::StructType *UnionType::getLLVMType(llvm::Module &module) const {
  return mLLVMType;
}
void UnionType::setBody(SymbolTable &&table, llvm::Module &module) {
  mTable = std::move(table);
  std::vector<llvm::Type *> fields;
  for (const auto &pair : mTable) {
    if (const auto *obj = dynamic_cast<const ObjectSymbol *>(pair.second)) {
      if (const auto *type = dynamic_cast<const ObjectType *>(obj->getQualifiedType().getType())) {
        fields.push_back(obj->getQualifiedType().getType()->getLLVMType(module));
        auto size = type->getSizeInBits();
        mSizeInBits = mSizeInBits > size ? size : mSizeInBits;
      }
    }
  }
  mLLVMType->setBody(fields);
}
unsigned int EnumerationType::getSizeInBits() const {
  return IntegerType::sIntType.getSizeInBits();
}
llvm::IntegerType *EnumerationType::getLLVMType(llvm::Module &module) const {
  return llvm::IntegerType::get(module.getContext(), getSizeInBits());
}
void EnumerationType::setBody(SymbolTable &&table, llvm::Module &module) {
  mTable = std::move(table);
}

