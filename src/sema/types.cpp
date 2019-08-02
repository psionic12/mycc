#include <sema/types.h>
#include <llvm/IR/DerivedTypes.h>
#include <sema/ast.h>
#include <sema/qualifiedType.h>
bool Type::compatible(Type *type) const {
  return this == type;
}
bool Type::complete() const {
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
bool IntegerType::compatible(Type *type) const {
  return dynamic_cast<IntegerType *>(type) || dynamic_cast<FloatingType *>(type);
}
IntegerType::IntegerType(unsigned int mSizeInBits) : mSizeInBits(mSizeInBits) {}
llvm::IntegerType *IntegerType::getLLVMType(llvm::Module &module) const {
  return llvm::IntegerType::get(module.getContext(), mSizeInBits);
}
bool FloatingType::compatible(Type *type) const {
  return dynamic_cast<IntegerType *>(type) || dynamic_cast<FloatingType *>(type);
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
FunctionType::FunctionType(QualifiedType returnType, std::vector<QualifiedType> &&parameters, bool varArg)
    : mReturnType(std::move(returnType)), mParameters(parameters), mVarArg(varArg), mPointerType(this) {}
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
    : mSize(size), mElementType(elementType), mPointerType(this) {}
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
PointerType::PointerType(const Type *referencedType)
    : mReferencedType(referencedType) {}
const Type *PointerType::getReferencedType() const {
  return mReferencedType;
}
const std::set<TypeQualifier> &PointerType::qualifiersToReferencedType() const {
  return mQualifersToReferencedType;
}
llvm::PointerType *PointerType::getLLVMType(llvm::Module &module) const {
  return llvm::PointerType::get(mReferencedType->getLLVMType(module), 0);
}
unsigned int PointerType::getSizeInBits() const {
  //TODO 32 or 64?
  return 64;
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
    : mSizeInBits(0), mComplete(false) {}
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
//void StructType::setBody(std::vector<std::pair<const std::string *, std::unique_ptr<ObjectSymbol>>> symbols,
//                         llvm::Module &module) {
//  std::vector<llvm::Type *> fields;
//  auto end = symbols.size() > 1 ? symbols.end() - 1 : symbols.end();
//  for (auto it = symbols.begin(); it != end; it++) {
//    const std::string *string = it->first;
//    auto symbol = std::move(it->second);
//    if (!symbol->getType()->complete()) {
//      throw SemaException(std::string("member ") + (string ? *string : "") + " is incomplete", symbol->mInvolvedTokens);
//    }
//    fields.push_back(symbol->getType()->getLLVMType(module));
//  }
//  //TODO the standard says "a structure with more than one named member",
//  // which I don't fully understand, I'll come back later
//  if (symbols.size() > 1) {
//    auto symbol = std::move(symbols.back().second);
//    if (!symbol->getType()->complete()) {
//      if (!dynamic_cast<ArrayType *>(symbol->getType())) {
//        throw SemaException("only flexible array member can be the last member which is incomplete",
//                            symbol->mInvolvedTokens);
//      } else {
//        mComplete = false;
//      }
//    } else {
//      mComplete = true;
//    }
//    fields.push_back(symbol->getType()->getLLVMType(module));
//  }
//  mLLVMType->setBody(fields);
//}
llvm::StructType *UnionType::getLLVMType(llvm::Module &module) const {
  return nullptr;
}
unsigned int EnumerationType::getSizeInBits() const {
  return IntegerType::sIntType.getSizeInBits();
}
llvm::IntegerType *EnumerationType::getLLVMType(llvm::Module &module) const {
  return llvm::IntegerType::get(module.getContext(), getSizeInBits());
}

