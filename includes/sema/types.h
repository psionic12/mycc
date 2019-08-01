#ifndef MYCCPILER_TYPES_H
#define MYCCPILER_TYPES_H

#include <set>
#include <vector>
#include <memory>
#include <llvm/IR/Module.h>
#include <tokens/token.h>
#include "operator.h"
#include "llvm/IR/Type.h"

class Type {
 private:
 public:
  virtual ~Type() = default;
  virtual bool compatible(Type *type) const;
  virtual bool complete() const;
  virtual llvm::Type *getLLVMType(llvm::Module &module) const = 0;
};

class ObjectType : public Type {
 public:
  virtual unsigned int getSizeInBits() const = 0;
};

// Types saved as public static members
class IntegerType : public ObjectType {
 public:
  IntegerType(unsigned int mSizeInBits);
  unsigned int getSizeInBits() const;
  bool compatible(Type *type) const override;
  static const IntegerType sCharType;
  static const IntegerType sShortIntType;
  static const IntegerType sIntType;
  static const IntegerType sLongIntType;
  static const IntegerType sLongLongIntType;
  static const IntegerType sUnsignedCharType;
  static const IntegerType sUnsignedShortIntType;
  static const IntegerType sUnsignedIntType;
  static const IntegerType sUnsignedLongIntType;
  static const IntegerType sUnsignedLongLongIntType;
  llvm::IntegerType *getLLVMType(llvm::Module &module) const override;
 private:
  unsigned int mSizeInBits;
};

// Types saved as public static members
class FloatingType : public ObjectType {
 public:
  FloatingType(unsigned int mSizeInBits);
  bool compatible(Type *type) const override;
  static const FloatingType sFloatType;
  static const FloatingType sDoubleType;
  static const FloatingType sLongDoubleType;
  llvm::Type *getLLVMType(llvm::Module &module) const override;
  unsigned int getSizeInBits() const override;
 private:
  unsigned int mSizeInBits;
};

// Types saved as public static members
class VoidType : public ObjectType {
 public:
  static const VoidType sVoidType;
  bool complete() const override;
  llvm::Type *getLLVMType(llvm::Module &module) const override;
  unsigned int getSizeInBits() const override;
};

// Types created dynamically
class PointerType : public ObjectType {
 public:
  PointerType(const Type *referencedType);
  const std::set<TypeQualifier> &qualifiersToReferencedType() const;
  const Type *getReferencedType() const;
  llvm::PointerType *getLLVMType(llvm::Module &module) const override;
  unsigned int getSizeInBits() const override;
 protected:
  const Type *mReferencedType;
  std::set<TypeQualifier> mQualifersToReferencedType;
};

// Types stored in symbol table
class FunctionType : public Type {
 public:
  FunctionType(Type *returnType, std::vector<ObjectType *> &&parameters, bool varArg);
  Type *getReturnType() const;
  const std::vector<ObjectType *> &getParameters() const;
  llvm::FunctionType * getLLVMType(llvm::Module &module) const override;
  explicit operator const PointerType*() const;
 private:
  bool mVarArg;
  Type *mReturnType;
  std::vector<ObjectType *> mParameters;
  PointerType mPointerType;
};

// Types create dynamically
class ArrayType : public ObjectType {
 public:
  ArrayType(const ObjectType *elementType);
  ArrayType(const ObjectType *elementType, unsigned int size);
  bool complete() const override;
  void setSize(unsigned int size);
  llvm::ArrayType *getLLVMType(llvm::Module &module) const override;
  explicit operator const PointerType*() const;
 private:
  int64_t mSize = 0; // same with llvm
  const ObjectType *mElementType;
  PointerType mPointerType;
};

// Types stored in symbol table
class CompoundType : public ObjectType {
 public:
  CompoundType();
  bool complete() const override;
  unsigned int getSizeInBits() const override;
  std::map<std::string, const ObjectType*> mMember;
//  virtual void setBody(std::vector<std::pair<const std::string *, std::unique_ptr<ObjectSymbol>>> symbols,
//                         llvm::Module &module) = 0;
  virtual void setBody(std::map<std::string, const ObjectType*> member) = 0;
 protected:
  bool mComplete;
  unsigned mSizeInBits;
};

class StructType : public CompoundType {
 public:
  StructType(const std::string &tag, llvm::Module &module);
  StructType(llvm::Module &module);
  llvm::StructType *getLLVMType(llvm::Module &module) const override;
//  void setBody(std::vector<std::pair<const std::string *, std::unique_ptr<ObjectSymbol>>> symbols,
//                 llvm::Module &module) override;
 private:
  llvm::StructType *mLLVMType;
};

//TODO
class UnionType : public CompoundType {
 public:
  llvm::StructType *getLLVMType(llvm::Module &module) const override;
};

//TODO
class EnumerationType : public CompoundType {
 public:
  unsigned int getSizeInBits() const override;
  llvm::IntegerType *getLLVMType(llvm::Module &module) const override;
};
#endif //MYCCPILER_TYPES_H

