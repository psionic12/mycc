#ifndef MYCCPILER_TYPES_H
#define MYCCPILER_TYPES_H

#include <set>
#include <vector>
#include <memory>
#include <llvm/IR/Module.h>
#include <tokens/token.h>
#include "operator.h"
#include "llvm/IR/Type.h"
#include "qualifiedType.h"
#include "symbol_tables.h"

class Type {
 private:
 public:
  virtual ~Type() = default;
  virtual bool compatible(const Type *type) const;
  virtual bool complete() const;
  virtual llvm::Type *getLLVMType(llvm::Module &module) const = 0;
};

class ObjectType : public Type {
 public:
  virtual unsigned int getSizeInBits() const = 0;
};

class IntegerType : public ObjectType {
 public:
  unsigned int getSizeInBits() const;
  bool compatible(const Type *type) const override;
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
  llvm::APInt getAPInt(uint64_t value) const;
 private:
  IntegerType(unsigned int mSizeInBits, bool bSigned);
  unsigned int mSizeInBits;
  bool mSigned;
};

class FloatingType : public ObjectType {
 public:
  bool compatible(const Type *type) const override;
  static const FloatingType sFloatType;
  static const FloatingType sDoubleType;
  static const FloatingType sLongDoubleType;
  llvm::Type *getLLVMType(llvm::Module &module) const override;
  unsigned int getSizeInBits() const override;
  llvm::APFloat getAPFloat(long double) const;
 private:
  FloatingType(unsigned int mSizeInBits);
  unsigned int mSizeInBits;
};

class VoidType : public ObjectType {
 public:
  static const VoidType sVoidType;
  bool complete() const override;
  llvm::Type *getLLVMType(llvm::Module &module) const override;
  unsigned int getSizeInBits() const override;
};

class PointerType : public ObjectType {
 public:
  PointerType(QualifiedType referencedQualifiedType);
  const Type *getReferencedType() const;
  llvm::PointerType *getLLVMType(llvm::Module &module) const override;
  unsigned int getSizeInBits() const override;
  const QualifiedType &getReferencedQualifiedType() const;
 protected:
  QualifiedType mReferencedQualifiedType;
};

class FunctionType : public Type {
 public:
  FunctionType(QualifiedType returnType, std::vector<QualifiedType> &&parameters, bool varArg);
  QualifiedType getReturnType() const;
  const std::vector<QualifiedType> &getParameters() const;
  llvm::FunctionType *getLLVMType(llvm::Module &module) const override;
  explicit operator const PointerType *() const;
 private:
  bool mVarArg;
  QualifiedType mReturnType;
  std::vector<QualifiedType> mParameters;
  PointerType mPointerType;
};

class ArrayType : public ObjectType {
 public:
  ArrayType(QualifiedType elementType, unsigned int size);
  bool complete() const override;
  void setSize(unsigned int size);
  llvm::ArrayType *getLLVMType(llvm::Module &module) const override;
  explicit operator const PointerType *() const;
 private:
  int64_t mSize = 0; // same with llvm
  const QualifiedType mElementType;
  PointerType mPointerType;
};

class CompoundType : public ObjectType {
 public:
  CompoundType();
  CompoundType(std::string tagName);
  bool complete() const override;
  unsigned int getSizeInBits() const override;
  SymbolTable mTable;
  virtual void setBody(SymbolTable &&table, llvm::Module &module) = 0;
  const std::string &getTagName() const;
 protected:
  bool mComplete;
  unsigned mSizeInBits;
  std::string mTagName;
};

class StructType : public CompoundType {
 public:
  StructType(const std::string &tag, llvm::Module &module);
  StructType(llvm::Module &module);
  llvm::StructType *getLLVMType(llvm::Module &module) const override;
  void setBody(SymbolTable &&table, llvm::Module &module) override;
 private:
  llvm::StructType *mLLVMType;
};

//TODO
class UnionType : public CompoundType {
 public:
  UnionType(const std::string &tag, llvm::Module &module);
  UnionType(llvm::Module &module);
  llvm::StructType *getLLVMType(llvm::Module &module) const override;
  void setBody(SymbolTable &&table, llvm::Module &module) override;
 private:
  llvm::StructType *mLLVMType;
};

//TODO
class EnumerationType : public CompoundType {
 public:
  void setBody(SymbolTable &&table, llvm::Module &module) override;
  unsigned int getSizeInBits() const override;
  llvm::IntegerType *getLLVMType(llvm::Module &module) const override;
};
#endif //MYCCPILER_TYPES_H

