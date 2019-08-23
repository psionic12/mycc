#ifndef MYCCPILER_TYPES_H
#define MYCCPILER_TYPES_H

#include <set>
#include <vector>
#include <memory>
#include <tokens/token.h>
#include "operator.h"
#include "qualifiedType.h"
#include "symbol_tables.h"
#include "llvm/IR/IRBuilder.h"

class AST;

class Type {
 private:
 public:
  virtual ~Type() = default;
  virtual bool compatible(const Type *type) const;
  virtual bool complete() const;
  virtual llvm::Type *getLLVMType() const = 0;
};

class ObjectType : public Type {
 public:
  virtual unsigned int getSizeInBits() const = 0;
};

class ScalarType {};
class ArithmeticType : public ScalarType {};
class AggregateType {};

class IntegerType : public ObjectType, public ArithmeticType {
 public:
  unsigned int getSizeInBits() const;
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
  static const IntegerType sOneBitBoolIntType;// used for llvm i1 only
  llvm::IntegerType *getLLVMType() const override;
  llvm::APInt getAPInt(uint64_t value) const;
  bool isSigned() const;
  llvm::Value *cast(const Type *type, llvm::Value *value, const AST *ast) const;
  std::pair<const IntegerType *, llvm::Value *> promote(llvm::Value *value) const;
 private:
  unsigned int mSizeInBits;
  bool mSigned;
 protected:
  IntegerType(unsigned int mSizeInBits, bool bSigned);
};

class FloatingType : public ObjectType, public ArithmeticType {
 public:
  static const FloatingType sFloatType;
  static const FloatingType sDoubleType;
  static const FloatingType sLongDoubleType;
  llvm::Type *getLLVMType() const override;
  unsigned int getSizeInBits() const override;
  llvm::APFloat getAPFloat(long double) const;
  llvm::Value *cast(const Type *type,
                    llvm::Value *value,
                    const AST *ast) const;
 private:
  FloatingType(unsigned int mSizeInBits);
  unsigned int mSizeInBits;
};

class VoidType : public ObjectType {
 public:
  static const VoidType sVoidType;
  bool complete() const override;
  llvm::Type *getLLVMType() const override;
  unsigned int getSizeInBits() const override;
};

class PointerType : public ObjectType, public ScalarType {
 public:
  PointerType(QualifiedType referencedQualifiedType);
  const Type *getReferencedType() const;
  llvm::PointerType *getLLVMType() const override;
  unsigned int getSizeInBits() const override;
  const QualifiedType &getReferencedQualifiedType() const;
  llvm::Value *cast(const Type *type,
                    llvm::Value *value,
                    const AST *ast) const;
  bool complete() const override;
  bool compatible(const Type *type) const override;
  const static IntegerType *const sAddrType;
 protected:
  QualifiedType mReferencedQualifiedType;
};

class FunctionType : public Type {
 public:
  FunctionType(QualifiedType returnType, std::vector<QualifiedType> &&parameters, bool varArg);
  QualifiedType getReturnType() const;
  const std::vector<QualifiedType> &getParameters() const;
  llvm::FunctionType *getLLVMType() const override;
  bool compatible(const Type *type) const override;
 private:
  bool mVarArg;
  QualifiedType mReturnType;
  std::vector<QualifiedType> mParameters;
  PointerType mPointerType;
};

class ArrayType : public ObjectType, public AggregateType {
 public:
  ArrayType(QualifiedType elementType, unsigned int size);
  bool complete() const override;
  void setSize(unsigned int size);
  llvm::ArrayType *getLLVMType() const override;
  bool compatible(const Type *type) const override;
  const QualifiedType &getReferencedQualifiedType() const;
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

class StructType : public CompoundType, public AggregateType {
 public:
  StructType(const std::string &tag, llvm::Module &module);
  StructType(llvm::Module &module);
  llvm::StructType *getLLVMType() const override;
  void setBody(SymbolTable &&table, llvm::Module &module) override;
  bool compatible(const Type *type) const override;
 private:
  llvm::StructType *mLLVMType;
};

//TODO
class UnionType : public CompoundType {
 public:
  UnionType(const std::string &tag, llvm::Module &module);
  UnionType(llvm::Module &module);
  llvm::StructType *getLLVMType() const override;
  void setBody(SymbolTable &&table, llvm::Module &module) override;
  bool compatible(const Type *type) const override;
 private:
  llvm::StructType *mLLVMType;
};

//TODO
class EnumerationType : public CompoundType, public IntegerType {
 public:
  EnumerationType() : IntegerType(IntegerType::sIntType.getSizeInBits(), IntegerType::sIntType.isSigned()) {}
  void setBody(SymbolTable &&table, llvm::Module &module) override;
};
#endif //MYCCPILER_TYPES_H

