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
#include "value.h"

class AST;
class InitializerAST;

class Type {
 private:
 public:
  virtual ~Type() = default;
  virtual bool compatible(const Type *type) const;
  virtual bool complete() const;
  virtual llvm::Type *getLLVMType() const = 0;
  virtual llvm::Value *cast(const Type *type, llvm::Value *value, const AST *ast) const;
};

class ObjectType : public Type {
 public:
  virtual unsigned int getSizeInBits() const = 0;
  virtual Value initializerCodegen(InitializerAST *ast) const = 0;
  virtual llvm::Constant *getDefaultValue() const = 0;
};

class ScalarType : public ObjectType {
 public:
  Value initializerCodegen(InitializerAST *ast) const override;
};
class ArithmeticType : public ScalarType {};
class AggregateType : public ObjectType {};

class IntegerType : public ArithmeticType {
 public:
  unsigned int getSizeInBits() const;
  static const IntegerType &sCharType;
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
  llvm::Value *cast(const Type *targetTy, llvm::Value *value, const AST *ast) const;
  std::pair<const IntegerType *, llvm::Value *> promote(llvm::Value *value, AST *ast) const;
  llvm::Constant *getDefaultValue() const override;
 private:
  unsigned int mSizeInBits;
  bool mSigned;
 protected:
  IntegerType(unsigned int mSizeInBits, bool bSigned);
};

class FloatingType : public ArithmeticType {
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
  llvm::Constant *getDefaultValue() const override;
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
  Value initializerCodegen(InitializerAST *ast) const override;
  llvm::Constant *getDefaultValue() const override;
};

class ImplicitPointerType {
 public:
  virtual ~ImplicitPointerType() = default;
  virtual const QualifiedType &getReferencedQualifiedType() const = 0;
};

class PointerType : public ScalarType, public ImplicitPointerType {
 public:
  PointerType(QualifiedType referencedQualifiedType);
  const Type *getReferencedType() const;
  llvm::PointerType *getLLVMType() const override;
  unsigned int getSizeInBits() const override;
  const QualifiedType &getReferencedQualifiedType() const override;
  llvm::Value *cast(const Type *type,
                    llvm::Value *value,
                    const AST *ast) const;
  bool complete() const override;
  bool compatible(const Type *type) const override;
  const static IntegerType *const sAddrType;
  llvm::Constant *getDefaultValue() const override;
 protected:
  QualifiedType mReferencedQualifiedType;
};

class FunctionType : public Type, public ImplicitPointerType {
 public:
  FunctionType(QualifiedType returnType, std::vector<QualifiedType> &&parameters, bool varArg);
  QualifiedType getReturnType() const;
  const std::vector<QualifiedType> &getParameters() const;
  llvm::FunctionType *getLLVMType() const override;
  bool compatible(const Type *type) const override;
  bool hasVarArg() const;
  const QualifiedType &getReferencedQualifiedType() const override;
 private:
  bool mVarArg;
  QualifiedType mReturnType;
  std::vector<QualifiedType> mParameters;
  QualifiedType mReferencedQualifiedType;
};

class ArrayType : public AggregateType, public ImplicitPointerType {
 public:
  ArrayType(QualifiedType elementType, unsigned int size);
  bool complete() const override;
  void setSize(unsigned int size);
  llvm::ArrayType *getLLVMType() const override;
  bool compatible(const Type *type) const override;
  const QualifiedType &getReferencedQualifiedType() const;
  Value initializerCodegen(InitializerAST *ast) const override;
  llvm::Constant *getDefaultValue() const override;
  unsigned int getSizeInBits() const override;
  llvm::Value *cast(const Type *type, llvm::Value *value, const AST *ast) const override;
 private:
  int64_t mSize = 0; // same with llvm
  const QualifiedType mElementType;
};

class CompoundType {
 public:
  CompoundType();
  CompoundType(std::string tagName);
  SymbolTable mTable;
  virtual void setBody(SymbolTable &&table) = 0;
  const std::string &getTagName() const;
 protected:
  bool mComplete;
  unsigned mSizeInBits;
  std::string mTagName;
};

class StructType : public CompoundType, public AggregateType {
 public:
  StructType() = default;
  StructType(const std::string &tag);
  llvm::StructType *getLLVMType() const override;
  void setBody(SymbolTable &&table) override;
  bool compatible(const Type *type) const override;
  Value initializerCodegen(InitializerAST *ast) const override;
  bool complete() const override;
  unsigned int getSizeInBits() const override;
  llvm::Constant *getDefaultValue() const override;
 private:
  llvm::StructType *mLLVMType;
  std::vector<QualifiedType> mOrderedFields;
};

class UnionType : public CompoundType, public ObjectType {
 public:
  UnionType() = default;
  UnionType(const std::string &tag);
  llvm::StructType *getLLVMType() const override;
  void setBody(SymbolTable &&table) override;
  bool compatible(const Type *type) const override;
  unsigned int getSizeInBits() const override;
  bool complete() const override;
  Value initializerCodegen(InitializerAST *ast) const override;
  llvm::Constant *getDefaultValue() const override;
 private:
  llvm::StructType *mLLVMType;
  const ObjectType *mBigestType;
  std::vector<QualifiedType> mOrderedFields;
};

class EnumerationType : public CompoundType, public ObjectType {
 public:
  EnumerationType() = default;
  EnumerationType(const std::string &tag);
  void setBody(SymbolTable &&table) override;
  llvm::Type *getLLVMType() const override;
  bool complete() const override;
  unsigned int getSizeInBits() const override;
  Value initializerCodegen(InitializerAST *ast) const override;
  llvm::Constant *getDefaultValue() const override;
};

class EnumerationMemberType : public IntegerType {
 public:
  EnumerationMemberType(const EnumerationType *enumType) : mEnumerationType(enumType), IntegerType(32, true) {}
 private:
  const EnumerationType *mEnumerationType;
};
#endif //MYCCPILER_TYPES_H

