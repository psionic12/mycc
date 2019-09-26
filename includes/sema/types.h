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
 public:
  virtual ~Type() = default;
  virtual bool compatible(Type *type);
  virtual bool complete();
  virtual llvm::Type *getLLVMType() = 0;
  virtual llvm::Value *castTo(Type *toType, llvm::Value *fromValue, const AST *ast);
};

class ObjectType : public Type {
 public:
  virtual unsigned int getSizeInBits() = 0;
  virtual Value initializerCodegen(InitializerAST *ast) = 0;
  virtual llvm::Constant *getDefaultValue() = 0;
};

class ScalarType : public ObjectType {
 public:
  Value initializerCodegen(InitializerAST *ast) override;
};
class ArithmeticType : public ScalarType {
 public:
  virtual std::pair<ArithmeticType *, llvm::Value *> promote(llvm::Value *value, AST *ast) = 0;
};
class AggregateType : public ObjectType {};

class IntegerType : public ArithmeticType {
 public:
  unsigned int getSizeInBits();
  static IntegerType &sCharType;
  static IntegerType sShortIntType;
  static IntegerType sIntType;
  static IntegerType sLongIntType;
  static IntegerType sLongLongIntType;
  static IntegerType sUnsignedCharType;
  static IntegerType sUnsignedShortIntType;
  static IntegerType sUnsignedIntType;
  static IntegerType sUnsignedLongIntType;
  static IntegerType sUnsignedLongLongIntType;
  static IntegerType sOneBitBoolIntType;// used for llvm i1 only
  llvm::IntegerType *getLLVMType() override;
  llvm::APInt getAPInt(uint64_t value) const;
  bool isSigned() const;
  llvm::Value *castTo(Type *toType, llvm::Value *fromValue, const AST *ast);
  std::pair<ArithmeticType *, llvm::Value *> promote(llvm::Value *value, AST *ast) override;
  llvm::Constant *getDefaultValue() override;
 private:
  unsigned int mSizeInBits;
  bool mSigned;
 protected:
  IntegerType(unsigned int mSizeInBits, bool bSigned);
};

class FloatingType : public ArithmeticType {
 public:
  static FloatingType sFloatType;
  static FloatingType sDoubleType;
  static FloatingType sLongDoubleType;
  llvm::Type *getLLVMType() override;
  unsigned int getSizeInBits() override;
  llvm::APFloat getAPFloat(long double) const;
  llvm::Value *castTo(Type *toType,
                      llvm::Value *fromValue,
                      const AST *ast);
  llvm::Constant *getDefaultValue() override;
  std::pair<ArithmeticType *, llvm::Value *> promote(llvm::Value *value, AST *ast) override;
 private:
  FloatingType(unsigned int mSizeInBits);
  unsigned int mSizeInBits;
};

class VoidType : public ObjectType {
 public:
  static VoidType sVoidType;
  bool complete() override;
  llvm::Type *getLLVMType() override;
  unsigned int getSizeInBits() override;
  Value initializerCodegen(InitializerAST *ast) override;
  llvm::Constant *getDefaultValue() override;
};

class PointerType : public ScalarType {
 public:
  PointerType(QualifiedType referencedQualifiedType);
  Type *getReferencedType();
  llvm::PointerType *getLLVMType() override;
  unsigned int getSizeInBits() override;
  QualifiedType &getReferencedQualifiedType();
  llvm::Value *castTo(Type *toType,
                      llvm::Value *fromValue,
                      const AST *ast);
  bool complete() override;
  bool compatible(Type *type) override;
  static IntegerType *sAddrType;
  llvm::Constant *getDefaultValue() override;
 protected:
  QualifiedType mReferencedQualifiedType;
};

class FunctionType : public Type {
 public:
  FunctionType(QualifiedType returnType, std::vector<QualifiedType> &&parameters, bool varArg);
  QualifiedType getReturnType();
  std::vector<QualifiedType> &getParameters();
  llvm::FunctionType *getLLVMType() override;
  bool compatible(Type *type) override;
  bool hasVarArg() const;
  QualifiedType &getReferencedQualifiedType();
 private:
  bool mVarArg;
  QualifiedType mReturnType;
  std::vector<QualifiedType> mParameters;
  QualifiedType mReferencedQualifiedType;
};

class ArrayType : public AggregateType {
 public:
  ArrayType(QualifiedType elementType, unsigned int size);
  bool complete() override;
  void setSize(unsigned int size);
  llvm::ArrayType *getLLVMType() override;
  bool compatible(Type *type) override;
  QualifiedType &getReferencedQualifiedType();
  Value initializerCodegen(InitializerAST *ast) override;
  llvm::Constant *getDefaultValue() override;
  unsigned int getSizeInBits() override;
  llvm::Value *castTo(Type *toType, llvm::Value *fromValue, const AST *ast) override;
 private:
  int64_t mSize = 0; // same with llvm
  QualifiedType mElementType;
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
  llvm::StructType *getLLVMType() override;
  void setBody(SymbolTable &&table) override;
  bool compatible(Type *type) override;
  Value initializerCodegen(InitializerAST *ast) override;
  bool complete() override;
  unsigned int getSizeInBits() override;
  llvm::Constant *getDefaultValue() override;
 private:
  llvm::StructType *mLLVMType;
  std::vector<QualifiedType> mOrderedFields;
};

class UnionType : public CompoundType, public ObjectType {
 public:
  UnionType() = default;
  UnionType(const std::string &tag);
  llvm::StructType *getLLVMType() override;
  void setBody(SymbolTable &&table) override;
  bool compatible(Type *type) override;
  unsigned int getSizeInBits() override;
  bool complete() override;
  Value initializerCodegen(InitializerAST *ast) override;
  llvm::Constant *getDefaultValue() override;
 private:
  llvm::StructType *mLLVMType;
  ObjectType *mBigestType;
  std::vector<QualifiedType> mOrderedFields;
};

class EnumerationType : public CompoundType, public IntegerType {
 public:
  EnumerationType();
  EnumerationType(const std::string &tag);
  void setBody(SymbolTable &&table) override;
  bool complete() override;
  llvm::Constant *getDefaultValue() override;
};

class EnumerationMemberType : public IntegerType {
 public:
  EnumerationMemberType(EnumerationType *enumType) : mEnumerationType(enumType), IntegerType(32, true) {}
 private:
  EnumerationType *mEnumerationType;
};
#endif //MYCCPILER_TYPES_H

