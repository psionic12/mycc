#ifndef MYCCPILER_VALUE_H
#define MYCCPILER_VALUE_H
#include <llvm/IR/Value.h>
#include <llvm/IR/DerivedTypes.h>
#include "qualifiedType.h"
class Value {
 public:
  Value(QualifiedType qualifiedType, bool lvalue, llvm::Value *value);
  llvm::Value *getValue() const;
  llvm::Value *getPtr() const;
  const Type* getType() const;
  const std::set<TypeQualifier> &getQualifiers() const;
  // this checks if the qualifiers has type "const" (lvalue aspect)
  bool isConst() const;
  bool isVolatile() const;
  // this checks if the llvm value is a constant (rvalue aspect)
  llvm::Constant * isConatant() const;
  llvm::GlobalVariable* isGlobalVariable() const;
  bool isLValue() const;
  bool modifiable() const;
  const QualifiedType &getQualifiedType() const;
 private:
  llvm::Value *mValue;
  const bool mLValue;
  const QualifiedType qualifiedType;
};
#endif //MYCCPILER_VALUE_H
