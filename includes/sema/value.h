#ifndef MYCCPILER_VALUE_H
#define MYCCPILER_VALUE_H
#include <llvm/IR/Value.h>
#include <llvm/IR/DerivedTypes.h>
#include "qualifiedType.h"
class Value {
 public:
  Value(QualifiedType qualifiedType, bool lvalue, llvm::Value *value);
  llvm::Value *getValue();
  llvm::Value *getPtr();
  Type * getType();
  const std::set<TypeQualifier> &getQualifiers() const;
  // this checks if the qualifiers has type "const" (lvalue aspect)
  bool isConst() const;
  bool isVolatile() const;
  // this checks if the llvm value is a constant data (rvalue aspect)
  llvm::Constant * isConatantData();
  llvm::GlobalVariable* isGlobalVariable();
  bool isLValue() const;
  bool modifiable();
  QualifiedType &getQualifiedType();
 private:
  llvm::Value *mValue;
  const bool mLValue;
  QualifiedType mQualifiedType;
};
#endif //MYCCPILER_VALUE_H
