#ifndef MYCCPILER_VALUE_H
#define MYCCPILER_VALUE_H
#include <llvm/IR/Value.h>
#include <llvm/IR/DerivedTypes.h>
#include "qualifiedType.h"
class Value {
 public:
  Value(QualifiedType qualifiedType, bool lvalue, llvm::Value *value);
  const QualifiedType qualifiedType;
  const bool lvalue;
  llvm::Value *getValue() const;
  llvm::Value *getPtr() const;
  bool modifiable() const;
 private:
  llvm::Value *mValue;
};
#endif //MYCCPILER_VALUE_H
