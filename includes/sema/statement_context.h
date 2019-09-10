#ifndef MYCCPILER_STATEMENT_CONTEXT_H
#define MYCCPILER_STATEMENT_CONTEXT_H

#include "types.h"

/// these classes are just information for statements like case, default, return, break and continue whose behavior is
/// highly depends on the outside statement
/// because I design the abstract syntax tree as one direction, the child node do not know it's father, so I have to
/// design these classes and pass them to the child when codegen
class StatementContext {
 public:
  virtual ~StatementContext() = default;
};

class FunctionContext : public StatementContext {
 public:
  FunctionContext(const FunctionType *functionTy) : mFunctionType(functionTy) {}
  const FunctionType *getFunctionType() const {
    return mFunctionType;
  }
 private:
  const FunctionType *mFunctionType;
};

class SwitchContext : public StatementContext {
 public:
  SwitchContext(llvm::SwitchInst *switchInst, llvm::BasicBlock *endBlock)
      : mSwitchInst(switchInst), mBreakBB(endBlock) {}
 private:
  llvm::SwitchInst *mSwitchInst;
 public:
  llvm::SwitchInst *getSwitchInst() const {
    return mSwitchInst;
  }
  llvm::BasicBlock *getBreakBB() const {
    return mBreakBB;
  }
 private:
  llvm::BasicBlock *mBreakBB;
};

class LoopContext : public StatementContext {
 public:
  LoopContext(llvm::BasicBlock *continueBlock, llvm::BasicBlock *endBlock)
      : mBreakBB(endBlock), mContinueBB(continueBlock) {}
  llvm::BasicBlock *getBreakBB() const {
    return mBreakBB;
  }
  llvm::BasicBlock *getContinueBB() const {
    return mContinueBB;
  }
 private:
  llvm::BasicBlock *mContinueBB;
  llvm::BasicBlock *mBreakBB;
};

class StatementContexts {
 public:
  StatementContexts(llvm::Function *containingFunction) : mContainingFunction(containingFunction) {}
  llvm::Function *getContainingFunction() const {
    return mContainingFunction;
  }
  void add(std::unique_ptr<StatementContext> &&context) {
    mContexts.push_back(std::move(context));
  }
  // find the closest required context by type, return null if not found any.
  template<typename T>
  T *getLastContext() {
    for (auto it = mContexts.rbegin(); it != mContexts.rend(); ++it) {
      T *context = dynamic_cast<T *>(it->get());
      if (context) {
        return context;
      } else {
        continue;
      }
    }
    return nullptr;
  }
 private:
  llvm::Function *mContainingFunction;
  std::list<std::unique_ptr<StatementContext>> mContexts;
};

#endif //MYCCPILER_STATEMENT_CONTEXT_H
