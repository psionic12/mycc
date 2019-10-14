/** this Abstract Sytex Tree data struction follow the Backus-Naur Form
 *  https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm
 */
#ifndef MYCCPILER_AST_H
#define MYCCPILER_AST_H
#include <vector>
#include <memory>
#include <sema/operator.h>
#include <tokens/token.h>
#include <iostream>
#include <tokens/combination_kind.h>
#include <llvm/IR/Module.h>
#include "types.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "qualifiedType.h"
#include "symbol_tables.h"
#include "value.h"
#include "statement_context.h"
class SymbolTable;
class SemaException : public std::exception {
 public:
  SemaException(std::string error, const Token &token);
  SemaException(std::string error, const Token &start, const Token &end);
  SemaException(std::string error, std::pair<const Token &, const Token &> range);
  const char *what() const noexcept override;
 private:
  std::string error;
};
class AST {
 public:
  enum class Kind {
    TRANSLATION_UNIT,
    EXTERNAL_DECLARATION,
    FUNCTION_DEFINITION,
    DECLARATION,
    DECLARATION_SPECIFIER,
    DECLARATOR,
    COMPOUND_STATEMENT,
    STORAGE_CLASS_SPECIFIER,
    TYPE_SPECIFIER,
    TYPE_SPECIFIERS,
    TYPE_QUALIFIER,
    PROTO_TYPE_SPECIFIER,
    STRUCT_OR_UNION_SPECIFIER,
    ENUM_SPECIFIER,
    TYPEDEF_NAME,
    STRUCT_OR_UNION,
    IDENTIFIER,
    STRUCT_DECLARATION,
    SPECIFIER_QUALIFIER,
    STRUCT_DECLARATOR_LIST,
    STRUCT_DECLARATOR,
    CONSTANT_EXPRESSION,
    POINTER,
    DIRECT_DECLARATOR,
    PARAMETER_TYPE_LIST,
    CONDITIONAL_EXPRESSION,
    EXPRESSION,
    CAST_EXPRESSION,
    UNARY_EXPRESSION,
    LOGICAL_OR_EXPRESSION,
    TYPE_NAME,
    POSTFIX_EXPRESSION,
    UNARY_OPERATOR,
    PRIMARY_EXPRESSION,
    ASSIGNMENT_EXPRESSION,
    CONSTANT,
    STRING,
    INTEGER_CONSTANT,
    CHARACTER_CONSTANT,
    FLOATING_CONSTANT,
    ENUMERATION_CONSTANT,
    PARAMETER_LIST,
    PARAMETER_DECLARATION,
    ENUMERATOR_LIST,
    ENUMERATOR,
    INIT_DECLARATOR,
    INITIALIZER,
    INITIALIZER_LIST,
    STATEMENT,
    LABELED_STATEMENT,
    EXPRESSION_STATEMENT,
    SELECTION_STATEMENT,
    ITERATION_STATEMENT,
    JUMP_STATEMENT,
    ARGUMENT_EXPRESSION_LIST,
  };
  AST(Kind kind, int id = 0);
  const Kind getKind() const {
    return mKind;
  }
  const int getProduction() const {
    return mProductionId;
  }
  virtual const char *toString();
  virtual void print(int indent = 0);
  virtual void printIndent(int indent);
  std::pair<const Token &, const Token &> involvedTokens() const;
  virtual ~AST() = default;
  friend class Parser;
  static llvm::LLVMContext &getContext();
  static llvm::Module *getModule();
  static llvm::IRBuilder<> &getBuilder();
  static SymbolTables &getTables();
  static std::unique_ptr<llvm::Module> takeModule();
  static std::unique_ptr<llvm::LLVMContext> takeContext();
 private:
  const Kind mKind;
  //the id of which production
  const int mProductionId;
  static SymbolTables mTables;
 protected:
  static std::unique_ptr<llvm::LLVMContext> sContext;
  static std::unique_ptr<llvm::Module> sModule;
  static llvm::IRBuilder<> sBuilder;
  static SymbolTable *sObjectTable;
  static SymbolTable *sTagTable;
  static SymbolTable *sLabelTable;
  const Token *mLeftMost;
  const Token *mRightMost;
  static bool sIsInitializerList;
};
template<typename T>
class Terminal {
 public:
  Terminal(T type, const Token &token) : token(token), type(type) {}
  const Token &token;
  const T type;
  void print(int indent) const {
    for (int i = 0; i < indent; ++i) {
      std::cout << "\t";
    }
    std::cout << static_cast<int>(type) << " ";
    std::cout << token.toString() << std::endl;
  }
};
template<typename T>
class ts : public std::vector<Terminal<T>> {
 public:
  void print(int indent = 0) const {
    for (const auto &terminal : *this) {
      terminal.print(indent);
    }
  }
};
// nt is short for None Terminal
template<typename T>
using nt = std::unique_ptr<T>;

template<typename T>
class nts : public std::vector<std::unique_ptr<T>> {
 public:
  void print(int indent = 0) const {
    for (const auto &noneTerminal : *this) {
      noneTerminal->print(indent);
    }
  }
};

class StatementContext;

class SpecifierQualifierAST;
class DeclaratorAST;
class ConstantExpressionAST;
class CastExpressionAST;
class ExpressionAST;
class ConditionalExpressionAST;
class UnaryExpressionAST;
class PointerAST;
class DeclarationSpecifiersAST;
class AssignmentExpressionAST;
class InitializerAST;
class CompoundStatementAST;
class StatementAST;
class IExpression : public AST {
 public:
  IExpression(AST::Kind kind) : AST(kind) {}
  virtual Value codegen() = 0;
};
class StringAST : public AST {
 public:
  StringAST(const Token &token);
  ArrayType *getType();
  const Token &getToken() const;
 private:
  std::unique_ptr<ArrayType> mType;
  const Token &mToken;
};
class IdentifierAST : public AST {
 public:
  IdentifierAST(const Token &token);
  void print(int indent) override;
 private:
  const Token &mToken;
 public:
  const Token &getToken() const;
};
class TypedefNameAST : public AST {
 public:
  TypedefNameAST(nt<IdentifierAST> identifier);
  void print(int indent) override;
  QualifiedType codegen();
 private:
  const nt<IdentifierAST> mIdentifier;
};
class TypeQualifierAST : public AST {
 public:
  TypeQualifierAST(Terminal<TypeQualifier> op);
  void print(int indent) override;
  TypeQualifier codegen();
 private:
  friend class DeclarationSpecifiersAST;
  friend class SpecifierQualifierAST;
  const Terminal<TypeQualifier> mOp;
};

class StatementAST : public AST {
 public:
  StatementAST(Kind kind) : AST(kind) {}
  virtual void codegen(StatementContexts &contexts) = 0;
};

class LabeledStatementAST : public StatementAST {
 public:
  LabeledStatementAST(nt<StatementAST> statement);
 protected:
  nt<StatementAST> mStatement;
};

class IdentifierLabeledStatementAST : public LabeledStatementAST {
 public:
  IdentifierLabeledStatementAST(nt<IdentifierAST> id, nt<StatementAST> statement);
  void print(int indent) override;
  void codegen(StatementContexts &contexts) override;
 private:
  nt<IdentifierAST> mIdentifier;
  std::unique_ptr<LabelSymbol> mLabelSymbol;
};
class CaseLabeledStatementAST : public LabeledStatementAST {
 public:
  CaseLabeledStatementAST(nt<ConstantExpressionAST> constantExpression, nt<StatementAST> statement);
  void print(int indent) override;
  void codegen(StatementContexts &contexts) override;
 private:
  nt<ConstantExpressionAST> mConstantExpression;
};
class DefaultLabeledStatementAST : public LabeledStatementAST {
 public:
  DefaultLabeledStatementAST(nt<StatementAST> statement);
  void codegen(StatementContexts &contexts) override;
  void print(int indent) override;
};

class JumpStatementAST : public StatementAST {
 public:
  JumpStatementAST();
};

class GotoJumpStatementAST : public JumpStatementAST {
 public:
  GotoJumpStatementAST(nt<IdentifierAST> identifier);
  void print(int indent) override;
  void codegen(StatementContexts &contexts) override;
 private:
  nt<IdentifierAST> mIdentifier;
  std::unique_ptr<LabelSymbol> mLabelSymbol;
};

class ContinueJumpStatementAST : public JumpStatementAST {
 public:
  void codegen(StatementContexts &contexts) override;
};

class BreakJumpStatementAST : public JumpStatementAST {
 public:
  void codegen(StatementContexts &contexts) override;
};

class ReturnJumpStatementAST : public JumpStatementAST {
 public:
  ReturnJumpStatementAST(nt<ExpressionAST> expression);
  void print(int indent) override;
  void codegen(StatementContexts &contexts) override;
 private:
  nt<ExpressionAST> mExpression;
};

class IterationStatementAST : public StatementAST {
 public:
  IterationStatementAST();
};

class WhileIterationStatementAST : public IterationStatementAST {
 public:
  WhileIterationStatementAST(nt<ExpressionAST> expression, nt<StatementAST> statement);
  void codegen(StatementContexts &contexts) override;
 private:
  nt<ExpressionAST> mExpression;
  nt<StatementAST> mStatement;
};

class DoIterationStatementAST : public IterationStatementAST {
 public:
  DoIterationStatementAST(nt<StatementAST> statement, nt<ExpressionAST> expression)
      : mExpression(std::move(expression)), mStatement(std::move(statement)) {}
  void codegen(StatementContexts &contexts) override;
 private:
  nt<ExpressionAST> mExpression;
  nt<StatementAST> mStatement;
};

class ForIterationStatementAST : public IterationStatementAST {
 public:
  ForIterationStatementAST(nt<ExpressionAST> expression,
                           nt<ExpressionAST> condition_expression,
                           nt<ExpressionAST> step_expression,
                           nt<StatementAST> statement)
      : mExpression(std::move(expression)),
        mConditionExpression(std::move(condition_expression)),
        mStepExpression(std::move(step_expression)),
        mStatement(std::move(statement)) {}
  void codegen(StatementContexts &contexts) override;
 private:
  nt<ExpressionAST> mExpression;
  nt<ExpressionAST> mConditionExpression;
  nt<ExpressionAST> mStepExpression;
  nt<StatementAST> mStatement;
};

class SelectionStatementAST : public StatementAST {
 public:
  SelectionStatementAST();
};

class IfSelectionStatementAST : public SelectionStatementAST {
 public:
  IfSelectionStatementAST(nt<ExpressionAST> expression, nt<StatementAST> statement, nt<StatementAST> elseStatement);
  void print(int indent) override;
  void codegen(StatementContexts &contexts) override;
 private:
  const nt<ExpressionAST> mExpression;
  const nt<StatementAST> mStatement;
  const nt<StatementAST> mElseStatement;
};

class SwitchSelectionStatementAST : public SelectionStatementAST {
 public:
  SwitchSelectionStatementAST(nt<ExpressionAST> expression, nt<StatementAST> statement);
  void print(int indent) override;
  void codegen(StatementContexts &contexts) override;
 private:
  const nt<ExpressionAST> mExpression;
  const nt<StatementAST> mStatement;
};

class ExpressionStatementAST : public StatementAST {
 public:
  ExpressionStatementAST(nt<ExpressionAST> expression);
  void print(int indent) override;
  void codegen(StatementContexts &contexts) override;
 private:
  const nt<ExpressionAST> mExpression;
};

class InitializerListAST : public AST {
 public:
  InitializerListAST(nts<InitializerAST> initializer);
  void print(int indent) override;
  const nts<InitializerAST> & getInitializers();
 private:
  const nts<InitializerAST> mInitializers;
};
class InitializerAST : public AST {
 public:
  InitializerAST(nt<AssignmentExpressionAST> assignment_expression);
  InitializerAST(nt<InitializerListAST> initializer_list);
  void print(int indent) override;
  AST* getInitializer();
 private:
  const nt<AST> mAST;
};

typedef std::vector<std::pair<nt<DeclaratorAST>, nt<InitializerAST>>> InitDeclarators;
class EnumeratorAST : public AST {
 public:
  EnumeratorAST(nt<IdentifierAST> id);
  EnumeratorAST(nt<IdentifierAST> id, nt<ConstantExpressionAST> constant_expression);
  void print(int indent) override;
  EnumConstSymbol *codegen(EnumerationType *enumerationType, int64_t index);
 private:
  std::unique_ptr<EnumConstSymbol> mSymbol;
  const nt<IdentifierAST> mIdentifier;
  const nt<ConstantExpressionAST> mConstantExpression;
};
class EnumeratorListAST : public AST {
 public:
  EnumeratorListAST(nts<EnumeratorAST> enumerator);
  void print(int indent) override;
  std::vector<EnumConstSymbol *> codegen(EnumerationType *enumType);
 private:
  const nts<EnumeratorAST> mEnumerators;
};
class ParameterDeclarationAST : public AST {
 public:
  ParameterDeclarationAST(nt<DeclarationSpecifiersAST> declaration_specifiers, nt<DeclaratorAST> declarator);
  void print(int indent) override;
  ISymbol *codegen();
 private:
  const nt<DeclarationSpecifiersAST> mDeclarationSpecifiers;
  nt<DeclaratorAST> mDeclarator;
};
class ParameterListAST : public AST {
 public:
  ParameterListAST(nts<ParameterDeclarationAST> parameter_declaration,
                   bool hasMultiple,
                   SymbolTable &table);
  void print(int indent) override;
  std::vector<QualifiedType> codegen();
  bool hasMultiple() const;
 private:
  friend class FunctionDefinitionAST;
  friend class FunctionDeclaratorAST;
  bool mMultiple;
  const nts<ParameterDeclarationAST> mParameterDeclaration;
  SymbolTable &mObjectTable;
};
class EnumerationConstantAST : public AST {
 public:
  EnumerationConstantAST(nt<IdentifierAST> id);
 private:
  const nt<IdentifierAST> mIdentifier;
};
class FloatingConstantAST : public AST {
 public:
  FloatingConstantAST(const Token &token);
  enum class Suffix {
    None,
    F,
    L,
  };
  void print(int indent) override;
 private:
  friend class FloatingConstantPrimaryExpressionAST;
  const Token &mToken;
  //TODO use APFloat
  long double mValue;
  Suffix mSuffix;
};
class CharacterConstantAST : public AST {
 public:
  CharacterConstantAST(const Token &token);
 private:
  friend class CharacterConstantPrimaryExpressionAST;
  const Token &mToken;
  char mChar;
};
class IntegerConstantAST : public AST {
 public:
  IntegerConstantAST(const Token &token);
  enum class Suffix {
    None,
    U,
    L,
    UL,
    LL,
    ULL,
  };
  void print(int indent) override;
 private:
  friend class IntegerConstantPrimaryExpressionAST;
  const Token &mToken;
  //TODO use APInt
  unsigned long long mValue;
  Suffix mSuffix;
};
class AssignmentExpressionAST : public IExpression {
 public:
  AssignmentExpressionAST(nt<ConditionalExpressionAST> conditional_expression);
  AssignmentExpressionAST(nt<ConditionalExpressionAST> conditional_expression,
                          Terminal<AssignmentOp> op,
                          nt<AssignmentExpressionAST> assignment_expression);
  void print(int indent) override;
  Value codegen() override;
  static llvm::Value *eqCodegen(Value &lhs,
                                Value &rhs,
                                AST *lhsAST,
                                AST *rhsAST,
                                bool isInitialization);
 private:
  const nt<ConditionalExpressionAST> mConditionalExpression;
  const std::unique_ptr<Terminal<AssignmentOp>> mOp;
  const nt<AssignmentExpressionAST> mAssignmentExpression;
};
class PrimaryExpressionAST : public IExpression {
 public:
  PrimaryExpressionAST() : IExpression(Kind::PRIMARY_EXPRESSION) {}
};

class IdentifierPrimaryExpressionAST : public PrimaryExpressionAST {
 public:
  IdentifierPrimaryExpressionAST(nt<IdentifierAST> identifier);
  void print(int indent) override;
  Value codegen() override;
 private:
  const nt<IdentifierAST> mIdentifier;
};

class IntegerConstantPrimaryExpressionAST : public PrimaryExpressionAST {
 public:
  IntegerConstantPrimaryExpressionAST(nt<IntegerConstantAST> integer_constant);
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<IntegerConstantAST> mIntegerConstant;
};

class FloatingConstantPrimaryExpressionAST : public PrimaryExpressionAST {
 public:
  FloatingConstantPrimaryExpressionAST(nt<FloatingConstantAST> floating_constant);
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<FloatingConstantAST> mFloatingConstant;
};

class CharacterConstantPrimaryExpressionAST : public PrimaryExpressionAST {
 public:
  CharacterConstantPrimaryExpressionAST(nt<CharacterConstantAST> character_constant);
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<CharacterConstantAST> mCharacterConstant;
};

class StringPrimaryExpressionAST : public PrimaryExpressionAST {
 public:
  StringPrimaryExpressionAST(nt<StringAST> string);
  void print(int indent) override;
  Value codegen() override;
 private:
  std::unique_ptr<PointerType> mPointerType;
  nt<StringAST> mStringAST;
};

class ExpressionPrimaryExpressionAST : public PrimaryExpressionAST {
 public:
  ExpressionPrimaryExpressionAST(nt<ExpressionAST> expression);
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<ExpressionAST> mExpression;
};

class ArgumentExpressionList : public AST {
 public:
  ArgumentExpressionList(nts<AssignmentExpressionAST> argumentList);
  void print(int indent) override;
  std::vector<Value> codegen();
 private:
  const nts<AssignmentExpressionAST> mArgumentsList;
};
class PostfixExpressionAST : public IExpression {
 public:
  PostfixExpressionAST();
};

class SimplePostfixExpressionAST : public PostfixExpressionAST {
 public:
  SimplePostfixExpressionAST(nt<PrimaryExpressionAST> primary);
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<PrimaryExpressionAST> mPrimaryExpression;
};

class ArrayPostfixExpressionAST : public PostfixExpressionAST {
 public:
  ArrayPostfixExpressionAST(nt<PostfixExpressionAST> postfix_expression, nt<ExpressionAST> expression);
  void print(int indent) override;
  Value codegen() override;
 private:
  std::unique_ptr<PointerType> mArrayToPointer;
  nt<PostfixExpressionAST> mPostfixExpression;
  nt<ExpressionAST> mExpression;
};

class FunctionPostfixExpressionAST : public PostfixExpressionAST {
 public:
  FunctionPostfixExpressionAST(nt<PostfixExpressionAST> postfix_expression, nt<ArgumentExpressionList> arguments);
  void print(int indent) override;
  Value codegen();
 private:
  nt<PostfixExpressionAST> mPostfixExpression;
  nt<ArgumentExpressionList> mArgumentExpressionList;
};

class MemberPostfixExpressionAST : public PostfixExpressionAST {
 public:
  MemberPostfixExpressionAST(nt<PostfixExpressionAST> postfix_expression, nt<IdentifierAST> identifier)
      : mPostfixExpression(std::move(postfix_expression)), mIdentifier(std::move(identifier)) {}

  void print(int indent) override;
  Value codegen() override;
 private:
  nt<PostfixExpressionAST> mPostfixExpression;
  nt<IdentifierAST> mIdentifier;
};

class PointerMemberPostfixExpressionAST : public PostfixExpressionAST {
 public:
  PointerMemberPostfixExpressionAST(nt<PostfixExpressionAST> postfix_expression, nt<IdentifierAST> identifier);
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<PostfixExpressionAST> mPostfixExpression;
  nt<IdentifierAST> mIdentifier;
};

class IncrementPostfixExpression : public PostfixExpressionAST {
 public:
  IncrementPostfixExpression(nt<PostfixExpressionAST> postfix_expression);
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<PostfixExpressionAST> mPostfixExpression;
};

class DecrementPostfixExpression : public PostfixExpressionAST {
 public:
  DecrementPostfixExpression(nt<PostfixExpressionAST> postfix_expression);
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<PostfixExpressionAST> mPostfixExpression;
};

class TypeNameAST : public AST {
 public:
  TypeNameAST(nt<SpecifierQualifierAST> specifier, nt<DeclaratorAST> declarator);
  void print(int indent) override;
  QualifiedType codegen();
 private:
  const nt<SpecifierQualifierAST> mSpecifiers;
  const nt<DeclaratorAST> mDeclarator;
};
class UnaryExpressionAST : public IExpression {
 public:
  UnaryExpressionAST() : IExpression(Kind::UNARY_EXPRESSION) {}
};

class SimpleUnaryExpressionAST : public UnaryExpressionAST {
 public:
  SimpleUnaryExpressionAST(nt<PostfixExpressionAST> postfixExpression);
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<PostfixExpressionAST> mPostfixExpression;
  std::unique_ptr<PointerType> mConvertedPointer;
};

class PrefixIncrementExpressionAST : public UnaryExpressionAST {
 public:
  PrefixIncrementExpressionAST(nt<UnaryExpressionAST> unaryExpressionAST);
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<UnaryExpressionAST> mUnaryExpression;
};

class PrefixDecrementExpressionAST : public UnaryExpressionAST {
 public:
  PrefixDecrementExpressionAST(nt<UnaryExpressionAST> unaryExpressionAST);
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<UnaryExpressionAST> mUnaryExpression;
};

class UnaryOperatorExpressionAST : public UnaryExpressionAST {
 public:
  UnaryOperatorExpressionAST(Terminal<UnaryOp> op, nt<CastExpressionAST> castExpressionAST);
  void print(int indent) override;
  Value codegen() override;
 private:
  Terminal<UnaryOp> mOp;
  nt<CastExpressionAST> mCastExpression;
  std::unique_ptr<PointerType> mPointerType;
};

class SizeofUnaryExpressionAST : public UnaryExpressionAST {
 public:
  SizeofUnaryExpressionAST(nt<UnaryExpressionAST> unaryExpressionAST);
  SizeofUnaryExpressionAST(nt<TypeNameAST> typeNameAST);
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<AST> mAST;
};

class CastExpressionAST : public IExpression {
 public:
  CastExpressionAST();
};

class SimpleCastExpressionAST : public CastExpressionAST {
 public:
  SimpleCastExpressionAST(nt<UnaryExpressionAST> unaryExpression);
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<UnaryExpressionAST> mUnaryExpression;
};

class RealCastExpressionAST : public CastExpressionAST {
 public:
  RealCastExpressionAST(nt<TypeNameAST> typeName, nt<CastExpressionAST> castExpression);
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<TypeNameAST> mTypeName;
  nt<CastExpressionAST> mCastExpression;
};

class ExpressionAST : public IExpression {
 public:
  ExpressionAST(nt<ExpressionAST> expression, nt<AssignmentExpressionAST> assignment_expression);;
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<ExpressionAST> mExpression;
  nt<AssignmentExpressionAST> mAssignmentExpression;
};

class IBinaryOperationAST : public IExpression {
 public:
  IBinaryOperationAST();
};

class SimpleBinaryOperatorAST : public IBinaryOperationAST {
 public :
  SimpleBinaryOperatorAST(nt<CastExpressionAST> castExpression) : mCastExpression(std::move(castExpression)) {}
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<CastExpressionAST> mCastExpression;
};

class BinaryOperatorAST : public IBinaryOperationAST {
 public:
  BinaryOperatorAST(nt<IBinaryOperationAST> left, Terminal<InfixOp> op, nt<IBinaryOperationAST> right);
  void print(int indent) override;
  Value codegen() override;
  static Value codegen(Value &lhs, Value &rhs, InfixOp op, const AST *lAST, const AST *rAST);
  static std::tuple<Type *, llvm::Value *, llvm::Value *> UsualArithmeticConversions(Value &lhs,
                                                                                     Value &rhs,
                                                                                     const AST *ast);
  static bool UsualArithmeticConversions(Type *lhs, Type *rhs, const AST *ast);
 protected:
  nt<IBinaryOperationAST> mLeft;
  Terminal<InfixOp> mOp;
  nt<IBinaryOperationAST> mRight;
};

class LogicalBinaryOperatorAST : public BinaryOperatorAST {
 public:
  using BinaryOperatorAST::BinaryOperatorAST;
  Value codegen() override;
};
class ConditionalExpressionAST : public IExpression {
 public:
  ConditionalExpressionAST(nt<IBinaryOperationAST> logical_or_expression);
  ConditionalExpressionAST(nt<IBinaryOperationAST> logical_or_expression,
                           nt<ExpressionAST> expression,
                           nt<ConditionalExpressionAST> conditional_expression);
  void print(int indent) override;
  Value codegen() override;
 private:
  const nt<IBinaryOperationAST> mLogicalOrExpression;
  const nt<ExpressionAST> mExpression;
  const nt<ConditionalExpressionAST> mConditionalExpression;
};
class DirectDeclaratorAST : public AST {
 public:
  DirectDeclaratorAST();
  virtual const Token *getIdentifier() = 0;
  virtual ISymbol *codegen(StorageSpecifier storageSpecifier, QualifiedType derivedType) = 0;
};

class SimpleDirectDeclaratorAST : public DirectDeclaratorAST {
 public:
  SimpleDirectDeclaratorAST(nt<IdentifierAST> identifier);
  void print(int indent) override;
  const Token *getIdentifier() override;
  ISymbol *codegen(StorageSpecifier storageSpecifier, QualifiedType derivedType) override;
 private:
  std::unique_ptr<ISymbol> mSymbol;
  const nt<IdentifierAST> mIdentifier;
};

class ParenthesedDirectDeclaratorAST : public DirectDeclaratorAST {
 public:
  ParenthesedDirectDeclaratorAST(nt<DeclaratorAST> declarator);
  void print(int indent) override;
  const Token *getIdentifier() override;
  ISymbol *codegen(StorageSpecifier storageSpecifier, QualifiedType derivedType) override;
 private:
  const nt<DeclaratorAST> mDeclarator;
};

class ArrayDeclaratorAST : public DirectDeclaratorAST {
 public:
  ArrayDeclaratorAST(nt<DirectDeclaratorAST> directDeclarator, nt<ConstantExpressionAST> constantExpression);
  void print(int indent) override;
  const Token *getIdentifier() override;
  ISymbol *codegen(StorageSpecifier storageSpecifier, QualifiedType derivedType) override;
 private:
  const nt<DirectDeclaratorAST> mDirectDeclarator;
  const nt<ConstantExpressionAST> mConstantExpression;
  std::unique_ptr<ArrayType> mArrayType;
};

class FunctionDeclaratorAST : public DirectDeclaratorAST {
 public:
  FunctionDeclaratorAST(nt<DirectDeclaratorAST> directDeclarator,
                        nt<ParameterListAST> parameterList);
  void print(int indent) override;
  const Token *getIdentifier() override;
  ISymbol *codegen(StorageSpecifier storageSpecifier, QualifiedType derivedType) override;
 private:
  friend class FunctionDefinitionAST;
  const nt<DirectDeclaratorAST> mDirectDeclarator;
  const nt<ParameterListAST> mParameterList;
  std::unique_ptr<FunctionType> mFunctionType;
};

class PointerAST : public AST {
 public:
  PointerAST(nts<TypeQualifierAST> type_qualifiers, nt<PointerAST> pointer);
  void print(int indent) override;
  QualifiedType codegen(QualifiedType derivedType);
 private:
  std::unique_ptr<PointerType> mPointerType;
  const nts<TypeQualifierAST> mTypeQualifiers;
  const nt<PointerAST> mPointer;
};
class ConstantExpressionAST : public IExpression {
 public:
  ConstantExpressionAST(nt<ConditionalExpressionAST> conditional_expression);
  void print(int indent) override;
  Value codegen() override;
 private:
  const nt<ConditionalExpressionAST> mConditionalExpression;
};
class StructDeclaratorAST : public AST {
 public:
  StructDeclaratorAST(nt<DeclaratorAST> declarator);
  StructDeclaratorAST(nt<DeclaratorAST> declarator, nt<ConstantExpressionAST> constant_expression);
  StructDeclaratorAST(nt<ConstantExpressionAST> constant_expression);
  void print(int indent) override;
  ISymbol *codegen(const QualifiedType &derivedType);
 private:
  const nt<DeclaratorAST> mDeclarator;
  const nt<ConstantExpressionAST> mConstantExpression;
};
class StructDeclaratorListAST : public AST {
 public:
  StructDeclaratorListAST(nts<StructDeclaratorAST> struct_declarators);
  void print(int indent) override;
 private:
  friend class StructDeclarationAST;
  const nts<StructDeclaratorAST> mStructDeclarators;
};
class StructDeclarationAST : public AST {
 public:
  StructDeclarationAST(nt<SpecifierQualifierAST> specifier_qualifier,
                       nt<StructDeclaratorListAST> struct_declarator_list);
  void print(int indent) override;
  std::vector<ObjectSymbol *> codegen();
 private:
  const nt<SpecifierQualifierAST> mSpecifierQualifier;
  const nt<StructDeclaratorListAST> mStructDeclaratorList;
};
class EnumSpecifierAST : public AST {
 public:
  EnumSpecifierAST(nt<IdentifierAST> identifier, nt<EnumeratorListAST> enumeratorList);
  EnumSpecifierAST(nt<EnumeratorListAST> enumeratorList);
  EnumSpecifierAST(nt<IdentifierAST> identifier);
  void print(int indent) override;
  EnumerationType *codegen();
 private:
  std::unique_ptr<TagSymbol> mSymbol;
  const nt<IdentifierAST> mIdentifier;
  const nt<EnumeratorListAST> mEnumList;
};
class StructOrUnionSpecifierAST : public AST {
 public:
  StructOrUnionSpecifierAST(StructOrUnion type, nt<IdentifierAST> id, nts<StructDeclarationAST> declarations);
  void print(int indent) override;
  ObjectType *codegen();
 private:
  std::unique_ptr<TagSymbol> mSymbol;
  const StructOrUnion mIsStruct;
  const nt<IdentifierAST> mIdentifier;
  const nts<StructDeclarationAST> mDeclarations;
  const ObjectType *mType;
};
class ProtoTypeSpecifierAST : public AST, public Terminal<ProtoTypeSpecifierOp> {
 public:
  ProtoTypeSpecifierAST(Terminal<ProtoTypeSpecifierOp> specifier);
  void print(int indent) override;
 private:
  const Terminal<ProtoTypeSpecifierOp> mSpecifier;
};
class TypeSpecifierAST : public AST {
 public:
  TypeSpecifierAST(Terminal<ProtoTypeSpecifierOp> specifier);
  TypeSpecifierAST(nt<StructOrUnionSpecifierAST> specifier);
  TypeSpecifierAST(nt<EnumSpecifierAST> specifier);
  TypeSpecifierAST(nt<TypedefNameAST> specifier);
  void print(int indent) override;
 private:
  friend class TypeSpecifiersAST;
  const nt<AST> mSpecifier;
};
class TypeSpecifiersAST : public AST {
 public:
  TypeSpecifiersAST(CombinationKind kind, nts<TypeSpecifierAST> specifiers);
  bool empty();
  void print(int indent) override;
  QualifiedType codegen();
 private:
  const CombinationKind mCombinationKind;
  nts<TypeSpecifierAST> mTypeSpecifiers;
  ObjectType *mType;
};
class SpecifierQualifierAST : public AST {
 public:
  SpecifierQualifierAST(nt<TypeSpecifiersAST> types, nts<TypeQualifierAST> qualifiers);
  void print(int indent) override;
  QualifiedType codegen();
 private:
  nt<TypeSpecifiersAST> mTypes;
  nts<TypeQualifierAST> mQualifiers;
  const ObjectType *mType;
};
class StorageClassSpecifierAST : public AST {
 public:
  StorageClassSpecifierAST(Terminal<StorageSpecifier> storage_speicifier);
  void print(int indent) override;
 private:
  const Terminal<StorageSpecifier> mStorageSpeicifier;
};
class DeclaratorAST : public AST {
 public:
  DeclaratorAST(nt<PointerAST> pointer, nt<DirectDeclaratorAST> direct_declarator);
  void print(int indent) override;
  const Token *getIdentifier() const;
  ISymbol *codegen(StorageSpecifier storageSpecifier, QualifiedType derivedType);
 private:
  friend class FunctionDefinitionAST;
  nt<PointerAST> mPointer;
  const nt<DirectDeclaratorAST> mDirectDeclarator;
};
class DeclarationSpecifiersAST : public AST {
 public:
  DeclarationSpecifiersAST(ts<StorageSpecifier> storage_specifiers,
                           nt<TypeSpecifiersAST> type_specifiers,
                           nts<TypeQualifierAST> type_qualifiers);
  bool empty();
  void print(int indent) override;
  std::pair<StorageSpecifier, QualifiedType> codegen();
 private:
  friend class FunctionDefinitionAST;
  friend class DeclarationAST;
  const ts<StorageSpecifier> mStorageSpecifiers;
  const nt<TypeSpecifiersAST> mTypeSpecifiers;
  const nts<TypeQualifierAST> mTypeQualifiers;
};
class DeclarationAST : public AST {
 public:
  DeclarationAST(nt<DeclarationSpecifiersAST> declaration_specifiers,
                 InitDeclarators init_declarators,
                 SymbolTable &table);
  void print(int indent) override;
  void codegen();
 private:
  const nt<DeclarationSpecifiersAST> mDeclarationSpecifiers;
  const InitDeclarators mInitDeclarators;
  static TypedefSymbol sFakeTypedef;
};
class CompoundStatementAST : public StatementAST {
 public:
  CompoundStatementAST(nts<AST> asts,
                       SymbolTable &objectTable,
                       SymbolTable &tagTable);
  friend class FunctionDefinitionAST;
 private:
  nts<AST> mASTs;
  SymbolTable &mObjectTable;
  SymbolTable &mTagTable;
  void print(int indent) override;
  void codegen(StatementContexts &contexts) override;
};
class FunctionDefinitionAST : public AST {
 public:
  FunctionDefinitionAST(nt<DeclarationSpecifiersAST> declaration_spcifiers,
                        nt<DeclaratorAST> declarator,
                        nts<DeclarationAST> declarations,
                        nt<CompoundStatementAST> compound_statement,
                        SymbolTable &labelTable);
  void print(int indent) override;
  llvm::Value *codegen();
 private:
  const nt<DeclarationSpecifiersAST> mDeclarationSpcifiers;
  const nt<DeclaratorAST> mDeclarator;
  const nts<DeclarationAST> mDeclarations;
  const nt<CompoundStatementAST> mCompoundStatement;
  SymbolTable &mLabelTable;

};
class ExternalDeclarationAST : public AST {
 public:
  explicit ExternalDeclarationAST(nt<AST> def);
  void print(int indent) override;
  void codegen();
 private:
  nt<AST> mDef;
};
class TranslationUnitAST : public AST {
 public:
  TranslationUnitAST(nts<ExternalDeclarationAST> external_declarations,
                     SymbolTable &objectTable,
                     SymbolTable &tagTable);
  void print(int indent) override;
  void codegen();
 private:
  const nts<ExternalDeclarationAST> mExternalDeclarations;
  SymbolTable &mObjectTable;
  SymbolTable &mTagTable;
};
#endif //MYCCPILER_AST_H
