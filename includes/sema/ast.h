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
  const Token &token;
  void print(int indent) override;
};
class TypedefNameAST : public AST {
 public:
  TypedefNameAST(nt<IdentifierAST> identifier);
  const nt<IdentifierAST> id;
  void print(int indent) override;
  QualifiedType codegen();
};
class TypeQualifierAST : public AST {
 public:
  TypeQualifierAST(Terminal<TypeQualifier> op);
  const Terminal<TypeQualifier> op;
  void print(int indent) override;
  TypeQualifier codegen();
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
  const nt<ExpressionAST> expression;
  void print(int indent) override;
  void codegen(StatementContexts &contexts) override;
};

class InitializerListAST : public AST {
 public:
  InitializerListAST(nts<InitializerAST> initializer);
  const nts<InitializerAST> initializers;
  void print(int indent) override;

};
class InitializerAST : public AST {
 public:
  InitializerAST(nt<AssignmentExpressionAST> assignment_expression);
  InitializerAST(nt<InitializerListAST> initializer_list);
  const nt<AST> ast;
  void print(int indent) override;
};

typedef std::vector<std::pair<nt<DeclaratorAST>, nt<InitializerAST>>> InitDeclarators;
class EnumeratorAST : public AST {
 public:
  EnumeratorAST(nt<IdentifierAST> id);
  EnumeratorAST(nt<IdentifierAST> id, nt<ConstantExpressionAST> constant_expression);
  const nt<IdentifierAST> id;
  const nt<ConstantExpressionAST> constant_expression;
  void print(int indent) override;
  EnumConstSymbol *codegen(EnumerationType *enumerationType, int64_t index);
 private:
  std::unique_ptr<EnumConstSymbol> mSymbol;
};
class EnumeratorListAST : public AST {
 public:
  EnumeratorListAST(nts<EnumeratorAST> enumerator);
  const nts<EnumeratorAST> enumerators;
  void print(int indent) override;
  std::vector<EnumConstSymbol *> codegen(EnumerationType *enumType);
};
class ParameterDeclarationAST : public AST {
 public:
  ParameterDeclarationAST(nt<DeclarationSpecifiersAST> declaration_specifiers, nt<DeclaratorAST> declarator);
  const nt<DeclarationSpecifiersAST> declaration_specifiers;
  nt<DeclaratorAST> declarator;
  void print(int indent) override;
  ISymbol *codegen();
};
class ParameterListAST : public AST {
 public:
  ParameterListAST(nts<ParameterDeclarationAST> parameter_declaration,
                   bool hasMultiple,
                   SymbolTable &table);
  const nts<ParameterDeclarationAST> parameter_declaration;
  SymbolTable &mObjectTable;
  void print(int indent) override;
  std::vector<QualifiedType> codegen();
  bool hasMultiple() const;
 private:
  bool mMultiple;
};
class EnumerationConstantAST : public AST {
 public:
  EnumerationConstantAST(nt<IdentifierAST> id);
  const nt<IdentifierAST> id;
};
class FloatingConstantAST : public AST {
 public:
  FloatingConstantAST(const Token &token);
  enum class Suffix {
    None,
    F,
    L,
  };
  const Token &mToken;
  //TODO use APFloat
  long double value;
  Suffix suffix;
  void print(int indent) override;
};
class CharacterConstantAST : public AST {
 public:
  CharacterConstantAST(const Token &token);
  const Token &mToken;
  char c;
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
  const Token &mToken;
  //TODO use APInt
  unsigned long long value;
  Suffix suffix;
  void print(int indent) override;
};
class AssignmentExpressionAST : public IExpression {
 public:
  AssignmentExpressionAST(nt<ConditionalExpressionAST> conditional_expression);
  AssignmentExpressionAST(nt<ConditionalExpressionAST> conditional_expression,
                          Terminal<AssignmentOp> op,
                          nt<AssignmentExpressionAST> assignment_expression);
  const nt<ConditionalExpressionAST> conditional_expression;
  const std::unique_ptr<Terminal<AssignmentOp>> op;
  const nt<AssignmentExpressionAST> assignment_expression;
  void print(int indent) override;
  Value codegen() override;
  static llvm::Value *eqCodegen(Value &lhs,
                                Value &rhs,
                                AST *lhsAST,
                                AST *rhsAST,
                                bool isInitialization);
};
class PrimaryExpressionAST : public IExpression {
 public:
  PrimaryExpressionAST() : IExpression(Kind::PRIMARY_EXPRESSION) {}
};

class IdentifierPrimaryExpressionAST : public PrimaryExpressionAST {
 public:
  IdentifierPrimaryExpressionAST(nt<IdentifierAST> identifier);
  const nt<IdentifierAST> identifier;
  void print(int indent) override;
  Value codegen() override;
};

class IntegerConstantPrimaryExpressionAST : public PrimaryExpressionAST {
 public:
  IntegerConstantPrimaryExpressionAST(nt<IntegerConstantAST> integer_constant);
  nt<IntegerConstantAST> integer_constant;
  void print(int indent) override;
  Value codegen() override;
};

class FloatingConstantPrimaryExpressionAST : public PrimaryExpressionAST {
 public:
  FloatingConstantPrimaryExpressionAST(nt<FloatingConstantAST> floating_constant);
  nt<FloatingConstantAST> floating_constant;
  void print(int indent) override;
  Value codegen() override;
};

class CharacterConstantPrimaryExpressionAST : public PrimaryExpressionAST {
 public:
  CharacterConstantPrimaryExpressionAST(nt<CharacterConstantAST> character_constant);
  nt<CharacterConstantAST> character_constant;
  void print(int indent) override;
  Value codegen() override;
};

class StringPrimaryExpressionAST : public PrimaryExpressionAST {
 public:
  StringPrimaryExpressionAST(nt<StringAST> string);
  nt<StringAST> string;
  void print(int indent) override;
  Value codegen() override;
 private:
  std::unique_ptr<PointerType> mPointerType;
};

class ExpressionPrimaryExpressionAST : public PrimaryExpressionAST {
 public:
  ExpressionPrimaryExpressionAST(nt<ExpressionAST> expression);
  nt<ExpressionAST> expression;
  void print(int indent) override;
  Value codegen() override;
};

class ArgumentExpressionList : public AST {
 public:
  ArgumentExpressionList(nts<AssignmentExpressionAST> argumentList);
  void print(int indent) override;
  const nts<AssignmentExpressionAST> argumentsList;
  std::vector<Value> codegen();
};
class PostfixExpressionAST : public IExpression {
 public:
  PostfixExpressionAST() : IExpression(Kind::POSTFIX_EXPRESSION) {}
};

class SimplePostfixExpressionAST : public PostfixExpressionAST {
 public:
  SimplePostfixExpressionAST(nt<PrimaryExpressionAST> primary) : primary_expression(std::move(primary)) {}
  nt<PrimaryExpressionAST> primary_expression;
  void print(int indent) override;
  Value codegen() override;
};

class ArrayPostfixExpressionAST : public PostfixExpressionAST {
 public:
  ArrayPostfixExpressionAST(nt<PostfixExpressionAST> postfix_expression, nt<ExpressionAST> expression)
      : postfix_expression(std::move(postfix_expression)), expression(std::move(expression)) {}
  nt<PostfixExpressionAST> postfix_expression;
  nt<ExpressionAST> expression;
  void print(int indent) override;
  Value codegen() override;
 private:
  std::unique_ptr<PointerType> mArrayToPointer;
};

class FunctionPostfixExpressionAST : public PostfixExpressionAST {
 public:
  FunctionPostfixExpressionAST(nt<PostfixExpressionAST> postfix_expression, nt<ArgumentExpressionList> arguments)
      : postfix_expression(std::move(postfix_expression)), argument_expression_list(std::move(arguments)) {}
  nt<PostfixExpressionAST> postfix_expression;
  nt<ArgumentExpressionList> argument_expression_list;
  void print(int indent) override;
  Value codegen();
};

class MemberPostfixExpressionAST : public PostfixExpressionAST {
 public:
  MemberPostfixExpressionAST(nt<PostfixExpressionAST> postfix_expression, nt<IdentifierAST> identifier)
      : postfix_expression(std::move(postfix_expression)), identifier(std::move(identifier)) {}
  nt<PostfixExpressionAST> postfix_expression;
  nt<IdentifierAST> identifier;
  void print(int indent) override;
  Value codegen() override;
};

class PointerMemberPostfixExpressionAST : public PostfixExpressionAST {
 public:
  PointerMemberPostfixExpressionAST(nt<PostfixExpressionAST> postfix_expression, nt<IdentifierAST> identifier)
      : postfix_expression(std::move(postfix_expression)), identifier(std::move(identifier)) {}
  nt<PostfixExpressionAST> postfix_expression;
  nt<IdentifierAST> identifier;
  void print(int indent) override;
  Value codegen() override;
};

class IncrementPostfixExpression : public PostfixExpressionAST {
 public:
  IncrementPostfixExpression(nt<PostfixExpressionAST> postfix_expression) :
      postfix_expression(std::move(postfix_expression)) {}
  nt<PostfixExpressionAST> postfix_expression;
  void print(int indent) override;
  Value codegen() override;
};

class DecrementPostfixExpression : public PostfixExpressionAST {
 public:
  DecrementPostfixExpression(nt<PostfixExpressionAST> postfix_expression) :
      postfix_expression(std::move(postfix_expression)) {}
  nt<PostfixExpressionAST> postfix_expression;
  void print(int indent) override;
  Value codegen() override;
};

class TypeNameAST : public AST {
 public:
  TypeNameAST(nt<SpecifierQualifierAST> specifier, nt<DeclaratorAST> declarator);
  const nt<SpecifierQualifierAST> specifiers;
  const nt<DeclaratorAST> declarator;
  void print(int indent) override;
  QualifiedType codegen();
};
class UnaryExpressionAST : public IExpression {
 public:
  UnaryExpressionAST() : IExpression(Kind::UNARY_EXPRESSION) {}
};

class SimpleUnaryExpressionAST : public UnaryExpressionAST {
 public:
  SimpleUnaryExpressionAST(nt<PostfixExpressionAST> postfixExpression)
      : mPostfixExpression(std::move(postfixExpression)) {}
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<PostfixExpressionAST> mPostfixExpression;
  std::unique_ptr<PointerType> mConvertedPointer;
};

class PrefixIncrementExpressionAST : public UnaryExpressionAST {
 public:
  PrefixIncrementExpressionAST(nt<UnaryExpressionAST> unaryExpressionAST)
      : mUnaryExpression(std::move(unaryExpressionAST)) {}
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<UnaryExpressionAST> mUnaryExpression;
};

class PrefixDecrementExpressionAST : public UnaryExpressionAST {
 public:
  PrefixDecrementExpressionAST(nt<UnaryExpressionAST> unaryExpressionAST)
      : mUnaryExpression(std::move(unaryExpressionAST)) {}
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<UnaryExpressionAST> mUnaryExpression;
};

class UnaryOperatorExpressionAST : public UnaryExpressionAST {
 public:
  UnaryOperatorExpressionAST(Terminal<UnaryOp> op, nt<CastExpressionAST> castExpressionAST)
      : mOp(op), mCastExpression(std::move(castExpressionAST)) {}
  void print(int indent) override;
  Value codegen() override;
 private:
  Terminal<UnaryOp> mOp;
  nt<CastExpressionAST> mCastExpression;
  std::unique_ptr<PointerType> mPointerType;
};

class SizeofUnaryExpressionAST : public UnaryExpressionAST {
 public:
  SizeofUnaryExpressionAST(nt<UnaryExpressionAST> unaryExpressionAST)
      : mAST(std::move(unaryExpressionAST)) {}
  SizeofUnaryExpressionAST(nt<TypeNameAST> typeNameAST)
      : mAST(std::move(typeNameAST)) {}
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<AST> mAST;
};

class CastExpressionAST : public IExpression {
 public:
  CastExpressionAST() : IExpression(Kind::CAST_EXPRESSION) {}
};

class SimpleCastExpressionAST : public CastExpressionAST {
 public:
  SimpleCastExpressionAST(nt<UnaryExpressionAST> unaryExpression)
      : mUnaryExpression(std::move(unaryExpression)) {}
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<UnaryExpressionAST> mUnaryExpression;
};

class RealCastExpressionAST : public CastExpressionAST {
 public:
  RealCastExpressionAST(nt<TypeNameAST> typeName, nt<CastExpressionAST> castExpression)
      : mTypeName(std::move(typeName)), mCastExpression(std::move(castExpression)) {}
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<TypeNameAST> mTypeName;
  nt<CastExpressionAST> mCastExpression;
};

class ExpressionAST : public IExpression {
 public:
  ExpressionAST(nt<ExpressionAST> expression, nt<AssignmentExpressionAST> assignment_expression)
      : IExpression(AST::Kind::EXPRESSION),
        mExpression(std::move(expression)),
        mAssignmentExpression(std::move(assignment_expression)) {};
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<ExpressionAST> mExpression;
  nt<AssignmentExpressionAST> mAssignmentExpression;
};

class IBinaryOperationAST : public IExpression {
 public:
  IBinaryOperationAST() : IExpression(Kind::LOGICAL_OR_EXPRESSION) {}
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
  BinaryOperatorAST(nt<IBinaryOperationAST> left, Terminal<InfixOp> op, nt<IBinaryOperationAST> right)
      : mLeft(std::move(left)), mOp(op), mRight(std::move(right)) {}
  void print(int indent) override;
  Value codegen() override;
  static Value codegen(Value &lhs, Value &rhs, InfixOp op, const AST *lAST, const AST *rAST);
  static std::tuple<Type *, llvm::Value *, llvm::Value *> UsualArithmeticConversions(Value &lhs,
                                                                                     Value &rhs,
                                                                                     const AST *ast);
  static bool UsualArithmeticConversions(Type* lhs, Type* rhs, const AST *ast);
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
  const nt<IBinaryOperationAST> logical_or_expression;
  const nt<ExpressionAST> expression;
  const nt<ConditionalExpressionAST> conditional_expression;
  void print(int indent) override;
  Value codegen() override;
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
  const nt<IdentifierAST> identifier;
  void print(int indent) override;
  const Token *getIdentifier() override;
  ISymbol *codegen(StorageSpecifier storageSpecifier, QualifiedType derivedType) override;
 private:
  std::unique_ptr<ISymbol> mSymbol;
};

class ParenthesedDirectDeclaratorAST : public DirectDeclaratorAST {
 public:
  ParenthesedDirectDeclaratorAST(nt<DeclaratorAST> declarator);
  const nt<DeclaratorAST> declarator;
  void print(int indent) override;
  const Token *getIdentifier() override;
  ISymbol *codegen(StorageSpecifier storageSpecifier, QualifiedType derivedType) override;
};

class ArrayDeclaratorAST : public DirectDeclaratorAST {
 public:
  ArrayDeclaratorAST(nt<DirectDeclaratorAST> directDeclarator, nt<ConstantExpressionAST> constantExpression);
  const nt<DirectDeclaratorAST> directDeclarator;
  const nt<ConstantExpressionAST> constantExpression;
  void print(int indent) override;
  const Token *getIdentifier() override;
  ISymbol *codegen(StorageSpecifier storageSpecifier, QualifiedType derivedType) override;
 private:
  std::unique_ptr<ArrayType> mArrayType;
};

class FunctionDeclaratorAST : public DirectDeclaratorAST {
 public:
  FunctionDeclaratorAST(nt<DirectDeclaratorAST> directDeclarator,
                        nt<ParameterListAST> parameterList);
  const nt<DirectDeclaratorAST> directDeclarator;
  const nt<ParameterListAST> parameterList;
  void print(int indent) override;
  const Token *getIdentifier() override;
  ISymbol *codegen(StorageSpecifier storageSpecifier, QualifiedType derivedType) override;
 private:
  std::unique_ptr<FunctionType> mFunctionType;
};

class PointerAST : public AST {
 public:
  PointerAST(nts<TypeQualifierAST> type_qualifiers, nt<PointerAST> pointer);
  const nts<TypeQualifierAST> type_qualifiers;
  const nt<PointerAST> pointer;
  void print(int indent) override;
  QualifiedType codegen(QualifiedType derivedType);
 private:
  std::unique_ptr<PointerType> mPointerType;
};
class ConstantExpressionAST : public IExpression {
 public:
  ConstantExpressionAST(nt<ConditionalExpressionAST> conditional_expression);
  const nt<ConditionalExpressionAST> conditional_expression;
  void print(int indent) override;
  Value codegen() override;
};
class StructDeclaratorAST : public AST {
 public:
  StructDeclaratorAST(nt<DeclaratorAST> declarator);
  StructDeclaratorAST(nt<DeclaratorAST> declarator, nt<ConstantExpressionAST> constant_expression);
  StructDeclaratorAST(nt<ConstantExpressionAST> constant_expression);
  const nt<DeclaratorAST> declarator;
  const nt<ConstantExpressionAST> constant_expression;
  void print(int indent) override;
  ISymbol *codegen(const QualifiedType &derivedType);
};
class StructDeclaratorListAST : public AST {
 public:
  StructDeclaratorListAST(nts<StructDeclaratorAST> struct_declarators);
  const nts<StructDeclaratorAST> struct_declarators;
  void print(int indent) override;
};
class StructDeclarationAST : public AST {
 public:
  StructDeclarationAST(nt<SpecifierQualifierAST> specifier_qualifier,
                       nt<StructDeclaratorListAST> struct_declarator_list);
  const nt<SpecifierQualifierAST> specifier_qualifier;
  const nt<StructDeclaratorListAST> struct_declarator_list;
  void print(int indent) override;
  std::vector<ObjectSymbol *> codegen();
};
class EnumSpecifierAST : public AST {
 public:
  EnumSpecifierAST(nt<IdentifierAST> identifier, nt<EnumeratorListAST> enumeratorList);
  EnumSpecifierAST(nt<EnumeratorListAST> enumeratorList);
  EnumSpecifierAST(nt<IdentifierAST> identifier);
  const nt<IdentifierAST> id;
  const nt<EnumeratorListAST> enum_list;
  void print(int indent) override;
  EnumerationType *codegen();
 private:
  std::unique_ptr<TagSymbol> mSymbol;
};
class StructOrUnionSpecifierAST : public AST {
 public:
  StructOrUnionSpecifierAST(StructOrUnion type, nt<IdentifierAST> id, nts<StructDeclarationAST> declarations);
  const StructOrUnion bStruct;
  const nt<IdentifierAST> id;
  const nts<StructDeclarationAST> declarations;
  void print(int indent) override;
  const ObjectType *type;
  ObjectType *codegen();
 private:
  std::unique_ptr<TagSymbol> mSymbol;
};
class ProtoTypeSpecifierAST : public AST, public Terminal<ProtoTypeSpecifierOp> {
 public:
  ProtoTypeSpecifierAST(Terminal<ProtoTypeSpecifierOp> specifier);
  const Terminal<ProtoTypeSpecifierOp> specifier;
  void print(int indent) override;
};
class TypeSpecifierAST : public AST {
 public:
  TypeSpecifierAST(Terminal<ProtoTypeSpecifierOp> specifier);
  TypeSpecifierAST(nt<StructOrUnionSpecifierAST> specifier);
  TypeSpecifierAST(nt<EnumSpecifierAST> specifier);
  TypeSpecifierAST(nt<TypedefNameAST> specifier);
  const nt<AST> specifier;
  void print(int indent) override;
};
class TypeSpecifiersAST : public AST {
 public:
  TypeSpecifiersAST(CombinationKind kind, nts<TypeSpecifierAST> specifiers);
  const CombinationKind combination_kind;
  nts<TypeSpecifierAST> type_specifiers;
  bool empty();
  void print(int indent) override;
  ObjectType *type;
  QualifiedType codegen();
};
class SpecifierQualifierAST : public AST {
 public:
  SpecifierQualifierAST(nt<TypeSpecifiersAST> types, nts<TypeQualifierAST> qualifiers);
  nt<TypeSpecifiersAST> types;
  nts<TypeQualifierAST> qualifiers;
  void print(int indent) override;
  const ObjectType *type;
  QualifiedType codegen();
};
class StorageClassSpecifierAST : public AST {
 public:
  StorageClassSpecifierAST(Terminal<StorageSpecifier> storage_speicifier);
  const Terminal<StorageSpecifier> storage_speicifier;
  void print(int indent) override;
};
class DeclaratorAST : public AST {
 public:
  DeclaratorAST(nt<PointerAST> pointer, nt<DirectDeclaratorAST> direct_declarator);
  nt<PointerAST> pointer;
  const nt<DirectDeclaratorAST> direct_declarator;
  void print(int indent) override;
  const Token *getIdentifier() const;
  ISymbol *codegen(StorageSpecifier storageSpecifier, QualifiedType derivedType);
};
class DeclarationSpecifiersAST : public AST {
 public:
  DeclarationSpecifiersAST(ts<StorageSpecifier> storage_specifiers,
                           nt<TypeSpecifiersAST> type_specifiers,
                           nts<TypeQualifierAST> type_qualifiers);
  const ts<StorageSpecifier> storage_specifiers;
  const nt<TypeSpecifiersAST> type_specifiers;
  const nts<TypeQualifierAST> type_qualifiers;
  bool empty();
  void print(int indent) override;
  std::pair<StorageSpecifier, QualifiedType> codegen();
};
class DeclarationAST : public AST {
 public:
  DeclarationAST(nt<DeclarationSpecifiersAST> declaration_specifiers,
                 InitDeclarators init_declarators,
                 SymbolTable &table);
  const nt<DeclarationSpecifiersAST> declaration_specifiers;
  const InitDeclarators init_declarators;
  void print(int indent) override;
  void codegen();
 private:
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
  const nt<DeclarationSpecifiersAST> declaration_spcifiers;
  const nt<DeclaratorAST> declarator;
  const nts<DeclarationAST> declarations;
  const nt<CompoundStatementAST> compound_statement;
  SymbolTable &mLabelTable;
  void print(int indent) override;
  llvm::Value *codegen();

};
class ExternalDeclarationAST : public AST {
 public:
  explicit ExternalDeclarationAST(nt<AST> def);
  const nt<AST> def;
  void print(int indent) override;
  void codegen();
};
class TranslationUnitAST : public AST {
 public:
  TranslationUnitAST(nts<ExternalDeclarationAST> external_declarations,
                     SymbolTable &objectTable,
                     SymbolTable &tagTable);
  const nts<ExternalDeclarationAST> external_declarations;
  void print(int indent) override;
  void codegen();
  //TODO private members
 private:
  SymbolTable &mObjectTable;
  SymbolTable &mTagTable;
};
#endif //MYCCPILER_AST_H
