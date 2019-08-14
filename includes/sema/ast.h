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
    return kind;
  }
  const int getProduction() const {
    return productionId;
  }
  virtual const char *toString();
  virtual void print(int indent = 0);
  virtual void printIndent(int indent);
  std::pair<const Token &, const Token &> involvedTokens();
  virtual ~AST() = default;
  const Token *mLeftMost;
  const Token *mRightMost;
 private:
  const Kind kind;
  //the id of which production
  const int productionId;
 protected:
  static llvm::LLVMContext sContext;
  static llvm::Module sModule;
  static llvm::IRBuilder<> sBuilder;
  SymbolTable *sObjectTable;
  SymbolTable *sTagTable;
  SymbolTable *sLabelTable;
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
    std::cout << token.getValue() << std::endl;
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

class SpecifierQualifierAST;
class DeclaratorAST;
class ConstantExpressionAST;
class CastExpressionAST;
class ExpressionAST;
class ConditionalExpressionAST;
class UnaryExpressionAST;
class PointerAST;
class DeclarationSpecifiersAST;
class ParameterTypeListAST;
class AssignmentExpressionAST;
class InitializerAST;
class CompoundStatementAST;
class StatementAST;
class IExpression {
 public:
  class Value {
   public:
    Value(QualifiedType qualifiedType, bool lvalue, llvm::Value *value)
        : qualifiedType(std::move(qualifiedType)), lvalue(lvalue), value(value) {}
    const QualifiedType qualifiedType;
    bool lvalue = false;
    llvm::Value *value = nullptr;
  };
  virtual Value codegen() = 0;
};
class StringAST : public AST {
 public:
  StringAST(const Token &token);
  std::unique_ptr<Type> mType;
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
class JumpStatementAST : public AST {
 public:
  JumpStatementAST(nt<IdentifierAST> id);
  JumpStatementAST(bool is_continue);
  JumpStatementAST(nt<ExpressionAST>);
  const nt<IdentifierAST> id;
  const nt<ExpressionAST> expression;
  void print(int indent) override;
};
class IterationStatementAST : public AST {
 public:
  IterationStatementAST(nt<ExpressionAST> expression, nt<StatementAST> statement);
  IterationStatementAST(nt<StatementAST> statement, nt<ExpressionAST> expression);
  IterationStatementAST(nt<ExpressionAST> expression,
                        nt<ExpressionAST> condition_expression,
                        nt<ExpressionAST> step_expression,
                        nt<StatementAST> statement);
  const nt<ExpressionAST> expression;
  const nt<StatementAST> statement;
  const nt<ExpressionAST> condition_expression;
  const nt<ExpressionAST> step_expression;
  void print(int indent) override;
};
class SelectionStatementAST : public AST {
 public:
  SelectionStatementAST(nt<ExpressionAST> expression, nt<StatementAST> statement, bool is_if);
  SelectionStatementAST(nt<ExpressionAST> expression, nt<StatementAST> statement, nt<StatementAST> else_statement);
  const nt<ExpressionAST> expression;
  const nt<StatementAST> statement;
  const nt<StatementAST> else_statement;
  void print(int indent) override;
};
class ExpressionStatementAST : public AST {
 public:
  ExpressionStatementAST(nt<ExpressionAST> expression);
  const nt<ExpressionAST> expression;
  void print(int indent) override;
};
class LabeledStatementAST : public AST {
 public:
  LabeledStatementAST(nt<IdentifierAST> id, nt<StatementAST> statement);
  LabeledStatementAST(nt<ConstantExpressionAST> constant_expression, nt<StatementAST> statement);
  LabeledStatementAST(nt<StatementAST> statement);
  const nt<IdentifierAST> id;
  const nt<StatementAST> statement;
  const nt<ConstantExpressionAST> constant_expression;
  void print(int indent) override;
};
class StatementAST : public AST {
 public:
  StatementAST(nt<LabeledStatementAST>);
  StatementAST(nt<ExpressionStatementAST>);
  StatementAST(nt<CompoundStatementAST>);
  StatementAST(nt<SelectionStatementAST>);
  StatementAST(nt<IterationStatementAST>);
  StatementAST(nt<JumpStatementAST>);
  const nt<AST> ast;
  void print(int indent) override;
};
class InitializerListAST : public AST {
 public:
  InitializerListAST(nts<InitializerAST> initializer);
  const nts<InitializerAST> initializer;
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
  EnumConstSymbol *codegen(const EnumerationType *enumerationType, int64_t index);
 private:
  std::unique_ptr<EnumConstSymbol> mSymbol;
};
class EnumeratorListAST : public AST {
 public:
  EnumeratorListAST(nts<EnumeratorAST> enumerator);
  const nts<EnumeratorAST> enumerators;
  void print(int indent) override;
  std::vector<EnumConstSymbol *> codegen();
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
  ParameterListAST(nts<ParameterDeclarationAST> parameter_declaration, SymbolTable &table);
  const nts<ParameterDeclarationAST> parameter_declaration;
  SymbolTable &mObjectTable;
  void print(int indent) override;
  std::vector<QualifiedType> codegen();
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
class AssignmentExpressionAST : public AST, public IExpression {
 public:
  AssignmentExpressionAST(nt<ConditionalExpressionAST> conditional_expression);
  AssignmentExpressionAST(nt<ConditionalExpressionAST> conditional_expression,
                          Terminal<AssignmentOp> op,
                          nt<AssignmentExpressionAST> assignment_expression);
  //TODO check is LHS a lvalue
  const nt<ConditionalExpressionAST> conditional_expression;
  const std::unique_ptr<Terminal<AssignmentOp>> op;
  const nt<AssignmentExpressionAST> assignment_expression;
  void print(int indent) override;
};
class PrimaryExpressionAST : public AST, public IExpression {
 public:
  PrimaryExpressionAST() : AST(Kind::PRIMARY_EXPRESSION) {}
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
  std::vector<IExpression::Value> codegen();
};
class PostfixExpressionAST : public AST, public IExpression {
 public:
  PostfixExpressionAST() : AST(Kind::POSTFIX_EXPRESSION) {}
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
};
class UnaryExpressionAST : public AST, public IExpression {
 public:
  UnaryExpressionAST() : AST(Kind::UNARY_EXPRESSION) {}
};

class SimpleUnaryExpressionAST : public UnaryExpressionAST {
 public:
  SimpleUnaryExpressionAST(nt<PostfixExpressionAST> postfixExpression)
      : mPostfixExpression(std::move(postfixExpression)) {}
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<PostfixExpressionAST> mPostfixExpression;
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
  UnaryOperatorExpressionAST(Terminal<UnaryOp> op, nt<UnaryExpressionAST> unaryExpressionAST)
      : mOp(op), mUnaryExpression(std::move(unaryExpressionAST)) {}
  void print(int indent) override;
  Value codegen() override;
 private:
  Terminal<UnaryOp> mOp;
  nt<UnaryExpressionAST> mUnaryExpression;
};

class SizeofUnaryExpressionAST : public UnaryExpressionAST {
 public:
  SizeofUnaryExpressionAST(nt<UnaryExpressionAST> unaryExpressionAST)
      : mUnaryExpression(std::move(unaryExpressionAST)) {}
  void print(int indent) override;
  Value codegen() override;
 private:
  nt<UnaryExpressionAST> mUnaryExpression;
};

class CastExpressionAST : public AST {
 public:
  CastExpressionAST(nt<UnaryExpressionAST> unary_expression);
  CastExpressionAST(nt<TypeNameAST> type_name, nt<CastExpressionAST> cast_expression);
  const nt<UnaryExpressionAST> unary_expression;
  const nt<TypeNameAST> type_name;
  const nt<CastExpressionAST> cast_expression;
  void print(int indent) override;
};
class ExpressionAST : public AST, public IExpression {
 public:
  ExpressionAST(nts<AssignmentExpressionAST> assignment_expression);
  const nts<AssignmentExpressionAST> assignment_expression;
  void print(int indent) override;
};
class LogicalOrExpressionAST : public AST {
 public:
  LogicalOrExpressionAST(nt<LogicalOrExpressionAST> left, Terminal<InfixOp> op, nt<LogicalOrExpressionAST> right);
  LogicalOrExpressionAST(nt<CastExpressionAST> leaf);
  const nt<AST> left;
  const std::unique_ptr<Terminal<InfixOp>> op;
  const nt<LogicalOrExpressionAST> right;
  void print(int indent) override;
};
class ConditionalExpressionAST : public AST {
 public:
  ConditionalExpressionAST(nt<LogicalOrExpressionAST> logical_or_expression);
  ConditionalExpressionAST(nt<LogicalOrExpressionAST> logical_or_expression,
                           nt<ExpressionAST> expression,
                           nt<ConditionalExpressionAST> conditional_expression);
  const nt<LogicalOrExpressionAST> logical_or_expression;
  const nt<ExpressionAST> expression;
  const nt<ConditionalExpressionAST> conditional_expression;
  void print(int indent) override;
};
class ParameterTypeListAST : public AST {
 public:
  ParameterTypeListAST(nt<ParameterListAST> parameter_list, bool hasMultiple);
  const nt<ParameterListAST> parameter_list;
  void print(int indent) override;
};
class DirectDeclaratorAST : public AST {
 public:
  DirectDeclaratorAST();
  virtual const Token *getIdentifier() = 0;
  virtual ISymbol *codegen(StorageSpecifier storageSpecifier, const QualifiedType &derivedType) = 0;
  virtual bool isAbstract() const = 0;
};

class SimpleDirectDeclaratorAST : public DirectDeclaratorAST {
 public:
  SimpleDirectDeclaratorAST(nt<IdentifierAST> identifier);
  const nt<IdentifierAST> identifier;
  void print(int indent) override;
  const Token *getIdentifier() override;
  ISymbol *codegen(StorageSpecifier storageSpecifier, const QualifiedType &derivedType) override;
  bool isAbstract() const override;
 private:
  std::unique_ptr<ISymbol> mSymbol;
};

class ParenthesedDirectDeclaratorAST : public DirectDeclaratorAST {
 public:
  ParenthesedDirectDeclaratorAST(nt<DeclaratorAST> declarator);
  const nt<DeclaratorAST> declarator;
  void print(int indent) override;
  const Token *getIdentifier() override;
  bool isAbstract() const override;
  ISymbol *codegen(StorageSpecifier storageSpecifier, const QualifiedType &derivedType) override;
};

class ArrayDeclaratorAST : public DirectDeclaratorAST {
 public:
  ArrayDeclaratorAST(nt<DirectDeclaratorAST> directDeclarator, nt<ConstantExpressionAST> constantExpression);
  const nt<DirectDeclaratorAST> directDeclarator;
  const nt<ConstantExpressionAST> constantExpression;
  void print(int indent) override;
  const Token *getIdentifier() override;
  ISymbol *codegen(StorageSpecifier storageSpecifier, const QualifiedType &derivedType) override;
 private:
  std::unique_ptr<ArrayType> mArrayType;
};

class FunctionDeclaratorAST : public DirectDeclaratorAST {
 public:
  FunctionDeclaratorAST(nt<DirectDeclaratorAST> directDeclarator, nt<ParameterListAST> parameterList);
  const nt<DirectDeclaratorAST> directDeclarator;
  const nt<ParameterListAST> parameterList;
  void print(int indent) override;
  const Token *getIdentifier() override;
  ISymbol *codegen(StorageSpecifier storageSpecifier, const QualifiedType &derivedType) override;
 private:
  std::unique_ptr<FunctionType> mFunctionType;
};

class PointerAST : public AST {
 public:
  PointerAST(nts<TypeQualifierAST> type_qualifiers, nt<PointerAST> pointer);
  const nts<TypeQualifierAST> type_qualifiers;
  const nt<PointerAST> pointer;
  void print(int indent) override;
  const QualifiedType codegen(const QualifiedType &derivedType);
 private:
  std::unique_ptr<PointerType> mPointerType;
};
class ConstantExpressionAST : public AST, public IExpression {
 public:
  ConstantExpressionAST(nt<ConditionalExpressionAST> conditional_expression);
  const nt<ConditionalExpressionAST> conditional_expression;
  void print(int indent) override;
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
  CompoundType *codegen();
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
  const ObjectType *type;
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
  const nt<PointerAST> pointer;
  const nt<DirectDeclaratorAST> direct_declarator;
  void print(int indent) override;
  const Token *getIdentifier() const;
  bool isAbstract() const;
  ISymbol *codegen(StorageSpecifier storageSpecifier, const QualifiedType &derivedType);
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
  std::pair<const Terminal<StorageSpecifier> &, QualifiedType> codegen();
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
};
class CompoundStatementAST : public AST {
 public:
  CompoundStatementAST(nts<DeclarationAST> declarations,
                       nts<StatementAST> statements,
                       SymbolTable &objectTable,
                       SymbolTable &tagTable);
  const nts<DeclarationAST> declarations;
  const nts<StatementAST> statements;
  SymbolTable &mObjectTable;
  SymbolTable &mTagTable;
  void print(int indent) override;
  llvm::Value *codegen();
};
class FunctionDefinitionAST : public AST {
 public:
  FunctionDefinitionAST(nt<DeclarationSpecifiersAST> declaration_spcifiers,
                        nt<DeclaratorAST> declarator,
                        nts<DeclarationAST> declarations,
                        nt<CompoundStatementAST> compound_statement,
                        SymbolTable &lableTable);
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
  SymbolTable &mObjectTable;
  SymbolTable &mTagTable;
  void print(int indent) override;
  void codegen();
};
#endif //MYCCPILER_AST_H
