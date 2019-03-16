/** this Abstract Sytex Tree data struction follow the Backus-Naur Form
 *  https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm
 */
#ifndef MYCCPILER_AST_H
#define MYCCPILER_AST_H
#include <vector>
#include <memory>
#include <tokens/token.h>

namespace mycc {
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
    TYPE_QUALIFIER,
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
    LOGICAL_OR_EXPRESSION,
    EXPRESSION,
    LOGICAL_AND_EXPRESSION,
    INCLUSIVE_OR_EXPRESSION,
    EXCLUSIVE_OR_EXPRESSION,
    AND_EXPRESSION,
    EQUALITY_EXPRESSION,
    RELATIONAL_EXPRESSION,
    SHIFT_EXPRESSION,
    ADDITIVE_EXPRESSION,
    MULTIPLICATIVE_EXPRESSION,
    CAST_EXPRESSION,
    UNARY_EXPRESSION,
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
    ASSIGNMENT_OPERATOR,
    ABSTRACT_DECLARATOR,
    PARAMETER_LIST,
    PARAMETER_DECLARATION,
    DIRECT_ABSTRACT_DECLARATOR,
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
  };
  AST(Kind kind, int id = 0) : kind(kind), pro_id(id) {}
  const Kind getKind() const {
    return kind;
  }
  const int getPro_id() const {
    return pro_id;
  }
 private:
  const Kind kind;
  //the id of which production
  const int pro_id;
};
// nt is short for None Terminal
template<typename T>
using nt = std::unique_ptr<T>;
// nts is short for None TerminalS
template<typename T>
using nts = std::vector<nt<T>>;
class SpecifierQualifierAST;
class DeclaratorAST;
class ConstantExpressionAST;
class CastExpressionAST;
class ExpressionAST;
class ConditionalExpressionAST;
class UnaryExpressionAST;
class PointerAST;
class DeclarationSpecifierAST;
class AbstractDeclaratorAST;
class ParameterTypeListAST;
class AssignmentExpressionAST;
class InitializerAST;
class CompoundStatementAST;
class StatementAST;
class StringAST : public AST { public:StringAST(std::string value) : AST(AST::Kind::STRING) {}};
class IdentifierAST : public AST {
 public:
  IdentifierAST(std::string name, std::string value)
      : AST(AST::Kind::IDENTIFIER), name(std::move(name)), value(std::move(value)) {}
 private:
  std::string name;
  std::string value;
};
class AssignmentOperatorAST : public AST {
 public:
  AssignmentOperatorAST(TokenKind)
      : AST(AST::Kind::ASSIGNMENT_OPERATOR) {}
};
class StructOrUnionAST : public AST {
 public:
  StructOrUnionAST(TokenKind token_kind) : AST(AST::Kind::STRUCT_OR_UNION), token_kind(token_kind) {}
 private:
  TokenKind token_kind;
};
class TypedefNameAST : public AST {
 public:
  TypedefNameAST(nt<IdentifierAST> identifier) : AST(AST::Kind::TYPEDEF_NAME), id(std::move(identifier)) {}
 private:
  nt<IdentifierAST> id;
};
class TypeQualifierAST : public AST {
 public:
  TypeQualifierAST(TokenKind kind) : AST(AST::Kind::TYPE_QUALIFIER), token_kind(kind) {}
 private:
  TokenKind token_kind;
};
class UnaryOperatorAST : public AST { public:UnaryOperatorAST(TokenKind) : AST(AST::Kind::UNARY_OPERATOR) {}};
class JumpStatementAST : public AST {
 public:
  enum class JumpType : int {
    CTN = 1,  // continue
    BRK = 2,  // break
  };
  JumpStatementAST(nt<IdentifierAST>) : AST(AST::Kind::JUMP_STATEMENT, 0) {}
  JumpStatementAST(JumpType type) : AST(AST::Kind::JUMP_STATEMENT, static_cast<int>(type)) {}
  JumpStatementAST(nt<ExpressionAST>) : AST(AST::Kind::JUMP_STATEMENT, 3) {}
};
class IterationStatementAST : public AST {
 public:
  IterationStatementAST(nt<ExpressionAST>, nt<StatementAST>) : AST(AST::Kind::ITERATION_STATEMENT, 0) {}
  IterationStatementAST(nt<StatementAST>, nt<ExpressionAST>) : AST(AST::Kind::ITERATION_STATEMENT, 1) {}
  IterationStatementAST(nt<ExpressionAST>, nt<ExpressionAST>, nt<ExpressionAST>, nt<StatementAST>)
      : AST(AST::Kind::ITERATION_STATEMENT, 2) {}
};
class SelectionStatementAST : public AST {
 public:
  SelectionStatementAST(nt<ExpressionAST>, nt<StatementAST>, bool true_for_if_false_for_switch)
      : AST(AST::Kind::SELECTION_STATEMENT, true_for_if_false_for_switch ? 0 : 2) {}
  SelectionStatementAST(nt<ExpressionAST>, nt<StatementAST>, nt<StatementAST>)
      : AST(AST::Kind::SELECTION_STATEMENT, 1) {}
  SelectionStatementAST(nt<ExpressionAST>, nt<StatementAST>) : AST(AST::Kind::SELECTION_STATEMENT, 2) {}
};
class ExpressionStatementAST : public AST {
 public:
  ExpressionStatementAST(nt<ExpressionAST>)
      : AST(AST::Kind::EXPRESSION_STATEMENT) {}
};
class LabeledStatementAST : public AST {
 public:
  LabeledStatementAST(nt<IdentifierAST>, nt<StatementAST>) : AST(AST::Kind::LABELED_STATEMENT, 0) {}
  LabeledStatementAST(nt<ConstantExpressionAST>, nt<StatementAST>) : AST(AST::Kind::LABELED_STATEMENT, 1) {}
  LabeledStatementAST(nt<StatementAST>) : AST(AST::Kind::LABELED_STATEMENT, 2) {}
};
class StatementAST : public AST {
 public:
  StatementAST(nt<LabeledStatementAST>) : AST(AST::Kind::STATEMENT, 0) {}
  StatementAST(nt<ExpressionStatementAST>) : AST(AST::Kind::STATEMENT, 1) {}
  StatementAST(nt<CompoundStatementAST>) : AST(AST::Kind::STATEMENT, 2) {}
  StatementAST(nt<SelectionStatementAST>) : AST(AST::Kind::STATEMENT, 3) {}
  StatementAST(nt<IterationStatementAST>) : AST(AST::Kind::STATEMENT, 4) {}
  StatementAST(nt<JumpStatementAST>) : AST(AST::Kind::STATEMENT, 5) {}
};
class InitializerListAST : public AST {
 public:
  InitializerListAST(nt<InitializerAST>) : AST(AST::Kind::INITIALIZER_LIST, 0) {}
  InitializerListAST(nt<InitializerListAST>, nt<InitializerAST>) : AST(AST::Kind::INITIALIZER_LIST, 1) {}
};
class InitializerAST : public AST {
 public:
  InitializerAST(nt<AssignmentExpressionAST>) : AST(AST::Kind::INITIALIZER, 0) {}
  InitializerAST(nt<InitializerListAST>, bool multiple) : AST(AST::Kind::INITIALIZER, multiple ? 2 : 1) {}
};
class InitDeclaratorAST : public AST {
 public:
  InitDeclaratorAST(nt<DeclaratorAST>) : AST(AST::Kind::INIT_DECLARATOR, 0) {}
  InitDeclaratorAST(nt<DeclaratorAST>, nt<InitializerAST>) : AST(AST::Kind::INIT_DECLARATOR, 1) {}
};
class EnumeratorAST : public AST {
 public:
  EnumeratorAST(nt<IdentifierAST>) : AST(AST::Kind::ENUMERATOR, 0) {}
  EnumeratorAST(nt<IdentifierAST>, nt<ConstantExpressionAST>) : AST(AST::Kind::ENUMERATOR, 1) {}
};
class EnumeratorListAST : public AST {
 public:
  EnumeratorListAST(nt<EnumeratorAST>) : AST(AST::Kind::ENUMERATOR_LIST, 0) {}
  EnumeratorListAST(nt<EnumeratorListAST>, nt<EnumeratorAST>) : AST(AST::Kind::ENUMERATOR_LIST, 1) {}
};
class DirectAbstractDeclaratorAST : public AST {
 public:
  DirectAbstractDeclaratorAST(nt<AbstractDeclaratorAST>)
      : AST(AST::Kind::DIRECT_ABSTRACT_DECLARATOR, 0) {}
  DirectAbstractDeclaratorAST(nt<DirectAbstractDeclaratorAST>, nt<ConstantExpressionAST>)
      : AST(AST::Kind::DIRECT_ABSTRACT_DECLARATOR, 1) {}
  DirectAbstractDeclaratorAST(nt<DirectAbstractDeclaratorAST>, nt<ParameterTypeListAST>)
      : AST(AST::Kind::DIRECT_ABSTRACT_DECLARATOR, 2) {}
};
class ParameterDeclarationAST : public AST {
 public:
  ParameterDeclarationAST(nts<DeclarationSpecifierAST>, nt<DeclaratorAST>)
      : AST(AST::Kind::PARAMETER_DECLARATION, 0) {}
  ParameterDeclarationAST(nts<DeclarationSpecifierAST>, nt<AbstractDeclaratorAST>)
      : AST(AST::Kind::PARAMETER_DECLARATION, 1) {}
  ParameterDeclarationAST(nts<DeclarationSpecifierAST>)
      : AST(AST::Kind::PARAMETER_DECLARATION, 2) {}
};
class ParameterListAST : public AST {
 public:
  ParameterListAST(nt<ParameterDeclarationAST>) : AST(AST::Kind::PARAMETER_LIST, 0) {}
  ParameterListAST(nt<ParameterListAST>, nt<ParameterDeclarationAST>) : AST(AST::Kind::PARAMETER_LIST, 1) {}
};
class AbstractDeclaratorAST : public AST {
 public:
  AbstractDeclaratorAST(nt<PointerAST>) : AST(AST::Kind::ABSTRACT_DECLARATOR, 0) {}
  AbstractDeclaratorAST(nt<PointerAST>, nt<DirectAbstractDeclaratorAST>) : AST(AST::Kind::ABSTRACT_DECLARATOR, 1) {}
  AbstractDeclaratorAST(nt<DirectAbstractDeclaratorAST>) : AST(AST::Kind::ABSTRACT_DECLARATOR, 2) {}
};
class EnumerationConstantAST : public AST { public:EnumerationConstantAST() : AST(AST::Kind::ENUMERATION_CONSTANT) {}};
class FloatingConstantAST : public AST { public:FloatingConstantAST() : AST(AST::Kind::FLOATING_CONSTANT) {}};
class CharacterConstantAST : public AST { public:CharacterConstantAST() : AST(AST::Kind::CHARACTER_CONSTANT) {}};
class IntegerConstantAST : public AST { public:IntegerConstantAST() : AST(AST::Kind::INTEGER_CONSTANT) {}};
class ConstantAST : public AST {
 public:
  ConstantAST(nt<IntegerConstantAST>) : AST(AST::Kind::CONSTANT, 0) {}
  ConstantAST(nt<CharacterConstantAST>) : AST(AST::Kind::CONSTANT, 1) {}
  ConstantAST(nt<FloatingConstantAST>) : AST(AST::Kind::CONSTANT, 2) {}
  ConstantAST(nt<EnumerationConstantAST>) : AST(AST::Kind::CONSTANT, 3) {}
};
class AssignmentExpressionAST : public AST {
 public:
  AssignmentExpressionAST(nt<ConditionalExpressionAST>)
      : AST(AST::Kind::ASSIGNMENT_EXPRESSION, 0) {}
  AssignmentExpressionAST(nt<UnaryExpressionAST>, nt<AssignmentOperatorAST>, nt<AssignmentExpressionAST>)
      : AST(AST::Kind::ASSIGNMENT_EXPRESSION, 1) {}
};
class PrimaryExpressionAST : public AST {
 public:
  PrimaryExpressionAST(nt<IdentifierAST>) : AST(AST::Kind::PRIMARY_EXPRESSION, 0) {}
  PrimaryExpressionAST(nt<ConstantAST>) : AST(AST::Kind::PRIMARY_EXPRESSION, 1) {}
  PrimaryExpressionAST(nt<StringAST>) : AST(AST::Kind::PRIMARY_EXPRESSION, 2) {}
  PrimaryExpressionAST(nt<ExpressionAST>) : AST(AST::Kind::PRIMARY_EXPRESSION, 3) {}
};
class PostfixExpressionAST : public AST {
 public:
  enum class RelationalType : int {
    DOT = 3,
    ARROW = 4,
    PLUSPLUS = 5,
    MINMIN = 6,

  };
  PostfixExpressionAST(nt<PrimaryExpressionAST>) : AST(AST::Kind::POSTFIX_EXPRESSION, 0) {}
  PostfixExpressionAST(nt<PostfixExpressionAST>, nt<ExpressionAST>) : AST(AST::Kind::POSTFIX_EXPRESSION, 1) {}
  PostfixExpressionAST(nt<PrimaryExpressionAST>, nt<AssignmentExpressionAST>) : AST(AST::Kind::POSTFIX_EXPRESSION, 2) {}
  PostfixExpressionAST(nt<PrimaryExpressionAST>, nt<IdentifierAST>, RelationalType type)
      : AST(AST::Kind::POSTFIX_EXPRESSION, static_cast<int>(type)) {}
  PostfixExpressionAST(nt<PrimaryExpressionAST>, RelationalType type)
      : AST(AST::Kind::POSTFIX_EXPRESSION, static_cast<int>(type)) {}
};
class TypeNameAST : public AST {
 public:
  TypeNameAST(nts<SpecifierQualifierAST>, nts<AbstractDeclaratorAST>)
      : AST(AST::Kind::TYPE_NAME) {}
};
class UnaryExpressionAST : public AST {
 public:
  enum class PrefixType : int {
    PLUSPLUS = 1,
    MINMIN = 2,
    SIZE_OF = 4,
  };
  UnaryExpressionAST(nt<PostfixExpressionAST>) : AST(AST::Kind::UNARY_EXPRESSION, 0) {}
  UnaryExpressionAST(nt<UnaryExpressionAST>, PrefixType type)
      : AST(AST::Kind::UNARY_EXPRESSION, static_cast<int>(type)) {}
  UnaryExpressionAST(nt<UnaryOperatorAST>, nt<CastExpressionAST>) : AST(AST::Kind::UNARY_EXPRESSION) {}
  UnaryExpressionAST(nt<TypeNameAST>) : AST(AST::Kind::UNARY_EXPRESSION) {}
};
class CastExpressionAST : public AST {
 public:
  CastExpressionAST(nt<UnaryExpressionAST>) : AST(AST::Kind::CAST_EXPRESSION, 0) {}
  CastExpressionAST(nt<TypeNameAST>, nt<CastExpressionAST>) : AST(AST::Kind::CAST_EXPRESSION, 1) {}
};
class MultiplicativeExpressionAST : public AST {
 public:
  MultiplicativeExpressionAST(nt<CastExpressionAST>)
      : AST(AST::Kind::MULTIPLICATIVE_EXPRESSION, 0) {}
  enum class RelationalType : int {
    MUL = 1,
    DIV = 2,
    MOD = 3,
  };
  MultiplicativeExpressionAST(nt<MultiplicativeExpressionAST>, nt<CastExpressionAST>, RelationalType type)
      : AST(AST::Kind::MULTIPLICATIVE_EXPRESSION, static_cast<int>(type)) {}
};
class AdditiveExpressionAST : public AST {
 public:
  AdditiveExpressionAST(nt<MultiplicativeExpressionAST>) : AST(AST::Kind::ADDITIVE_EXPRESSION, 0) {}
  enum class RelationalType : int {
    ADD = 1,
    MIN = 2,
  };
  AdditiveExpressionAST(nt<AdditiveExpressionAST>, nt<MultiplicativeExpressionAST>, RelationalType type)
      : AST(AST::Kind::ADDITIVE_EXPRESSION, static_cast<int>(type)) {}
};
class ShiftExpressionAST : public AST {
 public:
  ShiftExpressionAST(nt<AdditiveExpressionAST>) : AST(AST::Kind::SHIFT_EXPRESSION, 0) {}
  enum class RelationalType : int {
    LTLT = 1,
    GTGT = 2,
  };
  ShiftExpressionAST(nt<ShiftExpressionAST>, nt<AdditiveExpressionAST>, RelationalType type)
      : AST(AST::Kind::SHIFT_EXPRESSION, static_cast<int>(type)) {}
};
class RelationalExpressionAST : public AST {
 public:
  RelationalExpressionAST(nt<ShiftExpressionAST>)
      : AST(AST::Kind::RELATIONAL_EXPRESSION, 0) {}
  enum class RelationalType : int {
    LT = 1,
    GT = 2,
    LTEQ = 3,
    GTEQ = 4,
  };
  RelationalExpressionAST(nt<RelationalExpressionAST>, nt<ShiftExpressionAST>, RelationalType type)
      : AST(AST::Kind::RELATIONAL_EXPRESSION, static_cast<int>(type)) {}
};
class EqualityExpressionAST : public AST {
 public:
  EqualityExpressionAST(nt<RelationalExpressionAST>) : AST(AST::Kind::EQUALITY_EXPRESSION, 0) {}
  EqualityExpressionAST(nt<EqualityExpressionAST>, nt<RelationalExpressionAST>, bool equal)
      : AST(AST::Kind::EQUALITY_EXPRESSION, equal ? 1 : 2) {}

};
class AndExpressionAST : public AST {
 public:
  AndExpressionAST(nt<EqualityExpressionAST>) : AST(AST::Kind::AND_EXPRESSION, 0) {}
  AndExpressionAST(nt<AndExpressionAST>, nt<EqualityExpressionAST>) : AST(AST::Kind::AND_EXPRESSION, 1) {}
};
class ExclusiveOrExpressionAST : public AST {
 public:
  ExclusiveOrExpressionAST(nt<AndExpressionAST>)
      : AST(AST::Kind::EXCLUSIVE_OR_EXPRESSION, 0) {}
  ExclusiveOrExpressionAST(nt<ExclusiveOrExpressionAST>, nt<AndExpressionAST>)
      : AST(AST::Kind::EXCLUSIVE_OR_EXPRESSION, 1) {}
};
class InclusiveOrExpressionAST : public AST {
 public:
  InclusiveOrExpressionAST(nt<ExclusiveOrExpressionAST>)
      : AST(AST::Kind::INCLUSIVE_OR_EXPRESSION, 0) {}
  InclusiveOrExpressionAST(nt<InclusiveOrExpressionAST>, nt<ExclusiveOrExpressionAST>)
      : AST(AST::Kind::INCLUSIVE_OR_EXPRESSION, 1) {}
};

class LogicalAndExpressionAST : public AST {
 public:
  LogicalAndExpressionAST(nt<InclusiveOrExpressionAST> inclusive_or_expression)
      : AST(AST::Kind::LOGICAL_AND_EXPRESSION, 0), inclusive_or_expression(std::move(inclusive_or_expression)) {}
  LogicalAndExpressionAST(nt<LogicalAndExpressionAST> logical_and_exression,
                          nt<InclusiveOrExpressionAST> inclusive_or_expression)
      : AST(AST::Kind::LOGICAL_AND_EXPRESSION, 1),
        logical_and_exression(std::move(logical_and_exression)),
        inclusive_or_expression(std::move(inclusive_or_expression)) {}
 private:
  nt<InclusiveOrExpressionAST> inclusive_or_expression;
  nt<LogicalAndExpressionAST> logical_and_exression;
};
class ExpressionAST : public AST {
 public:
  ExpressionAST(nt<AssignmentExpressionAST> assignment_expression)
      : AST(AST::Kind::EXPRESSION, 0),
        assignment_expression(std::move(assignment_expression)) {}
  ExpressionAST(nt<ExpressionAST> expression, nt<AssignmentExpressionAST> assignment_expression)
      : AST(AST::Kind::EXPRESSION, 1),
        expression(std::move(expression)),
        assignment_expression(std::move(assignment_expression)) {}
 private:
  nt<AssignmentExpressionAST> assignment_expression;
  nt<ExpressionAST> expression;
};
class LogicalOrExpressionAST : public AST {
 public:
  LogicalOrExpressionAST(nt<LogicalAndExpressionAST> logical_and_expression)
      : AST(AST::Kind::LOGICAL_OR_EXPRESSION, 0), logical_and_expression(std::move(logical_and_expression)) {}
  LogicalOrExpressionAST(nt<LogicalOrExpressionAST> logical_or_expression,
                         nt<LogicalAndExpressionAST> logical_and_expression)
      : AST(AST::Kind::LOGICAL_OR_EXPRESSION, 1),
        logical_or_expression(std::move(logical_or_expression)),
        logical_and_expression(std::move(logical_and_expression)) {}
 private:
  nt<LogicalAndExpressionAST> logical_and_expression;
  nt<LogicalOrExpressionAST> logical_or_expression;
};
class ConditionalExpressionAST : public AST {
 public:
  ConditionalExpressionAST(nt<LogicalOrExpressionAST> logical_or_expression)
      : AST(AST::Kind::CONDITIONAL_EXPRESSION, 0),
        logical_or_expression(std::move(logical_or_expression)) {}
  ConditionalExpressionAST(nt<LogicalOrExpressionAST> logical_or_expression,
                           nt<ExpressionAST> expression,
                           nt<ConditionalExpressionAST> conditional_expression)
      : AST(AST::Kind::CONDITIONAL_EXPRESSION, 1),
        logical_or_expression(std::move(logical_or_expression)),
        expression(std::move(expression)),
        conditional_expression(std::move(
            conditional_expression)) {}
 private:
  nt<LogicalOrExpressionAST> logical_or_expression;
  nt<ExpressionAST> expression;
  nt<ConditionalExpressionAST> conditional_expression;
};
class ParameterTypeListAST : public AST {
 public:
  ParameterTypeListAST(nt<ParameterListAST> parameter_list, bool hasMultiple)
      : AST(AST::Kind::PARAMETER_TYPE_LIST, hasMultiple ? 1 : 0), parameter_list(std::move(parameter_list)) {}
 private:
  nt<ParameterListAST> parameter_list;
};
class DirectDeclaratorAST : public AST {
 public:
  DirectDeclaratorAST(nt<IdentifierAST> identifier)
      : AST(AST::Kind::DIRECT_DECLARATOR, 0),
        identifier(std::move(identifier)),
        declarator(nullptr),
        direct_declarator(nullptr),
        constant_expressions(),
        parameter_type_list(nullptr),
        identifiers() {}
  DirectDeclaratorAST(nt<DeclaratorAST> declarator)
      : AST(AST::Kind::DIRECT_DECLARATOR, 1),
        identifier(nullptr),
        declarator(std::move(declarator)),
        direct_declarator(nullptr),
        constant_expressions(),
        parameter_type_list(nullptr),
        identifiers() {}
  DirectDeclaratorAST(nt<DirectDeclaratorAST> direct_declarator, nts<ConstantExpressionAST> constant_expressions)
      : AST(AST::Kind::DIRECT_DECLARATOR, 2),
        identifier(nullptr),
        declarator(nullptr),
        direct_declarator(std::move(direct_declarator)),
        constant_expressions(std::move(constant_expressions)),
        parameter_type_list(nullptr),
        identifiers() {}
  DirectDeclaratorAST(nt<DirectDeclaratorAST> direct_declarator, nt<ParameterTypeListAST> parameter_type_list)
      : AST(AST::Kind::DIRECT_DECLARATOR, 3),
        identifier(nullptr),
        declarator(nullptr),
        direct_declarator(std::move(direct_declarator)),
        constant_expressions(),
        parameter_type_list(std::move(parameter_type_list)),
        identifiers() {}
  DirectDeclaratorAST(nt<DirectDeclaratorAST> direct_declarator, nts<IdentifierAST> identifiers)
      : AST(AST::Kind::DIRECT_DECLARATOR, 4),
        identifier(nullptr),
        declarator(nullptr),
        direct_declarator(std::move(direct_declarator)),
        constant_expressions(),
        parameter_type_list(nullptr),
        identifiers(std::move(identifiers)) {}
 private:
  nt<IdentifierAST> identifier;
  nt<DeclaratorAST> declarator;
  nt<DirectDeclaratorAST> direct_declarator;
  nts<ConstantExpressionAST> constant_expressions;
  nt<ParameterTypeListAST> parameter_type_list;
  nts<IdentifierAST> identifiers;
};
class PointerAST : public AST {
 public:
  PointerAST(nts<TypeQualifierAST> type_qualifiers, nt<PointerAST> pointer)
      : AST(AST::Kind::POINTER), type_qualifiers(std::move(type_qualifiers)), pointer(std::move(pointer)) {}
 private:
  nts<TypeQualifierAST> type_qualifiers;
  nt<PointerAST> pointer;
};
class ConstantExpressionAST : public AST {
 public:
  ConstantExpressionAST(nt<ConditionalExpressionAST> conditional_expression)
      : AST(AST::Kind::CONSTANT_EXPRESSION), conditional_expression(std::move(conditional_expression)) {}
 private:
  nt<ConditionalExpressionAST> conditional_expression;
};
class StructDeclaratorAST : public AST {
 public:
  StructDeclaratorAST(nt<DeclaratorAST> declarator)
      : AST(AST::Kind::STRUCT_DECLARATOR, 0),
        declarator(std::move(declarator)),
        constant_expression(nullptr) {}
  StructDeclaratorAST(nt<DeclaratorAST> declarator, nt<ConstantExpressionAST> constant_expression)
      : AST(AST::Kind::STRUCT_DECLARATOR, 1),
        declarator(std::move(declarator)),
        constant_expression(nullptr) {}
  StructDeclaratorAST(nt<ConstantExpressionAST> constant_expression)
      : AST(AST::Kind::STRUCT_DECLARATOR, 2),
        declarator(nullptr),
        constant_expression(std::move(constant_expression)) {}
 private:
  nt<DeclaratorAST> declarator;
  nt<ConstantExpressionAST> constant_expression;
};
class StructDeclaratorListAST : public AST {
 public:
  StructDeclaratorListAST(nt<StructDeclaratorAST> struct_declarator)
      : AST(AST::Kind::STRUCT_DECLARATOR_LIST, 0),
        decl_tor(std::move(struct_declarator)),
        decl_tor_list(nullptr) {}
  StructDeclaratorListAST(nt<StructDeclaratorListAST> struct_declarator_list, nt<StructDeclaratorAST> struct_declarator)
      : AST(AST::Kind::STRUCT_DECLARATOR_LIST, 1),
        decl_tor_list(std::move(struct_declarator_list)),
        decl_tor(std::move(struct_declarator)) {}
 private:
  nt<StructDeclaratorAST> decl_tor;
  nt<StructDeclaratorListAST> decl_tor_list;
};
class StructDeclarationAST : public AST {
 public:
  StructDeclarationAST(nts<SpecifierQualifierAST> specifier_qualifier,
                       nt<StructDeclaratorListAST> struct_declarator_list)
      : AST(AST::Kind::STRUCT_DECLARATION),
        spec_qual(std::move(specifier_qualifier)),
        decl_tor_list(std::move(struct_declarator_list)) {}
 private:
  nts<SpecifierQualifierAST> spec_qual;
  nt<StructDeclaratorListAST> decl_tor_list;
};
/// enum
class EnumSpecifierAST : public AST {
 public:
  EnumSpecifierAST(nt<IdentifierAST> identifier, nt<EnumeratorListAST> enumeratorList)
      : AST(AST::Kind::ENUM_SPECIFIER), id(std::move(identifier)), enum_list(std::move(enumeratorList)) {}
  EnumSpecifierAST(nt<EnumeratorListAST> enumeratorList)
      : AST(AST::Kind::ENUM_SPECIFIER), id(nullptr), enum_list(std::move(enumeratorList)) {}
  EnumSpecifierAST(nt<IdentifierAST> identifier)
      : AST(AST::Kind::ENUM_SPECIFIER), id(std::move(identifier)), enum_list() {}
 private:
  nt<IdentifierAST> id;
  nt<EnumeratorListAST> enum_list;
};
class StructOrUnionSpecifierAST : public AST {
 public:
  StructOrUnionSpecifierAST(nt<StructOrUnionAST> structOrUnion, nt<IdentifierAST> identifier,
                            nts<StructDeclarationAST> struct_declarations)
      : AST(AST::Kind::STRUCT_OR_UNION_SPECIFIER, 0),
        struct_or_union(std::move(structOrUnion)),
        id(std::move(identifier)),
        struct_decls(std::move(struct_declarations)) {}
  StructOrUnionSpecifierAST(nt<StructOrUnionAST> structOrUnion, nts<StructDeclarationAST> struct_declarations)
      : AST(AST::Kind::STRUCT_OR_UNION_SPECIFIER, 1), struct_or_union(std::move(structOrUnion)),
        id(nullptr),
        struct_decls(std::move(struct_declarations)) {}
  StructOrUnionSpecifierAST(nt<StructOrUnionAST> structOrUnion, nt<IdentifierAST> identifier)
      : AST(AST::Kind::STRUCT_OR_UNION_SPECIFIER, 2), struct_or_union(std::move(structOrUnion)),
        id(std::move(identifier)),
        struct_decls() {}
 private:
  nt<StructOrUnionAST> struct_or_union;
  nt<IdentifierAST> id;
  nts<StructDeclarationAST> struct_decls;
};
class TypeSpecifierAST : public AST {
 public:
  TypeSpecifierAST(TokenKind kind) : AST(AST::Kind::TYPE_SPECIFIER), token_kind(kind) {}
  TypeSpecifierAST(nt<StructOrUnionSpecifierAST> specifier)
      : AST(AST::Kind::TYPE_SPECIFIER, 9), spec(std::move(specifier)) {}
  TypeSpecifierAST(nt<EnumeratorListAST> specifier) : AST(AST::Kind::TYPE_SPECIFIER, 10), spec(std::move(specifier)) {}
  TypeSpecifierAST(nt<TypedefNameAST> specifier) : AST(AST::Kind::TYPE_SPECIFIER, 11), spec(std::move(specifier)) {}
 private:
  TokenKind token_kind;
  nt<AST> spec;
};
class SpecifierQualifierAST : public AST {
 public:
  SpecifierQualifierAST(nt<TypeSpecifierAST> speciler) : AST(AST::Kind::SPECIFIER_QUALIFIER, 0),
                                                         spec(std::move(speciler)) {}
  SpecifierQualifierAST(nt<TypeQualifierAST> speciler) : AST(AST::Kind::SPECIFIER_QUALIFIER, 1),
                                                         spec(std::move(speciler)) {}
 private:
  nt<AST> spec;
};
class StorageClassSpecifierAST : public AST {
 public:
  StorageClassSpecifierAST(TokenKind kind)
      : AST(AST::Kind::STORAGE_CLASS_SPECIFIER), token_kind(kind) {}
 private:
  TokenKind token_kind;
};
class DeclaratorAST : public AST {
 public:
  DeclaratorAST(nt<PointerAST> pointer, nt<DirectDeclaratorAST> direct_declarator)
      : AST(AST::Kind::DECLARATOR), pointer(std::move(pointer)), dir_decl_tor(std::move(direct_declarator)) {}
 private:
  nt<PointerAST> pointer;
  nt<DirectDeclaratorAST> dir_decl_tor;
};
class DeclarationSpecifierAST : public AST {
 public:
  DeclarationSpecifierAST(nt<StorageClassSpecifierAST> specifier)
      : AST(AST::Kind::DECLARATION_SPECIFIER, 0), specifier(std::move(specifier)) {}
  DeclarationSpecifierAST(nt<TypeSpecifierAST> specifier)
      : AST(AST::Kind::DECLARATION_SPECIFIER, 1), specifier(std::move(specifier)) {}
  DeclarationSpecifierAST(nt<TypeQualifierAST> specifier)
      : AST(AST::Kind::DECLARATION_SPECIFIER, 2), specifier(std::move(specifier)) {}
 private:
  nt<AST> specifier;
};
class DeclarationAST : public AST {
 public:
  DeclarationAST(nts<DeclarationSpecifierAST> declaration_specifiers,
                 nts<InitDeclaratorAST> init_declarators)
      : AST(AST::Kind::DECLARATION),
        decl_specs(std::move(declaration_specifiers)),
        init_dec_tors(std::move(init_declarators)) {}
 private:
  nts<DeclarationSpecifierAST> decl_specs;
  nts<InitDeclaratorAST> init_dec_tors;
};
/// {
class CompoundStatementAST : public AST {
 public:
  CompoundStatementAST(nts<DeclarationAST> declarations, nts<StatementAST> statements)
      : AST(AST::Kind::COMPOUND_STATEMENT), decls(std::move(declarations)), stats(std::move(statements)) {}
 private:
  nts<DeclarationAST> decls;
  nts<StatementAST> stats;

};
class FunctionDefinitionAST : public AST {
 public:
  FunctionDefinitionAST(nts<DeclarationSpecifierAST> declaration_spcifiers,
                        nt<DeclaratorAST> declarator,
                        nts<DeclarationAST> declarations,
                        nt<CompoundStatementAST> compound_statement)
      : AST(AST::Kind::FUNCTION_DEFINITION), decl_specs(std::move(declaration_spcifiers)),
        decl_tor(std::move(declarator)), decls(std::move(declarations)), stat(std::move(compound_statement)) {}
 private:
  nts<DeclarationSpecifierAST> decl_specs;
  nt<DeclaratorAST> decl_tor;
  nts<DeclarationAST> decls;
  nt<CompoundStatementAST> stat;
};
class ExternalDeclarationAST : public AST {
 public:
  explicit ExternalDeclarationAST(nt<FunctionDefinitionAST> def)
      : AST(AST::Kind::EXTERNAL_DECLARATION, 0), def_or_decl(std::move(def)) {}
  explicit ExternalDeclarationAST(nt<DeclarationAST> decl)
      : AST(AST::Kind::EXTERNAL_DECLARATION, 1), def_or_decl(std::move(decl)) {}
 private:
  nt<AST> def_or_decl;
};
class TranslationUnitAST : public AST {
 public:
  TranslationUnitAST(nts<ExternalDeclarationAST> external_declarations)
      : AST(AST::Kind::TRANSLATION_UNIT), decls(std::move(external_declarations)) {}
 private:
  nts<ExternalDeclarationAST> decls;
};
} //namespace mycc
#endif //MYCCPILER_AST_H
