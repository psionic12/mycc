/** this Abstract Sytex Tree data struction follow the Backus-Naur Form
 *  https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm
 */
#ifndef MYCCPILER_AST_H
#define MYCCPILER_AST_H
#include <vector>
#include <memory>
#include <sema/operator.h>
#include <sema/SymbolTable.h>

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
    EXPRESSION,
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
class ParameterTypeListAST;
class AssignmentExpressionAST;
class InitializerAST;
class CompoundStatementAST;
class StatementAST;
class StringAST : public AST { public:StringAST(std::string value) : AST(AST::Kind::STRING) {}};
class IdentifierAST : public AST {
 public:
  IdentifierAST(std::string value)
      : AST(AST::Kind::IDENTIFIER), value(std::move(value)) {}
 private:
  std::string value;
};
class StructOrUnionAST : public AST {
 public:
  StructOrUnionAST(bool is_struct) : AST(AST::Kind::STRUCT_OR_UNION), is_struct(is_struct) {}
 private:
  bool is_struct;
};
class TypedefNameAST : public AST {
 public:
  TypedefNameAST(nt<IdentifierAST> identifier) : AST(AST::Kind::TYPEDEF_NAME), id(std::move(identifier)) {}
 private:
  nt<IdentifierAST> id;
};
class TypeQualifierAST : public AST {
 public:
  // true for const, false for volatile
  TypeQualifierAST(bool is_const) : AST(AST::Kind::TYPE_QUALIFIER) {}
};
class UnaryOperatorAST : public AST { public:UnaryOperatorAST(UnaryOp op) : AST(AST::Kind::UNARY_OPERATOR) {}};
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
  InitializerListAST(nts<InitializerAST>) : AST(AST::Kind::INITIALIZER_LIST) {}
};
class InitializerAST : public AST {
 public:
  InitializerAST(nt<AssignmentExpressionAST>) : AST(AST::Kind::INITIALIZER, 0) {}
  InitializerAST(nt<InitializerListAST>) : AST(AST::Kind::INITIALIZER, 1) {}
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
  EnumeratorListAST(nts<EnumeratorAST> &&) : AST(AST::Kind::ENUMERATOR_LIST) {}
};
class ParameterDeclarationAST : public AST {
 public:
  ParameterDeclarationAST(nts<DeclarationSpecifierAST>, nt<DeclaratorAST>) : AST(AST::Kind::PARAMETER_DECLARATION) {}
};
class ParameterListAST : public AST {
 public:
  ParameterListAST(nts<ParameterDeclarationAST>) : AST(AST::Kind::PARAMETER_LIST) {}
};
class EnumerationConstantAST : public AST {
 public:
  EnumerationConstantAST(nt<IdentifierAST>)
      : AST(AST::Kind::ENUMERATION_CONSTANT) {}
};
class FloatingConstantAST : public AST {
 public:
  FloatingConstantAST(std::string)
      : AST(AST::Kind::FLOATING_CONSTANT) {}
};
class CharacterConstantAST : public AST {
 public:
  CharacterConstantAST(std::string)
      : AST(AST::Kind::CHARACTER_CONSTANT) {}
};
class IntegerConstantAST : public AST { public:IntegerConstantAST(std::string) : AST(AST::Kind::INTEGER_CONSTANT) {}};
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
  AssignmentExpressionAST(nt<ConditionalExpressionAST>, AssignmentOp, nt<AssignmentExpressionAST>)
      : AST(AST::Kind::ASSIGNMENT_EXPRESSION, 1) {}
  //TODO check is LHS a lvalue
};
class PrimaryExpressionAST : public AST {
 public:
  PrimaryExpressionAST(nt<IdentifierAST>) : AST(AST::Kind::PRIMARY_EXPRESSION, 0) {}
  PrimaryExpressionAST(nt<IntegerConstantAST>) : AST(AST::Kind::PRIMARY_EXPRESSION, 1) {}
  PrimaryExpressionAST(nt<FloatingConstantAST>) : AST(AST::Kind::PRIMARY_EXPRESSION, 1) {}
  PrimaryExpressionAST(nt<CharacterConstantAST>) : AST(AST::Kind::PRIMARY_EXPRESSION, 1) {}
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
  TypeNameAST(nts<SpecifierQualifierAST>, nts<DeclaratorAST>)
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
class ExpressionAST : public AST {
 public:
  ExpressionAST(nts<AssignmentExpressionAST>) : AST(AST::Kind::EXPRESSION, 0) {}
};
class LogicalOrExpressionAST : public AST {
 public:
  LogicalOrExpressionAST(nt<LogicalOrExpressionAST> left, InfixOp op, nt<LogicalOrExpressionAST> right);
  LogicalOrExpressionAST(nt<CastExpressionAST> leaf);
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
  enum class Term2 {
    CONST_EXPR,
    PARA_LIST,
    ID,
  };
  DirectDeclaratorAST(nt<AST> term1, std::vector<std::pair<Term2, nt<AST>>> term2s)
      : AST(Kind::DIRECT_DECLARATOR) {}
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
  StructDeclaratorListAST(nts<StructDeclaratorAST> struct_declarators) : AST(AST::Kind::STRUCT_DECLARATOR_LIST) {}
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
  StructOrUnionSpecifierAST(StructOrUnion type, nt<IdentifierAST> id, nts<StructDeclarationAST> declarations);
};
class TypeSpecifierAST : public AST {
 public:
  TypeSpecifierAST(ProtoTypeSpecifier type_specifier) : AST(AST::Kind::TYPE_SPECIFIER) {}
  TypeSpecifierAST(nt<StructOrUnionSpecifierAST> specifier)
      : AST(AST::Kind::TYPE_SPECIFIER, 9) {}
  TypeSpecifierAST(nt<EnumSpecifierAST> specifier) : AST(AST::Kind::TYPE_SPECIFIER, 10) {}
  TypeSpecifierAST(nt<TypedefNameAST> specifier) : AST(AST::Kind::TYPE_SPECIFIER, 11) {}
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
  StorageClassSpecifierAST(StorageSpecifier storage_speicifier)
      : AST(AST::Kind::STORAGE_CLASS_SPECIFIER) {}
};
class DeclaratorAST : public AST {
 public:
  DeclaratorAST(nt<PointerAST> pointer, nt<DirectDeclaratorAST> direct_declarator)
      : AST(AST::Kind::DECLARATOR) {}
};
class DeclarationSpecifierAST : public AST {
 public:
  DeclarationSpecifierAST(StorageSpecifier specifier)
      : AST(AST::Kind::DECLARATION_SPECIFIER, 0) {}
  DeclarationSpecifierAST(nt<TypeSpecifierAST> specifier)
      : AST(AST::Kind::DECLARATION_SPECIFIER, 1) {}
  DeclarationSpecifierAST(nt<TypeQualifierAST> specifier)
      : AST(AST::Kind::DECLARATION_SPECIFIER, 2) {}
};
class DeclarationAST : public AST {
 public:
  DeclarationAST(nts<DeclarationSpecifierAST> declaration_specifiers,
                 nts<InitDeclaratorAST> init_declarators)
      : AST(AST::Kind::DECLARATION),
        decl_specs(std::move(declaration_specifiers)),
        init_dec_tors(std::move(init_declarators)) {}
  bool isType() const;
 private:
  nts<DeclarationSpecifierAST> decl_specs;
  nts<InitDeclaratorAST> init_dec_tors;
};
/// {
class CompoundStatementAST : public AST {
 public:
  CompoundStatementAST(nts<DeclarationAST> declarations, nts<StatementAST> statements)
      : AST(AST::Kind::COMPOUND_STATEMENT),
        decls(std::move(declarations)),
        stats(std::move(statements)),
        table(table) {}
 private:
  const SymbolTable &table;
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
  TranslationUnitAST(nts<ExternalDeclarationAST> external_declarations, const SymbolTable &table)
      : AST(AST::Kind::TRANSLATION_UNIT), decls(std::move(external_declarations)),
        table(table) {}
 private:
  const SymbolTable &table;
  nts<ExternalDeclarationAST> decls;
};
} //namespace mycc
#endif //MYCCPILER_AST_H
