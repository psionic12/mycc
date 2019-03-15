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
template<typename T, typename std::enable_if<std::is_base_of<AST, T>::value>::type * = nullptr>
using nt = std::unique_ptr<T>;
// nts is short for None TerminalS
template<typename T>
using nts = std::vector<nt<T>>;
class JumpStatementAST : public AST { public:JumpStatementAST() : AST(AST::Kind::JUMP_STATEMENT) {}};
class IterationStatementAST : public AST { public:IterationStatementAST() : AST(AST::Kind::ITERATION_STATEMENT) {}};
class SelectionStatementAST : public AST { public:SelectionStatementAST() : AST(AST::Kind::SELECTION_STATEMENT) {}};
class ExpressionStatementAST : public AST { public:ExpressionStatementAST() : AST(AST::Kind::EXPRESSION_STATEMENT) {}};
class LabeledStatementAST : public AST { public:LabeledStatementAST() : AST(AST::Kind::LABELED_STATEMENT) {}};
class StatementAST : public AST { public:StatementAST() : AST(AST::Kind::STATEMENT) {}};
class InitializerListAST : public AST { public:InitializerListAST() : AST(AST::Kind::INITIALIZER_LIST) {}};
class InitializerAST : public AST { public:InitializerAST() : AST(AST::Kind::INITIALIZER) {}};
class InitDeclaratorAST : public AST { public:InitDeclaratorAST() : AST(AST::Kind::INIT_DECLARATOR) {}};
class EnumeratorAST : public AST { public:EnumeratorAST() : AST(AST::Kind::ENUMERATOR) {}};
class EnumeratorListAST : public AST { public:EnumeratorListAST() : AST(AST::Kind::ENUMERATOR_LIST) {}};
class DirectAbstractDeclaratorAST : public AST {
 public:
  DirectAbstractDeclaratorAST()
      : AST(AST::Kind::DIRECT_ABSTRACT_DECLARATOR) {}
};
class ParameterDeclarationAST : public AST {
 public:
  ParameterDeclarationAST()
      : AST(AST::Kind::PARAMETER_DECLARATION) {}
};
class ParameterListAST : public AST { public:ParameterListAST() : AST(AST::Kind::PARAMETER_LIST) {}};
class AbstractDeclaratorAST : public AST { public:AbstractDeclaratorAST() : AST(AST::Kind::ABSTRACT_DECLARATOR) {}};
class AssignmentOperatorAST : public AST { public:AssignmentOperatorAST() : AST(AST::Kind::ASSIGNMENT_OPERATOR) {}};
class EnumerationConstantAST : public AST { public:EnumerationConstantAST() : AST(AST::Kind::ENUMERATION_CONSTANT) {}};
class FloatingConstantAST : public AST { public:FloatingConstantAST() : AST(AST::Kind::FLOATING_CONSTANT) {}};
class CharacterConstantAST : public AST { public:CharacterConstantAST() : AST(AST::Kind::CHARACTER_CONSTANT) {}};
class IntegerConstantAST : public AST { public:IntegerConstantAST() : AST(AST::Kind::INTEGER_CONSTANT) {}};
class StringAST : public AST { public:StringAST() : AST(AST::Kind::STRING) {}};
class ConstantAST : public AST { public:ConstantAST() : AST(AST::Kind::CONSTANT) {}};
class AssignmentExpressionAST : public AST {
 public:
  AssignmentExpressionAST()
      : AST(AST::Kind::ASSIGNMENT_EXPRESSION) {}
};
class PrimaryExpressionAST : public AST { public:PrimaryExpressionAST() : AST(AST::Kind::PRIMARY_EXPRESSION) {}};
class UnaryOperatorAST : public AST { public:UnaryOperatorAST() : AST(AST::Kind::UNARY_OPERATOR) {}};
class PostfixExpressionAST : public AST { public:PostfixExpressionAST() : AST(AST::Kind::POSTFIX_EXPRESSION) {}};
class TypeNameAST : public AST { public:TypeNameAST() : AST(AST::Kind::TYPE_NAME) {}};
class UnaryExpressionAST : public AST { public:UnaryExpressionAST() : AST(AST::Kind::UNARY_EXPRESSION) {}};
class CastExpressionAST : public AST { public:CastExpressionAST() : AST(AST::Kind::CAST_EXPRESSION) {}};
class MultiplicativeExpressionAST : public AST {
 public:
  MultiplicativeExpressionAST()
      : AST(AST::Kind::MULTIPLICATIVE_EXPRESSION) {}
};
class AdditiveExpressionAST : public AST { public:AdditiveExpressionAST() : AST(AST::Kind::ADDITIVE_EXPRESSION) {}};
class ShiftExpressionAST : public AST { public:ShiftExpressionAST() : AST(AST::Kind::SHIFT_EXPRESSION) {}};
class RelationalExpressionAST : public AST {
 public:
  RelationalExpressionAST()
      : AST(AST::Kind::RELATIONAL_EXPRESSION) {}
};
class EqualityExpressionAST : public AST { public:EqualityExpressionAST() : AST(AST::Kind::EQUALITY_EXPRESSION) {}};
class AndExpressionAST : public AST { public:AndExpressionAST() : AST(AST::Kind::AND_EXPRESSION) {}};
class ExclusiveOrExpressionAST : public AST {
 public:
  ExclusiveOrExpressionAST()
      : AST(AST::Kind::EXCLUSIVE_OR_EXPRESSION) {}
};
class InclusiveOrExpressionAST : public AST {
 public:
  InclusiveOrExpressionAST()
      : AST(AST::Kind::INCLUSIVE_OR_EXPRESSION) {}
};
class LogicalAndExpressionAST : public AST {
 public:
  LogicalAndExpressionAST()
      : AST(AST::Kind::LOGICAL_AND_EXPRESSION) {}
};
class ExpressionAST : public AST { public:ExpressionAST() : AST(AST::Kind::EXPRESSION) {}};
class LogicalOrExpressionAST : public AST { public:LogicalOrExpressionAST() : AST(AST::Kind::LOGICAL_OR_EXPRESSION) {}};
class ConditionalExpressionAST : public AST {
 public:
  ConditionalExpressionAST()
      : AST(AST::Kind::CONDITIONAL_EXPRESSION) {}
};
class ParameterTypeListAST : public AST { public:ParameterTypeListAST() : AST(AST::Kind::PARAMETER_TYPE_LIST) {}};
class DirectDeclaratorAST : public AST { public:DirectDeclaratorAST() : AST(AST::Kind::DIRECT_DECLARATOR) {}};
class PointerAST : public AST { public:PointerAST() : AST(AST::Kind::POINTER) {}};
class ConstantExpressionAST : public AST { public:ConstantExpressionAST() : AST(AST::Kind::CONSTANT_EXPRESSION) {}};
class StructDeclaratorAST : public AST { public:StructDeclaratorAST() : AST(AST::Kind::STRUCT_DECLARATOR) {}};
class StructDeclaratorListAST : public AST {
 public:
  StructDeclaratorListAST()
      : AST(AST::Kind::STRUCT_DECLARATOR_LIST) {}
};
class StructDeclarationAST : public AST { public:StructDeclarationAST() : AST(AST::Kind::STRUCT_DECLARATION) {}};
class IdentifierAST : public AST { public:IdentifierAST() : AST(AST::Kind::IDENTIFIER) {}};
class StructOrUnionAST : public AST { public:StructOrUnionAST() : AST(AST::Kind::STRUCT_OR_UNION) {}};
class TypedefNameAST : public AST { public:TypedefNameAST() : AST(AST::Kind::TYPEDEF_NAME) {}};
/// enum
class EnumSpecifierAST : public AST {
 public:
  EnumSpecifierAST(nt<IdentifierAST> identifier, nt<EnumeratorListAST> enumeratorList)
  : AST(AST::Kind::ENUM_SPECIFIER), id(std::move(identifier)), enum_list(std::move(enumeratorList)) {}
  EnumSpecifierAST(nt<EnumSpecifierAST> enumeratorList)
      : AST(AST::Kind::ENUM_SPECIFIER), id(nullptr), enum_list(std::move(enumeratorList)) {}
  EnumSpecifierAST(nt<IdentifierAST> identifier)
      : AST(AST::Kind::ENUM_SPECIFIER), id(std::move(identifier)), enum_list()) {}
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
class TypeQualifierAST : public AST {
 public:
  TypeQualifierAST(TokenKind kind) : AST(AST::Kind::TYPE_QUALIFIER), token_kind(kind) {}
 private:
  TokenKind token_kind;
};
class TypeSpecifierAST : public AST {
 public:
  TypeSpecifierAST(TokenKind kind) : AST(AST::Kind::TYPE_SPECIFIER), token_kind(kind) {}
  TypeSpecifierAST(nt<StructOrUnionSpecifierAST> specifier)
      : AST(AST::Kind::TYPE_SPECIFIER, 9), spec(std::move(specifier)) {}
  TypeSpecifierAST(nt<EnumSpecifierAST> specifier) : AST(AST::Kind::TYPE_SPECIFIER, 10), spec(std::move(specifier)) {}
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
