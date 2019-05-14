/** this Abstract Sytex Tree data struction follow the Backus-Naur Form
 *  https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm
 */
#ifndef MYCCPILER_AST_H
#define MYCCPILER_AST_H
#include <vector>
#include <memory>
#include <sema/operator.h>
#include <sema/SymbolTable.h>
#include <tokens/token.h>
class SemaException : public std::exception {
 public:
  SemaException(std::string error, const Token &token);
  const char *what() const noexcept override;
 private:
  const Token &token;
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
  };
  AST(Kind kind, int id = 0);
  const Kind getKind() const {
    return kind;
  }
  const int getPro_id() const {
    return pro_id;
  }
  virtual const char *toString();
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
class DeclarationSpecifiersAST;
class ParameterTypeListAST;
class AssignmentExpressionAST;
class InitializerAST;
class CompoundStatementAST;
class StatementAST;
class StringAST : public AST {
 public:
  StringAST(std::string value);
};
class IdentifierAST : public AST {
 public:
  IdentifierAST(Token token);
 private:
  Token token;
};
class StructOrUnionAST : public AST {
 public:
  StructOrUnionAST(StructOrUnion struct_or_union);

};
class TypedefNameAST : public AST {
 public:
  TypedefNameAST(nt<IdentifierAST> identifier);
 private:
  nt<IdentifierAST> id;
};
class TypeQualifierAST : public AST {
 public:
  // true for const, false for volatile
  TypeQualifierAST(Operator<TypeQuailifier> op);
};
class UnaryOperatorAST : public AST { public:UnaryOperatorAST(UnaryOp op) : AST(AST::Kind::UNARY_OPERATOR) {}};
class JumpStatementAST : public AST {
 public:
  JumpStatementAST(nt<IdentifierAST>);
  JumpStatementAST(bool is_continue);
  JumpStatementAST(nt<ExpressionAST>);
};
class IterationStatementAST : public AST {
 public:
  IterationStatementAST(nt<ExpressionAST>, nt<StatementAST>);
  IterationStatementAST(nt<StatementAST>, nt<ExpressionAST>);
  IterationStatementAST(nt<ExpressionAST>, nt<ExpressionAST>, nt<ExpressionAST>, nt<StatementAST>);
};
class SelectionStatementAST : public AST {
 public:
  SelectionStatementAST(nt<ExpressionAST>, nt<StatementAST>, bool is_if);
  SelectionStatementAST(nt<ExpressionAST>, nt<StatementAST>, nt<StatementAST>);
};
class ExpressionStatementAST : public AST {
 public:
  ExpressionStatementAST(nt<ExpressionAST>);
};
class LabeledStatementAST : public AST {
 public:
  LabeledStatementAST(nt<IdentifierAST>, nt<StatementAST>);
  LabeledStatementAST(nt<ConstantExpressionAST>, nt<StatementAST>);
  LabeledStatementAST(nt<StatementAST>);
};
class StatementAST : public AST {
 public:
  StatementAST(nt<LabeledStatementAST>);
  StatementAST(nt<ExpressionStatementAST>);
  StatementAST(nt<CompoundStatementAST>);
  StatementAST(nt<SelectionStatementAST>);
  StatementAST(nt<IterationStatementAST>);
  StatementAST(nt<JumpStatementAST>);
};
class InitializerListAST : public AST {
 public:
  InitializerListAST(nts<InitializerAST>);
};
class InitializerAST : public AST {
 public:
  InitializerAST(nt<AssignmentExpressionAST>);
  InitializerAST(nt<InitializerListAST>);
};

typedef std::vector<std::pair<nt<DeclaratorAST>, nt<InitializerAST>>> InitDeclarators;
class EnumeratorAST : public AST {
 public:
  EnumeratorAST(nt<IdentifierAST>);
  EnumeratorAST(nt<IdentifierAST>, nt<ConstantExpressionAST>);
};
class EnumeratorListAST : public AST {
 public:
  EnumeratorListAST(nts<EnumeratorAST> &&);
};
class ParameterDeclarationAST : public AST {
 public:
  ParameterDeclarationAST(nt<DeclarationSpecifiersAST>, nt<DeclaratorAST>);
};
class ParameterListAST : public AST {
 public:
  ParameterListAST(nts<ParameterDeclarationAST>);
};
class EnumerationConstantAST : public AST {
 public:
  EnumerationConstantAST(nt<IdentifierAST>);
};
class FloatingConstantAST : public AST {
 public:
  FloatingConstantAST(std::string);
};
class CharacterConstantAST : public AST {
 public:
  CharacterConstantAST(std::string);
};
class IntegerConstantAST : public AST { public:IntegerConstantAST(std::string) : AST(AST::Kind::INTEGER_CONSTANT) {}};
class ConstantAST : public AST {
 public:
  ConstantAST(nt<IntegerConstantAST>);
  ConstantAST(nt<CharacterConstantAST>);
  ConstantAST(nt<FloatingConstantAST>);
  ConstantAST(nt<EnumerationConstantAST>);
};
class AssignmentExpressionAST : public AST {
 public:
  AssignmentExpressionAST(nt<ConditionalExpressionAST>);
  AssignmentExpressionAST(nt<ConditionalExpressionAST>, Operator<AssignmentOp>, nt<AssignmentExpressionAST>);
  //TODO check is LHS a lvalue
};
class PrimaryExpressionAST : public AST {
 public:
  PrimaryExpressionAST(nt<IdentifierAST>);
  PrimaryExpressionAST(nt<IntegerConstantAST>);
  PrimaryExpressionAST(nt<FloatingConstantAST>);
  PrimaryExpressionAST(nt<CharacterConstantAST>);
  PrimaryExpressionAST(nt<StringAST>);
  PrimaryExpressionAST(nt<ExpressionAST>);
};
class PostfixExpressionAST : public AST {
 public:
  PostfixExpressionAST(nt<PrimaryExpressionAST> primary, std::vector<std::pair<int, nt<AST>>> terms);
};
class TypeNameAST : public AST {
 public:
  TypeNameAST(nts<SpecifierQualifierAST>, nt<DeclaratorAST>);
};
class UnaryExpressionAST : public AST {
 public:
  enum class PrefixType : int {
    PLUSPLUS = 1,
    SUBSUB = 2,
    SIZE_OF = 4,
  };
  UnaryExpressionAST(nt<PostfixExpressionAST>);
  UnaryExpressionAST(nt<UnaryExpressionAST>, PrefixType type);
  UnaryExpressionAST(Operator<UnaryOp> op, nt<CastExpressionAST>);
  UnaryExpressionAST(nt<TypeNameAST>);
};
class CastExpressionAST : public AST {
 public:
  CastExpressionAST(nt<UnaryExpressionAST>);
  CastExpressionAST(nt<TypeNameAST>, nt<CastExpressionAST>);
};
class ExpressionAST : public AST {
 public:
  ExpressionAST(nts<AssignmentExpressionAST>);
};
class LogicalOrExpressionAST : public AST {
 public:
  LogicalOrExpressionAST(nt<LogicalOrExpressionAST> left, Operator<InfixOp> op, nt<LogicalOrExpressionAST> right);;
  LogicalOrExpressionAST(nt<CastExpressionAST> leaf);;
};
class ConditionalExpressionAST : public AST {
 public:
  ConditionalExpressionAST(nt<LogicalOrExpressionAST> logical_or_expression);
  ConditionalExpressionAST(nt<LogicalOrExpressionAST> logical_or_expression,
                           nt<ExpressionAST> expression,
                           nt<ConditionalExpressionAST> conditional_expression);
 private:
  nt<LogicalOrExpressionAST> logical_or_expression;
  nt<ExpressionAST> expression;
  nt<ConditionalExpressionAST> conditional_expression;
};
class ParameterTypeListAST : public AST {
 public:
  ParameterTypeListAST(nt<ParameterListAST> parameter_list, bool hasMultiple);
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
  DirectDeclaratorAST(nt<AST> term1, std::vector<std::pair<Term2, nt<AST>>> term2s);
};
class PointerAST : public AST {
 public:
  PointerAST(nts<TypeQualifierAST> type_qualifiers, nt<PointerAST> pointer);
 private:
  nts<TypeQualifierAST> type_qualifiers;
  nt<PointerAST> pointer;
};
class ConstantExpressionAST : public AST {
 public:
  ConstantExpressionAST(nt<ConditionalExpressionAST> conditional_expression);
 private:
  nt<ConditionalExpressionAST> conditional_expression;
};
class StructDeclaratorAST : public AST {
 public:
  StructDeclaratorAST(nt<DeclaratorAST> declarator);
  StructDeclaratorAST(nt<DeclaratorAST> declarator, nt<ConstantExpressionAST> constant_expression);
  StructDeclaratorAST(nt<ConstantExpressionAST> constant_expression);
 private:
  nt<DeclaratorAST> declarator;
  nt<ConstantExpressionAST> constant_expression;
};
class StructDeclaratorListAST : public AST {
 public:
  StructDeclaratorListAST(nts<StructDeclaratorAST> struct_declarators);
};
class StructDeclarationAST : public AST {
 public:
  StructDeclarationAST(nts<SpecifierQualifierAST> specifier_qualifier,
                       nt<StructDeclaratorListAST> struct_declarator_list);
 private:
  nts<SpecifierQualifierAST> spec_qual;
  nt<StructDeclaratorListAST> decl_tor_list;
};
class EnumSpecifierAST : public AST {
 public:
  EnumSpecifierAST(nt<IdentifierAST> identifier, nt<EnumeratorListAST> enumeratorList);
  EnumSpecifierAST(nt<EnumeratorListAST> enumeratorList);
  EnumSpecifierAST(nt<IdentifierAST> identifier);
 private:
  nt<IdentifierAST> id;
  nt<EnumeratorListAST> enum_list;
};
class StructOrUnionSpecifierAST : public AST {
 public:
  StructOrUnionSpecifierAST(StructOrUnion type, nt<IdentifierAST> id, nts<StructDeclarationAST> declarations);;
};
class TypeSpecifierAST : public AST {
 public:
  TypeSpecifierAST(Operator<ProtoTypeSpecifier> type_specifier);
  TypeSpecifierAST(nt<StructOrUnionSpecifierAST> specifier);
  TypeSpecifierAST(nt<EnumSpecifierAST> specifier);
  TypeSpecifierAST(nt<TypedefNameAST> specifier);
};
class SpecifierQualifierAST : public AST {
 public:
  SpecifierQualifierAST(nt<TypeSpecifierAST> speciler);
  SpecifierQualifierAST(nt<TypeQualifierAST> speciler);
 private:
  nt<AST> spec;
};
class StorageClassSpecifierAST : public AST {
 public:
  StorageClassSpecifierAST(Operator<StorageSpecifier> storage_speicifier);
};
class DeclaratorAST : public AST {
 public:
  DeclaratorAST(nt<PointerAST> pointer, nt<DirectDeclaratorAST> direct_declarator);
};
class DeclarationSpecifiersAST : public AST {
 public:

  DeclarationSpecifiersAST(std::vector<Operator<StorageSpecifier>> storage_specifiers,
                           nts<TypeSpecifierAST> type_specifiers,
                           nts<TypeQualifierAST> type_qualifiers);
  const std::vector<Operator<StorageSpecifier>> storage_specifiers;
  const nts<TypeSpecifierAST> type_specifiers;
  const nts<TypeQualifierAST> type_qualifiers;

};
class DeclarationAST : public AST {
 public:
  DeclarationAST(nt<DeclarationSpecifiersAST> declaration_specifiers,
                 InitDeclarators init_declarators);
  const nt<DeclarationSpecifiersAST> declaration_spcifiers;
  const InitDeclarators init_dec_tors;
};
class CompoundStatementAST : public AST {
 public:
  CompoundStatementAST(nts<DeclarationAST> declarations, nts<StatementAST> statements);
 private:
  nts<DeclarationAST> decls;
  nts<StatementAST> stats;

};
class FunctionDefinitionAST : public AST {
 public:
  FunctionDefinitionAST(nt<DeclarationSpecifiersAST> declaration_spcifiers,
                        nt<DeclaratorAST> declarator,
                        nts<DeclarationAST> declarations,
                        nt<CompoundStatementAST> compound_statement);
  const nt<DeclarationSpecifiersAST> declaration_spcifiers;
  const nt<DeclaratorAST> declarator;
  const nts<DeclarationAST> declarations;
  const nt<CompoundStatementAST> compound_statement;

};
class ExternalDeclarationAST : public AST {
 public:
  explicit ExternalDeclarationAST(nt<AST> def);;
 private:
  nt<AST> def_or_decl;
};
class TranslationUnitAST : public AST {
 public:
  TranslationUnitAST(nts<ExternalDeclarationAST> external_declarations);
};
#endif //MYCCPILER_AST_H
