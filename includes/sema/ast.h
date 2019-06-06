/** this Abstract Sytex Tree data struction follow the Backus-Naur Form
 *  https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm
 */
#ifndef MYCCPILER_AST_H
#define MYCCPILER_AST_H
#include <vector>
#include <memory>
#include <sema/operator.h>
#include <tokens/token.h>
class SymbolTable;
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
  void printIndent(int indent);
  virtual ~AST() = default;
 private:
  const Kind kind;
  //the id of which production
  const int productionId;
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
//    std::cout << static_cast<int>(type) << std::endl;
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
class StringAST : public AST {
 public:
  StringAST(std::string value);
};
class IdentifierAST : public AST {
 public:
  IdentifierAST(const Token& token);
  const Token& token;
  void print(int indent) override;
};
//class StructOrUnionAST : public AST {
// public:
//  StructOrUnionAST(StructOrUnion struct_or_union);
//  const StructOrUnion struct_or_union;
//};
class TypedefNameAST : public AST {
 public:
  TypedefNameAST(nt<IdentifierAST> identifier);
  const nt<IdentifierAST> id;
  void print(int indent) override;
};
class TypeQualifierAST : public AST {
 public:
  TypeQualifierAST(Terminal<TypeQuailifier> op);
  const Terminal<TypeQuailifier> op;
  void print(int indent) override;
};
//class UnaryOperatorAST : public AST {
// public:
//  UnaryOperatorAST(UnaryOp op);
//  const UnaryOp op;
//};
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
};
class EnumeratorListAST : public AST {
 public:
  EnumeratorListAST(nts<EnumeratorAST> enumerator);
  const nts<EnumeratorAST> enumerator;
  void print(int indent) override;
};
class ParameterDeclarationAST : public AST {
 public:
  ParameterDeclarationAST(nt<DeclarationSpecifiersAST> declaration_specifiers, nt<DeclaratorAST> declarator);
  const nt<DeclarationSpecifiersAST> declaration_specifiers;
  const nt<DeclaratorAST> declarator;
  void print(int indent) override;
};
class ParameterListAST : public AST {
 public:
  ParameterListAST(nts<ParameterDeclarationAST> parameter_declaration, SymbolTable &table);
  const nts<ParameterDeclarationAST> parameter_declaration;
  SymbolTable& table;
  void print(int indent) override;
};
class EnumerationConstantAST : public AST {
 public:
  EnumerationConstantAST(nt<IdentifierAST> id);
  const nt<IdentifierAST> id;
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
//class ConstantAST : public AST {
// public:
//  ConstantAST(nt<IntegerConstantAST>);
//  ConstantAST(nt<CharacterConstantAST>);
//  ConstantAST(nt<FloatingConstantAST>);
//  ConstantAST(nt<EnumerationConstantAST>);
//};
class AssignmentExpressionAST : public AST {
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
class PrimaryExpressionAST : public AST {
 public:
  PrimaryExpressionAST(nt<IdentifierAST>);
  PrimaryExpressionAST(nt<IntegerConstantAST>);
  PrimaryExpressionAST(nt<FloatingConstantAST>);
  PrimaryExpressionAST(nt<CharacterConstantAST>);
  PrimaryExpressionAST(nt<StringAST>);
  PrimaryExpressionAST(nt<ExpressionAST>);
  const nt<AST> ast;
  void print(int indent) override;
};
class PostfixExpressionAST : public AST {
 public:
  PostfixExpressionAST(nt<PrimaryExpressionAST> primary, std::vector<std::pair<int, nt<AST>>> terms);
  const nt<PrimaryExpressionAST> primary;
  const std::vector<std::pair<int, nt<AST>>> terms;
  void print(int indent) override;
};
class TypeNameAST : public AST {
 public:
  TypeNameAST(nts<SpecifierQualifierAST> specifier, nt<DeclaratorAST> declarator);
  const nts<SpecifierQualifierAST> specifier;
  const nt<DeclaratorAST> declarator;
  void print(int indent) override;
};
class UnaryExpressionAST : public AST {
 public:
  enum class PrefixType : int {
    PLUSPLUS = 1,
    SUBSUB = 2,
    SIZE_OF = 4,
  };
  UnaryExpressionAST(nt<PostfixExpressionAST> postfix_expression);
  UnaryExpressionAST(nt<UnaryExpressionAST> unary_expression, PrefixType type);
  UnaryExpressionAST(Terminal<UnaryOp> op, nt<CastExpressionAST> cast_expression);
  UnaryExpressionAST(nt<TypeNameAST> type_name);
  const nt<PostfixExpressionAST> postfix_expression;
  const nt<UnaryExpressionAST> unary_expression;
  const std::unique_ptr<Terminal<UnaryOp>> op;
  const nt<CastExpressionAST> cast_expression;
  const nt<TypeNameAST> type_name;
  void print(int indent) override;
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
class ExpressionAST : public AST {
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
  enum class Term2 {
    CONST_EXPR,
    PARA_LIST,
    ID,
  };
  DirectDeclaratorAST(nt<AST> term1, std::vector<std::pair<Term2, nt<AST>>> term2s);
  const nt<AST> term1;
  const std::vector<std::pair<Term2, nt<AST>>> term2s;
  void print(int indent) override;
};
class PointerAST : public AST {
 public:
  PointerAST(nts<TypeQualifierAST> type_qualifiers, nt<PointerAST> pointer);
  const nts<TypeQualifierAST> type_qualifiers;
  const nt<PointerAST> pointer;
  void print(int indent) override;
};
class ConstantExpressionAST : public AST {
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
};
class StructDeclaratorListAST : public AST {
 public:
  StructDeclaratorListAST(nts<StructDeclaratorAST> struct_declarators);
  const nts<StructDeclaratorAST> struct_declarators;
  void print(int indent) override;
};
class StructDeclarationAST : public AST {
 public:
  StructDeclarationAST(nts<SpecifierQualifierAST> specifier_qualifier,
                       nt<StructDeclaratorListAST> struct_declarator_list);
  const nts<SpecifierQualifierAST> specifier_qualifier;
  const nt<StructDeclaratorListAST> struct_declarator_list;
  void print(int indent) override;
};
class EnumSpecifierAST : public AST {
 public:
  EnumSpecifierAST(nt<IdentifierAST> identifier, nt<EnumeratorListAST> enumeratorList);
  EnumSpecifierAST(nt<EnumeratorListAST> enumeratorList);
  EnumSpecifierAST(nt<IdentifierAST> identifier);
  const nt<IdentifierAST> id;
  const nt<EnumeratorListAST> enum_list;
  void print(int indent) override;
};
class StructOrUnionSpecifierAST : public AST {
 public:
  StructOrUnionSpecifierAST(StructOrUnion type, nt<IdentifierAST> id, nts<StructDeclarationAST> declarations);
  const StructOrUnion type;
  const nt<IdentifierAST> id;
  const nts<StructDeclarationAST> declarations;
  void print(int indent) override;
};
class ProtoTypeSpecifierAST : public AST, Terminal<ProtoTypeSpecifier> {
 public:
  ProtoTypeSpecifierAST(Terminal<ProtoTypeSpecifier> specifier);
  const Terminal<ProtoTypeSpecifier> specifier;
  void print(int indent) override;
};
class TypeSpecifierAST : public AST {
 public:
  TypeSpecifierAST(Terminal<ProtoTypeSpecifier> specifier);
  TypeSpecifierAST(nt<StructOrUnionSpecifierAST> specifier);
  TypeSpecifierAST(nt<EnumSpecifierAST> specifier);
  TypeSpecifierAST(nt<TypedefNameAST> specifier);
  const nt<AST> specifier;
  void print(int indent) override;
};
class SpecifierQualifierAST : public AST {
 public:
  SpecifierQualifierAST(nt<TypeSpecifierAST> speciler);
  SpecifierQualifierAST(nt<TypeQualifierAST> speciler);
  const nt<AST> speciler;
  void print(int indent) override;
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
};
class DeclarationSpecifiersAST : public AST {
 public:
  DeclarationSpecifiersAST(ts<StorageSpecifier> storage_specifiers,
                           nts<TypeSpecifierAST> type_specifiers,
                           nts<TypeQualifierAST> type_qualifiers);
  const ts<StorageSpecifier> storage_specifiers;
  const nts<TypeSpecifierAST> type_specifiers;
  const nts<TypeQualifierAST> type_qualifiers;
  void print(int indent) override;
};
class DeclarationAST : public AST {
 public:
  DeclarationAST(nt<DeclarationSpecifiersAST> declaration_specifiers,
                 InitDeclarators init_declarators);
  const nt<DeclarationSpecifiersAST> declaration_specifiers;
  const InitDeclarators init_declarators;
  void print(int indent) override;
};
class CompoundStatementAST : public AST {
 public:
  CompoundStatementAST(nts<DeclarationAST> declarations,
                         nts<StatementAST> statements,
                         SymbolTable &table);
  const nts<DeclarationAST> declarations;
  const nts<StatementAST> statements;
  SymbolTable& table;
  void print(int indent) override;
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
  void print(int indent) override;

};
class ExternalDeclarationAST : public AST {
 public:
  explicit ExternalDeclarationAST(nt<AST> def);
  const nt<AST> def;
  void print(int indent) override;
};
class TranslationUnitAST : public AST {
 public:
  TranslationUnitAST(nts<ExternalDeclarationAST> external_declarations);
  const nts<ExternalDeclarationAST> external_declarations;
  void print(int indent) override;
};
#endif //MYCCPILER_AST_H
