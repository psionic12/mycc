#ifndef MYCCPILER_AST1_H
#define MYCCPILER_AST1_H

#include <string>
#include <memory>
#include <vector>
#include <tokens/token.h>
enum class NoneTerminalKind {
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

class ITerm {
 public:
  virtual ~ITerm() = 0;
 protected:
  static int indent;
  virtual std::string toString(std::string &&s) = 0;
};

int ITerm::indent = 0;

class INoneTerminal : public ITerm {
 public:
  static std::string toString(NoneTerminalKind kind) {
    switch (kind) {
      case NoneTerminalKind::TRANSLATION_UNIT:return "TRANSLATION_UNIT";
      case NoneTerminalKind::EXTERNAL_DECLARATION:return "EXTERNAL_DECLARATION";
      case NoneTerminalKind::FUNCTION_DEFINITION:return "FUNCTION_DEFINITION";
      case NoneTerminalKind::DECLARATION:return "DECLARATION";
      case NoneTerminalKind::DECLARATION_SPECIFIER:return "DECLARATION_SPECIFIER";
      case NoneTerminalKind::DECLARATOR:return "DECLARATOR";
      case NoneTerminalKind::COMPOUND_STATEMENT:return "COMPOUND_STATEMENT";
      case NoneTerminalKind::STORAGE_CLASS_SPECIFIER:return "STORAGE_CLASS_SPECIFIER";
      case NoneTerminalKind::TYPE_SPECIFIER:return "TYPE_SPECIFIER";
      case NoneTerminalKind::TYPE_QUALIFIER:return "TYPE_QUALIFIER";
      case NoneTerminalKind::STRUCT_OR_UNION_SPECIFIER:return "STRUCT_OR_UNION_SPECIFIER";
      case NoneTerminalKind::ENUM_SPECIFIER:return "ENUM_SPECIFIER";
      case NoneTerminalKind::TYPEDEF_NAME:return "TYPEDEF_NAME";
      case NoneTerminalKind::STRUCT_OR_UNION:return "STRUCT_OR_UNION";
      case NoneTerminalKind::IDENTIFIER:return "IDENTIFIER";
      case NoneTerminalKind::STRUCT_DECLARATION:return "STRUCT_DECLARATION";
      case NoneTerminalKind::SPECIFIER_QUALIFIER:return "SPECIFIER_QUALIFIER";
      case NoneTerminalKind::STRUCT_DECLARATOR_LIST:return "STRUCT_DECLARATOR_LIST";
      case NoneTerminalKind::STRUCT_DECLARATOR:return "STRUCT_DECLARATOR";
      case NoneTerminalKind::CONSTANT_EXPRESSION:return "CONSTANT_EXPRESSION";
      case NoneTerminalKind::POINTER:return "POINTER";
      case NoneTerminalKind::DIRECT_DECLARATOR:return "DIRECT_DECLARATOR";
      case NoneTerminalKind::PARAMETER_TYPE_LIST:return "PARAMETER_TYPE_LIST";
      case NoneTerminalKind::CONDITIONAL_EXPRESSION:return "CONDITIONAL_EXPRESSION";
      case NoneTerminalKind::EXPRESSION:return "EXPRESSION";
      case NoneTerminalKind::CAST_EXPRESSION:return "CAST_EXPRESSION";
      case NoneTerminalKind::UNARY_EXPRESSION:return "UNARY_EXPRESSION";
      case NoneTerminalKind::LOGICAL_OR_EXPRESSION:return "LOGICAL_OR_EXPRESSION";
      case NoneTerminalKind::TYPE_NAME:return "TYPE_NAME";
      case NoneTerminalKind::POSTFIX_EXPRESSION:return "POSTFIX_EXPRESSION";
      case NoneTerminalKind::UNARY_OPERATOR:return "UNARY_OPERATOR";
      case NoneTerminalKind::PRIMARY_EXPRESSION:return "PRIMARY_EXPRESSION";
      case NoneTerminalKind::ASSIGNMENT_EXPRESSION:return "ASSIGNMENT_EXPRESSION";
      case NoneTerminalKind::CONSTANT:return "CONSTANT";
      case NoneTerminalKind::STRING:return "STRING";
      case NoneTerminalKind::INTEGER_CONSTANT:return "INTEGER_CONSTANT";
      case NoneTerminalKind::CHARACTER_CONSTANT:return "CHARACTER_CONSTANT";
      case NoneTerminalKind::FLOATING_CONSTANT:return "FLOATING_CONSTANT";
      case NoneTerminalKind::ENUMERATION_CONSTANT:return "ENUMERATION_CONSTANT";
      case NoneTerminalKind::PARAMETER_LIST:return "PARAMETER_LIST";
      case NoneTerminalKind::PARAMETER_DECLARATION:return "PARAMETER_DECLARATION";
      case NoneTerminalKind::ENUMERATOR_LIST:return "ENUMERATOR_LIST";
      case NoneTerminalKind::ENUMERATOR:return "ENUMERATOR";
      case NoneTerminalKind::INIT_DECLARATOR:return "INIT_DECLARATOR";
      case NoneTerminalKind::INITIALIZER:return "INITIALIZER";
      case NoneTerminalKind::INITIALIZER_LIST:return "INITIALIZER_LIST";
      case NoneTerminalKind::STATEMENT:return "STATEMENT";
      case NoneTerminalKind::LABELED_STATEMENT:return "LABELED_STATEMENT";
      case NoneTerminalKind::EXPRESSION_STATEMENT:return "EXPRESSION_STATEMENT";
      case NoneTerminalKind::SELECTION_STATEMENT:return "SELECTION_STATEMENT";
      case NoneTerminalKind::ITERATION_STATEMENT:return "ITERATION_STATEMENT";
      case NoneTerminalKind::JUMP_STATEMENT:return "JUMP_STATEMENT";
    }
  }
  virtual NoneTerminalKind getKind() = 0;
};

template<typename ...Terms>
class NoneTerminal : public INoneTerminal, private std::tuple<Terms...> {
 public:
  NoneTerminal(NoneTerminalKind kind) : kind(kind) {}
 protected:
  std::string toString(std::string &s) override {
    for (int i = 0; i < indent; ++i) {
      s.append("\t");
    }
    s.append(INoneTerminal::toString(kind));
    ++indent;
    std::apply([s](auto &&... args) { ((s.append(args.toString(s))), ...); }, this);
    --indent;
  }
 private:
  const NoneTerminalKind kind;
};

template<typename T>
class Terminal : public ITerm {
 private:
  T t;
  const Token &token;
};

class NonTerminals : public std::vector<std::unique_ptr<INoneTerminal>>, public ITerm {
};

#endif //MYCCPILER_AST1_H
