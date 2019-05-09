#include <sema/ast.h>
#include <iostream>
static int indent;
mycc::TranslationUnitAST::TranslationUnitAST(mycc::nts<mycc::ExternalDeclarationAST> external_declarations)
    : AST(AST::Kind::TRANSLATION_UNIT) {}
mycc::ExternalDeclarationAST::ExternalDeclarationAST(mycc::nt<mycc::AST> def) : AST(AST::Kind::EXTERNAL_DECLARATION) {}
mycc::FunctionDefinitionAST::FunctionDefinitionAST(mycc::nt<mycc::DeclarationSpecifiersAST> declaration_spcifiers,
                                                   mycc::nt<mycc::DeclaratorAST> declarator,
                                                   mycc::nts<mycc::DeclarationAST> declarations,
                                                   mycc::nt<mycc::CompoundStatementAST> compound_statement)
    : AST(AST::Kind::FUNCTION_DEFINITION),
      declaration_spcifiers(std::move(declaration_spcifiers)),
      declarator(std::move(declarator)),
      declarations(std::move(declarations)),
      compound_statement(std::move(compound_statement)) {}
mycc::CompoundStatementAST::CompoundStatementAST(mycc::nts<mycc::DeclarationAST> declarations,
                                                 mycc::nts<mycc::StatementAST> statements)
    : AST(AST::Kind::COMPOUND_STATEMENT),
      decls(std::move(declarations)),
      stats(std::move(statements)) {}
mycc::DeclarationAST::DeclarationAST(mycc::nt<mycc::DeclarationSpecifiersAST> declaration_specifiers,
                                     mycc::InitDeclarators init_declarators) : AST(AST::Kind::DECLARATION) {}
mycc::DeclarationSpecifiersAST::DeclarationSpecifiersAST(std::vector<mycc::Operator<mycc::StorageSpecifier>> storage_specifiers,
                                                         mycc::nts<mycc::TypeSpecifierAST> type_specifiers,
                                                         mycc::nts<mycc::TypeQualifierAST> type_qualifiers)
    : AST(AST::Kind::DECLARATION_SPECIFIER),
      storage_specifiers(std::move(storage_specifiers)),
      type_specifiers(std::move(type_specifiers)),
      type_qualifiers(std::move(type_qualifiers)) {}
mycc::DeclaratorAST::DeclaratorAST(mycc::nt<mycc::PointerAST> pointer,
                                   mycc::nt<mycc::DirectDeclaratorAST> direct_declarator)
    : AST(AST::Kind::DECLARATOR) {}
mycc::StorageClassSpecifierAST::StorageClassSpecifierAST(mycc::Operator<mycc::StorageSpecifier> storage_speicifier)
    : AST(AST::Kind::STORAGE_CLASS_SPECIFIER) {}
mycc::SpecifierQualifierAST::SpecifierQualifierAST(mycc::nt<mycc::TypeQualifierAST> speciler)
    : AST(AST::Kind::SPECIFIER_QUALIFIER, 1),
      spec(std::move(speciler)) {}
mycc::SpecifierQualifierAST::SpecifierQualifierAST(mycc::nt<mycc::TypeSpecifierAST> speciler)
    : AST(AST::Kind::SPECIFIER_QUALIFIER, 0),
      spec(std::move(speciler)) {}
mycc::TypeSpecifierAST::TypeSpecifierAST(mycc::Operator<mycc::ProtoTypeSpecifier> type_specifier)
    : AST(AST::Kind::TYPE_SPECIFIER) {}
mycc::TypeSpecifierAST::TypeSpecifierAST(mycc::nt<mycc::StructOrUnionSpecifierAST> specifier)
    : AST(AST::Kind::TYPE_SPECIFIER, 9) {}
mycc::TypeSpecifierAST::TypeSpecifierAST(mycc::nt<mycc::EnumSpecifierAST> specifier) : AST(AST::Kind::TYPE_SPECIFIER,
                                                                                           10) {}
mycc::TypeSpecifierAST::TypeSpecifierAST(mycc::nt<mycc::TypedefNameAST> specifier) : AST(AST::Kind::TYPE_SPECIFIER,
                                                                                         11) {}
mycc::StructOrUnionSpecifierAST::StructOrUnionSpecifierAST(mycc::StructOrUnion type,
                                                           mycc::nt<mycc::IdentifierAST> id,
                                                           mycc::nts<mycc::StructDeclarationAST> declarations)
    : AST(AST::Kind::DECLARATION) {}
mycc::EnumSpecifierAST::EnumSpecifierAST(mycc::nt<mycc::IdentifierAST> identifier,
                                         mycc::nt<mycc::EnumeratorListAST> enumeratorList)
    : AST(AST::Kind::ENUM_SPECIFIER), id(std::move(identifier)), enum_list(std::move(enumeratorList)) {}
mycc::EnumSpecifierAST::EnumSpecifierAST(mycc::nt<mycc::EnumeratorListAST> enumeratorList)
    : AST(AST::Kind::ENUM_SPECIFIER), id(nullptr), enum_list(std::move(enumeratorList)) {}
mycc::EnumSpecifierAST::EnumSpecifierAST(mycc::nt<mycc::IdentifierAST> identifier)
    : AST(AST::Kind::ENUM_SPECIFIER), id(std::move(identifier)), enum_list() {}
mycc::StructDeclarationAST::StructDeclarationAST(mycc::nts<mycc::SpecifierQualifierAST> specifier_qualifier,
                                                 mycc::nt<mycc::StructDeclaratorListAST> struct_declarator_list)
    : AST(AST::Kind::STRUCT_DECLARATION),
      spec_qual(std::move(specifier_qualifier)),
      decl_tor_list(std::move(struct_declarator_list)) {}
mycc::StructDeclaratorListAST::StructDeclaratorListAST(mycc::nts<mycc::StructDeclaratorAST> struct_declarators) : AST(
    AST::Kind::STRUCT_DECLARATOR_LIST) {}
mycc::StructDeclaratorAST::StructDeclaratorAST(mycc::nt<mycc::DeclaratorAST> declarator)
    : AST(AST::Kind::STRUCT_DECLARATOR, 0),
      declarator(std::move(declarator)),
      constant_expression(nullptr) {}
mycc::StructDeclaratorAST::StructDeclaratorAST(mycc::nt<mycc::DeclaratorAST> declarator,
                                               mycc::nt<mycc::ConstantExpressionAST> constant_expression)
    : AST(AST::Kind::STRUCT_DECLARATOR, 1),
      declarator(std::move(declarator)),
      constant_expression(nullptr) {}
mycc::StructDeclaratorAST::StructDeclaratorAST(mycc::nt<mycc::ConstantExpressionAST> constant_expression)
    : AST(AST::Kind::STRUCT_DECLARATOR, 2),
      declarator(nullptr),
      constant_expression(std::move(constant_expression)) {}
mycc::ConstantExpressionAST::ConstantExpressionAST(mycc::nt<mycc::ConditionalExpressionAST> conditional_expression)
    : AST(AST::Kind::CONSTANT_EXPRESSION), conditional_expression(std::move(conditional_expression)) {}
mycc::PointerAST::PointerAST(mycc::nts<mycc::TypeQualifierAST> type_qualifiers, mycc::nt<mycc::PointerAST> pointer)
    : AST(AST::Kind::POINTER), type_qualifiers(std::move(type_qualifiers)), pointer(std::move(pointer)) {}
mycc::DirectDeclaratorAST::DirectDeclaratorAST(mycc::nt<mycc::AST> term1,
                                               std::vector<std::pair<mycc::DirectDeclaratorAST::Term2,
                                                                     mycc::nt<mycc::AST>>> term2s)
    : AST(Kind::DIRECT_DECLARATOR) {}
mycc::ParameterTypeListAST::ParameterTypeListAST(mycc::nt<mycc::ParameterListAST> parameter_list, bool hasMultiple)
    : AST(AST::Kind::PARAMETER_TYPE_LIST, hasMultiple ? 1 : 0), parameter_list(std::move(parameter_list)) {}
mycc::ConditionalExpressionAST::ConditionalExpressionAST(mycc::nt<mycc::LogicalOrExpressionAST> logical_or_expression)
    : AST(AST::Kind::CONDITIONAL_EXPRESSION, 0),
      logical_or_expression(std::move(logical_or_expression)) {}
mycc::ConditionalExpressionAST::ConditionalExpressionAST(mycc::nt<mycc::LogicalOrExpressionAST> logical_or_expression,
                                                         mycc::nt<mycc::ExpressionAST> expression,
                                                         mycc::nt<mycc::ConditionalExpressionAST> conditional_expression)
    : AST(AST::Kind::CONDITIONAL_EXPRESSION, 1),
      logical_or_expression(std::move(logical_or_expression)),
      expression(std::move(expression)),
      conditional_expression(std::move(
          conditional_expression)) {}
mycc::LogicalOrExpressionAST::LogicalOrExpressionAST(mycc::nt<mycc::LogicalOrExpressionAST> left,
                                                     mycc::Operator<mycc::InfixOp> op,
                                                     mycc::nt<mycc::LogicalOrExpressionAST> right)
    : AST(AST::Kind::LOGICAL_OR_EXPRESSION) {}
mycc::LogicalOrExpressionAST::LogicalOrExpressionAST(mycc::nt<mycc::CastExpressionAST> leaf)
    : AST(AST::Kind::LOGICAL_OR_EXPRESSION) {}
mycc::ExpressionAST::ExpressionAST(mycc::nts<mycc::AssignmentExpressionAST>) : AST(AST::Kind::EXPRESSION, 0) {}
mycc::CastExpressionAST::CastExpressionAST(mycc::nt<mycc::UnaryExpressionAST>) : AST(AST::Kind::CAST_EXPRESSION, 0) {}
mycc::CastExpressionAST::CastExpressionAST(mycc::nt<mycc::TypeNameAST>, mycc::nt<mycc::CastExpressionAST>)
    : AST(AST::Kind::CAST_EXPRESSION, 1) {}
mycc::UnaryExpressionAST::UnaryExpressionAST(mycc::nt<mycc::TypeNameAST>) : AST(AST::Kind::UNARY_EXPRESSION) {}
mycc::UnaryExpressionAST::UnaryExpressionAST(mycc::Operator<mycc::UnaryOp> op, mycc::nt<mycc::CastExpressionAST>)
    : AST(AST::Kind::UNARY_EXPRESSION) {}
mycc::UnaryExpressionAST::UnaryExpressionAST(mycc::nt<mycc::UnaryExpressionAST>,
                                             mycc::UnaryExpressionAST::PrefixType type)
    : AST(AST::Kind::UNARY_EXPRESSION, static_cast<int>(type)) {}
mycc::UnaryExpressionAST::UnaryExpressionAST(mycc::nt<mycc::PostfixExpressionAST>) : AST(AST::Kind::UNARY_EXPRESSION,
                                                                                         0) {}
mycc::TypeNameAST::TypeNameAST(mycc::nts<mycc::SpecifierQualifierAST>, mycc::nt<mycc::DeclaratorAST>)
    : AST(AST::Kind::TYPE_NAME) {}
mycc::PostfixExpressionAST::PostfixExpressionAST(mycc::nt<mycc::PrimaryExpressionAST> primary,
                                                 std::vector<std::pair<int, mycc::nt<mycc::AST>>> terms)
    : AST(AST::Kind::POSTFIX_EXPRESSION, 0) {}
mycc::PrimaryExpressionAST::PrimaryExpressionAST(mycc::nt<mycc::IdentifierAST>) : AST(AST::Kind::PRIMARY_EXPRESSION,
                                                                                      0) {}
mycc::PrimaryExpressionAST::PrimaryExpressionAST(mycc::nt<mycc::IntegerConstantAST>)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 1) {}
mycc::PrimaryExpressionAST::PrimaryExpressionAST(mycc::nt<mycc::FloatingConstantAST>)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 1) {}
mycc::PrimaryExpressionAST::PrimaryExpressionAST(mycc::nt<mycc::CharacterConstantAST>)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 1) {}
mycc::PrimaryExpressionAST::PrimaryExpressionAST(mycc::nt<mycc::StringAST>) : AST(AST::Kind::PRIMARY_EXPRESSION, 2) {}
mycc::PrimaryExpressionAST::PrimaryExpressionAST(mycc::nt<mycc::ExpressionAST>) : AST(AST::Kind::PRIMARY_EXPRESSION,
                                                                                      3) {}
mycc::AssignmentExpressionAST::AssignmentExpressionAST(mycc::nt<mycc::ConditionalExpressionAST>)
    : AST(AST::Kind::ASSIGNMENT_EXPRESSION, 0) {}
mycc::AssignmentExpressionAST::AssignmentExpressionAST(mycc::nt<mycc::ConditionalExpressionAST>,
                                                       mycc::Operator<mycc::AssignmentOp>,
                                                       mycc::nt<mycc::AssignmentExpressionAST>)
    : AST(AST::Kind::ASSIGNMENT_EXPRESSION, 1) {}
mycc::ConstantAST::ConstantAST(mycc::nt<mycc::IntegerConstantAST>) : AST(AST::Kind::CONSTANT, 0) {}
mycc::ConstantAST::ConstantAST(mycc::nt<mycc::CharacterConstantAST>) : AST(AST::Kind::CONSTANT, 1) {}
mycc::ConstantAST::ConstantAST(mycc::nt<mycc::FloatingConstantAST>) : AST(AST::Kind::CONSTANT, 2) {}
mycc::ConstantAST::ConstantAST(mycc::nt<mycc::EnumerationConstantAST>) : AST(AST::Kind::CONSTANT, 3) {}
mycc::CharacterConstantAST::CharacterConstantAST(std::string)
    : AST(AST::Kind::CHARACTER_CONSTANT) {}
mycc::FloatingConstantAST::FloatingConstantAST(std::string)
    : AST(AST::Kind::FLOATING_CONSTANT) {}
mycc::EnumerationConstantAST::EnumerationConstantAST(mycc::nt<mycc::IdentifierAST>)
    : AST(AST::Kind::ENUMERATION_CONSTANT) {}
mycc::ParameterListAST::ParameterListAST(mycc::nts<mycc::ParameterDeclarationAST>) : AST(AST::Kind::PARAMETER_LIST) {}
mycc::ParameterDeclarationAST::ParameterDeclarationAST(mycc::nt<mycc::DeclarationSpecifiersAST>,
                                                       mycc::nt<mycc::DeclaratorAST>)
    : AST(AST::Kind::PARAMETER_DECLARATION) {}
mycc::EnumeratorListAST::EnumeratorListAST(mycc::nts<mycc::EnumeratorAST> &&) : AST(AST::Kind::ENUMERATOR_LIST) {}
mycc::EnumeratorAST::EnumeratorAST(mycc::nt<mycc::IdentifierAST>) : AST(AST::Kind::ENUMERATOR, 0) {}
mycc::EnumeratorAST::EnumeratorAST(mycc::nt<mycc::IdentifierAST>, mycc::nt<mycc::ConstantExpressionAST>)
    : AST(AST::Kind::ENUMERATOR, 1) {}
mycc::InitializerAST::InitializerAST(mycc::nt<mycc::AssignmentExpressionAST>) : AST(AST::Kind::INITIALIZER, 0) {}
mycc::InitializerAST::InitializerAST(mycc::nt<mycc::InitializerListAST>) : AST(AST::Kind::INITIALIZER, 1) {}
mycc::InitializerListAST::InitializerListAST(mycc::nts<mycc::InitializerAST>) : AST(AST::Kind::INITIALIZER_LIST) {}
mycc::StatementAST::StatementAST(mycc::nt<mycc::LabeledStatementAST>) : AST(AST::Kind::STATEMENT, 0) {}
mycc::StatementAST::StatementAST(mycc::nt<mycc::ExpressionStatementAST>) : AST(AST::Kind::STATEMENT, 1) {}
mycc::StatementAST::StatementAST(mycc::nt<mycc::CompoundStatementAST>) : AST(AST::Kind::STATEMENT, 2) {}
mycc::StatementAST::StatementAST(mycc::nt<mycc::SelectionStatementAST>) : AST(AST::Kind::STATEMENT, 3) {}
mycc::StatementAST::StatementAST(mycc::nt<mycc::IterationStatementAST>) : AST(AST::Kind::STATEMENT, 4) {}
mycc::StatementAST::StatementAST(mycc::nt<mycc::JumpStatementAST>) : AST(AST::Kind::STATEMENT, 5) {}
mycc::LabeledStatementAST::LabeledStatementAST(mycc::nt<mycc::IdentifierAST>, mycc::nt<mycc::StatementAST>)
    : AST(AST::Kind::LABELED_STATEMENT, 0) {}
mycc::LabeledStatementAST::LabeledStatementAST(mycc::nt<mycc::ConstantExpressionAST>, mycc::nt<mycc::StatementAST>)
    : AST(AST::Kind::LABELED_STATEMENT, 1) {}
mycc::LabeledStatementAST::LabeledStatementAST(mycc::nt<mycc::StatementAST>) : AST(AST::Kind::LABELED_STATEMENT, 2) {}
mycc::ExpressionStatementAST::ExpressionStatementAST(mycc::nt<mycc::ExpressionAST>)
    : AST(AST::Kind::EXPRESSION_STATEMENT) {}
mycc::SelectionStatementAST::SelectionStatementAST(mycc::nt<mycc::ExpressionAST>,
                                                   mycc::nt<mycc::StatementAST>,
                                                   bool is_if)
    : AST(AST::Kind::SELECTION_STATEMENT, is_if ? 0 : 2) {}
mycc::SelectionStatementAST::SelectionStatementAST(mycc::nt<mycc::ExpressionAST>,
                                                   mycc::nt<mycc::StatementAST>,
                                                   mycc::nt<mycc::StatementAST>)
    : AST(AST::Kind::SELECTION_STATEMENT, 1) {}
mycc::IterationStatementAST::IterationStatementAST(mycc::nt<mycc::ExpressionAST>, mycc::nt<mycc::StatementAST>) : AST(
    AST::Kind::ITERATION_STATEMENT,
    0) {}
mycc::IterationStatementAST::IterationStatementAST(mycc::nt<mycc::StatementAST>, mycc::nt<mycc::ExpressionAST>) : AST(
    AST::Kind::ITERATION_STATEMENT,
    1) {}
mycc::IterationStatementAST::IterationStatementAST(mycc::nt<mycc::ExpressionAST>,
                                                   mycc::nt<mycc::ExpressionAST>,
                                                   mycc::nt<mycc::ExpressionAST>,
                                                   mycc::nt<mycc::StatementAST>)
    : AST(AST::Kind::ITERATION_STATEMENT, 2) {}
mycc::JumpStatementAST::JumpStatementAST(mycc::nt<mycc::IdentifierAST>) : AST(AST::Kind::JUMP_STATEMENT, 0) {}
mycc::JumpStatementAST::JumpStatementAST(bool is_continue) : AST(AST::Kind::JUMP_STATEMENT, is_continue ? 1 : 2) {}
mycc::JumpStatementAST::JumpStatementAST(mycc::nt<mycc::ExpressionAST>) : AST(AST::Kind::JUMP_STATEMENT, 3) {}
mycc::TypeQualifierAST::TypeQualifierAST(bool is_const) : AST(AST::Kind::TYPE_QUALIFIER) {}
mycc::TypedefNameAST::TypedefNameAST(mycc::nt<mycc::IdentifierAST> identifier)
    : AST(AST::Kind::TYPEDEF_NAME), id(std::move(identifier)) {}
mycc::StructOrUnionAST::StructOrUnionAST(mycc::StructOrUnion struct_or_union) : AST(AST::Kind::STRUCT_OR_UNION) {}
mycc::IdentifierAST::IdentifierAST(mycc::Token token)
    : AST(AST::Kind::IDENTIFIER), token(std::move(token)) {}
mycc::StringAST::StringAST(std::string value) : AST(AST::Kind::STRING) {}
mycc::AST::AST(mycc::AST::Kind kind, int id) : kind(kind), pro_id(id) {
  ++indent;
  for (int i = 0; i <= indent; ++i) {
    std::cout << "  ";
  }
  std::cout << toString() << std::endl;
  --indent;
}
const char *mycc::AST::toString() {
  using namespace mycc;
  switch (kind) {
    case AST::Kind::TRANSLATION_UNIT:return "TRANSLATION_UNIT";
    case AST::Kind::EXTERNAL_DECLARATION:return "EXTERNAL_DECLARATION";
    case AST::Kind::FUNCTION_DEFINITION:return "FUNCTION_DEFINITION";
    case AST::Kind::DECLARATION:return "DECLARATION";
    case AST::Kind::DECLARATION_SPECIFIER:return "DECLARATION_SPECIFIER";
    case AST::Kind::DECLARATOR:return "DECLARATOR";
    case AST::Kind::COMPOUND_STATEMENT:return "COMPOUND_STATEMENT";
    case AST::Kind::STORAGE_CLASS_SPECIFIER:return "STORAGE_CLASS_SPECIFIER";
    case AST::Kind::TYPE_SPECIFIER:return "TYPE_SPECIFIER";
    case AST::Kind::TYPE_QUALIFIER:return "TYPE_QUALIFIER";
    case AST::Kind::STRUCT_OR_UNION_SPECIFIER:return "STRUCT_OR_UNION_SPECIFIER";
    case AST::Kind::ENUM_SPECIFIER:return "ENUM_SPECIFIER";
    case AST::Kind::TYPEDEF_NAME:return "TYPEDEF_NAME";
    case AST::Kind::STRUCT_OR_UNION:return "STRUCT_OR_UNION";
    case AST::Kind::IDENTIFIER:return "IDENTIFIER";
    case AST::Kind::STRUCT_DECLARATION:return "STRUCT_DECLARATION";
    case AST::Kind::SPECIFIER_QUALIFIER:return "SPECIFIER_QUALIFIER";
    case AST::Kind::STRUCT_DECLARATOR_LIST:return "STRUCT_DECLARATOR_LIST";
    case AST::Kind::STRUCT_DECLARATOR:return "STRUCT_DECLARATOR";
    case AST::Kind::CONSTANT_EXPRESSION:return "CONSTANT_EXPRESSION";
    case AST::Kind::POINTER:return "POINTER";
    case AST::Kind::DIRECT_DECLARATOR:return "DIRECT_DECLARATOR";
    case AST::Kind::PARAMETER_TYPE_LIST:return "PARAMETER_TYPE_LIST";
    case AST::Kind::CONDITIONAL_EXPRESSION:return "CONDITIONAL_EXPRESSION";
    case AST::Kind::EXPRESSION:return "EXPRESSION";
    case AST::Kind::CAST_EXPRESSION:return "CAST_EXPRESSION";
    case AST::Kind::UNARY_EXPRESSION:return "UNARY_EXPRESSION";
    case AST::Kind::LOGICAL_OR_EXPRESSION:return "LOGICAL_OR_EXPRESSION";
    case AST::Kind::TYPE_NAME:return "TYPE_NAME";
    case AST::Kind::POSTFIX_EXPRESSION:return "POSTFIX_EXPRESSION";
    case AST::Kind::UNARY_OPERATOR:return "UNARY_OPERATOR";
    case AST::Kind::PRIMARY_EXPRESSION:return "PRIMARY_EXPRESSION";
    case AST::Kind::ASSIGNMENT_EXPRESSION:return "ASSIGNMENT_EXPRESSION";
    case AST::Kind::CONSTANT:return "CONSTANT";
    case AST::Kind::STRING:return "STRING";
    case AST::Kind::INTEGER_CONSTANT:return "INTEGER_CONSTANT";
    case AST::Kind::CHARACTER_CONSTANT:return "CHARACTER_CONSTANT";
    case AST::Kind::FLOATING_CONSTANT:return "FLOATING_CONSTANT";
    case AST::Kind::ENUMERATION_CONSTANT:return "ENUMERATION_CONSTANT";
    case AST::Kind::PARAMETER_LIST:return "PARAMETER_LIST";
    case AST::Kind::PARAMETER_DECLARATION:return "PARAMETER_DECLARATION";
    case AST::Kind::ENUMERATOR_LIST:return "ENUMERATOR_LIST";
    case AST::Kind::ENUMERATOR:return "ENUMERATOR";
    case AST::Kind::INIT_DECLARATOR:return "INIT_DECLARATOR";
    case AST::Kind::INITIALIZER:return "INITIALIZER";
    case AST::Kind::INITIALIZER_LIST:return "INITIALIZER_LIST";
    case AST::Kind::STATEMENT:return "STATEMENT";
    case AST::Kind::LABELED_STATEMENT:return "LABELED_STATEMENT";
    case AST::Kind::EXPRESSION_STATEMENT:return "EXPRESSION_STATEMENT";
    case AST::Kind::SELECTION_STATEMENT:return "SELECTION_STATEMENT";
    case AST::Kind::ITERATION_STATEMENT:return "ITERATION_STATEMENT";
    case AST::Kind::JUMP_STATEMENT:return "JUMP_STATEMENT";
  }
}
