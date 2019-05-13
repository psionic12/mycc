#include <sema/ast.h>
#include <iostream>
static int indent;
TranslationUnitAST::TranslationUnitAST(nts<ExternalDeclarationAST> external_declarations)
    : AST(AST::Kind::TRANSLATION_UNIT) {}
ExternalDeclarationAST::ExternalDeclarationAST(nt<AST> def) : AST(AST::Kind::EXTERNAL_DECLARATION) {}
FunctionDefinitionAST::FunctionDefinitionAST(nt<DeclarationSpecifiersAST> declaration_spcifiers,
                                                   nt<DeclaratorAST> declarator,
                                                   nts<DeclarationAST> declarations,
                                                   nt<CompoundStatementAST> compound_statement)
    : AST(AST::Kind::FUNCTION_DEFINITION),
      declaration_spcifiers(std::move(declaration_spcifiers)),
      declarator(std::move(declarator)),
      declarations(std::move(declarations)),
      compound_statement(std::move(compound_statement)) {}
CompoundStatementAST::CompoundStatementAST(nts<DeclarationAST> declarations,
                                                 nts<StatementAST> statements)
    : AST(AST::Kind::COMPOUND_STATEMENT),
      decls(std::move(declarations)),
      stats(std::move(statements)) {}
DeclarationAST::DeclarationAST(nt<DeclarationSpecifiersAST> declaration_specifiers,
                                     InitDeclarators init_declarators) : AST(AST::Kind::DECLARATION) {}
DeclarationSpecifiersAST::DeclarationSpecifiersAST(std::vector<Operator<StorageSpecifier>> storage_specifiers,
                                                         nts<TypeSpecifierAST> type_specifiers,
                                                         nts<TypeQualifierAST> type_qualifiers)
    : AST(AST::Kind::DECLARATION_SPECIFIER),
      storage_specifiers(std::move(storage_specifiers)),
      type_specifiers(std::move(type_specifiers)),
      type_qualifiers(std::move(type_qualifiers)) {}
DeclaratorAST::DeclaratorAST(nt<PointerAST> pointer,
                                   nt<DirectDeclaratorAST> direct_declarator)
    : AST(AST::Kind::DECLARATOR) {}
StorageClassSpecifierAST::StorageClassSpecifierAST(Operator<StorageSpecifier> storage_speicifier)
    : AST(AST::Kind::STORAGE_CLASS_SPECIFIER) {}
SpecifierQualifierAST::SpecifierQualifierAST(nt<TypeQualifierAST> speciler)
    : AST(AST::Kind::SPECIFIER_QUALIFIER, 1),
      spec(std::move(speciler)) {}
SpecifierQualifierAST::SpecifierQualifierAST(nt<TypeSpecifierAST> speciler)
    : AST(AST::Kind::SPECIFIER_QUALIFIER, 0),
      spec(std::move(speciler)) {}
TypeSpecifierAST::TypeSpecifierAST(Operator<ProtoTypeSpecifier> type_specifier)
    : AST(AST::Kind::TYPE_SPECIFIER) {}
TypeSpecifierAST::TypeSpecifierAST(nt<StructOrUnionSpecifierAST> specifier)
    : AST(AST::Kind::TYPE_SPECIFIER, 9) {}
TypeSpecifierAST::TypeSpecifierAST(nt<EnumSpecifierAST> specifier) : AST(AST::Kind::TYPE_SPECIFIER,
                                                                                           10) {}
TypeSpecifierAST::TypeSpecifierAST(nt<TypedefNameAST> specifier) : AST(AST::Kind::TYPE_SPECIFIER,
                                                                                         11) {}
StructOrUnionSpecifierAST::StructOrUnionSpecifierAST(StructOrUnion type,
                                                           nt<IdentifierAST> id,
                                                           nts<StructDeclarationAST> declarations)
    : AST(AST::Kind::DECLARATION) {}
EnumSpecifierAST::EnumSpecifierAST(nt<IdentifierAST> identifier,
                                         nt<EnumeratorListAST> enumeratorList)
    : AST(AST::Kind::ENUM_SPECIFIER), id(std::move(identifier)), enum_list(std::move(enumeratorList)) {}
EnumSpecifierAST::EnumSpecifierAST(nt<EnumeratorListAST> enumeratorList)
    : AST(AST::Kind::ENUM_SPECIFIER), id(nullptr), enum_list(std::move(enumeratorList)) {}
EnumSpecifierAST::EnumSpecifierAST(nt<IdentifierAST> identifier)
    : AST(AST::Kind::ENUM_SPECIFIER), id(std::move(identifier)), enum_list() {}
StructDeclarationAST::StructDeclarationAST(nts<SpecifierQualifierAST> specifier_qualifier,
                                                 nt<StructDeclaratorListAST> struct_declarator_list)
    : AST(AST::Kind::STRUCT_DECLARATION),
      spec_qual(std::move(specifier_qualifier)),
      decl_tor_list(std::move(struct_declarator_list)) {}
StructDeclaratorListAST::StructDeclaratorListAST(nts<StructDeclaratorAST> struct_declarators) : AST(
    AST::Kind::STRUCT_DECLARATOR_LIST) {}
StructDeclaratorAST::StructDeclaratorAST(nt<DeclaratorAST> declarator)
    : AST(AST::Kind::STRUCT_DECLARATOR, 0),
      declarator(std::move(declarator)),
      constant_expression(nullptr) {}
StructDeclaratorAST::StructDeclaratorAST(nt<DeclaratorAST> declarator,
                                               nt<ConstantExpressionAST> constant_expression)
    : AST(AST::Kind::STRUCT_DECLARATOR, 1),
      declarator(std::move(declarator)),
      constant_expression(nullptr) {}
StructDeclaratorAST::StructDeclaratorAST(nt<ConstantExpressionAST> constant_expression)
    : AST(AST::Kind::STRUCT_DECLARATOR, 2),
      declarator(nullptr),
      constant_expression(std::move(constant_expression)) {}
ConstantExpressionAST::ConstantExpressionAST(nt<ConditionalExpressionAST> conditional_expression)
    : AST(AST::Kind::CONSTANT_EXPRESSION), conditional_expression(std::move(conditional_expression)) {}
PointerAST::PointerAST(nts<TypeQualifierAST> type_qualifiers, nt<PointerAST> pointer)
    : AST(AST::Kind::POINTER), type_qualifiers(std::move(type_qualifiers)), pointer(std::move(pointer)) {}
DirectDeclaratorAST::DirectDeclaratorAST(nt<AST> term1,
                                               std::vector<std::pair<DirectDeclaratorAST::Term2,
                                                                     nt<AST>>> term2s)
    : AST(Kind::DIRECT_DECLARATOR) {}
ParameterTypeListAST::ParameterTypeListAST(nt<ParameterListAST> parameter_list, bool hasMultiple)
    : AST(AST::Kind::PARAMETER_TYPE_LIST, hasMultiple ? 1 : 0), parameter_list(std::move(parameter_list)) {}
ConditionalExpressionAST::ConditionalExpressionAST(nt<LogicalOrExpressionAST> logical_or_expression)
    : AST(AST::Kind::CONDITIONAL_EXPRESSION, 0),
      logical_or_expression(std::move(logical_or_expression)) {}
ConditionalExpressionAST::ConditionalExpressionAST(nt<LogicalOrExpressionAST> logical_or_expression,
                                                         nt<ExpressionAST> expression,
                                                         nt<ConditionalExpressionAST> conditional_expression)
    : AST(AST::Kind::CONDITIONAL_EXPRESSION, 1),
      logical_or_expression(std::move(logical_or_expression)),
      expression(std::move(expression)),
      conditional_expression(std::move(
          conditional_expression)) {}
LogicalOrExpressionAST::LogicalOrExpressionAST(nt<LogicalOrExpressionAST> left,
                                                     Operator<InfixOp> op,
                                                     nt<LogicalOrExpressionAST> right)
    : AST(AST::Kind::LOGICAL_OR_EXPRESSION) {}
LogicalOrExpressionAST::LogicalOrExpressionAST(nt<CastExpressionAST> leaf)
    : AST(AST::Kind::LOGICAL_OR_EXPRESSION) {}
ExpressionAST::ExpressionAST(nts<AssignmentExpressionAST>) : AST(AST::Kind::EXPRESSION, 0) {}
CastExpressionAST::CastExpressionAST(nt<UnaryExpressionAST>) : AST(AST::Kind::CAST_EXPRESSION, 0) {}
CastExpressionAST::CastExpressionAST(nt<TypeNameAST>, nt<CastExpressionAST>)
    : AST(AST::Kind::CAST_EXPRESSION, 1) {}
UnaryExpressionAST::UnaryExpressionAST(nt<TypeNameAST>) : AST(AST::Kind::UNARY_EXPRESSION) {}
UnaryExpressionAST::UnaryExpressionAST(Operator<UnaryOp> op, nt<CastExpressionAST>)
    : AST(AST::Kind::UNARY_EXPRESSION) {}
UnaryExpressionAST::UnaryExpressionAST(nt<UnaryExpressionAST>,
                                             UnaryExpressionAST::PrefixType type)
    : AST(AST::Kind::UNARY_EXPRESSION, static_cast<int>(type)) {}
UnaryExpressionAST::UnaryExpressionAST(nt<PostfixExpressionAST>) : AST(AST::Kind::UNARY_EXPRESSION,
                                                                                         0) {}
TypeNameAST::TypeNameAST(nts<SpecifierQualifierAST>, nt<DeclaratorAST>)
    : AST(AST::Kind::TYPE_NAME) {}
PostfixExpressionAST::PostfixExpressionAST(nt<PrimaryExpressionAST> primary,
                                                 std::vector<std::pair<int, nt<AST>>> terms)
    : AST(AST::Kind::POSTFIX_EXPRESSION, 0) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<IdentifierAST>) : AST(AST::Kind::PRIMARY_EXPRESSION,
                                                                                      0) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<IntegerConstantAST>)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 1) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<FloatingConstantAST>)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 1) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<CharacterConstantAST>)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 1) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<StringAST>) : AST(AST::Kind::PRIMARY_EXPRESSION, 2) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<ExpressionAST>) : AST(AST::Kind::PRIMARY_EXPRESSION,
                                                                                      3) {}
AssignmentExpressionAST::AssignmentExpressionAST(nt<ConditionalExpressionAST>)
    : AST(AST::Kind::ASSIGNMENT_EXPRESSION, 0) {}
AssignmentExpressionAST::AssignmentExpressionAST(nt<ConditionalExpressionAST>,
                                                       Operator<AssignmentOp>,
                                                       nt<AssignmentExpressionAST>)
    : AST(AST::Kind::ASSIGNMENT_EXPRESSION, 1) {}
ConstantAST::ConstantAST(nt<IntegerConstantAST>) : AST(AST::Kind::CONSTANT, 0) {}
ConstantAST::ConstantAST(nt<CharacterConstantAST>) : AST(AST::Kind::CONSTANT, 1) {}
ConstantAST::ConstantAST(nt<FloatingConstantAST>) : AST(AST::Kind::CONSTANT, 2) {}
ConstantAST::ConstantAST(nt<EnumerationConstantAST>) : AST(AST::Kind::CONSTANT, 3) {}
CharacterConstantAST::CharacterConstantAST(std::string)
    : AST(AST::Kind::CHARACTER_CONSTANT) {}
FloatingConstantAST::FloatingConstantAST(std::string)
    : AST(AST::Kind::FLOATING_CONSTANT) {}
EnumerationConstantAST::EnumerationConstantAST(nt<IdentifierAST>)
    : AST(AST::Kind::ENUMERATION_CONSTANT) {}
ParameterListAST::ParameterListAST(nts<ParameterDeclarationAST>) : AST(AST::Kind::PARAMETER_LIST) {}
ParameterDeclarationAST::ParameterDeclarationAST(nt<DeclarationSpecifiersAST>,
                                                       nt<DeclaratorAST>)
    : AST(AST::Kind::PARAMETER_DECLARATION) {}
EnumeratorListAST::EnumeratorListAST(nts<EnumeratorAST> &&) : AST(AST::Kind::ENUMERATOR_LIST) {}
EnumeratorAST::EnumeratorAST(nt<IdentifierAST>) : AST(AST::Kind::ENUMERATOR, 0) {}
EnumeratorAST::EnumeratorAST(nt<IdentifierAST>, nt<ConstantExpressionAST>)
    : AST(AST::Kind::ENUMERATOR, 1) {}
InitializerAST::InitializerAST(nt<AssignmentExpressionAST>) : AST(AST::Kind::INITIALIZER, 0) {}
InitializerAST::InitializerAST(nt<InitializerListAST>) : AST(AST::Kind::INITIALIZER, 1) {}
InitializerListAST::InitializerListAST(nts<InitializerAST>) : AST(AST::Kind::INITIALIZER_LIST) {}
StatementAST::StatementAST(nt<LabeledStatementAST>) : AST(AST::Kind::STATEMENT, 0) {}
StatementAST::StatementAST(nt<ExpressionStatementAST>) : AST(AST::Kind::STATEMENT, 1) {}
StatementAST::StatementAST(nt<CompoundStatementAST>) : AST(AST::Kind::STATEMENT, 2) {}
StatementAST::StatementAST(nt<SelectionStatementAST>) : AST(AST::Kind::STATEMENT, 3) {}
StatementAST::StatementAST(nt<IterationStatementAST>) : AST(AST::Kind::STATEMENT, 4) {}
StatementAST::StatementAST(nt<JumpStatementAST>) : AST(AST::Kind::STATEMENT, 5) {}
LabeledStatementAST::LabeledStatementAST(nt<IdentifierAST>, nt<StatementAST>)
    : AST(AST::Kind::LABELED_STATEMENT, 0) {}
LabeledStatementAST::LabeledStatementAST(nt<ConstantExpressionAST>, nt<StatementAST>)
    : AST(AST::Kind::LABELED_STATEMENT, 1) {}
LabeledStatementAST::LabeledStatementAST(nt<StatementAST>) : AST(AST::Kind::LABELED_STATEMENT, 2) {}
ExpressionStatementAST::ExpressionStatementAST(nt<ExpressionAST>)
    : AST(AST::Kind::EXPRESSION_STATEMENT) {}
SelectionStatementAST::SelectionStatementAST(nt<ExpressionAST>,
                                                   nt<StatementAST>,
                                                   bool is_if)
    : AST(AST::Kind::SELECTION_STATEMENT, is_if ? 0 : 2) {}
SelectionStatementAST::SelectionStatementAST(nt<ExpressionAST>,
                                                   nt<StatementAST>,
                                                   nt<StatementAST>)
    : AST(AST::Kind::SELECTION_STATEMENT, 1) {}
IterationStatementAST::IterationStatementAST(nt<ExpressionAST>, nt<StatementAST>) : AST(
    AST::Kind::ITERATION_STATEMENT,
    0) {}
IterationStatementAST::IterationStatementAST(nt<StatementAST>, nt<ExpressionAST>) : AST(
    AST::Kind::ITERATION_STATEMENT,
    1) {}
IterationStatementAST::IterationStatementAST(nt<ExpressionAST>,
                                                   nt<ExpressionAST>,
                                                   nt<ExpressionAST>,
                                                   nt<StatementAST>)
    : AST(AST::Kind::ITERATION_STATEMENT, 2) {}
JumpStatementAST::JumpStatementAST(nt<IdentifierAST>) : AST(AST::Kind::JUMP_STATEMENT, 0) {}
JumpStatementAST::JumpStatementAST(bool is_continue) : AST(AST::Kind::JUMP_STATEMENT, is_continue ? 1 : 2) {}
JumpStatementAST::JumpStatementAST(nt<ExpressionAST>) : AST(AST::Kind::JUMP_STATEMENT, 3) {}
TypeQualifierAST::TypeQualifierAST(Operator<TypeQuailifier> op) : AST(AST::Kind::TYPE_QUALIFIER) {}
TypedefNameAST::TypedefNameAST(nt<IdentifierAST> identifier)
    : AST(AST::Kind::TYPEDEF_NAME), id(std::move(identifier)) {}
StructOrUnionAST::StructOrUnionAST(StructOrUnion struct_or_union) : AST(AST::Kind::STRUCT_OR_UNION) {}
IdentifierAST::IdentifierAST(Token token)
    : AST(AST::Kind::IDENTIFIER), token(std::move(token)) {}
StringAST::StringAST(std::string value) : AST(AST::Kind::STRING) {}
AST::AST(AST::Kind kind, int id) : kind(kind), pro_id(id) {
  ++indent;
  for (int i = 0; i <= indent; ++i) {
    std::cout << "  ";
  }
  std::cout << toString() << std::endl;
  --indent;
}
const char *AST::toString() {
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
