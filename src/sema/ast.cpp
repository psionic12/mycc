#include <sema/ast.h>
#include <iostream>
SemaException::SemaException(std::string error, const Token &token) :
    token(token), error(std::move(error)) {
  this->error.append("\n").append(token.getTokenInLine());
}
const char *SemaException::what() const noexcept {
  return error.c_str();
}
TranslationUnitAST::TranslationUnitAST(nts<ExternalDeclarationAST> external_declarations)
    : AST(AST::Kind::TRANSLATION_UNIT), external_declarations(std::move(external_declarations)) {}
ExternalDeclarationAST::ExternalDeclarationAST(nt<AST> def)
    : AST(AST::Kind::EXTERNAL_DECLARATION), def(std::move(def)) {
  const DeclarationSpecifiersAST *ds;
  if (def->getKind() == AST::Kind::FUNCTION_DEFINITION) {
    ds = static_cast<FunctionDefinitionAST *>(def.get())->declaration_spcifiers.get();
  } else {
    ds = static_cast<DeclarationAST *>(def.get())->declaration_specifiers.get();
  }

  //The storage-class specifiers auto and register shall not appear in the declaration specifiers in an external declaration.
  for (auto specifier : ds->storage_specifiers) {
    if (specifier.type == StorageSpecifier::kAUTO || specifier.type == StorageSpecifier::kREGISTER) {
      throw SemaException(
          "The storage-class specifiers auto and register shall not appear in the declaration specifiers in an external declaration.",
          specifier.token);
    }
  }

}
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
      declarations(std::move(declarations)),
      statements(std::move(statements)) {}
DeclarationAST::DeclarationAST(nt<DeclarationSpecifiersAST> declaration_specifiers,
                               InitDeclarators init_declarators)
    : AST(AST::Kind::DECLARATION),
      declaration_specifiers(std::move(declaration_specifiers)),
      init_declarators(std::move(init_declarators)) {}
DeclarationSpecifiersAST::DeclarationSpecifiersAST(std::vector<Operator<StorageSpecifier>> storage_specifiers,
                                                   nts<TypeSpecifierAST> type_specifiers,
                                                   nts<TypeQualifierAST> type_qualifiers)
    : AST(AST::Kind::DECLARATION_SPECIFIER),
      storage_specifiers(std::move(storage_specifiers)),
      type_specifiers(std::move(type_specifiers)),
      type_qualifiers(std::move(type_qualifiers)) {}
DeclaratorAST::DeclaratorAST(nt<PointerAST> pointer,
                             nt<DirectDeclaratorAST> direct_declarator)
    : AST(AST::Kind::DECLARATOR), pointer(std::move(pointer)), direct_declarator(std::move(direct_declarator)) {}
StorageClassSpecifierAST::StorageClassSpecifierAST(Operator<StorageSpecifier> storage_speicifier)
    : AST(AST::Kind::STORAGE_CLASS_SPECIFIER), storage_speicifier(storage_speicifier) {}
SpecifierQualifierAST::SpecifierQualifierAST(nt<TypeQualifierAST> speciler)
    : AST(AST::Kind::SPECIFIER_QUALIFIER, 1),
      spec(std::move(speciler)) {}
SpecifierQualifierAST::SpecifierQualifierAST(nt<TypeSpecifierAST> speciler)
    : AST(AST::Kind::SPECIFIER_QUALIFIER, 0),
      spec(std::move(speciler)) {}
ProtoTypeSpecifierAST::ProtoTypeSpecifierAST(Operator<ProtoTypeSpecifier> specifier)
    : AST(AST::Kind::PROTO_TYPE_SPECIFIER), Operator<ProtoTypeSpecifier>(specifier), specifier(specifier) {}
TypeSpecifierAST::TypeSpecifierAST(Operator<ProtoTypeSpecifier> specifier)
    : AST(AST::Kind::TYPE_SPECIFIER), specifier(std::make_unique<ProtoTypeSpecifierAST>(specifier)) {}
TypeSpecifierAST::TypeSpecifierAST(nt<StructOrUnionSpecifierAST> specifier)
    : AST(AST::Kind::TYPE_SPECIFIER, 9), specifier(std::move(specifier)) {}
TypeSpecifierAST::TypeSpecifierAST(nt<EnumSpecifierAST> specifier) : AST(AST::Kind::TYPE_SPECIFIER,
                                                                         10), specifier(std::move(specifier)) {}
TypeSpecifierAST::TypeSpecifierAST(nt<TypedefNameAST> specifier) : AST(AST::Kind::TYPE_SPECIFIER,
                                                                       11), specifier(std::move(specifier)) {}
StructOrUnionSpecifierAST::StructOrUnionSpecifierAST(StructOrUnion type,
                                                     nt<IdentifierAST> id,
                                                     nts<StructDeclarationAST> declarations)
    : AST(AST::Kind::DECLARATION), type(type), id(std::move(id)), declarations(std::move(declarations)) {}
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
      specifier_qualifier(std::move(specifier_qualifier)),
      struct_declarator_list(std::move(struct_declarator_list)) {}
StructDeclaratorListAST::StructDeclaratorListAST(nts<StructDeclaratorAST> struct_declarators) : AST(
    AST::Kind::STRUCT_DECLARATOR_LIST), struct_declarators(std::move(struct_declarators)) {}
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
                                         std::vector<std::pair<DirectDeclaratorAST::Term2, nt<AST>>> term2s)
    : AST(Kind::DIRECT_DECLARATOR), term1(std::move(term1)), term2s(std::move(term2s)) {}
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
    : AST(AST::Kind::LOGICAL_OR_EXPRESSION, 0),
      left(std::move(left)),
      op(std::make_unique<Operator<InfixOp>>(op)),
      right(std::move(right)) {}
LogicalOrExpressionAST::LogicalOrExpressionAST(nt<CastExpressionAST> leaf)
    : AST(AST::Kind::LOGICAL_OR_EXPRESSION, 1), left(std::move(leaf)), op(nullptr), right(nullptr) {}
ExpressionAST::ExpressionAST(nts<AssignmentExpressionAST> assignment_expression)
    : AST(AST::Kind::EXPRESSION, 0),
      assignment_expression(std::move(
          assignment_expression)) {}
CastExpressionAST::CastExpressionAST(nt<UnaryExpressionAST> unary_expression)
    : AST(AST::Kind::CAST_EXPRESSION, 0),
      unary_expression(std::move(
          unary_expression)),
      type_name(nullptr),
      cast_expression(nullptr) {}
CastExpressionAST::CastExpressionAST(nt<TypeNameAST> type_name, nt<CastExpressionAST> cast_expression)
    : AST(AST::Kind::CAST_EXPRESSION, 1),
      unary_expression(nullptr),
      type_name(std::move(type_name)),
      cast_expression(std::move(cast_expression)) {}
UnaryExpressionAST::UnaryExpressionAST(nt<TypeNameAST> type_name) : AST(AST::Kind::UNARY_EXPRESSION, 5) {}
UnaryExpressionAST::UnaryExpressionAST(Operator<UnaryOp> op, nt<CastExpressionAST> cast_expression)
    : AST(AST::Kind::UNARY_EXPRESSION, 3) {}
UnaryExpressionAST::UnaryExpressionAST(nt<UnaryExpressionAST> unary_expression,
                                       UnaryExpressionAST::PrefixType type)
    : AST(AST::Kind::UNARY_EXPRESSION, static_cast<int>(type)) {}
UnaryExpressionAST::UnaryExpressionAST(nt<PostfixExpressionAST> postfix_expression)
    : AST(AST::Kind::UNARY_EXPRESSION, 0) {}
TypeNameAST::TypeNameAST(nts<SpecifierQualifierAST> specifier, nt<DeclaratorAST> declarator)
    : AST(AST::Kind::TYPE_NAME), specifier(std::move(specifier)), declarator(std::move(declarator)) {}
PostfixExpressionAST::PostfixExpressionAST(nt<PrimaryExpressionAST> primary,
                                           std::vector<std::pair<int, nt<AST>>> terms)
    : AST(AST::Kind::POSTFIX_EXPRESSION, 0), primary(std::move(primary)), terms(std::move(terms)) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<IdentifierAST>)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 0) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<IntegerConstantAST>)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 1) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<FloatingConstantAST>)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 1) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<CharacterConstantAST>)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 1) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<StringAST>) : AST(AST::Kind::PRIMARY_EXPRESSION, 2) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<ExpressionAST>) : AST(AST::Kind::PRIMARY_EXPRESSION,
                                                                    3) {}
AssignmentExpressionAST::AssignmentExpressionAST(nt<ConditionalExpressionAST> conditional_expression)
    : AST(AST::Kind::ASSIGNMENT_EXPRESSION, 0), conditional_expression(std::move(conditional_expression)) {}
AssignmentExpressionAST::AssignmentExpressionAST(nt<ConditionalExpressionAST> conditional_expression,
                                                 Operator<AssignmentOp> op,
                                                 nt<AssignmentExpressionAST> assignment_expression)
    : AST(AST::Kind::ASSIGNMENT_EXPRESSION, 1), conditional_expression(std::move(conditional_expression)),
      op(std::make_unique<Operator<AssignmentOp>>(op)), assignment_expression(std::move(assignment_expression)) {}
ConstantAST::ConstantAST(nt<IntegerConstantAST>) : AST(AST::Kind::CONSTANT, 0) {}
ConstantAST::ConstantAST(nt<CharacterConstantAST>) : AST(AST::Kind::CONSTANT, 1) {}
ConstantAST::ConstantAST(nt<FloatingConstantAST>) : AST(AST::Kind::CONSTANT, 2) {}
ConstantAST::ConstantAST(nt<EnumerationConstantAST>) : AST(AST::Kind::CONSTANT, 3) {}
CharacterConstantAST::CharacterConstantAST(std::string)
    : AST(AST::Kind::CHARACTER_CONSTANT) {}
FloatingConstantAST::FloatingConstantAST(std::string)
    : AST(AST::Kind::FLOATING_CONSTANT) {}
EnumerationConstantAST::EnumerationConstantAST(nt<IdentifierAST>)
    : AST(AST::Kind::ENUMERATION_CONSTANT), id(std::move(id)) {}
ParameterListAST::ParameterListAST(nts<ParameterDeclarationAST> parameter_declaration)
    : AST(AST::Kind::PARAMETER_LIST), parameter_declaration(std::move(parameter_declaration)) {}
ParameterDeclarationAST::ParameterDeclarationAST(nt<DeclarationSpecifiersAST> declaration_specifiers,
                                                 nt<DeclaratorAST> declarator)
    : AST(AST::Kind::PARAMETER_DECLARATION), declaration_specifiers(std::move(declaration_specifiers)),
      declarator(std::move(declarator)) {}
EnumeratorListAST::EnumeratorListAST(nts<EnumeratorAST> enumerator)
    : AST(AST::Kind::ENUMERATOR_LIST), enumerator(std::move(enumerator)) {}
EnumeratorAST::EnumeratorAST(nt<IdentifierAST> id) : AST(AST::Kind::ENUMERATOR, 0), id(std::move(id)) {}
EnumeratorAST::EnumeratorAST(nt<IdentifierAST> id, nt<ConstantExpressionAST> constant_expression)
    : AST(AST::Kind::ENUMERATOR, 1), constant_expression(std::move(constant_expression)) {}
InitializerAST::InitializerAST(nt<AssignmentExpressionAST> assignment_expression)
    : AST(AST::Kind::INITIALIZER, 0), ast(std::move(assignment_expression)) {}
InitializerAST::InitializerAST(nt<InitializerListAST> initializer_list)
    : AST(AST::Kind::INITIALIZER, 1), ast(std::move(initializer_list)) {}
InitializerListAST::InitializerListAST(nts<InitializerAST> initializer)
    : AST(AST::Kind::INITIALIZER_LIST), initializer(std::move(initializer)) {}
StatementAST::StatementAST(nt<LabeledStatementAST> ast) : AST(AST::Kind::STATEMENT, 0), ast(std::move(ast)) {}
StatementAST::StatementAST(nt<ExpressionStatementAST> ast) : AST(AST::Kind::STATEMENT, 1), ast(std::move(ast)) {}
StatementAST::StatementAST(nt<CompoundStatementAST> ast) : AST(AST::Kind::STATEMENT, 2), ast(std::move(ast)) {}
StatementAST::StatementAST(nt<SelectionStatementAST> ast) : AST(AST::Kind::STATEMENT, 3), ast(std::move(ast)) {}
StatementAST::StatementAST(nt<IterationStatementAST> ast) : AST(AST::Kind::STATEMENT, 4), ast(std::move(ast)) {}
StatementAST::StatementAST(nt<JumpStatementAST> ast) : AST(AST::Kind::STATEMENT, 5), ast(std::move(ast)) {}
LabeledStatementAST::LabeledStatementAST(nt<IdentifierAST> id, nt<StatementAST> statement)
    : AST(AST::Kind::LABELED_STATEMENT, 0), id(std::move(id)), statement(std::move(statement)) {}
LabeledStatementAST::LabeledStatementAST(nt<ConstantExpressionAST> constant_expression, nt<StatementAST> statement)
    : AST(AST::Kind::LABELED_STATEMENT, 1),
      constant_expression(std::move(constant_expression)),
      statement(std::move(statement)) {}
LabeledStatementAST::LabeledStatementAST(nt<StatementAST> statement)
    : AST(AST::Kind::LABELED_STATEMENT, 2), statement(std::move(statement)) {}
ExpressionStatementAST::ExpressionStatementAST(nt<ExpressionAST> expression)
    : AST(AST::Kind::EXPRESSION_STATEMENT), expression(std::move(expression)) {}
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
  std::cout << toString() << std::endl;
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
