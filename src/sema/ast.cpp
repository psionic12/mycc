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
void TranslationUnitAST::print(int indent) {
  AST::print(indent);
  external_declarations.print(++indent);
}
ExternalDeclarationAST::ExternalDeclarationAST(nt<AST> def)
    : AST(AST::Kind::EXTERNAL_DECLARATION), def(std::move(def)) {
  const DeclarationSpecifiersAST *ds;
  if (this->def->getKind() == AST::Kind::FUNCTION_DEFINITION) {
    ds = static_cast<FunctionDefinitionAST *>(this->def.get())->declaration_spcifiers.get();
  } else {
    ds = static_cast<DeclarationAST *>(this->def.get())->declaration_specifiers.get();
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
void ExternalDeclarationAST::print(int indent) {
  AST::print(indent);
  def->print(++indent);
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
void FunctionDefinitionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  declaration_spcifiers->print(indent);
  declarator->print(indent);
  declarations.print(indent);
  compound_statement->print(indent);
}
CompoundStatementAST::CompoundStatementAST(nts<DeclarationAST> declarations,
                                           nts<StatementAST> statements,
                                           SymbolTable &table)
    : AST(AST::Kind::COMPOUND_STATEMENT),
      declarations(std::move(declarations)),
      statements(std::move(statements)), table(table) {}
void CompoundStatementAST::print(int indent) {
  AST::print(indent);
  ++indent;
  declarations.print(indent);
  statements.print(indent);
}
DeclarationAST::DeclarationAST(nt<DeclarationSpecifiersAST> declaration_specifiers, InitDeclarators init_declarators)
    : AST(AST::Kind::DECLARATION),
      declaration_specifiers(std::move(declaration_specifiers)),
      init_declarators(std::move(init_declarators)) {}
void DeclarationAST::print(int indent) {
  AST::print(indent);
  ++indent;
  declaration_specifiers->print(indent);
  for (const auto &ds : init_declarators) {
    ds.first->print(indent);
    if (ds.second) ds.second->print(indent);
  }
}
DeclarationSpecifiersAST::DeclarationSpecifiersAST(ts<StorageSpecifier> storage_specifiers,
                                                   nts<TypeSpecifierAST> type_specifiers,
                                                   nts<TypeQualifierAST> type_qualifiers)
    : AST(AST::Kind::DECLARATION_SPECIFIER),
      storage_specifiers(std::move(storage_specifiers)),
      type_specifiers(std::move(type_specifiers)),
      type_qualifiers(std::move(type_qualifiers)) {}
void DeclarationSpecifiersAST::print(int indent) {
  AST::print(indent);
  ++indent;
  storage_specifiers.print(indent);
  type_qualifiers.print(indent);
  type_specifiers.print(indent);
}
DeclaratorAST::DeclaratorAST(nt<PointerAST> pointer,
                             nt<DirectDeclaratorAST> direct_declarator)
    : AST(AST::Kind::DECLARATOR), pointer(std::move(pointer)), direct_declarator(std::move(direct_declarator)) {}
void DeclaratorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  if (pointer) pointer->print(indent);
  direct_declarator->print(indent);
}
StorageClassSpecifierAST::StorageClassSpecifierAST(Terminal<StorageSpecifier> storage_speicifier)
    : AST(AST::Kind::STORAGE_CLASS_SPECIFIER), storage_speicifier(storage_speicifier) {}
void StorageClassSpecifierAST::print(int indent) {
  AST::print(indent);
  storage_speicifier.print(++indent);
}
SpecifierQualifierAST::SpecifierQualifierAST(nt<TypeQualifierAST> speciler)
    : AST(AST::Kind::SPECIFIER_QUALIFIER, 1),
      speciler(std::move(speciler)) {}
SpecifierQualifierAST::SpecifierQualifierAST(nt<TypeSpecifierAST> speciler)
    : AST(AST::Kind::SPECIFIER_QUALIFIER, 0),
      speciler(std::move(speciler)) {}
void SpecifierQualifierAST::print(int indent) {
  AST::print(indent);
  speciler->print(++indent);
}
ProtoTypeSpecifierAST::ProtoTypeSpecifierAST(Terminal<ProtoTypeSpecifier> specifier)
    : AST(AST::Kind::PROTO_TYPE_SPECIFIER), Terminal<ProtoTypeSpecifier>(specifier), specifier(specifier) {}
void ProtoTypeSpecifierAST::print(int indent) {
  AST::print(indent);
  specifier.print(++indent);
}
TypeSpecifierAST::TypeSpecifierAST(Terminal<ProtoTypeSpecifier> specifier)
    : AST(AST::Kind::TYPE_SPECIFIER), specifier(std::make_unique<ProtoTypeSpecifierAST>(specifier)) {}
TypeSpecifierAST::TypeSpecifierAST(nt<StructOrUnionSpecifierAST> specifier)
    : AST(AST::Kind::TYPE_SPECIFIER, 9), specifier(std::move(specifier)) {}
TypeSpecifierAST::TypeSpecifierAST(nt<EnumSpecifierAST> specifier) : AST(AST::Kind::TYPE_SPECIFIER,
                                                                         10), specifier(std::move(specifier)) {}
TypeSpecifierAST::TypeSpecifierAST(nt<TypedefNameAST> specifier) : AST(AST::Kind::TYPE_SPECIFIER,
                                                                       11), specifier(std::move(specifier)) {}
void TypeSpecifierAST::print(int indent) {
  AST::print(indent);
  specifier->print(++indent);
}
StructOrUnionSpecifierAST::StructOrUnionSpecifierAST(StructOrUnion type,
                                                     nt<IdentifierAST> id,
                                                     nts<StructDeclarationAST> declarations)
    : AST(AST::Kind::STRUCT_OR_UNION_SPECIFIER), type(type), id(std::move(id)), declarations(std::move(declarations)) {}
void StructOrUnionSpecifierAST::print(int indent) {
  AST::print(indent);
  ++indent;
  AST::printIndent(indent);
  type == StructOrUnion::kSTRUCT ? std::cout << "struct" : std::cout << "union";
  if (id != nullptr) id->print(indent);
  declarations.print(indent);
}
EnumSpecifierAST::EnumSpecifierAST(nt<IdentifierAST> identifier,
                                   nt<EnumeratorListAST> enumeratorList)
    : AST(AST::Kind::ENUM_SPECIFIER), id(std::move(identifier)), enum_list(std::move(enumeratorList)) {}
EnumSpecifierAST::EnumSpecifierAST(nt<EnumeratorListAST> enumeratorList)
    : AST(AST::Kind::ENUM_SPECIFIER), id(nullptr), enum_list(std::move(enumeratorList)) {}
EnumSpecifierAST::EnumSpecifierAST(nt<IdentifierAST> identifier)
    : AST(AST::Kind::ENUM_SPECIFIER), id(std::move(identifier)), enum_list() {}
void EnumSpecifierAST::print(int indent) {
  AST::print(indent);
  ++indent;
  id->print(indent);
  enum_list->print(indent);
}
StructDeclarationAST::StructDeclarationAST(nts<SpecifierQualifierAST> specifier_qualifier,
                                           nt<StructDeclaratorListAST> struct_declarator_list)
    : AST(AST::Kind::STRUCT_DECLARATION),
      specifier_qualifier(std::move(specifier_qualifier)),
      struct_declarator_list(std::move(struct_declarator_list)) {}
void StructDeclarationAST::print(int indent) {
  AST::print(indent);
  ++indent;
  specifier_qualifier.print(indent);
  struct_declarator_list->print(indent);
}
StructDeclaratorListAST::StructDeclaratorListAST(nts<StructDeclaratorAST> struct_declarators) : AST(
    AST::Kind::STRUCT_DECLARATOR_LIST), struct_declarators(std::move(struct_declarators)) {}
void StructDeclaratorListAST::print(int indent) {
  AST::print(indent);
  struct_declarators.print(++indent);
}
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
void StructDeclaratorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  if (declarator != nullptr) declarator->print(indent);
  if (constant_expression != nullptr) constant_expression->print(indent);
}
ConstantExpressionAST::ConstantExpressionAST(nt<ConditionalExpressionAST> conditional_expression)
    : AST(AST::Kind::CONSTANT_EXPRESSION), conditional_expression(std::move(conditional_expression)) {}
void ConstantExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  conditional_expression->print(indent);
}
PointerAST::PointerAST(nts<TypeQualifierAST> type_qualifiers, nt<PointerAST> pointer)
    : AST(AST::Kind::POINTER), type_qualifiers(std::move(type_qualifiers)), pointer(std::move(pointer)) {}
void PointerAST::print(int indent) {
  AST::print(indent);
  ++indent;
  type_qualifiers.print(indent);
  if (pointer) pointer->print(indent);
}
DirectDeclaratorAST::DirectDeclaratorAST(nt<AST> term1,
                                         std::vector<std::pair<DirectDeclaratorAST::Term2, nt<AST>>> term2s)
    : AST(Kind::DIRECT_DECLARATOR), term1(std::move(term1)), term2s(std::move(term2s)) {}
void DirectDeclaratorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  term1->print(indent);
  for (const auto &term : term2s) {
    if (term.second) {
      term.second->print(indent);
    } else {
      AST::printIndent(indent);
      std::cout << static_cast<int>(term.first) << std::endl;
    }
  }
}
ParameterTypeListAST::ParameterTypeListAST(nt<ParameterListAST> parameter_list, bool hasMultiple)
    : AST(AST::Kind::PARAMETER_TYPE_LIST, hasMultiple ? 1 : 0), parameter_list(std::move(parameter_list)) {}
void ParameterTypeListAST::print(int indent) {
  AST::print(indent);
  parameter_list->print(++indent);
}
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
void ConditionalExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  logical_or_expression->print(indent);
  if (expression) {
    expression->print(indent);
    conditional_expression->print(indent);
  }
}
LogicalOrExpressionAST::LogicalOrExpressionAST(nt<LogicalOrExpressionAST> left,
                                               Terminal<InfixOp> op,
                                               nt<LogicalOrExpressionAST> right)
    : AST(AST::Kind::LOGICAL_OR_EXPRESSION, 0),
      left(std::move(left)),
      op(std::make_unique<Terminal<InfixOp>>(op)),
      right(std::move(right)) {}
LogicalOrExpressionAST::LogicalOrExpressionAST(nt<CastExpressionAST> leaf)
    : AST(AST::Kind::LOGICAL_OR_EXPRESSION, 1), left(std::move(leaf)), op(nullptr), right(nullptr) {}
void LogicalOrExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  left->print(indent);
  if (op) {
    op->print(indent);
    right->print(indent);
  }
}
ExpressionAST::ExpressionAST(nts<AssignmentExpressionAST> assignment_expression)
    : AST(AST::Kind::EXPRESSION, 0),
      assignment_expression(std::move(
          assignment_expression)) {}
void ExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  assignment_expression.print(indent);
}
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
void CastExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  if (getProduction() == 0) {
    unary_expression->print(indent);
  } else {
    type_name->print(indent);
    cast_expression->print(indent);
  }
}
UnaryExpressionAST::UnaryExpressionAST(nt<TypeNameAST> type_name)
    : AST(AST::Kind::UNARY_EXPRESSION, 5), type_name(std::move(type_name)) {}
UnaryExpressionAST::UnaryExpressionAST(Terminal<UnaryOp> op, nt<CastExpressionAST> cast_expression)
    : AST(AST::Kind::UNARY_EXPRESSION, 3),
      op(std::make_unique<Terminal<UnaryOp>>(op)),
      cast_expression(std::move(cast_expression)) {}
UnaryExpressionAST::UnaryExpressionAST(nt<UnaryExpressionAST> unary_expression,
                                       UnaryExpressionAST::PrefixType type)
    : AST(AST::Kind::UNARY_EXPRESSION, static_cast<int>(type)), unary_expression(std::move(unary_expression)) {}
UnaryExpressionAST::UnaryExpressionAST(nt<PostfixExpressionAST> postfix_expression)
    : AST(AST::Kind::UNARY_EXPRESSION, 0), postfix_expression(std::move(postfix_expression)) {}
void UnaryExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  switch (getProduction()) {
    case 0:postfix_expression->print(indent);
      break;
    case 1:
    case 2:unary_expression->print(indent);
      break;
    case 3:unary_expression->print(indent);
      cast_expression->print(indent);
      break;
    case 4:unary_expression->print(indent);
      break;
    case 5:type_name->print(indent);
      break;
  }
}

TypeNameAST::TypeNameAST(nts<SpecifierQualifierAST> specifier, nt<DeclaratorAST> declarator)
    : AST(AST::Kind::TYPE_NAME), specifier(std::move(specifier)), declarator(std::move(declarator)) {}
void TypeNameAST::print(int indent) {
  AST::print(indent);
  ++indent;
  specifier.print(indent);
  if (declarator) declarator->print(indent);
}
PostfixExpressionAST::PostfixExpressionAST(nt<PrimaryExpressionAST> primary,
                                           std::vector<std::pair<int, nt<AST>>> terms)
    : AST(AST::Kind::POSTFIX_EXPRESSION, 0), primary(std::move(primary)), terms(std::move(terms)) {}
void PostfixExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  primary->print(indent);
  for (const auto &term : terms) {
    term.second->print(indent);
  }
}
PrimaryExpressionAST::PrimaryExpressionAST(nt<IdentifierAST> id)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 0), ast(std::move(id)) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<IntegerConstantAST> interger_constant)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 1), ast(std::move(interger_constant)) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<FloatingConstantAST> floating_constatnt)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 1), ast(std::move(floating_constatnt)) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<CharacterConstantAST> character_constant)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 1), ast(std::move(character_constant)) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<StringAST> string)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 2), ast(std::move(string)) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<ExpressionAST> exp)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 3), ast(std::move(exp)) {}
void PrimaryExpressionAST::print(int indent) {
  AST::print(indent);
  ast->print(++indent);
}
AssignmentExpressionAST::AssignmentExpressionAST(nt<ConditionalExpressionAST> conditional_expression)
    : AST(AST::Kind::ASSIGNMENT_EXPRESSION, 0), conditional_expression(std::move(conditional_expression)) {}
AssignmentExpressionAST::AssignmentExpressionAST(nt<ConditionalExpressionAST> conditional_expression,
                                                 Terminal<AssignmentOp> op,
                                                 nt<AssignmentExpressionAST> assignment_expression)
    : AST(AST::Kind::ASSIGNMENT_EXPRESSION, 1), conditional_expression(std::move(conditional_expression)),
      op(std::make_unique<Terminal<AssignmentOp>>(op)), assignment_expression(std::move(assignment_expression)) {}
void AssignmentExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  conditional_expression->print(indent);
  if (getProduction() == 1) {
    op->print(indent);
    assignment_expression->print(indent);
  }
}
CharacterConstantAST::CharacterConstantAST(std::string)
    : AST(AST::Kind::CHARACTER_CONSTANT) {}
FloatingConstantAST::FloatingConstantAST(std::string)
    : AST(AST::Kind::FLOATING_CONSTANT) {}
EnumerationConstantAST::EnumerationConstantAST(nt<IdentifierAST> id)
    : AST(AST::Kind::ENUMERATION_CONSTANT), id(std::move(id)) {}
ParameterListAST::ParameterListAST(nts<ParameterDeclarationAST> parameter_declaration, SymbolTable &table)
    : AST(AST::Kind::PARAMETER_LIST), parameter_declaration(std::move(parameter_declaration)), table(table) {}
void ParameterListAST::print(int indent) {
  AST::print(indent);
  parameter_declaration.print(++indent);
}
ParameterDeclarationAST::ParameterDeclarationAST(nt<DeclarationSpecifiersAST> declaration_specifiers,
                                                 nt<DeclaratorAST> declarator)
    : AST(AST::Kind::PARAMETER_DECLARATION), declaration_specifiers(std::move(declaration_specifiers)),
      declarator(std::move(declarator)) {}
void ParameterDeclarationAST::print(int indent) {
  AST::print(indent);
  ++indent;
  declaration_specifiers->print(indent);
  if (declarator) declarator->print(indent);
}
EnumeratorListAST::EnumeratorListAST(nts<EnumeratorAST> enumerator)
    : AST(AST::Kind::ENUMERATOR_LIST), enumerator(std::move(enumerator)) {}
void EnumeratorListAST::print(int indent) {
  AST::print(indent);
  enumerator.print(++indent);
}
EnumeratorAST::EnumeratorAST(nt<IdentifierAST> id) : AST(AST::Kind::ENUMERATOR, 0), id(std::move(id)) {}
EnumeratorAST::EnumeratorAST(nt<IdentifierAST> id, nt<ConstantExpressionAST> constant_expression)
    : AST(AST::Kind::ENUMERATOR, 1), id(std::move(id)), constant_expression(std::move(constant_expression)) {}
void EnumeratorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  id->print(indent);
  if (getProduction() == 1) {
    constant_expression->print(indent);
  }
}
InitializerAST::InitializerAST(nt<AssignmentExpressionAST> assignment_expression)
    : AST(AST::Kind::INITIALIZER, 0), ast(std::move(assignment_expression)) {}
InitializerAST::InitializerAST(nt<InitializerListAST> initializer_list)
    : AST(AST::Kind::INITIALIZER, 1), ast(std::move(initializer_list)) {}
void InitializerAST::print(int indent) {
  AST::print(indent);
  ast->print(++indent);
}
InitializerListAST::InitializerListAST(nts<InitializerAST> initializer)
    : AST(AST::Kind::INITIALIZER_LIST), initializer(std::move(initializer)) {}
void InitializerListAST::print(int indent) {
  AST::print(indent);
  initializer.print(++indent);
}
StatementAST::StatementAST(nt<LabeledStatementAST> ast) : AST(AST::Kind::STATEMENT, 0), ast(std::move(ast)) {}
StatementAST::StatementAST(nt<ExpressionStatementAST> ast) : AST(AST::Kind::STATEMENT, 1), ast(std::move(ast)) {}
StatementAST::StatementAST(nt<CompoundStatementAST> ast) : AST(AST::Kind::STATEMENT, 2), ast(std::move(ast)) {}
StatementAST::StatementAST(nt<SelectionStatementAST> ast) : AST(AST::Kind::STATEMENT, 3), ast(std::move(ast)) {}
StatementAST::StatementAST(nt<IterationStatementAST> ast) : AST(AST::Kind::STATEMENT, 4), ast(std::move(ast)) {}
StatementAST::StatementAST(nt<JumpStatementAST> ast) : AST(AST::Kind::STATEMENT, 5), ast(std::move(ast)) {}
void StatementAST::print(int indent) {
  AST::print(indent);
  ast->print(++indent);
}
LabeledStatementAST::LabeledStatementAST(nt<IdentifierAST> id, nt<StatementAST> statement)
    : AST(AST::Kind::LABELED_STATEMENT, 0), id(std::move(id)), statement(std::move(statement)) {}
LabeledStatementAST::LabeledStatementAST(nt<ConstantExpressionAST> constant_expression, nt<StatementAST> statement)
    : AST(AST::Kind::LABELED_STATEMENT, 1),
      constant_expression(std::move(constant_expression)),
      statement(std::move(statement)) {}
LabeledStatementAST::LabeledStatementAST(nt<StatementAST> statement)
    : AST(AST::Kind::LABELED_STATEMENT, 2), statement(std::move(statement)) {}
void LabeledStatementAST::print(int indent) {
  AST::print(indent);
  ++indent;
  if (getProduction() == 0) {
    id->print(indent);
  } else if (getProduction() == 1) {
    constant_expression->print(indent);
  }
  statement->print(indent);
}
ExpressionStatementAST::ExpressionStatementAST(nt<ExpressionAST> expression)
    : AST(AST::Kind::EXPRESSION_STATEMENT), expression(std::move(expression)) {}
void ExpressionStatementAST::print(int indent) {
  AST::print(indent);
  expression->print(++indent);
}
SelectionStatementAST::SelectionStatementAST(nt<ExpressionAST> expression,
                                             nt<StatementAST> statement,
                                             bool is_if)
    : AST(AST::Kind::SELECTION_STATEMENT, is_if ? 0 : 2),
      expression(std::move(expression)),
      statement(std::move(statement)) {}
SelectionStatementAST::SelectionStatementAST(nt<ExpressionAST> expression,
                                             nt<StatementAST> statement,
                                             nt<StatementAST> else_statement)
    : AST(AST::Kind::SELECTION_STATEMENT, 1), expression(std::move(expression)), statement(std::move(statement)),
      else_statement(std::move(else_statement)) {}
void SelectionStatementAST::print(int indent) {
  AST::print(indent);
  ++indent;
  expression->print(indent);
  statement->print(indent);
  if (else_statement) {
    else_statement->print(indent);
  }
}
IterationStatementAST::IterationStatementAST(nt<ExpressionAST> expression, nt<StatementAST> statement) : AST(
    AST::Kind::ITERATION_STATEMENT, 0), expression(std::move(expression)), statement(std::move(statement)) {}
IterationStatementAST::IterationStatementAST(nt<StatementAST> statement, nt<ExpressionAST> expression) : AST(
    AST::Kind::ITERATION_STATEMENT, 1), statement(std::move(statement)), expression(std::move(expression)) {}
IterationStatementAST::IterationStatementAST(nt<ExpressionAST> expression,
                                             nt<ExpressionAST> condition_expression,
                                             nt<ExpressionAST> step_expression,
                                             nt<StatementAST> statement)
    : AST(AST::Kind::ITERATION_STATEMENT, 2),
      expression(std::move(expression)),
      condition_expression(std::move(condition_expression)),
      step_expression(std::move(step_expression)),
      statement(std::move(statement)) {}
void IterationStatementAST::print(int indent) {
  AST::print(indent);
  ++indent;
  if (getProduction() == 0) {
    expression->print(indent);
    statement->print(indent);
  } else if (getProduction() == 1) {
    statement->print(indent);
    expression->print(indent);
  } else {
    expression->print(indent);
    condition_expression->print(indent);
    step_expression->print(indent);
    statement->print(indent);
  }
}
JumpStatementAST::JumpStatementAST(nt<IdentifierAST> id) : AST(AST::Kind::JUMP_STATEMENT, 0), id(std::move(id)) {}
JumpStatementAST::JumpStatementAST(bool is_continue) : AST(AST::Kind::JUMP_STATEMENT, is_continue ? 1 : 2) {}
JumpStatementAST::JumpStatementAST(nt<ExpressionAST> expression) : AST(AST::Kind::JUMP_STATEMENT, 3),
                                                                   expression(std::move(expression)) {}
void JumpStatementAST::print(int indent) {
  AST::print(indent);
  ++indent;
  if (getProduction() == 0) {
    id->print(indent);
  } else if (getProduction() == 1) {
    AST::printIndent(indent);
    std::cout << "continue\n";
  } else if (getProduction() == 2) {
    AST::printIndent(indent);
    std::cout << "break\n";
  } else {
    expression->print(indent);
  }
}
TypeQualifierAST::TypeQualifierAST(Terminal<TypeQuailifier> op) : AST(AST::Kind::TYPE_QUALIFIER), op(op) {}
void TypeQualifierAST::print(int indent) {
  AST::print(indent);
  op.print(++indent);
}
TypedefNameAST::TypedefNameAST(nt<IdentifierAST> identifier)
    : AST(AST::Kind::TYPEDEF_NAME), id(std::move(identifier)) {}
void TypedefNameAST::print(int indent) {
  AST::print(indent);
  id->print(++indent);
}
IdentifierAST::IdentifierAST(const Token &token)
    : AST(AST::Kind::IDENTIFIER), token(token) {}
void IdentifierAST::print(int indent) {
  AST::print(indent);
  AST::printIndent(++indent);
  std::cout << token.getValue() << std::endl;
}
StringAST::StringAST(std::string value) : AST(AST::Kind::STRING) {}
AST::AST(AST::Kind kind, int id) : kind(kind), productionId(id) {}
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
    case AST::Kind::PROTO_TYPE_SPECIFIER: return "PROTO_TYPE_SPECIFIER";
    default:return "unknown kind";
  }
}
void AST::print(int indent) {
  printIndent(indent);
  std::cout << toString() << std::endl;
}
void AST::printIndent(int indent) {
  for (int i = 0; i < indent; ++i) {
    std::cout << "\t";
  }
}
