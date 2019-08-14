#include <sema/ast.h>
#include <iostream>
#include <sema/symbol_tables.h>
#include "llvm/IR/Constants.h"
#include <llvm/IR/IRBuilder.h>
SemaException::SemaException(std::string error, const Token &token) : error(std::move(error)) {
  this->error.append("\n").append(token.getTokenInLine());
}
const char *SemaException::what() const noexcept {
  return error.c_str();
}
SemaException::SemaException(std::string error, const Token &start, const Token &end) : error(std::move(error)) {
  this->error.append("\n").append(Token::getTokenInLine(start, end));
}
SemaException::SemaException(std::string error, std::pair<const Token &, const Token &> range)
    : SemaException(std::move(error), range.first, range.second) {}
TranslationUnitAST::TranslationUnitAST(nts<ExternalDeclarationAST> external_declarations,
                                       SymbolTable &objectTable,
                                       SymbolTable &tagTable)
    : AST(AST::Kind::TRANSLATION_UNIT), external_declarations(std::move(external_declarations)),
      mObjectTable(objectTable), mTagTable(tagTable) {}
void TranslationUnitAST::print(int indent) {
  AST::print(indent);
  external_declarations.print(++indent);
}
void TranslationUnitAST::codegen() {
  sObjectTable = &mObjectTable;
  sTagTable = &mTagTable;
  for (const auto &ds : external_declarations) {
    ds->codegen();
  }
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
void ExternalDeclarationAST::codegen() {
  if (def->getKind() == AST::Kind::FUNCTION_DEFINITION) {
    static_cast<FunctionDefinitionAST *>(def.get())->codegen();
  } else {
    static_cast<DeclarationAST *>(def.get())->codegen();
  }
}
FunctionDefinitionAST::FunctionDefinitionAST(nt<DeclarationSpecifiersAST> declaration_spcifiers,
                                             nt<DeclaratorAST> declarator,
                                             nts<DeclarationAST> declarations,
                                             nt<CompoundStatementAST> compound_statement,
                                             SymbolTable &lableTable)
    : AST(AST::Kind::FUNCTION_DEFINITION),
      declaration_spcifiers(std::move(declaration_spcifiers)),
      declarator(std::move(declarator)),
      declarations(std::move(declarations)),
      compound_statement(std::move(compound_statement)),
      mLabelTable(lableTable) {
  if (auto *functionDeclarator = dynamic_cast<FunctionDeclaratorAST *>(this->declarator->direct_declarator.get())) {
    if (functionDeclarator->parameterList) {
      auto table = functionDeclarator->parameterList->mObjectTable;
      this->compound_statement->mObjectTable.setFather(&table);
    }
  }
}
void FunctionDefinitionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  declaration_spcifiers->print(indent);
  declarator->print(indent);
  declarations.print(indent);
  compound_statement->print(indent);
}
llvm::Value *FunctionDefinitionAST::codegen() {
  sLabelTable = &mLabelTable;
  for (const auto &specifier : declaration_spcifiers->storage_specifiers) {
    if (specifier.type != StorageSpecifier::kEXTERN && specifier.type != StorageSpecifier::kSTATIC) {
      throw SemaException("declaration specifiers shall be either extern or static", specifier.token);
    }
  }
  sLabelTable = nullptr;
  //TODO After adjustment, the parameters in a parameter type list in a function declarator that is part of a definition of that function shall not have incomplete type.
  return nullptr;
}
CompoundStatementAST::CompoundStatementAST(nts<DeclarationAST> declarations,
                                           nts<StatementAST> statements,
                                           SymbolTable &objectTable,
                                           SymbolTable &tagTable)
    : AST(AST::Kind::COMPOUND_STATEMENT),
      declarations(std::move(declarations)),
      statements(std::move(statements)), mObjectTable(objectTable), mTagTable(tagTable) {}
void CompoundStatementAST::print(int indent) {
  AST::print(indent);
  ++indent;
  declarations.print(indent);
  statements.print(indent);
}
llvm::Value *CompoundStatementAST::codegen() {
  SymbolScope s1(sObjectTable, &mObjectTable);
  SymbolScope s2(sTagTable, &mTagTable);
  //TODO create basic block and assign it to symbol table;
}
DeclarationAST::DeclarationAST(nt<DeclarationSpecifiersAST> declaration_specifiers,
                               InitDeclarators init_declarators,
                               SymbolTable &table)
    : AST(AST::Kind::DECLARATION),
      declaration_specifiers(std::move(declaration_specifiers)),
      init_declarators(std::move(init_declarators)) {
  for (auto &specifiers : this->declaration_specifiers->storage_specifiers) {
    if (specifiers.type == StorageSpecifier::kTYPEDEF) {
      for (const auto &pair : this->init_declarators) {
        const DeclaratorAST *ast = pair.first.get();
        table.insert(*ast->getIdentifier(), nullptr);
      }
      break;
    }
  }
}
void DeclarationAST::print(int indent) {
  AST::print(indent);
  ++indent;
  declaration_specifiers->print(indent);
  for (const auto &ds : init_declarators) {
    ds.first->print(indent);
    if (ds.second) ds.second->print(indent);
  }
}
void DeclarationAST::codegen() {
  const auto &pair = declaration_specifiers->codegen();
  const auto &storageSpecifier = pair.first;
  const auto &qualifiers = pair.second;
  for (auto &ast : init_declarators) {
    ast.first->codegen(storageSpecifier.type, qualifiers);
//  TODO  ast.second->codegen();
  }
  bool hasDeclarator = !init_declarators.empty();
  bool hasTag = false;
  bool hasEnumMembers = false;
  // TODO make this more readable
  for (auto &type_specifier : declaration_specifiers->type_specifiers->type_specifiers) {
    if (type_specifier->getProduction() == 9) {
      hasTag = static_cast<StructOrUnionSpecifierAST *> (type_specifier->specifier.get())->id != nullptr;
    } else if (type_specifier->getProduction() == 10) {
      auto enum_list = static_cast<EnumSpecifierAST *> (type_specifier->specifier.get())->enum_list.get();
      hasEnumMembers = enum_list && !enum_list->enumerators.empty();
    }
  }

  if (!hasDeclarator && !hasTag && !hasEnumMembers) {
    throw SemaException("A declaration shall declare at least a declarator, a tag, or the members of an enumeration",
                        *mLeftMost);
  }
  //TODO 6.7.3
}
DeclarationSpecifiersAST::DeclarationSpecifiersAST(ts<StorageSpecifier> storage_specifiers,
                                                   nt<TypeSpecifiersAST> type_specifiers,
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
  type_specifiers->print(indent);
}
bool DeclarationSpecifiersAST::empty() {
  return storage_specifiers.empty() && type_qualifiers.empty() && type_specifiers->empty();
}
std::pair<const Terminal<StorageSpecifier> &, QualifiedType> DeclarationSpecifiersAST::codegen() {
  if (storage_specifiers.size() > 1) {
    throw SemaException(
        "At most, one storage-class specifier may be given in the declaration specifiers in a declaration",
        storage_specifiers.back().token);
  }
  const auto &storageSpecifier = storage_specifiers.back();
  std::set<TypeQualifier> qualifiers;
  for (const auto &qualifier : type_qualifiers) {
    qualifiers.emplace(qualifier);
  }
  //TODO If an aggregate or union object is declared with a storage-class specifier other than typedef, the properties resulting from the storage-class specifier, except with respect to linkage, also apply to the members of the object, and so on recursively for any aggregate or union member objects.
  //TODO At least one type specifier shall be given in the declaration specifiers in each declaration,  and in the specifier-qualifier list in each struct declaration and type name.
  QualifiedType qualifiedType = type_specifiers->codegen();
  qualifiedType.addQualifiers(qualifiers);
  return std::make_pair<const Terminal<StorageSpecifier> &, QualifiedType>(storageSpecifier, std::move(qualifiedType));
}
DeclaratorAST::DeclaratorAST(nt<PointerAST> pointer,
                             nt<DirectDeclaratorAST> direct_declarator)
    : AST(AST::Kind::DECLARATOR), pointer(std::move(pointer)), direct_declarator(std::move(direct_declarator)) {}
void DeclaratorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  if (pointer) pointer->print(indent);
  if (direct_declarator) direct_declarator->print(indent);
}
const Token *DeclaratorAST::getIdentifier() const {
  return direct_declarator->getIdentifier();
}
ISymbol *DeclaratorAST::codegen(StorageSpecifier storageSpecifier, const QualifiedType &derivedType) {
  return direct_declarator->codegen(storageSpecifier, pointer->codegen(derivedType));
}
bool DeclaratorAST::isAbstract() const {
  return direct_declarator->isAbstract();
}
StorageClassSpecifierAST::StorageClassSpecifierAST(Terminal<StorageSpecifier> storage_speicifier)
    : AST(AST::Kind::STORAGE_CLASS_SPECIFIER), storage_speicifier(storage_speicifier) {}
void StorageClassSpecifierAST::print(int indent) {
  AST::print(indent);
  storage_speicifier.print(++indent);
}
void SpecifierQualifierAST::print(int indent) {
  AST::print(indent);
  ++indent;
  if (types) types->print(indent);
  qualifiers.print(indent);
}
SpecifierQualifierAST::SpecifierQualifierAST(nt<TypeSpecifiersAST> types, nts<TypeQualifierAST> qualifiers)
    : AST(AST::Kind::SPECIFIER_QUALIFIER), types(std::move(types)), qualifiers(std::move(qualifiers)) {

}
QualifiedType SpecifierQualifierAST::codegen() {
  QualifiedType qualifiedType = types->codegen();
  for (auto &qualifier : qualifiers) {
    qualifiedType.addQualifier(qualifier->op.type);
  }
  return qualifiedType;
}
ProtoTypeSpecifierAST::ProtoTypeSpecifierAST(Terminal<ProtoTypeSpecifierOp> specifier)
    : AST(AST::Kind::PROTO_TYPE_SPECIFIER), Terminal<ProtoTypeSpecifierOp>(specifier), specifier(specifier) {}
void ProtoTypeSpecifierAST::print(int indent) {
  AST::print(indent);
  specifier.print(++indent);
}
TypeSpecifierAST::TypeSpecifierAST(Terminal<ProtoTypeSpecifierOp> specifier)
    : AST(AST::Kind::TYPE_SPECIFIER, static_cast<int>(specifier.type)),
      specifier(std::make_unique<ProtoTypeSpecifierAST>(specifier)) {}
TypeSpecifierAST::TypeSpecifierAST(nt<StructOrUnionSpecifierAST> specifier)
    : AST(AST::Kind::TYPE_SPECIFIER, 9), specifier(std::move(specifier)) {}
TypeSpecifierAST::TypeSpecifierAST(nt<EnumSpecifierAST> specifier)
    : AST(AST::Kind::TYPE_SPECIFIER, 10), specifier(std::move(specifier)) {}
TypeSpecifierAST::TypeSpecifierAST(nt<TypedefNameAST> specifier)
    : AST(AST::Kind::TYPE_SPECIFIER, 11), specifier(std::move(specifier)) {}
void TypeSpecifierAST::print(int indent) {
  AST::print(indent);
  specifier->print(++indent);
}
StructOrUnionSpecifierAST::StructOrUnionSpecifierAST(StructOrUnion type,
                                                     nt<IdentifierAST> id,
                                                     nts<StructDeclarationAST> declarations)
    : AST(AST::Kind::STRUCT_OR_UNION_SPECIFIER),
      bStruct(type),
      id(std::move(id)),
      declarations(std::move(declarations)) {}
void StructOrUnionSpecifierAST::print(int indent) {
  AST::print(indent);
  ++indent;
  AST::printIndent(indent);
  bStruct == StructOrUnion::kSTRUCT ? std::cout << "STRUCT" << std::endl : std::cout << "UNION" << std::endl;
  if (id != nullptr) id->print(indent);
  declarations.print(indent);
}
CompoundType *StructOrUnionSpecifierAST::codegen() {
  CompoundType *tagType;
  //TODO A struct-declaration that does not declare an anonymous structure or anonymous union shall contain a struct-declarator-list.
  if (bStruct == StructOrUnion::kSTRUCT) {
    if (id) {
      const auto &token = id->token;
      mSymbol = std::make_unique<TagSymbol>(std::make_unique<StructType>(token.getValue(), sModule), &token);
      sTagTable->insert(token, mSymbol.get());
    } else {
      mSymbol = std::make_unique<TagSymbol>(std::make_unique<StructType>(sModule), nullptr);
      sTagTable->insert(mSymbol.get());
    }
  } else {
    if (id) {
      const auto &token = id->token;
      mSymbol = std::make_unique<TagSymbol>(std::make_unique<UnionType>(token.getValue(), sModule), &token);
      sTagTable->insert(token, mSymbol.get());
    } else {
      mSymbol = std::make_unique<TagSymbol>(std::make_unique<UnionType>(sModule), nullptr);
      sTagTable->insert(mSymbol.get());
    }
  }
  tagType = mSymbol->getTagType();
  SymbolTable table(ScopeKind::TAG);
  int index = 0;
  for (const auto &declaration :declarations) {
    auto d = declaration->codegen();
    for (auto *symbol : d) {
      if (const Token *token = symbol->getToken()) {
        symbol->setIndex(index);
        ++index;
        table.insert(*token, symbol);
      } else {
        throw std::runtime_error("WTF: member has to got a name");
      }
    }
  }
  tagType->setBody(std::move(table), sModule);
  return tagType;
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
  if (id) id->print(indent);
  if (enum_list) enum_list->print(indent);
}
EnumerationType *EnumSpecifierAST::codegen() {
  const Token *token = id ? &id->token : nullptr;
  mSymbol = std::make_unique<TagSymbol>(std::make_unique<EnumerationType>(), token);
  SymbolTable table(ScopeKind::TAG);
  for (auto *symbol : enum_list->codegen()) {
    table.insert(*symbol->getToken(), symbol);
  }
  mSymbol->getTagType()->setBody(std::move(table), sModule);
  return static_cast<EnumerationType *> (mSymbol->getTagType());
}
StructDeclarationAST::StructDeclarationAST(nt<SpecifierQualifierAST> specifier_qualifier,
                                           nt<StructDeclaratorListAST> struct_declarator_list)
    : AST(AST::Kind::STRUCT_DECLARATION),
      specifier_qualifier(std::move(specifier_qualifier)),
      struct_declarator_list(std::move(struct_declarator_list)) {}
void StructDeclarationAST::print(int indent) {
  AST::print(indent);
  ++indent;
  specifier_qualifier->print(indent);
  struct_declarator_list->print(indent);
}
std::vector<ObjectSymbol *> StructDeclarationAST::codegen() {
  auto qualifiedType = specifier_qualifier->codegen();
  std::vector<ObjectSymbol *> symbols;
  for (auto &ast : struct_declarator_list->struct_declarators) {
    ISymbol *symbol = ast->codegen(qualifiedType);
    if (auto *obj = dynamic_cast<ObjectSymbol *>(symbol)) {
      if (const auto *type = dynamic_cast<const ObjectType *>(obj->getQualifiedType().getType())) {
        if (type->complete()) {
          symbols.push_back(obj);
        } else {
          //TODO flexible array member
          throw SemaException("struct or union should have completed member", ast->involvedTokens());
        }
      }
    } else {
      throw SemaException("struct or union should not contain a member with function type", ast->involvedTokens());
    }
  }
  return symbols;
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
ISymbol *StructDeclaratorAST::codegen(const QualifiedType &derivedType) {
  return declarator->codegen(StorageSpecifier::kNone, derivedType);
  //TODO bit fields
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
const QualifiedType PointerAST::codegen(const QualifiedType &derivedType) {
  std::set<TypeQualifier> qualifiers;
  for (auto &qualifier : type_qualifiers) {
    qualifiers.insert(qualifier->codegen());
  }
  if (pointer) {
    mPointerType = std::make_unique<PointerType>(pointer->codegen(derivedType));
  } else {
    mPointerType = std::make_unique<PointerType>(derivedType);
  }
  return QualifiedType(mPointerType.get(), std::move(qualifiers));
}
ParameterTypeListAST::ParameterTypeListAST(nt<ParameterListAST>
                                           parameter_list, bool
                                           hasMultiple)
    : AST(AST::Kind::PARAMETER_TYPE_LIST, hasMultiple ? 1 : 0), parameter_list(std::move(parameter_list)) {}
void ParameterTypeListAST::print(int indent) {
  AST::print(indent);
  parameter_list->print(++indent);
}
ConditionalExpressionAST::ConditionalExpressionAST(nt<LogicalOrExpressionAST>
                                                   logical_or_expression)
    : AST(AST::Kind::CONDITIONAL_EXPRESSION, 0),
      logical_or_expression(std::move(logical_or_expression)) {}
ConditionalExpressionAST::ConditionalExpressionAST(nt<LogicalOrExpressionAST>
                                                   logical_or_expression,
                                                   nt<ExpressionAST>
                                                   expression,
                                                   nt<ConditionalExpressionAST>
                                                   conditional_expression)
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
LogicalOrExpressionAST::LogicalOrExpressionAST(nt<LogicalOrExpressionAST>
                                               left,
                                               Terminal<InfixOp>
                                               op,
                                               nt<LogicalOrExpressionAST>
                                               right)
    : AST(AST::Kind::LOGICAL_OR_EXPRESSION, 0),
      left(std::move(left)),
      op(std::make_unique<Terminal<InfixOp >>(op)),
      right(std::move(right)) {}
LogicalOrExpressionAST::LogicalOrExpressionAST(nt<CastExpressionAST>
                                               leaf)
    : AST(AST::Kind::LOGICAL_OR_EXPRESSION, 1), left(std::move(leaf)), op(nullptr), right(nullptr) {}
void LogicalOrExpressionAST::print(int indent) {
  AST::print(indent);
  if (getProduction() == 0) {
    left->print(++indent);
    op->print(indent);
    right->print(++indent);
  } else {
    left->print(indent);
  }
}
ExpressionAST::ExpressionAST(nts<AssignmentExpressionAST>
                             assignment_expression)
    : AST(AST::Kind::EXPRESSION, 0),
      assignment_expression(std::move(
          assignment_expression)) {}
void ExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  assignment_expression.print(indent);
}
CastExpressionAST::CastExpressionAST(nt<UnaryExpressionAST>
                                     unary_expression)
    : AST(AST::Kind::CAST_EXPRESSION, 0),
      unary_expression(std::move(
          unary_expression)),
      type_name(nullptr),
      cast_expression(nullptr) {}
CastExpressionAST::CastExpressionAST(nt<TypeNameAST>
                                     type_name, nt<CastExpressionAST>
                                     cast_expression)
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

TypeNameAST::TypeNameAST(nt<SpecifierQualifierAST>
                         specifier, nt<DeclaratorAST>
                         declarator)
    : AST(AST::Kind::TYPE_NAME), specifiers(std::move(specifier)), declarator(std::move(declarator)) {}
void TypeNameAST::print(int indent) {
  AST::print(indent);
  ++indent;
  specifiers->print(indent);
  if (declarator) declarator->print(indent);
}
AssignmentExpressionAST::AssignmentExpressionAST(nt<ConditionalExpressionAST>
                                                 conditional_expression)
    : AST(AST::Kind::ASSIGNMENT_EXPRESSION, 0), conditional_expression(std::move(conditional_expression)) {}
AssignmentExpressionAST::AssignmentExpressionAST(nt<ConditionalExpressionAST>
                                                 conditional_expression,
                                                 Terminal<AssignmentOp>
                                                 op,
                                                 nt<AssignmentExpressionAST>
                                                 assignment_expression)
    : AST(AST::Kind::ASSIGNMENT_EXPRESSION, 1), conditional_expression(std::move(conditional_expression)),
      op(std::make_unique<Terminal<AssignmentOp >>(op)), assignment_expression(std::move(assignment_expression)) {}
void AssignmentExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  conditional_expression->print(indent);
  if (getProduction() == 1) {
    op->print(indent);
    assignment_expression->print(indent);
  }
}
CharacterConstantAST::CharacterConstantAST(
    const Token &token)
    : AST(AST::Kind::CHARACTER_CONSTANT), mToken(token), c(token.getValue().c_str()[0]) {}
FloatingConstantAST::FloatingConstantAST(
    const Token &token)
    : AST(AST::Kind::FLOATING_CONSTANT), mToken(token) {
  std::string::size_type sz;
  try {
    value = std::stof(this->mToken.getValue(), &sz);
    const std::string &sub = this->mToken.getValue().substr(sz);
    if (sub.empty()) {
      suffix = Suffix::None;
    } else if (sub.size() == 1) {
      if (sub[0] == 'f' || sub[0] == 'F') {
        suffix = Suffix::F;
      } else if (sub[0] == 'l' || sub[0] == 'L') {
        suffix = Suffix::L;
      } else {
        throw SemaException("cannot parse integer constant", token);
      }
    } else {
      throw SemaException("cannot parse integer constant", token);
    }
  } catch (const std::invalid_argument &) {
    throw SemaException("cannot parse integer constant", token);
  }
}
void FloatingConstantAST::print(int indent) {
  AST::print(indent);
  AST::printIndent(++indent);
  std::cout << mToken.getValue() << std::endl;
}
EnumerationConstantAST::EnumerationConstantAST(nt<IdentifierAST>
                                               id)
    : AST(AST::Kind::ENUMERATION_CONSTANT), id(std::move(id)) {}
ParameterListAST::ParameterListAST(nts<ParameterDeclarationAST>
                                   parameter_declaration, SymbolTable &table)
    : AST(AST::Kind::PARAMETER_LIST), parameter_declaration(std::move(parameter_declaration)), mObjectTable(table) {}
void ParameterListAST::print(int indent) {
  AST::print(indent);
  parameter_declaration.print(++indent);
}
std::vector<QualifiedType> ParameterListAST::codegen() {
  SymbolScope s(sObjectTable, &mObjectTable);
  std::vector<QualifiedType> parameters;
  for (const auto &ast : parameter_declaration) {
    ISymbol *symbol = ast->codegen();
    if (auto *obj = dynamic_cast<ObjectSymbol *>(symbol)) {
      if (obj->getQualifiedType().getType() == &VoidType::sVoidType && parameter_declaration.size() > 1) {
        throw SemaException("void should be the first and only parameter", involvedTokens());
      }
      if (const Token *token = symbol->getToken()) {
        sObjectTable->insert(*token, symbol);
      } else {
        sObjectTable->insert(symbol);
      }
      parameters.push_back(obj->getQualifiedType());
    } else {
      throw std::runtime_error("WTF: how could parameter list have symbols other than object symbol");
    }
  }
  return parameters;
}
ParameterDeclarationAST::ParameterDeclarationAST(nt<DeclarationSpecifiersAST>
                                                 declaration_specifiers,
                                                 nt<DeclaratorAST>
                                                 declarator)
    : AST(AST::Kind::PARAMETER_DECLARATION), declaration_specifiers(std::move(declaration_specifiers)),
      declarator(std::move(declarator)) {}
void ParameterDeclarationAST::print(int indent) {
  AST::print(indent);
  ++indent;
  declaration_specifiers->print(indent);
  if (declarator) declarator->print(indent);
}
ISymbol *ParameterDeclarationAST::codegen() {
  if (!declarator) {
    declarator = std::make_unique<DeclaratorAST>(nullptr, std::make_unique<SimpleDirectDeclaratorAST>(nullptr));
  }
  const auto &pair = declaration_specifiers->codegen();
  const StorageSpecifier &storageSpecifier = pair.first.type;
  if (storageSpecifier != StorageSpecifier::kREGISTER && storageSpecifier != StorageSpecifier::kNone) {
    throw SemaException("The only storage-class specifier that shall occur in a parameter declaration is register.",
                        involvedTokens());
  }
  const QualifiedType &qualifiedType = pair.second;
  ISymbol *symbol = declarator->codegen(storageSpecifier, qualifiedType);
  if (const auto *obj = dynamic_cast<ObjectSymbol *>(symbol)) {
    if (const auto *type = dynamic_cast<const ArrayType *>(obj->getQualifiedType().getType())) {
      return declarator->codegen(storageSpecifier, QualifiedType((const PointerType *) type, {}));
    }
  } else if (const auto *func = dynamic_cast<FunctionSymbol *>(symbol)) {
    return declarator->codegen(storageSpecifier, QualifiedType((const PointerType *) (func->getType()), {}));
  } else {
    return symbol;
  }

}
EnumeratorListAST::EnumeratorListAST(nts<EnumeratorAST>
                                     enumerator)
    : AST(AST::Kind::ENUMERATOR_LIST), enumerators(std::move(enumerator)) {}
void EnumeratorListAST::print(int indent) {
  AST::print(indent);
  enumerators.print(++indent);
}
std::vector<EnumConstSymbol *> EnumeratorListAST::codegen() {
  std::vector<EnumConstSymbol *> members;
  int64_t index = 0;
  for (auto &enumerator : enumerators) {
    auto *symbol = enumerator->codegen(nullptr, index);
    index = symbol->getIndex();
    members.push_back(symbol);
    ++index;
  }
  return members;
}
EnumeratorAST::EnumeratorAST(nt<IdentifierAST> id) : AST(AST::Kind::ENUMERATOR, 0), id(std::move(id)) {}
EnumeratorAST::EnumeratorAST(nt<IdentifierAST> id,
                             nt<ConstantExpressionAST>
                             constant_expression)
    : AST(AST::Kind::ENUMERATOR, 1), id(std::move(id)), constant_expression(std::move(constant_expression)) {}
void EnumeratorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  id->print(indent);
  if (getProduction() == 1) {
    constant_expression->print(indent);
  }
}
EnumConstSymbol *EnumeratorAST::codegen(const EnumerationType *enumerationType, int64_t index) {
  if (constant_expression) {
    auto value = constant_expression->codegen();
    if (!dynamic_cast<const IntegerType *>(value.qualifiedType.getType())) {
      throw SemaException("the expression shall have an integer type", constant_expression->involvedTokens());
    } else {
      auto *constInt = llvm::dyn_cast<llvm::ConstantInt>(value.value);
      if (!constInt) throw std::runtime_error("WTF: not a llvm::ConstantInt, weird");
      index = constInt->getSExtValue();
    }
  }
  llvm::ConstantInt *value = llvm::ConstantInt::get(sModule.getContext(), llvm::APInt(32, index, true));
  mSymbol = std::make_unique<EnumConstSymbol>(enumerationType, &id->token, value);
  return mSymbol.get();
}
InitializerAST::InitializerAST(nt<AssignmentExpressionAST>
                               assignment_expression)
    : AST(AST::Kind::INITIALIZER, 0), ast(std::move(assignment_expression)) {}
InitializerAST::InitializerAST(nt<InitializerListAST>
                               initializer_list)
    : AST(AST::Kind::INITIALIZER, 1), ast(std::move(initializer_list)) {}
void InitializerAST::print(int indent) {
  AST::print(indent);
  ast->print(++indent);
}
InitializerListAST::InitializerListAST(nts<InitializerAST>
                                       initializer)
    : AST(AST::Kind::INITIALIZER_LIST), initializer(std::move(initializer)) {}
void InitializerListAST::print(int indent) {
  AST::print(indent);
  initializer.print(++indent);
}
StatementAST::StatementAST(nt<LabeledStatementAST>
                           ast) : AST(AST::Kind::STATEMENT, 0), ast(std::move(ast)) {}
StatementAST::StatementAST(nt<ExpressionStatementAST>
                           ast) : AST(AST::Kind::STATEMENT, 1), ast(std::move(ast)) {}
StatementAST::StatementAST(nt<CompoundStatementAST>
                           ast) : AST(AST::Kind::STATEMENT, 2), ast(std::move(ast)) {}
StatementAST::StatementAST(nt<SelectionStatementAST>
                           ast) : AST(AST::Kind::STATEMENT, 3), ast(std::move(ast)) {}
StatementAST::StatementAST(nt<IterationStatementAST>
                           ast) : AST(AST::Kind::STATEMENT, 4), ast(std::move(ast)) {}
StatementAST::StatementAST(nt<JumpStatementAST>
                           ast) : AST(AST::Kind::STATEMENT, 5), ast(std::move(ast)) {}
void StatementAST::print(int indent) {
  AST::print(indent);
  ast->print(++indent);
}
LabeledStatementAST::LabeledStatementAST(nt<IdentifierAST>
                                         id, nt<StatementAST>
                                         statement)
    : AST(AST::Kind::LABELED_STATEMENT, 0), id(std::move(id)), statement(std::move(statement)) {}
LabeledStatementAST::LabeledStatementAST(nt<ConstantExpressionAST>
                                         constant_expression, nt<StatementAST>
                                         statement)
    : AST(AST::Kind::LABELED_STATEMENT, 1),
      constant_expression(std::move(constant_expression)),
      statement(std::move(statement)) {}
LabeledStatementAST::LabeledStatementAST(nt<StatementAST>
                                         statement)
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
ExpressionStatementAST::ExpressionStatementAST(nt<ExpressionAST>
                                               expression)
    : AST(AST::Kind::EXPRESSION_STATEMENT), expression(std::move(expression)) {}
void ExpressionStatementAST::print(int indent) {
  AST::print(indent);
  expression->print(++indent);
}
SelectionStatementAST::SelectionStatementAST(nt<ExpressionAST>
                                             expression,
                                             nt<StatementAST>
                                             statement,
                                             bool
                                             is_if)
    : AST(AST::Kind::SELECTION_STATEMENT, is_if ? 0 : 2),
      expression(std::move(expression)),
      statement(std::move(statement)) {}
SelectionStatementAST::SelectionStatementAST(nt<ExpressionAST>
                                             expression,
                                             nt<StatementAST>
                                             statement,
                                             nt<StatementAST>
                                             else_statement)
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
IterationStatementAST::IterationStatementAST(nt<ExpressionAST>
                                             expression, nt<StatementAST>
                                             statement)
    : AST(AST::Kind::ITERATION_STATEMENT, 0),
      expression(std::move(expression)),
      statement(std::move(statement)) {}
IterationStatementAST::IterationStatementAST(nt<StatementAST>
                                             statement, nt<ExpressionAST>
                                             expression)
    : AST(AST::Kind::ITERATION_STATEMENT, 1),
      statement(std::move(statement)),
      expression(std::move(expression)) {}
IterationStatementAST::IterationStatementAST(nt<ExpressionAST>
                                             expression,
                                             nt<ExpressionAST>
                                             condition_expression,
                                             nt<ExpressionAST>
                                             step_expression,
                                             nt<StatementAST>
                                             statement)
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
JumpStatementAST::JumpStatementAST(nt<IdentifierAST>
                                   id) : AST(AST::Kind::JUMP_STATEMENT, 0), id(std::move(id)) {}
JumpStatementAST::JumpStatementAST(bool
                                   is_continue) : AST(AST::Kind::JUMP_STATEMENT, is_continue ? 1 : 2) {}
JumpStatementAST::JumpStatementAST(nt<ExpressionAST>
                                   expression) : AST(AST::Kind::JUMP_STATEMENT, 3),
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
TypeQualifierAST::TypeQualifierAST(Terminal<TypeQualifier>
                                   op) : AST(AST::Kind::TYPE_QUALIFIER), op(op) {}
void TypeQualifierAST::print(int indent) {
  AST::print(indent);
  op.print(++indent);
}
TypeQualifier TypeQualifierAST::codegen() {
  return op.type;
}
TypedefNameAST::TypedefNameAST(nt<IdentifierAST>
                               identifier)
    : AST(AST::Kind::TYPEDEF_NAME), id(std::move(identifier)) {}
void TypedefNameAST::print(int indent) {
  AST::print(indent);
  id->print(++indent);
}
QualifiedType TypedefNameAST::codegen() {
  ISymbol *symbol = sObjectTable->lookup(id->token);
  if (!symbol) {
    throw SemaException(std::string("Type ") + id->token.getValue() + "not declared", id->token);
  } else {
    auto *typedefSymbol = dynamic_cast<TypedefSymbol *>(symbol);
    if (typedefSymbol) {
      return typedefSymbol->getType();
    } else {
      throw std::runtime_error("WTF: not a typedef name");
    }
  }
}
IdentifierAST::IdentifierAST(
    const Token &token)
    : AST(AST::Kind::IDENTIFIER), token(token) {}
void IdentifierAST::print(int indent) {
  AST::print(indent);
  AST::printIndent(++indent);
  std::cout << token.getValue() << std::endl;
}
StringAST::StringAST(
    const Token &token) : AST(AST::Kind::STRING), mToken(token) {
  mType = std::make_unique<ArrayType>(&IntegerType::sCharType, mToken.getValue().size());
}
llvm::LLVMContext AST::sContext;
llvm::Module AST::sModule("top", sContext);
llvm::IRBuilder<> AST::sBuilder(sContext);
AST::AST(AST::Kind
         kind, int
         id) : kind(kind), productionId(id) {}
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
    case AST::Kind::ARGUMENT_EXPRESSION_LIST: return "ARGUMENT_EXPRESSION_LIST";
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
std::pair<const Token &, const Token &> AST::involvedTokens() {
  return {*mLeftMost, *mRightMost};
}
IntegerConstantAST::IntegerConstantAST(
    const Token &token)
    : AST(AST::Kind::INTEGER_CONSTANT), mToken(token) {
  std::string::size_type sz;
  try {
    this->value = std::stoull(this->mToken.getValue(), &sz, 0);
    const std::string sub = token.getValue().substr(sz);
    if (sub.empty()) {
      this->suffix = Suffix::None;
    } else {
      auto it = sub.begin();
      if (*it == 'u' || *it == 'U') {
        ++it;
        if (it == sub.end()) {
          suffix = Suffix::U;
        } else if (*it == 'l' || *it == 'L') {
          ++it;
          if (it == sub.end()) {
            suffix = Suffix::UL;
          } else {
            if ((*it == 'l' || *it == 'L') && *it == *--it) {
              ++it;
              if (it == sub.end()) {
                suffix = Suffix::ULL;
              } else {
                throw SemaException("cannot parse integer constant", token);
              }
            } else {
              throw SemaException("cannot parse integer constant", token);
            }
          }
        }
      } else if (*it == 'l' || *it == 'L') {
        ++it;
        if (it == sub.end()) {
          suffix = Suffix::L;
        } else if ((*it == 'l' || *it == 'L') && *it == *--it) {
          ++it;
          if (it == sub.end()) {
            suffix = Suffix::LL;
          } else {
            if (*it == 'u' || *it == 'U') {
              ++it;
              if (it == sub.end()) {
                suffix = Suffix::ULL;
              } else {
                throw SemaException("cannot parse integer constant", token);
              }
            } else {
              throw SemaException("cannot parse integer constant", token);
            }
          }
        }
      } else {
        throw SemaException("cannot parse integer constant", token);
      }
    }
  } catch (const std::invalid_argument &) {
    throw SemaException("cannot parse integer constant", token);
  }
}
void IntegerConstantAST::print(int indent) {
  AST::print(indent);
  printIndent(++indent);
  std::cout << mToken.getValue() << std::endl;
}
void ArgumentExpressionList::print(int indent) {
  AST::print(indent);
  ++indent;
  for (const auto &argument : argumentsList) {
    argument->print(indent);
  }
}
ArgumentExpressionList::ArgumentExpressionList(nts<AssignmentExpressionAST>
                                               argumentList)
    : AST(AST::Kind::ARGUMENT_EXPRESSION_LIST), argumentsList(std::move(argumentList)) {}
std::vector<IExpression::Value> ArgumentExpressionList::codegen() {
  std::vector<IExpression::Value> arguments;
  for (const auto &argument : argumentsList) {
    argument->codegen();
  }
}
TypeSpecifiersAST::TypeSpecifiersAST(CombinationKind
                                     kind, nts<TypeSpecifierAST>
                                     specifiers)
    : AST(AST::Kind::TYPE_SPECIFIERS), combination_kind(kind), type_specifiers(std::move(specifiers)) {

}
void TypeSpecifiersAST::print(int indent) {
  AST::print(indent);
  ++indent;
  for (auto &ast: type_specifiers) {
    ast->print(indent);
  }
}
bool TypeSpecifiersAST::empty() {
  return type_specifiers.empty();
}
QualifiedType TypeSpecifiersAST::codegen() {
  switch (combination_kind) {
    case CombinationKind::kUnknown:type = nullptr;
      break;
    case CombinationKind::kVoid:type = &VoidType::sVoidType;
      break;
    case CombinationKind::kUnsignedChar:
    case CombinationKind::kChar:type = &IntegerType::sUnsignedCharType;
      break;
    case CombinationKind::kSignedChar:type = &IntegerType::sCharType;
      break;
    case CombinationKind::kShort:
    case CombinationKind::kShortInt:
    case CombinationKind::kSignedShort:
    case CombinationKind::kSignedShortInt:type = &IntegerType::sShortIntType;
      break;
    case CombinationKind::kUnsignedShort:
    case CombinationKind::kUnsignedShortInt:type = &IntegerType::sUnsignedShortIntType;
      break;
    case CombinationKind::kInt:
    case CombinationKind::kSigned:
    case CombinationKind::kSignedInt:type = &IntegerType::sIntType;
      break;
    case CombinationKind::kUnsigned:
    case CombinationKind::kUnsignedInt:type = &IntegerType::sUnsignedIntType;
      break;
    case CombinationKind::kLong:
    case CombinationKind::kSignedLong:
    case CombinationKind::kSignedLongInt:type = &IntegerType::sLongIntType;
      break;
    case CombinationKind::kUnsignedLong:
    case CombinationKind::kUnsignedLongInt:type = &IntegerType::sUnsignedLongIntType;
      break;
    case CombinationKind::kLongLong:
    case CombinationKind::kSignedLongLong:
    case CombinationKind::kLongLongInt:
    case CombinationKind::kSignedLongLongInt:type = &IntegerType::sLongLongIntType;
      break;
    case CombinationKind::kUnsignedLongLong:
    case CombinationKind::kUnsignedLongLongInt:type = &IntegerType::sUnsignedLongLongIntType;
      break;
    case CombinationKind::kFloat:type = &FloatingType::sFloatType;
      break;
    case CombinationKind::kDouble:type = &FloatingType::sDoubleType;
      break;
    case CombinationKind::kLongDouble:type = &FloatingType::sLongDoubleType;
      break;
    case CombinationKind::kStruct:
    case CombinationKind::kUnion: {
      auto *structAST =
          static_cast<StructOrUnionSpecifierAST *>(type_specifiers.back()->specifier.get());
      type = structAST->codegen();
      break;
    }
    case CombinationKind::kEnum: {
      auto *enumAST = static_cast<EnumSpecifierAST *>(type_specifiers.back()->specifier.get());
      type = enumAST->codegen();
      break;
    }
    case CombinationKind::kTypeName:
      auto *typeNameAST = static_cast<TypedefNameAST *>(type_specifiers.back()->specifier.get());
      return typeNameAST->codegen();

  }
  return QualifiedType(type, {});
}
DirectDeclaratorAST::DirectDeclaratorAST() : AST(Kind::DIRECT_DECLARATOR) {}
SimpleDirectDeclaratorAST::SimpleDirectDeclaratorAST(nt<IdentifierAST> identifier)
    : identifier(std::move(identifier)) {}
void SimpleDirectDeclaratorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  identifier->print(indent);
}
const Token *SimpleDirectDeclaratorAST::getIdentifier() {
  return &identifier->token;
}
ISymbol *SimpleDirectDeclaratorAST::codegen(StorageSpecifier storageSpecifier, const QualifiedType &derivedType) {
  Linkage linkage = Linkage::kNone;
  llvm::Value *value = nullptr;

  ISymbol *priorDeclartion = sObjectTable->lookup(identifier->token);
  if (!priorDeclartion) {
    throw SemaException(identifier->token.getValue() + " is not declared", identifier->token);
  }

  switch (storageSpecifier) {
    case StorageSpecifier::kTYPEDEF: {
      if (!identifier) {
        throw SemaException("typedef must have an identifier in declarator", involvedTokens());
      }
      mSymbol = std::make_unique<TypedefSymbol>(derivedType, identifier->token);
      sObjectTable->insert(identifier->token, mSymbol.get());
      return mSymbol.get();
    }
    case StorageSpecifier::kREGISTER:break;
    case StorageSpecifier::kAUTO:break;
    case StorageSpecifier::kSTATIC: {
      if (sObjectTable->getScopeKind() == ScopeKind::FUNCTION_PROTOTYPE) {
        linkage = Linkage::kNone;
      } else if (sObjectTable->getScopeKind() == ScopeKind::FILE) {
        linkage = Linkage::kInternal;
      } else if (sObjectTable->getScopeKind() == ScopeKind::BLOCK
          && dynamic_cast<const FunctionType *>(derivedType.getType())) {
        throw SemaException(
            "The declaration of an identifier for a function that has block scope shall have no explicit storage-class specifier other than extern.\n",
            involvedTokens());
      }
      break;
    }
    case StorageSpecifier::kNone: {
      if (sObjectTable->getScopeKind() == ScopeKind::FILE) {
        linkage = Linkage::kExternal;
        break;
      } else if (dynamic_cast<const FunctionType *>(derivedType.getType())) {
        // will fall into extern case
      } else {
        break;
      }
    }
    case StorageSpecifier::kEXTERN: {
      if (sObjectTable->getScopeKind() == ScopeKind::FUNCTION_PROTOTYPE) {
        linkage = Linkage::kNone;
      }
      if (priorDeclartion) {
        auto *priorObject = dynamic_cast<ObjectSymbol *>(priorDeclartion);
        if (!priorObject
            || priorObject->getQualifiedType() != derivedType) {
          throw SemaException(std::string("redefine ") + identifier->token.getValue(), involvedTokens());
        }

        if (priorDeclartion->getLinkage() != Linkage::kNone) {
          // since there's a prior declaration, do nothing
          return nullptr;
        } else {
          linkage = Linkage::kExternal;
          break;
        }
      } else {
        linkage = Linkage::kExternal;
        break;
      }
    }
  }
  if (!derivedType.getType()->complete()) {
    throw SemaException("cannot initiate incomplete type: ", involvedTokens());
  }
  if (const auto *objectType = dynamic_cast<const ObjectType *>(derivedType.getType())) {
    switch (sObjectTable->getScopeKind()) {
      case ScopeKind::FILE: {
        sModule.getOrInsertGlobal(identifier->token.getValue(), objectType->getLLVMType(sModule));
        llvm::GlobalVariable *gVar = sModule.getNamedGlobal(identifier->token.getValue());
        if (linkage != Linkage::kNone) {
          gVar->setLinkage(linkage == Linkage::kExternal ? llvm::GlobalValue::LinkageTypes::ExternalLinkage
                                                         : llvm::GlobalVariable::LinkageTypes::InternalLinkage);
        }
        if (derivedType.getQualifiers().find(TypeQualifier::kCONST) != derivedType.getQualifiers().end()) {
          gVar->setConstant(true);
        }
        value = gVar;
        break;
      }
      case ScopeKind::BLOCK: {
        value = sBuilder.CreateAlloca(objectType->getLLVMType(sModule), nullptr, identifier->token.getValue());
        break;
      }
      case ScopeKind::LABEL:throw std::runtime_error("WTF: how could a object type declared in a tag symbol");
      case ScopeKind::FUNCTION_PROTOTYPE:
      case ScopeKind::TAG:
        //do not create value
        break;
    }
  } else if (const auto *functionType = dynamic_cast<const FunctionType *>(derivedType.getType())) {
    switch (sObjectTable->getScopeKind()) {
      case ScopeKind::FILE:
      case ScopeKind::BLOCK: {
        llvm::Function::Create(functionType->getLLVMType(sModule),
                               llvm::Function::ExternalLinkage,
                               identifier->token.getValue(),
                               &sModule);
      }
      case ScopeKind::LABEL:throw std::runtime_error("WTF: how could a function type declared in a label/proto symbol");
      case ScopeKind::FUNCTION_PROTOTYPE:break;
      case ScopeKind::TAG:throw SemaException("cannot declare funciton in a tag", involvedTokens());
    }
  }
  mSymbol->setLinkage(linkage);
  sObjectTable->insert(identifier->token, mSymbol.get());

  const Token *token = identifier ? &identifier->token : nullptr;
  mSymbol = std::make_unique<ObjectSymbol>(derivedType, value, token);
  return mSymbol.get();
}
bool SimpleDirectDeclaratorAST::isAbstract() const {
  return identifier == nullptr;
}
ParenthesedDirectDeclaratorAST::ParenthesedDirectDeclaratorAST(nt<DeclaratorAST> declarator)
    : declarator(std::move(declarator)) {}
void ParenthesedDirectDeclaratorAST::print(int indent) {
  AST::print(indent);
  declarator->print(indent);
}
const Token *ParenthesedDirectDeclaratorAST::getIdentifier() {
  return declarator->getIdentifier();
}
bool ParenthesedDirectDeclaratorAST::isAbstract() const {
  return declarator->isAbstract();
}
ISymbol *ParenthesedDirectDeclaratorAST::codegen(StorageSpecifier storageSpecifier, const QualifiedType &derivedType) {
  return declarator->codegen(storageSpecifier, derivedType);
}
ArrayDeclaratorAST::ArrayDeclaratorAST(nt<DirectDeclaratorAST> directDeclarator,
                                       nt<ConstantExpressionAST> constantExpression)
    : directDeclarator(std::move(directDeclarator)), constantExpression(std::move(constantExpression)) {}
void ArrayDeclaratorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  directDeclarator->print(indent);
  if (constantExpression) constantExpression->print(indent);
}
const Token *ArrayDeclaratorAST::getIdentifier() {
  return directDeclarator->getIdentifier();
}
ISymbol *ArrayDeclaratorAST::codegen(StorageSpecifier storageSpecifier, const QualifiedType &derivedType) {
  // 6.7.6.2 Array declarators
  // BNF wrote by Brian W. Kernighan and Dennis M. Ritchie,Prentice Hall, 1988 do not support variable length array
  int64_t size = 0;
  if (constantExpression) {
    auto value = constantExpression->codegen();
    if (!dynamic_cast<const IntegerType *>(value.qualifiedType.getType())) {
      throw SemaException("the expression shall have an integer type", constantExpression->involvedTokens());
    } else {
      auto *constInt = llvm::dyn_cast<llvm::ConstantInt>(value.value);
      if (!constInt) throw std::runtime_error("WTF: not a llvm::ConstantInt, weird");
      size = constInt->getSExtValue();
      if (size <= 0) {
        throw SemaException("array size should great than 0", constantExpression->involvedTokens());
      }
    }
  }
  // The element type shall not be an incomplete or function type.
  if (!derivedType.getType()->complete() || dynamic_cast<const FunctionType *>(derivedType.getType())) {
    throw SemaException("The element type shall not be an incomplete or function type.", involvedTokens());
  }
  mArrayType = std::make_unique<ArrayType>(derivedType, size);
  return directDeclarator->codegen(storageSpecifier,
                                   QualifiedType(mArrayType.get(), std::set<TypeQualifier>{}));
}
void FunctionDeclaratorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  directDeclarator->print(indent);
  if (parameterList) parameterList->print(indent);
}
FunctionDeclaratorAST::FunctionDeclaratorAST(nt<DirectDeclaratorAST> directDeclarator,
                                             nt<ParameterListAST> parameterList)
    : directDeclarator(std::move(directDeclarator)), parameterList(std::move(parameterList)) {}
const Token *FunctionDeclaratorAST::getIdentifier() {
  return directDeclarator->getIdentifier();
}
ISymbol *FunctionDeclaratorAST::codegen(StorageSpecifier storageSpecifier, const QualifiedType &derivedType) {
  if (dynamic_cast<const FunctionType *>(derivedType.getType())
      || dynamic_cast<const ArrayType *>(derivedType.getType())) {
    throw SemaException(
        std::string("function ") + getIdentifier()->getValue() + "cannot be array type or function type",
        involvedTokens());
  }
  mFunctionType = std::make_unique<FunctionType>(derivedType, parameterList->codegen());
  QualifiedType qualifiedType(mFunctionType.get(), {});
  return directDeclarator->codegen(storageSpecifier, qualifiedType);
}
void IdentifierPrimaryExpressionAST::print(int indent) {
  PrimaryExpressionAST::print(indent);
  identifier->print(++indent);
}
IExpression::Value IdentifierPrimaryExpressionAST::codegen() {
  auto *symbol = sObjectTable->lookup(identifier->token);
  if (!symbol) {
    throw SemaException(identifier->token.getValue() + " is not declared", identifier->token);
  }
  QualifiedType qualifiedType;
  bool lvalue = false;
  llvm::Value *value;
  if (auto *obj = dynamic_cast<ObjectSymbol *>(symbol)) {
    qualifiedType = obj->getQualifiedType();
    lvalue = true;
    value = obj->getValue();
  } else if (auto *enumeration = dynamic_cast<EnumConstSymbol *>(symbol)) {
    qualifiedType = QualifiedType(enumeration->getType(), {TypeQualifier::kCONST});
    value = enumeration->getValue();
  } else {
    throw SemaException("identifier as a primary expression must be object or function or enumeration const",
                        identifier->token);
  }
  return Value(qualifiedType, lvalue, value);
}
IdentifierPrimaryExpressionAST::IdentifierPrimaryExpressionAST(nt<IdentifierAST> identifier) : identifier(std::move(
    identifier)) {}
void IntegerConstantPrimaryExpressionAST::print(int indent) {
  PrimaryExpressionAST::printIndent(indent);
  integer_constant->print(++indent);
}
IExpression::Value IntegerConstantPrimaryExpressionAST::codegen() {
  bool isBase10 = integer_constant->mToken.getValue()[0] != 0;
  unsigned long long int n = integer_constant->value;

  unsigned int intSize = IntegerType::sIntType.getSizeInBits();
  unsigned int longSize = IntegerType::sLongIntType.getSizeInBits();
  unsigned int longLongSize = IntegerType::sLongLongIntType.getSizeInBits();
  const IntegerType *type;
  switch (integer_constant->suffix) {
    case IntegerConstantAST::Suffix::None:
      if (isBase10) {
        if (!(n >> (intSize - 1))) {
          type = &IntegerType::sIntType;
        } else if (!(n >> (longSize - 1))) {
          type = &IntegerType::sLongIntType;
        } else {
          type = &IntegerType::sLongLongIntType;
        }
      } else {
        if (!(n >> (intSize - 1))) {
          type = &IntegerType::sIntType;
        } else if (!(n >> (intSize))) {
          type = &IntegerType::sUnsignedIntType;
        } else if (!(n >> (longSize - 1))) {
          type = &IntegerType::sLongIntType;
        } else if (!(n >> (longSize))) {
          type = &IntegerType::sUnsignedLongIntType;
        } else {
          type =
              !(n >> (longLongSize - 1)) ? &IntegerType::sLongLongIntType
                                         : &IntegerType::sUnsignedLongLongIntType;
        }
      }
      break;
    case IntegerConstantAST::Suffix::U:
      if (!(n >> (intSize - 1))) {
        type = &IntegerType::sUnsignedIntType;
      } else if (!(n >> (longSize - 1))) {
        type = &IntegerType::sUnsignedLongIntType;
      } else {
        type = &IntegerType::sUnsignedLongLongIntType;
      }
      break;
    case IntegerConstantAST::Suffix::L:
      if (isBase10) {
        if (!(n >> (longSize - 1))) {
          type = &IntegerType::sLongIntType;
        } else {
          type = &IntegerType::sLongLongIntType;
        }
      } else {
        if (!(n >> (longSize - 1))) {
          type = &IntegerType::sLongIntType;
        } else if (!(n >> (longSize))) {
          type = &IntegerType::sUnsignedLongIntType;
        } else {
          type =
              !(n >> (longLongSize - 1)) ? &IntegerType::sLongLongIntType
                                         : &IntegerType::sUnsignedLongLongIntType;
        }
      }
      break;
    case IntegerConstantAST::Suffix::UL:
      if (!(n >> (longSize - 1))) {
        type = &IntegerType::sUnsignedLongIntType;
      } else {
        type = &IntegerType::sUnsignedLongLongIntType;
      }
      break;
    case IntegerConstantAST::Suffix::LL:
      if (isBase10) {
        type = &IntegerType::sLongLongIntType;
      } else {
        type =
            !(n >> (longLongSize - 1)) ? &IntegerType::sLongLongIntType : &IntegerType::sUnsignedLongLongIntType;
      }
      break;
    case IntegerConstantAST::Suffix::ULL:type = &IntegerType::sUnsignedLongLongIntType;
      break;
  }
  return Value(QualifiedType(type, {TypeQualifier::kCONST}),
               false,
               llvm::ConstantInt::get(type->getLLVMType(sModule), type->getAPInt(n)));
}
IntegerConstantPrimaryExpressionAST::IntegerConstantPrimaryExpressionAST(nt<IntegerConstantAST> integer_constant)
    : integer_constant(std::move(
    integer_constant)) {}
void FloatingConstantPrimaryExpressionAST::print(int indent) {
  PrimaryExpressionAST::printIndent(indent);
  floating_constant->print(++indent);
}
IExpression::Value FloatingConstantPrimaryExpressionAST::codegen() {
  const FloatingType *type;
  switch (floating_constant->suffix) {
    case FloatingConstantAST::Suffix::None:type = &FloatingType::sDoubleType;
      break;
    case FloatingConstantAST::Suffix::F: type = &FloatingType::sFloatType;
      break;
    case FloatingConstantAST::Suffix::L: type = &FloatingType::sLongDoubleType;
      break;
  }
  return Value(QualifiedType(type, {TypeQualifier::kCONST}),
               false,
               llvm::ConstantFP::get(sModule.getContext(), type->getAPFloat(floating_constant->value)));
}
FloatingConstantPrimaryExpressionAST::FloatingConstantPrimaryExpressionAST(nt<FloatingConstantAST> floating_constant)
    : floating_constant(std::move(
    floating_constant)) {}
void CharacterConstantPrimaryExpressionAST::print(int indent) {
  PrimaryExpressionAST::print(indent);
  character_constant->print(++indent);
}
IExpression::Value CharacterConstantPrimaryExpressionAST::codegen() {
  const IntegerType *type = &IntegerType::sCharType;
  return Value(QualifiedType(type, {TypeQualifier::kCONST}),
               false,
               llvm::ConstantInt::get(type->getLLVMType(sModule),
                                      type->getAPInt(static_cast<uint64_t>(character_constant->c))));
}
CharacterConstantPrimaryExpressionAST::CharacterConstantPrimaryExpressionAST(nt<CharacterConstantAST> character_constant)
    : character_constant(std::move(
    character_constant)) {}
void StringPrimaryExpressionAST::print(int indent) {
  PrimaryExpressionAST::print(indent);
  string->print(++indent);
}
IExpression::Value StringPrimaryExpressionAST::codegen() {
  return Value(QualifiedType(string->mType.get(), {TypeQualifier::kCONST}),
               true,
               sBuilder.CreateGlobalStringPtr(string->mToken.getValue()));
}
StringPrimaryExpressionAST::StringPrimaryExpressionAST(nt<StringAST> string) : string(std::move(string)) {}
void ExpressionPrimaryExpressionAST::print(int indent) {
  PrimaryExpressionAST::print(indent);
  expression->print(++indent);
}
IExpression::Value ExpressionPrimaryExpressionAST::codegen() {
  return expression->codegen();
}
ExpressionPrimaryExpressionAST::ExpressionPrimaryExpressionAST(nt<ExpressionAST> expression) : expression(std::move(
    expression)) {}
void SimplePostfixExpressionAST::print(int indent) {
  AST::print(indent);
  primary_expression->print(++indent);
}
IExpression::Value SimplePostfixExpressionAST::codegen() {
  return primary_expression->codegen();
}
void ArrayPostfixExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  postfix_expression->print(indent);
  expression->print(indent);
}
IExpression::Value ArrayPostfixExpressionAST::codegen() {
  // 6.5.2.1 Array subscripting
  Value lhs = postfix_expression->codegen();
  Value rhs = expression->codegen();
  const auto *tPointer = dynamic_cast<const PointerType *>(lhs.qualifiedType.getType());
  if (!tPointer) {
    throw SemaException("array left side should be pointer to complete object type",
                        postfix_expression->involvedTokens());
  }
  const auto *tObject = dynamic_cast<const ObjectType *>(tPointer->getReferencedType());
  if (!tObject || !tObject->complete()) {
    throw SemaException("array left side should be pointer to complete object type",
                        postfix_expression->involvedTokens());
  }

  if (!(dynamic_cast<const IntegerType *>(rhs.qualifiedType.getType()))) {
    throw SemaException("array right side should be an integer type",
                        postfix_expression->involvedTokens());
  }

  llvm::Value *value = nullptr;
  auto *constantInt = llvm::dyn_cast<llvm::ConstantInt>(rhs.value);
  auto index = constantInt->getSExtValue();
  if (index < 0) {
    //TODO Warning index is before 0
    value = nullptr;
  } else if (const auto *globalVariable = llvm::dyn_cast<llvm::GlobalVariable>(lhs.value)) {
    if (globalVariable->hasInitializer()) {
      if (const auto *constantArray = llvm::dyn_cast<llvm::ConstantArray>(globalVariable->getInitializer())) {
        value = constantArray->getAggregateElement(constantInt);
      }
    }
  }

  if (!value) {
    if (sObjectTable->getScopeKind() == ScopeKind::BLOCK) {
      value = sBuilder.CreateGEP(lhs.qualifiedType.getType()->getLLVMType(sModule),
                                 lhs.value,
                                 constantInt);
    } else {
      throw std::runtime_error("WTF: array position query in other scope? we may need global constructor here");
    }
  }

  return Value(tPointer->getReferencedQualifiedType(), true, value);
}
void FunctionPostfixExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  postfix_expression->print(indent);
  argument_expression_list->print(indent);
}
IExpression::Value FunctionPostfixExpressionAST::codegen() {
  //6.5.2.2 Function calls
  Value lhs = postfix_expression->codegen();
  auto *p = dynamic_cast<const PointerType *>(lhs.qualifiedType.getType());
  if (!p) {
    throw SemaException("The expression that denotes the called function92) shall have type pointer to function",
                        postfix_expression->involvedTokens());
  }
  auto *tFunction = dynamic_cast<const FunctionType *>(p->getReferencedType());
  if (!tFunction) {
    throw SemaException("left side must be a function type", postfix_expression->involvedTokens());
  }
  auto returnType = tFunction->getReturnType();
  if (dynamic_cast<const ArrayType *>(returnType.getType())) {
    throw SemaException("return type cannot be array type", postfix_expression->involvedTokens());
  }
  if (dynamic_cast<const FunctionType *>(returnType.getType())) {
    throw SemaException("return type cannot be function type", postfix_expression->involvedTokens());
  }
  const auto &arguments = argument_expression_list->codegen();
  if (tFunction->getParameters().size() != arguments.size()) {
    throw SemaException("arguments do not match function proto type", postfix_expression->involvedTokens());
  }
  auto para = tFunction->getParameters().begin();
  auto arg = arguments.begin();
  std::vector<llvm::Value *> argumentValues;
  while (para != tFunction->getParameters().end()) {
    auto *argType = dynamic_cast<const ObjectType *>(arg->qualifiedType.getType());
    if (!argType) {
      throw SemaException("arguments must be object type", postfix_expression->involvedTokens());
    }
    if (!argType->complete()) {
      throw SemaException("arguments must be completed type", postfix_expression->involvedTokens());
    }
    if (!argType->compatible(para->getType())) {
      throw SemaException("arguments do not match function proto type", postfix_expression->involvedTokens());
    }
    argumentValues.push_back(arg->value);
    ++para;
    ++arg;
  }
  return Value(tFunction->getReturnType(), false, sBuilder.CreateCall(lhs.value, argumentValues));
}
void MemberPostfixExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  postfix_expression->print(indent);
  identifier->print(indent);
}
IExpression::Value MemberPostfixExpressionAST::codegen() {
  //6.5.2.3 Structure and union members
  Value lhs = postfix_expression->codegen();
  const CompoundType *compoundTy;
  if (!dynamic_cast<const StructType *>(lhs.qualifiedType.getType())
      && !dynamic_cast<const UnionType *>(lhs.qualifiedType.getType())) {
    throw SemaException("left side must be a struct type or union type", postfix_expression->involvedTokens());
  } else {
    compoundTy = dynamic_cast<const CompoundType *>(lhs.qualifiedType.getType());
  }
  auto *memberSymbol = dynamic_cast<ObjectSymbol *>(compoundTy->mTable.lookup(identifier->token));
  //TODO 6.5.2.3.6
  if (!memberSymbol) {
    throw SemaException(identifier->token.getValue() + "is not a member of " + compoundTy->getTagName(),
                        identifier->involvedTokens());
  }
  QualifiedType qualifiedType(memberSymbol->getQualifiedType());
  qualifiedType.addQualifiers(lhs.qualifiedType.getQualifiers());
  unsigned int index;
  if (dynamic_cast<const StructType *>(lhs.qualifiedType.getType())) {
    index = memberSymbol->getIndex();
  } else {
    index = 0;
  }
  llvm::Value *value = sBuilder.CreateConstGEP1_32(lhs.qualifiedType.getType()->getLLVMType(sModule),
                                                   lhs.value,
                                                   index);
  return Value(qualifiedType, lhs.lvalue, value);
}
void PointerMemberPostfixExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  postfix_expression->print(indent);
  identifier->print(indent);
}
IExpression::Value PointerMemberPostfixExpressionAST::codegen() {
  //6.5.2.3 Structure and union members
  Value lhs = postfix_expression->codegen();
  const CompoundType *compoundTy;
  if (const auto *p = dynamic_cast<const PointerType *>(lhs.qualifiedType.getType())) {
    if (!dynamic_cast<const StructType *>(p->getReferencedType())
        && !dynamic_cast<const UnionType *>(p->getReferencedType())) {
      throw SemaException("left side must be a pointer to a struct type or union type",
                          postfix_expression->involvedTokens());
    } else {
      compoundTy = dynamic_cast<const CompoundType *>(p->getReferencedType());
    }
  } else {
    throw SemaException("left side must be a pointer type", postfix_expression->involvedTokens());
  }
  auto *memberSymbol = dynamic_cast<ObjectSymbol *>(compoundTy->mTable.lookup(identifier->token));
  if (!memberSymbol) {
    throw SemaException(identifier->token.getValue() + "is not a member of " + compoundTy->getTagName(),
                        identifier->involvedTokens());
  }
  QualifiedType qualifiedType(memberSymbol->getQualifiedType());
  qualifiedType.addQualifiers(lhs.qualifiedType.getQualifiers());
  unsigned int index;
  if (dynamic_cast<const StructType *>(lhs.qualifiedType.getType())) {
    index = memberSymbol->getIndex();
  } else {
    index = 0;
  }
  llvm::Value *value = sBuilder.CreateConstGEP1_32(lhs.qualifiedType.getType()->getLLVMType(sModule),
                                                   lhs.value,
                                                   index);
  return Value(qualifiedType, true, value);
}
void IncrementPostfixExpression::print(int indent) {
  AST::print(indent);
  postfix_expression->print(++indent);
}
IExpression::Value IncrementPostfixExpression::codegen() {
  // 6.5.2.4 Postfix increment and decrement operators
  Value lhs = postfix_expression->codegen();
  const Type *type = lhs.qualifiedType.getType();
  llvm::Value *result = sBuilder.CreateAlloca(lhs.qualifiedType.getType()->getLLVMType(sModule));
  sBuilder.CreateStore(lhs.value, result, lhs.qualifiedType.isVolatile());
  llvm::Value *newVal;
  if (dynamic_cast<const IntegerType *>(type) || dynamic_cast<const EnumerationType *>(type)) {
    newVal = sBuilder.CreateAdd(lhs.value, llvm::ConstantInt::get(sContext, llvm::APInt(32, 1)));
  } else if (dynamic_cast<const FloatingType *>(type)) {
    newVal = sBuilder.CreateFAdd(lhs.value, llvm::ConstantFP::get(sContext, llvm::APFloat(1.0f)));
  } else if (dynamic_cast<const PointerType *>(type)) {
    newVal = sBuilder.CreateGEP(lhs.value, llvm::ConstantInt::get(sContext, llvm::APInt(32, 1)));
  } else {
    throw SemaException(
        "The operand of the postfix increment or decrement operator shall have real or pointer type, and shall be a modifiable lvalue.",
        postfix_expression->involvedTokens());
  }
  sBuilder.CreateStore(newVal, lhs.value, lhs.qualifiedType.isVolatile());
  if (lhs.qualifiedType.isConst() || !lhs.lvalue) {
    throw SemaException("The operand shall be a modifiable lvalue", postfix_expression->involvedTokens());
  }
  return Value(lhs.qualifiedType, true, result);
}
void DecrementPostfixExpression::print(int indent) {
  AST::print(indent);
  postfix_expression->print(++indent);
}
IExpression::Value DecrementPostfixExpression::codegen() {
  // 6.5.2.4 Postfix increment and decrement operators
  Value lhs = postfix_expression->codegen();
  const Type *type = lhs.qualifiedType.getType();
  llvm::Value *result = sBuilder.CreateAlloca(lhs.qualifiedType.getType()->getLLVMType(sModule));
  sBuilder.CreateStore(lhs.value, result, lhs.qualifiedType.isVolatile());
  llvm::Value *newVal;
  if (dynamic_cast<const IntegerType *>(type) || dynamic_cast<const EnumerationType *>(type)) {
    newVal = sBuilder.CreateSub(lhs.value, llvm::ConstantInt::get(sContext, llvm::APInt(32, 1)));
  } else if (dynamic_cast<const FloatingType *>(type)) {
    newVal = sBuilder.CreateFSub(lhs.value, llvm::ConstantFP::get(sContext, llvm::APFloat(1.0f)));
  } else if (dynamic_cast<const PointerType *>(type)) {
    newVal = sBuilder.CreateGEP(lhs.value, llvm::ConstantInt::get(sContext, llvm::APInt(32, -1, true)));
  } else {
    throw SemaException(
        "The operand of the postfix increment or decrement operator shall have real or pointer type, and shall be a modifiable lvalue.",
        postfix_expression->involvedTokens());
  }
  sBuilder.CreateStore(newVal, lhs.value, lhs.qualifiedType.isVolatile());
  if (lhs.qualifiedType.contains(TypeQualifier::kCONST) || !lhs.lvalue) {
    throw SemaException("The operand shall be a modifiable lvalue", postfix_expression->involvedTokens());
  }
  return Value(lhs.qualifiedType, true, result);
}
void SimpleUnaryExpressionAST::print(int indent) {
  AST::print(indent);
  mPostfixExpression->print(++indent);
}
IExpression::Value SimpleUnaryExpressionAST::codegen() {
  return mPostfixExpression->codegen();
}
void PrefixIncrementExpressionAST::print(int indent) {
  AST::print(indent);
  mUnaryExpression->print(++indent);
}
IExpression::Value PrefixIncrementExpressionAST::codegen() {
  //6.5.3.1 Prefix increment and decrement operators
  Value value = mUnaryExpression->codegen();
  llvm::Value *newVal;
  if (dynamic_cast<const IntegerType *>(value.qualifiedType.getType())
      || dynamic_cast<const EnumerationType *>(value.qualifiedType.getType())) {
    newVal = sBuilder.CreateAdd(value.value, llvm::ConstantInt::get(sContext, llvm::APInt(32, 1)));
  } else if (dynamic_cast<const FloatingType *>(value.qualifiedType.getType())) {
    newVal = sBuilder.CreateFAdd(value.value, llvm::ConstantFP::get(sContext, llvm::APFloat(1.0f)));
  } else if (!dynamic_cast<const PointerType *>(value.qualifiedType.getType())) {
    newVal = sBuilder.CreateGEP(value.value, llvm::ConstantInt::get(sContext, llvm::APInt(32, 1)));
  } else {
    throw SemaException(
        "The operand of the prefix increment or decrement operator shall have real or pointer type, and shall be a modifiable lvalue.",
        mUnaryExpression->involvedTokens());
  }
  sBuilder.CreateStore(newVal, value.value, value.qualifiedType.isVolatile());
  if (!value.qualifiedType.isConst() || !value.lvalue) {
    throw SemaException("The operand shall be a modifiable lvalue", mUnaryExpression->involvedTokens());
  }
  return value;
}
void PrefixDecrementExpressionAST::print(int indent) {
  AST::print(indent);
  mUnaryExpression->print(++indent);
}
IExpression::Value PrefixDecrementExpressionAST::codegen() {
  //6.5.3.1 Prefix increment and decrement operators
  Value value = mUnaryExpression->codegen();
  llvm::Value *newVal;
  if (dynamic_cast<const IntegerType *>(value.qualifiedType.getType())
      || dynamic_cast<const EnumerationType *>(value.qualifiedType.getType())) {
    newVal = sBuilder.CreateSub(value.value, llvm::ConstantInt::get(sContext, llvm::APInt(32, 1)));
  } else if (dynamic_cast<const FloatingType *>(value.qualifiedType.getType())) {
    newVal = sBuilder.CreateFSub(value.value, llvm::ConstantFP::get(sContext, llvm::APFloat(1.0f)));
  } else if (!dynamic_cast<const PointerType *>(value.qualifiedType.getType())) {
    newVal = sBuilder.CreateGEP(value.value, llvm::ConstantInt::get(sContext, llvm::APInt(32, -1, true)));
  } else {
    throw SemaException(
        "The operand of the prefix increment or decrement operator shall have real or pointer type, and shall be a modifiable lvalue.",
        mUnaryExpression->involvedTokens());
  }
  sBuilder.CreateStore(newVal, value.value, value.qualifiedType.isVolatile());
  if (!value.qualifiedType.isConst() || !value.lvalue) {
    throw SemaException("The operand shall be a modifiable lvalue", mUnaryExpression->involvedTokens());
  }
  return value;
}
