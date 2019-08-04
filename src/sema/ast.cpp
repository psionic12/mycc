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
  //TODO not finished
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
        table.insert(ast->getIdentifier(),
                     std::make_unique<TypedefSymbol>(std::make_pair<const Token &, const Token &>(ast->getIdentifier(),
                                                                                                  ast->getIdentifier())));
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
  declaration_specifiers->codegen();
  for (auto &ast : init_declarators) {
    //TODO   ast.first->codegen();
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
void DeclarationSpecifiersAST::codegen() {
  if (storage_specifiers.size() > 1) {
    throw SemaException(
        "At most, one storage-class specifier may be given in the declaration specifiers in a declaration",
        storage_specifiers.back().token);
  }
  //TODO The declaration of an identifier for a function that has block scope shall have no explicit storage-class specifier other than extern.
  //TODO If an aggregate or union object is declared with a storage-class specifier other than typedef, the properties resulting from the storage-class specifier, except with respect to linkage, also apply to the members of the object, and so on recursively for any aggregate or union member objects.
  //TODO At least one type specifier shall be given in the declaration specifiers in each declaration,  and in the specifier-qualifier list in each struct declaration and type name.
  type_specifiers->codegen();
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
const Token &DeclaratorAST::getIdentifier() const {
  return direct_declarator->getIdentifier();
}
void DeclaratorAST::codegen(Terminal<StorageSpecifier> storageSpecifier,
                            const QualifiedType &derivedType,
                            bool isStruct) {
  direct_declarator->codegen(storageSpecifier, pointer->codegen(derivedType), false);
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
  std::set<TypeQualifier> qualifierSet;
  for (auto &qualifier : qualifiers) {
    qualifierSet.emplace(qualifier->op);
  }
  return QualifiedType(types->codegen(), std::move(qualifierSet));
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
  ISymbol *symbol;
  CompoundType *tagType;
  //TODO A struct-declaration that does not declare an anonymous structure or anonymous union shall contain a struct-declarator-list.
  if (bStruct == StructOrUnion::kSTRUCT) {
    if (id) {
      const auto &token = id->token;
      mSymbol = std::make_unique<TagSymbol>(std::make_unique<StructType>(token.getValue(), sModule, involvedTokens()));
      sTagTable->insert(token, mSymbol.get());
    } else {
      mSymbol = std::make_unique<TagSymbol>(std::make_unique<StructType>(sModule), involvedTokens());
      sTagTable->insert(mSymbol.get());
    }
  } else {
    if (id) {
      const auto &token = id->token;
      mSymbol = std::make_unique<TagSymbol>(std::make_unique<UnionType>(token.getValue(), sModule), involvedTokens());
      sTagTable->insert(token, mSymbol.get());
    } else {
      mSymbol = std::make_unique<TagSymbol>(std::make_unique<UnionType>(sModule), involvedTokens());
      symbol = sTagTable->insert(mSymbol.get());
    }
  }
  tagType = static_cast<TagSymbol *>(symbol)->getTagType();
  std::map<std::string, const QualifiedType> members;
  for (const auto &declaration :declarations) {
    auto d = declaration->codegen();
    for (const std::string &name : d.second) {
      members.emplace(name, d.first);
    }
  }
  tagType->setBody(std::move(members));
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
std::pair<const QualifiedType, std::vector<std::string>> StructDeclarationAST::codegen() {

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
      op(std::make_unique<Terminal<InfixOp>>(op)),
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
UnaryExpressionAST::UnaryExpressionAST(nt<TypeNameAST>
                                       type_name)
    : AST(AST::Kind::UNARY_EXPRESSION, 5),
      type_name(std::move(type_name)) {}
UnaryExpressionAST::UnaryExpressionAST(Terminal<UnaryOp>
                                       op, nt<CastExpressionAST>
                                       cast_expression)
    : op(std::make_unique<Terminal<UnaryOp>>(op)),
      cast_expression(std::move(cast_expression)),
      AST(AST::Kind::UNARY_EXPRESSION, 3) {}
UnaryExpressionAST::UnaryExpressionAST(nt<UnaryExpressionAST>
                                       unary_expression,
                                       UnaryExpressionAST::PrefixType
                                       type)
    : AST(AST::Kind::UNARY_EXPRESSION, static_cast<int>(type)),
      unary_expression(std::move(unary_expression)) {}
UnaryExpressionAST::UnaryExpressionAST(nt<PostfixExpressionAST>
                                       postfix_expression)
    : AST(AST::Kind::UNARY_EXPRESSION, 0),
      postfix_expression(std::move(postfix_expression)) {
}
void UnaryExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  switch (getProduction()) {
    case 0:postfix_expression->print(indent);
      break;
    case 1:
    case 2:
    case 4:unary_expression->print(indent);
      break;
    case 3:op->print(indent);
      cast_expression->print(indent);
      break;
    case 5:type_name->print(indent);
      break;
  }
}
IExpression::Value UnaryExpressionAST::codegen() {
  switch (getProduction()) {
    case 0: {
      auto *postfix = postfix_expression.get();
      postfix->codegen();
      mType = postfix->mType;
      mQualifiers = postfix->mQualifiers;
      mLvalue = postfix->mLvalue;
      break;
    }
    case 1:
    case 2: {
      //6.5.3.1 Prefix increment and decrement operators
      auto *unary = unary_expression.get();
      unary->codegen();
      if (!dynamic_cast<const IntegerType *>(unary->mType) &&
          !dynamic_cast<const FloatingType *>(unary->mType) &&
          !dynamic_cast<const PointerType *>(unary->mType)) {
        throw SemaException(
            "The operand of the prefix increment or decrement operator shall have real or pointer type, and shall be a modifiable lvalue.",
            unary->involvedTokens());
      }
      if (unary->mQualifiers.find(TypeQualifier::kCONST) == unary->mQualifiers.end()
          || !unary->mLvalue) {
        throw SemaException("The operand shall be a modifiable lvalue", unary->involvedTokens());
      }
      mType = unary->mType;
      mLvalue = unary->mLvalue;
      mQualifiers = unary->mQualifiers;
      break;
    }
    case 3: {
      //6.5.3.2 Address and indirection operators
      auto op = this->op->type;
      auto *cast_exp = cast_expression.get();
      cast_exp->codegen();
      switch (op) {
        case UnaryOp::AMP:break;
      }
    }
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
PostfixExpressionAST::PostfixExpressionAST(nt<PrimaryExpressionAST>
                                           primary)
    : AST(AST::Kind::POSTFIX_EXPRESSION, 0),
      left(std::move(primary)),
      right(nullptr) {}
PostfixExpressionAST::PostfixExpressionAST(nt<PostfixExpressionAST>
                                           left, nt<ExpressionAST>
                                           right)
    : AST(AST::Kind::POSTFIX_EXPRESSION, 1),
      left(std::move(left)),
      right(std::move(right)) {}
PostfixExpressionAST::PostfixExpressionAST(nt<PostfixExpressionAST>
                                           left, nt<ArgumentExpressionList>
                                           right)
    : AST(AST::Kind::POSTFIX_EXPRESSION, 2),
      left(std::move(left)),
      right(std::move(right)) {}
PostfixExpressionAST::PostfixExpressionAST(nt<PostfixExpressionAST>
                                           left,
                                           PostfixExpressionAST::identifierOperator
                                           io,
                                           nt<IdentifierAST>
                                           right)
    : AST(AST::Kind::POSTFIX_EXPRESSION, static_cast<int>(io)),
      left(std::move(left)),
      right(std::move(right)) {}
PostfixExpressionAST::PostfixExpressionAST(nt<PostfixExpressionAST>
                                           left, PostfixExpressionAST::Xcrement
                                           x)
    : AST(AST::Kind::POSTFIX_EXPRESSION, static_cast<int>(x)),
      left(std::move(left)),
      right(nullptr) {}
void PostfixExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  left->print(indent);
  if (right) {
    right->print(indent);
  }
}
IExpression::Value PostfixExpressionAST::codegen() {
  switch (getProduction()) {
    case 0: {
      auto *primary = static_cast<PrimaryExpressionAST *>(left.get());
      primary->codegen();
      mType = primary->mType;
      mQualifiers = primary->mQualifiers;
      mLvalue = primary->mLvalue;
      break;
    }
    case 1: {
      // 6.5.2.1 Array subscripting
      auto *postfix = static_cast<PostfixExpressionAST *>(left.get());
      postfix->codegen();
      auto *exp = static_cast<ExpressionAST *>(right.get());
      exp->codegen();
      const auto *tPointer = dynamic_cast<const PointerType *>(postfix->mType);
      if (!tPointer) {
        throw SemaException("array left side should be pointer to complete object type",
                            postfix->involvedTokens());
      }
      const auto *tObject = dynamic_cast<const ObjectType *>(tPointer->getReferencedType());
      if (!tObject || !tObject->complete()) {
        throw SemaException("array left side should be pointer to complete object type",
                            postfix->involvedTokens());
      }

      if (!(dynamic_cast<const IntegerType *>(exp->mType))) {
        throw SemaException("array right side should be an integer type",
                            postfix->involvedTokens());
      }
      mType = tPointer->getReferencedType();
      mQualifiers = tPointer->qualifiersToReferencedType();
      mLvalue = true;
      break;
    }
    case 2: {
      //6.5.2.2 Function calls
      auto *postfix = static_cast<PostfixExpressionAST *>(left.get());
      postfix->codegen();
      auto *p = dynamic_cast<const PointerType *>(postfix->mType);
      auto *tFunction = dynamic_cast<const FunctionType *>(p->getReferencedType());
      if (!tFunction) {
        throw SemaException("left side must be a function type", postfix->involvedTokens());
      }
      auto *returnType = tFunction->getReturnType();
      if (dynamic_cast<const ArrayType *>(returnType)) {
        throw SemaException("return type cannot be array type", postfix->involvedTokens());
      }
      if (dynamic_cast<const FunctionType *>(returnType)) {
        throw SemaException("return type cannot be function type", postfix->involvedTokens());
      }
      auto *arguments = static_cast<ArgumentExpressionList *>(right.get());
      if (tFunction->getParameters().size() != arguments->mArgumentList.size()) {
        throw SemaException("arguments do not match function proto type", postfix->involvedTokens());
      }
      auto para = tFunction->getParameters().begin();
      auto arg = arguments->mArgumentList.begin();
      while (para != tFunction->getParameters().end()) {
        (*arg)->codegen();
        auto *argType = dynamic_cast<const ObjectType *>((*arg)->mType);
        if (!argType) {
          throw SemaException("arguments must be object type", postfix->involvedTokens());
        }
        if (!argType->complete()) {
          throw SemaException("arguments must be completed type", postfix->involvedTokens());
        }
        if (!argType->compatible(*para)) {
          throw SemaException("arguments do not match function proto type", postfix->involvedTokens());
        }
      }
      mType = tFunction->getReturnType();
      mLvalue = false;
      break;
    }
    case 3:
    case 4: {
      //6.5.2.3 Structure and union members
      auto *postfix = static_cast<PostfixExpressionAST *>(left.get());
      postfix->codegen();
      const CompoundType *tCompoundType;
      if (getProduction() == 3) {
        if (!dynamic_cast<const StructType *>(postfix->mType) && !dynamic_cast<const UnionType *>(postfix->mType)) {
          throw SemaException("left side must be a struct type or union type", postfix->involvedTokens());
        } else {
          tCompoundType = dynamic_cast<const CompoundType *>(postfix->mType);
        }
      } else {
        if (const auto *p = dynamic_cast<const PointerType *>(postfix->mType)) {
          if (!dynamic_cast<const StructType *>(p->getReferencedType())
              && !dynamic_cast<const UnionType *>(p->getReferencedType())) {
            throw SemaException("left side must be a pointer to a struct type or union type",
                                postfix->involvedTokens());
          } else {
            tCompoundType = dynamic_cast<const CompoundType *>(p->getReferencedType());
          }
        } else {
          throw SemaException("left side must be a pointer type", postfix->involvedTokens());
        }
      }
      auto *id = static_cast<IdentifierAST *>(right.get());
      const std::string &memberName = id->token.getValue();
      // TODO check if not a member
      mLvalue = postfix->mLvalue;
      mQualifiers.insert(postfix->mQualifiers.begin(), postfix->mQualifiers.end());
      mLvalue = postfix->mLvalue;
      break;
    }
    case 5:
    case 6: {
      // 6.5.2.4 Postfix increment and decrement operators
      auto *postfix = static_cast<PostfixExpressionAST *>(left.get());
      postfix->codegen();
      if (!dynamic_cast<const IntegerType *>(postfix->mType) &&
          !dynamic_cast<const FloatingType *>(postfix->mType) &&
          !dynamic_cast<const PointerType *>(postfix->mType)) {
        throw SemaException(
            "The operand of the postfix increment or decrement operator shall have real or pointer type, and shall be a modifiable lvalue.",
            postfix->involvedTokens());
      }
      if (postfix->mQualifiers.find(TypeQualifier::kCONST) == postfix->mQualifiers.end()
          || !postfix->mLvalue) {
        throw SemaException("The operand shall be a modifiable lvalue", postfix->involvedTokens());
      }
      mType = postfix->mType;
      mLvalue = postfix->mLvalue;
      mQualifiers = postfix->mQualifiers;
      break;
    }
  }
}
PrimaryExpressionAST::PrimaryExpressionAST(nt<IdentifierAST>
                                           id)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 0), ast(std::move(id)) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<IntegerConstantAST>
                                           interger_constant)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 1), ast(std::move(interger_constant)) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<FloatingConstantAST>
                                           floating_constatnt)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 2), ast(std::move(floating_constatnt)) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<CharacterConstantAST>
                                           character_constant)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 3), ast(std::move(character_constant)) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<StringAST>
                                           string)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 4), ast(std::move(string)) {}
PrimaryExpressionAST::PrimaryExpressionAST(nt<ExpressionAST>
                                           exp)
    : AST(AST::Kind::PRIMARY_EXPRESSION, 5), ast(std::move(exp)) {}
void PrimaryExpressionAST::print(int indent) {
  AST::print(indent);
  ast->print(++indent);
}
IExpression::Value PrimaryExpressionAST::codegen() {
  switch (getProduction()) {
    case 0: { // identifier
      auto *identifierAST = static_cast<IdentifierAST *>(ast.get());
      auto *symbol = sObjectTable->lookup(identifierAST->token);
      SymbolKind kind = symbol->getKind();
      if (kind == SymbolKind::OBJECT) {
        mType = static_cast<ObjectSymbol *>(symbol)->getType();
        mQualifiers = static_cast<ObjectSymbol *>(symbol)->getQualifiers();
        mLvalue = true;
      } else if (kind == SymbolKind::ENUMERATION_CONSTANT) {
        mType = static_cast<EnumConstSymbol *>(symbol)->getType();
      } else {
        throw SemaException("identifier as a primary expression must be object or function or enumeration const",
                            identifierAST->token);
      }
      break;
    }
    case 1: {
      auto *integerConstAST = static_cast<IntegerConstantAST *>(ast.get());
      bool isBase10 = integerConstAST->mToken.getValue()[0] != 0;
      unsigned long long int n = integerConstAST->value;

      unsigned int intSize = IntegerType::sIntType.getSizeInBits();
      unsigned int longSize = IntegerType::sLongIntType.getSizeInBits();
      unsigned int longLongSize = IntegerType::sLongLongIntType.getSizeInBits();

      switch (integerConstAST->suffix) {
        case IntegerConstantAST::Suffix::None:
          if (isBase10) {
            if (!(n >> (intSize - 1))) {
              mType = &IntegerType::sIntType;
            } else if (!(n >> (longSize - 1))) {
              mType = &IntegerType::sLongIntType;
            } else {
              mType = &IntegerType::sLongLongIntType;
            }
          } else {
            if (!(n >> (intSize - 1))) {
              mType = &IntegerType::sIntType;
            } else if (!(n >> (intSize))) {
              mType = &IntegerType::sUnsignedIntType;
            } else if (!(n >> (longSize - 1))) {
              mType = &IntegerType::sLongIntType;
            } else if (!(n >> (longSize))) {
              mType = &IntegerType::sUnsignedLongIntType;
            } else {
              mType =
                  !(n >> (longLongSize - 1)) ? &IntegerType::sLongLongIntType
                                             : &IntegerType::sUnsignedLongLongIntType;
            }
          }
          break;
        case IntegerConstantAST::Suffix::U:
          if (!(n >> (intSize - 1))) {
            mType = &IntegerType::sUnsignedIntType;
          } else if (!(n >> (longSize - 1))) {
            mType = &IntegerType::sUnsignedLongIntType;
          } else {
            mType = &IntegerType::sUnsignedLongLongIntType;
          }
          break;
        case IntegerConstantAST::Suffix::L:
          if (isBase10) {
            if (!(n >> (longSize - 1))) {
              mType = &IntegerType::sLongIntType;
            } else {
              mType = &IntegerType::sLongLongIntType;
            }
          } else {
            if (!(n >> (longSize - 1))) {
              mType = &IntegerType::sLongIntType;
            } else if (!(n >> (longSize))) {
              mType = &IntegerType::sUnsignedLongIntType;
            } else {
              mType =
                  !(n >> (longLongSize - 1)) ? &IntegerType::sLongLongIntType
                                             : &IntegerType::sUnsignedLongLongIntType;
            }
          }
          break;
        case IntegerConstantAST::Suffix::UL:
          if (!(n >> (longSize - 1))) {
            mType = &IntegerType::sUnsignedLongIntType;
          } else {
            mType = &IntegerType::sUnsignedLongLongIntType;
          }
          break;
        case IntegerConstantAST::Suffix::LL:
          if (isBase10) {
            mType = &IntegerType::sLongLongIntType;
          } else {
            mType =
                !(n >> (longLongSize - 1)) ? &IntegerType::sLongLongIntType : &IntegerType::sUnsignedLongLongIntType;
          }
          break;
        case IntegerConstantAST::Suffix::ULL:mType = &IntegerType::sUnsignedLongLongIntType;
          break;
      }
    }
    case 2: {
      auto *floatingConstAST = static_cast<FloatingConstantAST *>(ast.get());
      switch (floatingConstAST->mSuffix) {
        case FloatingConstantAST::Suffix::None:mType = &FloatingType::sDoubleType;
          break;
        case FloatingConstantAST::Suffix::F: mType = &FloatingType::sFloatType;
          break;
        case FloatingConstantAST::Suffix::L: mType = &FloatingType::sLongDoubleType;
          break;
      }
      break;
    }
    case 3:mType = &IntegerType::sCharType;
    case 4: {
      auto *stringAST = static_cast<StringAST *>(ast.get());
      mType = stringAST->mType.get();
      mQualifiers.emplace(TypeQualifier::kCONST);
      mLvalue = true;
      break;
    }
    case 5: {
      auto *exp = static_cast<ExpressionAST *>(ast.get());
      mType = exp->mType;
      mQualifiers = exp->mQualifiers;
      mLvalue = exp->mLvalue;
      break;
    }
    default:break;
  }
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
CharacterConstantAST::CharacterConstantAST(
    const Token &token)
    : AST(AST::Kind::CHARACTER_CONSTANT), mToken(token), c(token.getValue().c_str()[0]) {}
FloatingConstantAST::FloatingConstantAST(
    const Token &token)
    : AST(AST::Kind::FLOATING_CONSTANT), mToken(token) {
  std::string::size_type sz;
  try {
    mValue = std::stof(this->mToken.getValue(), &sz);
    const std::string &sub = this->mToken.getValue().substr(sz);
    if (sub.empty()) {
      mSuffix = Suffix::None;
    } else if (sub.size() == 1) {
      if (sub[0] == 'f' || sub[0] == 'F') {
        mSuffix = Suffix::F;
      } else if (sub[0] == 'l' || sub[0] == 'L') {
        mSuffix = Suffix::L;
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
  //TODO an identifier declared to be a function parameter do not have linkage
  //TODO A declaration of a parameter as ''array of type'' shall be adjusted to ''qualified pointer to type'',
  // where the type qualifiers (if any) are those specified within the [ and ] of the array type derivation.
  //TODO A declaration of a parameter as ''function returning type'' shall be adjusted to ''pointer to function returning type''
  //TODO The special case of an unnamed parameter of type void as the only item in the list specifies that the function has no parameters

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
EnumeratorListAST::EnumeratorListAST(nts<EnumeratorAST>
                                     enumerator)
    : AST(AST::Kind::ENUMERATOR_LIST), enumerators(std::move(enumerator)) {}
void EnumeratorListAST::print(int indent) {
  AST::print(indent);
  enumerators.print(++indent);
}
EnumeratorAST::EnumeratorAST(nt<IdentifierAST>
                             id) : AST(AST::Kind::ENUMERATOR, 0), id(std::move(id)) {}
EnumeratorAST::EnumeratorAST(nt<IdentifierAST>
                             id, nt<ConstantExpressionAST>
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
  for (const auto &argument : mArgumentList) {
    argument->print(indent);
  }
}
ArgumentExpressionList::ArgumentExpressionList(nts<AssignmentExpressionAST>
                                               argumentList)
    : AST(AST::Kind::ARGUMENT_EXPRESSION_LIST), mArgumentList(std::move(argumentList)) {}
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
const ObjectType *TypeSpecifiersAST::codegen() {
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
    case CombinationKind::kEnum:break;
    case CombinationKind::kTypeName:break;
  }
  return type;
}
DirectDeclaratorAST::DirectDeclaratorAST() : AST(Kind::DIRECT_DECLARATOR) {}
SimpleDirectDeclaratorAST::SimpleDirectDeclaratorAST(nt<IdentifierAST> identifier)
    : identifier(std::move(identifier)) {}
void SimpleDirectDeclaratorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  identifier->print(indent);
}
const Token &SimpleDirectDeclaratorAST::getIdentifier() {
  return identifier->token;
}
const QualifiedType SimpleDirectDeclaratorAST::codegen(Terminal<StorageSpecifier> storageSpecifier,
                                                       const QualifiedType &derivedType,
                                                       bool isStruct) {
  Linkage linkage = Linkage::kNone;
  ISymbol *priorDeclartion = sObjectTable->lookup(identifier->token);

  switch (storageSpecifier.type) {
    case StorageSpecifier::kTYPEDEF: {

      return derivedType;
    }
    case StorageSpecifier::kREGISTER:break;
    case StorageSpecifier::kAUTO:break;
    case StorageSpecifier::kSTATIC: {
      if (sObjectTable->getScopeKind() == ScopeKind::FILE) {
        linkage = Linkage::kInternal;
      }
      if (sObjectTable->getScopeKind() == ScopeKind::BLOCK
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
      if (priorDeclartion) {
        auto *priorObject = dynamic_cast<ObjectSymbol *>(priorDeclartion);
        if (!priorObject
            || priorObject->getQualifiers() != derivedType.getQualifiers()
            || priorObject->getType() != derivedType.getType()) {
          throw SemaException(std::string("redefine ") + identifier->token.getValue(), involvedTokens());
        }

        if (priorDeclartion->getLinkage() != Linkage::kNone) {
          // since there's a prior declaration, do nothing
          return derivedType;
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
  llvm::Value *value = nullptr;
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
        llvm::IRBuilder<> builder(sObjectTable->getBasicBlock());
        value = builder.CreateAlloca(objectType->getLLVMType(sModule), nullptr, identifier->token.getValue());
        break;
      }
      case ScopeKind::TAG:throw std::runtime_error("WTF: how could a object type declared in a tag symbol");
      case ScopeKind::FUNCTION_PROTOTYPE:
        // we don't add symbol here, we add them in function declaration
        return derivedType;
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
      case ScopeKind::TAG:
      case ScopeKind::FUNCTION_PROTOTYPE:
        throw std::runtime_error("WTF: how could a function type declared in a tag/proto symbol");
    }
  }
  mSymbol = std::make_unique<ObjectSymbol>(derivedType,
                                           value,
                                           involvedTokens());
  mSymbol->setLinkage(linkage);
  if (isStruct) {

  } else {
    sObjectTable->insert(identifier->token, mSymbol.get());
  }
  return derivedType;
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
const Token &ParenthesedDirectDeclaratorAST::getIdentifier() {
  return declarator->getIdentifier();
}
bool ParenthesedDirectDeclaratorAST::isAbstract() const {
  return declarator->isAbstract();
}
const QualifiedType ParenthesedDirectDeclaratorAST::codegen(Terminal<StorageSpecifier> storageSpecifier,
                                                            const QualifiedType &derivedType,
                                                            bool isStruct) {
  return declarator->codegen(storageSpecifier, derivedType, isStruct);
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
const Token &ArrayDeclaratorAST::getIdentifier() {
  return directDeclarator->getIdentifier();
}
const QualifiedType ArrayDeclaratorAST::codegen(Terminal<StorageSpecifier> storageSpecifier,
                                                const QualifiedType &derivedType,
                                                bool isStruct) {
  // 6.7.6.2 Array declarators
  // BNF wrote by Brian W. Kernighan and Dennis M. Ritchie,Prentice Hall, 1988 do not support variable length array
  int64_t size = 0;
  if (constantExpression) {
    auto value = constantExpression->codegen();
    if (!dynamic_cast<const IntegerType *>(value.type)) {
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
                                   QualifiedType(mArrayType.get(), std::set<TypeQualifier>{}),
                                   isStruct);
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
const Token &FunctionDeclaratorAST::getIdentifier() {
  return directDeclarator->getIdentifier();
}
const QualifiedType FunctionDeclaratorAST::codegen(Terminal<StorageSpecifier> storageSpecifier,
                                                   const QualifiedType &derivedType,
                                                   bool isStruct) {
  if (dynamic_cast<const FunctionType *>(derivedType.getType())
      || dynamic_cast<const ArrayType *>(derivedType.getType())) {
    throw SemaException(std::string("function ") + getIdentifier().getValue() + "cannot be array type or function type",
                        involvedTokens());
  }
  mFunctionType = std::make_unique<FunctionType>(derivedType, parameterList->codegen());
  QualifiedType qualifiedType(mFunctionType.get(), {});
  return directDeclarator->codegen(storageSpecifier, qualifiedType, isStruct);
}
