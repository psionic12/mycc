#include <sema/ast.h>
#include <iostream>
#include <sema/symbol_tables.h>
#include "llvm/IR/Constants.h"
#include <llvm/IR/IRBuilder.h>
#include <sema/statement_context.h>
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
    : AST(AST::Kind::TRANSLATION_UNIT), mExternalDeclarations(std::move(external_declarations)),
      mObjectTable(objectTable), mTagTable(tagTable) {}
void TranslationUnitAST::print(int indent) {
  AST::print(indent);
  mExternalDeclarations.print(++indent);
}
void TranslationUnitAST::codegen() {
  sObjectTable = &mObjectTable;
  sTagTable = &mTagTable;
  // create global initialization function
  // c do not have global constructors for initiate global variables, the reason why we create one is because
  // we don't know if an initializer is a constant or not before we evaluate it,
  // if there's no BB for global variable to evaluate, the IRBuilder will crash.

  // there's other way to the check, I just want use this way in case global variable initiation is needed;
  llvm::FunctionType *globalVarInit_t = llvm::FunctionType::get(
      VoidType::sVoidType.getLLVMType(), {}, false);

  auto *globalVarInit = llvm::Function::Create(globalVarInit_t,
                                               llvm::GlobalVariable::LinkageTypes::InternalLinkage,
                                               "globalVarInit", sModule.get());
  // we do not create llvm.global_ctors cause we don't really use globalVarInit;

  auto *globalVarInitBB = llvm::BasicBlock::Create(getContext(), "globalVarInitBB", globalVarInit);
  sBuilder.SetInsertPoint(globalVarInitBB);
  for (const auto &ds : mExternalDeclarations) {
    ds->codegen();
  }
  if (!globalVarInitBB->empty()) {
//    throw std::runtime_error("WTF: globalVarInitBB is not empty");
  } else {
    globalVarInit->eraseFromParent();
  }
}
ExternalDeclarationAST::ExternalDeclarationAST(nt<AST> def)
    : AST(AST::Kind::EXTERNAL_DECLARATION), mDef(std::move(def)) {}
void ExternalDeclarationAST::print(int indent) {
  AST::print(indent);
  mDef->print(++indent);
}
void ExternalDeclarationAST::codegen() {
  if (mDef->getKind() == AST::Kind::FUNCTION_DEFINITION) {
    static_cast<FunctionDefinitionAST *>(mDef.get())->codegen();
  } else {
    static_cast<DeclarationAST *>(mDef.get())->codegen();
  }
}
FunctionDefinitionAST::FunctionDefinitionAST(nt<DeclarationSpecifiersAST> declaration_spcifiers,
                                             nt<DeclaratorAST> declarator,
                                             nts<DeclarationAST> declarations,
                                             nt<CompoundStatementAST> compound_statement,
                                             SymbolTable &labelTable)
    : AST(AST::Kind::FUNCTION_DEFINITION),
      mDeclarationSpcifiers(std::move(declaration_spcifiers)),
      mDeclarator(std::move(declarator)),
      mDeclarations(std::move(declarations)),
      mCompoundStatement(std::move(compound_statement)),
      mLabelTable(labelTable) {}
void FunctionDefinitionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mDeclarationSpcifiers->print(indent);
  mDeclarator->print(indent);
  mDeclarations.print(indent);
  mCompoundStatement->print(indent);
}
llvm::Value *FunctionDefinitionAST::codegen() {
  sLabelTable = &mLabelTable;
  // set parameter table to compound statements table as father
  if (auto *functionDeclarator = dynamic_cast<FunctionDeclaratorAST *>(this->mDeclarator->mDirectDeclarator.get())) {
    if (functionDeclarator->mParameterList) {
      auto &table = functionDeclarator->mParameterList->mObjectTable;
      table.setFather(sObjectTable);
      this->mCompoundStatement->mObjectTable.setFather(&table);
    }
  }
  for (const auto &specifier : mDeclarationSpcifiers->mStorageSpecifiers) {
    if (specifier.type != StorageSpecifier::kEXTERN && specifier.type != StorageSpecifier::kSTATIC) {
      throw SemaException("declaration specifiers shall be either extern or static", specifier.token);
    }
  }
  auto pair = mDeclarationSpcifiers->codegen();
  auto *symbol = mDeclarator->codegen(pair.first, pair.second);
  if (auto *functionSymbol = dynamic_cast<FunctionSymbol *>(symbol)) {
    if (auto *theFunction = llvm::dyn_cast<llvm::Function>(functionSymbol->getValue())) {
      if (theFunction->empty()) {
        StatementContexts contexts(theFunction);
        auto *functionTy = functionSymbol->getType();
        auto *BB = llvm::BasicBlock::Create(getContext(), "", theFunction);
        auto *returnBB = llvm::BasicBlock::Create(getContext());
        sBuilder.SetInsertPoint(BB);
        llvm::AllocaInst *returnAlloca = nullptr;
        if (functionTy->getReturnType().getType() != &VoidType::sVoidType) {
          returnAlloca = sBuilder.CreateAlloca(theFunction->getReturnType());
        }
        bool hasReturn = false;
        contexts.add(std::make_unique<FunctionContext>(functionTy, theFunction, returnAlloca, hasReturn, returnBB));
        // what codes below doing:
        // arguments in llvm is not pointer, which means if you get argument by name, and using it to some address opreration,
        // llvm will assert errors. But in nomal language, arguments are identifiers, which is a pointer in low level, so in
        // order to do the address operation, we need save the value in a memery and save the memory address to the
        // identifier(symbol table);
        auto it = theFunction->arg_begin();
        while (it != theFunction->arg_end()) {
          const auto &name = it->getName();
          auto *symbol = dynamic_cast<ObjectSymbol *>(mCompoundStatement->mObjectTable.lookup(name));
          if (!symbol) {
            throw std::runtime_error("WTF: cannot find declared parameter");
          }
          auto *alloc = sBuilder.CreateAlloca(it->getType());
          //TODO any chance I need to check is_volatile?
          sBuilder.CreateStore(it, alloc);
          symbol->setValue(alloc);
          ++it;
        }
        mCompoundStatement->codegen(contexts);
        if (!sBuilder.GetInsertBlock()->getTerminator()) {
          sBuilder.CreateBr(returnBB);
        }
        theFunction->getBasicBlockList().push_back(returnBB);
        sBuilder.SetInsertPoint(returnBB);
        if (!returnAlloca) {
          sBuilder.CreateRetVoid();
        } else if (hasReturn) {
          sBuilder.CreateRet(sBuilder.CreateLoad(returnAlloca));
        } else {
          throw SemaException("function missing return statement", *mRightMost);
        }
      } else {
        throw SemaException("Function cannot redefined", mDeclarator->involvedTokens());
      }
    } else {
      throw std::runtime_error("WTF: function definition is not a function type");
    }
  } else {
    throw std::runtime_error("WTF: function definition is not a function type");
  }
  for (auto &pair : *sLabelTable) {
    if (auto *labelSymbol = dynamic_cast<LabelSymbol *> (pair.second)) {
      if (!labelSymbol->isImplemented()) {
        throw SemaException("Label not implement", *labelSymbol->getToken());
      }
    }
  }
  sLabelTable = nullptr;
  //TODO After adjustment, the parameters in a parameter type list in a function declarator that is part of a definition of that function shall not have incomplete type.
  return nullptr;
}
void CompoundStatementAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mASTs.print(indent);
}
void CompoundStatementAST::codegen(StatementContexts &contexts) {
  SymbolScope s1(sObjectTable, &mObjectTable);
  SymbolScope s2(sTagTable, &mTagTable);
  for (auto &ast : mASTs) {
    if (auto *decl = dynamic_cast<DeclarationAST *>(ast.get())) {
      decl->codegen();
    } else if (auto *statement = dynamic_cast<StatementAST *>(ast.get())) {
      contexts.add(std::make_unique<CodeBlockContext>());
      statement->codegen(contexts);
    }
  }
}
CompoundStatementAST::CompoundStatementAST(nts<AST>
                                           asts, SymbolTable &objectTable, SymbolTable &tagTable)
    : StatementAST(AST::Kind::COMPOUND_STATEMENT),
      mASTs(std::move(asts)), mObjectTable(objectTable), mTagTable(tagTable) {}
TypedefSymbol DeclarationAST::sFakeTypedef(nullptr);
DeclarationAST::DeclarationAST(nt<DeclarationSpecifiersAST>
                               declaration_specifiers,
                               InitDeclarators
                               init_declarators,
                               SymbolTable &table)
    : AST(AST::Kind::DECLARATION),
      mDeclarationSpecifiers(std::move(declaration_specifiers)),
      mInitDeclarators(std::move(init_declarators)) {
  for (auto &specifiers : this->mDeclarationSpecifiers->mStorageSpecifiers) {
    if (specifiers.type == StorageSpecifier::kTYPEDEF) {
      for (const auto &pair : this->mInitDeclarators) {
        const DeclaratorAST *ast = pair.first.get();
        table.insert(*ast->getIdentifier(), &sFakeTypedef);
      }
      break;
    }
  }
}
void DeclarationAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mDeclarationSpecifiers->print(indent);
  for (const auto &ds : mInitDeclarators) {
    ds.first->print(indent);
    if (ds.second) ds.second->print(indent);
  }
}
void DeclarationAST::codegen() {
  auto pair = mDeclarationSpecifiers->codegen();
  const auto &storageSpecifier = pair.first;
  auto &qualifiers = pair.second;
  for (auto &initPair : mInitDeclarators) {
    ISymbol *symbol = initPair.first->codegen(storageSpecifier, qualifiers);
    if (auto *objSymbol = dynamic_cast<ObjectSymbol *>(symbol)) {
      auto *type = objSymbol->getQualifiedType().getType();
      auto *value = objSymbol->getValue();
      if (initPair.second.get()) {
        if (auto *objtype = dynamic_cast<ObjectType *>(type)) {
          bool complete = objtype->complete();
          sIsInitializerList = true;
          auto initValue = objtype->initializerCodegen(initPair.second.get());
          sIsInitializerList = false;
          // if the initializer is an incomplete array type, the declaration is not imcomplete any more
          if (dynamic_cast<ArrayType *>(objtype) && !complete) {
            if (auto *gVar = llvm::dyn_cast<llvm::GlobalVariable>(value)) {
              //gVar->mutateType() not work, re-create the global variable
              auto linkage = gVar->getLinkage();
              gVar->eraseFromParent();
              gVar = new llvm::GlobalVariable(*getModule(),
                                              objtype->getLLVMType(),
                                              objSymbol->getQualifiedType().isConst(),
                                              linkage,
                                              nullptr);
              objSymbol->setValue(gVar);
              value = gVar;
            } else {
              throw std::runtime_error("WTF: array type has non-global value");
            }
          }
          Value lhs = Value(objSymbol->getQualifiedType(), true, value);
          AssignmentExpressionAST::eqCodegen(lhs,
                                             initValue,
                                             initPair.first.get(),
                                             initPair.second.get(),
                                             true);
        } else {
          throw SemaException("only object types can be initialized", initPair.first->involvedTokens());
        }
      }
    } else if (initPair.second != nullptr) {
      throw SemaException("not assignable: ", initPair.first->involvedTokens());
    }

  }
  bool hasDeclarator = !mInitDeclarators.empty();
  bool hasTag = false;
  bool hasEnumMembers = false;
  // check definations in codegen;

  if (!hasDeclarator && !hasTag && !hasEnumMembers) {
    throw SemaException("A declaration shall declare at least a declarator, a tag, or the members of an enumeration",
                        *mLeftMost);
  }
  //TODO 6.7.3
}
DeclarationSpecifiersAST::DeclarationSpecifiersAST(ts<StorageSpecifier>
                                                   storage_specifiers,
                                                   nt<TypeSpecifiersAST>
                                                   type_specifiers,
                                                   nts<TypeQualifierAST>
                                                   type_qualifiers)
    : AST(AST::Kind::DECLARATION_SPECIFIER),
      mStorageSpecifiers(std::move(storage_specifiers)),
      mTypeSpecifiers(std::move(type_specifiers)),
      mTypeQualifiers(std::move(type_qualifiers)) {}
void DeclarationSpecifiersAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mStorageSpecifiers.print(indent);
  mTypeQualifiers.print(indent);
  mTypeSpecifiers->print(indent);
}
bool DeclarationSpecifiersAST::empty() {
  return mStorageSpecifiers.empty() && mTypeQualifiers.empty() && mTypeSpecifiers->empty();
}
std::pair<StorageSpecifier, QualifiedType> DeclarationSpecifiersAST::codegen() {
  if (mStorageSpecifiers.size() > 1) {
    throw SemaException(
        "At most, one storage-class specifier may be given in the declaration specifiers in a declaration",
        mStorageSpecifiers.back().token);
  }
  StorageSpecifier storageSpecifier;
  if (mStorageSpecifiers.empty()) {
    storageSpecifier = StorageSpecifier::kNone;
  } else {
    storageSpecifier = mStorageSpecifiers.back().type;
  }
  std::set<TypeQualifier> qualifiers;
  for (const auto &qualifier : mTypeQualifiers) {
    qualifiers.emplace(qualifier->mOp.type);
  }
  //TODO If an aggregate or union object is declared with a storage-class specifier other than typedef, the properties resulting from the storage-class specifier, except with respect to linkage, also apply to the members of the object, and so on recursively for any aggregate or union member objects.
  //TODO At least one type specifier shall be given in the declaration specifiers in each declaration,  and in the specifier-qualifier list in each struct declaration and type name.
  QualifiedType qualifiedType = mTypeSpecifiers->codegen();
  qualifiedType.addQualifiers(qualifiers);
  return {storageSpecifier, qualifiedType};
}
DeclaratorAST::DeclaratorAST(nt<PointerAST>
                             pointer,
                             nt<DirectDeclaratorAST>
                             direct_declarator)
    : AST(AST::Kind::DECLARATOR), mPointer(std::move(pointer)), mDirectDeclarator(std::move(direct_declarator)) {}
void DeclaratorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  if (mPointer) mPointer->print(indent);
  if (mDirectDeclarator) mDirectDeclarator->print(indent);
}
const Token *DeclaratorAST::getIdentifier() const {
  return mDirectDeclarator->getIdentifier();
}
ISymbol *DeclaratorAST::codegen(StorageSpecifier storageSpecifier, QualifiedType derivedType) {
  return mDirectDeclarator->codegen(storageSpecifier, mPointer ? mPointer->codegen(derivedType) : derivedType);
}
StorageClassSpecifierAST::StorageClassSpecifierAST(Terminal<StorageSpecifier>
                                                   storage_speicifier)
    : AST(AST::Kind::STORAGE_CLASS_SPECIFIER), mStorageSpeicifier(storage_speicifier) {}
void StorageClassSpecifierAST::print(int indent) {
  AST::print(indent);
  mStorageSpeicifier.print(++indent);
}
void SpecifierQualifierAST::print(int indent) {
  AST::print(indent);
  ++indent;
  if (mTypes) mTypes->print(indent);
  mQualifiers.print(indent);
}
SpecifierQualifierAST::SpecifierQualifierAST(nt<TypeSpecifiersAST>
                                             types, nts<TypeQualifierAST>
                                             qualifiers)
    : AST(AST::Kind::SPECIFIER_QUALIFIER), mTypes(std::move(types)), mQualifiers(std::move(qualifiers)) {

}
QualifiedType SpecifierQualifierAST::codegen() {
  QualifiedType qualifiedType = mTypes->codegen();
  for (auto &qualifier : mQualifiers) {
    qualifiedType.addQualifier(qualifier->mOp.type);
  }
  return qualifiedType;
}
ProtoTypeSpecifierAST::ProtoTypeSpecifierAST(Terminal<ProtoTypeSpecifierOp> specifier)
    : AST(AST::Kind::PROTO_TYPE_SPECIFIER), Terminal<ProtoTypeSpecifierOp>(specifier), mSpecifier(specifier) {}
void ProtoTypeSpecifierAST::print(int indent) {
  AST::print(indent);
  mSpecifier.print(++indent);
}
TypeSpecifierAST::TypeSpecifierAST(Terminal<ProtoTypeSpecifierOp>
                                   specifier)
    : AST(AST::Kind::TYPE_SPECIFIER, static_cast<int>(specifier.type)),
      mSpecifier(std::make_unique<ProtoTypeSpecifierAST>(specifier)) {}
TypeSpecifierAST::TypeSpecifierAST(nt<StructOrUnionSpecifierAST>
                                   specifier)
    : AST(AST::Kind::TYPE_SPECIFIER, 9), mSpecifier(std::move(specifier)) {}
TypeSpecifierAST::TypeSpecifierAST(nt<EnumSpecifierAST>
                                   specifier)
    : AST(AST::Kind::TYPE_SPECIFIER, 10), mSpecifier(std::move(specifier)) {}
TypeSpecifierAST::TypeSpecifierAST(nt<TypedefNameAST>
                                   specifier)
    : AST(AST::Kind::TYPE_SPECIFIER, 11), mSpecifier(std::move(specifier)) {}
void TypeSpecifierAST::print(int indent) {
  AST::print(indent);
  mSpecifier->print(++indent);
}
StructOrUnionSpecifierAST::StructOrUnionSpecifierAST(StructOrUnion
                                                     type,
                                                     nt<IdentifierAST>
                                                     id,
                                                     nts<StructDeclarationAST>
                                                     declarations)
    : AST(AST::Kind::STRUCT_OR_UNION_SPECIFIER),
      mIsStruct(type),
      mIdentifier(std::move(id)),
      mDeclarations(std::move(declarations)) {}
void StructOrUnionSpecifierAST::print(int indent) {
  AST::print(indent);
  ++indent;
  AST::printIndent(indent);
  mIsStruct == StructOrUnion::kSTRUCT ? std::cout << "STRUCT" << std::endl : std::cout << "UNION" << std::endl;
  if (mIdentifier != nullptr) mIdentifier->print(indent);
  mDeclarations.print(indent);
}
ObjectType *StructOrUnionSpecifierAST::codegen() {
  //TODO A struct-declaration that does not declare an anonymous structure or anonymous union shall contain a struct-declarator-list.
  const auto &token = mIdentifier->getToken();
  auto *decl = dynamic_cast<TagSymbol *>(sTagTable->lookup(token));
  ObjectType *tagType = decl ? decl->getTagType() : nullptr;
  if (!tagType || !tagType->complete()) {
    std::unique_ptr<ObjectType> structTy;
    if (!tagType) {
      if (mIsStruct == StructOrUnion::kSTRUCT) {
        structTy = std::make_unique<StructType>(token.toString());
      } else {
        structTy = std::make_unique<UnionType>(token.toString());
      }
      mSymbol = std::make_unique<TagSymbol>(std::move(structTy), mIdentifier ? &token : nullptr);
      sTagTable->insert(token, mSymbol.get());
      tagType = mSymbol->getTagType();
    }
    SymbolTable table(ScopeKind::TAG);
    {
      SymbolScope s(sObjectTable, &table);
      int index = 0;
      for (const auto &declaration :mDeclarations) {
        auto d = declaration->codegen();
        for (auto *symbol : d) {
          if (const Token *token = symbol->getToken()) {
            symbol->setIndex(index);
            ++index;
          } else {
            throw std::runtime_error("WTF: member has to got a name");
          }
        }
      }
    }
    dynamic_cast<CompoundType *>(tagType)->setBody(std::move(table));
  }
  return tagType;
}
EnumSpecifierAST::EnumSpecifierAST(nt<IdentifierAST>
                                   identifier,
                                   nt<EnumeratorListAST>
                                   enumeratorList)
    : AST(AST::Kind::ENUM_SPECIFIER), mIdentifier(std::move(identifier)), mEnumList(std::move(enumeratorList)) {}
EnumSpecifierAST::EnumSpecifierAST(nt<EnumeratorListAST>
                                   enumeratorList)
    : AST(AST::Kind::ENUM_SPECIFIER), mIdentifier(nullptr), mEnumList(std::move(enumeratorList)) {}
EnumSpecifierAST::EnumSpecifierAST(nt<IdentifierAST>
                                   identifier)
    : AST(AST::Kind::ENUM_SPECIFIER), mIdentifier(std::move(identifier)), mEnumList() {}
void EnumSpecifierAST::print(int indent) {
  AST::print(indent);
  ++indent;
  if (mIdentifier) mIdentifier->print(indent);
  if (mEnumList) mEnumList->print(indent);
}
EnumerationType *EnumSpecifierAST::codegen() {
  const Token *token = mIdentifier ? &mIdentifier->getToken() : nullptr;
  mSymbol = std::make_unique<TagSymbol>(std::make_unique<EnumerationType>(), token);
  SymbolTable table(ScopeKind::TAG);
  auto *enumType = static_cast< EnumerationType *>(static_cast<TagSymbol *>(mSymbol.get())->getTagType());
  if (mEnumList) {
    for (auto *symbol : mEnumList->codegen(enumType)) {
      table.insert(*symbol->getToken(), symbol);
      sObjectTable->insert(*symbol->getToken(), symbol);
    }
    dynamic_cast<CompoundType *>(mSymbol->getTagType())->setBody(std::move(table));
  }
  return static_cast<EnumerationType *> (mSymbol->getTagType());
}
StructDeclarationAST::StructDeclarationAST(nt<SpecifierQualifierAST>
                                           specifier_qualifier,
                                           nt<StructDeclaratorListAST>
                                           struct_declarator_list)
    : AST(AST::Kind::STRUCT_DECLARATION),
      mSpecifierQualifier(std::move(specifier_qualifier)),
      mStructDeclaratorList(std::move(struct_declarator_list)) {}
void StructDeclarationAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mSpecifierQualifier->print(indent);
  mStructDeclaratorList->print(indent);
}
std::vector<ObjectSymbol *> StructDeclarationAST::codegen() {
  auto qualifiedType = mSpecifierQualifier->codegen();
  std::vector<ObjectSymbol *> symbols;
  for (auto &ast : mStructDeclaratorList->mStructDeclarators) {
    ISymbol *symbol = ast->codegen(qualifiedType);
    if (auto *obj = dynamic_cast<ObjectSymbol *>(symbol)) {
      if (auto *type = dynamic_cast<ObjectType *>(obj->getQualifiedType().getType())) {
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
StructDeclaratorListAST::StructDeclaratorListAST(nts<StructDeclaratorAST>
                                                 struct_declarators) : AST(
    AST::Kind::STRUCT_DECLARATOR_LIST), mStructDeclarators(std::move(struct_declarators)) {}
void StructDeclaratorListAST::print(int indent) {
  AST::print(indent);
  mStructDeclarators.print(++indent);
}
StructDeclaratorAST::StructDeclaratorAST(nt<DeclaratorAST>
                                         declarator)
    : AST(AST::Kind::STRUCT_DECLARATOR, 0),
      mDeclarator(std::move(declarator)),
      mConstantExpression(nullptr) {}
StructDeclaratorAST::StructDeclaratorAST(nt<DeclaratorAST>
                                         declarator,
                                         nt<ConstantExpressionAST>
                                         constant_expression)
    : AST(AST::Kind::STRUCT_DECLARATOR, 1),
      mDeclarator(std::move(declarator)),
      mConstantExpression(nullptr) {}
StructDeclaratorAST::StructDeclaratorAST(nt<ConstantExpressionAST>
                                         constant_expression)
    : AST(AST::Kind::STRUCT_DECLARATOR, 2),
      mDeclarator(nullptr),
      mConstantExpression(std::move(constant_expression)) {}
void StructDeclaratorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  if (mDeclarator != nullptr) mDeclarator->print(indent);
  if (mConstantExpression != nullptr) mConstantExpression->print(indent);
}
ISymbol *StructDeclaratorAST::codegen(const QualifiedType &derivedType) {
  return mDeclarator->codegen(StorageSpecifier::kNone, derivedType);
  //TODO bit fields
}

ConstantExpressionAST::ConstantExpressionAST(nt<ConditionalExpressionAST>
                                             conditional_expression)
    : IExpression(AST::Kind::CONSTANT_EXPRESSION), mConditionalExpression(std::move(conditional_expression)) {}
void ConstantExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mConditionalExpression->print(indent);
}
Value ConstantExpressionAST::codegen() {
  Value result = mConditionalExpression->codegen();
  if (!result.isConatantData()) {
    throw SemaException("cannot envalue on compile time", mConditionalExpression->involvedTokens());
  }
  return result;
}
PointerAST::PointerAST(nts<TypeQualifierAST>
                       type_qualifiers, nt<PointerAST>
                       pointer)
    : AST(AST::Kind::POINTER), mTypeQualifiers(std::move(type_qualifiers)), mPointer(std::move(pointer)) {}
void PointerAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mTypeQualifiers.print(indent);
  if (mPointer) mPointer->print(indent);
}
QualifiedType PointerAST::codegen(QualifiedType derivedType) {
  std::set<TypeQualifier> qualifiers;
  for (auto &qualifier : mTypeQualifiers) {
    qualifiers.insert(qualifier->codegen());
  }
  if (mPointer) {
    mPointerType = std::make_unique<PointerType>(mPointer->codegen(derivedType));
  } else {
    mPointerType = std::make_unique<PointerType>(derivedType);
  }
  return QualifiedType(mPointerType.get(), std::move(qualifiers));
}
ConditionalExpressionAST::ConditionalExpressionAST(nt<IBinaryOperationAST>
                                                   logical_or_expression)
    : IExpression(AST::Kind::CONDITIONAL_EXPRESSION),
      mLogicalOrExpression(std::move(logical_or_expression)) {}
ConditionalExpressionAST::ConditionalExpressionAST(nt<IBinaryOperationAST>
                                                   logical_or_expression,
                                                   nt<ExpressionAST>
                                                   expression,
                                                   nt<ConditionalExpressionAST>
                                                   conditional_expression)
    : IExpression(AST::Kind::CONDITIONAL_EXPRESSION),
      mLogicalOrExpression(std::move(logical_or_expression)),
      mExpression(std::move(expression)),
      mConditionalExpression(std::move(
          conditional_expression)) {}
void ConditionalExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mLogicalOrExpression->print(indent);
  if (mExpression) {
    mExpression->print(indent);
    mConditionalExpression->print(indent);
  }
}
Value ConditionalExpressionAST::codegen() {
  auto condValue = mLogicalOrExpression->codegen();
  auto *condType = dynamic_cast<ScalarType *> (condValue.getType());
  if (mExpression) {
    if (!condType) {
      throw SemaException("The first operand shall have scalar type.", mLogicalOrExpression->involvedTokens());
    }
    llvm::Value *cond;
    auto *currentFunction = sBuilder.GetInsertBlock()->getParent();
    auto *trueBlock = llvm::BasicBlock::Create(getContext(), "", currentFunction);
    auto *falseBlock = llvm::BasicBlock::Create(getContext());
    auto *endBlock = llvm::BasicBlock::Create(getContext());

    Type *type;

    if (auto *ltype = dynamic_cast<IntegerType *> (condValue.getType())) {
      auto *const0 = llvm::ConstantInt::get(ltype->getLLVMType(), 0);
      cond = sBuilder.CreateICmpNE(condValue.getValue(), const0);
    } else if (auto *ltype = dynamic_cast<FloatingType *> (condValue.getType())) {
      auto *const0 = llvm::ConstantFP::get(ltype->getLLVMType(), 0.0);
      cond = sBuilder.CreateFCmpONE(condValue.getValue(), const0);
    } else if (auto *ltype = dynamic_cast<PointerType *> (condValue.getType())) {
      auto *const0 = llvm::ConstantPointerNull::get(ltype->getLLVMType());
      cond = sBuilder.CreateICmpNE(condValue.getValue(), const0);
    } else {
      throw std::runtime_error("WTF: other than integer, float, pointer");
    }
    sBuilder.CreateCondBr(cond, trueBlock, falseBlock);
    // true block
    sBuilder.SetInsertPoint(trueBlock);
    auto exp1 = mExpression->codegen();
    llvm::Value *trueValue = exp1.getValue();

    // false block
    currentFunction->getBasicBlockList().push_back(falseBlock);
    sBuilder.SetInsertPoint(falseBlock);
    auto exp2 = mConditionalExpression->codegen();
    llvm::Value *falseValue = exp2.getValue();

    Type *lType = exp1.getType();
    Type *rType = exp2.getType();
    if (dynamic_cast< ArithmeticType *>(lType)
        && dynamic_cast< ArithmeticType *>(rType)) {
      if (BinaryOperatorAST::UsualArithmeticConversions(lType, rType, this)) {
        sBuilder.SetInsertPoint(falseBlock);
        falseValue = rType->castTo(lType, falseValue, this);
        type = lType;
      } else {
        sBuilder.SetInsertPoint(trueBlock);
        trueValue = lType->castTo(rType, trueValue, this);
        type = rType;
      }
    } else if ((dynamic_cast< StructType *>(lType)
        || dynamic_cast< UnionType *>(lType))
        && lType == rType) {
      type = lType;
    } else if (lType == &VoidType::sVoidType
        && rType == &VoidType::sVoidType) {
      type = &VoidType::sVoidType;
    } else if (dynamic_cast< PointerType *>(lType)
        && dynamic_cast< PointerType *>(rType)) {
      if (lType->compatible(rType)) {
        type = lType;
      } else {
        auto *p1 = static_cast<PointerType *>(lType);
        auto *p2 = static_cast<PointerType *>(rType);
        if (p1->getReferencedQualifiedType().getType() == &VoidType::sVoidType
            && dynamic_cast< ObjectType *> (p2->getReferencedQualifiedType().getType())) {
          type = p1;
        } else if (p1->getReferencedQualifiedType().getType() == &VoidType::sVoidType
            && dynamic_cast< ObjectType *> (p2->getReferencedQualifiedType().getType())) {
          type = p2;
        } else {
          throw SemaException(
              "one operand is a pointer to an object type and the other is a pointer to a qualified or unqualified version of void.",
              involvedTokens());
        }
      }
    } else {
      throw SemaException("incompatible operand types", involvedTokens());
    }
    sBuilder.SetInsertPoint(trueBlock);
    if (!sBuilder.GetInsertBlock()->getTerminator())
      sBuilder.CreateBr(endBlock);

    sBuilder.SetInsertPoint(falseBlock);
    if (!sBuilder.GetInsertBlock()->getTerminator())
      sBuilder.CreateBr(endBlock);

    // end block
    currentFunction->getBasicBlockList().push_back(endBlock);
    sBuilder.SetInsertPoint(endBlock);
    auto *phi = sBuilder.CreatePHI(type->getLLVMType(), 2);
    phi->addIncoming(trueValue, trueBlock);
    phi->addIncoming(falseValue, falseBlock);
    return Value(QualifiedType(type, {}), false, phi);
  } else {
    return condValue;
  }

}
void ExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  if (mExpression) {
    mExpression->print(indent);
  }
  mAssignmentExpression->print(indent);
}
Value ExpressionAST::codegen() {
  if (mExpression) {
    mExpression->codegen();
  }
  return mAssignmentExpression->codegen();
}
ExpressionAST::ExpressionAST(nt<ExpressionAST> expression, nt<AssignmentExpressionAST> assignment_expression)
    : IExpression(AST::Kind::EXPRESSION),
      mExpression(std::move(expression)),
      mAssignmentExpression(std::move(assignment_expression)) {}

TypeNameAST::TypeNameAST(nt<SpecifierQualifierAST>
                         specifier, nt<DeclaratorAST>
                         declarator)
    : AST(AST::Kind::TYPE_NAME), mSpecifiers(std::move(specifier)), mDeclarator(std::move(declarator)) {}
void TypeNameAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mSpecifiers->print(indent);
  if (mDeclarator) mDeclarator->print(indent);
}
QualifiedType TypeNameAST::codegen() {
  auto qualifiedType = mSpecifiers->codegen();
  if (mDeclarator) {
    auto *symbol = mDeclarator->codegen(StorageSpecifier::kNone, qualifiedType);
    if (auto *obj = dynamic_cast<ObjectSymbol *>(symbol)) {
      return obj->getQualifiedType();
    } else {
      throw std::runtime_error("WTF: type name is not an object symbol");
    }
  } else {
    return qualifiedType;
  }
}
AssignmentExpressionAST::AssignmentExpressionAST(nt<ConditionalExpressionAST>
                                                 conditional_expression)
    : IExpression(AST::Kind::ASSIGNMENT_EXPRESSION), mConditionalExpression(std::move(conditional_expression)) {}
AssignmentExpressionAST::AssignmentExpressionAST(nt<ConditionalExpressionAST>
                                                 conditional_expression,
                                                 Terminal<AssignmentOp>
                                                 op,
                                                 nt<AssignmentExpressionAST>
                                                 assignment_expression)
    : IExpression(AST::Kind::ASSIGNMENT_EXPRESSION), mConditionalExpression(std::move(conditional_expression)),
      mOp(std::make_unique<Terminal<AssignmentOp >>
              (op)),
      mAssignmentExpression(std::move(assignment_expression)) {}
void AssignmentExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mConditionalExpression->print(indent);
  if (getProduction() == 1) {
    mOp->print(indent);
    mAssignmentExpression->print(indent);
  }
}
Value AssignmentExpressionAST::codegen() {
  auto lhs = mConditionalExpression->codegen();
  if (!mOp) {
    return lhs;
  } else {
    if (!lhs.modifiable()) {
      throw SemaException("An assignment operator shall have a modifiable lvalue as its left operand.",
                          mConditionalExpression->involvedTokens());
    }
    llvm::Value *result;
    auto rhs = mAssignmentExpression->codegen();
    switch (mOp->type) {
      case AssignmentOp::STAREQ: {
        Value v = BinaryOperatorAST::codegen(lhs,
                                             rhs,
                                             InfixOp::STAR,
                                             mConditionalExpression.get(),
                                             mAssignmentExpression.get());

        result = eqCodegen(lhs, v, mConditionalExpression.get(), mAssignmentExpression.get(), false);
        break;
      }
      case AssignmentOp::SLASHEQ: {
        Value v = BinaryOperatorAST::codegen(lhs,
                                             rhs,
                                             InfixOp::SLASH,
                                             mConditionalExpression.get(),
                                             mAssignmentExpression.get());

        result = eqCodegen(lhs, v, mConditionalExpression.get(), mAssignmentExpression.get(), false);
        break;
      }
      case AssignmentOp::PERCENTEQ: {
        Value v = BinaryOperatorAST::codegen(lhs,
                                             rhs,
                                             InfixOp::PERCENT,
                                             mConditionalExpression.get(),
                                             mAssignmentExpression.get());

        result = eqCodegen(lhs, v, mConditionalExpression.get(), mAssignmentExpression.get(), false);
        break;
      }
      case AssignmentOp::PLUSEQ: {
        Value v = BinaryOperatorAST::codegen(lhs,
                                             rhs,
                                             InfixOp::PLUS,
                                             mConditionalExpression.get(),
                                             mAssignmentExpression.get());

        result = eqCodegen(lhs, v, mConditionalExpression.get(), mAssignmentExpression.get(), false);
        break;
      }
      case AssignmentOp::SUBEQ: {
        Value v = BinaryOperatorAST::codegen(lhs,
                                             rhs,
                                             InfixOp::SUB,
                                             mConditionalExpression.get(),
                                             mAssignmentExpression.get());

        result = eqCodegen(lhs, v, mConditionalExpression.get(), mAssignmentExpression.get(), false);
        break;
      }
      case AssignmentOp::LTLTEQ: {
        Value v = BinaryOperatorAST::codegen(lhs,
                                             rhs,
                                             InfixOp::LTLT,
                                             mConditionalExpression.get(),
                                             mAssignmentExpression.get());

        result = eqCodegen(lhs, v, mConditionalExpression.get(), mAssignmentExpression.get(), false);
        break;
      }
      case AssignmentOp::GTGTEQ: {
        Value v = BinaryOperatorAST::codegen(lhs,
                                             rhs,
                                             InfixOp::GTGT,
                                             mConditionalExpression.get(),
                                             mAssignmentExpression.get());

        result = eqCodegen(lhs, v, mConditionalExpression.get(), mAssignmentExpression.get(), false);
        break;
      }
      case AssignmentOp::AMPEQ: {
        Value v = BinaryOperatorAST::codegen(lhs,
                                             rhs,
                                             InfixOp::AMP,
                                             mConditionalExpression.get(),
                                             mAssignmentExpression.get());

        result = eqCodegen(lhs, v, mConditionalExpression.get(), mAssignmentExpression.get(), false);
        break;
      }
      case AssignmentOp::CARETEQ: {
        Value v = BinaryOperatorAST::codegen(lhs,
                                             rhs,
                                             InfixOp::CARET,
                                             mConditionalExpression.get(),
                                             mAssignmentExpression.get());

        result = eqCodegen(lhs, v, mConditionalExpression.get(), mAssignmentExpression.get(), false);
        break;
      }
      case AssignmentOp::BAREQ: {
        Value v = BinaryOperatorAST::codegen(lhs,
                                             rhs,
                                             InfixOp::BAR,
                                             mConditionalExpression.get(),
                                             mAssignmentExpression.get());

        result = eqCodegen(lhs, v, mConditionalExpression.get(), mAssignmentExpression.get(), false);
        break;
      }
      case AssignmentOp::EQ:
        result =
            eqCodegen(lhs, rhs, mConditionalExpression.get(), mAssignmentExpression.get(), false);
        break;
    }
    return Value(lhs.getQualifiedType(), false, result);
  }
}
llvm::Value *AssignmentExpressionAST::eqCodegen(Value &lhs,
                                                Value &rhs,
                                                AST *lhsAST,
                                                AST *rhsAST,
                                                bool isInitialization) {
  if (sObjectTable->getScopeKind() == ScopeKind::FILE) {
    if (!rhs.isConatantData()) {
      throw SemaException("initializer element is not a compile-time constant", rhsAST->involvedTokens());
    }
  }

  llvm::Value *rhsValue = rhs.getValue();
  if (dynamic_cast<ArithmeticType *> (lhs.getType())
      && dynamic_cast<ArithmeticType *> (rhs.getType())) {
    rhsValue = static_cast<ArithmeticType *>(rhs.getType())->castTo(lhs.getType(), rhsValue, rhsAST);
  } else if (dynamic_cast< StructType *>(lhs.getType())
      && lhs.getQualifiedType().compatible(rhs.getQualifiedType())) {
    rhsValue = rhs.getValue();
  } else if (dynamic_cast< PointerType *>(lhs.getType())
      && dynamic_cast< PointerType *>(rhs.getType())) {
    auto *p1 = static_cast<PointerType *>(lhs.getType());
    auto *p2 = static_cast<PointerType *>(rhs.getType());
    if (!p1->getReferencedQualifiedType().isSub(p2->getReferencedQualifiedType().getQualifiers())) {
      throw SemaException(
          "qualifiers of the left operand do not contains all the qualifiers of the right operand",
          lhsAST->involvedTokens());
    }
    if (p1->getReferencedQualifiedType().getType()->compatible(p2->getReferencedQualifiedType().getType())) {

    } else if ((dynamic_cast< ObjectType *>(p1->getReferencedQualifiedType().getType())
        && p2->getReferencedQualifiedType().getType() == &VoidType::sVoidType) ||
        (dynamic_cast< ObjectType *>(p2->getReferencedQualifiedType().getType())
            && p1->getReferencedQualifiedType().getType() == &VoidType::sVoidType)) {
      rhsValue = p2->castTo(p1->getReferencedQualifiedType().getType(), rhsValue, rhsAST);
    } else {
      throw SemaException("pointers are not compatible", lhsAST->involvedTokens());
    }
  } else if (dynamic_cast<ArrayType *> (lhs.getType())
      && dynamic_cast<ArrayType *> (rhs.getType())) {
    rhsValue = rhs.getValue();
  } else if (dynamic_cast<UnionType *>(lhs.getType()) && lhs.getQualifiedType().compatible(rhs.getQualifiedType())) {
    rhsValue = rhs.getValue();
  } else {
    throw SemaException("cannot assign", lhsAST->involvedTokens());
  }
  auto *constantInitValue = rhs.isConatantData();
  if (isInitialization && constantInitValue) {
    if (auto *global = llvm::dyn_cast<llvm::GlobalVariable>(lhs.getPtr())) {
      global->setInitializer(constantInitValue);
      return nullptr;
    } else {
//              sBuilder.CreateMemCpy(value,
//                                    initValue,
//                                    objtype->getSizeInBits() / 8,
//                                    0,
//                                    objSymbol->getQualifiedType().isVolatile());
      sBuilder.CreateStore(rhsValue, lhs.getPtr(), lhs.isVolatile());
    }
  } else {
    sBuilder.CreateStore(rhsValue, lhs.getPtr(), lhs.isVolatile());
  }
  return sBuilder.CreateLoad(lhs.getPtr(), lhs.isVolatile());
}
CharacterConstantAST::CharacterConstantAST(
    const Token &token)
    : AST(AST::Kind::CHARACTER_CONSTANT), mToken(token), mChar(token.toString().c_str()[0]) {}
FloatingConstantAST::FloatingConstantAST(
    const Token &token)
    : AST(AST::Kind::FLOATING_CONSTANT), mToken(token) {
  std::string::size_type sz;
  try {
    mValue = std::stof(this->mToken.toString(), &sz);
    const std::string &sub = this->mToken.toString().substr(sz);
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
  std::cout << mToken.toString() << std::endl;
}
EnumerationConstantAST::EnumerationConstantAST(nt<IdentifierAST>
                                               id)
    : AST(AST::Kind::ENUMERATION_CONSTANT), mIdentifier(std::move(id)) {}
ParameterListAST::ParameterListAST(nts<ParameterDeclarationAST>
                                   parameter_declaration,
                                   bool
                                   hasMultiple,
                                   SymbolTable &table)
    : AST(AST::Kind::PARAMETER_LIST),
      mParameterDeclaration(std::move(parameter_declaration)),
      mObjectTable(table),
      mMultiple(hasMultiple) {}
void ParameterListAST::print(int indent) {
  AST::print(indent);
  mParameterDeclaration.print(++indent);
}
std::vector<QualifiedType> ParameterListAST::codegen() {
  SymbolScope s(sObjectTable, &mObjectTable);
  std::vector<QualifiedType> parameters;
  for (const auto &ast : mParameterDeclaration) {
    ISymbol *symbol = ast->codegen();
    if (auto *obj = dynamic_cast<ObjectSymbol *>(symbol)) {
      if (obj->getQualifiedType().getType() == &VoidType::sVoidType && mParameterDeclaration.size() > 1) {
        throw SemaException("void should be the first and only parameter", involvedTokens());
      }
      obj->setIndex(parameters.size());
      parameters.push_back(obj->getQualifiedType());
    } else {
      throw std::runtime_error("WTF: how could parameter list have symbols other than object symbol");
    }
  }
  return parameters;
}
bool ParameterListAST::hasMultiple() const {
  return mMultiple;
}
ParameterDeclarationAST::ParameterDeclarationAST(nt<DeclarationSpecifiersAST>
                                                 declaration_specifiers,
                                                 nt<DeclaratorAST>
                                                 declarator)
    : AST(AST::Kind::PARAMETER_DECLARATION), mDeclarationSpecifiers(std::move(declaration_specifiers)),
      mDeclarator(std::move(declarator)) {}
void ParameterDeclarationAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mDeclarationSpecifiers->print(indent);
  if (mDeclarator) mDeclarator->print(indent);
}
ISymbol *ParameterDeclarationAST::codegen() {
  if (!mDeclarator) {
    mDeclarator = std::make_unique<DeclaratorAST>(nullptr, std::make_unique<SimpleDirectDeclaratorAST>(nullptr));
  }
  const auto &pair = mDeclarationSpecifiers->codegen();
  const StorageSpecifier storageSpecifier = pair.first;
  if (storageSpecifier != StorageSpecifier::kREGISTER && storageSpecifier != StorageSpecifier::kNone) {
    throw SemaException("The only storage-class specifier that shall occur in a parameter declaration is register.",
                        involvedTokens());
  }
  const QualifiedType &qualifiedType = pair.second;
  return mDeclarator->codegen(storageSpecifier, qualifiedType);
}
EnumeratorListAST::EnumeratorListAST(nts<EnumeratorAST>
                                     enumerator)
    : AST(AST::Kind::ENUMERATOR_LIST), mEnumerators(std::move(enumerator)) {}
void EnumeratorListAST::print(int indent) {
  AST::print(indent);
  mEnumerators.print(++indent);
}
std::vector<EnumConstSymbol *> EnumeratorListAST::codegen(EnumerationType *enumType) {
  std::vector<EnumConstSymbol *> members;
  int64_t index = 0;
  for (auto &enumerator : mEnumerators) {
    auto *symbol = enumerator->codegen(enumType, index);
    index = symbol->getIndex();
    members.push_back(symbol);
    ++index;
  }
  return members;
}
EnumeratorAST::EnumeratorAST(nt<IdentifierAST>
                             id) : AST(AST::Kind::ENUMERATOR, 0), mIdentifier(std::move(id)) {}
EnumeratorAST::EnumeratorAST(nt<IdentifierAST>
                             id,
                             nt<ConstantExpressionAST>
                             constant_expression)
    : AST(AST::Kind::ENUMERATOR, 1), mIdentifier(std::move(id)), mConstantExpression(std::move(constant_expression)) {}
void EnumeratorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mIdentifier->print(indent);
  if (getProduction() == 1) {
    mConstantExpression->print(indent);
  }
}
EnumConstSymbol *EnumeratorAST::codegen(EnumerationType *enumerationType, int64_t index) {
  if (mConstantExpression) {
    auto value = mConstantExpression->codegen();
    if (!dynamic_cast< IntegerType *>(value.getType())) {
      throw SemaException("the expression shall have an integer type", mConstantExpression->involvedTokens());
    } else {
      auto *constInt = llvm::dyn_cast<llvm::ConstantInt>(value.getValue());
      if (!constInt) throw std::runtime_error("WTF: not a llvm::ConstantInt, weird");
      index = constInt->getSExtValue();
    }
  }
  llvm::ConstantInt *value = llvm::ConstantInt::get(sModule->getContext(), llvm::APInt(32, index, true));
  mSymbol =
      std::make_unique<EnumConstSymbol>(std::make_unique<EnumerationMemberType>(enumerationType),
                                        &mIdentifier->getToken(),
                                        value);
  //TODO insert now or after type is complete?
  sObjectTable->insert(mSymbol.get());
  return mSymbol.get();
}
InitializerAST::InitializerAST(nt<AssignmentExpressionAST>
                               assignment_expression)
    : AST(AST::Kind::INITIALIZER, 0), mAST(std::move(assignment_expression)) {}
InitializerAST::InitializerAST(nt<InitializerListAST>
                               initializer_list)
    : AST(AST::Kind::INITIALIZER, 1), mAST(std::move(initializer_list)) {}
void InitializerAST::print(int indent) {
  AST::print(indent);
  mAST->print(++indent);
}
AST *InitializerAST::getInitializer() {
  return mAST.get();
}
InitializerListAST::InitializerListAST(nts<InitializerAST>
                                       initializer)
    : AST(AST::Kind::INITIALIZER_LIST), mInitializers(std::move(initializer)) {}
void InitializerListAST::print(int indent) {
  AST::print(indent);
  mInitializers.print(++indent);
}
const nts<InitializerAST> & InitializerListAST::getInitializers() {
  return mInitializers;
}
LabeledStatementAST::LabeledStatementAST(nt<StatementAST>
                                         statement)
    : StatementAST(AST::Kind::LABELED_STATEMENT), mStatement(std::move(statement)) {}
ExpressionStatementAST::ExpressionStatementAST(nt<ExpressionAST>
                                               expression)
    : StatementAST(AST::Kind::EXPRESSION_STATEMENT), mExpression(std::move(expression)) {}
void ExpressionStatementAST::print(int indent) {
  AST::print(indent);
  if (mExpression) mExpression->print(++indent);
}
void ExpressionStatementAST::codegen(StatementContexts &contexts) {
  if (sBuilder.GetInsertBlock()->getTerminator()) return;
  if (mExpression) mExpression->codegen();
}
SelectionStatementAST::SelectionStatementAST() : StatementAST(AST::Kind::SELECTION_STATEMENT) {}
IterationStatementAST::IterationStatementAST() : StatementAST(AST::Kind::ITERATION_STATEMENT) {}
JumpStatementAST::JumpStatementAST() : StatementAST(AST::Kind::JUMP_STATEMENT) {}
TypeQualifierAST::TypeQualifierAST(Terminal<TypeQualifier>
                                   op) : AST(AST::Kind::TYPE_QUALIFIER), mOp(op) {}
void TypeQualifierAST::print(int indent) {
  AST::print(indent);
  mOp.print(++indent);
}
TypeQualifier TypeQualifierAST::codegen() {
  return mOp.type;
}
TypedefNameAST::TypedefNameAST(nt<IdentifierAST>
                               identifier)
    : AST(AST::Kind::TYPEDEF_NAME), mIdentifier(std::move(identifier)) {}
void TypedefNameAST::print(int indent) {
  AST::print(indent);
  mIdentifier->print(++indent);
}
QualifiedType TypedefNameAST::codegen() {
  ISymbol *symbol = sObjectTable->lookup(mIdentifier->getToken());
  if (!symbol) {
    throw SemaException(std::string("Type ") + mIdentifier->getToken().toString() + "not declared",
                        mIdentifier->getToken());
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
    : AST(AST::Kind::IDENTIFIER), mToken(token) {}
void IdentifierAST::print(int indent) {
  AST::print(indent);
  AST::printIndent(++indent);
  std::cout << mToken.toString() << std::endl;
}
const Token &IdentifierAST::getToken() const {
  return mToken;
}
StringAST::StringAST(
    const Token &token) : AST(AST::Kind::STRING), mToken(token) {
  mType = std::make_unique<ArrayType>(QualifiedType(&IntegerType::sCharType, {TypeQualifier::kCONST}),
                                      mToken.toString().size() + 1);
}
ArrayType *StringAST::getType() {
  return mType.get();
}
const Token &StringAST::getToken() const {
  return mToken;
}
std::unique_ptr<llvm::LLVMContext> AST::sContext = std::make_unique<llvm::LLVMContext>();
std::unique_ptr<llvm::Module> AST::sModule = std::make_unique<llvm::Module>("top", getContext());
llvm::IRBuilder<> AST::sBuilder(getContext());
SymbolTable *AST::sObjectTable = nullptr;
SymbolTable *AST::sTagTable = nullptr;
SymbolTable *AST::sLabelTable = nullptr;
SymbolTables AST::mTables;
bool AST::sIsInitializerList = false;
AST::AST(AST::Kind
         kind, int
         id) : mKind(kind), mProductionId(id) {}
const char *AST::toString() {
  switch (mKind) {
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
std::pair<const Token &, const Token &> AST::involvedTokens() const {
  return {*mLeftMost, *mRightMost};
}
llvm::LLVMContext &AST::getContext() {
  return *sContext;
}
llvm::Module *AST::getModule() {
  return sModule.get();
}
llvm::IRBuilder<> &AST::getBuilder() {
  return sBuilder;
}
SymbolTables &AST::getTables() {
  return mTables;
}
std::unique_ptr<llvm::Module> AST::takeModule() {
  auto module = std::move(sModule);
  sModule = std::make_unique<llvm::Module>("top", getContext());
  return std::move(module);
}
std::unique_ptr<llvm::LLVMContext> AST::takeContext() {
  auto context = std::move(sContext);
  sContext = std::make_unique<llvm::LLVMContext>();
  return std::move(context);
}
IntegerConstantAST::IntegerConstantAST(
    const Token &token)
    : AST(AST::Kind::INTEGER_CONSTANT), mToken(token) {
  std::string::size_type sz;
  try {
    this->mValue = std::stoull(this->mToken.toString(), &sz, 0);
    const std::string sub = token.toString().substr(sz);
    if (sub.empty()) {
      this->mSuffix = Suffix::None;
    } else {
      auto it = sub.begin();
      if (*it == 'u' || *it == 'U') {
        ++it;
        if (it == sub.end()) {
          mSuffix = Suffix::U;
        } else if (*it == 'l' || *it == 'L') {
          ++it;
          if (it == sub.end()) {
            mSuffix = Suffix::UL;
          } else {
            if ((*it == 'l' || *it == 'L') && *it == *--it) {
              ++it;
              if (it == sub.end()) {
                mSuffix = Suffix::ULL;
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
          mSuffix = Suffix::L;
        } else if ((*it == 'l' || *it == 'L') && *it == *--it) {
          ++it;
          if (it == sub.end()) {
            mSuffix = Suffix::LL;
          } else {
            if (*it == 'u' || *it == 'U') {
              ++it;
              if (it == sub.end()) {
                mSuffix = Suffix::ULL;
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
  std::cout << mToken.toString() << std::endl;
}
void ArgumentExpressionList::print(int indent) {
  AST::print(indent);
  ++indent;
  for (const auto &argument : mArgumentsList) {
    argument->print(indent);
  }
}
ArgumentExpressionList::ArgumentExpressionList(nts<AssignmentExpressionAST>
                                               argumentList)
    : AST(AST::Kind::ARGUMENT_EXPRESSION_LIST), mArgumentsList(std::move(argumentList)) {}
std::vector<Value> ArgumentExpressionList::codegen() {
  std::vector<Value> arguments;
  for (const auto &argument : mArgumentsList) {
    arguments.push_back(argument->codegen());
  }
  return arguments;
}
TypeSpecifiersAST::TypeSpecifiersAST(CombinationKind
                                     kind, nts<TypeSpecifierAST>
                                     specifiers)
    : AST(AST::Kind::TYPE_SPECIFIERS), mCombinationKind(kind), mTypeSpecifiers(std::move(specifiers)) {

}
void TypeSpecifiersAST::print(int indent) {
  AST::print(indent);
  ++indent;
  for (auto &ast: mTypeSpecifiers) {
    ast->print(indent);
  }
}
bool TypeSpecifiersAST::empty() {
  return mTypeSpecifiers.empty();
}
QualifiedType TypeSpecifiersAST::codegen() {
  switch (mCombinationKind) {
    case CombinationKind::kUnknown:mType = nullptr;
      break;
    case CombinationKind::kVoid:mType = &VoidType::sVoidType;
      break;
    case CombinationKind::kUnsignedChar:
    case CombinationKind::kChar:mType = &IntegerType::sUnsignedCharType;
      break;
    case CombinationKind::kSignedChar:mType = &IntegerType::sCharType;
      break;
    case CombinationKind::kShort:
    case CombinationKind::kShortInt:
    case CombinationKind::kSignedShort:
    case CombinationKind::kSignedShortInt:mType = &IntegerType::sShortIntType;
      break;
    case CombinationKind::kUnsignedShort:
    case CombinationKind::kUnsignedShortInt:mType = &IntegerType::sUnsignedShortIntType;
      break;
    case CombinationKind::kInt:
    case CombinationKind::kSigned:
    case CombinationKind::kSignedInt:mType = &IntegerType::sIntType;
      break;
    case CombinationKind::kUnsigned:
    case CombinationKind::kUnsignedInt:mType = &IntegerType::sUnsignedIntType;
      break;
    case CombinationKind::kLong:
    case CombinationKind::kSignedLong:
    case CombinationKind::kSignedLongInt:mType = &IntegerType::sLongIntType;
      break;
    case CombinationKind::kUnsignedLong:
    case CombinationKind::kUnsignedLongInt:mType = &IntegerType::sUnsignedLongIntType;
      break;
    case CombinationKind::kLongLong:
    case CombinationKind::kSignedLongLong:
    case CombinationKind::kLongLongInt:
    case CombinationKind::kSignedLongLongInt:mType = &IntegerType::sLongLongIntType;
      break;
    case CombinationKind::kUnsignedLongLong:
    case CombinationKind::kUnsignedLongLongInt:mType = &IntegerType::sUnsignedLongLongIntType;
      break;
    case CombinationKind::kFloat:mType = &FloatingType::sFloatType;
      break;
    case CombinationKind::kDouble:mType = &FloatingType::sDoubleType;
      break;
    case CombinationKind::kLongDouble:mType = &FloatingType::sLongDoubleType;
      break;
    case CombinationKind::kStruct:
    case CombinationKind::kUnion: {
      auto *structAST =
          static_cast<StructOrUnionSpecifierAST *>(mTypeSpecifiers.back()->mSpecifier.get());
      mType = structAST->codegen();
      break;
    }
    case CombinationKind::kEnum: {
      auto *enumAST = static_cast<EnumSpecifierAST *>(mTypeSpecifiers.back()->mSpecifier.get());
      mType = enumAST->codegen();
      break;
    }
    case CombinationKind::kTypeName:
      auto *typeNameAST = static_cast<TypedefNameAST *>(mTypeSpecifiers.back()->mSpecifier.get());
      return typeNameAST->codegen();

  }
  return QualifiedType(mType, {});
}
DirectDeclaratorAST::DirectDeclaratorAST() : AST(Kind::DIRECT_DECLARATOR) {}
SimpleDirectDeclaratorAST::SimpleDirectDeclaratorAST(nt<IdentifierAST>
                                                     identifier)
    : mIdentifier(std::move(identifier)) {}
void SimpleDirectDeclaratorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mIdentifier->print(indent);
}
const Token *SimpleDirectDeclaratorAST::getIdentifier() {
  return &mIdentifier->getToken();
}
ISymbol *SimpleDirectDeclaratorAST::codegen(StorageSpecifier storageSpecifier, QualifiedType derivedType) {
  Linkage linkage = Linkage::kNone;

  if (!mIdentifier) {
    mSymbol = std::make_unique<ObjectSymbol>(derivedType, nullptr, nullptr);
    return mSymbol.get();
  }

  if (derivedType.getType() == &VoidType::sVoidType) {
    throw SemaException("cannot initialize a void type", involvedTokens());
  }

  ISymbol *priorDeclartion = sObjectTable->lookup(mIdentifier->getToken());

  switch (storageSpecifier) {
    case StorageSpecifier::kTYPEDEF: {
      if (!mIdentifier) {
        throw SemaException("typedef must have an identifier in declarator", involvedTokens());
      }
      mSymbol = std::make_unique<TypedefSymbol>(derivedType, &mIdentifier->getToken());
      sObjectTable->insert(mIdentifier->getToken(), mSymbol.get());
      return mSymbol.get();
    }
    case StorageSpecifier::kREGISTER:
    case StorageSpecifier::kAUTO:
      if (sObjectTable->getScopeKind() == ScopeKind::FILE) {
        throw SemaException(
            "The storage-class specifiers auto and register shall not appear in the declaration specifiers in an external declaration.",
            involvedTokens());
      } else {
        break;
      }
    case StorageSpecifier::kSTATIC: {
      if (sObjectTable->getScopeKind() == ScopeKind::FUNCTION_PROTOTYPE) {
        linkage = Linkage::kNone;
      } else if (sObjectTable->getScopeKind() == ScopeKind::FILE) {
        linkage = Linkage::kInternal;
      } else if (sObjectTable->getScopeKind() == ScopeKind::BLOCK
          && dynamic_cast<FunctionType *>(derivedType.getType())) {
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
      } else if (dynamic_cast<FunctionType *>(derivedType.getType())) {
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
          throw SemaException(std::string("redefine ") + mIdentifier->getToken().toString(), involvedTokens());
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
  QualifiedType newType = derivedType;
  const Token *token = mIdentifier ? &mIdentifier->getToken() : nullptr;
  if (auto *objectType = dynamic_cast<ObjectType *>(derivedType.getType())) {
    llvm::Value *value = nullptr;
    switch (sObjectTable->getScopeKind()) {
      create_in_file_scope:
      case ScopeKind::FILE: {
        llvm::GlobalValue::LinkageTypes llvmLinkage = llvm::GlobalValue::LinkageTypes::PrivateLinkage;
        if (linkage != Linkage::kNone) {
          llvmLinkage = linkage == Linkage::kExternal ? llvm::GlobalValue::LinkageTypes::ExternalLinkage
                                                      : llvm::GlobalValue::LinkageTypes::InternalLinkage;
        }
        llvm::GlobalVariable *gVar = new llvm::GlobalVariable(*getModule(), objectType->getLLVMType(),
                                                              derivedType.isConst(), llvmLinkage, nullptr);
        value = gVar;
        break;
      }
      case ScopeKind::BLOCK: {
        if (dynamic_cast< ArrayType *>(objectType)) {
          goto create_in_file_scope;
        }
        value = sBuilder.CreateAlloca(objectType->getLLVMType(), nullptr, mIdentifier->getToken().toString());
        break;
      }
      case ScopeKind::LABEL:throw std::runtime_error("WTF: how could a object type declared in a tag symbol");
      case ScopeKind::FUNCTION_PROTOTYPE:break;
      case ScopeKind::TAG:
        //do not create value
        break;
    }
    mSymbol = std::make_unique<ObjectSymbol>(newType, value, token);
  } else if (auto *functionType = dynamic_cast< FunctionType *>(derivedType.getType())) {
    llvm::Function *value = nullptr;
    switch (sObjectTable->getScopeKind()) {
      case ScopeKind::FILE:
      case ScopeKind::BLOCK: {
        auto *symbol = sObjectTable->lookup(mIdentifier->getToken());
        if (auto *functionSymbol = dynamic_cast<FunctionSymbol *>(symbol)) {
          return functionSymbol;
        } else {
          value = llvm::Function::Create(functionType->getLLVMType(),
                                         llvm::Function::ExternalLinkage,
                                         mIdentifier->getToken().toString(),
                                         sModule.get());
        }
        break;
      }
      case ScopeKind::LABEL:throw std::runtime_error("WTF: how could a function type declared in a label/proto symbol");
      case ScopeKind::FUNCTION_PROTOTYPE:break;
      case ScopeKind::TAG:throw SemaException("cannot declare funciton in a tag", involvedTokens());
    }
    mSymbol = std::make_unique<FunctionSymbol>(functionType, value, token);
  }
  mSymbol->setLinkage(linkage);
  sObjectTable->insert(*token, mSymbol.get());
  return mSymbol.get();
}
ParenthesedDirectDeclaratorAST::ParenthesedDirectDeclaratorAST(nt<DeclaratorAST>
                                                               declarator)
    : mDeclarator(std::move(declarator)) {}
void ParenthesedDirectDeclaratorAST::print(int indent) {
  AST::print(indent);
  mDeclarator->print(indent);
}
const Token *ParenthesedDirectDeclaratorAST::getIdentifier() {
  return mDeclarator->getIdentifier();
}
ISymbol *ParenthesedDirectDeclaratorAST::codegen(StorageSpecifier storageSpecifier,
                                                 QualifiedType derivedType) {
  return mDeclarator->codegen(storageSpecifier, derivedType);
}
ArrayDeclaratorAST::ArrayDeclaratorAST(nt<DirectDeclaratorAST>
                                       directDeclarator,
                                       nt<ConstantExpressionAST>
                                       constantExpression)
    : mDirectDeclarator(std::move(directDeclarator)), mConstantExpression(std::move(constantExpression)) {}
void ArrayDeclaratorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mDirectDeclarator->print(indent);
  if (mConstantExpression) mConstantExpression->print(indent);
}
const Token *ArrayDeclaratorAST::getIdentifier() {
  return mDirectDeclarator->getIdentifier();
}
ISymbol *ArrayDeclaratorAST::codegen(StorageSpecifier storageSpecifier, QualifiedType derivedType) {
  // 6.7.6.2 Array declarators
  // BNF wrote by Brian W. Kernighan and Dennis M. Ritchie,Prentice Hall, 1988 do not support variable length array
  int64_t size = 0;
  if (mConstantExpression) {
    auto value = mConstantExpression->codegen();
    if (!dynamic_cast< IntegerType *>(value.getType())) {
      throw SemaException("the expression shall have an integer type", mConstantExpression->involvedTokens());
    } else {
      auto *constInt = llvm::dyn_cast<llvm::ConstantInt>(value.getValue());
      if (!constInt) throw std::runtime_error("WTF: not a llvm::ConstantInt, weird");
      size = constInt->getSExtValue();
      if (size <= 0) {
        throw SemaException("array size should great than 0", mConstantExpression->involvedTokens());
      }
    }
  }
  // The element type shall not be an incomplete or function type.
  if (!derivedType.getType()->complete() || dynamic_cast< FunctionType *>(derivedType.getType())) {
    throw SemaException("The element type shall not be an incomplete or function type.", involvedTokens());
  }
  mArrayType = std::make_unique<ArrayType>(derivedType, size);
  return mDirectDeclarator->codegen(storageSpecifier,
                                    QualifiedType(mArrayType.get(), std::set<TypeQualifier>{}));
}
void FunctionDeclaratorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mDirectDeclarator->print(indent);
  if (mParameterList) mParameterList->print(indent);
}
FunctionDeclaratorAST::FunctionDeclaratorAST(nt<DirectDeclaratorAST>
                                             directDeclarator,
                                             nt<ParameterListAST>
                                             parameterList)
    : mDirectDeclarator(std::move(directDeclarator)), mParameterList(std::move(parameterList)) {}
const Token *FunctionDeclaratorAST::getIdentifier() {
  return mDirectDeclarator->getIdentifier();
}
ISymbol *FunctionDeclaratorAST::codegen(StorageSpecifier storageSpecifier, QualifiedType derivedType) {
  if (dynamic_cast< FunctionType *>(derivedType.getType())
      || dynamic_cast< ArrayType *>(derivedType.getType())) {
    throw SemaException(
        std::string("function ") + getIdentifier()->toString() + "cannot be array type or function type",
        involvedTokens());
  }
  if (mParameterList) {
    mFunctionType =
        std::make_unique<FunctionType>(derivedType, mParameterList->codegen(), mParameterList->hasMultiple());
  } else {
    mFunctionType = std::make_unique<FunctionType>(derivedType, std::move(std::vector<QualifiedType>()), false);
  }
  QualifiedType qualifiedType(mFunctionType.get(), {});
  auto *symbol = mDirectDeclarator->codegen(storageSpecifier, qualifiedType);
  auto functionSymbol = dynamic_cast<FunctionSymbol *>(symbol);
  if (!functionSymbol) {
    //this is a pointer to function
    return symbol;
  }
  auto *theFunction = functionSymbol->getValue();
  auto args = theFunction->arg_begin();
  if (mParameterList) {
    for (auto &pair :mParameterList->mObjectTable) {
      const auto &name = pair.first;
      auto *item = dynamic_cast<ObjectSymbol *>(pair.second);
      llvm::Value *value = args + item->getIndex();
      value->setName(name);
      // we do not set value to the symbol, but leave it in compound statement codegen
    }
  }
  return symbol;
}
void IdentifierPrimaryExpressionAST::print(int indent) {
  PrimaryExpressionAST::print(indent);
  mIdentifier->print(++indent);
}
Value IdentifierPrimaryExpressionAST::codegen() {
  auto *symbol = sObjectTable->lookup(mIdentifier->getToken());
  if (!symbol) {
    throw SemaException(mIdentifier->getToken().toString() + " is not declared", mIdentifier->getToken());
  }
  QualifiedType qualifiedType;
  bool lvalue = false;
  llvm::Value *value;
  if (auto *obj = dynamic_cast<ObjectSymbol *>(symbol)) {
    qualifiedType = obj->getQualifiedType();
    lvalue = true;
    value = obj->getValue();
  } else if (auto *function = dynamic_cast<FunctionSymbol *>(symbol)) {
    qualifiedType = {function->getType(), {}};
    lvalue = true;
    value = function->getValue();
  } else if (auto *enumeration = dynamic_cast<EnumConstSymbol *>(symbol)) {
    qualifiedType = QualifiedType(enumeration->getType(), {TypeQualifier::kCONST});
    value = enumeration->getValue();
  } else {
    throw SemaException("identifier as a primary expression must be object or function or enumeration const",
                        mIdentifier->getToken());
  }
  return Value(qualifiedType, lvalue, value);
}
IdentifierPrimaryExpressionAST::IdentifierPrimaryExpressionAST(nt<IdentifierAST>
                                                               identifier) : mIdentifier(std::move(
    identifier)) {}
void IntegerConstantPrimaryExpressionAST::print(int indent) {
  PrimaryExpressionAST::printIndent(indent);
  mIntegerConstant->print(++indent);
}
Value IntegerConstantPrimaryExpressionAST::codegen() {
  bool isBase10 = mIntegerConstant->mToken.toString()[0] != 0;
  unsigned long long int n = mIntegerConstant->mValue;

  unsigned int intSize = IntegerType::sIntType.getSizeInBits();
  unsigned int longSize = IntegerType::sLongIntType.getSizeInBits();
  unsigned int longLongSize = IntegerType::sLongLongIntType.getSizeInBits();
  IntegerType *type;
  switch (mIntegerConstant->mSuffix) {
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
               llvm::ConstantInt::get(type->getLLVMType(), type->getAPInt(n)));
}
IntegerConstantPrimaryExpressionAST::IntegerConstantPrimaryExpressionAST(nt<IntegerConstantAST>
                                                                         integer_constant)
    : mIntegerConstant(std::move(
    integer_constant)) {}
void FloatingConstantPrimaryExpressionAST::print(int indent) {
  PrimaryExpressionAST::printIndent(indent);
  mFloatingConstant->print(++indent);
}
Value FloatingConstantPrimaryExpressionAST::codegen() {
  FloatingType *type;
  llvm::ConstantFP *constantFp;
  switch (mFloatingConstant->mSuffix) {
    case FloatingConstantAST::Suffix::None:type = &FloatingType::sDoubleType;
      constantFp =
          llvm::ConstantFP::get(AST::getContext(), llvm::APFloat(static_cast<double>(mFloatingConstant->mValue)));
      break;
    case FloatingConstantAST::Suffix::F: type = &FloatingType::sFloatType;
      constantFp =
          llvm::ConstantFP::get(AST::getContext(), llvm::APFloat(static_cast<float>(mFloatingConstant->mValue)));
      break;
    case FloatingConstantAST::Suffix::L: type = &FloatingType::sLongDoubleType;
      constantFp =
          llvm::ConstantFP::get(AST::getContext(), llvm::APFloat(static_cast<double>(mFloatingConstant->mValue)));
      break;
  }
  return Value(QualifiedType(type, {TypeQualifier::kCONST}), false, constantFp);
}
FloatingConstantPrimaryExpressionAST::FloatingConstantPrimaryExpressionAST(nt<FloatingConstantAST>
                                                                           floating_constant)
    : mFloatingConstant(std::move(
    floating_constant)) {}
void CharacterConstantPrimaryExpressionAST::print(int indent) {
  PrimaryExpressionAST::print(indent);
  mCharacterConstant->print(++indent);
}
Value CharacterConstantPrimaryExpressionAST::codegen() {
  IntegerType *type = &IntegerType::sCharType;
  return Value(QualifiedType(type, {TypeQualifier::kCONST}),
               false,
               llvm::ConstantInt::get(type->getLLVMType(),
                                      type->getAPInt(static_cast<uint64_t>(mCharacterConstant->mChar))));
}
CharacterConstantPrimaryExpressionAST::CharacterConstantPrimaryExpressionAST(nt<CharacterConstantAST> character_constant)
    : mCharacterConstant(std::move(character_constant)) {}
void StringPrimaryExpressionAST::print(int indent) {
  PrimaryExpressionAST::print(indent);
  mStringAST->print(++indent);
}
Value StringPrimaryExpressionAST::codegen() {
  if (sIsInitializerList) {
    auto *v = llvm::ConstantDataArray::getString(getContext(), mStringAST->getToken().toString());
    QualifiedType qualifiedType(mStringAST->getType(), {});
    return {qualifiedType, false, v};
  } else {
    mPointerType = std::make_unique<PointerType>(mStringAST->getType()->getReferencedQualifiedType());
    auto *ptr = sBuilder.CreateGlobalStringPtr(mStringAST->getToken().toString());
    return Value(QualifiedType(mPointerType.get(), {}), false, ptr);
  }
}
StringPrimaryExpressionAST::StringPrimaryExpressionAST(nt<StringAST> string) : mStringAST(std::move(string)) {}
void ExpressionPrimaryExpressionAST::print(int indent) {
  PrimaryExpressionAST::print(indent);
  mExpression->print(++indent);
}
Value ExpressionPrimaryExpressionAST::codegen() {
  return mExpression->codegen();
}
ExpressionPrimaryExpressionAST::ExpressionPrimaryExpressionAST(nt<ExpressionAST> expression)
    : mExpression(std::move(expression)) {}
void SimplePostfixExpressionAST::print(int indent) {
  AST::print(indent);
  mPrimaryExpression->print(++indent);
}
Value SimplePostfixExpressionAST::codegen() {
  return mPrimaryExpression->codegen();
}
SimplePostfixExpressionAST::SimplePostfixExpressionAST(nt<PrimaryExpressionAST> primary) : mPrimaryExpression(std::move(
    primary)) {}
void ArrayPostfixExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mPostfixExpression->print(indent);
  mExpression->print(indent);
}
Value ArrayPostfixExpressionAST::codegen() {
  // 6.5.2.1 Array subscripting
  Value lhs = mPostfixExpression->codegen();
  Value rhs = mExpression->codegen();
  auto *pointerType = dynamic_cast<PointerType *>(lhs.getType());
  if (!pointerType) {
    auto *arrayType = dynamic_cast<ArrayType *>(lhs.getType());
    if (arrayType) {
      mArrayToPointer = std::make_unique<PointerType>(arrayType->getReferencedQualifiedType());
      pointerType = mArrayToPointer.get();
    } else {
      throw SemaException("array left side should be pointer", mPostfixExpression->involvedTokens());
    }
  }

  if (!pointerType->getReferencedQualifiedType().getType()->complete()) {
    throw SemaException("array left side should be pointer to complete object type",
                        mPostfixExpression->involvedTokens());
  }

  if (!(dynamic_cast< IntegerType *>(rhs.getType()))) {
    throw SemaException("array right side should be an integer type",
                        mPostfixExpression->involvedTokens());
  }

  llvm::Value *value = nullptr;
  auto *constantInt = llvm::dyn_cast<llvm::ConstantInt>(rhs.getValue());
  auto index = constantInt->getSExtValue();

  if (sObjectTable->getScopeKind() == ScopeKind::BLOCK) {
    value = sBuilder.CreateGEP(lhs.getType()->getLLVMType(),
                               lhs.getPtr(),
                               {llvm::ConstantInt::get(getContext(), llvm::APInt(64, 0)), constantInt});
  } else {
    throw std::runtime_error("WTF: array position query in other scope? we may need global constructor here");
  }
  return Value(pointerType->getReferencedQualifiedType(), true, value);
}
ArrayPostfixExpressionAST::ArrayPostfixExpressionAST(nt<PostfixExpressionAST> postfix_expression,
                                                     nt<ExpressionAST> expression)
    : mPostfixExpression(std::move(postfix_expression)), mExpression(std::move(expression)) {}
void FunctionPostfixExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mPostfixExpression->print(indent);
  mArgumentExpressionList->print(indent);
}
Value FunctionPostfixExpressionAST::codegen() {
  //6.5.2.2 Function calls
  Value lhs = mPostfixExpression->codegen();
  FunctionType *functionType = nullptr;
  llvm::Value *callee;
  if (auto *p = dynamic_cast<PointerType *>(lhs.getType())) {
    functionType = dynamic_cast<FunctionType *>(p->getReferencedType());
    callee = sBuilder.CreateLoad(lhs.getPtr());
  } else if (auto *f = dynamic_cast< FunctionType *>(lhs.getType())) {
    functionType = f;
    callee = lhs.getPtr();
  } else {
    throw SemaException("The expression that denotes the called function92) shall have type pointer to function",
                        mPostfixExpression->involvedTokens());
  }
  auto returnType = functionType->getReturnType();
  if (dynamic_cast< ArrayType *>(returnType.getType())) {
    throw SemaException("return type cannot be array type", mPostfixExpression->involvedTokens());
  }
  if (dynamic_cast< FunctionType *>(returnType.getType())) {
    throw SemaException("return type cannot be function type", mPostfixExpression->involvedTokens());
  }
  auto arguments = mArgumentExpressionList->codegen();
  auto paramtersSize = functionType->getParameters().size();
  auto argumentsSize = arguments.size();
  if (!functionType->hasVarArg() && paramtersSize != argumentsSize) {
    throw SemaException("arguments do not match function proto type", mPostfixExpression->involvedTokens());
  } else if (paramtersSize > argumentsSize) {
    throw SemaException("arguments are less than function defined", mPostfixExpression->involvedTokens());
  }
  auto para = functionType->getParameters().begin();
  auto arg = arguments.begin();
  std::vector<llvm::Value *> argumentValues;
  while (arg != arguments.end()) {
    llvm::Value *argumentValue = arg->getValue();
    auto *argType = dynamic_cast< ObjectType *>(arg->getType());
    if (!argType) {
      throw SemaException("arguments must be object type", mPostfixExpression->involvedTokens());
    }
    if (!argType->complete()) {
      throw SemaException("arguments must be completed type", mPostfixExpression->involvedTokens());
    }
    if (para != functionType->getParameters().end()) {
      if (!argType->compatible(para->getType())) {
        throw SemaException("arguments do not match function proto type", mPostfixExpression->involvedTokens());
      }
      ++para;
    } else {
      //default argument promotions
      if (auto *arithmeticType = dynamic_cast<ArithmeticType *>(arg->getType())) {
        argumentValue = arithmeticType->promote(arg->getValue(), mArgumentExpressionList.get()).second;
      }
    }
    argumentValues.push_back(argumentValue);
    ++arg;
  }
  return Value(functionType->getReturnType(), false, sBuilder.CreateCall(callee, argumentValues));
}
FunctionPostfixExpressionAST::FunctionPostfixExpressionAST(nt<PostfixExpressionAST> postfix_expression,
                                                           nt<ArgumentExpressionList> arguments)
    : mPostfixExpression(std::move(postfix_expression)), mArgumentExpressionList(std::move(arguments)) {}
void MemberPostfixExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mPostfixExpression->print(indent);
  mIdentifier->print(indent);
}
Value MemberPostfixExpressionAST::codegen() {
  //6.5.2.3 Structure and union members
  Value lhs = mPostfixExpression->codegen();
  CompoundType *compoundTy;
  if (!dynamic_cast< StructType *>(lhs.getType())
      && !dynamic_cast< UnionType *>(lhs.getType())) {
    throw SemaException("left side must be a struct type or union type", mPostfixExpression->involvedTokens());
  } else {
    compoundTy = dynamic_cast< CompoundType *>(lhs.getType());
  }
  auto *memberSymbol = dynamic_cast<ObjectSymbol *>(compoundTy->mTable.lookup(mIdentifier->getToken()));
  //TODO 6.5.2.3.6
  if (!memberSymbol) {
    throw SemaException(mIdentifier->getToken().toString() + "is not a member of " + compoundTy->getTagName(),
                        mIdentifier->involvedTokens());
  }
  QualifiedType qualifiedType(memberSymbol->getQualifiedType());
  qualifiedType.addQualifiers(lhs.getQualifiers());

  llvm::Value *value;
  if (dynamic_cast<StructType *>(compoundTy)) {
    unsigned int index;
    index = memberSymbol->getIndex();

    llvm::APInt apint(32, index);
    value = sBuilder.CreateGEP(lhs.getType()->getLLVMType(),
                               lhs.getPtr(),
                               {llvm::ConstantInt::get(AST::getContext(), llvm::APInt(32, 0)),
                                llvm::ConstantInt::get(AST::getContext(), apint)});
  } else if (dynamic_cast<UnionType *>(compoundTy)) {
    auto *pointerType = llvm::PointerType::get(memberSymbol->getQualifiedType().getType()->getLLVMType(), 0);
    value = sBuilder.CreateBitCast(lhs.getPtr(), pointerType);
  } else throw std::runtime_error("WTF: member access other than union and struct");

  return Value(qualifiedType, lhs.isLValue(), value);
}
void PointerMemberPostfixExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mPostfixExpression->print(indent);
  mIdentifier->print(indent);
}
Value PointerMemberPostfixExpressionAST::codegen() {
  //6.5.2.3 Structure and union members
  Value lhs = mPostfixExpression->codegen();
  CompoundType *compoundTy;
  auto *p = dynamic_cast< PointerType *>(lhs.getType());
  if (p) {
    if (!dynamic_cast< StructType *>(p->getReferencedType())
        && !dynamic_cast< UnionType *>(p->getReferencedType())) {
      throw SemaException("left side must be a pointer to a struct type or union type",
                          mPostfixExpression->involvedTokens());
    } else {
      compoundTy = dynamic_cast< CompoundType *>(p->getReferencedType());
    }
  } else {
    throw SemaException("left side must be a pointer type", mPostfixExpression->involvedTokens());
  }
  auto *memberSymbol = dynamic_cast<ObjectSymbol *>(compoundTy->mTable.lookup(mIdentifier->getToken()));
  if (!memberSymbol) {
    throw SemaException(mIdentifier->getToken().toString() + "is not a member of " + compoundTy->getTagName(),
                        mIdentifier->involvedTokens());
  }
  QualifiedType qualifiedType(memberSymbol->getQualifiedType());
  qualifiedType.addQualifiers(lhs.getQualifiers());
  unsigned int index;
  llvm::Value *value;
  if (dynamic_cast< StructType *>(compoundTy)) {
    index = memberSymbol->getIndex();

    llvm::APInt apint(32, index);
    value = sBuilder.CreateGEP(p->getReferencedQualifiedType().getType()->getLLVMType(),
                               lhs.getValue(),
                               {llvm::ConstantInt::get(AST::getContext(), llvm::APInt(32, 0)),
                                llvm::ConstantInt::get(AST::getContext(), apint)});
  } else if (dynamic_cast<UnionType *>(compoundTy)) {
    auto *pointerType = llvm::PointerType::get(p->getReferencedQualifiedType().getType()->getLLVMType(), 0);
    value = sBuilder.CreateBitCast(lhs.getPtr(), pointerType);
  } else throw std::runtime_error("WTF: member access other than union and struct");
  return Value(qualifiedType, true, value);
}
PointerMemberPostfixExpressionAST::PointerMemberPostfixExpressionAST(nt<PostfixExpressionAST> postfix_expression,
                                                                     nt<IdentifierAST> identifier)
    : mPostfixExpression(std::move(postfix_expression)), mIdentifier(std::move(identifier)) {}
void IncrementPostfixExpression::print(int indent) {
  AST::print(indent);
  mPostfixExpression->print(++indent);
}
Value IncrementPostfixExpression::codegen() {
  // 6.5.2.4 Postfix increment and decrement operators
  Value value = mPostfixExpression->codegen();
  if (!value.modifiable()) {
    throw SemaException("operand must be modifiable", mPostfixExpression->involvedTokens());
  }
  Type *type = value.getType();
  llvm::Value *result = sBuilder.CreateAlloca(value.getType()->getLLVMType());
  sBuilder.CreateStore(value.getValue(), result, value.isVolatile());
  llvm::Value *newVal;
  if (dynamic_cast< IntegerType *>(type)) {
    newVal = sBuilder.CreateAdd(value.getValue(), llvm::ConstantInt::getSigned(type->getLLVMType(), 1));
  } else if (dynamic_cast< FloatingType *>(type)) {
    newVal = sBuilder.CreateFAdd(value.getValue(), llvm::ConstantFP::get(type->getLLVMType(), 1.0));
  } else if (dynamic_cast< PointerType *>(type)) {
    newVal = sBuilder.CreateGEP(value.getValue(), llvm::ConstantInt::get(getContext(), llvm::APInt(32, 1)));
  } else {
    throw SemaException(
        "The operand of the postfix increment or decrement operator shall have real or pointer type, and shall be a modifiable lvalue.",
        mPostfixExpression->involvedTokens());
  }
  sBuilder.CreateStore(newVal, value.getPtr(), value.isVolatile());
  if (value.isConst() || !value.isLValue()) {
    throw SemaException("The operand shall be a modifiable lvalue", mPostfixExpression->involvedTokens());
  }
  return Value(value.getQualifiedType(), true, result);
}
IncrementPostfixExpression::IncrementPostfixExpression(nt<PostfixExpressionAST> postfix_expression) :
    mPostfixExpression(std::move(postfix_expression)) {}
void DecrementPostfixExpression::print(int indent) {
  AST::print(indent);
  mPostfixExpression->print(++indent);
}
Value DecrementPostfixExpression::codegen() {
  // 6.5.2.4 Postfix increment and decrement operators
  Value value = mPostfixExpression->codegen();
  if (!value.modifiable()) {
    throw SemaException("operand must be modifiable", mPostfixExpression->involvedTokens());
  }
  Type *type = value.getType();
  llvm::Value *result = sBuilder.CreateAlloca(value.getType()->getLLVMType());
  sBuilder.CreateStore(value.getValue(), result, value.isVolatile());
  llvm::Value *newVal;
  if (dynamic_cast< IntegerType *>(type)) {
    newVal = sBuilder.CreateSub(value.getValue(), llvm::ConstantInt::getSigned(type->getLLVMType(), 1));
  } else if (dynamic_cast< FloatingType *>(type)) {
    newVal = sBuilder.CreateFSub(value.getValue(), llvm::ConstantFP::get(type->getLLVMType(), 1.0f));
  } else if (dynamic_cast< PointerType *>(type)) {
    newVal = sBuilder.CreateGEP(value.getValue(), {llvm::ConstantInt::get(getContext(), llvm::APInt(32, -1, true))});
  } else {
    throw SemaException(
        "The operand of the postfix increment or decrement operator shall have real or pointer type, and shall be a modifiable isLValue().",
        mPostfixExpression->involvedTokens());
  }
  sBuilder.CreateStore(newVal, value.getPtr(), value.isVolatile());
  if (value.getQualifiedType().contains(TypeQualifier::kCONST) || !value.isLValue()) {
    throw SemaException("The operand shall be a modifiable lvalue", mPostfixExpression->involvedTokens());
  }
  return Value(value.getQualifiedType(), true, result);
}
DecrementPostfixExpression::DecrementPostfixExpression(nt<PostfixExpressionAST> postfix_expression) :
    mPostfixExpression(std::move(postfix_expression)) {}
void SimpleUnaryExpressionAST::print(int indent) {
  AST::print(indent);
  mPostfixExpression->print(++indent);
}
Value SimpleUnaryExpressionAST::codegen() {
  auto v = mPostfixExpression->codegen();
  if (dynamic_cast<FunctionType *>(v.getQualifiedType().getType())) {
    // convert to pointer
    mConvertedPointer = std::make_unique<PointerType>(v.getQualifiedType());
    return Value(QualifiedType(mConvertedPointer.get(), v.getQualifiedType().getQualifiers()),
                 false,
                 v.getPtr());
  } else if (auto *arrayType = dynamic_cast<ArrayType *>(v.getQualifiedType().getType())) {
    if (v.isLValue()) {
      auto *ptr =
          sBuilder.CreateGEP(arrayType->getLLVMType(),
                             v.getPtr(),
                             {IntegerType::sIntType.getDefaultValue(), IntegerType::sIntType.getDefaultValue()});
      mConvertedPointer = std::make_unique<PointerType>(arrayType->getReferencedQualifiedType());
      return Value(QualifiedType(mConvertedPointer.get(), v.getQualifiedType().getQualifiers()),
                   false,
                   ptr);
    } else {
      return v;
    }
  } else {
    return v;
  }
}
SimpleUnaryExpressionAST::SimpleUnaryExpressionAST(nt<PostfixExpressionAST> postfixExpression)
    : mPostfixExpression(std::move(postfixExpression)) {}
void PrefixIncrementExpressionAST::print(int indent) {
  AST::print(indent);
  mUnaryExpression->print(++indent);
}
Value PrefixIncrementExpressionAST::codegen() {
  //6.5.3.1 Prefix increment and decrement operators
  Value value = mUnaryExpression->codegen();
  if (!value.modifiable()) {
    throw SemaException("oprand must be modifiable", mUnaryExpression->involvedTokens());
  }
  Type *type = value.getType();
  llvm::Value *newVal;
  if (dynamic_cast< IntegerType *>(type)) {
    newVal = sBuilder.CreateAdd(value.getValue(), llvm::ConstantInt::getSigned(type->getLLVMType(), 1));
  } else if (dynamic_cast< FloatingType *>(type)) {
    newVal = sBuilder.CreateFAdd(value.getValue(), llvm::ConstantFP::get(type->getLLVMType(), 1.0f));
  } else if (dynamic_cast< PointerType *>(type)) {
    newVal = sBuilder.CreateGEP(value.getValue(), {llvm::ConstantInt::get(getContext(), llvm::APInt(32, 1))});
  } else {
    throw SemaException(
        "The operand of the prefix increment or decrement operator shall have real or pointer type, and shall be a modifiable lvalue.",
        mUnaryExpression->involvedTokens());
  }
  sBuilder.CreateStore(newVal, value.getPtr(), value.isVolatile());
  return value;
}
PrefixIncrementExpressionAST::PrefixIncrementExpressionAST(nt<UnaryExpressionAST> unaryExpressionAST)
    : mUnaryExpression(std::move(unaryExpressionAST)) {}
void PrefixDecrementExpressionAST::print(int indent) {
  AST::print(indent);
  mUnaryExpression->print(++indent);
}
Value PrefixDecrementExpressionAST::codegen() {
  //6.5.3.1 Prefix increment and decrement operators
  Value value = mUnaryExpression->codegen();
  if (!value.modifiable()) {
    throw SemaException("oprand must be modifiable", mUnaryExpression->involvedTokens());
  }
  llvm::Value *newVal;
  Type *type = value.getType();
  if (dynamic_cast< IntegerType *>(type)) {
    newVal = sBuilder.CreateSub(value.getValue(), llvm::ConstantInt::get(type->getLLVMType(), 1));
  } else if (dynamic_cast< FloatingType *>(type)) {
    newVal = sBuilder.CreateFSub(value.getValue(), llvm::ConstantFP::get(type->getLLVMType(), 1.0f));
  } else if (dynamic_cast< PointerType *>(type)) {
    newVal = sBuilder.CreateGEP(value.getValue(), {llvm::ConstantInt::get(getContext(), llvm::APInt(32, -1, true))});
  } else {
    throw SemaException(
        "The operand of the prefix increment or decrement operator shall have real or pointer type, and shall be a modifiable lvalue.",
        mUnaryExpression->involvedTokens());
  }
  sBuilder.CreateStore(newVal, value.getPtr(), value.isVolatile());
  return value;
}
PrefixDecrementExpressionAST::PrefixDecrementExpressionAST(nt<UnaryExpressionAST> unaryExpressionAST)
    : mUnaryExpression(std::move(unaryExpressionAST)) {}
void UnaryOperatorExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mOp.print(indent);
  mCastExpression->print(indent);
}
Value UnaryOperatorExpressionAST::codegen() {
  Value value = mCastExpression->codegen();
  Type *type = value.getType();
  llvm::Value *newVal = value.getValue();
  switch (mOp.type) {
    case UnaryOp::AMP: {
      //TODO is checking lvalue fit constaints of 6.5.3.2 Address and indirection operators?
      if (!value.isLValue()) {
        throw SemaException("only lvalue can access address", mOp.token);
      }
      mPointerType = std::make_unique<PointerType>(value.getQualifiedType());
      type = mPointerType.get();
      if (!llvm::cast<llvm::PointerType>(value.getPtr()->getType())) {
        throw std::runtime_error("WTF: expression value is not llvm::PointerType");
      }
      auto *alloca = sBuilder.CreateAlloca(type->getLLVMType());
      sBuilder.CreateStore(value.getPtr(), alloca);
      return Value(QualifiedType(type, {}), true, alloca);
    }
    case UnaryOp::STAR:
      if (auto *pointerType = dynamic_cast< PointerType *>(type)) {
        return Value(pointerType->getReferencedQualifiedType(), true, newVal);
      } else {
        throw SemaException("The operand of the unary * operator shall have pointer type.", mOp.token);
      }
    case UnaryOp::PLUS:
      if (auto *integerType = dynamic_cast< IntegerType *>(type)) {
        std::tie(type, newVal) = integerType->promote(value.getValue(), mCastExpression.get());
      } else if (dynamic_cast< FloatingType *>(type)) {
      } else
        throw SemaException("The operand of the unary + or - operator shall have arithmetic type",
                            mCastExpression->involvedTokens());
      return Value(QualifiedType(type, value.getQualifiers()), false, newVal);
    case UnaryOp::SUB:
      if (auto *integerType = dynamic_cast< IntegerType *>(type)) {
        std::tie(type, newVal) = integerType->promote(value.getValue(), mCastExpression.get());
        newVal = sBuilder.CreateSub(
            llvm::ConstantInt::get(getContext(), llvm::APInt(32, 0)),
            newVal);

      } else if (dynamic_cast< FloatingType *>(type)) {
        newVal = sBuilder.CreateFSub(
            llvm::ConstantFP::get(getContext(), llvm::APFloat(0.0f)),
            newVal);
      } else
        throw SemaException("The operand of the unary + or - operator shall have arithmetic type",
                            mCastExpression->involvedTokens());
      return Value(QualifiedType(type, value.getQualifiers()), false, newVal);
    case UnaryOp::TILDE:
      if (auto *integerType = dynamic_cast< IntegerType *>(type)) {
        std::tie(type, newVal) = integerType->promote(value.getValue(), mCastExpression.get());
        newVal = sBuilder.CreateXor(newVal, llvm::ConstantInt::get(getContext(), llvm::APInt(32, -1, true)));
      } else {
        throw SemaException("The operand of the unary ~ operator shall have integer type",
                            mCastExpression->involvedTokens());
      }
      return Value(QualifiedType(type, value.getQualifiers()), false, newVal);
    case UnaryOp::BANG:
      if (auto *integerType = dynamic_cast< IntegerType *>(type)) {
        newVal = sBuilder.CreateICmpEQ(newVal, llvm::ConstantInt::get(getContext(), llvm::APInt(32, 0)));
        type = &IntegerType::sIntType;
        newVal = sBuilder.CreateZExt(newVal, IntegerType::sIntType.getLLVMType());
      } else if (dynamic_cast< FloatingType *>(type)) {
        newVal = sBuilder.CreateFCmpUEQ(newVal, llvm::ConstantFP::get(getContext(), llvm::APFloat(0.0f)));
        newVal = sBuilder.CreateXor(newVal, llvm::ConstantInt::get(getContext(), llvm::APInt(1, 1)));
        type = &IntegerType::sIntType;
        newVal = sBuilder.CreateZExt(newVal, IntegerType::sIntType.getLLVMType());
      } else if (auto *pointerType = dynamic_cast< PointerType *>(type)) {
        auto *const_null = llvm::ConstantPointerNull::get(pointerType->getLLVMType());
        newVal = sBuilder.CreateICmpEQ(newVal, const_null);
        type = &IntegerType::sIntType;
        newVal = sBuilder.CreateZExt(newVal, IntegerType::sIntType.getLLVMType());
      } else {
        throw SemaException("The operand of the unary ! operator shall have scalar type",
                            mCastExpression->involvedTokens());
      }
      break;
  }
  return Value(QualifiedType(type, value.getQualifiers()), false, newVal);
}
UnaryOperatorExpressionAST::UnaryOperatorExpressionAST(Terminal<UnaryOp> op, nt<CastExpressionAST> castExpressionAST)
    : mOp(op), mCastExpression(std::move(castExpressionAST)) {}
Value SizeofUnaryExpressionAST::codegen() {
  Type *type;
  if (auto *ue = dynamic_cast<UnaryExpressionAST *>(mAST.get())) {
    auto value = ue->codegen();
    type = value.getType();
  } else {
    type = static_cast<TypeNameAST *>(mAST.get())->codegen().getType();
  }

  if (dynamic_cast< FunctionType *>(type)) {
    throw SemaException("The sizeof operator shall not be applied to an expression that has function type",
                        mAST->involvedTokens());
  } else if (!type->complete()) {
    throw SemaException("The sizeof operator shall not be applied to an expression that has an incomplete type",
                        mAST->involvedTokens());
  } // or a bit field

  auto *objectType = dynamic_cast<ObjectType *>(type);
  if (!objectType) {
    throw std::runtime_error("WTF: sizeof on a non-object type");
  }
  return Value(QualifiedType(&IntegerType::sIntType, {TypeQualifier::kCONST}),
               false,
               llvm::ConstantInt::get(getContext(),
                                      llvm::APInt(IntegerType::sIntType.getSizeInBits(),
                                                  objectType->getSizeInBits() / 8)));
}
void SizeofUnaryExpressionAST::print(int indent) {
  AST::print(indent);
  mAST->print(++indent);
}
SizeofUnaryExpressionAST::SizeofUnaryExpressionAST(nt<UnaryExpressionAST> unaryExpressionAST)
    : mAST(std::move(unaryExpressionAST)) {}
SizeofUnaryExpressionAST::SizeofUnaryExpressionAST(nt<TypeNameAST> typeNameAST)
    : mAST(std::move(typeNameAST)) {}
void SimpleCastExpressionAST::print(int indent) {
  AST::print(indent);
  mUnaryExpression->print(++indent);
}
Value SimpleCastExpressionAST::codegen() {
  return mUnaryExpression->codegen();
}
SimpleCastExpressionAST::SimpleCastExpressionAST(nt<UnaryExpressionAST> unaryExpression)
    : mUnaryExpression(std::move(unaryExpression)) {}
void RealCastExpressionAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mTypeName->print(indent);
  mCastExpression->print(indent);
}
Value RealCastExpressionAST::codegen() {
  Type *castType = mTypeName->codegen().getType();

  auto operand = mCastExpression->codegen();
  if (auto *integerType = dynamic_cast< IntegerType *>(operand.getType())) {
    return Value(QualifiedType(castType, {}),
                 false,
                 integerType->castTo(castType, operand.getValue(), mCastExpression.get()));
  } else if (auto *floatType = dynamic_cast< FloatingType * >(operand.getType())) {
    return Value(QualifiedType(castType, {}),
                 false,
                 floatType->castTo(castType, operand.getValue(), mCastExpression.get()));
  } else if (auto *pointerType = dynamic_cast< PointerType *>(operand.getType())) {
    return Value(QualifiedType(castType, {}),
                 false,
                 pointerType->castTo(castType, operand.getValue(), mCastExpression.get()));
  }
  throw SemaException(std::string("illegel cast"), involvedTokens());
  //TODO Conversions that involve pointers, other than where permitted by the constraints of 6.5.16.1, shall be specified by means of an explicit cast.
}
RealCastExpressionAST::RealCastExpressionAST(nt<TypeNameAST> typeName, nt<CastExpressionAST> castExpression)
    : mTypeName(std::move(typeName)), mCastExpression(std::move(castExpression)) {}
void SimpleBinaryOperatorAST::print(int indent) {
  AST::print(indent);
  mCastExpression->print(++indent);
}
Value SimpleBinaryOperatorAST::codegen() {
  return mCastExpression->codegen();
}
void BinaryOperatorAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mLeft->print(indent);
  mOp.print(indent);
  mRight->print(indent);
}
Value BinaryOperatorAST::codegen() {
  auto lhs = mLeft->codegen();
  auto rhs = mRight->codegen();
  return codegen(lhs, rhs, mOp.type, mLeft.get(), mRight.get());
}
std::tuple<Type *, llvm::Value *, llvm::Value *>
BinaryOperatorAST::UsualArithmeticConversions(Value &lhs, Value &rhs, const AST *ast) {
  Type *lType = lhs.getType();
  Type *rType = rhs.getType();
  llvm::Value *lValue = lhs.getValue();
  llvm::Value *rValue = rhs.getValue();
  if (UsualArithmeticConversions(lType, rType, ast)) {
    return {lType, lValue, rType->castTo(lType, rValue, ast)};
  } else {
    return {rType, lType->castTo(rType, lValue, ast), rValue};
  }
}
Value BinaryOperatorAST::codegen(Value &lhs,
                                 Value &rhs,
                                 InfixOp op,
                                 const AST *lAST,
                                 const AST *rAST) {
  Type *type;
  llvm::Value *lValue = lhs.getValue();
  llvm::Value *rValue = rhs.getValue();

  switch (op) {
    case InfixOp::STAR:
      if (!dynamic_cast< ArithmeticType *>(lhs.getType())
          && !dynamic_cast< ArithmeticType *>(rhs.getType())) {
        throw SemaException("Each of the operands shall have arithmetic type", lAST->involvedTokens());
      }
      std::tie(type, lValue, rValue) = UsualArithmeticConversions(lhs, rhs, lAST);
      if (dynamic_cast< IntegerType *>(type)) {
        return Value(QualifiedType(type, {}), false, sBuilder.CreateMul(lValue, rValue));
      } else {
        return Value(QualifiedType(type, {}), false, sBuilder.CreateFMul(lValue, rValue));
      }
    case InfixOp::SLASH:
      if (!dynamic_cast< ArithmeticType *>(lhs.getType())
          && !dynamic_cast< ArithmeticType *>(rhs.getType())) {
        throw SemaException("Each of the operands shall have arithmetic type", lAST->involvedTokens());
      }
      std::tie(type, lValue, rValue) = UsualArithmeticConversions(lhs, rhs, lAST);
      if (auto *integerType = dynamic_cast< IntegerType *>(type)) {
        if (integerType->isSigned()) {
          return Value(QualifiedType(type, {}), false, sBuilder.CreateSDiv(lValue, rValue));
        } else {
          return Value(QualifiedType(type, {}), false, sBuilder.CreateUDiv(lValue, rValue));
        }
      } else {
        return Value(QualifiedType(type, {}), false, sBuilder.CreateFDiv(lValue, rValue));
      }
    case InfixOp::PERCENT:std::tie(type, lValue, rValue) = UsualArithmeticConversions(lhs, rhs, lAST);
      if (!dynamic_cast< IntegerType *>(type)
          && !dynamic_cast< IntegerType *>(rhs.getType())) {
        throw SemaException("Each of the operands shall have arithmetic type", lAST->involvedTokens());
      }
      if (static_cast<const IntegerType *>(type)->isSigned()) {
        return Value(QualifiedType(type, {}), false, sBuilder.CreateSRem(lValue, rValue));
      } else {
        return Value(QualifiedType(type, {}), false, sBuilder.CreateURem(lValue, rValue));
      }
    case InfixOp::PLUS:
      if (auto *pointerType = dynamic_cast< PointerType *> (lhs.getType())) {
        if (!pointerType->complete()) {
          throw SemaException("Additive of pointer must be a complete type", lAST->involvedTokens());
        } else {
          if (dynamic_cast< IntegerType *>(rhs.getType())) {
            return Value(QualifiedType(pointerType, {}),
                         false,
                         sBuilder.CreateGEP(lhs.getType()->getLLVMType(), lhs.getPtr(), rhs.getValue()));
          }
        }
      }
      if (auto *integerType = dynamic_cast< IntegerType *>(lhs.getType())) {
        if (auto *pointerType = dynamic_cast< PointerType *> (rhs.getType())) {
          if (!pointerType->complete()) {
            throw SemaException("Additive of pointer must be a complete type", lAST->involvedTokens());
          } else {
            return Value(QualifiedType(pointerType, {}),
                         false,
                         sBuilder.CreateGEP(lhs.getType()->getLLVMType(), rhs.getPtr(), lhs.getValue()));
          }
        }
      }
      if (dynamic_cast< ArithmeticType *>(lhs.getType())
          && dynamic_cast< ArithmeticType *>(rhs.getType())) {
        std::tie(type, lValue, rValue) = UsualArithmeticConversions(lhs, rhs, lAST);
        if (dynamic_cast< IntegerType *>(type)) {
          return Value(QualifiedType(type, {}), false, sBuilder.CreateAdd(lValue, rValue));
        } else {
          return Value(QualifiedType(type, {}), false, sBuilder.CreateFAdd(lValue, rValue));
        }
      }
      break;
    case InfixOp::SUB:
      if (auto *lp = dynamic_cast< PointerType *> (lhs.getType())) {
        auto *obj1 = dynamic_cast< ObjectType *>(lp->getReferencedQualifiedType().getType());
        if (!obj1) {
          throw SemaException("pointer in subtraction must point to object types", lAST->involvedTokens());
        }
        if (auto *rp = dynamic_cast< PointerType *> (rhs.getType())) {
          if (!dynamic_cast< ObjectType *>(rp->getReferencedQualifiedType().getType())) {
            throw SemaException("pointer in subtraction must point to object types", lAST->involvedTokens());
          }
          if (lp->complete() && lp->compatible(rp)) {
            auto *v1 = lp->castTo(PointerType::sAddrType, lhs.getValue(), lAST);
            auto *v2 = rp->castTo(PointerType::sAddrType, rhs.getValue(), rAST);
            auto *v3 = sBuilder.CreateSub(v1, v2);
            auto *v4 = sBuilder.CreateUDiv(v3,
                                           llvm::ConstantInt::get(PointerType::sAddrType->getLLVMType(),
                                                                  obj1->getSizeInBits()));
            return Value(QualifiedType(PointerType::sAddrType, {}), false, v4);
          }
        } else if (auto *rp = dynamic_cast< IntegerType *>(rhs.getType())) {
          auto *const0 = llvm::ConstantInt::get(rp->getLLVMType(), 0, true);
          auto *negativeValue = sBuilder.CreateSub(const0, rhs.getValue());
          return Value(QualifiedType(lp, {}),
                       false,
                       sBuilder.CreateGEP(lhs.getValue(), {negativeValue}));
        }
      } else if (dynamic_cast< ArithmeticType *>(lhs.getType())
          && dynamic_cast< ArithmeticType *>(rhs.getType())) {
        std::tie(type, lValue, rValue) = UsualArithmeticConversions(lhs, rhs, lAST);
        if (dynamic_cast< IntegerType *>(type)) {
          return Value(QualifiedType(type, {}), false, sBuilder.CreateSub(lValue, rValue));
        } else {
          return Value(QualifiedType(type, {}), false, sBuilder.CreateFSub(lValue, rValue));
        }
      }
      break;
    case InfixOp::LTLT:
    case InfixOp::GTGT: {
      auto *ltype = dynamic_cast< IntegerType *>(lhs.getType());
      auto *rtype = dynamic_cast< IntegerType *>(lhs.getType());
      if (!ltype) {
        throw SemaException("Each of the operands shall have integer type.", lAST->involvedTokens());
      }
      if (!rtype) {
        throw SemaException("Each of the operands shall have integer type.", lAST->involvedTokens());
      }
      rValue = rtype->castTo(ltype, rhs.getValue(), lAST);
      if (op == InfixOp::LTLT) {
        lValue = sBuilder.CreateShl(lhs.getValue(), rValue);
      } else {
        if (ltype->isSigned()) {
          lValue = sBuilder.CreateAShr(lhs.getValue(), rValue);
        } else {
          lValue = sBuilder.CreateLShr(lhs.getValue(), rValue);
        }
      }
      return Value(QualifiedType(ltype, {}), false, lValue);
    }
    case InfixOp::LT:
    case InfixOp::GT:
    case InfixOp::LTEQ:
    case InfixOp::GTEQ: {
      llvm::ICmpInst::Predicate predicate;
      llvm::Value *cmpRes;
      bool isSigned;
      if (auto *lPointer = dynamic_cast< PointerType *>(lhs.getType())) {
        if (!lPointer->complete()) {
          throw SemaException("operand should be completed pointer type", lAST->involvedTokens());
        }
        if (auto *rPointer = dynamic_cast< PointerType *>(rhs.getType())) {
          if (!rPointer->complete()) {
            throw SemaException("operand should be completed pointer type", lAST->involvedTokens());
          }
          if (lPointer->compatible(rPointer)) {
            throw SemaException("referenced type is not compatible", lAST->involvedTokens());
          }
          isSigned = PointerType::sAddrType->isSigned();
          goto icmp;
        } else {
          throw SemaException("both operands should be completed pointer type", lAST->involvedTokens());
        }
      } else {
        if (!dynamic_cast< ArithmeticType *>(lhs.getType())
            && !dynamic_cast< ArithmeticType *>(rhs.getType())) {
          throw SemaException("Each of the operands shall have real type", lAST->involvedTokens());
        }
        std::tie(type, lValue, rValue) = UsualArithmeticConversions(lhs, rhs, lAST);
        if (auto *intergerType = dynamic_cast< IntegerType *>(type)) {
          isSigned = intergerType->isSigned();
          goto icmp;
        } else {
          goto fcmp;
        }
      }
      icmp:
      switch (op) {
        case InfixOp::LT:
          if (isSigned) {
            predicate = llvm::ICmpInst::Predicate::ICMP_SLT;
          } else {
            predicate = llvm::ICmpInst::Predicate::ICMP_ULT;
          }
          break;
        case InfixOp::GT:
          if (isSigned) {
            predicate = llvm::ICmpInst::Predicate::ICMP_SGT;
          } else {
            predicate = llvm::ICmpInst::Predicate::ICMP_UGT;
          }
          break;
        case InfixOp::LTEQ:
          if (isSigned) {
            predicate = llvm::ICmpInst::Predicate::ICMP_SLE;
          } else {
            predicate = llvm::ICmpInst::Predicate::ICMP_ULE;
          }
          break;
        case InfixOp::GTEQ:
          if (isSigned) {
            predicate = llvm::ICmpInst::Predicate::ICMP_SGE;
          } else {
            predicate = llvm::ICmpInst::Predicate::ICMP_UGE;
          }
          break;
      }
      cmpRes = sBuilder.CreateICmp(predicate, lValue, rValue);
      goto codegen;
      fcmp:
      {
        switch (op) {
          case InfixOp::LT:predicate = llvm::ICmpInst::Predicate::FCMP_OLT;
            break;
          case InfixOp::GT:predicate = llvm::ICmpInst::Predicate::FCMP_OGT;
            break;
          case InfixOp::LTEQ:predicate = llvm::ICmpInst::Predicate::FCMP_OLE;
            break;
          case InfixOp::GTEQ:predicate = llvm::ICmpInst::Predicate::FCMP_OGE;
            break;
        }
      };
      cmpRes = sBuilder.CreateFCmp(predicate, lValue, rValue);
      codegen:
      cmpRes =
          IntegerType::sOneBitBoolIntType.castTo(&IntegerType::sUnsignedIntType, cmpRes, lAST);
      return Value(QualifiedType(&IntegerType::sIntType, {}), false, cmpRes);
    }
    case InfixOp::EQEQ:
    case InfixOp::BANGEQ: {
      llvm::Value *result;
      if (dynamic_cast< ArithmeticType *>(lhs.getType())
          && dynamic_cast< ArithmeticType *>(lhs.getType())) {
        std::tie(type, lValue, rValue) = UsualArithmeticConversions(lhs, rhs, lAST);
        if (dynamic_cast< IntegerType *>(type)) {
          if (op == InfixOp::EQEQ) {
            result = sBuilder.CreateICmpEQ(lValue, rValue);
          } else {
            result = sBuilder.CreateICmpNE(lValue, rValue);
          }
        } else {
          if (op == InfixOp::EQEQ) {
            result = sBuilder.CreateFCmpOEQ(lValue, rValue);
          } else {
            result = sBuilder.CreateFCmpONE(lValue, rValue);
          }
        }
      } else if (auto *ltype = dynamic_cast< PointerType *>(lhs.getType())) {
        if (auto *rtype = dynamic_cast< PointerType *>(rhs.getType())) {
          if (!ltype->compatible(rtype)) {
            if (ltype->getReferencedQualifiedType().getType() == &VoidType::sVoidType) {
              lValue = sBuilder.CreateBitCast(lValue, rtype->getLLVMType());
            } else if (rtype->getReferencedQualifiedType().getType() == &VoidType::sVoidType) {
              rValue = sBuilder.CreateBitCast(rValue, ltype->getLLVMType());
            } else {
              throw SemaException("both pointers should be compatible or one must be a void pointer",
                                  lAST->involvedTokens());
            }
          }
          if (op == InfixOp::EQEQ) {
            result = sBuilder.CreateICmpEQ(lValue, rValue);
          } else {
            result = sBuilder.CreateICmpNE(lValue, rValue);
          }
        } else {
          throw SemaException("right hand side must be a pointer type", lAST->involvedTokens());
        }
      } else {
        throw SemaException("invalid operands to binary expression", lAST->involvedTokens());
      }
      result =
          IntegerType::sOneBitBoolIntType.castTo(&IntegerType::sUnsignedIntType, result, lAST);
      return Value(QualifiedType(&IntegerType::sIntType, {}), false, result);
    }
    case InfixOp::AMP:
    case InfixOp::CARET:
    case InfixOp::BAR: {
      auto *integer1 = dynamic_cast< IntegerType *>(lhs.getType());
      auto *integer2 = dynamic_cast< IntegerType *>(rhs.getType());
      if (!integer1 || !integer2) {
        throw SemaException("both operands must be integer type", lAST->involvedTokens());
      }
      std::tie(type, lValue, rValue) = UsualArithmeticConversions(lhs, rhs, lAST);
      llvm::Value *result;
      if (op == InfixOp::AMP) {
        result = sBuilder.CreateAnd(lValue, rValue);
      } else if (op == InfixOp::CARET) {
        result = sBuilder.CreateXor(lValue, rValue);
      } else {
        result = sBuilder.CreateOr(lValue, rValue);
      }
      return Value(QualifiedType(type, {}), false, result);
    }
    default:break;
  }
  throw SemaException("invalid operands to binary expression", lAST->involvedTokens());
}
bool BinaryOperatorAST::UsualArithmeticConversions(Type *lhs, Type *rhs, const AST *ast) {
  if (dynamic_cast<IntegerType *>(lhs) && dynamic_cast<IntegerType *>(rhs)) {
    return static_cast<IntegerType *>(lhs)->getSizeInBits() > static_cast<IntegerType *>(rhs)->getSizeInBits();
  } else if (dynamic_cast<IntegerType *>(lhs) && dynamic_cast<FloatingType *>(rhs)) {
    return false;
  } else if (dynamic_cast<FloatingType *>(lhs) && dynamic_cast<IntegerType *>(rhs)) {
    return true;
  } else if (dynamic_cast<FloatingType *>(lhs) && dynamic_cast<FloatingType *>(rhs)) {
    return static_cast<FloatingType *>(lhs)->getSizeInBits() > static_cast<FloatingType *>(rhs)->getSizeInBits();
  } else {
    throw SemaException("operands must be integer or float type", ast->involvedTokens());
  }
}
BinaryOperatorAST::BinaryOperatorAST(nt<IBinaryOperationAST> left, Terminal<InfixOp> op, nt<IBinaryOperationAST> right)
    : mLeft(std::move(left)), mOp(op), mRight(std::move(right)) {}
Value LogicalBinaryOperatorAST::codegen() {
  auto *currentFunction = sBuilder.GetInsertBlock()->getParent();
  auto *thisBlock = sBuilder.GetInsertBlock();
  auto *otherBlock = llvm::BasicBlock::Create(getContext());
  auto *endBlock = llvm::BasicBlock::Create(getContext());
  llvm::Value *cond1;
  llvm::Value *cond2;
  auto lhs = mLeft->codegen();
  if (auto *ltype = dynamic_cast< IntegerType *> (lhs.getType())) {
    auto *const0 = llvm::ConstantInt::get(ltype->getLLVMType(), 0);
    cond1 = sBuilder.CreateICmpNE(lhs.getValue(), const0);
  } else if (auto *ltype = dynamic_cast< FloatingType *> (lhs.getType())) {
    auto *const0 = llvm::ConstantFP::get(ltype->getLLVMType(), 0.0);
    cond1 = sBuilder.CreateFCmpONE(lhs.getValue(), const0);
  } else if (auto *ltype = dynamic_cast< PointerType *> (lhs.getType())) {
    auto *const0 = llvm::ConstantPointerNull::get(ltype->getLLVMType());
    cond1 = sBuilder.CreateICmpNE(lhs.getValue(), const0);
  } else {
    throw SemaException("left side operand must be scalar type", mLeft->involvedTokens());
  }
  if (mOp.type == InfixOp::AMPAMP) {
    sBuilder.CreateCondBr(cond1, otherBlock, endBlock);
  } else {
    sBuilder.CreateCondBr(cond1, endBlock, otherBlock);
  }

  // otherBlock
  currentFunction->getBasicBlockList().push_back(otherBlock);
  sBuilder.SetInsertPoint(otherBlock);
  auto rhs = mRight->codegen();
  if (auto *rtype = dynamic_cast< IntegerType *> (rhs.getType())) {
    auto *const0 = llvm::ConstantInt::get(rtype->getLLVMType(), 0);
    cond2 = sBuilder.CreateICmpNE(rhs.getValue(), const0);
  } else if (auto *rtype = dynamic_cast< FloatingType *> (rhs.getType())) {
    auto *const0 = llvm::ConstantFP::get(rtype->getLLVMType(), 0.0);
    cond2 = sBuilder.CreateFCmpONE(rhs.getValue(), const0);
  } else if (auto *rtype = dynamic_cast< PointerType *> (rhs.getType())) {
    auto *const0 = llvm::ConstantPointerNull::get(rtype->getLLVMType());
    cond2 = sBuilder.CreateICmpNE(rhs.getValue(), const0);
  } else {
    throw SemaException("right side operand must be scalar type", mRight->involvedTokens());
  }
  sBuilder.CreateBr(endBlock);
  // endBlock
  currentFunction->getBasicBlockList().push_back(endBlock);
  sBuilder.SetInsertPoint(endBlock);
  auto *phi = sBuilder.CreatePHI(IntegerType::sOneBitBoolIntType.getLLVMType(), 2);
  phi->addIncoming(cond1, thisBlock);
  phi->addIncoming(cond2, otherBlock);
  auto *result =
      IntegerType::sOneBitBoolIntType.castTo(&IntegerType::sUnsignedIntType, phi, mRight.get());
  return Value(QualifiedType(&IntegerType::sIntType, {}), false, result);
}
IfSelectionStatementAST::IfSelectionStatementAST(nt<ExpressionAST> expression,
                                                 nt<StatementAST> statement,
                                                 nt<StatementAST> elseStatement)
    : mExpression(std::move(expression)), mStatement(std::move(statement)),
      mElseStatement(std::move(elseStatement)) {}
void IfSelectionStatementAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mExpression->print(indent);
  mStatement->print(indent);
  if (mElseStatement) {
    mElseStatement->print(indent);
  }
}
void IfSelectionStatementAST::codegen(StatementContexts &contexts) {
  if (sBuilder.GetInsertBlock()->getTerminator()) return;
  auto exp = mExpression->codegen();
  llvm::Value *cond;
  if (auto *integerTy = dynamic_cast< IntegerType *>(exp.getType())) {
    cond = sBuilder.CreateICmpNE(exp.getValue(), integerTy->getDefaultValue());
  } else if (auto *floatTy = dynamic_cast< FloatingType *>(exp.getType())) {
    cond = sBuilder.CreateFCmpONE(exp.getValue(), floatTy->getDefaultValue());
  } else if (auto *pointerTy = dynamic_cast< PointerType *>(exp.getType())) {
    cond = sBuilder.CreateICmpNE(exp.getValue(), pointerTy->getDefaultValue());
  } else {
    throw SemaException("The controlling expression of an if statement shall have scalar type.",
                        mExpression->involvedTokens());
  }
  auto *function = contexts.getContainingFunction();
  auto *trueBB = llvm::BasicBlock::Create(getContext(), "", function);
  auto *endBB = llvm::BasicBlock::Create(getContext());
  if (mElseStatement) {
    auto *falseBB = llvm::BasicBlock::Create(getContext());
    sBuilder.CreateCondBr(cond, trueBB, falseBB);
    // false BB
    function->getBasicBlockList().push_back(falseBB);
    sBuilder.SetInsertPoint(falseBB);
    mElseStatement->codegen(contexts);
    if (!sBuilder.GetInsertBlock()->getTerminator())
      sBuilder.CreateBr(endBB);
  } else {
    sBuilder.CreateCondBr(cond, trueBB, endBB);
  }
  // ture BB
  sBuilder.SetInsertPoint(trueBB);
  mStatement->codegen(contexts);
  if (!sBuilder.GetInsertBlock()->getTerminator())
    sBuilder.CreateBr(endBB);
  // endBB
  function->getBasicBlockList().push_back(endBB);
  sBuilder.SetInsertPoint(endBB);
}
SwitchSelectionStatementAST::SwitchSelectionStatementAST(nt<ExpressionAST> expression, nt<StatementAST> statement)
    : mExpression(std::move(expression)), mStatement(std::move(statement)) {}
void SwitchSelectionStatementAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mExpression->print(indent);
  mStatement->print(indent);
}
void SwitchSelectionStatementAST::codegen(StatementContexts &contexts) {
  if (sBuilder.GetInsertBlock()->getTerminator()) return;
  auto exp = mExpression->codegen();
  if (dynamic_cast<IntegerType *>(exp.getType())) {
    auto *switchInst = sBuilder.CreateSwitch(exp.getValue(), nullptr);
    auto *endBB = llvm::BasicBlock::Create(getContext());
    contexts.add(std::make_unique<SwitchContext>(switchInst, endBB));
    mStatement->codegen(contexts);
//    if (switchInst->getNumCases() == 0) {
//      throw SemaException("switch body is empty", involvedTokens());
//    }
    contexts.getContainingFunction()->getBasicBlockList().push_back(endBB);
    sBuilder.SetInsertPoint(endBB);
  } else {
    throw SemaException("The controlling expression of a switch statement shall have integer type.",
                        mExpression->involvedTokens());
  }
}
void IdentifierLabeledStatementAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mIdentifier->print(indent);
  mStatement->print(indent);
}
IdentifierLabeledStatementAST::IdentifierLabeledStatementAST(nt<IdentifierAST> id, nt<StatementAST> statement)
    : LabeledStatementAST(std::move(statement)), mIdentifier(std::move(id)) {}
void IdentifierLabeledStatementAST::codegen(StatementContexts &contexts) {
  if (sBuilder.GetInsertBlock()->getTerminator()) return;
  llvm::BasicBlock *BB;
  auto *labelSymbol = dynamic_cast<LabelSymbol *>(sLabelTable->lookup(mIdentifier->getToken()));
  if (labelSymbol) {
    if (labelSymbol->isDefinedByGoto()) {
      BB = labelSymbol->getBasicBlock();
      contexts.getContainingFunction()->getBasicBlockList().push_back(BB);
    } else {
      throw SemaException("WTF: redefined label", mIdentifier->involvedTokens());
    }
  } else {
    BB = llvm::BasicBlock::Create(getContext(), "", contexts.getContainingFunction());
    mLabelSymbol = std::make_unique<LabelSymbol>(&mIdentifier->getToken(), BB, false);
    sLabelTable->insert(mIdentifier->getToken(), mLabelSymbol.get());
  }
  labelSymbol->setAsImplemented();
  if (!sBuilder.GetInsertBlock()->getTerminator()) {
    // fall into label statement
    sBuilder.CreateBr(BB);
  }
  sBuilder.SetInsertPoint(BB);
  mStatement->codegen(contexts);
}
void CaseLabeledStatementAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mConstantExpression->print(indent);
  mStatement->print(indent);
}
CaseLabeledStatementAST::CaseLabeledStatementAST(nt<ConstantExpressionAST> constantExpression,
                                                 nt<StatementAST> statement)
    : LabeledStatementAST(std::move(statement)), mConstantExpression(std::move(constantExpression)) {}
void CaseLabeledStatementAST::codegen(StatementContexts &contexts) {
  auto *switchContext = contexts.getLastContext<SwitchContext>();
  if (switchContext) {
    auto *switchInst = switchContext->getSwitchInst();
    auto *constInt =
        llvm::dyn_cast<llvm::ConstantInt>(mConstantExpression->codegen().getValue()); // no need to promote
    if (!constInt)
      throw SemaException("The expression of each case label shall be an integer constant expression",
                          mConstantExpression->involvedTokens());
    auto *BB = llvm::BasicBlock::Create(getContext(), "", contexts.getContainingFunction());
    // check if last case fall through to this case
    if (switchInst->getNumCases() != 0 && !(--switchInst->case_end())->getCaseSuccessor()->getTerminator()) {
      sBuilder.CreateBr(BB);
    }
    sBuilder.SetInsertPoint(BB);
    mStatement->codegen(contexts);
    if (switchInst->findCaseValue(constInt) == switchInst->case_default()) {
      switchInst->addCase(constInt, BB);
    } else {
      throw SemaException(
          "no two of the case constant expressions in the same switch statement shall have the same value",
          mConstantExpression->involvedTokens());
    }
  } else {
    throw SemaException("A case or default label shall appear only in a switch statement.", involvedTokens());
  }
}
void DefaultLabeledStatementAST::print(int indent) {
  AST::print(indent);
  ++indent;
  mStatement->print(indent);
}
void DefaultLabeledStatementAST::codegen(StatementContexts &contexts) {
  auto *switchContext = contexts.getLastContext<SwitchContext>();
  if (switchContext) {
    auto *switchInst = switchContext->getSwitchInst();
    if (!switchInst->getOperand(1)) {
      auto *BB = llvm::BasicBlock::Create(getContext(), "", contexts.getContainingFunction());
      // check if last case fall through to this case
      if (switchInst->getNumCases() != 0 && !(--switchInst->case_end())->getCaseSuccessor()->getTerminator()) {
        sBuilder.CreateBr(BB);
      }
      sBuilder.SetInsertPoint(BB);
      mStatement->codegen(contexts);
      sBuilder.CreateBr(switchContext->getBreakBB());
      switchInst->setDefaultDest(BB);
    } else {
      throw SemaException(
          "There may be at most one default label in a switch statement", involvedTokens());
    }
  } else {
    throw SemaException("A case or default label shall appear only in a switch statement.", involvedTokens());
  }
}
DefaultLabeledStatementAST::DefaultLabeledStatementAST(nt<StatementAST> statement) : LabeledStatementAST(std::move(
    statement)) {}
void GotoJumpStatementAST::print(int indent) {
  AST::print(indent);
  mIdentifier->print(++indent);
}
GotoJumpStatementAST::GotoJumpStatementAST(nt<IdentifierAST> identifier) : mIdentifier(std::move(identifier)) {}
void GotoJumpStatementAST::codegen(StatementContexts &contexts) {
  if (sBuilder.GetInsertBlock()->getTerminator()) return;
  llvm::BasicBlock *BB;
  if (auto *symbol = sLabelTable->lookup(mIdentifier->getToken())) {
    if (auto *label = dynamic_cast<LabelSymbol *>(symbol)) {
      BB = label->getBasicBlock();
    } else {
      throw std::runtime_error("WTF: label table has other symbol type");
    }
  } else {
    BB = llvm::BasicBlock::Create(getContext(), "");
    mLabelSymbol = std::make_unique<LabelSymbol>(&mIdentifier->getToken(), BB, true);
    sLabelTable->insert(mIdentifier->getToken(), mLabelSymbol.get());
  }
  sBuilder.CreateBr(BB);
}
void ReturnJumpStatementAST::print(int indent) {
  AST::print(indent);
  mExpression->print(++indent);
}
ReturnJumpStatementAST::ReturnJumpStatementAST(nt<ExpressionAST> expression) : mExpression(std::move(expression)) {}
void ReturnJumpStatementAST::codegen(StatementContexts &contexts) {
  if (sBuilder.GetInsertBlock()->getTerminator()) return;
  auto *function = contexts.getLastContext<FunctionContext>();
  if (!function) {
    throw SemaException("return statement should only appear in functions", involvedTokens());
  }
  auto *type = function->getFunctionType()->getReturnType().getType();
  auto *returnAlloca = function->getReturnAlloca();
  if (!mExpression && returnAlloca) {
    throw SemaException(
        "A return statement without an expression shall only appear in a function whose return type is void",
        involvedTokens());
  } else {
    if (type == &VoidType::sVoidType) {
      throw SemaException(
          "A return statement with an expression shall not appear in a function whose return type is void",
          involvedTokens());
    } else {
      auto v = mExpression->codegen();
      sBuilder.CreateStore(v.getType()->castTo(type, v.getValue(), this), returnAlloca);
    }
  }
  sBuilder.CreateBr(function->getReturnBlock());
  function->addReturn();
}
WhileIterationStatementAST::WhileIterationStatementAST(nt<ExpressionAST>
                                                       expression, nt<StatementAST>
                                                       statement)
    : mExpression(std::move(expression)), mStatement(std::move(statement)) {}
void WhileIterationStatementAST::codegen(StatementContexts &contexts) {
  if (sBuilder.GetInsertBlock()->getTerminator()) return;
  auto *conditionBB = llvm::BasicBlock::Create(getContext(), "", contexts.getContainingFunction());
  auto *loopBB = llvm::BasicBlock::Create(getContext());
  auto *endBB = llvm::BasicBlock::Create(getContext());
  sBuilder.CreateBr(conditionBB);
  //condition bb
  sBuilder.SetInsertPoint(conditionBB);
  auto value = mExpression->codegen();
  llvm::Value *cond;
  if (auto *integerTy = dynamic_cast< IntegerType *>(value.getType())) {
    cond = sBuilder.CreateICmpNE(value.getValue(), integerTy->getDefaultValue());
  } else if (auto *floatTy = dynamic_cast< FloatingType *>(value.getType())) {
    cond = sBuilder.CreateFCmpONE(value.getValue(), floatTy->getDefaultValue());
  } else if (auto *pointerTy = dynamic_cast< PointerType *>(value.getType())) {
    cond = sBuilder.CreateICmpNE(value.getValue(), pointerTy->getDefaultValue());
  } else {
    throw SemaException("The controlling expression of an iteration statement shall have scalar type.",
                        mExpression->involvedTokens());
  }
  sBuilder.CreateCondBr(cond, loopBB, endBB);
  contexts.add(std::make_unique<LoopContext>(conditionBB, endBB));
  // loop body
  contexts.getContainingFunction()->getBasicBlockList().push_back(loopBB);
  sBuilder.SetInsertPoint(loopBB);
  mStatement->codegen(contexts);
  if (!sBuilder.GetInsertBlock()->getTerminator())
    sBuilder.CreateBr(conditionBB);
  // end body
  contexts.getContainingFunction()->getBasicBlockList().push_back(endBB);
  sBuilder.SetInsertPoint(endBB);
}
void DoIterationStatementAST::codegen(StatementContexts &contexts) {
  if (sBuilder.GetInsertBlock()->getTerminator()) return;
  auto *conditionBB = llvm::BasicBlock::Create(getContext(), "", contexts.getContainingFunction());
  auto *loopBB = llvm::BasicBlock::Create(getContext());
  auto *endBB = llvm::BasicBlock::Create(getContext());
  sBuilder.CreateBr(loopBB);
  // loop body
  contexts.getContainingFunction()->getBasicBlockList().push_back(loopBB);
  sBuilder.SetInsertPoint(loopBB);
  mStatement->codegen(contexts);
  if (!sBuilder.GetInsertBlock()->getTerminator())
    sBuilder.CreateBr(conditionBB);
  //condition bb
  sBuilder.SetInsertPoint(conditionBB);
  auto value = mExpression->codegen();
  llvm::Value *cond;
  if (auto *integerTy = dynamic_cast< IntegerType *>(value.getType())) {
    cond = sBuilder.CreateICmpNE(value.getValue(), integerTy->getDefaultValue());
  } else if (auto *floatTy = dynamic_cast< FloatingType *>(value.getType())) {
    cond = sBuilder.CreateFCmpONE(value.getValue(), floatTy->getDefaultValue());
  } else if (auto *pointerTy = dynamic_cast< PointerType *>(value.getType())) {
    cond = sBuilder.CreateICmpNE(value.getValue(), pointerTy->getDefaultValue());
  } else {
    throw SemaException("The controlling expression of an iteration statement shall have scalar type.",
                        mExpression->involvedTokens());
  }
  sBuilder.CreateCondBr(cond, loopBB, endBB);
  contexts.add(std::make_unique<LoopContext>(conditionBB, endBB));
  // end body
  contexts.getContainingFunction()->getBasicBlockList().push_back(endBB);
  sBuilder.SetInsertPoint(endBB);
}
void ForIterationStatementAST::codegen(StatementContexts &contexts) {
  if (sBuilder.GetInsertBlock()->getTerminator()) return;
  if (mExpression) {
    mExpression->codegen();
  }
  auto *conditionBB = llvm::BasicBlock::Create(getContext(), "", contexts.getContainingFunction());
  auto *loopBB = llvm::BasicBlock::Create(getContext());
  auto *afterLoopBB = llvm::BasicBlock::Create(getContext());
  auto *endBB = llvm::BasicBlock::Create(getContext());
  sBuilder.CreateBr(conditionBB);
  //condition bb
  sBuilder.SetInsertPoint(conditionBB);
  llvm::Value *cond;
  if (mConditionExpression) {
    auto value = mConditionExpression->codegen();
    if (auto *integerTy = dynamic_cast< IntegerType *>(value.getType())) {
      cond = sBuilder.CreateICmpNE(value.getValue(), integerTy->getDefaultValue());
    } else if (auto *floatTy = dynamic_cast< FloatingType *>(value.getType())) {
      cond = sBuilder.CreateFCmpONE(value.getValue(), floatTy->getDefaultValue());
    } else if (auto *pointerTy = dynamic_cast< PointerType *>(value.getType())) {
      cond = sBuilder.CreateICmpNE(value.getValue(), pointerTy->getDefaultValue());
    } else {
      throw SemaException("The controlling expression of an iteration statement shall have scalar type.",
                          mExpression->involvedTokens());
    }
  } else {
    cond = llvm::ConstantInt::get(IntegerType::sOneBitBoolIntType.getLLVMType(), 1);
  }
  sBuilder.CreateCondBr(cond, loopBB, endBB);
  contexts.add(std::make_unique<LoopContext>(conditionBB, endBB));
  // loop body
  contexts.getContainingFunction()->getBasicBlockList().push_back(loopBB);
  sBuilder.SetInsertPoint(loopBB);
  mStatement->codegen(contexts);
  if (!sBuilder.GetInsertBlock()->getTerminator())
    sBuilder.CreateBr(afterLoopBB);
  //after loop
  contexts.getContainingFunction()->getBasicBlockList().push_back(afterLoopBB);
  sBuilder.SetInsertPoint(afterLoopBB);
  if (mStepExpression) mStepExpression->codegen();
  if (!sBuilder.GetInsertBlock()->getTerminator())
    sBuilder.CreateBr(conditionBB);
  // end body
  contexts.getContainingFunction()->getBasicBlockList().push_back(endBB);
  sBuilder.SetInsertPoint(endBB);
}
void ContinueJumpStatementAST::codegen(StatementContexts &contexts) {
  if (sBuilder.GetInsertBlock()->getTerminator()) return;
  if (auto *context = contexts.getLastContext<LoopContext>()) {
    sBuilder.CreateBr(context->getContinueBB());
  }
}
void BreakJumpStatementAST::codegen(StatementContexts &contexts) {
  if (sBuilder.GetInsertBlock()->getTerminator()) return;
  if (auto *context = contexts.getLastContext<LoopContext>()) {
    sBuilder.CreateBr(context->getBreakBB());
  } else if (auto *context = contexts.getLastContext<SwitchContext>()) {
    sBuilder.CreateBr(context->getBreakBB());
  }
}
IBinaryOperationAST::IBinaryOperationAST() : IExpression(Kind::LOGICAL_OR_EXPRESSION) {}
CastExpressionAST::CastExpressionAST() : IExpression(Kind::CAST_EXPRESSION) {}
PostfixExpressionAST::PostfixExpressionAST() : IExpression(Kind::POSTFIX_EXPRESSION) {}
