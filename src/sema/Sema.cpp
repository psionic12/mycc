#include <sema/Sema.h>
void Sema::analyze() {
  analyzeTranslationUnitAST(root.get());
}
void Sema::analyzeTranslationUnitAST(TranslationUnitAST *ast) {
  mObjectTable = &ast->mObjectTable;
  mTagTable = &ast->mTagTable;
  for (const auto &ds : ast->external_declarations) {
    analyzeExternalDeclaration(ds.get());
  }
}
void Sema::analyzeExternalDeclaration(ExternalDeclarationAST *ast) {
  if (ast->def->getKind() == AST::Kind::FUNCTION_DEFINITION) {
    analyzeFunctionDefinition(static_cast<FunctionDefinitionAST *>(ast->def.get()));
  } else {
    analyzeDeclaration(static_cast<DeclarationAST *>(ast->def.get()));
  }
}
void Sema::analyzeFunctionDefinition(FunctionDefinitionAST *ast) {
  mLabelTable = &ast->mLabelTable;
  for (const auto &specifier : ast->declaration_spcifiers->storage_specifiers) {
    if (specifier.type != StorageSpecifier::kEXTERN && specifier.type != StorageSpecifier::kSTATIC) {
      throw SemaException("declaration specifiers shall be either extern or static", specifier.token);
    }
  }
  mLabelTable = nullptr;
}
void Sema::analyzeIdentifier(IdentifierAST *ast) {

}
void Sema::analyzeDeclarationSpecifiers(DeclarationSpecifiersAST *ast) {
  if (ast->storage_specifiers.size() > 1) {
    throw SemaException(
        "At most, one storage-class specifier may be given in the declaration specifiers in a declaration",
        ast->storage_specifiers.back().token);
  }
  //TODO The declaration of an identifier for a function that has block scope shall have no explicit storage-class specifier other than extern.
  //TODO If an aggregate or union object is declared with a storage-class specifier other than typedef, the properties resulting from the storage-class specifier, except with respect to linkage, also apply to the members of the object, and so on recursively for any aggregate or union member objects.
  //TODO At least one type specifier shall be given in the declaration specifiers in each declaration,  and in the specifier-qualifier list in each struct declaration and type name.
  auto it = ast->type_specifiers.begin();
  switch (it->get()->getProduction()) {
    case 0 ://void
      break;
    case 7 :// float
      break;
    case 9 :// strcut or union
      break;
    case 10:// enum
      break;
    case 11:// typedef
      break;
    default:break;

  }
}
void Sema::analyzeStorageClassSpecifier(StorageClassSpecifierAST *ast) {

}
void Sema::analyzeTypeSpecifier(TypeSpecifierAST *ast) {

}
void Sema::analyzeStructOrUnionSpecifier(StructOrUnionSpecifierAST *ast) {

}
void Sema::analyzeStructDeclaration(StructDeclarationAST *ast) {

}
void Sema::analyzeSpecifierQualifier(SpecifierQualifierAST *ast) {

}
void Sema::analyzeStructDeclaratorList(StructDeclaratorListAST *ast) {

}
void Sema::analyzeStructDeclarator(StructDeclaratorAST *ast) {

}
void Sema::analyzeDeclarator(DeclaratorAST *ast) {

}
void Sema::analyzePointer(PointerAST *ast) {

}
void Sema::analyzeTypeQualifier(TypeQualifierAST *ast) {

}
void Sema::analyzeDirectDeclarator(DirectDeclaratorAST *ast) {

}
void Sema::analyzeConstantExpression(ConstantExpressionAST *ast) {

}
void Sema::analyzeConditionalExpression(ConditionalExpressionAST *ast) {

}
void Sema::analyzeLogicalOrExpression(LogicalOrExpressionAST *ast) {

}
void Sema::analyzeCastExpression(CastExpressionAST *ast) {

}
void Sema::analyzeUnaryExpression(UnaryExpressionAST *ast) {
  switch (ast->getProduction()) {
    case 0: {
      auto *postfix = ast->postfix_expression.get();
      analyzePostfixExpression(postfix);
      ast->mType = postfix->mType;
      ast->mQualifiers = postfix->mQualifiers;
      ast->mLvalue = postfix->mLvalue;
      break;
    }
    case 1:
    case 2: {
      //6.5.3.1 Prefix increment and decrement operators
      auto *unary = ast->unary_expression.get();
      analyzeUnaryExpression(unary);
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
      ast->mType = unary->mType;
      ast->mLvalue = unary->mLvalue;
      ast->mQualifiers = unary->mQualifiers;
      break;
    }
    case 3: {
      //6.5.3.2 Address and indirection operators
      auto op = ast->op->type;
      auto *cast_exp = ast->cast_expression.get();
      analyzeCastExpression(cast_exp);
      switch (op) {
        case UnaryOp::AMP:break;
      }
    }
  }
}
void Sema::analyzePostfixExpression(PostfixExpressionAST *ast) {
  switch (ast->getProduction()) {
    case 0: {
      auto *primary = static_cast<PrimaryExpressionAST *>(ast->left.get());
      analyzePrimaryExpression(primary);
      ast->mType = primary->mType;
      ast->mQualifiers = primary->mQualifiers;
      ast->mLvalue = primary->mLvalue;
      break;
    }
    case 1: {
      // 6.5.2.1 Array subscripting
      auto *postfix = static_cast<PostfixExpressionAST *>(ast->left.get());
      analyzePostfixExpression(postfix);
      auto *exp = static_cast<ExpressionAST *>(ast->right.get());
      analyzeExpression(exp);
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
      ast->mType = tPointer->getReferencedType();
      ast->mQualifiers = tPointer->qualifersToReferencedType();
      ast->mLvalue = true;
      break;
    }
    case 2: {
      //6.5.2.2 Function calls
      auto *postfix = static_cast<PostfixExpressionAST *>(ast->left.get());
      analyzePostfixExpression(postfix);
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
      auto *arguments = static_cast<ArgumentExpressionList *>(ast->right.get());
      if (tFunction->getParameters().size() != arguments->mArgumentList.size()) {
        throw SemaException("arguments do not match function proto type", postfix->involvedTokens());
      }
      auto para = tFunction->getParameters().begin();
      auto arg = arguments->mArgumentList.begin();
      while (para != tFunction->getParameters().end()) {
        analyzeAssignmentExpression(arg->get());
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
      ast->mType = tFunction->getReturnType();
      ast->mLvalue = false;
      break;
    }
    case 3:
    case 4: {
      //6.5.2.3 Structure and union members
      auto *postfix = static_cast<PostfixExpressionAST *>(ast->left.get());
      analyzePostfixExpression(postfix);
      const CompoundType *tCompoundType;
      if (ast->getProduction() == 3) {
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
      auto *id = static_cast<IdentifierAST *>(ast->right.get());
      const std::string &memberName = id->token.getValue();
      std::tie(ast->mType, ast->mQualifiers) = tCompoundType->getMember(memberName);
      if (!ast->mType) {
        throw SemaException(std::string(memberName).append(" is not a member of ").append(tCompoundType->getTag()),
                            postfix->involvedTokens());
      }
      ast->mLvalue = postfix->mLvalue;
      ast->mQualifiers.insert(postfix->mQualifiers.begin(), postfix->mQualifiers.end());
      ast->mLvalue = postfix->mLvalue;
      break;
    }
    case 5:
    case 6: {
      // 6.5.2.4 Postfix increment and decrement operators
      auto *postfix = static_cast<PostfixExpressionAST *>(ast->left.get());
      analyzePostfixExpression(postfix);
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
      ast->mType = postfix->mType;
      ast->mLvalue = postfix->mLvalue;
      ast->mQualifiers = postfix->mQualifiers;
      break;
    }
  }
}
void Sema::analyzePrimaryExpression(PrimaryExpressionAST *ast) {
  switch (ast->getProduction()) {
    case 0: { // identifier
      auto *identifierAST = static_cast<IdentifierAST *>(ast->ast.get());
      auto *symbol = mObjectTable->lookup(identifierAST->token);
      SymbolKind kind = symbol->getKind();
      if (kind == SymbolKind::OBJECT) {
        ast->mType = static_cast<ObjectSymbol *>(symbol)->getType();
        ast->mQualifiers = static_cast<ObjectSymbol *>(symbol)->getQualifiers();
        ast->mLvalue = true;
      } else if (kind == SymbolKind::ENUMERATION_CONSTANT) {
        ast->mType = static_cast<EnumConstSymbol *>(symbol)->getType();
      } else {
        throw SemaException("identifier as a primary expression must be object or function or enumeration const",
                            identifierAST->token);
      }
      break;
    }
    case 1: {
      auto *integerConstAST = static_cast<IntegerConstantAST *>(ast->ast.get());
      bool isBase10 = integerConstAST->mToken.getValue()[0] != 0;
      unsigned long long int n = integerConstAST->value;

      unsigned int intSize = IntegerType::sIntType.getSizeInBits();
      unsigned int longSize = IntegerType::sLongIntType.getSizeInBits();
      unsigned int longLongSize = IntegerType::sLongLongIntType.getSizeInBits();

      switch (integerConstAST->suffix) {
        case IntegerConstantAST::Suffix::None:
          if (isBase10) {
            if (!(n >> (intSize - 1))) {
              ast->mType = &IntegerType::sIntType;
            } else if (!(n >> (longSize - 1))) {
              ast->mType = &IntegerType::sLongIntType;
            } else {
              ast->mType = &IntegerType::sLongLongIntType;
            }
          } else {
            if (!(n >> (intSize - 1))) {
              ast->mType = &IntegerType::sIntType;
            } else if (!(n >> (intSize))) {
              ast->mType = &IntegerType::sUnsignedIntType;
            } else if (!(n >> (longSize - 1))) {
              ast->mType = &IntegerType::sLongIntType;
            } else if (!(n >> (longSize))) {
              ast->mType = &IntegerType::sUnsignedLongIntType;
            } else {
              ast->mType =
                  !(n >> (longLongSize - 1)) ? &IntegerType::sLongLongIntType : &IntegerType::sUnsignedLongLongIntType;
            }
          }
          break;
        case IntegerConstantAST::Suffix::U:
          if (!(n >> (intSize - 1))) {
            ast->mType = &IntegerType::sUnsignedIntType;
          } else if (!(n >> (longSize - 1))) {
            ast->mType = &IntegerType::sUnsignedLongIntType;
          } else {
            ast->mType = &IntegerType::sUnsignedLongLongIntType;
          }
          break;
        case IntegerConstantAST::Suffix::L:
          if (isBase10) {
            if (!(n >> (longSize - 1))) {
              ast->mType = &IntegerType::sLongIntType;
            } else {
              ast->mType = &IntegerType::sLongLongIntType;
            }
          } else {
            if (!(n >> (longSize - 1))) {
              ast->mType = &IntegerType::sLongIntType;
            } else if (!(n >> (longSize))) {
              ast->mType = &IntegerType::sUnsignedLongIntType;
            } else {
              ast->mType =
                  !(n >> (longLongSize - 1)) ? &IntegerType::sLongLongIntType : &IntegerType::sUnsignedLongLongIntType;
            }
          }
          break;
        case IntegerConstantAST::Suffix::UL:
          if (!(n >> (longSize - 1))) {
            ast->mType = &IntegerType::sUnsignedLongIntType;
          } else {
            ast->mType = &IntegerType::sUnsignedLongLongIntType;
          }
          break;
        case IntegerConstantAST::Suffix::LL:
          if (isBase10) {
            ast->mType = &IntegerType::sLongLongIntType;
          } else {
            ast->mType =
                !(n >> (longLongSize - 1)) ? &IntegerType::sLongLongIntType : &IntegerType::sUnsignedLongLongIntType;
          }
          break;
        case IntegerConstantAST::Suffix::ULL:ast->mType = &IntegerType::sUnsignedLongLongIntType;
          break;
      }
    }
    case 2: {
      auto *floatingConstAST = static_cast<FloatingConstantAST *>(ast->ast.get());
      switch (floatingConstAST->mSuffix) {
        case FloatingConstantAST::Suffix::None:ast->mType = &FloatingType::sDoubleType;
          break;
        case FloatingConstantAST::Suffix::F: ast->mType = &FloatingType::sFloatType;
          break;
        case FloatingConstantAST::Suffix::L: ast->mType = &FloatingType::sLongDoubleType;
          break;
      }
      break;
    }
    case 3:ast->mType = &IntegerType::sCharType;
    case 4: {
      auto *stringAST = static_cast<StringAST *>(ast->ast.get());
      ast->mType = stringAST->mType.get();
      ast->mQualifiers.emplace(TypeQualifier::kCONST);
      ast->mLvalue = true;
      break;
    }
    case 5: {
      auto *exp = static_cast<ExpressionAST *>(ast->ast.get());
      ast->mType = exp->mType;
      ast->mQualifiers = exp->mQualifiers;
      ast->mLvalue = exp->mLvalue;
      break;
    }
    default:break;
  }
}
void Sema::analyzeExpression(ExpressionAST *ast) {

}
void Sema::analyzeAssignmentExpression(AssignmentExpressionAST *ast) {

}
void Sema::analyzeTypeName(TypeNameAST *ast) {

}
void Sema::analyzeParameterTypeList(ParameterTypeListAST *ast) {

}
void Sema::analyzeParameterList(ParameterListAST *ast) {
  SymbolScope s(mObjectTable, &ast->mObjectTable);
}
void Sema::analyzeParameterDeclaration(ParameterDeclarationAST *ast) {

}
void Sema::analyzeEnumSpecifier(EnumSpecifierAST *ast) {

}
void Sema::analyzeEnumeratorList(EnumeratorListAST *ast) {

}
void Sema::analyzeEnumerator(EnumeratorAST *ast) {

}
void Sema::analyzeTypedefName(TypedefNameAST *ast) {

}
void Sema::analyzeDeclaration(DeclarationAST *declaration) {
  analyzeDeclarationSpecifiers(declaration->declaration_specifiers.get());
  analyzeInitDeclarators(declaration->init_declarators);
  bool hasDeclarator = !declaration->init_declarators.empty();
  bool hasTag = false;
  bool hasEnumMembers = false;
  for (auto &type_specifier : declaration->declaration_specifiers->type_specifiers) {
    if (type_specifier->getProduction() == 9) {
      hasTag = static_cast<StructOrUnionSpecifierAST *> (type_specifier->specifier.get())->id != nullptr;
    } else if (type_specifier->getProduction() == 10) {
      auto enum_list = static_cast<EnumSpecifierAST *> (type_specifier->specifier.get())->enum_list.get();
      hasEnumMembers = enum_list && !enum_list->enumerators.empty();
    }
  }

  if (!hasDeclarator && !hasTag && !hasEnumMembers) {
    throw SemaException("A declaration shall declare at least a declarator, a tag, or the members of an enumeration",
                        *declaration->mLeftMost);
  }
  //TODO 6.7.3
}
void Sema::analyzeInitDeclarators(const InitDeclarators &initDeclarators) {

}
void Sema::analyzeInitializer(InitializerAST *ast) {

}
void Sema::analyzeInitializerList(InitializerListAST *ast) {

}
void Sema::analyzeCompoundStatement(CompoundStatementAST *ast) {
  SymbolScope s1(mObjectTable, &ast->mObjectTable);
  SymbolScope s2(mTagTable, &ast->mTagTable);
}
void Sema::analyzeStatement(StatementAST *ast) {

}
void Sema::analyzeLabeledStatement(LabeledStatementAST *ast) {

}
void Sema::analyzeExpressionStatement(ExpressionStatementAST *ast) {

}
void Sema::analyzeSelectionStatement(SelectionStatementAST *ast) {

}
void Sema::analyzeIterationStatement(IterationStatementAST *ast) {

}
void Sema::analyzeJumpStatement(JumpStatementAST *ast) {

}
void Sema::analyzeString(StringAST *ast) {

}
