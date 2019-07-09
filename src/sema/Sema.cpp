#include <sema/Sema.h>
void Sema::analyze() {
  analyzeTranslationUnitAST(root.get());
}
void Sema::analyzeTranslationUnitAST(TranslationUnitAST *ast) {
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
  for (const auto &specifier : ast->declaration_spcifiers->storage_specifiers) {
    if (specifier.type != StorageSpecifier::kEXTERN && specifier.type != StorageSpecifier::kSTATIC) {
      throw SemaException("declaration specifiers shall be either extern or static", specifier.token);
    }
  }

}
void Sema::analyzeIdentifier(IdentifierAST *ast) {

}
void Sema::analyzeDeclarationSpecifiers(DeclarationSpecifiersAST *ast) {

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

}
void Sema::analyzePostfixExpression(PostfixExpressionAST *ast) {
  switch (ast->getProduction()) {
    case 0: {
      auto *primary = static_cast<PrimaryExpressionAST *>(ast->left.get());
      analyzePrimaryExpression(primary);
      ast->mType = primary->mType;
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
      ast->qualifiers = tPointer->qualifersToReferencedType();
      break;
    }
    case 2: {
      //6.5.2.2 Function calls
      auto *postfix = static_cast<PostfixExpressionAST *>(ast->left.get());
      analyzePostfixExpression(postfix);
      auto *p = dynamic_cast<const PointerType *>(postfix->mType);
      auto *tFunction = dynamic_cast<FunctionType *>(p->getReferencedType());
      if (!tFunction) {
        throw SemaException("left side must be a function type", postfix->involvedTokens());
      }
      auto *returnType = tFunction->getReturnType();
      if (dynamic_cast<ArrayType *>(returnType)) {
        throw SemaException("return type cannot be array type", postfix->involvedTokens());
      }
      if (dynamic_cast<FunctionType *>(returnType)) {
        throw SemaException("return type cannot be function type", postfix->involvedTokens());
      }
      auto *arguments = static_cast<ArgumentExpressionList *>(ast->right.get());
      if (tFunction->getParameters().size() != arguments->getArgumentList().size()) {
        throw SemaException("arguments do not match function proto type", postfix->involvedTokens());
      }
      auto para = tFunction->getParameters().begin();
      auto arg = arguments->getArgumentList().begin();
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
          if (!dynamic_cast<StructType *>(p->getReferencedType())
              && !dynamic_cast<UnionType *>(p->getReferencedType())) {
            throw SemaException("left side must be a pointer to a struct type or union type",
                                postfix->involvedTokens());
          } else {
            tCompoundType = dynamic_cast<CompoundType *>(p->getReferencedType());
          }
        } else {
          throw SemaException("left side must be a pointer type", postfix->involvedTokens());
        }
      }
      auto *id = static_cast<IdentifierAST *>(ast->right.get());
      const std::string &memberName = id->token.getValue();
      ast->mType = tCompoundType->getMember(memberName);
      if (!ast->mType) {
        throw SemaException(std::string(memberName).append(" is not a member of ").append(tCompoundType->getTag()),
                            postfix->involvedTokens());
      }
      ast->lvalue = postfix->lvalue;

      break;
    }
  }
}
void Sema::analyzePrimaryExpression(PrimaryExpressionAST *ast) {
  switch (ast->getProduction()) {
    case 0: { // identifier
      auto *identifierAST = static_cast<IdentifierAST *>(ast->ast.get());
      auto *symbol = table->lookup(identifierAST->token);
      SymbolKind kind = symbol->getKind();
      if (kind == SymbolKind::OBJECT) {
        ast->mType = static_cast<ObjectSymbol *>(symbol)->getType();
        ast->qualifiers = static_cast<ObjectSymbol *>(symbol)->getQualifiers();
        ast->lvalue = true;
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
      ast->qualifiers.emplace(TypeQualifier::kCONST);
      ast->lvalue = true;
      break;
    }
    case 5: {
      auto *exp = static_cast<ExpressionAST *>(ast->ast.get());
      ast->mType = exp->mType;
      ast->qualifiers = exp->qualifiers;
      ast->lvalue = exp->lvalue;
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

}
void Sema::analyzeInitDeclarators(InitDeclarators *ast) {

}
void Sema::analyzeInitializer(InitializerAST *ast) {

}
void Sema::analyzeInitializerList(InitializerListAST *ast) {

}
void Sema::analyzeCompoundStatement(CompoundStatementAST *ast) {

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
