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
Type *Sema::analyzePostfixExpression(PostfixExpressionAST *ast) {
  switch (ast->getProduction()) {
    case 0: {
      auto *primary = static_cast<PrimaryExpressionAST *>(ast->left.get());
      analyzePrimaryExpression(primary);
      break;
    }
    case 1: {
      // 6.5.2.1 Array subscripting
      auto *postfix = static_cast<PostfixExpressionAST *>(ast->left.get());
      analyzePostfixExpression(postfix);
      auto *exp = static_cast<ExpressionAST *>(ast->right.get());
      analyzeExpression(exp);
      auto *pointer = dynamic_cast<PointerType *>(postfix->type);
      if (!pointer) {
        throw SemaException("array left side should be pointer to complete object type",
                            postfix->involvedTokens());
      }
      auto *object = dynamic_cast<ObjectType *>(pointer->getReferencedType());
      if (!object || !object->complete()) {
        throw SemaException("array left side should be pointer to complete object type",
                            postfix->involvedTokens());
      }

      if (!(dynamic_cast<IntegerType *>(exp->type))) {
        throw SemaException("array right side should be an integer type",
                            postfix->involvedTokens());
      }
      ast->type = pointer->getReferencedType();
      break;
    }
    case 2: {
      //6.5.2.2 Function calls
      auto *postfix = static_cast<PostfixExpressionAST *>(ast->left.get());
      analyzePostfixExpression(postfix);
      auto *p = dynamic_cast<PointerType *>(postfix->type);
      auto *function = dynamic_cast<FunctionType *>(p->getReferencedType());
      if (!function) {
        throw SemaException("left side must be a function type", postfix->involvedTokens());
      }
      auto *returnType = function->getReturnType();
      if (dynamic_cast<ArrayType *>(returnType)) {
        throw SemaException("return type cannot be array type", postfix->involvedTokens());
      }
      if (dynamic_cast<FunctionType *>(returnType)) {
        throw SemaException("return type cannot be function type", postfix->involvedTokens());
      }
      auto *arguments = static_cast<ArgumentExpressionList *>(ast->right.get());
      if (function->getParameters().size() != arguments->getArgumentList().size()) {
        throw SemaException("arguments do not match function proto type", postfix->involvedTokens());
      }
      auto para = function->getParameters().begin();
      auto arg = arguments->getArgumentList().begin();
      while (para != function->getParameters().end()) {
        analyzeAssignmentExpression(arg->get());
        auto *argType = dynamic_cast<ObjectType *>((*arg)->type);
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
      ast->type = function->getReturnType();
      break;
    }
    case 3:
    case 4: {
      //6.5.2.3 Structure and union members
      auto *postfix = static_cast<PostfixExpressionAST *>(ast->left.get());
      analyzePostfixExpression(postfix);
      CompoundType *compoundType;
      if (ast->getProduction() == 3) {
        if (!dynamic_cast<StructType *>(postfix->type) && !dynamic_cast<UnionType *>(postfix->type)) {
          throw SemaException("left side must be a struct type or union type", postfix->involvedTokens());
        } else {
          compoundType = dynamic_cast<CompoundType *>(postfix->type);
        }
      } else {
        if (auto *p = dynamic_cast<PointerType *>(postfix->type)) {
          if (!dynamic_cast<StructType *>(p->getReferencedType())
              && !dynamic_cast<UnionType *>(p->getReferencedType())) {
            throw SemaException("left side must be a pointer to a struct type or union type",
                                postfix->involvedTokens());
          } else {
            compoundType = dynamic_cast<CompoundType *>(p->getReferencedType());
          }
        } else {
          throw SemaException("left side must be a pointer type", postfix->involvedTokens());
        }
      }
      auto *id = static_cast<IdentifierAST *>(ast->right.get());
      const std::string &memberName = id->token.getValue();
      ast->type = compoundType->getMember(memberName);
      if (!ast->type) {
        throw SemaException(std::string(memberName).append(" is not a member of ").append(compoundType->getTag()),
                            postfix->involvedTokens());
      }
      break;
    }
  }
  return ast->type;
}
Type *Sema::analyzePrimaryExpression(PrimaryExpressionAST *ast) {
  switch (ast->getProduction()) {
    case 0: { // identifier
      auto *identifierAST = static_cast<IdentifierAST *>(ast->ast.get());
      auto *symbol = table->lookup(identifierAST->token);
      SymbolKind kind = symbol->getKind();
      if (kind == SymbolKind::OBJECT) {
        ast->mType = static_cast<ObjectSymbol *>(symbol)->getType();
      } else if (kind == SymbolKind::FUNCTION) {
        ast->mType = static_cast<FunctionSymbol *>(symbol)->convertToPointer();
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

      IntegerType *uIntType = IntegerType::getIntegerType(false, false, false, IntegerType::Kind::kInt);
      IntegerType *uLongType = IntegerType::getIntegerType(false, false, false, IntegerType::Kind::kLongInt);
      IntegerType *uLongLongType = IntegerType::getIntegerType(false, false, false, IntegerType::Kind::kLongLongInt);

      unsigned int intSize = uIntType->getSizeInBits();
      unsigned int longSize = uLongType->getSizeInBits();
      unsigned int longLongSize = uLongLongType->getSizeInBits();

      bool isSigned;
      IntegerType::Kind kind;

      switch (integerConstAST->suffix) {
        case IntegerConstantAST::Suffix::None:
          if (isBase10) {
            isSigned = true;
            if (!(n >> (intSize - 1))) {
              kind = IntegerType::Kind::kInt;
            } else if (!(n >> (longSize - 1))) {
              kind = IntegerType::Kind::kLongInt;
            } else {
              kind = IntegerType::Kind::kLongLongInt;
            }
          } else {
            if (!(n >> (intSize - 1))) {
              isSigned = true;
              kind = IntegerType::Kind::kInt;
            } else if (!(n >> (intSize))) {
              isSigned = false;
              kind = IntegerType::Kind::kInt;
            } else if (!(n >> (longSize - 1))) {
              isSigned = true;
              kind = IntegerType::Kind::kLongInt;
            } else if (!(n >> (longSize))) {
              isSigned = false;
              kind = IntegerType::Kind::kLongInt;
            } else {
              isSigned = !(n >> (longLongSize - 1));
              kind = IntegerType::Kind::kLongLongInt;
            }
          }
          break;
        case IntegerConstantAST::Suffix::U:isSigned = false;
          if (!(n >> (intSize - 1))) {
            kind = IntegerType::Kind::kInt;
          } else if (!(n >> (longSize - 1))) {
            kind = IntegerType::Kind::kLongInt;
          } else {
            kind = IntegerType::Kind::kLongLongInt;
          }
          break;
        case IntegerConstantAST::Suffix::L:
          if (isBase10) {
            isSigned = true;
            if (!(n >> (longSize - 1))) {
              kind = IntegerType::Kind::kLongInt;
            } else {
              kind = IntegerType::Kind::kLongLongInt;
            }
          } else {
            if (!(n >> (longSize - 1))) {
              isSigned = true;
              kind = IntegerType::Kind::kLongInt;
            } else if (!(n >> (longSize))) {
              isSigned = false;
              kind = IntegerType::Kind::kLongInt;
            } else {
              isSigned = !(n >> (longLongSize - 1));
              kind = IntegerType::Kind::kLongLongInt;
            }
          }
          break;
        case IntegerConstantAST::Suffix::UL:isSigned = false;
          if (!(n >> (longSize - 1))) {
            kind = IntegerType::Kind::kLongInt;
          } else {
            kind = IntegerType::Kind::kLongLongInt;
          }
          break;
        case IntegerConstantAST::Suffix::LL:
          if (isBase10) {
            isSigned = true;
            kind = IntegerType::Kind::kLongLongInt;
          } else {
            isSigned = !(n >> (longLongSize - 1));
            kind = IntegerType::Kind::kLongLongInt;
          }
          break;
        case IntegerConstantAST::Suffix::ULL:isSigned = false;
          kind = IntegerType::Kind::kLongLongInt;
          break;
      }
      ast->type = IntegerType::getIntegerType(isSigned, true, false, kind);
    }
    case 2: {
      auto *floatingConstAST = static_cast<FloatingConstantAST *>(ast->ast.get());
      FloatingType::Kind kind;
      switch (floatingConstAST->mSuffix) {
        case FloatingConstantAST::Suffix::None:kind = FloatingType::Kind::kDouble;
          break;
        case FloatingConstantAST::Suffix::F:kind = FloatingType::Kind::kFloat;
          break;
        case FloatingConstantAST::Suffix::L:kind = FloatingType::Kind::kLongDouble;
          break;
      }
      ast->type = FloatingType::getFloatingType(true, false, kind);
      break;
    }
    case 3:ast->type = IntegerType::getIntegerType(false, true, false, IntegerType::Kind::kChar);
    case 4: {
      auto *stringAST = static_cast<StringAST *>(ast->ast.get());
      ast->type = stringAST->mType.get();
      break;
    }
    case 5: {
      auto *exp = static_cast<ExpressionAST *>(ast->ast.get());
      ast->type = exp->type;
      break;
    }
    default:break;
  }
  return ast->type;
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
