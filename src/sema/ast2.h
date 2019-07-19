#ifndef MYCCPILER_AST2_H
#define MYCCPILER_AST2_H

#include <vector>
#include <memory>
#include <sema/operator.h>
#include <tokens/token.h>

template<typename T>
class Terminal {
 public:
  Terminal(T type, const Token &token) : token(token), type(type) {}
  const Token &token;
  const T type;
};
template<typename T>
class ts : public std::vector<Terminal<T>> {
};

// nt is short for None Terminal
template<typename T>
using nt = std::unique_ptr<T>;

template<typename T>
class nts : public std::vector<std::unique_ptr<T>> {};

class Declaration;
class Declarator;
class AssignmentExpression;

template<typename ...Ts>
class AST {
 public:
  virtual void printInternal() {
  }
  void print() {
    printInternal();
  }
};

template<size_t, class>
struct elem_type_holder;

template<typename T, typename ...Ts>
class AST<T, Ts...> : public AST<Ts...> {
 public:
  AST(T t, Ts... ts) : AST<Ts...>(ts...), member(std::move(t)) {}
  template<size_t k, class... Us>
  friend
  typename std::enable_if<
      k == 0, typename elem_type_holder<0, AST<Us...>>::type &>::type
  get(AST<Us...> &t);
  void print() {
    member.print();
    AST<Ts...>::print();
  }
 protected:
  T member;
};

template<class T, class... Ts>
struct elem_type_holder<0, AST<T, Ts...>> {
  typedef T type;
};

template<size_t k, class T, class... Ts>
struct elem_type_holder<k, AST<T, Ts...>> {
  typedef typename elem_type_holder<k - 1, AST<Ts...>>::type type;
};

template<size_t k, class... Ts>
typename std::enable_if<
    k == 0, typename elem_type_holder<0, AST<Ts...>>::type &>::type
get(AST<Ts...> &t) {
  return t.member;
}

template<size_t k, class T, class... Ts>
typename std::enable_if<
    k != 0, typename elem_type_holder<k, AST<T, Ts...>>::type &>::type
get(AST<T, Ts...> &t) {
  AST<Ts...> &base = t;
  return get<k - 1>(base);
}

class Identifier : public AST2 {};

class Expression : public AST2 {
 private:
  nts<AssignmentExpression> mAssignmentExpressions;
};

class AssignmentExpression : public Expression{
 private:

};

class DirectDeclarator : public AST2 {};

class IdentifierDirectDeclarator : public DirectDeclarator {
 private:
  nt<Identifier> mIdentifier;
};

class DeclaratorDirectDeclarator : public DirectDeclarator {
 private:
  nt<Declarator> mDeclarator;
};

class ArrayDeclarator : public DirectDeclarator {
 private:
  nt<DirectDeclarator> mDirectDeclarator;
  nt<ConstantExpression> mConstantExpression;
};

class ParameterListFunctionDeclarator : public DirectDeclarator {
 private:
  bool dotdotdot;
  nt<DirectDeclarator> mDirectDeclarator;
  nts<ParameterDeclaration> mParameterDeclarations;
};

class IdentifierListFunctionDeclarator : public DirectDeclarator {
 private:
  nt<DirectDeclarator> mDirectDeclarator;
  nts<Identifier> mIdentifiers;
};

class Pointer : public AST2 {
 private:
  nts<TypeQualifier> mTypeQualifier;
  nt<Pointer> mPointer;
};

class Declarator : public AST2 {
 private:
  nt<Pointer> mPointer;
  nt<DirectDeclarator> mDirectDeclarator;
};

class StructDeclarator : public AST2 {
  nt<Declarator> mDeclarator;
  nt<ConstantExpression> mConstantExpression;
};

class TypeSpecifier : public AST2 {};

class SpecifierQualifiers : public AST2 {
 private:
  nt<TypeSpecifier> mTypeSpecifier;
  nts<TypeQualifier> mTypeQualifiers;
};

class StructDeclaration : public AST2 {
 private:
  nt<SpecifierQualifiers> mSpecifierQualifiers;
  nts<StructDeclarator> mStructDeclarators;
};

class ProtoTypeSpecifier : public TypeSpecifier {};

class StructOrUnionSpecifier : public TypeSpecifier {
 protected:
  nt<Identifier> mIdentifier;
  nts<StructDeclaration> mStructDeclarations;
};

class StructSpecifier : public StructOrUnionSpecifier {

};

class UnionSpecifier : public StructOrUnionSpecifier {};

class EnumSpecifier : public TypeSpecifier {};

class TypedefName : public TypeSpecifier {};

class CompoundStatement : public AST2 {};

class DeclarationSpecifiers : public AST2 {
 private:
  ts<StorageSpecifier> mStorageSpecifiers;
  nt<TypeSpecifier> mTypeSpecifier;
  ts<TypeQualifier> mTypeQualifiers;
};

class ExternalDeclaration : public AST2 {};

class FunctionDefinition : public ExternalDeclaration {
 private:
  nt<DeclarationSpecifiers> mSpecifiers;
  nt<Declarator> mDeclarator;
  nts<Declaration> mDeclarations;
  nt<CompoundStatement> mCompoundStatement;
};

class Declaration : public ExternalDeclaration {};

class TranslationUnit : public AST2 {
 private:
  nts<ExternalDeclaration> mDeclarations;
};

#endif //MYCCPILER_AST2_H
