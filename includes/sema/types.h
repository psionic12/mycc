#ifndef MYCCPILER_TYPES_H
#define MYCCPILER_TYPES_H

#include <memory>
#include <vector>
#include <string>
#include "operator.h"

namespace mycc {
enum class TypeKind {
  kChar,
  kShortInt,
  kInt,
  kLongInt,
  kLongLongInt,
  kFloat,
  kDouble,
  kLongDouble,
  kVoid,
  kArray,
  kStructure,
  kEnumerated,
  kUnion,
  kFuncion,
  kPointer,
};

class Type {
 public:
  Type(TypeKind kind, bool is_const, bool is_volatile) : kind(kind), is_const(is_const), is_volatile(is_volatile) {}
  virtual bool isComplete() = 0;
  virtual bool operator==(const Type &operand) {
    return (this->kind == operand.kind
        && this->is_const == operand.is_const && this->is_volatile
        && operand.is_volatile);
  }
  bool operator!=(const Type &operand) {
    return !operator==(operand);
  }
  TypeKind getTypeKind() {
    return kind;
  }
 private:
  TypeKind kind;

  bool is_const;
  bool is_volatile;
};

class IntegerType : public Type {
 public:
  IntegerType(TypeKind kind, bool is_const, bool is_volatile, bool is_signed) : Type(kind, is_const, is_volatile),
                                                                                is_signed(is_signed) {}
  bool isComplete() override {
    return true;
  }
  bool operator==(const Type &operand) override {
    return Type::operator==(operand) && this->is_signed == static_cast<const IntegerType &>(operand).is_signed;
  }
 private:
  bool is_signed;
};

class RealFloatingType : public Type {
 public:
  RealFloatingType(TypeKind kind, bool is_const, bool is_volatile) : Type(kind, is_const, is_volatile) {}
  bool isComplete() override {
    return true;
  }
};

class VoidType : public Type {
 public:
  VoidType(bool is_const, bool is_volatile) : Type(TypeKind::kVoid, is_const, is_volatile) {}
  bool isComplete() override {
    return false;
  }
};

class ArrayType : public Type {
 public:
  ArrayType(std::unique_ptr<Type> member, int size, bool is_const, bool is_volatile)
      : Type(TypeKind::kArray, is_const, is_volatile), size_(size), member(std::move(member)) {}
  bool isComplete() override {
    return size_ != 0;
  }
  int size() {
    return size_;
  }
  bool operator==(const Type &operand) override {
    return Type::operator==(operand)
        && this->size_ == static_cast<const ArrayType &>(operand).size_
        && this->member == static_cast<const ArrayType &>(operand).member;
  }
 private:
  int size_;
  std::unique_ptr<Type> member;
};

class StructureType : public Type {
 public:
  StructureType(std::string name,
                std::vector<std::unique_ptr<Type>> members,
                bool complete,
                bool is_const,
                bool is_volatile)
      : Type(TypeKind::kStructure, is_const, is_volatile),
        name(std::move(name)),
        complete(complete),
        members(std::move(members)) {}
  bool isComplete() override {
    return complete;
  }
  bool operator==(const Type &operand) override {
    return Type::operator==(operand) && this->name == static_cast<const StructureType &>(operand).name;
  }
 private:
  std::string name;
  bool complete;
  std::vector<std::unique_ptr<Type>> members;
};

class UnionType : public Type {
 public:
  UnionType(std::string name,
            std::vector<std::unique_ptr<Type>> members,
            bool complete,
            bool is_const,
            bool is_volatile)
      : Type(TypeKind::kUnion, is_const, is_volatile),
        name(std::move(name)),
        complete(complete),
        members(std::move(members)) {}
  bool isComplete() override {
    return complete;
  }
  bool operator==(const Type &operand) override {
    return Type::operator==(operand) && this->name == static_cast<const UnionType &>(operand).name;
  }
 private:
  std::string name;
  std::vector<std::unique_ptr<Type>> members;
  bool complete;
};

class EnumeratedType : public Type {
 public:
  EnumeratedType(std::string name,
                 std::vector<std::pair<std::string, int>> named_values,
                 bool is_const,
                 bool is_volatile)
      : Type(TypeKind::kEnumerated, is_const, is_volatile), name(std::move(name)), named_values(std::move(named_values)) {}
  bool isComplete() override {
    return true;
  }
  bool operator==(const Type &operand) override {
    return Type::operator==(operand) && this->name == static_cast<const EnumeratedType &>(operand).name;
  }
 private:
  std::string name;
  std::vector<std::pair<std::string, int>> named_values;
};

class FunctionType : public Type {
 public:
  FunctionType(std::vector<std::unique_ptr<Type>> parameters,
               std::unique_ptr<Type> return_type,
               bool is_const,
               bool is_volatile)
      : Type(TypeKind::kFuncion, is_const, is_volatile),
        parameters(std::move(parameters)), return_type(std::move(return_type)) {
  }
  bool operator==(const Type &operand) override {
    return Type::operator==(operand) && this->return_type == static_cast<const FunctionType &>(operand).return_type
        && this->parameters == static_cast<const FunctionType &>(operand).parameters;
  }
 private:
  std::vector<std::unique_ptr<Type>> parameters;
  std::unique_ptr<Type> return_type;
};

class Pointer : public Type {
 public:
  Pointer(std::unique_ptr<Type> member, bool is_const, bool is_volatile) : Type(TypeKind::kPointer,
                                                                                is_const,
                                                                                is_volatile),
                                                                           member(std::move(member)) {}
  bool isComplete() override {
    return false;
  }
  bool operator==(const Type &operand) override {
    return Type::operator==(operand) && this->member == static_cast<const Pointer &>(operand).member;
  }
 private:
  std::unique_ptr<Type> member;
};
} //namespace mycc

#endif //MYCCPILER_TYPES_H
