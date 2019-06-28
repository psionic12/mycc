#ifndef MYCCPILER_TYPES_H
#define MYCCPILER_TYPES_H

#include <vector>
#include <string>
#include <memory>
class Type {
 public:
  virtual bool equals(Type *type) {
    return true;
  };
};

class ObjectType : public Type {
 public:
  virtual bool complete() {
    return true;
  }
};

class IntegerType : public ObjectType {
 public:
  enum class Kind {
    kChar,
    kShortInt,
    kInt,
    kLongInt,
    kLongLongInt,
  };
  IntegerType(Kind kind) : kind_(kind) {}
  bool equals(Type *type) override {
    auto *it = dynamic_cast<IntegerType *>(type);
    return it
        && it->kind_ == this->kind_
        && it->signed_ == this->signed_
        && ObjectType::equals(type);
  }
 private:
  bool signed_;
  Kind kind_;
};

class FloatingType : public ObjectType {
 public:
  enum class Kind {
    kFloat,
    kDouble,
    kLongDouble,
  };
  FloatingType(Kind kind_) : kind_(kind_) {}
 private:
  Kind kind_;
};

class VoidType : public ObjectType {
 public:
  bool complete() override {
    return false;
  }
  bool equals(Type *type) override {
    auto *v = dynamic_cast<VoidType *>(type);
    return v;
  }
};

class FunctionType : public Type {
 public:
  FunctionType(ObjectType *returnType_, std::vector<ObjectType *> &&parameters_)
      : returnType_(returnType_), parameters_(parameters_) {}
  bool equals(Type *type) override {
    auto *ft = dynamic_cast<FunctionType *>(type);
    if (!ft) return false;
    if (!returnType_->equals(ft->returnType_)) return false;
    if (parameters_.size() != ft->parameters_.size()) return false;
    auto sit = parameters_.begin();
    auto rit = ft->parameters_.begin();
    while (sit != parameters_.end()) {
      if (!(*sit)->equals(*rit)) return false;
      ++sit;
      ++rit;
    }
    return true;
  }
 private:
  ObjectType *returnType_;
  std::vector<ObjectType *> parameters_;
};

//TODO
class EnumerationType : public Type {

};

class ArrayType : public ObjectType {
 public:
  ArrayType(ObjectType *elementType) : elementType_(elementType) {}
  ArrayType(ObjectType *elementType, int size) : elementType_(elementType), size_(size) {}
  bool equals(Type *type) override {
    auto *at = dynamic_cast<ArrayType *>(type);
    return at
        && at->elementType_->equals(at->elementType_);
  }
  bool complete() override {
    return size_ > 0;
  }
  void setSize(unsigned int size) {
    size_ = size;
  }
 private:
  ObjectType *elementType_;
  //TODO is int enough?
  unsigned int size_ = 0;
};

//TODO
class StructType : public Type {

};

//TODO
class UnionType : public Type {

};

class PointerType : public ObjectType {
 public:
  PointerType(Type *referencedType) : referencedType_(referencedType) {}
  bool equals(Type *type) override {
    auto *pt = dynamic_cast<PointerType *>(type);
    return pt && this->referencedType_->equals(pt->referencedType_);
  }
 private:
  Type *referencedType_;
};
#endif //MYCCPILER_TYPES_H
