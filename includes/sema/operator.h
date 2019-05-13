#ifndef MYCCPILER_OPERATOR_H
#define MYCCPILER_OPERATOR_H

#include <tokens/token.h>
enum class InfixOp {
  BARBAR,
  AMPAMP,
  BAR,
  CARET,
  AMP,
  EQEQ,
  BANGEQ,
  LT,
  GT,
  LTEQ,
  GTEQ,
  LTLT,
  GTGT,
  PLUS,
  SUB,
  STAR,
  SLASH,
  PERCENT,
};

enum class AssignmentOp {
  EQ,
  STAREQ,
  SLASHEQ,
  PERCENTEQ,
  PLUSEQ,
  SUBEQ,
  LTLTEQ,
  GTGTEQ,
  AMPEQ,
  CARETEQ,
  BAREQ,
};

enum class UnaryOp {
  AMP,
  STAR,
  PLUS,
  SUB,
  TILDE,
  BANG,
};

enum class ProtoTypeSpecifier {
  kVOID,
  kCHAR,
  kSHORT,
  kINT,
  kLONG,
  kFLOAT,
  kDOUBLE,
  kSIGNED,
  kUNSIGNED,
};

enum class StorageSpecifier {
  kAUTO, kREGISTER, kSTATIC, kEXTERN, kTYPEDEF,
};

enum class StructOrUnion {
  kSTRUCT,
  kUNION,
};

enum class TypeQuailifier {
  kCONST,
  kVOLATILE,
};

template<typename T>
class Operator {
 public:
  Operator(T type, const Token &token) : token(token), type(type) {}
  const Token &token;
  const T type;
};

#endif //MYCCPILER_OPERATOR_H
