#ifndef MYCCPILER_OPERATOR_H
#define MYCCPILER_OPERATOR_H

#include <iostream>
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
  kNone,
};



#endif //MYCCPILER_OPERATOR_H
