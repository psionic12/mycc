#ifndef MYCCPILER_OPERATOR_H
#define MYCCPILER_OPERATOR_H
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
  kINT,
  kSHORT,
  kLONG,
  kSIGNED,
  kUNSIGNED,
  kFLOAT,
  kDOUBLE,
};

enum class StorageSpecifier {
  kAUTO, kREGISTER, kSTATIC, kEXTERN, kTYPEDEF,
};

enum class StructOrUnion {
  kSTRUCT,
  kUNION,
};

enum class TypeQualifier {
  kCONST,
  kVOLATILE,
};



#endif //MYCCPILER_OPERATOR_H
