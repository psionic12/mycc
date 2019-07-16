#ifndef MYCCPILER_SPECIFIER_COMBINATION_H
#define MYCCPILER_SPECIFIER_COMBINATION_H
#include <vector>
#include "token.h"
#include "combination_kind.h"
class CheckBox {
 public:
  CheckBox(TokenKind kind) : mKind(kind) {}
  const TokenKind getKind() const {
    return mKind;
  }
  bool isChecked() const {
    return mChecked;
  }
  void setChecked() {
    mChecked = true;
  }
  std::string dump() const {
    return std::string(Token::enumToString(mKind)) + "[" + (mChecked ? "1" : "0") + "]";
  }
 private:
  const TokenKind mKind;
  bool mChecked = false;
};

class Combination {
 public:
  Combination(CombinationKind kind, std::initializer_list<CheckBox> list) : mKind(kind), mCheckBoxes(list) {}
  // negative for not matched, 0 for matched, positive for perfect matched
  void matches(TokenKind kind) {
    for (auto &checkBox : mCheckBoxes) {
      if (kind == checkBox.getKind() && !checkBox.isChecked()) {
        checkBox.setChecked();
        ++mMatched;
        return;
      }
    }
    mMatched = -1;
  };
  std::vector<CheckBox>::size_type size() const {
    return mCheckBoxes.size();
  }
  CombinationKind getKind() const {
    return mKind;
  }
  std::string dump() const {
    std::string s("(");
    s += std::to_string(mMatched);
    s += ")";
    s += enumToString(mKind);
    s += ":";
    for (const auto &checkBox: mCheckBoxes) {
      s += checkBox.dump() + "->";
    }
    return s;
  }
  static const char *enumToString(CombinationKind kind) {
    switch (kind) {
      case CombinationKind::kUnknown:return "kUnknown";
      case CombinationKind::kVoid:return "kVoid";
      case CombinationKind::kChar:return "kChar";
      case CombinationKind::kSignedChar:return "kSignedChar";
      case CombinationKind::kUnsignedChar:return "kUnsignedChar";
      case CombinationKind::kShort:return "kShort";
      case CombinationKind::kSignedShort:return "kSignedShort";
      case CombinationKind::kShortInt:return "kShortInt";
      case CombinationKind::kSignedShortInt:return "kSignedShortInt";
      case CombinationKind::kUnsignedShort:return "kUnsignedShort";
      case CombinationKind::kUnsignedShortInt:return "kUnsignedShortInt";
      case CombinationKind::kInt:return "kInt";
      case CombinationKind::kSigned:return "kSigned";
      case CombinationKind::kSignedInt:return "kSignedInt";
      case CombinationKind::kUnsigned:return "kUnsigned";
      case CombinationKind::kUnsignedInt:return "kUnsignedInt";
      case CombinationKind::kLong:return "kLong";
      case CombinationKind::kSignedLong:return "kSignedLong";
      case CombinationKind::kLongInt:return "kLongInt";
      case CombinationKind::kSignedLongInt:return "kSignedLongInt";
      case CombinationKind::kUnsignedLong:return "kUnsignedLong";
      case CombinationKind::kUnsignedLongInt:return "kUnsignedLongInt";
      case CombinationKind::kLongLong:return "kLongLong";
      case CombinationKind::kSignedLongLong:return "kSignedLongLong";
      case CombinationKind::kLongLongInt:return "kLongLongInt";
      case CombinationKind::kSignedLongLongInt:return "kSignedLongLongInt";
      case CombinationKind::kUnsignedLongLong:return "kUnsignedLongLong";
      case CombinationKind::kUnsignedLongLongInt:return "kUnsignedLongLongInt";
      case CombinationKind::kFloat:return "kFloat";
      case CombinationKind::kDouble:return "kDouble";
      case CombinationKind::kLongDouble:return "kLongDouble";
      case CombinationKind::kStruct:return "kStruct";
      case CombinationKind::kUnion:return "kUnion";
      case CombinationKind::kEnum:return "kEnum";
      case CombinationKind::kTypeName:return "kTypeName";
    }
  }
  int getMatched() const {
    return mMatched;
  }
 private:
  int mMatched = 0;
  CombinationKind mKind;
  std::vector<CheckBox> mCheckBoxes;
};

class TokenCombinations {
 public:
  CombinationKind getType() const {
    return mPerfectMatch;
  }
  // return false if not a single combination matches
  bool put(TokenKind kind) {
    mPerfectMatch = CombinationKind::kUnknown;
    int matchCount = 0;
    for (auto &combination : mCombinations) {
      if (combination.getMatched() < 0) continue;
      combination.matches(kind);
      if (combination.getMatched() == combination.size()) {
        mPerfectMatch = combination.getKind();
        ++matchCount;
      } else if (combination.getMatched() > 0) {
        ++matchCount;
      }
    }
    return matchCount != 0;
  }
  std::string dump() {
    std::string s;
    for (const auto &combination : mCombinations) {
      s += combination.dump() + "\n";
    }
    return s;
  }
 private:
  CombinationKind mPerfectMatch = CombinationKind::kUnknown;
  std::vector<Combination> mCombinations{
      {CombinationKind::kVoid, {TokenKind::TOKEN_VOID}},
      {CombinationKind::kChar, {TokenKind::TOKEN_CHAR}},
      {CombinationKind::kSignedChar, {TokenKind::TOKEN_SIGNED, TokenKind::TOKEN_CHAR}},
      {CombinationKind::kUnsignedChar, {TokenKind::TOKEN_UNSIGNED, TokenKind::TOKEN_CHAR}},
      {CombinationKind::kShort, {TokenKind::TOKEN_SHORT}},
      {CombinationKind::kSignedShort, {TokenKind::TOKEN_SIGNED, TokenKind::TOKEN_SHORT}},
      {CombinationKind::kShortInt, {TokenKind::TOKEN_SHORT, TokenKind::TOKEN_INT}},
      {CombinationKind::kSignedShortInt, {TokenKind::TOKEN_SIGNED, TokenKind::TOKEN_SHORT, TokenKind::TOKEN_INT}},
      {CombinationKind::kUnsignedShort, {TokenKind::TOKEN_UNSIGNED, TokenKind::TOKEN_SHORT}},
      {CombinationKind::kUnsignedShortInt, {TokenKind::TOKEN_UNSIGNED, TokenKind::TOKEN_SHORT, TokenKind::TOKEN_INT}},
      {CombinationKind::kInt, {TokenKind::TOKEN_INT}},
      {CombinationKind::kSigned, {TokenKind::TOKEN_SIGNED}},
      {CombinationKind::kSignedInt, {TokenKind::TOKEN_SIGNED, TokenKind::TOKEN_INT}},
      {CombinationKind::kUnsigned, {TokenKind::TOKEN_UNSIGNED}},
      {CombinationKind::kUnsignedInt, {TokenKind::TOKEN_UNSIGNED, TokenKind::TOKEN_INT}},
      {CombinationKind::kLong, {TokenKind::TOKEN_LONG}},
      {CombinationKind::kSignedLong, {TokenKind::TOKEN_SIGNED, TokenKind::TOKEN_LONG}},
      {CombinationKind::kLongInt, {TokenKind::TOKEN_LONG, TokenKind::TOKEN_INT}},
      {CombinationKind::kSignedLongInt, {TokenKind::TOKEN_SIGNED, TokenKind::TOKEN_LONG, TokenKind::TOKEN_INT}},
      {CombinationKind::kUnsignedLong, {TokenKind::TOKEN_UNSIGNED, TokenKind::TOKEN_LONG}},
      {CombinationKind::kUnsignedLongInt, {TokenKind::TOKEN_UNSIGNED, TokenKind::TOKEN_LONG, TokenKind::TOKEN_INT}},
      {CombinationKind::kLongLong, {TokenKind::TOKEN_LONG, TokenKind::TOKEN_LONG}},
      {CombinationKind::kSignedLongLong, {TokenKind::TOKEN_SIGNED, TokenKind::TOKEN_LONG, TokenKind::TOKEN_LONG}},
      {CombinationKind::kLongLongInt, {TokenKind::TOKEN_LONG, TokenKind::TOKEN_LONG, TokenKind::TOKEN_INT}},
      {CombinationKind::kSignedLongLongInt,
       {TokenKind::TOKEN_SIGNED, TokenKind::TOKEN_LONG, TokenKind::TOKEN_LONG, TokenKind::TOKEN_INT}},
      {CombinationKind::kUnsignedLongLong, {TokenKind::TOKEN_UNSIGNED, TokenKind::TOKEN_LONG, TokenKind::TOKEN_LONG}},
      {CombinationKind::kUnsignedLongLongInt,
       {TokenKind::TOKEN_UNSIGNED, TokenKind::TOKEN_LONG, TokenKind::TOKEN_LONG, TokenKind::TOKEN_INT}},
      {CombinationKind::kFloat, {TokenKind::TOKEN_FLOAT}},
      {CombinationKind::kDouble, {TokenKind::TOKEN_DOUBLE}},
      {CombinationKind::kLongDouble, {TokenKind::TOKEN_LONG, TokenKind::TOKEN_DOUBLE}},
      {CombinationKind::kStruct, {TokenKind::TOKEN_STRUCT}},
      {CombinationKind::kUnion, {TokenKind::TOKEN_UNION}},
      {CombinationKind::kEnum, {TokenKind::TOKEN_ENUM}},
      {CombinationKind::kTypeName, {TokenKind::TOKEN_IDENTIFIER}},
  };
};

#endif //MYCCPILER_SPECIFIER_COMBINATION_H
