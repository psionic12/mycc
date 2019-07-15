#ifndef MYCCPILER_SPECIFIER_COMBINATION_H
#define MYCCPILER_SPECIFIER_COMBINATION_H
#include <vector>
#include "token.h"
enum class CombinationKind {
  kVoid,
  kChar,
  kSignedChar,
  kUnsignedChar,
  kShort,
  kSignedShort,
  kShortInt,
  kSignedShortInt,
  kUnsignedShort,
  kUnsignedShortInt,
  kInt,
  kSigned,
  kSignedInt,
  kUnsigned,
  kUnsignedInt,
  kLong,
  kSignedLong,
  kLongInt,
  kSignedLongInt,
  kUnsignedLong,
  kUnsignedLongInt,
  kLongLong,
  kSignedLongLong,
  kLongLongInt,
  kSignedLongLongInt,
  kUnsignedLongLong,
  kUnsignedLongLongInt,
  kFloat,
  kDouble,
  kLongDouble,
  kStruct,
  kUnion,
  kEnum,
  kTypeName,
};
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
 private:
  const TokenKind mKind;
  bool mChecked = false;
};

class Combination {
 public:
  Combination(CombinationKind kind, std::initializer_list<CheckBox> list) : mKind(kind), mCheckBoxes(list) {}
  // negative for not matched, 0 for matched, positive for perfect matched
  int matches(TokenKind kind) {
    for (auto &checkBox : mCheckBoxes) {
      if (kind == checkBox.getKind()) {
        checkBox.setChecked();
        ++mMatched;
        if (mMatched == mCheckBoxes.size()) {
          return 1;
        } else {
          return 0;
        }
      }
    }
    return -1;
  };
 private:
  int mMatched;
  CombinationKind mKind;
  std::vector<CheckBox> mCheckBoxes;
};

class TokenCombinations {
 private:
  std::vector<Combination> mCombinations{
      {CombinationKind::kVoid, {TokenKind::TOKEN_VOID}},
      {CombinationKind::kChar, {TokenKind::TOKEN_CHAR}},
      {CombinationKind::kSignedChar, {TokenKind::TOKEN_SIGNED, TokenKind::TOKEN_CHAR}},
      {CombinationKind::kUnsignedChar, {TokenKind::TOKEN_SIGNED, TokenKind::TOKEN_CHAR}},

  };
};

#endif //MYCCPILER_SPECIFIER_COMBINATION_H
