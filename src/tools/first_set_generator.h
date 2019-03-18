#ifndef MYCCPILER_FIRST_SET_GENERATOR_H
#define MYCCPILER_FIRST_SET_GENERATOR_H
#include <unordered_map>
#include <vector>
#include <set>
#include <fstream>
namespace mycc {
class Symbol {
 public:
  Symbol(long, bool);
  Symbol(const char *);
 private:
  bool none_terminal;
 public:
  bool isNone_terminal() const;
  long getType() const;
  const char *getName() const;
 private:
  long type;
  bool nullable;
 public:
  bool isNullable() const;
 private:
  char name[32];
};
typedef long NoneTerminalId;
typedef long ProductionId;
typedef std::pair<NoneTerminalId, std::vector<Symbol>> Production;
typedef std::vector<Production> Productions;
class FirstSetGenerator {
 public:
  static Productions ToProductions(std::ifstream &in, std::unordered_map<std::string, NoneTerminalId>& none_terminal_map);
  static std::vector<std::set<std::string>> getFirstSets(const Productions&, long size);
};
const std::set<std::string> pre_defined_none_terminal
    {"identifier", "string", "integer-constant", "character-constant", "floating-constant", "enumeration-constant"};
} //namespace mycc
#endif //MYCCPILER_FIRST_SET_GENERATOR_H
