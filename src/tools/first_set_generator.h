#ifndef MYCCPILER_FIRST_SET_GENERATOR_H
#define MYCCPILER_FIRST_SET_GENERATOR_H
#include <unordered_map>
#include <vector>
#include <set>
#include <fstream>
namespace mycc_first_set {
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
typedef std::vector<std::set<std::string>> FirstSets;
class FirstSetGenerator {
 public:
  static Productions ToProductions(std::ifstream &in,
      std::unordered_map<std::string, NoneTerminalId>& none_terminal_map);
  static FirstSets getFirstSets(const Productions&, long size);
  static FirstSets getProductionFirstSets(const Productions&, const FirstSets&);
};
} //namespace mycc_first_set
#endif //MYCCPILER_FIRST_SET_GENERATOR_H
