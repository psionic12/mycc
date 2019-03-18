#ifndef MYCCPILER_FIRST_SET_GENERATOR_H
#define MYCCPILER_FIRST_SET_GENERATOR_H
#include <unordered_map>
#include <vector>
#include <set>
#include <fstream>
namespace mycc {
class FirstSetGenerator {
 public:
  class Symbol {
   public:
    Symbol(long);
    Symbol(const char *);
   private:
    bool none_terminal;
   public:
    bool isNone_terminal() const;
    long getType() const;
    const char *getName() const;
   private:
    union {
      long type;
      char name[32];
    };
  };
  typedef long NoneTerminalId;
  typedef long ProductionId;
  typedef std::pair<NoneTerminalId, std::vector<Symbol>> Production;
  typedef std::vector<Production> Productions;
  static Productions ToProductions(std::ifstream &in, std::set<NoneTerminalId> &nullalble_set);
};
const std::set<std::string> pre_defined_none_terminal
    {"identifier", "string", "integer-constant", "character-constant", "floating-constant", "enumeration-constant"};
} //namespace mycc
#endif //MYCCPILER_FIRST_SET_GENERATOR_H
