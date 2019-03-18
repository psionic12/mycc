#include <regex>
#include "first_set_generator.h"
mycc::FirstSetGenerator::Symbol::Symbol(long type) : none_terminal(true), type(type) {}
mycc::FirstSetGenerator::Symbol::Symbol(const char *src) : none_terminal(false) {
  std::strcpy(name, src);
}
bool mycc::FirstSetGenerator::Symbol::isNone_terminal() const {
  return none_terminal;
}
long mycc::FirstSetGenerator::Symbol::getType() const {
  return type;
}
const char *mycc::FirstSetGenerator::Symbol::getName() const {
  return name;
}
mycc::FirstSetGenerator::Productions mycc::FirstSetGenerator::ToProductions(std::ifstream &in,
                                                                            std::set<NoneTerminalId> &nullalble_set) {
  ProductionId current_production = -1;
  NoneTerminalId current_none_terminal = -1;
  std::unordered_map<std::string, NoneTerminalId> none_terminal_map;
  Productions productions;
  std::string line;
  std::regex r_production(R"((<([\w][\w-]+)>)? *(::=|\|)(( *[^ \t\n\r]+)+))");
  std::regex r_symbols(R"([^ \t\n\r]+)");
  std::regex r_none_terminal(R"(\{?<(\w[\w-]*)>\}?([\*\+\?]?))");
  std::smatch matches;
  std::smatch symbols;
  std::smatch symbol;
  while (std::getline(in, line)) {
    if (!std::regex_match(line, matches, r_production)) {
      if (line.empty()) continue;
      std::string error("syntax error: ");
      error.append(line);
      throw std::runtime_error(error);
    }
    ++current_production;
    if (matches[2].length()) {
      try {
        std::string s(matches[2]);
        current_none_terminal = none_terminal_map.at(s);
      } catch (const std::out_of_range &) {
        current_none_terminal = none_terminal_map.size();
        none_terminal_map.emplace(matches[2].str(), current_none_terminal);
      }
    }
    productions.emplace_back();
    productions[current_production].first = current_none_terminal;
    std::string symbols_str(matches[4]);
    while (std::regex_search(symbols_str, symbols, r_symbols)) {
      std::string symbol_str(symbols[0]);
      if (std::regex_match(symbol_str, symbol, r_none_terminal)) {
        std::string symbol_name(symbol[1]);
        bool is_pre_def = false;
        for (auto pre : pre_defined_none_terminal) {
          if (pre == symbol_name) {
            is_pre_def = true;
            break;
          }
        }
        if (is_pre_def) {
          productions[current_production].second.emplace_back(Symbol(symbol_name.c_str()));
        }
        else {
          NoneTerminalId id;
          try {
            id = none_terminal_map.at(symbol_name);
          } catch (const std::out_of_range &) {
            id = none_terminal_map.size();
            none_terminal_map.emplace(symbol_name, id);
          }
          if (symbol[2] == '*' || symbol[2] == '?') {
            nullalble_set.emplace(id);
          }
          productions[current_production].second.emplace_back(Symbol(id));
        }
      } else {
        productions[current_production].second.emplace_back(Symbol(symbol_str.c_str()));
      }
      symbols_str = symbols.suffix().str();
    }
  }
  return productions;
}