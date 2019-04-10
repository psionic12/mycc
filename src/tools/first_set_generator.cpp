#include <regex>
#include <iostream>
#include "first_set_generator.h"
mycc::Symbol::Symbol(long type, bool nullable)
    : none_terminal(true), type(type), nullable(nullable) {}
mycc::Symbol::Symbol(const char *src) : none_terminal(false), nullable(false) {
  std::strcpy(name, src);
}
bool mycc::Symbol::isNone_terminal() const {
  return none_terminal;
}
long mycc::Symbol::getType() const {
  return type;
}
const char *mycc::Symbol::getName() const {
  return name;
}
bool mycc::Symbol::isNullable() const {
  return nullable;
}
mycc::Productions

mycc::FirstSetGenerator::ToProductions(std::ifstream &in,
                                       std::unordered_map<std::string, NoneTerminalId> &none_terminal_map) {
  ProductionId current_production = -1;
  NoneTerminalId current_none_terminal = -1;
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
        NoneTerminalId id;
        bool nullable = false;
        try {
          id = none_terminal_map.at(symbol_name);
        } catch (const std::out_of_range &) {
          id = none_terminal_map.size();
          none_terminal_map.emplace(symbol_name, id);
        }
        if (symbol[2] == '*' || symbol[2] == '?') {
          nullable = true;
        }
        productions[current_production].second.emplace_back(Symbol(id, nullable));
      } else {
        productions[current_production].second.emplace_back(Symbol(symbol_str.c_str()));
      }
      symbols_str = symbols.suffix().str();
    }
  }
  return productions;
}
mycc::FirstSets mycc::FirstSetGenerator::getFirstSets(const mycc::Productions &productions,
                                                      long size) {
  std::vector<std::set<std::string>> first_sets(size);
  bool changed;
  do {
    changed = false;
    for (auto production : productions) {
      std::set<std::string> &current_set = first_sets[production.first];
      long before = current_set.size();
      for (auto symbol : production.second) {
        if (symbol.isNone_terminal()) {
          current_set.insert(first_sets[symbol.getType()].begin(), first_sets[symbol.getType()].end());
          if (symbol.isNullable()) {
            continue;
          } else {
            break;
          }
        } else {
          current_set.emplace(symbol.getName());
          break;
        }
      }
      if (before != current_set.size()) changed = true;
    }
  } while (changed);
  return first_sets;
}
mycc::FirstSets mycc::FirstSetGenerator::getProductionFirstSets(const mycc::Productions &productions,
                                                                const mycc::FirstSets &first_sets) {
  FirstSets production_first_sets;
  for (auto production : productions) {
    production_first_sets.emplace_back();
    for (auto symbol : production.second) {
      if (symbol.isNone_terminal()) {
        production_first_sets.back().insert(first_sets[symbol.getType()].begin(), first_sets[symbol.getType()].end());
        if (symbol.isNullable()) {
          continue;
        } else {
          break;
        }
      } else {
        production_first_sets.back().insert(symbol.getName());
        break;
      }
    }
  }
  return production_first_sets;
}
