#include <iostream>
#include <fstream>
#include <lex/lex.h>
#include "first_set_generator.h"

int main() {
  std::ifstream BNF;
  BNF.open("BNF");
  std::unordered_map<std::string, mycc_first_set::NoneTerminalId> none_terminal_map;
  mycc_first_set::Productions productions = mycc_first_set::FirstSetGenerator::ToProductions(BNF, none_terminal_map);
//  for(auto p : productions) {
//    std::cout << p.first << " -> ";
//    for (auto symbol : p.second) {
//      if (symbol.isNone_terminal()) {
//        std::cout << symbol.getType();
//      } else {
//        std::cout << symbol.getName();
//      }
//      std::cout << ' ';
//    }
//    std::cout << std::endl;
//  }
  mycc_first_set::FirstSets first_sets = mycc_first_set::FirstSetGenerator::getFirstSets(productions, none_terminal_map.size());
  for (auto k : none_terminal_map) {
    std::cout << k.first << " -> ";
    auto set = first_sets[k.second];
    for (const auto &string : set) {
      std::cout << string << " ";
    }
    std::cout << std::endl;
  }
  std::cout << std::endl;
  std::cout << std::endl;
  std::cout << std::endl;

  mycc_first_set::FirstSets production_first_sets = mycc_first_set::FirstSetGenerator::getProductionFirstSets(productions, first_sets);
  for (int i = 0; i < production_first_sets.size(); ++i) {
    std::cout << i + 1 << ": ";
    for (const auto& str: production_first_sets[i]) {
      std::cout << str << " ";
    }
    std::cout << std::endl;
  }
}