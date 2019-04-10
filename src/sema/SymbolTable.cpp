#include <sema/SymbolTable.h>
mycc::SymbolTable::SymbolTable(const std::string &name, const SymbolTable &father)
    : name(name), father(father) {}
mycc::SymbolTable::SymbolTable(const std::string& name) : name(name), father(*this) {}
const mycc::DeclarationAST &mycc::SymbolTable::lookup(const std::string &name) {
  try {
    map.at(name);
  } catch (const std::out_of_range &) {
    if (&father != this) {

    }
  }
}