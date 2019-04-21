#include <sema/SymbolTable.h>
#include <ast/ast.h>
mycc::SymbolTable::SymbolTable(const std::string &name, const SymbolTable &father)
    : name(name), father(father) {}
mycc::SymbolTable::SymbolTable(const std::string &name) : name(name), father(*this) {}
const mycc::DeclarationAST &mycc::SymbolTable::lookup(const std::string &name) const {
  try {
    map.at(name);
  } catch (const std::out_of_range &) {
    if (&father != this) {
      father.lookup(name);
    } else {
      throw SymbolNotFoundException{};
    }
  }
}