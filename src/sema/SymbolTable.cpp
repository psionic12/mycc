#include <sema/SymbolTable.h>
<<<<<<< HEAD
#include <ast/ast.h>
mycc::SymbolTable::SymbolTable(const std::string &name, const SymbolTable &father)
    : name(name), father(father) {}
mycc::SymbolTable::SymbolTable(const std::string &name) : name(name), father(*this) {}
const mycc::DeclarationAST &mycc::SymbolTable::lookup(const std::string &name) const {
=======
mycc::SymbolTable::SymbolTable(const std::string &name, const SymbolTable &father)
    : name(name), father(father) {}
mycc::SymbolTable::SymbolTable(const std::string& name) : name(name), father(*this) {}
const mycc::DeclarationAST &mycc::SymbolTable::lookup(const std::string &name) {
>>>>>>> fd9357080aa517249c7062a1c2578683b7eac34f
  try {
    map.at(name);
  } catch (const std::out_of_range &) {
    if (&father != this) {
<<<<<<< HEAD
      father.lookup(name);
=======

>>>>>>> fd9357080aa517249c7062a1c2578683b7eac34f
    }
  }
}