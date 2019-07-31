#include <sema/symbol_tables.h>
#include <sema/ast.h>
#include <llvm/IR/IRBuilder.h>
ISymbol *SymbolTable::lookup(const Token &token) {
  try {
    return at(token.getValue()).get();
  } catch (const std::out_of_range &) {
    if (father) {
      return father->lookup(token);
    } else {
      return nullptr;
    }
  }
}
bool SymbolTable::isTypedef(const Token &token) {
  return lookup(token) != nullptr && *lookup(token) == SymbolKind::TYPEDEF;
}
ISymbol *SymbolTable::insert(const Token &token, std::unique_ptr<ISymbol> &&symbol) {
  try {
    at(token.getValue());
    throw SemaException(std::string("symbol \"") + token.getValue() + "\" already defined", token);
  } catch (const std::out_of_range &) {
    return emplace(token.getValue(), std::move(symbol)).first->second.get();
  }
}
ISymbol *SymbolTable::insert(std::unique_ptr<ISymbol> &&symbol) {
  return emplace(std::string("$") + std::to_string(anonymousId++), std::move(symbol)).first->second.get();
}
void SymbolTable::setFather(SymbolTable *father) {
  SymbolTable::father = father;
}
SymbolTable *SymbolTable::getFather() const {
  return father;
}
ScopeKind SymbolTable::getScopeKind() const {
  return scope_kind;
}
void SymbolTable::setValue(const Token &token, ISymbol *symbol) {
  if (auto *s = dynamic_cast<FunctionSymbol *>(symbol)) {
    s->mValue = llvm::Function::Create(s->getType()->getLLVMType(mModule),
                                       llvm::Function::ExternalLinkage,
                                       token.getValue(),
                                       &mModule);
  }
}
SymbolTable *SymbolTables::createTable(ScopeKind kind, SymbolTable *father) {
  tables.emplace_back(kind, father);
  return &tables.back();
}
FileTable::FileTable(llvm::Module &module) : SymbolTable(ScopeKind::FILE, module, "FileScope") {}
void FileTable::setValue(const Token &token, ISymbol *symbol) {
  if (auto *s = dynamic_cast<ObjectSymbol *>(symbol)) {
    mModule.getOrInsertGlobal(token.getValue(), s->getType()->getLLVMType(mModule));
    llvm::GlobalVariable *gVar = mModule.getNamedGlobal(token.getValue());
    Linkage linkage = s->getLinkage();
    if (linkage != Linkage::kNone) {
      gVar->setLinkage(linkage == Linkage::kExternal ? llvm::GlobalValue::LinkageTypes::ExternalLinkage
                                                     : llvm::GlobalVariable::LinkageTypes::InternalLinkage);
    }
    if (s->getQualifiers().find(TypeQualifier::kCONST) != s->getQualifiers().end()) {
      gVar->setConstant(true);
    }
    s->mValue = gVar;
  } else {
    SymbolTable::setValue(token, symbol);
  }
}
BlockTable::BlockTable(llvm::Module &module, bool function, std::string name, llvm::BasicBlock *basicBlock)
    : SymbolTable(function ? ScopeKind::FUNCTION
                           : ScopeKind::BLOCK,
                  module, std::move(name)), mBasicBlock(basicBlock) {}
void BlockTable::setValue(const Token &token, ISymbol *symbol) {
  llvm::IRBuilder<> builder(mBasicBlock);
  if (auto *s = dynamic_cast<ObjectSymbol *>(symbol)) {
    s->mValue = builder.CreateAlloca(s->getType()->getLLVMType(mModule), nullptr, token.getValue());
  } else {
    SymbolTable::setValue(token, symbol);
  }
}
ProtoTable::ProtoTable(llvm::Module &module, std::string name, llvm::BasicBlock *basicBlock)
    : SymbolTable(ScopeKind::FUNCTION_PROTOTYPE,
                  module, std::move(name)),
      mBasicBlock(basicBlock) {}
void ProtoTable::setValue(const Token &token, ISymbol *symbol) {
  llvm::IRBuilder<> builder(mBasicBlock);
  if (auto *s = dynamic_cast<ObjectSymbol *>(symbol)) {

  } else {

  }
}