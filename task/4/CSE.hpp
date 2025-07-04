#pragma once

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Support/raw_ostream.h>

class CSE : public llvm::PassInfoMixin<CSE>
{
public:
  explicit CSE(llvm::raw_ostream& out)
    : mOut(out)
  {
  }
  llvm::PreservedAnalyses run(llvm::Module& module,
                              llvm::ModuleAnalysisManager& MAM);

private:
  llvm::raw_ostream& mOut;
};