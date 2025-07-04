#pragma once

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Support/raw_ostream.h>

class DCE : public llvm::PassInfoMixin<DCE>
{
public:
  explicit DCE(llvm::raw_ostream& out)
    : mOut(out)
  {
  }
  llvm::PreservedAnalyses run(llvm::Module& mod,
                              llvm::ModuleAnalysisManager& MAM);

private:
  bool runOnFunction(llvm::Function& F);
  llvm::raw_ostream& mOut;
};