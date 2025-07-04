#include "CSE.hpp"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/IR/Instructions.h"

using namespace llvm;

const int K = 20;

llvm::PreservedAnalyses
CSE::run(Module& mod, ModuleAnalysisManager& MAM)
{
  int CSETimes = 0;

  for (auto& func : mod) {
    for (auto& BB : func) {
      SmallPtrSet<Instruction*, 8> instToErase;

      for (auto it = BB.begin(); it != BB.end();) {
        auto* I = &*it;
        ++it; // 提前递增迭代器，避免失效

        if (!isa<BinaryOperator>(I)) {
          continue;
        }

        auto next_it = it;
        for (int k = 0; k < K && next_it != BB.end(); ++k, ++next_it) {
          auto* J = &*next_it;

          if (!isa<BinaryOperator>(J)) {
            continue;
          }

          if (I->getOpcode() != J->getOpcode()) {
            continue;
          }

          if (I->getOperand(0) != J->getOperand(0) ||
              I->getOperand(1) != J->getOperand(1)) {
            continue;
          }

          mOut << "CSE: Replace " << *J << " with " << *I << "\n";
          J->replaceAllUsesWith(I);
          instToErase.insert(J); // 自动去重
          CSETimes++;
        }
      }

      // 安全删除
      for (auto* I : instToErase) {
        if (I->use_empty()) {
          I->eraseFromParent();
        }
      }
    }
  }

  mOut << "CSE applied " << CSETimes << " times (BinaryOperator only).\n";
  return PreservedAnalyses::all();
}