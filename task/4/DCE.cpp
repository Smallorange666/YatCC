#include "DCE.hpp"
#include <llvm/Transforms/Utils/Local.h>
#include <set>

using namespace llvm;

llvm::PreservedAnalyses
DCE::run(Module& mod, ModuleAnalysisManager& MAM)
{
  int DCECount = 0;

  // 考虑删除没被使用过的全局变量的声明，以及结果没有被使用过的二元运算
  std::set<GlobalVariable*> usedGlobals;
  for (auto& func : mod) {
    for (auto& BB : func) {
      for (auto& I : BB) {
        if (auto loadInst = dyn_cast<LoadInst>(&I)) {
          Value* ptr = loadInst->getPointerOperand();
          if (auto globalVar = dyn_cast<GlobalVariable>(ptr)) {
            usedGlobals.insert(globalVar);
          }
        }
      }
    }
  }

  for (auto& func : mod) {
    for (auto& BB : func) {
      std::vector<Instruction*> instToErase;
      // 考虑从后往前遍历，消除掉无用的二元运算
      for (auto it = BB.rbegin(); it != BB.rend(); ++it) {
        auto& I = *it;
        if (auto binInst = dyn_cast<BinaryOperator>(&I)) {
          if (binInst->use_empty()) {
            instToErase.push_back(&I);
            DCECount++;
          } else {
            // 判断其所有user是否被删除
            bool flag = true;
            for (auto* user : binInst->users()) {
              if (auto userInst = dyn_cast<Instruction>(user)) {
                if (std::find(instToErase.begin(),
                              instToErase.end(),
                              userInst) == instToErase.end()) {
                  flag = false;
                  break;
                }
              }
            }
            if (flag) {
              instToErase.push_back(&I);
              DCECount++;
            }
          }
        } else if (auto storeInst = dyn_cast<StoreInst>(&I)) {
          // 向没使用过的全局变量store的指令也可以被删除
          Value* ptr = storeInst->getPointerOperand();
          if (auto globalVar = dyn_cast<GlobalVariable>(ptr)) {
            if (usedGlobals.find(globalVar) == usedGlobals.end()) {
              instToErase.push_back(&I);
              DCECount++;
            }
          }
        }
      }

      for (auto& inst : instToErase) {
        if (!inst->user_empty())
          continue;
        inst->eraseFromParent();
      }
    }
  }

  mOut << "DCE applied " << DCECount << " times.\n";
  return PreservedAnalyses::all();
}