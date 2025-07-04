#include "ConstantPropagation.hpp"
#include <map>
#include <set>

using namespace llvm;

PreservedAnalyses
ConstantPropagation::run(Module& mod, ModuleAnalysisManager& mam)
{
  int constantPropagationTimes = 0;
  std::map<Value*, Constant*> constantMap;

  // 所有没有被修改过的全局变量可以应用常量传播
  // 数组变量一律不考虑
  std::set<GlobalVariable*> modifiedGlobals;
  for (auto& func : mod) {
    for (auto& BB : func) {
      for (auto& I : BB) {
        if (auto storeInst = dyn_cast<StoreInst>(&I)) {
          if (auto globalVar =
                dyn_cast<GlobalVariable>(storeInst->getPointerOperand())) {
            if (auto initVal = globalVar->getInitializer()) {
              if (initVal->getType()->isArrayTy()) {
                continue;
              }
              modifiedGlobals.insert(globalVar);
            }
          }
        }
      }
    }
  }
  for (auto& var : mod.globals()) {
    if (auto globalVar = dyn_cast<GlobalVariable>(&var)) {
      if (auto initVal = globalVar->getInitializer()) {
        if (initVal->getType()->isArrayTy()) {
          continue;
        }
        if (modifiedGlobals.find(globalVar) != modifiedGlobals.end()) {
          continue;
        }

        constantMap[globalVar] = initVal;
      }
    }
  }

  // 用“全局常量”赋值的变量可以直接被替换为常量
  for (auto& func : mod) {
    for (auto& BB : func) {
      std::vector<Instruction*> instToErase;
      for (auto& I : BB) {
        if (auto loadInst = dyn_cast<LoadInst>(&I)) {
          Value* ptr = loadInst->getPointerOperand();
          if (constantMap.find(ptr) != constantMap.end()) {
            loadInst->replaceAllUsesWith(constantMap[ptr]);
            instToErase.push_back(loadInst);
            constantPropagationTimes++;
          }
        }
      }
    }
  }

  mOut << "ConstantPropagation applied " << constantPropagationTimes
       << " times.\n";
  return PreservedAnalyses::all();
}