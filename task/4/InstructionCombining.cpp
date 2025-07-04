#include "InstructionCombining.hpp"
#include "llvm/IR/Instructions.h"
#include <unordered_set>

using namespace llvm;

PreservedAnalyses
InstructionCombining::run(Module& mod, ModuleAnalysisManager& mam)
{
  int instructionCombiningCount = 0;

  // 考虑某条指令的结果，仅被后面的一条指令使用
  // 例如：a = b + 1; d = a + 1; 可以将两条指令合并为一条 d = (b + 1) + 1;
  // 仅考虑上面这种形式的合并
  for (auto& func : mod) {
    for (auto& BB : func) {
      std::vector<Instruction*> instToErase;
      std::unordered_set<Instruction*> instToEraseSet;
      for (auto& I : BB) {
        if (instToEraseSet.find(&I) != instToEraseSet.end())
          continue;
        if (auto* binInst = dyn_cast<BinaryOperator>(&I)) {
          if (binInst->getOpcode() == Instruction::Add &&
              binInst->hasOneUse()) {
            auto lhs = binInst->getOperand(0);
            auto rhs = binInst->getOperand(1);
            auto constLhs = dyn_cast<ConstantInt>(lhs);
            auto constRhs = dyn_cast<ConstantInt>(rhs);
            if (!constLhs && !constRhs)
              continue;

            auto* userInst = dyn_cast<Instruction>(*binInst->user_begin());
            if (!userInst || userInst->getParent() != &BB ||
                !isa<BinaryOperator>(userInst) ||
                userInst->getOpcode() != Instruction::Add) {
              continue;
            }
            auto userLhs = userInst->getOperand(0);
            auto userRhs = userInst->getOperand(1);
            auto constUserLhs = dyn_cast<ConstantInt>(userLhs);
            auto constUserRhs = dyn_cast<ConstantInt>(userRhs);
            if (!constUserLhs && !constUserRhs)
              continue;

            BinaryOperator* newInst = nullptr;
            if (userLhs == binInst) {
              if (constRhs) {
                // a = b + 1; d = a + 1;
                newInst = BinaryOperator::CreateAdd(
                  lhs,
                  ConstantInt::get(constRhs->getType(),
                                   constRhs->getValue() +
                                     constUserRhs->getValue()),
                  "",
                  userInst);
              } else {
                // a = 1 + b; d = a + 1;
                newInst = BinaryOperator::CreateAdd(
                  rhs,
                  ConstantInt::get(constLhs->getType(),
                                   constLhs->getValue() +
                                     constUserRhs->getValue()),
                  "",
                  userInst);
              }
            } else {
              if (constRhs) {
                // a = b + 1; d = 1 + a;
                newInst = BinaryOperator::CreateAdd(
                  lhs,
                  ConstantInt::get(constRhs->getType(),
                                   constRhs->getValue() +
                                     constUserLhs->getValue()),
                  "",
                  userInst);
              } else {
                // a = 1 + b; d = 1 + a;
                newInst = BinaryOperator::CreateAdd(
                  rhs,
                  ConstantInt::get(constLhs->getType(),
                                   constLhs->getValue() +
                                     constUserLhs->getValue()),
                  "",
                  userInst);
              }
            }

            userInst->replaceAllUsesWith(newInst);

            instToErase.push_back(userInst);
            instToErase.push_back(binInst);
            instToEraseSet.insert(userInst);
            instToEraseSet.insert(binInst);
            instructionCombiningCount++;
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

  mOut << "Instruction combining applied " << instructionCombiningCount
       << " times.\n";

  return PreservedAnalyses::all();
}