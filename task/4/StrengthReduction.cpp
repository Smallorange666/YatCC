#include "StrengthReduction.hpp"

using namespace llvm;

llvm::PreservedAnalyses
StrengthReduction::run(Module& mod, ModuleAnalysisManager& MAM)
{
  int strengthReductionTimes = 0;

  for (auto& func : mod) {
    for (auto& BB : func) {
      std::vector<Instruction*> instToErase;
      for (auto& I : BB) {
        if (auto* op = dyn_cast<BinaryOperator>(&I)) {
          auto lhs = op->getOperand(0);
          auto rhs = op->getOperand(1);
          auto* constLhs = dyn_cast<ConstantInt>(lhs);
          auto* constRhs = dyn_cast<ConstantInt>(rhs);

          if (!constLhs && !constRhs)
            continue;

          switch (op->getOpcode()) {
            case Instruction::Add: {
              if (constRhs) {
                if (constRhs->getValue() == 0) {
                  // x + 0 = x
                  op->replaceAllUsesWith(lhs);
                  instToErase.push_back(op);
                  strengthReductionTimes++;
                }
              } else if (constLhs) {
                if (constLhs->getValue() == 0) {
                  // 0 + x = x
                  op->replaceAllUsesWith(rhs);
                  instToErase.push_back(op);
                  strengthReductionTimes++;
                }
              }
              break;
            }
            case Instruction::Sub: {
              if (constRhs) {
                if (constRhs->getValue() == 0) {
                  // x - 0 = x
                  op->replaceAllUsesWith(lhs);
                  instToErase.push_back(op);
                  strengthReductionTimes++;
                }
              } else if (constLhs) {
                if (constLhs->getValue() == 0) {
                  // 0 - x = -x
                  IRBuilder<> irb(&I);
                  auto* newOp = irb.CreateNeg(rhs);
                  op->replaceAllUsesWith(newOp);
                  instToErase.push_back(op);
                  strengthReductionTimes++;
                }
              }
              break;
            }
            case Instruction::Mul: {
              if (constRhs) {
                if (constRhs->getValue() == 1) {
                  // x * 1 = x
                  op->replaceAllUsesWith(lhs);
                  instToErase.push_back(op);
                  strengthReductionTimes++;
                } else if (constRhs->getValue() == 0) {
                  // x * 0 = 0
                  op->replaceAllUsesWith(
                    ConstantInt::getNullValue(lhs->getType()));
                  instToErase.push_back(op);
                  strengthReductionTimes++;
                } else if (constRhs->getValue().isPowerOf2()) {
                  // x * 2^n = x << n
                  IRBuilder<> irb(&I);
                  auto* newOp = irb.CreateShl(
                    lhs,
                    ConstantInt::get(lhs->getType(),
                                     constRhs->getValue().logBase2()));
                  op->replaceAllUsesWith(newOp);
                  instToErase.push_back(op);
                  strengthReductionTimes++;
                }
              } else if (constLhs) {
                if (constLhs->getValue() == 1) {
                  // 1 * x = x
                  op->replaceAllUsesWith(rhs);
                  instToErase.push_back(op);
                  strengthReductionTimes++;
                } else if (constLhs->getValue() == 0) {
                  // 0 * x = 0
                  op->replaceAllUsesWith(
                    ConstantInt::getNullValue(rhs->getType()));
                  instToErase.push_back(op);
                  strengthReductionTimes++;
                } else if (constLhs->getValue().isPowerOf2()) {
                  // 2^n * x = x << n
                  IRBuilder<> irb(&I);
                  auto* newOp = irb.CreateShl(
                    rhs,
                    ConstantInt::get(rhs->getType(),
                                     constLhs->getValue().logBase2()));
                  op->replaceAllUsesWith(newOp);
                  instToErase.push_back(op);
                  strengthReductionTimes++;
                }
              }
              break;
            }
            case Instruction::UDiv:
            case Instruction::SDiv: {
              if (constRhs) {
                // x / 1 = x
                if (constRhs->getValue() == 1) {
                  op->replaceAllUsesWith(lhs);
                  instToErase.push_back(op);
                  strengthReductionTimes++;
                }
              } else if (constLhs) {
                // 0 / x=0
                if (constLhs->getValue() == 0) {
                  op->replaceAllUsesWith(
                    ConstantInt::getNullValue(lhs->getType()));
                  instToErase.push_back(op);
                  strengthReductionTimes++;
                }
              }
              break;
            }
            case Instruction::SRem: {
              if (constRhs) {
                // x % 1 = 0
                if (constRhs->getValue() == 1) {
                  op->replaceAllUsesWith(
                    ConstantInt::getNullValue(rhs->getType()));
                  instToErase.push_back(op);
                  strengthReductionTimes++;
                }
              } else if (constLhs) {
                // 0 % x = 0
                if (constLhs->getValue() == 0) {
                  op->replaceAllUsesWith(
                    ConstantInt::getNullValue(rhs->getType()));
                  instToErase.push_back(op);
                  strengthReductionTimes++;
                }
              }
              break;
            }
            default:
              break;
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

  mOut << "Strength reduction applied " << strengthReductionTimes
       << " times.\n";

  // 返回所有分析未被破坏
  return PreservedAnalyses::all();
}