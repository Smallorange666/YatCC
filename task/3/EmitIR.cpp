#include "EmitIR.hpp"
#include <llvm/Transforms/Utils/ModuleUtils.h>

#define self (*this)

using namespace asg;

EmitIR::EmitIR(Obj::Mgr& mgr, llvm::LLVMContext& ctx, llvm::StringRef mid)
  : mMgr(mgr)
  , mMod(mid, ctx)
  , mCtx(ctx)
  , mIntTy(llvm::Type::getInt32Ty(ctx))
  , mCurIrb(std::make_unique<llvm::IRBuilder<>>(ctx))
  , mCtorTy(llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), false))
{
}

llvm::Module&
EmitIR::operator()(asg::TranslationUnit* tu)
{
  for (auto&& i : tu->decls)
    self(i);
  return mMod;
}

//==============================================================================
// 类型
//==============================================================================

llvm::Type*
EmitIR::operator()(const Type* type)
{
  if (type->texp == nullptr) {
    switch (type->spec) {
      case Type::Spec::kVoid:
        return llvm::Type::getVoidTy(mCtx);
      case Type::Spec::kChar:
        return llvm::Type::getInt8Ty(mCtx);
      case Type::Spec::kInt:
        return llvm::Type::getInt32Ty(mCtx);
      // TODO: 在此添加对更多基础类型的处理
      case Type::Spec::kLong:
        return llvm::Type::getInt64Ty(mCtx);
      case Type::Spec::kLongLong:
        return llvm::Type::getInt128Ty(mCtx);
      default:
        ABORT();
    }
  }

  // 递归地处理子类型
  Type subt;
  subt.spec = type->spec;
  subt.qual = type->qual;
  subt.texp = type->texp->sub;

  // TODO: 在此添加对指针类型、数组类型和函数类型的处理
  if (auto p = type->texp->dcst<PointerType>()) {
    auto subTy = self(&subt);
    return llvm::PointerType::get(subTy, 0);
  }

  if (auto p = type->texp->dcst<ArrayType>()) {
    auto subTy = self(&subt);

    if (p->len == ArrayType::kUnLen)
      return llvm::ArrayType::get(subTy, 0);
    else
      return llvm::ArrayType::get(subTy, p->len);
  }

  if (auto p = type->texp->dcst<FunctionType>()) {
    std::vector<llvm::Type*> pty;
    // TODO: 在此添加对函数参数类型的处理
    for (auto&& i : p->params) {
      auto paramTy = self(i);
      pty.push_back(paramTy);
    }

    return llvm::FunctionType::get(self(&subt), std::move(pty), false);
  }

  ABORT();
}

//==============================================================================
// 表达式
//==============================================================================

llvm::Value*
EmitIR::operator()(Expr* obj)
{
  // TODO: 在此添加对更多表达式处理的跳转
  if (auto p = obj->dcst<IntegerLiteral>())
    return self(p);

  if (auto p = obj->dcst<DeclRefExpr>())
    return self(p);

  if (auto p = obj->dcst<ParenExpr>())
    return self(p);

  if (auto p = obj->dcst<UnaryExpr>())
    return self(p);

  if (auto p = obj->dcst<BinaryExpr>())
    return self(p);

  if (auto p = obj->dcst<CallExpr>())
    return self(p);

  if (auto p = obj->dcst<ImplicitCastExpr>())
    return self(p);

  ABORT();
}

llvm::Constant*
EmitIR::operator()(IntegerLiteral* obj)
{
  return llvm::ConstantInt::get(self(obj->type), obj->val);
}

llvm::Value*
EmitIR::operator()(DeclRefExpr* obj)
{
  // 在LLVM IR层面，左值体现为返回指向值的指针
  // 声明创建时，会把地址放到any里
  // 在ImplicitCastExpr::kLValueToRValue中发射load指令从而变成右值
  return reinterpret_cast<llvm::Value*>(obj->decl->any);
}

llvm::Value*
EmitIR::operator()(ParenExpr* obj)
{
  return self(obj->sub);
}

llvm::Value*
EmitIR::operator()(UnaryExpr* obj)
{
  auto subVal = self(obj->sub);

  auto& irb = *mCurIrb;
  switch (obj->op) {
    case UnaryExpr::kPos:
      return subVal;
    case UnaryExpr::kNeg:
      return irb.CreateNeg(subVal);
    case UnaryExpr::kNot: {
      if (!subVal->getType()->isIntegerTy(1)) {
        auto subTy = subVal->getType();
        subVal = irb.CreateICmpNE(subVal, llvm::ConstantInt::get(subTy, 0));
      }
      return irb.CreateXor(subVal, irb.getTrue());
    }
    default:
      ABORT();
  }
}

llvm::Value*
EmitIR::operator()(ImplicitCastExpr* obj)
{
  auto sub = self(obj->sub);

  auto& irb = *mCurIrb;
  switch (obj->kind) {
    case ImplicitCastExpr::kLValueToRValue: {
      auto ty = self(obj->sub->type);
      auto loadVal = irb.CreateLoad(ty, sub);
      return loadVal;
    }
    case ImplicitCastExpr::kFunctionToPointerDecay: {
      return sub;
    }
    case ImplicitCastExpr::kArrayToPointerDecay: {
      return sub;
    }
    default:
      ABORT();
  }
}

llvm::Value*
EmitIR::operator()(BinaryExpr* obj)
{
  auto& irb = *mCurIrb;
  llvm::Value *lftVal, *rhtVal;

  if (obj->op == BinaryExpr::kAnd) {
    auto rhsBb = llvm::BasicBlock::Create(mCtx, "land.rhs", mCurFunc);
    auto mergeBb = llvm::BasicBlock::Create(mCtx, "land.end", mCurFunc);

    lftVal = self(obj->lft);
    // 紧跟着生成左值的语句后面，获取当前基本块，保证拿到左值所在的基本块
    auto lvalBb = irb.GetInsertBlock();
    if (!lftVal->getType()->isIntegerTy(1)) {
      auto lftTy = lftVal->getType();
      lftVal = irb.CreateICmpNE(lftVal, llvm::ConstantInt::get(lftTy, 0));
    }
    irb.CreateCondBr(lftVal, rhsBb, mergeBb);

    irb.SetInsertPoint(rhsBb);
    rhtVal = self(obj->rht);
    // 紧跟着生成右值的语句后面，获取当前基本块，保证拿到右值所在的基本块
    auto rvalBb = irb.GetInsertBlock();
    // 逻辑表达式左右都应该是bool类型
    if (!rhtVal->getType()->isIntegerTy(1)) {
      auto rhtTy = rhtVal->getType();
      rhtVal = irb.CreateICmpNE(rhtVal, llvm::ConstantInt::get(rhtTy, 0));
    }
    irb.CreateBr(mergeBb);

    irb.SetInsertPoint(mergeBb);
    auto* phi = irb.CreatePHI(llvm::Type::getInt1Ty(mCtx), 2, "land");
    phi->addIncoming(irb.getFalse(), lvalBb);
    phi->addIncoming(rhtVal, rvalBb);

    return phi;
  } else if (obj->op == BinaryExpr::kOr) {
    auto rhsBb = llvm::BasicBlock::Create(mCtx, "lor.rhs", mCurFunc);
    auto mergeBb = llvm::BasicBlock::Create(mCtx, "lor.end", mCurFunc);

    lftVal = self(obj->lft);
    // 紧跟着生成左值的语句后面，获取当前基本块，保证拿到左值所在的基本块
    auto lvalBb = irb.GetInsertBlock();
    if (!lftVal->getType()->isIntegerTy(1)) {
      auto lftTy = lftVal->getType();
      lftVal = irb.CreateICmpNE(lftVal, llvm::ConstantInt::get(lftTy, 0));
    }
    irb.CreateCondBr(lftVal, mergeBb, rhsBb);

    irb.SetInsertPoint(rhsBb);
    rhtVal = self(obj->rht);
    // 紧跟着生成右值的语句后面，获取当前基本块，保证拿到右值所在的基本块
    auto rvalBb = irb.GetInsertBlock();
    // 逻辑表达式左右都应该是bool类型
    if (!rhtVal->getType()->isIntegerTy(1)) {
      auto rhtTy = rhtVal->getType();
      rhtVal = irb.CreateICmpNE(rhtVal, llvm::ConstantInt::get(rhtTy, 0));
    }
    irb.CreateBr(mergeBb);

    irb.SetInsertPoint(mergeBb);
    auto* phi = irb.CreatePHI(llvm::Type::getInt1Ty(mCtx), 2, "lor");
    phi->addIncoming(irb.getTrue(), lvalBb);
    phi->addIncoming(rhtVal, rvalBb);

    return phi;
  } else {
    lftVal = self(obj->lft);
    rhtVal = self(obj->rht);
    switch (obj->op) {
      case BinaryExpr::kAdd:
        return irb.CreateAdd(lftVal, rhtVal);
      case BinaryExpr::kSub:
        return irb.CreateSub(lftVal, rhtVal);
      case BinaryExpr::kMul:
        return irb.CreateMul(lftVal, rhtVal);
      case BinaryExpr::kDiv:
        return irb.CreateSDiv(lftVal, rhtVal);
      case BinaryExpr::kMod:
        return irb.CreateSRem(lftVal, rhtVal);
      case BinaryExpr::kGt:
        return irb.CreateICmpSGT(lftVal, rhtVal);
      case BinaryExpr::kLt:
        return irb.CreateICmpSLT(lftVal, rhtVal);
      case BinaryExpr::kGe:
        return irb.CreateICmpSGE(lftVal, rhtVal);
      case BinaryExpr::kLe:
        return irb.CreateICmpSLE(lftVal, rhtVal);
      case BinaryExpr::kEq:
        return irb.CreateICmpEQ(lftVal, rhtVal);
      case BinaryExpr::kNe:
        return irb.CreateICmpNE(lftVal, rhtVal);
      case BinaryExpr::kAssign:
        return irb.CreateStore(rhtVal, lftVal);
      case BinaryExpr::kComma:
        ABORT();
      case BinaryExpr::kIndex: {
        auto subTy = obj->lft->dcst<ImplicitCastExpr>()->sub->type;
        Type tem;
        tem.spec = subTy->spec;
        tem.qual = subTy->qual;
        tem.texp = subTy->texp->sub;
        auto subArrayTy = self(&tem);

        return irb.CreateInBoundsGEP(subArrayTy, lftVal, { rhtVal });
      }
      default:
        ABORT();
    }
  }
}

llvm::Value*
EmitIR::operator()(CallExpr* obj)
{
  auto imcast = obj->head->dcst<ImplicitCastExpr>();
  if (imcast == nullptr)
    ABORT();
  auto declref = imcast->sub->dcst<DeclRefExpr>();
  if (declref == nullptr)
    ABORT();

  auto func = mMod.getFunction(declref->decl->name);

  std::vector<llvm::Value*> args;
  for (auto&& arg : obj->args)
    args.push_back(self(arg));

  return mCurIrb->CreateCall(func, args);
}

//==============================================================================
// 语句
//==============================================================================

void
EmitIR::operator()(Stmt* obj)
{
  // TODO: 在此添加对更多Stmt类型的处理的跳转
  if (auto p = obj->dcst<NullStmt>())
    return self(p);

  if (auto p = obj->dcst<DeclStmt>())
    return self(p);

  if (auto p = obj->dcst<ExprStmt>())
    return self(p);

  if (auto p = obj->dcst<CompoundStmt>())
    return self(p);

  if (auto p = obj->dcst<IfStmt>())
    return self(p);

  if (auto p = obj->dcst<WhileStmt>())
    return self(p);

  if (auto p = obj->dcst<DoStmt>())
    return self(p);

  if (auto p = obj->dcst<BreakStmt>())
    return self(p);

  if (auto p = obj->dcst<ContinueStmt>())
    return self(p);

  if (auto p = obj->dcst<ReturnStmt>())
    return self(p);

  ABORT();
}

// TODO: 在此添加对更多Stmt类型的处理
void
EmitIR::operator()(NullStmt* obj)
{
  return;
}

void
EmitIR::operator()(DeclStmt* obj)
{
  for (auto&& decl : obj->decls) {
    if (auto p = decl->dcst<VarDecl>()) {
      self(p, false);
    } else
      ABORT();
  }
}

void
EmitIR::operator()(ExprStmt* obj)
{
  self(obj->expr);
}

void
EmitIR::operator()(CompoundStmt* obj)
{
  // TODO: 可以在此添加对符号重名的处理
  for (auto&& stmt : obj->subs)
    self(stmt);
}

void
EmitIR::operator()(IfStmt* obj)
{
  auto& irb = *mCurIrb;

  auto thenBb = llvm::BasicBlock::Create(mCtx, "if.then", mCurFunc);
  auto mergeBb = llvm::BasicBlock::Create(mCtx, "if.end", mCurFunc);
  auto elseBb = llvm::BasicBlock::Create(mCtx, "if.else", mCurFunc);

  // 直接在原基本块开始判断if的条件
  auto condVal = self(obj->cond);
  if (!condVal->getType()->isIntegerTy(1)) {
    auto condTy = condVal->getType();
    condVal = irb.CreateICmpNE(condVal, llvm::ConstantInt::get(condTy, 0));
  }
  irb.CreateCondBr(condVal, thenBb, elseBb);

  irb.SetInsertPoint(thenBb);
  self(obj->then);
  // 判断是否有终结语句（如return），如果没有则跳转到mergeBb
  if (irb.GetInsertBlock()->getTerminator() == nullptr)
    irb.CreateBr(mergeBb);

  irb.SetInsertPoint(elseBb);
  if (obj->else_ != nullptr) {
    // 如果有else部分，需要生成
    self(obj->else_);
    // 判断是否有终结语句（如return），如果没有则跳转到mergeBb
    if (irb.GetInsertBlock()->getTerminator() == nullptr)
      irb.CreateBr(mergeBb);
  } else {
    // 如果没有else部分，则直接跳转到mergeBb
    irb.CreateBr(mergeBb);
  }

  mCurIrb->SetInsertPoint(mergeBb);
}

void
EmitIR::operator()(WhileStmt* obj)
{
  auto& irb = *mCurIrb;

  auto condBb = llvm::BasicBlock::Create(mCtx, "while.cond", mCurFunc);
  auto bodyBb = llvm::BasicBlock::Create(mCtx, "while.body", mCurFunc);
  auto mergeBb = llvm::BasicBlock::Create(mCtx, "while.end", mCurFunc);

  // 条件块和终结块入栈，为break和continue语句服务
  mCondBbStack.push(condBb);
  mMergeBbStack.push(mergeBb);

  // 直接在原基本块开始判断while的条件
  irb.CreateBr(condBb);

  // 条件块生成
  irb.SetInsertPoint(condBb);
  auto condVal = self(obj->cond);
  irb.CreateCondBr(condVal, bodyBb, mergeBb);

  // 循环体块生成
  irb.SetInsertPoint(bodyBb);
  self(obj->body);
  // 判断是否已经生成了终结语句（如return、break、continue）
  // 如果没有则跳转到终结块
  if (irb.GetInsertBlock()->getTerminator() == nullptr)
    irb.CreateBr(condBb);

  mCurIrb->SetInsertPoint(mergeBb);

  mCondBbStack.pop();
  mMergeBbStack.pop();
}

void
EmitIR::operator()(DoStmt* obj)
{
  auto& irb = *mCurIrb;

  auto condBb = llvm::BasicBlock::Create(mCtx, "do.cond", mCurFunc);
  auto bodyBb = llvm::BasicBlock::Create(mCtx, "do.body", mCurFunc);
  auto mergeBb = llvm::BasicBlock::Create(mCtx, "do.end", mCurFunc);

  irb.SetInsertPoint(bodyBb);
  self(obj->body);
  irb.CreateBr(condBb);

  irb.SetInsertPoint(condBb);
  auto condVal = self(obj->cond);
  irb.CreateCondBr(condVal, mergeBb, bodyBb);

  mCurIrb->SetInsertPoint(mergeBb);
}

void
EmitIR::operator()(BreakStmt* obj)
{
  auto& irb = *mCurIrb;

  auto lastMergeBb = mMergeBbStack.top();
  irb.CreateBr(lastMergeBb);
}

void
EmitIR::operator()(ContinueStmt* obj)
{
  auto& irb = *mCurIrb;

  auto lastCondBb = mCondBbStack.top();
  irb.CreateBr(lastCondBb);
}

void
EmitIR::operator()(ReturnStmt* obj)
{
  auto& irb = *mCurIrb;

  llvm::Value* retVal;
  if (!obj->expr)
    retVal = nullptr;
  else
    retVal = self(obj->expr);

  mCurIrb->CreateRet(retVal);

  auto exitBb = llvm::BasicBlock::Create(mCtx, "return_exit", mCurFunc);
  mCurIrb->SetInsertPoint(exitBb);
}

//==============================================================================
// 声明
//==============================================================================

void
EmitIR::operator()(Decl* obj)
{
  // TODO: 添加变量声明处理的跳转
  if (auto p = obj->dcst<VarDecl>())
    return self(p);

  if (auto p = obj->dcst<FunctionDecl>())
    return self(p);

  ABORT();
}

// TODO: 添加变量声明的处理
void
EmitIR::init(llvm::Value* val, Expr* obj)
{
  auto& irb = *mCurIrb;

  // 利用整数字面量的初始化
  if (auto p = obj->dcst<IntegerLiteral>()) {
    auto initVal = llvm::ConstantInt::get(self(p->type), p->val);
    irb.CreateStore(initVal, val);
    return;
  }

  if (auto p = obj->dcst<UnaryExpr>()) {
    auto initVal = self(p);
    irb.CreateStore(initVal, val);
    return;
  }

  if (auto p = obj->dcst<BinaryExpr>()) {
    auto initVal = self(p);
    irb.CreateStore(initVal, val);
    return;
  }

  // 利用函数调用的初始化
  if (auto p = obj->dcst<CallExpr>()) {
    auto initVal = self(p);
    irb.CreateStore(initVal, val);
    return;
  }

  // 利用初始化列表的初始化（数组）
  if (auto p = obj->dcst<InitListExpr>()) {
    if (p->list.size() == 0) {
      return;
    }

    std::vector<llvm::Value*> idxs = { irb.getInt32(0) };
    init_array(val, self(obj->type), idxs, obj);
    return;
  }

  // 利用已经声明的变量的初始化
  if (auto p = obj->dcst<ImplicitCastExpr>()) {
    auto initVal = self(p);
    irb.CreateStore(initVal, val);
    return;
  }

  // 如果表达式不是整数字面量，则中断编译
  ABORT();
}

void
EmitIR::init_array(llvm::Value* base,
                   llvm::Type* baseTy,
                   std::vector<llvm::Value*>& idxs,
                   Expr* obj)
{
  auto& irb = *mCurIrb;

  if (auto p = obj->dcst<InitListExpr>()) {
    for (int i = 0; i < p->list.size(); i++) {
      auto sub = p->list[i];
      idxs.push_back(llvm::ConstantInt::get(mIntTy, i));
      init_array(base, baseTy, idxs, sub);
      idxs.pop_back();
    }
  } else if (auto p = obj->dcst<Expr>()) {
    // 空初始化列表
    if (auto q = obj->dcst<ImplicitInitExpr>()) {
      auto addr = irb.CreateInBoundsGEP(baseTy, base, { irb.getInt32(0) });
      irb.CreateStore(llvm::Constant::getNullValue(baseTy), addr);
      return;
    }

    auto subVal = self(p);
    if (subVal) {
      auto addr = irb.CreateInBoundsGEP(baseTy, base, idxs);
      irb.CreateStore(subVal, addr);
    }
  }
}

void
EmitIR::operator()(VarDecl* obj, bool isGlobal)
{
  if (isGlobal) {
    // 判断当前是否在函数体内
    auto ty = self(obj->type);
    auto gvar = new llvm::GlobalVariable(mMod,
                                         ty,
                                         false,
                                         llvm::GlobalVariable::ExternalLinkage,
                                         nullptr,
                                         obj->name);
    obj->any = gvar;

    // 零初始化
    gvar->setInitializer(llvm::Constant::getNullValue(ty));

    if (obj->init == nullptr)
      return;

    mCurFunc = llvm::Function::Create(
      mCtorTy, llvm::GlobalVariable::PrivateLinkage, "ctor_" + obj->name, mMod);
    llvm::appendToGlobalCtors(mMod, mCurFunc, 65535);
    auto entryBb = llvm::BasicBlock::Create(mCtx, "entry", mCurFunc);
    mCurIrb->SetInsertPoint(entryBb);
    init(gvar, obj->init);
    mCurIrb->CreateRet(nullptr);
    mCurFunc = nullptr;
  } else {
    auto ty = self(obj->type);
    auto alloca = mCurIrb->CreateAlloca(ty, nullptr, obj->name);
    obj->any = alloca;

    if (obj->init != nullptr) {
      init(alloca, obj->init);
    }
  }
}

void
EmitIR::operator()(FunctionDecl* obj)
{
  // 创建函数
  auto fty = llvm::dyn_cast<llvm::FunctionType>(self(obj->type));
  auto func = llvm::Function::Create(
    fty, llvm::GlobalVariable::ExternalLinkage, obj->name, mMod);

  obj->any = func;

  if (obj->body == nullptr)
    return;
  auto entryBb = llvm::BasicBlock::Create(mCtx, "entry", func);
  mCurIrb->SetInsertPoint(entryBb);
  auto& entryIrb = *mCurIrb;

  // TODO: 添加对函数参数的处理
  int count = 0;
  for (auto param = func->arg_begin(); param != func->arg_end();
       ++param, ++count) {
    auto vardecl = obj->params[count];
    param->setName(vardecl->name);

    // 在entry块分配空间
    llvm::AllocaInst* alloca =
      entryIrb.CreateAlloca(param->getType(), nullptr, vardecl->name);
    entryIrb.CreateStore(param, alloca);
    vardecl->any = alloca;
  }

  // 翻译函数体
  mCurFunc = func;
  self(obj->body);

  // 如果函数没有返回值，则创建一个空的返回块
  auto& exitIrb = *mCurIrb;
  // 输出基本块的名字
  if (fty->getReturnType()->isVoidTy())
    exitIrb.CreateRetVoid();
  else
    exitIrb.CreateUnreachable();
}
