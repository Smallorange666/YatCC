/* 生成.output文件 */
%verbose

/* 用于调试 (yydebug) */
%define parse.trace

%code top {
int yylex (void);             // 该函数由 Flex 生成
void yyerror (char const *);	// 该函数定义在 par.cpp 中
}

%code requires {
#include "par.hpp"
#include <iostream>
}

%union {
  std::string* RawStr;
  par::Decls* Decls;
  par::Exprs* Exprs;

  asg::TranslationUnit* TranslationUnit;
  asg::Type* Type;
  asg::Expr* Expr;
  asg::Decl* Decl;
  asg::FunctionDecl* FunctionDecl;
  asg::Stmt* Stmt;
  asg::CompoundStmt* CompoundStmt;
  asg::ExprStmt* ExprStmt;
  asg::ReturnStmt* ReturnStmt;
  asg::IfStmt* IfStmt;
  asg::WhileStmt* WhileStmt;
  asg::DoStmt* DoStmt;
  asg::ContinueStmt* ContinueStmt;
  asg::BreakStmt* BreakStmt;
  asg::NullStmt* NullStmt;
}

/* 在下面说明每个非终结符对应的 union 成员，以便进行编译期类型检查 */
%type <Type> declaration_specifiers type_specifier type_qualifier

%type <Expr> additive_expression multiplicative_expression unary_expression postfix_expression
%type <Expr> expression primary_expression assignment_expression initializer initializer_list
%type <Expr> logical_or_expression logical_and_expression equality_expression relational_expression

%type <Stmt> block_item statement null_statement
%type <CompoundStmt> compound_statement block_item_list
%type <ExprStmt> expression_statement
%type <ReturnStmt> jump_statement
%type <BreakStmt> break_statement
%type <ContinueStmt> continue_statement
%type <IfStmt> if_statement
%type <WhileStmt> while_statement
%type <DoStmt> do_statement

%type <Decls> external_declaration declaration init_declarator_list parameter_list
%type <Exprs> argument_expression_list
%type <FunctionDecl> function_definition
%type <Decl> declarator init_declarator parameter_declaration

%type <TranslationUnit> translation_unit

%token <RawStr> IDENTIFIER CONSTANT STRING_LITERAL
%token VOID INT CHAR LONG CONST
%token BREAK CONTINUE DO ELSE IF RETURN WHILE
%token L_PAREN R_PAREN L_BRACE R_BRACE L_SQUARE R_SQUARE
%token PLUS MINUS STAR SLASH PERCENT GREATER LESS GREATEREQUAL LESSEQUAL EQUALEQUAL EXCLAIMEQUAL AMPAMP PIPEPIPE EXCLAIM
%token SEMI COMMA 
%token EQUAL


%start start

%%
start
  :	{
      par::Symtbl::g = new par::Symtbl();
    }
    translation_unit
    {
      par::gTranslationUnit = $2;
      delete par::Symtbl::g;
    }
  ;

translation_unit
  : external_declaration
    {
      $$ = par::gMgr.make<asg::TranslationUnit>();
      for (auto&& decl: *$1)
        $$->decls.push_back(decl);
      delete $1;
    }
  | translation_unit external_declaration
    {
      $$ = $1;
      for (auto&& decl: *$2)
        $$->decls.push_back(decl);
      delete $2;
    }
  ;

external_declaration
  : function_definition
    {
      $$ = new par::Decls();
      $$->push_back($1);
    }
  | declaration { $$ = $1; }
  ;

function_definition
  : declaration_specifiers declarator
    {
      auto funcDecl = $2->dcst<asg::FunctionDecl>();
      ASSERT(funcDecl);
      // 设置当前全局的函数作用变量
      par::gCurrentFunction = funcDecl; 
      auto ty = par::gMgr.make<asg::Type>();
      if (funcDecl->type != nullptr)
        ty->texp = funcDecl->type->texp; 
      ty->spec = $1->spec, ty->qual = $1->qual;
      funcDecl->type = ty;
    }
    compound_statement
    {	
      $$ = par::gCurrentFunction;
      $$->name = $2->name;
      $$->body = $4;
    }
  ;

declaration
  : declaration_specifiers init_declarator_list SEMI
    {
      for (auto decl: *$2)
      {
        auto ty = par::gMgr.make<asg::Type>();
        if (decl->type != nullptr)
          ty->texp = decl->type->texp; // 保留前面 ArrayType 的texp
        ty->spec = $1->spec, ty->qual = $1->qual;
        decl->type = ty;

        auto varDecl = dynamic_cast<asg::VarDecl*>(decl);
        if (varDecl != nullptr)
        {
          if (varDecl->init != nullptr)
            varDecl->init->type = decl->type;
        }
      }
      $$ = $2;
    }
  ;

declaration_specifiers
  : type_specifier { $$ = $1; }
  | type_specifier declaration_specifiers
    {
      $$ = $2;
      $$->spec = $1->spec;
    }
  | type_qualifier
    {
      $$ = par::gMgr.make<asg::Type>();
      $$->qual = $1->qual;
    }
  | type_qualifier declaration_specifiers
    {
      $$ = $2;
      $$->qual = $1->qual;
    } 
  ;

init_declarator_list
  : init_declarator
    {
      $$ = new par::Decls();
      $$->push_back($1);
    }
  | init_declarator_list COMMA init_declarator
    {
      $$ = $1;
      $$->push_back($3);
    }
  ;

// 初始化声明符 -> 声明符
// a
// 初始化声明符 -> 声明符 = 初始化器
// a = 1
init_declarator
  : declarator { $$ = $1; }
  | declarator EQUAL initializer
    {
      auto varDecl = $1->dcst<asg::VarDecl>();
      ASSERT(varDecl);
      $3->type = varDecl->type;
      varDecl->init = $3;
      $$ = varDecl;
    }
  ;

// 初始化右值
initializer
  : assignment_expression
    {
      auto callExpr = $1->dcst<asg::CallExpr>();
      if (callExpr != nullptr)
      {
        auto implicitCastExpr = dynamic_cast<asg::ImplicitCastExpr*>(callExpr->head);
        if(implicitCastExpr != nullptr) // ？？？
        {
          auto declRefExpr = dynamic_cast<asg::DeclRefExpr*>(implicitCastExpr->sub);
        }
        $$ = callExpr;
      }
      else
      {
        auto p = par::gMgr.make<asg::InitListExpr>();
        p->list.push_back($1);
        $$ = p;
      }
      $$->type = $1->type;
    }
  | L_BRACE initializer_list R_BRACE
    {
      $$ = $2;
    }
  | L_BRACE initializer_list COMMA R_BRACE
    {
      $$ = $2;
    }
  ;

// 初始化列表
initializer_list
  :  %empty
    {
      $$ = par::gMgr.make<asg::InitListExpr>();
    }
  | initializer
    {
      $$ = $1;
    }
  | initializer_list COMMA initializer
    {
      auto initListExpr3 = $3->dcst<asg::InitListExpr>();
      auto initListExpr1 = $1->dcst<asg::InitListExpr>();
      for(auto exper: initListExpr3->list)
        initListExpr1->list.push_back(exper);
      $$ = initListExpr1;
    }
  ;

// 类型说明符
type_specifier
  : VOID
    {
      $$ = par::gMgr.make<asg::Type>();
      $$->spec = asg::Type::Spec::kVoid;
    }
  | CHAR
    {
      $$ = par::gMgr.make<asg::Type>();
      $$->spec = asg::Type::Spec::kChar;
    }
  | INT
    {
      $$ = par::gMgr.make<asg::Type>();
      $$->spec = asg::Type::Spec::kInt;
    }
  | LONG
    {
      $$ = par::gMgr.make<asg::Type>();
      $$->spec = asg::Type::Spec::kLong;
    }
  ;

type_qualifier
  : CONST
    {
      $$ = par::gMgr.make<asg::Type>();
      $$->qual.const_=true;
    }

// 声明符 -> 标识符
// a
// 声明符 -> 声明符 [ ]
// a[]
// 声明符 -> 声明符 [ 表达式 ]
// a[1] 或 a[3+4]
// 声明符 -> 声明符 ( ) 
// a()
// 声明符 -> 声明符 ( 参数列表 )
// a(int, int)
declarator
  : IDENTIFIER
    {
      $$ = par::gMgr.make<asg::VarDecl>();
      $$->name = std::move(*$1);
      delete $1;

      // 插入符号表
      par::Symtbl::g->insert_or_assign($$->name, $$);
    }
  | declarator L_SQUARE R_SQUARE // 未知长度数组
    {
      $$ = $1; 
      // 填充Type
      auto ty = par::gMgr.make<asg::Type>();
      if ($$->type != nullptr)
        ty->texp=$$->type->texp;
      auto p = par::gMgr.make<asg::ArrayType>();
      p->len = asg::ArrayType::kUnLen;
      if (ty->texp == nullptr)
      {
        ty->texp = p;
      }
      else
      {
        ty->texp->sub = p;
      }
      $$->type = ty;

      // 插入符号表
      par::Symtbl::g->insert_or_assign($$->name, $$);
    }
  | declarator L_SQUARE assignment_expression R_SQUARE // 数组定义
    {
      $$ = $1; 
      // 填充Type
      auto ty = par::gMgr.make<asg::Type>();
      if ($$->type != nullptr)
        ty->texp=$$->type->texp;
      auto p = par::gMgr.make<asg::ArrayType>();
      auto integerLiteral = $3->dcst<asg::IntegerLiteral>();
      ASSERT(integerLiteral);
      p->len = integerLiteral->val;
      if (ty->texp == nullptr)
      {
        ty->texp = p;
      }
      else
      {
        ty->texp->sub = p;
      }
      $$->type = ty;

      // 插入符号表
      par::Symtbl::g->insert_or_assign($$->name, $$);
    }
  | declarator L_PAREN R_PAREN
    {
      $$ = par::gMgr.make<asg::FunctionDecl>();
      $$->name = $1->name;
      auto ty = par::gMgr.make<asg::Type>();
      auto p = par::gMgr.make<asg::FunctionType>();
      ty->texp = p;
      $$->type = ty;

      // 插入符号表
      par::Symtbl::g->insert_or_assign($$->name, $$);
    }
  // 函数列表的定义
  | declarator L_PAREN parameter_list R_PAREN
    {
      auto p = par::gMgr.make<asg::FunctionDecl>();
      p->name = $1->name;
      p->params = *$3;
      auto ty = par::gMgr.make<asg::Type>();
      auto functionType = par::gMgr.make<asg::FunctionType>();
      for (auto decl: *$3)
      {
        functionType->params.push_back(decl->type);
      }
      ty->texp = functionType;
      p->type = ty;
      $$ = p;

      // 插入符号表
      par::Symtbl::g->insert_or_assign($$->name, $$);
    }
  ;

// 参数列表 -> 参数声明
// int a
// 参数列表 -> 参数列表, 参数声明
// int a, int b
parameter_list
  : parameter_declaration
    {
      $$ = new par::Decls();
      $$->push_back($1);
    }
  | parameter_list COMMA parameter_declaration
    {
      $$ = $1;
      $$->push_back($3);
    }
  ;

// 参数声明 -> 声明说明符 声明符
// int a
parameter_declaration
  : declaration_specifiers declarator
    {
      // 保留之前定义的 Type
      auto ty = par::gMgr.make<asg::Type>();
      if ($2->type != nullptr)
        ty->texp = $2->type->texp;
      ty->spec = $1->spec, ty->qual = $1->qual;
      $2->type = ty;
      $$ = $2;
    }
  ;



//=========================================
// 语句
//=========================================
statement
  : compound_statement { $$ = $1; }
  | null_statement { $$ = $1; }
  | expression_statement { $$ = $1; }
  | if_statement { $$ = $1; }
  | while_statement { $$ = $1; }
  | do_statement { $$ = $1; }
  | jump_statement { $$ = $1; }
  | break_statement { $$ = $1; }
  | continue_statement { $$ = $1; }
  ;

// 复合语句 -> {}
// {}
// 复合语句 -> { 语句块列表 }
// { int a; a=1; {int b;}}
compound_statement
  : %empty {$$ = par::gMgr.make<asg::CompoundStmt>();} // 代码块为空的情况
  | L_BRACE R_BRACE { $$ = par::gMgr.make<asg::CompoundStmt>(); }
  | L_BRACE
    { new par::Symtbl(); } 		// 开启新的符号表作用域
    block_item_list
    R_BRACE
    {
      delete par::Symtbl::g; 	// 结束符号表作用域
      $$ = $block_item_list;
    }
  ;

// 语句块列表 -> 语句块
// int a;
// 语句块列表 -> 语句块列表 语句块
// int a; a=1; {int b;}
block_item_list
  : block_item
    {
      $$ = par::gMgr.make<asg::CompoundStmt>();
      $$->subs.push_back($1);
    }
  | block_item_list block_item
    {
      $$ = $1;
      $$->subs.push_back($2);
    }
  ;

// 语句块 -> 声明
// int a;
// 语句块 -> 语句
// a=1;
block_item
  : declaration
    {
      auto p = par::gMgr.make<asg::DeclStmt>();
      for (auto decl: *$1)
        p->decls.push_back(decl);
      $$ = p;
    }
  | statement { $$ = $1; }
  ;

null_statement
  : SEMI
    {
      $$ = par::gMgr.make<asg::NullStmt>();
    }
  ;

expression_statement
  : expression SEMI
    {
      $$ = par::gMgr.make<asg::ExprStmt>();
      $$->expr = $1;
    }
  ;

if_statement
  : IF L_PAREN expression R_PAREN statement
    {
      $$ = par::gMgr.make<asg::IfStmt>();
      $$->cond = $3;
      $$->then = $5;
    }
  | IF L_PAREN expression R_PAREN statement ELSE statement
    {
      $$ = par::gMgr.make<asg::IfStmt>();
      $$->cond = $3;
      $$->then = $5;
      $$->else_ = $7;
    }
  ;

while_statement
  : WHILE L_PAREN expression R_PAREN statement
    {
      $$ = par::gMgr.make<asg::WhileStmt>();
      $$->cond = $3;
      $$->body = $5;
    }
  ;

do_statement
  : DO statement WHILE L_PAREN expression R_PAREN SEMI
    {
      $$ = par::gMgr.make<asg::DoStmt>();
      $$->cond = $5;
      $$->body = $2;
    }
  ;


jump_statement
  :  RETURN SEMI
    {
      $$ = par::gMgr.make<asg::ReturnStmt>();
      $$->func = par::gCurrentFunction;
    }
    | RETURN expression SEMI
    {
      $$ = par::gMgr.make<asg::ReturnStmt>();
      $$->func = par::gCurrentFunction;
      $$->expr = $2;
    }
  ;

break_statement
  : BREAK SEMI
    {
      $$ = par::gMgr.make<asg::BreakStmt>();
    }
  ;

continue_statement
  : CONTINUE SEMI
    {
      $$ = par::gMgr.make<asg::ContinueStmt>();
    }
  ;

//=========================================
// 表达式
//=========================================

primary_expression
  : IDENTIFIER
    {
      // 查找符号表, 找到对应的Decl
      auto decl = par::Symtbl::resolve(*$1);
      ASSERT(decl);
      delete $1;
      auto p = par::gMgr.make<asg::DeclRefExpr>();
      p->decl = decl;
      $$ = p;
    }
  | CONSTANT
    {
      auto p = par::gMgr.make<asg::IntegerLiteral>();
      p->val = std::stoull(*$1, nullptr, 0);
      delete $1;
      $$ = p;
    }
  | STRING_LITERAL
    {
      auto p = par::gMgr.make<asg::StringLiteral>();
      p->val = *$1;
      delete $1;
      $$ = p;
    }
  | L_PAREN expression R_PAREN
    {
      auto p= par::gMgr.make<asg::ParenExpr>();
      p->sub = $2;
      $$ = p;
    }
  ;

postfix_expression
  : primary_expression { $$ = $1; }
  | postfix_expression L_SQUARE expression R_SQUARE 
    {
      auto p = par::gMgr.make<asg::BinaryExpr>();
      p->op = asg::BinaryExpr::Op::kIndex;
      p->lft = $1, p->rht = $3;
      $$ = p;
    }
  | postfix_expression L_PAREN R_PAREN
    {
      auto p = par::gMgr.make<asg::CallExpr>();
      p->head = $1;
      $$ = p;
    }
  | postfix_expression L_PAREN argument_expression_list R_PAREN
    {
      auto p = par::gMgr.make<asg::CallExpr>();
      p->head = $1;
      for (auto&& expr: *$3)
        p->args.push_back(expr);
      $$ = p;
    }
  ;


argument_expression_list
  : assignment_expression
    {
      $$ = new par::Exprs();
      $$->push_back($1);
    }
  | argument_expression_list COMMA assignment_expression
    {
      $$ = $1;
      $$->push_back($3);
    }
  ;
  
// 一元表达式
unary_expression
  : postfix_expression { $$ = $1;}
  | PLUS unary_expression
    {
      auto p = par::gMgr.make<asg::UnaryExpr>();
      p->op = asg::UnaryExpr::Op::kPos;
      p->sub = $2;
      $$ = p;
    }
  | MINUS unary_expression
    {
      auto p = par::gMgr.make<asg::UnaryExpr>();
      p->op = asg::UnaryExpr::Op::kNeg;
      p->sub = $2;
      $$ = p;
    }
  | EXCLAIM unary_expression
    {
      auto p = par::gMgr.make<asg::UnaryExpr>();
      p->op = asg::UnaryExpr::Op::kNot;
      p->sub = $2;
      $$ = p;
    }
  ;

multiplicative_expression
  : unary_expression  { $$ = $1;}
  | multiplicative_expression STAR unary_expression
    {
      auto p = par::gMgr.make<asg::BinaryExpr>();
      p->op = asg::BinaryExpr::Op::kMul;
      p->lft = $1, p->rht = $3;
      $$ = p;
    }
  | multiplicative_expression SLASH unary_expression
    {
      auto p = par::gMgr.make<asg::BinaryExpr>();
      p->op = asg::BinaryExpr::Op::kDiv;
      p->lft = $1, p->rht = $3;
      $$ = p;
    }
  | multiplicative_expression PERCENT unary_expression
    {
      auto p = par::gMgr.make<asg::BinaryExpr>();
      p->op = asg::BinaryExpr::Op::kMod;
      p->lft = $1, p->rht = $3;
      $$ = p;
    }
  ;

additive_expression
  : multiplicative_expression { $$ = $1;}
  | additive_expression PLUS multiplicative_expression
    {
      auto p = par::gMgr.make<asg::BinaryExpr>();
      p->op = asg::BinaryExpr::Op::kAdd;
      p->lft = $1, p->rht = $3;
      $$ = p;
    }
  | additive_expression MINUS multiplicative_expression
    {
      auto p = par::gMgr.make<asg::BinaryExpr>();
      p->op = asg::BinaryExpr::Op::kSub;
      p->lft = $1, p->rht = $3;
      $$ = p;
    }
  ;

relational_expression
  : additive_expression { $$ = $1; }
  | relational_expression LESS additive_expression
    {
      auto p = par::gMgr.make<asg::BinaryExpr>();
      p->op = asg::BinaryExpr::Op::kLt;
      p->lft = $1, p->rht = $3;
      $$ = p;
    }
  | relational_expression GREATER additive_expression
    {
      auto p = par::gMgr.make<asg::BinaryExpr>();
      p->op = asg::BinaryExpr::Op::kGt;
      p->lft = $1, p->rht = $3;
      $$ = p;
    }
  | relational_expression LESSEQUAL additive_expression
    {
      auto p = par::gMgr.make<asg::BinaryExpr>();
      p->op = asg::BinaryExpr::Op::kLe;
      p->lft = $1, p->rht = $3;
      $$ = p;
    }
  | relational_expression GREATEREQUAL additive_expression
    {
      auto p = par::gMgr.make<asg::BinaryExpr>();
      p->op = asg::BinaryExpr::Op::kGe;
      p->lft = $1, p->rht = $3;
      $$ = p;
    }
  ;

equality_expression
  : relational_expression { $$ = $1; }
  | equality_expression EQUALEQUAL relational_expression
    {
      auto p = par::gMgr.make<asg::BinaryExpr>();
      p->op = asg::BinaryExpr::Op::kEq;
      p->lft = $1, p->rht = $3;
      $$ = p;
    }
  | equality_expression EXCLAIMEQUAL relational_expression
    {
      auto p = par::gMgr.make<asg::BinaryExpr>();
      p->op = asg::BinaryExpr::Op::kNe;
      p->lft = $1, p->rht = $3;
      $$ = p;
    }
  ;

logical_and_expression
  : equality_expression { $$ = $1; }
  | logical_and_expression AMPAMP equality_expression
    {
      auto p = par::gMgr.make<asg::BinaryExpr>();
      p->op = asg::BinaryExpr::Op::kAnd;
      p->lft = $1, p->rht = $3;
      $$ = p;
    }
  ;

logical_or_expression
  : logical_and_expression { $$ = $1; }
  | logical_or_expression PIPEPIPE logical_and_expression
    {
      auto p = par::gMgr.make<asg::BinaryExpr>();
      p->op = asg::BinaryExpr::Op::kOr;
      p->lft = $1, p->rht = $3;
      $$ = p;
    }
  ;

assignment_expression
  : logical_or_expression { $$ = $1; }
  | unary_expression EQUAL assignment_expression
    {
      auto p = par::gMgr.make<asg::BinaryExpr>();
      p->op = asg::BinaryExpr::Op::kAssign;;
      p->lft = $1, p->rht = $3;
      $$ = p;
    }
  ;

expression
  : assignment_expression { $$ = $1; }
  | expression COMMA assignment_expression
    {
      auto p = par::gMgr.make<asg::BinaryExpr>();
      p->op = asg::BinaryExpr::Op::kComma;
      p->lft = $1, p->rht = $3;
      $$ = p;
    }
  ;
%%
