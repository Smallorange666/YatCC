# 使用 Bison 完成 Task2

## 任务介绍

同学们需要完成下面两个部分的内容：

1. 词法分析：若选择复活版本，task2 的输入为 task1 的标准输出，因此同学们需要参考 `build/test/task1/*/*/answer.txt` **补充`lex.cpp`文件的`kTokenId`数据结构**，将语法分析的输入与 task1 的 clang 标准输出进行匹配；
2. 语法分析：类型检查和 ASG 生成 json 文件的两部分代码已经提供基本的实现。同学们需要认真阅读`common/asg.hpp`文件，了解每个非终结符对应类型的结构和操作，结合文档示例中给出的参考文法，**补充`par.y`文件中的缺少的文法和语义动作**。

## 相关知识

### Bison 简介

打一个比喻，让大家对 Bison 的功能有基本了解：

假如同学们在学习一种新的语言，这时候我们需要一个“词典”来翻译听到或者看到的这门语言的单词。同样，在计算机领域，编程语言也需要这样一个“词典”，我们称之为“解析器”，它用来读懂程序代码。那么 Bison 就像是一个专门用来制作这种“词典”的工厂。你需要告诉 Bison 你的编程语言的规则，比如这门语言是怎样建立的，有哪些单词，单词之间又是怎样组合的等等，然后 Bison 就会根据你提供的这些规则，为你制作出一个符合这些规则的“词典”或者说“解析器”。

这个“解析器”可以作为一个针对特定编程语言的阅读工具，帮助计算机更好地理解这门语言的程序代码。比如说，当计算机通过这个“解析器”读到一个程序代码时，它可以告诉计算机这个代码的意思是让计算机执行什么样的操作。通过上述的比喻，相信大家可以理解，Bison 是一个语法分析器的生成器。而 flex 和 Bison 常常进行配合使用，从而共同完成词法分析和语法分析。

大致可以将其处理流程理解如下（更为准确地在**Bison 实现原理**部分介绍）：

- 输入一个文件，flex 可以对该文件进行正则表达式的匹配，从而生成一系列的 token 流（这个大家在实验一中已经很清楚了）。

- lex 生成每一个 token 之后，将其传给 Bison 进行处理：Bison 会对当前传入的 token 进行语法分析，即文法的匹配，并进行相应移进归约操作，从而完成语法分析。

- 同时，我们可以在 Bison 进行移进归约操作的时候，进行自定义语义动作，从而可以完成语法分析。

Bison 的使用方式很简单，给出下列重要的知识总结：

#### Flex 和 Bison 的使用范式

Flex 和 Bison 生成的代码分别处于两个 C 源代码文件，它们各自单独编译，然后通过外部链接机制最终链接为一个整体。Flex 和 Bison 的代码文件在整体结构上都是被两个%%分成了三个部分：前言、主体、后记。

Flex 和 Bison 默认用法的场景是传统的命令行指令式程序，生成使用全局变量的不可重入代码，并且 Flex 固定地从`<stdio.h>`输入输出数据。两者的关系以 Bison 为主，Flex 只是辅助的可选项：Bison 从代码文件生成一个`int yyparse()`函数，其内部调用两个需要我们补充定义的函数`int yylex()`、`void yyerror(const char *)`来读取词法单元流和报告错误，Flex 就是用于生成那个`yylex`函数。

在联合使用时，我们应该**首先编写 Bison 语法定义（`.y`），通过前言区的`%token`定义有哪几种词法单元，然后在 Flex 代码中包含生成的头文件，再编写词法单元的解析规则**，这和我们实验 1 到实验 2 的顺序是相反的。知道这些之后，我们就得到了基本的文件骨架：

- `parser.y`

  ```cpp
  %code requires {
  int yylex();
  void yyerror(const char *);
  }
  %%
  %%
  ```

  为了让 Bison 生成的代码能够通过编译环节，必须在其中加入`yylex`和`yyerror`的声明。

- `lexer.l`

  ```cpp
  %{
  #include "parser.tab.h"
  %}
  %%
  %%
  ```

  其中头文件名`"parser.tab.h"`是 Bison 的默认名字，你应该填你实际指定的文件名。这样生成的词法解析器代码默认会调用一个外部定义函数`yywrap`，如果你没定义就会导致链接通不过，对于本实验而言这个函数是没用的，因此实验 1 中的模板代码在前言区加入了一行`%option noyywrap`。

这里是一个最小的具体例子，用于解析正负数字：

```cpp
// parser.y
%code top {
int yylex (void);
void yyerror (char const *);
}

%token NUMBER
%token ADD
%token SUB

%%
start: NUMBER | ADD NUMBER | SUB NUMBER;
%%

// lexer.l
%{
#include "parser.tab.h"
%}

%option noyywrap

%%
[0-9]+    { return NUMBER; }
"+"        { return ADD; }
"-"        { return SUB; }
%%
```

联合使用就是上述的流程，如果同学们还是有点云里雾里。同学们可以找一些知乎 CSDN 等文章，下面这一篇是我找的，同学们可以上网搜更多的去看看：https://zhuanlan.zhihu.com/p/111445997 。 如果还是不懂， 欢迎咨询助教。

#### 使用 %union 和 $n 定义和访问语义值

同学们已经在理论课上了解到，每个语法解析树的结点都会和一些“属性”关联起来，不同结点有哪些属性一般都是不一样的，反映到代码里就是不同非终结符和终结符的语义值类型是不一样的，比如一个整数字面量可能对应一个`int`，而一个字符串字面量可能对应一个`char*`，所以总的而言文法符号的语义类型是这些类型的“或”，也就是一个联合类型，对应 C 中的联合体`union`。但是，使用联合体是十分容易出错的，Bison 考虑到了这一点，所以它提供了`%union`和`$n`机制代替我们直接编写和操作联合体。

在前言区，使用`%union`定义所有可能的语义值类型，然后在`%nterm`和`%token`中将文法符号和类型关联起来：

```cpp
%union {
  int num;
  char* str;
}

%nterm <str> start
%token <num> NUMBER
%token <str> STRING
```

然后在主体部分直接使用`$n`就可以操作文法符号对应的语义值：

```cpp
start: NUMBER STRING { $$ = $2 + $1; } ;
```

**其中`$$`、`$1`、`$2`会被 Bison 自动拓展为类似于`start.str`、`NUMBER.num`、`STRING.str`的联合体成员引用**，并且 Bison 会帮我们检查类型的使用是否正确。语义值最终的来源是词法解析器，在`yylex`函数（flex 主体部分）中，使用全局变量`yylval`填入词法单元的语义值，比如：

```cpp
[0-9]+ {
  yylval.num = atol(yytext);
  return NUMBER;
}

[a-zA-Z]+ {
  char* str = malloc(strlen(yytext));
  strcpy(str, yytext);
  yylval.str = str;
  return STRING;
}
```

#### Bison 实现原理（选读）

首先要明白的一点是，Flex 和 Bison 的代码不是 C 和 C++源代码，严格地说它们是专用于生成词法解析器和语法解析器的领域特定语言（DSL）。Bison 不是语法分析器，只是用于生成语法分析器的一种语言。使用 Bison 定义了语义分析规则之后，其会生成`y.tab.h`, `y.tab.c`, `y.output`等文件，将这些文件与 flex 生成的文件`lex.yy.c`一起进行编译运行，最后可以得到一个可执行文件，而这个文件才是用作输入文本的语法分析器所用。

上述中间过程，我们都为同学们进行省略和遮盖，同学们以后如果需要自己用的话，这些中间过程必不可少。

Bison 官方文档（不建议看）

https://www.gnu.org/software/Bison/manual/ 这是 Bison 的官方文档，不建议看，在熟悉 Bison 以后用于查找一些具体用法比较好。

#### Bison 总结

其实，在本实验中，**需要理解 Bison 的使用就是两个重要部分：文法书写和语义动作定义**。

```cpp
start : translation_unit ; //这是文法的定义，表示start终结符可以推导为translation_unit终结符
```

在文法后面加入 `{}`，可以定义语义动作，语义动作是用Ｃ语言代码进行撰写，表示文法在进行规约的时候应该执行的代码。

```cpp
start
    : translation_unit //在最后进行加入，代表在规约的时候将执行{}中的代码。
        {
        par::gTranslationUnit.reset($1);
        }　
    ;
```

## 任务说明

---

总体思路(main.cpp)

以 main.cpp 为入口，实验 2 首先进行语法分析：`yyparse`（在其中进行填充 ASG 的结构），然后进行类型检查 `typing(*par::gTranslationUnit)`，最后将 asg 生成 json 文件 `asg2json`，并且写入指定文件。

在语法分析中，Bison 的 `yyparse`中有下面的逻辑：

- 由于实验 2 以复活版本的进行实验，因此输入的是 task1-answer，同学们可以看下其中的一个文件：`/YatCC/build/test/task1/functional-0/000_main.sysu.c/answer.txt`，如下图所示

![task1-answer](../images/bison/task1-answer.png)

- Bison 会读取词法分析`lex`中的传入的`token`（`lex`每读取一个，就会传给 Bison 进行语法分析），因此将上述文件输入到实验 2 中，此时词法分析`lex`相关部分代码比起实验一会发生变化，不过这一部分的代码目前已经写好了，同学们可以自行查看。
  （其逻辑是：相比于实验一的输入直接是源文件从而进行相关的各个`token`的匹配，实验二复活版本将匹配上述输入文件的每一行，然后对每一行进行处理，提取出每行的第一个单词（`tokenId`）和每行的第二个单词中的引号内容（`tokenValue`）。例如，以一行为例，识别出的`token`：其`tokenId`为`int`，其`tokenValue`为引号内的内容，也为`int`。）
- Bison 拿到该`token`后，首先进行文法的匹配，进行移进归约操作，而后在每个移进归约的过程中完成用户自定义的语义动作，在本实验中，我们是生成并填充 ASG 结构。

在类型检查中，`typing`则将对生成的 ASG 中的每一个结构进行类型检查，如果不通过该类型检查，程序就会停止。同学可以利用这个方便地进行查错，判断自己到底是哪个类型没有写对。在 ASG 生成 json 文件中，`asg2json`将在 yyparse 中生成并通过类型检查的 ASG 结构进行输出并打印。而类型检查和 ASG 生成 json 文件的部分，已经进行了基本的实现，同学们只要负责**语法分析中的文法撰写和语义动作撰写**即可，即`par.y`文件的补充。由于 task1 的标准答案是复活部分的代码输入而非task0的预处理信息，因此同学们需要补充`lex.cpp`文件的`kTokenId`数据结构，使 clang 标准输出跟语法分析的输入进行匹配。

## 文件结构

对实验整体的整体过程有了把握，我们接下来看下这部分实验文件的整体结构。

```bash
-- Bison
   |-- lex.cpp
   |-- lex.hpp
   |-- lex.l
   |-- main.cpp
   |-- par.cpp
   |-- par.hpp
   |-- par.y
-- common
   |-- Asg2Json.cpp
   |-- Asg2Json.hpp
   |-- Typing.cpp
   |-- Typing.hpp
   |-- asg.hpp
   |-- asg.cpp
   |-- Obj.hpp
   |-- Obj.cpp
```

其中 `common`是共有文件，包含了：ASG 结构定义文件 `asg.hpp`和 `asg.cpp`（基类函数定义 `obj.hpp`和 `obj.cpp`），类型检查需要用到的文件:`Typing.hpp`和 `Typing.cpp`，ASG 转换为 Json 的文件：`Asg2Json.hpp`和 `Asg2Json.cpp`。其具体解析见公共代码介绍。

其中 Bison 是本实验的部分，包含了：

- `main.cpp` 程序入口，详细解析见上述总体思路。
- 词法分析相关文件：复活版本（即从标准答案 task1-answer 中进行读取），包含了：`lex.l`，`lex.hpp`和 `lex.cpp`文件。其中 `lex.l`是用于写词法分析的规则部分，`.hpp`和 `.cpp`文件是用于定义相关辅助函数的文件。
- 语法分析相关文件：包含了：`par.y`，`par.hpp`和 `par.cpp`文件。其中其中 `par.y`是用于写语法分析相关的文法以及语义动作，`.hpp`和 `.cpp`文件是用于定义相关辅助函数的文件。

## 代码说明和解析

这个部分将会对重点代码进行说明和解析，同时，将会以一个文法为小例子进行讲解，方便同学们理解。

---

例如，语句的其中一种文法表示如下：

```bash
statement
    ： compound_statement
    | expression_statement（表达式语句）
    | selection_statement（选择语句）
    | iteration_statement（迭代语句）
    | jump_statement ; （跳转语句）
```

其中选择语句的其中一种文法表示如下：

```bash
selection_statement
    ： IF '(' expression ')' statement
    | IF '(' expression ')' statement ELSE statement;
```

1. 文法撰写

   在`par.y`文件中添加的代码应该如下表示：

   ```cpp
   // 只是部分文法，并不是全面的
   statement
   : compound_statement
   | expression_statement
   | jump_statement
   ;
   jump_statement
   : RETURN ';'
   | RETURN expression ';'
   ;
   ```

2. 语义动作撰写
   从语法分析树直接转化到 JSON 的输出是十分困难的，我们需要进行一些封装和转换，从而可以简便地通过 Bison 的语义动作，进行填充定义的 ASG 结构，从而为之后的 json 转化做铺垫。

   由刚刚的例子，我们可以在`asg.hpp`中语句找到对应的结构如下所示（对于一些结构体的含义不太清楚的，可以通过`asg2json.cpp`中的每个结构的打印方式即可知道该结构体对应的是什么，建议同学们都看看`asg2json.cpp`这样对结构体的含义更为清晰，也以免出错。）

   ```cpp
   namespace asg {

   //==============================================================================
   // 类型
   //==============================================================================
   struct TypeExpr;
   struct Expr;
   struct Decl;
   struct Type : Obj；   /* Type用于表示节点的类型信息，包括基本类型和复合类型 */
   struct TypeExpr : Obj；   /* 表示更复杂的类型表达，如数组和函数类型 */
   struct PointerType : TypeExpr；
   struct ArrayType : TypeExpr；
   struct FunctionType : TypeExpr；

   //==============================================================================
   // 表达式
   //==============================================================================
   struct Decl;
   struct Expr : Obj；    /* Expr表示所有表达式节点的基类，比如字面量、二元运算、函数调用等 */
   struct IntegerLiteral : Expr；
   struct StringLiteral : Expr；
   struct DeclRefExpr : Expr； //表示对声明的引用
   struct ParenExpr : Expr；  //表示带括号的表达式
   struct UnaryExpr : Expr；  //一元表达式
   struct BinaryExpr : Expr；  /* 表示二元表达式 */
   struct CallExpr : Expr；   /* 表示函数调用的语法树节点 */
   struct InitListExpr : Expr；  /* 初始化列表（如数组或结构体初始化） */
   struct ImplicitInitExpr : Expr；  /* 被用来表示某种隐式初始化的表达式 */
   struct ImplicitCastExpr : Expr；   //表示隐式类型转换表达式

   //==============================================================================
   // 语句
   //==============================================================================
   struct FunctionDecl;
   struct Stmt : Obj  /* 所有语句的基类，例如表达式语句和符合语句 */
   struct NullStmt : Stmt
   struct DeclStmt : Stmt
   struct ExprStmt : Stmt
   struct CompoundStmt : Stmt
   struct IfStmt : Stmt
   struct WhileStmt : Stmt
   struct DoStmt : Stmt
   struct BreakStmt : Stmt
   struct ContinueStmt : Stmt
   struct ReturnStmt : Stmt

   //==============================================================================
   // 声明
   //==============================================================================
   struct Decl : Obj  /* 表示所有声明的基类，例如变量声明和函数声明 */
   struct VarDecl : Decl  /* 变量variable声明 */
   struct FunctionDecl : Decl  /* 函数声明 */

   //==============================================================================
   // 顶层
   //==============================================================================
   struct TranslationUnit : Obj   /* 代表整个程序或一个编译单元，是ASG的根节点 */

   } // namespace asg

   ```

   接下来对刚刚文法生成相应的 ASG 结构进行填充，需要在 Bison 的文法代码中加上对应的语义动作(即匹配到了相应的文法之后，所进行的代码动作)，如下{ }中的内容:

   ```cpp
   statement // Stmt
   : compound_statement
     {
     $$ = $1;
     }
   | expression_statement
     {
     $$ = $1;
     }
   | jump_statement
     {
     $$ = $1;
     }
   ;
   jump_statement // ReturnStmt
   : RETURN ';'
     {
     auto p = &par::gMgr.make<asg::ReturnStmt>();
     $$ = p;
     }
   | RETURN expression ';'
     {
     auto p = &par::gMgr.make<asg::ReturnStmt>();
     p->expr = $2;
     $$ = p;
     }
   ;
   ```

   解释`statement`的语义动作：

   - `$$ = $1;` 直接将`statement`的值令为文法左边第一个符号的值。

   解释`jump_statement`的语义动作：

   - 使用`&par::gMgr.make<asg::ReturnStmt>();`构造`ReturnStmt`结构体。具体可以看`make`的代码，只要是直接或间接继承于`Obj`类型的都可以用这个构造器进行构造。然后填充该结构体，根据 asg 的定义，需要填充`func`、`expr`结构，在这里只能填充`expr`结构。
   - 显然`$2`（文法右边第二个表示为`expression`，用`$`进行取出）为该语句的`expression`，因此有`p->expr = $2;`，其中文法右边第三个‘ ; ’表示为$3。
   - `$$ = p;` `$$`为文法右边的值即`jump_statement`的值，则此动作为将该`jump_statement`的值设置为`ReturnStmt`结构的 P 值。

3. 在`par.y`相对应的地方进行类型的定义

   在使用一个非终结符或终结符时，需要在`par.y`相对应的地方进行类型的定义，以`compound_statement`为例。

   ```cpp
   %union {
   asg::CompoundStmt* CompoundStmt;  // 首先进行类型的命名
   }

   %type <CompoundStmt> compound_statement // 然后即可再此使用
   ```

   其实最后所有文法的语义动作总的生成的就是一个`TranslationUnit`的结构体，其中包含了所有的 ASG 的信息，可以查看下面代码。最后`asg2json`就是抓住这个节点以其为根，然后进行遍历其中进行打印出相应所有的节点。

   ```cpp
   start
   : translation_unit
    {
    par::gTranslationUnit.reset($1);
    }
   ;
   ```

4. 其余代码解释

   ```cpp
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
   ```

   其文法定义为一句，但有两个语义动作的执行：

   - 在匹配到`declartion_specifiers declarator`的时候就要做后面的语义动作（即，想要他执行后面的代码），再匹配到最后的`compund_statement`的时候想要它再做后面的语义动作。
   - `$4`是因为分割之后，第一个大括号也被 Bison 识别为了一块语义，将其也计数了，但是这种分割这个时候不建议使用`$4`的数字，因为有时候 Bison 会识别不出来，建议使用`$`+名字，即类似`$compund_statement`。

## 如何 debug

### yydebug

yyparse 部分出现问题，即 Bison 的文法规约等出现问题，直接在 main.cpp 中加入 yydebug 为 1 的代码(如下图)，即可打印出详细的 Bison 文法移进规约栈的信息，从而进行定位。
![alt text](../images/bison/yydebug.png)

需要提醒的是，这部分是不适合使用断点进行调试的，因为其会跳到 Bison 生成的代码进行状态的不断跳转，根本不知道文法到底归约到哪里了。文法的移进规约直接使用 yydebug，而其语义动作的定位需要再配合`std::cout`打印即可。

### 断点调试

在`Typing`和`Asg2Json`部分使用断点调试，看下是哪一部分生成不到位出了问题。配合`std::cout`打印更佳。

### 输出到文件

有时候编译很顺畅的进行通过的时候，`std::cout`是打印不出的，因此这个时候可以将`std::cout`写入到文件里面。例如可以定义以下函数，然后使用该函数就可以打印到指定文件，即可进行 debug。

```cpp
void printToTxtFile(std::string message) {
    std::ofstream myfile;
    myfile.open ("/YatCC/task/2/Bison/log.txt", std::ios_base::app); // 'app' means appending to the end of the file, trunc: start of the file
    myfile << message << "\n";
    myfile.close();
}
```

## 可能会遇到的坑点

- **指针问题**
  取 type 的时候，其指针可能是空的，如果这个时候再取其 texp 对象，就会终止，也不会有报错信息，最好判断一下是不是空指针再去取。比如，如下图所示。

  ![alt text](../images/bison/point.png)

- **更改 ASG 的`Type`类型**
  更改 ASG 的`Type`类型，只能改变指针指向，不能直接去赋值。比如，如下图所示。新建一个`ty`的`Type`对象，更改`ty`，然后改变`$2`的`type`指针的指向为更改后的`ty`。如果直接进行`$2->type->spec=...`是不运行的，因为 ASG 结构体的`Type`为`const Type *`类型。

  ![alt text](../images/bison/type.png)

## 其他说明

实验二的 BreakStmt 中的 loop 属性，这个属性不用处理不用管，本实验不会用到（实验三也不会用到）

![alt text](../images/bison/loop.png)
