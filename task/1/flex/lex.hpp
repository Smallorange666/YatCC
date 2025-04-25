#pragma once

#include <cstring>
#include <string>
#include <string_view>

namespace lex {

enum Id
{
  YYEMPTY = -2,
  YYEOF = 0,     /* "end of file"  */
  YYerror = 256, /* error  */
  YYUNDEF = 257, /* "invalid token"  */

  IDENTIFIER,
  CONSTANT,
  STRING_LITERAL,

  AUTO,
  CHAR,
  CONST,
  DOUBLE,
  ENUM,
  EXTERN,
  FLOAT,
  INLINE,
  INT,
  LONG,
  REGISTER,
  RESTRICT,
  SHORT,
  SIGNED,
  STATIC,
  STRUCT,
  TYPEDEF,
  UNION,
  UNSIGNED,
  VOID,
  VOLATILE,

  BREAK,
  CASE,
  CONTINUE,
  DEFAULT,
  DO,
  ELSE,
  FOR,
  GOTO,
  IF,
  RETURN,
  SWITCH,
  WHILE,

  SIZEOF,

  L_PAREN,
  R_PAREN,
  L_SQUARE,
  R_SQUARE,
  L_BRACE,
  R_BRACE,

  PLUS,
  MINUS,
  STAR,
  SLASH,
  PERCENT,
  GREATER,
  LESS,
  GREATEREQUAL,
  LESSEQUAL,
  EQUALEQUAL,
  EXCLAIMEQUAL,
  AMPAMP,
  PIPEPIPE,
  EXCLAIM,
  AMP,
  PIPE,
  XOR,
  NOT_BIT,
  SHIFT_L,
  SHIFT_R,
  INC,
  DEC,
  ARROW,
  DOT,

  SEMI,
  COMMA,

  EQUAL,
};

const char*
id2str(Id id);

struct G
{
  Id mId{ YYEOF };              // 词号
  std::string_view mText;       // 对应文本
  std::string mFile;            // 文件路径
  int mLine{ 1 }, mColumn{ 1 }; // 行号、列号
  bool mStartOfLine{ true };    // 是否是行首
  bool mLeadingSpace{ false };  // 是否有前导空格
};

extern G g;

int
come(int tokenId, const char* yytext, int yyleng, int yylineno);

} // namespace lex
