#include "lex.hpp"
#include <iostream>

void
print_token();

namespace lex {

static const char* kTokenNames[] = {
  "identifier",
  "numeric_constant",
  "string_literal",

  "auto",
  "char",
  "const",
  "double",
  "enum",
  "extern",
  "float",
  "inline",
  "int",
  "long",
  "register",
  "restrict",
  "short",
  "signed",
  "static",
  "struct",
  "typedef",
  "union",
  "unsigned",
  "void",
  "volatile",

  "break",
  "case",
  "continue",
  "default",
  "do",
  "else",
  "for",
  "goto",
  "if",
  "return",
  "switch",
  "while",

  "sizeof",

  "l_paren",
  "r_paren",
  "l_square",
  "r_square",
  "l_brace",
  "r_brace",

  "plus",
  "minus",
  "star",
  "slash",
  "percent",
  "greater",
  "less",
  "greaterequal",
  "lessequal",
  "equalequal",
  "exclaimequal",
  "ampamp",
  "pipepipe",
  "exclaim",
  "amp",
  "pipe",
  "xor",
  "not_bit",
  "shift_l",
  "shift_r",
  "inc",
  "dec",
  "arrow",
  "dot",

  "semi",
  "comma",

  "equal",
};

const char*
id2str(Id id)
{
  static char sCharBuf[2] = { 0, 0 };
  if (id == Id::YYEOF) {
    return "eof";
  } else if (id < Id::IDENTIFIER) {
    sCharBuf[0] = char(id);
    return sCharBuf;
  }
  return kTokenNames[int(id) - int(Id::IDENTIFIER)];
}

G g;

int
come(int tokenId, const char* yytext, int yyleng, int yylineno)
{
  g.mId = Id(tokenId);
  g.mText = { yytext, std::size_t(yyleng) };

  g.mColumn = g.mColumn - yyleng;
  print_token();
  g.mColumn = g.mColumn + yyleng;

  if (g.mLeadingSpace)
    g.mLeadingSpace = false;
  if (g.mStartOfLine)
    g.mStartOfLine = false;

  return tokenId;
}

} // namespace lex
