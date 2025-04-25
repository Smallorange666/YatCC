#include "lex.hpp"
#include <cstring>
#include <iostream>
#include <unordered_map>

namespace lex {

G g;

int
come_line(const char* yytext, int yyleng, int yylineno)
{
  char name[64];
  char value[64];
  sscanf(yytext, "%s '%[^']'", name, value);

  static const std::unordered_map<std::string, int> kTokenId = {
    { "identifier", IDENTIFIER },
    { "numeric_constant", CONSTANT },
    { "string_literal", STRING_LITERAL },

    { "auto", AUTO },
    { "char", CHAR },
    { "const", CONST },
    { "double", DOUBLE },
    { "enum", ENUM },
    { "extern", EXTERN },
    { "float", FLOAT },
    { "inline", INLINE },
    { "int", INT },
    { "long", LONG },
    { "register", REGISTER },
    { "restrict", RESTRICT },
    { "short", SHORT },
    { "signed", SIGNED },
    { "static", STATIC },
    { "struct", STRUCT },
    { "typedef", TYPEDEF },
    { "union", UNION },
    { "unsigned", UNSIGNED },
    { "void", VOID },
    { "volatile", VOLATILE },

    { "break", BREAK },
    { "case", CASE },
    { "continue", CONTINUE },
    { "default", DEFAULT },
    { "do", DO },
    { "else", ELSE },
    { "for", FOR },
    { "goto", GOTO },
    { "if", IF },
    { "return", RETURN },
    { "switch", SWITCH },
    { "while", WHILE },

    { "sizeof", SIZEOF },

    { "l_paren", L_PAREN },
    { "r_paren", R_PAREN },
    { "l_square", L_SQUARE },
    { "r_square", R_SQUARE },
    { "l_brace", L_BRACE },
    { "r_brace", R_BRACE },

    { "plus", PLUS },
    { "minus", MINUS },
    { "star", STAR },
    { "slash", SLASH },
    { "percent", PERCENT },
    { "greater", GREATER },
    { "less", LESS },
    { "greaterequal", GREATEREQUAL },
    { "lessequal", LESSEQUAL },
    { "equalequal", EQUALEQUAL },
    { "exclaimequal", EXCLAIMEQUAL },
    { "ampamp", AMPAMP },
    { "pipepipe", PIPEPIPE },
    { "exclaim", EXCLAIM },
    { "amp", AMP },

    { "semi", SEMI },
    { "comma", COMMA },

    { "equal", EQUAL },

    { "eof", EOF },
  };

  auto iter = kTokenId.find(name);
  assert(iter != kTokenId.end());

  yylval.RawStr = new std::string(value, strlen(value));
  return iter->second;
}

int
come(int tokenId, const char* yytext, int yyleng, int yylineno)
{
  g.mId = tokenId;
  g.mText = { yytext, std::size_t(yyleng) };
  g.mLine = yylineno;

  g.mStartOfLine = false;
  g.mLeadingSpace = false;

  return tokenId;
}

} // namespace lex
