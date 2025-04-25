%{
#include "lex.hpp"
/* 所有代码全部抽离出来，放到 lex.hpp 和 lex.cpp 里 */

using namespace lex;

#define ADDCOL() g.mColumn += yyleng;
#define COME(id) return come(id, yytext, yyleng, yylineno)
%}

%option 8bit warn noyywrap yylineno

D     [0-9]
HEXD  [0-9a-fA-F]
L     [a-zA-Z_]
IS    ((u|U)|(u|U)?(l|L|ll|LL)|(l|L|ll|LL)(u|U))

%%
"auto"      { ADDCOL(); COME(AUTO); }
"char"      { ADDCOL(); COME(CHAR); }
"const"     { ADDCOL(); COME(CONST); }
"double"    { ADDCOL(); COME(DOUBLE); }
"enum"      { ADDCOL(); COME(ENUM); }
"extern"    { ADDCOL(); COME(EXTERN); }
"float"     { ADDCOL(); COME(FLOAT); }
"inline"    { ADDCOL(); COME(INLINE); }
"int"       { ADDCOL(); COME(INT); }
"long"      { ADDCOL(); COME(LONG); }
"register"  { ADDCOL(); COME(REGISTER); }
"restrict"  { ADDCOL(); COME(RESTRICT); }
"short"     { ADDCOL(); COME(SHORT); }
"signed"    { ADDCOL(); COME(SIGNED); }
"static"    { ADDCOL(); COME(STATIC); }
"struct"    { ADDCOL(); COME(STRUCT); }
"typedef"   { ADDCOL(); COME(TYPEDEF); }
"union"     { ADDCOL(); COME(UNION); }
"unsigned"  { ADDCOL(); COME(UNSIGNED); }
"void"      { ADDCOL(); COME(VOID); }
"volatile"  { ADDCOL(); COME(VOLATILE); }


"break"     { ADDCOL(); COME(BREAK); }
"case"      { ADDCOL(); COME(CASE); }
"continue"  { ADDCOL(); COME(CONTINUE); }
"default"   { ADDCOL(); COME(DEFAULT); }
"do"        { ADDCOL(); COME(DO); }
"else"      { ADDCOL(); COME(ELSE); }
"for"       { ADDCOL(); COME(FOR); }
"goto"      { ADDCOL(); COME(GOTO); }
"if"        { ADDCOL(); COME(IF); }
"return"    { ADDCOL(); COME(RETURN); }
"switch"    { ADDCOL(); COME(SWITCH); }
"while"     { ADDCOL(); COME(WHILE); }


"sizeof"         { ADDCOL(); COME(SIZEOF); }


"("         { ADDCOL(); COME(L_PAREN); }
")"         { ADDCOL(); COME(R_PAREN); }
"["         { ADDCOL(); COME(L_SQUARE); }
"]"         { ADDCOL(); COME(R_SQUARE); }
"{"         { ADDCOL(); COME(L_BRACE); }
"}"         { ADDCOL(); COME(R_BRACE); }


"+"         { ADDCOL(); COME(PLUS); }
"-"         { ADDCOL(); COME(MINUS); }
"*"         { ADDCOL(); COME(STAR); }
"/"         { ADDCOL(); COME(SLASH); }
"%"         { ADDCOL(); COME(PERCENT); }
">"         { ADDCOL(); COME(GREATER); }
"<"         { ADDCOL(); COME(LESS); }
">="        { ADDCOL(); COME(GREATEREQUAL); }
"<="        { ADDCOL(); COME(LESSEQUAL); }
"=="        { ADDCOL(); COME(EQUALEQUAL); }
"!="        { ADDCOL(); COME(EXCLAIMEQUAL); }
"&&"        { ADDCOL(); COME(AMPAMP); }
"||"        { ADDCOL(); COME(PIPEPIPE); }
"!"         { ADDCOL(); COME(EXCLAIM); }
"&"         { ADDCOL(); COME(AMP); }


";"         { ADDCOL(); COME(SEMI); }
","         { ADDCOL(); COME(COMMA); }


"="         { ADDCOL(); COME(EQUAL); }


{L}({L}|{D})*         { ADDCOL(); COME(IDENTIFIER); }

L?\"(\\.|[^\\"\n])*\" { ADDCOL(); COME(STRING_LITERAL); }

0x{HEXD}{HEXD}*{IS}?   { ADDCOL(); COME(CONSTANT); }
0[0-7]*{IS}?          { ADDCOL(); COME(CONSTANT); }
[1-9]{D}*{IS}?        { ADDCOL(); COME(CONSTANT); }

^#[^\n]*  { 
    if(strchr(yytext, '<')==NULL)
    {
        char* fileNameStart=strchr(yytext, '"')+1;
        char* fileNameEnd=strrchr(yytext, '"');
        int fileNameLength=fileNameEnd-fileNameStart;
        char* tem=new char[fileNameLength+1];
        strncpy(tem, fileNameStart, fileNameLength);
        g.mFile=tem;
        delete[] tem;
    }

    char* lineNumStart=yytext+2;
    int lineNum=0;
    while((*lineNumStart)!=' ')
    {
        lineNum=lineNum*10+(*lineNumStart)-'0';
        lineNumStart++;
    }
    g.mLine=lineNum-1;

    return ~YYEOF; 
    
} /* 预处理信息处理，可以从预处理信息中获得文件名以及行号 */



[ \t\v\n\f]  { 
    if(yytext[0]=='\n' || yytext[0]=='\f') 
    {
        g.mLine++;
        g.mColumn=1;
        g.mStartOfLine=true; 
    }
    else
    {
        if(yytext[0]==' ')
            g.mLeadingSpace=true;
        
        ADDCOL();
    }  
    return ~YYEOF; } 

<<EOF>>     { COME(YYEOF); }

%%

/* about symbols avaliable (yytext, yyleng etc.) in the context of Flex:
 * https://ftp.gnu.org/old-gnu/Manuals/flex-2.5.4/html_node/flex_14.html
 * https://ftp.gnu.org/old-gnu/Manuals/flex-2.5.4/html_node/flex_15.html
*/
