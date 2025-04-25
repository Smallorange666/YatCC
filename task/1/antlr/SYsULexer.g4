lexer grammar SYsULexer;

Auto : 'auto';
Char : 'char';
Const : 'const';
Double : 'double';
Enum : 'enum';
Extern : 'extern';
Float : 'float';
Inline : 'inline';
Int : 'int';
Long : 'long';
Register : 'register';
Restrict : 'restrict';
Short : 'short';
Signed : 'signed';
Static : 'static';
Struct : 'struct';
Typedef : 'typedef';
Union : 'union';
Unsigned : 'unsigned';
Void : 'void';
Volatile : 'volatile';

Break : 'break';
Case : 'case';
Continue : 'continue';
Default : 'default';
Do : 'do';
Else : 'else';
For : 'for';
Goto : 'goto';
If : 'if';
Return : 'return';
Switch : 'switch';
While : 'while';

Sizeof : 'sizeof';

Alignas : '_Alignas';
Alignof : '_Alignof';
Atomic : '_Atomic';
Bool : '_Bool';
Complex : '_Complex';
Generic : '_Generic';
Imaginary : '_Imaginary';
Noreturn : '_Noreturn';
StaticAssert : '_Static_assert';
ThreadLocal : '_Thread_local';


LeftParen : '(';
RightParen : ')';
LeftBracket : '[';
RightBracket : ']';
LeftBrace : '{';
RightBrace : '}';

Plus : '+';
Minus : '-';
Star : '*';
Slash : '/';
Percent : '%';
Greater : '>';
Less : '<';
GreaterEqual : '>=';
LessEqual : '<=';
EqualEqual : '==';
ExclaimEqual : '!=';
AmpAmp : '&&';
PipePipe : '||';
Exclaim : '!';
Amp : '&';
Pipe : '|';
Caret : '^';
Tilde : '~';
Equal : '=';


Semi : ';';
Comma : ',';



Identifier
    :   IdentifierNondigit
        (   IdentifierNondigit
        |   Digit
        )*
    ;

fragment
IdentifierNondigit
    :   Nondigit
    ;

fragment
Nondigit
    :   [a-zA-Z_]
    ;

fragment
Digit
    :   [0-9]
    ;

Constant
    :   IntegerConstant
    ;

fragment
IntegerConstant
    :   DecimalConstant
    |   OctalConstant
    |   HexConstant
    ;

fragment
DecimalConstant
    :   NonzeroDigit Digit*
    ;

fragment
OctalConstant
    :   '0' OctalDigit*
    ;

fragment
HexConstant
    :   '0x' HexDigit+
    ;

fragment
NonzeroDigit
    :   [1-9]
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
HexDigit
    :   [0-9a-fA-F]
    ;


// 预处理信息处理，可以从预处理信息中获得文件名以及行号
// 预处理信息中的第一个数字即为行号
LineAfterPreprocessing
    :   '#' Whitespace* ~[\r\n]*
    ;

Whitespace
    :   [ \t]+
    ;

// 换行符号，可以利用这个信息来更新行号
Newline
    :   (   '\r' '\n'?
        |   '\n'
        )
    ;

