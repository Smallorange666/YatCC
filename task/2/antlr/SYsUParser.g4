parser grammar SYsUParser;

options {
  tokenVocab=SYsULexer;
}

primaryExpression
    :   Identifier
    |   Constant
    |   StringLiteral
    |   LeftParen expression RightParen
    ;

postfixExpression
    :   primaryExpression  
    |   postfixExpression LeftParen argumentExpressionList? RightParen
    |   postfixExpression LeftSquare expression RightSquare
    ;

argumentExpressionList
    :   assignmentExpression (Comma assignmentExpression)*
    ;

unaryExpression
    :   postfixExpression
    |   unaryOperator unaryExpression
    ;

unaryOperator
    :   Plus | Minus | Exclaim
    ;

multiplicativeExpression
    :   unaryExpression ((Star|Slash|Percent) unaryExpression)*
    ;

additiveExpression
    :   multiplicativeExpression ((Plus|Minus) multiplicativeExpression)*
    ;

relationalExpression
    :   additiveExpression ((Less|Greater|LessEqual|GreaterEqual) additiveExpression)*
    ;

equalityExpression
    :   relationalExpression ((EqualEqual|ExclaimEqual) relationalExpression)*
    ;

logicalAndExpression
    :   equalityExpression ((AmpAmp) equalityExpression)*
    ;

logicalOrExpression
    :   logicalAndExpression ((PipePipe) logicalAndExpression)*
    ;

assignmentExpression
    :   logicalOrExpression
    |   unaryExpression Equal assignmentExpression
    ;

expression
    :   assignmentExpression (Comma assignmentExpression)*
    ;

declaration
    :   declarationSpecifiers initDeclaratorList Semi
    ;

declarationSpecifiers
    :   typeSpecifier declarationSpecifiers?
    |   typeQualifier declarationSpecifiers?
    ; 

initDeclaratorList
    :   initDeclarator (Comma initDeclarator)*
    ;

initDeclarator
    :   declarator (Equal initializer)?
    ;

typeSpecifier
    :   Void
    |   Char
    |   Int
    |   Long
    ;

typeQualifier
    :   Const
    ;


declarator
    :   directDeclarator
    ;

directDeclarator
    :   Identifier
    |   LeftSquare assignmentExpression? RightSquare
    |   LeftParen parameterList? RightParen
    |   directDeclarator LeftBrace assignmentExpression? RightBrace
    ;

parameterList
    :   parameterDeclaration (Comma parameterDeclaration)*
    ;

parameterDeclaration
    :   declarationSpecifiers declarator
    ;

identifierList
    :   Identifier (Comma Identifier)*
    ;

initializer
    :   assignmentExpression
    |   LeftBrace initializerList? Comma? RightBrace
    ;

initializerList
    :   initializer (Comma initializer)*
    ;

statement
    :   compoundStatement
    |   expressionStatement
    |   ifStatement
    |   whileStatement
    |   doStatement
    |   jumpStatement
    |   breakStatement
    |   continueStatement
    ;

compoundStatement
    :   LeftBrace blockItemList? RightBrace
    ;

blockItemList
    :   blockItem+
    ;

blockItem
    :   declaration
    |   statement
    ;

expressionStatement
    :   expression? Semi
    ;

ifStatement
    :   If LeftParen expression RightParen statement (Else statement)?
    ;

whileStatement
    :   While LeftParen expression RightParen statement
    ;

doStatement
    :   Do statement While LeftParen expression RightParen Semi
    ;

jumpStatement
    :   Return expression? Semi
    ;

breakStatement
    :   Break Semi
    ;

continueStatement
    :   Continue Semi
    ;

compilationUnit
    :   translationUnit? EOF
    ;

translationUnit
    :   externalDeclaration+
    ;

externalDeclaration
    :   functionDefinition
    |   declaration
    ;

functionDefinition
    : declarationSpecifiers directDeclarator compoundStatement
    ;

