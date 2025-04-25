#include "SYsULexer.h" // 确保这里的头文件名与您生成的词法分析器匹配
#include <fstream>
#include <iostream>
#include <unordered_map>

std::string nowFileName = "";
int nowLineNum = 1;
int nowColumnNum = 1;
bool startOfLine = true;
bool leadingSpace = false;

// 映射定义，将ANTLR的tokenTypeName映射到clang的格式
std::unordered_map<std::string, std::string> tokenTypeMapping = {
  { "Identifier", "identifier" },
  { "Constant", "numeric_constant" },
  { "EOF", "eof" },

  { "Auto", "auto" },
  { "Char", "char" },
  { "Const", "const" },
  { "Double", "double" },
  { "Enum", "enum" },
  { "Extern", "extern" },
  { "Float", "float" },
  { "Inline", "inline" },
  { "Int", "int" },
  { "Long", "long" },
  { "Register", "register" },
  { "Restrict", "restrict" },
  { "Short", "short" },
  { "Signed", "signed" },
  { "Static", "static" },
  { "Struct", "struct" },
  { "Typedef", "typedef" },
  { "Union", "union" },
  { "Unsigned", "unsigned" },
  { "Void", "void" },
  { "Volatile", "volatile" },

  { "Break", "break" },
  { "Case", "case" },
  { "Continue", "continue" },
  { "Default", "default" },
  { "Do", "do" },
  { "Else", "else" },
  { "For", "for" },
  { "Goto", "goto" },
  { "If", "if" },
  { "Return", "return" },
  { "Switch", "switch" },
  { "While", "while" },

  { "Sizeof", "sizeof" },

  { "Alignas", "_Alignas" },
  { "Alignof", "_Alignof" },
  { "Atomic", "_Atomic" },
  { "Bool", "_Bool" },
  { "Complex", "_Complex" },
  { "Generic", "_Generic" },
  { "Imaginary", "_Imaginary" },
  { "Noreturn", "_Noreturn" },
  { "StaticAssert", "_Static_assert" },
  { "ThreadLocal", "_Thread_local" },

  { "LeftParen", "l_paren" },
  { "RightParen", "r_paren" },
  { "LeftBracket", "l_square" },
  { "RightBracket", "r_square" },
  { "LeftBrace", "l_brace" },
  { "RightBrace", "r_brace" },

  { "Plus", "plus" },
  { "Minus", "minus" },
  { "Star", "star" },
  { "Slash", "slash" },
  { "Percent", "percent" },
  { "Greater", "greater" },
  { "Less", "less" },
  { "GreaterEqual", "greaterequal" },
  { "LessEqual", "lessequal" },
  { "EqualEqual", "equalequal" },
  { "ExclaimEqual", "exclaimequal" },
  { "AmpAmp", "ampamp" },
  { "PipePipe", "pipepipe" },
  { "Exclaim", "exclaim" },
  { "Amp", "amp" },
  { "Pipe", "pipe" },
  { "Caret", "caret" },
  { "Tilde", "tilde" },
  { "Equal", "equal" },
  { "Semi", "semi" },
  { "Comma", "comma" },

  { "Whitespace", "whitespace" },
  { "LineAfterPreprocessing", "preprocess" },
  { "Newline", "newline" }
};

void
print_token(const antlr4::Token* token,
            const antlr4::CommonTokenStream& tokens,
            std::ofstream& outFile,
            const antlr4::Lexer& lexer)
{
  auto& vocabulary = lexer.getVocabulary();

  auto tokenTypeName =
    std::string(vocabulary.getSymbolicName(token->getType()));

  if (tokenTypeName.empty())
    tokenTypeName = "<UNKNOWN>"; // 处理可能的空字符串情况

  if (tokenTypeMapping.find(tokenTypeName) != tokenTypeMapping.end()) {
    tokenTypeName = tokenTypeMapping[tokenTypeName];
  }

  std::string text = token->getText();
  if (tokenTypeName == "preprocess") {
    int lineNumStart = 2;
    int tem = 0;
    while (text[lineNumStart] != ' ') {
      tem = tem * 10 + (text[lineNumStart] - '0');
      lineNumStart++;
    }
    nowLineNum = tem - 1;

    int fileNameStart = text.find('"') + 1;
    int fileNameEnd = text.rfind('"');
    nowFileName = text.substr(fileNameStart, fileNameEnd - fileNameStart);

    return;
  } else if (tokenTypeName == "newline") {
    nowLineNum++;
    startOfLine = true;
    return;
  } else if (tokenTypeName == "whitespace") {
    leadingSpace = true;
    return;
  }

  nowColumnNum = token->getCharPositionInLine() + 1;

  if (token->getText() != "<EOF>")
    outFile << tokenTypeName << " '" << token->getText() << "'";
  else
    outFile << tokenTypeName << " '"
            << "'";

  if (startOfLine)
    outFile << "\t [StartOfLine]";
  if (leadingSpace)
    outFile << " [LeadingSpace]";
  outFile << "\t" << "Loc=<" << nowFileName << ":" << nowLineNum << ":"
          << nowColumnNum << ">" << std::endl;

  startOfLine = false;
  leadingSpace = false;
}

int
main(int argc, char* argv[])
{
  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <input> <output>\n";
    return -1;
  }

  std::ifstream inFile(argv[1]);
  if (!inFile) {
    std::cout << "Error: unable to open input file: " << argv[1] << '\n';
    return -2;
  }

  std::ofstream outFile(argv[2]);
  if (!outFile) {
    std::cout << "Error: unable to open output file: " << argv[2] << '\n';
    return -3;
  }

  std::cout << "程序 '" << argv[0] << std::endl;
  std::cout << "输入 '" << argv[1] << std::endl;
  std::cout << "输出 '" << argv[2] << std::endl;

  antlr4::ANTLRInputStream input(inFile);
  SYsULexer lexer(&input);

  antlr4::CommonTokenStream tokens(&lexer);
  tokens.fill();

  for (auto&& token : tokens.getTokens()) {
    print_token(token, tokens, outFile, lexer);
  }
}
