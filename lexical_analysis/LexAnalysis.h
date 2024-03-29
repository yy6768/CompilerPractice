// C语言词法分析器
#include <cctype>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>
using namespace std;
/* 不要修改这个标准输入函数 */
void read_prog(string& prog) {
  // char c;
  // while (scanf("%c", &c) != EOF)
  // {
  //     prog += c;
  // }
  ifstream prog_input;
  prog_input.open("input.txt", ios::in);
  char c;
  while ((c = prog_input.get()) != EOF) {
    prog += c;
  }
}
/* 你可以添加其他函数 */

/**
 * 关键字
 */
unordered_map<string, int> c_keys{
    {"auto", 1},      {"break", 2},    {"case", 3},      {"char", 4},
    {"const", 5},     {"continue", 6}, {"default", 7},   {"do", 8},
    {"double", 9},    {"else", 10},    {"enum", 11},     {"extern", 12},
    {"float", 13},    {"for", 14},     {"goto", 15},     {"if", 16},
    {"int", 17},      {"long", 18},    {"register", 19}, {"return", 20},
    {"short", 21},    {"signed", 22},  {"sizeof", 23},   {"static", 24},
    {"struct", 25},   {"switch", 26},  {"typedef", 27},  {"union", 28},
    {"unsigned", 29}, {"void", 30},    {"volatile", 31}, {"while", 32},
    {"-", 33},        {"--", 34},      {"-=", 35},       {"->", 36},
    {"!", 37},        {"!=", 38},      {"%", 39},        {"%=", 40},
    {"&", 41},        {"&&", 42},      {"&=", 43},       {"(", 44},
    {")", 45},        {"*", 46},       {"*=", 47},       {",", 48},
    {".", 49},        {"/", 50},       {"/=", 51},       {":", 52},
    {";", 53},        {"?", 54},       {"[", 55},        {"]", 56},
    {"^", 57},        {"^=", 58},      {"{", 59},        {"|", 60},
    {"||", 61},       {"|=", 62},      {"}", 63},        {"~", 64},
    {"+", 65},        {"++", 66},      {"+=", 67},       {"<", 68},
    {"<<", 69},       {"<<=", 70},     {"<=", 71},       {"=", 72},
    {"==", 73},       {">", 74},       {">=", 75},       {">>", 76},
    {">>=", 77},      {"\"", 78}};

/**
 * pattern
 */

/**
 * Token 类
 */
class Token {
 public:
  enum Type {
    TK_IDENTIFIER,
    TK_INTEGER,
    TK_FLOAT,
    TK_PLUS,
    TK_MINUS,
    TK_PROD,
    TK_DIVIDE,
    TK_KEYWORD,
    TK_COMMENT,
    TK_OPERATOR,
    TK_DELIMITER,
    TK_ERROR
  };

  explicit Token() = default;
  explicit Token(string value, Type type, int id)
      : value_(value), type_(type), id_(id) {}
  auto GetType() const -> Type { return type_; }
  auto GetValue() const -> const string& { return value_; }
  auto GetId() const -> int { return id_; }

 protected:
  // 字符串内容
  string value_;
  // 标号
  Type type_;
  int id_;
};

class ErrorToken : public Token {
 public:
  enum ErrorType {
    INVALID_CONSTANT,
    INVALID_IDENTIFIER,
    INVALID_OPERATOR,
    UNCLOSED_COMMENT,
    UNKNOWN_INPUT,
    UNKNOWN
  };
  explicit ErrorToken(string value, ErrorType error_type, int line_number)
      : Token(value, Type::TK_ERROR, -1),
        error_type_(error_type),
        line_number_(line_number) {}

  auto GetErrorMsg() const -> string {
    ostringstream os;
    os << line_number_;
    if (error_type_ == ErrorType::INVALID_IDENTIFIER) {
      return "There is an invalid identifier " + value_ + " at row " + os.str();
    } else if (error_type_ == ErrorType::INVALID_OPERATOR) {
      return "There is an invalid operator " + value_ + " at row " + os.str();
    } else if (error_type_ == ErrorType::UNCLOSED_COMMENT) {
      return "There is an unclosed commit ar row " + os.str();
    } else if (error_type_ == ErrorType::UNKNOWN_INPUT) {
      return "There is an unknown input symbol" + os.str();
    } else {
      return "Unknown type error at " + os.str();
    }
  }

 private:
  ErrorType error_type_;
  int line_number_;
};

class LexAnalyzer {
 public:
  /**
   * @brief 初始化
   */
  explicit LexAnalyzer(const string& prog) : input_(prog) {}

  /**
   * @brief 主执行函数
   */
  void Execute();

  /**
   * @brief 读取整数/ 浮点数
   *
   */
  void GetNumer(string& word);

  /**
   * @brief 连续读取字母
   */
  void GetAlpha(string& word);

  /**
   * @brief 连续读取操作符
   */
  void GetOperator(string& word);

  /**
   * @brief 输出错误数据
   */
  void OutputErrorMsg();

 private:
  /* 判断是否是空字符 */
  inline auto IsEmpty(char c) -> bool {
    return c == ' ' || c == '\n' || c == '\t' || c == EOF;
  }
  /* 判断是否为数字 */
  inline auto IsDigit(char c) -> bool { return isdigit(c); }
  /* 判断是否为字母 */
  inline auto IsAlpha(char c) -> bool { return isalpha(c); }
  /* 判断是否为字母、数字和下划线*/
  inline auto IsAlnumOrUnder(char c) -> bool { return isalnum(c) || c == '_'; }
  /* 判断是否为界符 */
  inline auto IsDelimiter(char c) -> bool {
    return c == '(' || c == ')' || c == '{' || c == '}' || c == '[' ||
           c == ']' || c == '"' || c == '\'' || c == ',' || c == '.' ||
           c == ';' || c == ':';
  }
  /* 判断是否为运算符 */
  inline auto IsOperator(char c) -> bool {
    return c == '<' || c == '=' || c == '>' || c == '+' || c == '-' ||
           c == '*' || c == '/' || c == '^' || c == '!' || c == '%' ||
           c == '&' || c == '|' || c == '?' || c == '~';
  }
  stringstream input_;
  // 存放结果
  vector<Token> tokens_;
  // 存放错误
  vector<ErrorToken> errors_;
  int line_number_{0};
  int counter_{0};
  bool pre_null_{true};
};

void LexAnalyzer::Execute() {
  string current;
  char c;
  while ((c = input_.peek()) && c != EOF) {
    // cout << c << endl;
    // 识别是否为空白符
    if (IsEmpty(c)) {
      if (c == '\n') {
        line_number_++;
      }
      pre_null_ = true;
      c = input_.get();
      continue;
    }

    current.clear();

    // 常量值
    if (IsDigit(c)) {
      GetNumer(current);
    } else if (IsAlpha(c)) {
      GetAlpha(current);
    } else if (IsOperator(c)) {
      GetOperator(current);
    } else if (IsDelimiter(c)) {
      current.push_back(c);
      tokens_.emplace_back(
          Token(current, Token::TK_DELIMITER, c_keys[current]));
      c = input_.get();
    } else {
      c = input_.get();
      errors_.emplace_back(ErrorToken(
          current, ErrorToken::ErrorType::UNKNOWN_INPUT, line_number_));
    }

    if (!pre_null_) {
      int sz = tokens_.size();
      auto& pre_token = tokens_[sz - 2];
      auto& token = tokens_.back();
      if ((pre_token.GetType() == Token::TK_INTEGER ||
           pre_token.GetType() == Token::TK_FLOAT) &&
          token.GetType() == Token::TK_IDENTIFIER) {
        errors_.emplace_back(ErrorToken(
            pre_token.GetValue() + token.GetValue(),
            ErrorToken::ErrorType::INVALID_IDENTIFIER, line_number_));
      } else if (token.GetType() == Token::TK_OPERATOR &&
                 token.GetType() == Token::TK_OPERATOR) {
        errors_.emplace_back(ErrorToken(pre_token.GetValue() + token.GetValue(),
                                        ErrorToken::ErrorType::INVALID_OPERATOR,
                                        line_number_));
      }
    }
    pre_null_ = false;
  }

  for (int i = 1; i <= tokens_.size(); i++) {
    cout << i << ": <" << tokens_[i - 1].GetValue() << ","
         << tokens_[i - 1].GetId() << ">" << endl;
  }
}

/**
 * 读取常数
 */
void LexAnalyzer::GetNumer(string& word) {
  char c = input_.peek();
  int state = 0;
  while (true) {
    if (state == 0 && c == '.') {
      state = 1;
    } else if (!IsDigit(c)) {
      break;
    }
    word.push_back(c);
    input_.get();
    c = input_.peek();
  }
  auto type = state ? Token::TK_FLOAT : Token::TK_INTEGER;
  tokens_.emplace_back(Token{word, type, 80});
}

void LexAnalyzer::GetAlpha(string& word) {
  char c = input_.peek();
  int state = 0;
  while (true) {
    if (state == 0 && (IsAlpha(c) || c == '_')) {
      state = 1;
    } else if (state != 1 || !IsAlnumOrUnder(c)) {
      break;
    }
    word.push_back(c);
    input_.get();
    c = input_.peek();
  }
  if (c_keys.find(word) != c_keys.end()) {
    tokens_.emplace_back(Token(word, Token::TK_KEYWORD, c_keys[word]));
  } else {
    tokens_.emplace_back(word, Token::TK_IDENTIFIER, 81);
  }
}

void LexAnalyzer::GetOperator(string& word) {
  char c = input_.get();
  word.push_back(c);
  bool is_identifier = false;
  switch (c) {
    case '-': {  // "-", "-=", "--"
      char prec = c;
      c = input_.peek();
      if (prec == c || c == '=' || c == '>') {
        word.push_back(c);
        input_.get();
      }
      break;
    }
    case '+':
    case '&':
    case '|': {  // +, &和 | 都可以变成 cc 或者 c=的状态
      char prec = c;
      c = input_.peek();
      if (c == prec || c == '=') {
        word.push_back(c);
        input_.get();
      }
      break;
    }
    case '*':
    case '^':
    case '!':
    case '=': {  // * ^ ! = 可以接受 c 和 c=的状态
      char prec = c;
      c = input_.peek();
      if (c == '=') {
        word.push_back(c);
        input_.get();
      }
      break;
    }
    case '?':
    case '~':  // 只能获得本身，不接受其他输入
      break;
    case '<':
    case '>': {  // 对于<和> ,可以接受 c,c=,cc,cc=的状态
      char prec = c;
      c = input_.peek();
      if (c == prec) {
        word.push_back(c);
        input_.get();
        c = input_.peek();
      }
      if (c == '=') {
        word.push_back(c);
        input_.get();
      }
      break;
    }
    case '/': {  // 对于/,可以接受// c^* \n， /* c^* */ 以及 /= 和 /
      c = input_.peek();
      if (c == '/') {
        while ((c = input_.get()) && c != '\n') {
          word.push_back(c);
        }
        tokens_.emplace_back(Token(word, Token::TK_COMMENT, 79));
        return;
      }
      if (c == '*') {  // 多行注释
        input_.get();
        word.push_back(c);
        c = input_.get();
        word.push_back(c);
        int old_line_number = line_number_;
        while (c != '*' || input_.peek() != '/') {
          c = input_.get();
          if (c == '\n') {
            ++line_number_;
          } else if (c == EOF) {
            errors_.emplace_back(ErrorToken(
                "", ErrorToken::ErrorType::UNCLOSED_COMMENT, old_line_number));
            return;
          }
          word.push_back(c);
        }
        input_.get();
        word.push_back('/');
        tokens_.emplace_back(Token(word, Token::TK_COMMENT, 79));
        return;
      }
      if (c == '=') {
        word.push_back(c);
        input_.get();
      }
      break;
    }
    case '%': {
      char prec = c;
      c = input_.peek();
      if (IsAlpha(c)) {
        word.push_back(c);
        input_.get();
        is_identifier = true;
        tokens_.push_back(Token(word, Token::TK_IDENTIFIER, 81));
      }
      if (c == '=') {
        word.push_back(c);
        input_.get();
      }
      break;
    }
    default: {
      input_.get();
      errors_.emplace_back(ErrorToken(
          word, ErrorToken::ErrorType::INVALID_OPERATOR, line_number_));
      return;
    }
  }
  if (c_keys.find(word) != c_keys.end()) {
    tokens_.emplace_back(Token(word, Token::TK_OPERATOR, c_keys[word]));
  } else if (!is_identifier) {
    errors_.emplace_back(ErrorToken(
        word, ErrorToken::ErrorType::INVALID_OPERATOR, line_number_));
  }
}

void LexAnalyzer::OutputErrorMsg() {
  if (!errors_.empty()) {
    cout << "The lex analyzer found " << errors_.size() << " errors:" << endl;
  }
  for (const auto& error_token : errors_) {
    cout << error_token.GetErrorMsg() << endl;
  }
}

void Analysis() {
  string prog;
  // 处理c_keys.txt
  // 读取程序
  read_prog(prog);
  /********* Begin *********/
  auto lex_analyzer = LexAnalyzer(prog);
  // 词法分析主过程
  lex_analyzer.Execute();
  // 输出错误信息
  lex_analyzer.OutputErrorMsg();
  /********* End *********/
}