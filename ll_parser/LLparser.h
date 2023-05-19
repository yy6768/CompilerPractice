// C语言词法分析器
#include <cstdio>
#include <cstring>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <sstream>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>
using namespace std;
/* 不要修改这个标准输入函数 */
void read_prog(string& prog) {
  // char c;
  // while (scanf("%c", &c) != EOF) {
  //   prog += c;
  // }
  ifstream prog_input;
  prog_input.open("input.txt", ios::in);
  char c;
  while ((c = prog_input.get()) != EOF) {
    prog += c;
  }
}
/* 你可以添加其他函数 */

// 终结符
unordered_set<string> terminates{
    "{", "}",  "if", "(",  ")",   "then", "else", "while", "(",
    ")", "ID", "=",  ">",  "<",   ">=",   "<=",   "==",    "+",
    "-", "*",  "/",  "ID", "NUM", "E",    ";",    "$"};

string grammer(
    "program -> compoundstmt \n"
    "stmt-> ifstmt | whilestmt | assgstmt | compoundstmt\n"
    "compoundstmt->{ stmts }\n"
    "stmts->stmt stmts | E\n"
    "ifstmt->if ( boolexpr ) then stmt else stmt\n"
    "whilestmt->while ( boolexpr ) stmt\n"
    "assgstmt-> ID = arithexpr ;\n"
    "boolexpr-> arithexpr boolop arithexpr\n"
    "boolop -> < | > | <= | >= | ==\n"
    "arithexpr-> multexpr arithexprprime\n"
    "arithexprprime-> + multexpr arithexprprime | - multexpr arithexprprime | "
    "E\n"
    "multexpr->simpleexpr multexprprime\n"
    "multexprprime->* simpleexpr multexprprime | / simpleexpr multexprprime | "
    "E\n"
    "simpleexpr->ID | NUM | ( arithexpr )");

/**
 * 辅助函数：去除字符串前后空格
 */
string& trim(std::string& s) {
  if (!s.empty()) {
    s.erase(0, s.find_first_not_of(" "));
    s.erase(s.find_last_not_of(" ") + 1);
  }
  return s;
}

/**
 * 错误处理
 */
class ErrorHandler {
 public:
  enum class ErrorType { MISSING_SYMBOL, UNEXPECTED_SYMBOL, EARLY_END };
  explicit ErrorHandler() = default;
  void Concat(int line_number, ErrorType error_type, string reason);
  void Output();

 private:
  stringstream error_msg_;
};

void ErrorHandler::Concat(int line_number, ErrorType error_type,
                          string symbol) {
  error_msg_ << "语法错误，第" << line_number << "行，";
  switch (error_type) {
    case ErrorType::MISSING_SYMBOL:
      error_msg_ << "缺少\"" << symbol << "\"" << endl;
      break;
    case ErrorType::UNEXPECTED_SYMBOL:
      error_msg_ << "未解析的符号\"" << symbol << "\"" << endl;
      break;
    case ErrorType::EARLY_END:
      error_msg_ << "代码不完整" << endl;
      break;
    default:
      error_msg_ << "未知错误" << endl;
  }
}

void ErrorHandler::Output() { cout << error_msg_.str(); }

class LLParser {
  // 终结符，非终结符和产生式
  using Terminal = string;
  using NonTerminal = string;
  using Production = pair<Terminal, string>;

 public:
  explicit LLParser(const string& input) : input_(input) {
    handler_ = ErrorHandler();
    stack_.emplace("program", 0);
    line_number_ = 0;
  }
  // 输入语法
  void GenerateGrammer(const string& raw_grammer);
  // 生成First集合
  void GenerateFist();
  // 生成Follow集合
  void GenerateFollow();
  // 主要转化过程
  void Parse();
  // 输出错误
  inline void ErrorOutput() { handler_.Output(); }
  // 输出解析结果
  inline void Output() { cout << output_.str(); }

 private:
  // 递归函数计算First集合
  void CalculateFirst(const NonTerminal& left);
  // 递归将A->aB形式的A的follow集放入B的follow集中
  void InsertDependsFollow(
      const NonTerminal& left,
      unordered_map<NonTerminal, unordered_set<NonTerminal>> depends,
      unordered_set<NonTerminal> flag);
  // 输出符号
  inline void Append(string symbol, int tabs) {
    for (int i = 0; i < tabs; i++) {
      output_ << "\t";
    }
    output_ << symbol << endl;
  }
  string input_;
  // 输入指针ip_
  int32_t ip_;
  // 行号
  int32_t line_number_;
  // 错误处理器
  ErrorHandler handler_;
  queue<string> buffer_;
  // 栈
  stack<pair<string, int>> stack_;
  // 文法
  unordered_map<NonTerminal, unordered_set<string>> grammer_;
  // First 集合
  unordered_map<NonTerminal, unordered_set<Terminal>> first_;
  // Follow集合
  unordered_map<NonTerminal, unordered_set<Terminal>> follow_;
  // First 集中空字符对应的产生式
  unordered_map<NonTerminal, unordered_set<string>> empty_grammer_;
  // 预测分析表
  unordered_map<NonTerminal, unordered_map<Terminal, Production>> ll_table_;
  // 输出流
  stringstream output_;
};

void LLParser::GenerateGrammer(const string& raw_grammer) {
  stringstream input(raw_grammer);
  string line;
  while (getline(input, line)) {
    Terminal left;
    string right;
    bool is_left = true;
    for (int i = 0; i < line.size(); i++) {
      char c = line[i];
      if (c == '\t' || c == '\n') {
        continue;
      }
      if (i + 1 < line.size() && c == '-' && line[i + 1] == '>') {
        is_left = false;
        left = trim(left);
        i++;
        continue;
      }
      if (is_left) {
        left.push_back(c);
      } else {
        if (c == '|') {  // 结束一个产生式
          grammer_[left].emplace(trim(right));
          right.clear();
          continue;
        } else {
          right.push_back(c);
        }
      }
    }
    if (!right.empty()) {
      grammer_[left].emplace(trim(right));
    }
  }
}

void LLParser::CalculateFirst(const NonTerminal& left) {
  if (first_.find(left) !=
      first_.end()) {  // 如果已经有First集合那么说明已经计算过
    return;
  }
  const auto& rights = grammer_[left];  // 产生式右部的结合
  for (const auto& right_expr : rights) {
    string right;
    stringstream right_expr_ss(right_expr);
    while (right_expr_ss >> right) {  // 每一个终结符 / 非终结符 “ ” 分开
      if (terminates.find(right) !=
          terminates.end()) {  // 如果起始是终结符，加入first集合直接结束
        first_[left].emplace(right);
        if (right != "E") {  // 插入到预测分析表中
          ll_table_[left][right] = {left, right_expr};
        } else {
          empty_grammer_[left].emplace(right_expr);
        }
        break;
      }
      CalculateFirst(right);   // 非终结符先要计算First集合
      bool may_empty = false;  // 可能为空的非终结符
      for (const auto& r : first_[right]) {
        first_[left].emplace(r);
        if (r == "E") {
          // 如果为空需要存储到first集为空的集合中的，用于构建预测分析表
          may_empty = true;
          empty_grammer_[left].emplace(right_expr);
        } else {  // 插入到预测分析表中
          ll_table_[left][r] = {left, right_expr};
        }
      }
      if (!may_empty) {
        break;
      }
    }
  }
}

void LLParser::GenerateFist() {
  // 迭代所有产生式, 计算其First
  for (const auto& grammer : grammer_) {
    CalculateFirst(grammer.first);
  }
}

void LLParser::InsertDependsFollow(
    const NonTerminal& left,
    unordered_map<NonTerminal, unordered_set<NonTerminal>> depends,
    unordered_set<NonTerminal> flag) {
  if (flag.find(left) != flag.end()) {
    return;
  }
  flag.emplace(left);
  for (const auto& child :
       depends[left]) {  // 对于 A->aB/aBb(b可能为空)， left
                         // 为B，先计算A，再将A的follow放入B中
    InsertDependsFollow(child, depends, flag);
    for (const auto& child_follow : follow_[child]) {
      follow_[left].emplace(child_follow);
    }
  }
}

void LLParser::GenerateFollow() {
  follow_[stack_.top().first].emplace("$");  // 起始符号 follow集含有$

  unordered_map<NonTerminal, unordered_set<NonTerminal>> depends;

  for (const auto& grammer : grammer_) {
    const auto& left = grammer.first;
    const auto& rights = grammer.second;
    for (const auto& right_expr : rights) {
      vector<string> exprs;
      stringstream right_expr_ss(right_expr);
      string expr;
      while (right_expr_ss >> expr) {
        // 有序存储产生式右部所有的终结符和非终结符
        exprs.emplace_back(expr);
      }
      // 计算所有产生式左部和右部结尾的依赖关系
      for (auto it = exprs.rbegin(); it != exprs.rend();
           it++) {  // 反向迭代产生式右部
        if (terminates.find(*it) == terminates.end()) {
          if (*it != left) {  // 自身不能相等
            depends[*it].emplace(left);
          }
          if (grammer_[*it].find("E") == grammer_[*it].end()) {
            // 如果可能为空还需要考虑 A->aBb 且b为空的情况，
            // A的所有follow需要添加到B中
            break;
          }
        } else {
          break;
        }
      }
      // 将S->AB 形式的产生式中的B的first集放入A的follow集中
      for (int i = 0; i < exprs.size(); i++) {
        if (terminates.find(exprs[i]) == terminates.end()) {
          for (int j = i + 1; j < exprs.size(); j++) {
            bool may_empty = false;
            if (terminates.find(exprs[j]) != terminates.end() &&
                exprs[j] != "E") {  // 终结符非空直接进入follow集
              follow_[exprs[i]].emplace(exprs[j]);
              break;
            }
            // B的first集合
            for (const auto& f : first_[exprs[j]]) {
              if (f == "E") {
                may_empty = true;
              } else {
                follow_[exprs[i]].emplace(f);
              }
            }
            if (!may_empty) {
              break;
            }
          }
        }
      }
    }
  }

  unordered_set<NonTerminal> flag;
  // 迭代所有产生式，计算Follow
  for (const auto& grammer : grammer_) {
    InsertDependsFollow(grammer.first, depends, flag);
  }

  for (const auto& f : follow_) {
    const auto& left = f.first;
    const auto& follows = f.second;
    if (empty_grammer_.find(left) != empty_grammer_.end()) {
      for (const auto& follow : follows) {
        for (const auto& right_expr : empty_grammer_[left]) {
          ll_table_[left][follow] = {left, right_expr};
        }
      }
    }
  }
  // for (const auto& [left, follows] : follow_) {
  //   cout << left << ":" << endl;
  //   for (const auto& follow : follows) {
  //     cout << follow << endl;
  //   }
  //   cout << endl;
  // }
  // for (const auto& [left, childrens] : depends) {
  //   cout << left << ":" << endl;
  //   for (const auto& child : childrens) {
  //     cout << child << endl;
  //   }
  //   cout << endl;
  // }

  // for (const auto& [non, cols] : ll_table_) {
  //   cout << non << ":" << endl;
  //   for (const auto& [term, pro] : cols) {
  //     cout << term << " "
  //          << " " << pro.first << "->" << pro.second << endl;
  //   }
  //   cout << endl;
  // }
}

void LLParser::Parse() {
  stringstream input_ss(input_);
  string line;
  bool need_input = true;
  while (getline(input_ss, line)) {
    // cout << line;
    stringstream line_ss(line);
    string word;
    while (!stack_.empty()) {
      if (need_input) {
        if (!(line_ss >> word)) {
          break;
        }
        need_input = false;
      }
      auto& x = stack_.top().first;
      auto tabs = stack_.top().second;
      if (x == "E") {
        stack_.pop();
        Append(x, tabs);
        continue;
      }
      if (x == word) {
        stack_.pop();
        Append(x, tabs);
        need_input = true;
        continue;
      }

      if (terminates.find(x) != terminates.end()) {
        handler_.Concat(line_number_, ErrorHandler::ErrorType::MISSING_SYMBOL,
                        x);
        stack_.pop();
        Append(x, tabs);  // 修复
        continue;
      }
      if (ll_table_[x].find(word) == ll_table_[x].end()) {
        // need_input = true;
        stack_.pop();
        Append(x, tabs);  // 修复
        if (first_[x].find("E") != first_[x].end()) {
          stack_.emplace("E", tabs + 1);
        } else {
          handler_.Concat(line_number_,
                          ErrorHandler::ErrorType::UNEXPECTED_SYMBOL, word);
        }
        continue;  // 恐慌模式
      }
      auto& production = ll_table_[x][word];
      auto& right = production.second;
      stringstream right_ss(right);
      vector<string> exprs;
      string expr;
      while (right_ss >> expr) {
        exprs.emplace_back(expr);
      }
      stack_.pop();  // 弹栈
      Append(x, tabs);
      for (auto it = exprs.rbegin(); it != exprs.rend(); it++) {
        stack_.emplace(*it, tabs + 1);
      }
    }
    line_number_++;
  }
  if (!stack_.empty()) {
    handler_.Concat(line_number_, ErrorHandler::ErrorType::EARLY_END, "");
  }
}

void Analysis() {
  string prog;
  read_prog(prog);
  /* 骚年们 请开始你们的表演 */
  /********* Begin *********/
  LLParser ll_parser(prog);
  ll_parser.GenerateGrammer(grammer);
  ll_parser.GenerateFist();
  ll_parser.GenerateFollow();
  ll_parser.Parse();
  ll_parser.ErrorOutput();
  ll_parser.Output();
  /********* End *********/
}