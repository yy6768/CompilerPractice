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
string& trim(string& s) {
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
  enum class ErrorType { MISSING_SYMBOL, UNEXPECTED_SYMBOL, EARLY_END, NOT_LR };
  explicit ErrorHandler() = default;
  explicit ErrorHandler(const ErrorHandler& handler)
      : error_msg_(handler.error_msg_.str()) {}
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
    case ErrorType::NOT_LR:
      error_msg_ << "文法错误" << endl;
    default:
      error_msg_ << "未知错误" << endl;
  }
}

void ErrorHandler::Output() { cout << error_msg_.str(); }

/**
 * LR项
 */
struct Item {
  using NonTerminal = string;
  int dot_idx_;
  NonTerminal non_term_;
  string formula_;
  // 点的位置
  unordered_set<string> symbol_;  // 展望串
  Item() = default;
  Item(const Item& item)
      : non_term_(item.non_term_),
        formula_(item.formula_),
        dot_idx_(item.dot_idx_),
        symbol_(item.symbol_) {}
  Item(NonTerminal non_term, string formula, int dot_idx)
      : non_term_(non_term), formula_(formula), dot_idx_(dot_idx) {}

  bool operator==(const Item& rhs) const {
    return dot_idx_ == rhs.dot_idx_ && non_term_ == rhs.non_term_ &&
           formula_ == rhs.formula_ && symbol_ == rhs.symbol_;
  }
};

namespace std {
template <>
class hash<Item> {  // Item 的hash函数
 public:
  size_t operator()(const Item& item) const {
    size_t seed = 0;
    seed ^=
        hash<string>()(item.formula_) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    seed ^=
        hash<string>()(item.non_term_) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    seed ^= hash<int>()(item.dot_idx_) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    return seed;
  }
};
};  // namespace std

struct CanonicalCollection {
  int idx_;  // 编号
  unordered_set<Item> items_;
  CanonicalCollection() = default;
  CanonicalCollection(const CanonicalCollection& rhs)
      : idx_(rhs.idx_), items_(rhs.items_) {}
  CanonicalCollection(int idx, unordered_set<Item>& items)
      : idx_(idx), items_(items) {}

  bool operator==(const CanonicalCollection& rhs) const {
    return items_ == rhs.items_;
  }
};

namespace std {
template <>
class hash<CanonicalCollection> {
 public:
  size_t operator()(const CanonicalCollection& collection) const {
    size_t seed = 0;
    for (const auto& item : collection.items_) {
      seed ^= hash<Item>()(item) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
      for (const auto& symbol : item.symbol_) {
        seed ^= hash<string>()(symbol) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
      }
    }
    return seed;
  }
};
};  // namespace std

/**
 * ParseTable 表项
 */
struct TableItem {
  enum class ActionState { ERROR = 0, SHIFT, REDUCE, ACCEPT, GOTO };

  int sid_{0};
  ActionState state_;
  TableItem() = default;
  TableItem(int sid, ActionState state) : sid_(sid), state_(state) {}

  inline void Output() const {
    switch (state_) {
      case ActionState::ACCEPT:
        cout << "a";
        break;
      case ActionState::REDUCE:
        cout << "r";
        break;
      case ActionState::GOTO:
        cout << "g";
        break;
      case ActionState::ERROR:
        cout << "e";
        break;
      case ActionState::SHIFT:
        cout << "s";
      default:
        break;
    }
    cout << sid_ << endl;
  }
};

/**
 * Goto表项
 */
struct Reflect {
  int first_;
  string symbol_;
  int next_;
  Reflect() = default;
};

struct Token {
  string value_;
  int ln_;
  explicit Token() = default;
  explicit Token(const Token& token) = default;
  explicit Token(string value, int ln) : value_(value), ln_(ln) {}
};

class LRParser;
/**
 * LR(1) 预测表
 *
 */
class LRParseTable {
  friend class LRParser;

 public:
  LRParseTable() = default;
  LRParseTable(LRParser* parser) : parser_(parser) {}
  void Init();   // 初始化
  void Build();  // 构建表

 private:
  LRParser* parser_;
  unordered_map<int, unordered_map<string, TableItem>> table_;
};

/**
 * LRParser
 */
class LRParser {
  // 终结符，非终结符和产生式
  using Terminal = string;
  using NonTerminal = string;
  using Production = pair<Terminal, string>;

  friend class LRParseTable;  // 解析表友元

 public:
  explicit LRParser(const string& input, const NonTerminal& start_symbol)
      : input_(input), start_symbol_(start_symbol), table_(this) {
    GenerateGrammer(grammer);
    GenerateFist();
    GenerateFollow();
    CalculateItemSet();
    table_.Init();
    table_.Build();
  }
  // 输入语法
  void GenerateGrammer(const string& raw_grammer);
  // 生成First集合
  void GenerateFist();
  // 生成Follow集合
  void GenerateFollow();
  // 计算所有项集集合集合
  void CalculateItemSet();
  // 主要转化过程
  void Parse();
  // 输出错误
  inline void ErrorOutput() { handler_.Output(); }
  // 输出解析结果
  void Output();

 private:
  // 递归函数计算First集合
  void CalculateFirst(const NonTerminal& left);
  // 递归将A->aB形式的A的follow集放入B的follow集中
  void InsertDependsFollow(
      const NonTerminal& left,
      unordered_map<NonTerminal, unordered_set<NonTerminal>> depends,
      unordered_set<NonTerminal> flag);
  // 计算闭包
  void Closure(unordered_set<Item>& items);
  string input_;
  // 起始符号
  NonTerminal start_symbol_;
  // 错误处理器
  ErrorHandler handler_;

  // 输入栈
  stack<Token> buffer_;
  // 工作栈
  stack<Token> stack_;
  // 状态栈
  stack<int> state_;
  // reduce 操作， 用于输出
  vector<TableItem> reduce_action_;

  // 文法 + 编号
  unordered_map<NonTerminal, unordered_map<string, int>> grammer_;
  unordered_map<int, Production> idx_grammer_;
  // 非终结符集合
  unordered_set<NonTerminal> non_terminals_;
  // 终结符集合
  unordered_set<Terminal> terminals_;
  // First 集合
  unordered_map<NonTerminal, unordered_set<Terminal>> first_;
  // Follow集合
  unordered_map<NonTerminal, unordered_set<Terminal>> follow_;

  // LR（1）语法分析必须
  // Item 集合
  unordered_set<CanonicalCollection> canonical_collections_;
  // Reflect 项
  vector<Reflect> reflects_;
  // First 集中空字符对应的产生式
  unordered_map<NonTerminal, unordered_set<string>> empty_grammer_;
  // LR(1) 语法分析表
  LRParseTable table_;
};

void LRParser::GenerateGrammer(const string& raw_grammer) {
  stringstream input(raw_grammer);
  string line;

  // 放置增广文法
  grammer_[start_symbol_ + "'"].emplace(start_symbol_, 0);
  idx_grammer_[0] = {start_symbol_ + "'", start_symbol_};
  non_terminals_.emplace(start_symbol_ + "'");
  // 将raw grammer中的文法放入grammer_集合中
  int gid = 1;
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
          idx_grammer_[gid] = {left, right};
          grammer_[left].emplace(trim(right), gid++);
          non_terminals_.emplace(left);
          right.clear();
          continue;
        } else {
          right.push_back(c);
        }
      }
    }
    if (!right.empty()) {
      idx_grammer_[gid] = {left, right};
      grammer_[left].emplace(trim(right), gid++);
      non_terminals_.emplace(left);
    }
  }
  // 非终结符
  terminals_ = terminates;
}

void LRParser::CalculateFirst(const NonTerminal& left) {
  if (first_.find(left) !=
      first_.end()) {  // 如果已经有First集合那么说明已经计算过
    return;
  }
  const auto& rights = grammer_[left];  // 产生式右部的结合
  for (const auto& right_expr : rights) {
    string right;
    stringstream right_expr_ss(right_expr.first);
    while (right_expr_ss >> right) {  // 每一个终结符 / 非终结符 “ ” 分开
      if (terminates.find(right) !=
          terminates.end()) {  // 如果起始是终结符，加入first集合直接结束
        first_[left].emplace(right);
        if (right == "E") {  // 插入到可空的表达式中
          empty_grammer_[left].emplace(right_expr.first);
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
          empty_grammer_[left].emplace(right_expr.first);
        }
      }
      if (!may_empty) {
        break;
      }
    }
  }
}

void LRParser::GenerateFist() {
  // 迭代所有产生式, 计算其First
  for (const auto& grammer : grammer_) {
    CalculateFirst(grammer.first);
  }
}

void LRParser::InsertDependsFollow(
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

void LRParser::GenerateFollow() {
  follow_[start_symbol_].emplace("$");  // 起始符号 follow集含有$

  unordered_map<NonTerminal, unordered_set<NonTerminal>> depends;

  for (const auto& grammer : grammer_) {
    const auto& left = grammer.first;
    const auto& rights = grammer.second;
    for (const auto& right_expr : rights) {
      vector<string> exprs;
      stringstream right_expr_ss(right_expr.first);
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
}

void split(const string& formula, vector<string>& symbols) {
  stringstream ss(formula);
  string word;
  while (ss >> word) {
    symbols.emplace_back(word);
  }
}

// 闭包计算
void LRParser::Closure(unordered_set<Item>& items) {
  queue<Item> que;
  for (const auto& item : items) {
    que.emplace(item);
  }
  while (!que.empty()) {
    auto& item = que.front();
    if (item.dot_idx_ != -1) {
      // 解析产生式右部
      vector<string> symbols;
      split(item.formula_, symbols);
      // 分析.后的符号是否为非终结符
      string B;
      B = symbols[item.dot_idx_];
      // 如果B是非终结符
      if (non_terminals_.find(B) != non_terminals_.end()) {
        for (const auto& grammer : grammer_[B]) {
          Item new_item{B, grammer.first, 0};
          if (grammer.first == "E") {
            new_item.dot_idx_ = -1;
          }
          if (item.dot_idx_ + 1 == symbols.size()) {  // beta 为空
            new_item.symbol_ = item.symbol_;
          } else {  // 判断first(beta)
            string beta = symbols[item.dot_idx_ + 1];
            if (non_terminals_.find(beta) !=
                non_terminals_.end()) {  // beta 是非终结符
              if (empty_grammer_.find(beta) ==
                  empty_grammer_.end()) {  // 不含有空串
                new_item.symbol_ = first_[beta];
              } else {
                for (const auto& s : first_[beta]) {
                  if (s != "E") {
                    new_item.symbol_.emplace(s);
                  }
                }
                for (const auto& s : item.symbol_) {
                  new_item.symbol_.emplace(s);
                }
              }
            } else {
              new_item.symbol_.emplace(beta);
            }
          }
          if (items.find(new_item) == items.end()) {
            que.emplace(new_item);
            items.emplace(new_item);
          }
        }
      }
    }
    que.pop();
  }
}

// 首项簇集合计算
void LRParser::CalculateItemSet() {
  int index = 0;
  queue<int> que;  // 项目集编号队列

  unordered_set<string> all_terms;
  for (const auto& vt : terminals_) {
    all_terms.emplace(vt);
  }
  for (const auto& vn : non_terminals_) {
    all_terms.emplace(vn);
  }

  unordered_set<Item> first_set;
  Item item{start_symbol_ + "'", start_symbol_, 0};
  item.symbol_.emplace("$");
  first_set.emplace(item);

  Closure(first_set);
  canonical_collections_.emplace(CanonicalCollection{index, first_set});
  que.emplace(index++);
  while (!que.empty()) {
    auto id = que.front();
    CanonicalCollection collection;
    for (const auto& c : canonical_collections_) {
      if (id == c.idx_) {
        collection = c;
        break;
      }
    }
    // 遍历符号
    for (const auto& term : all_terms) {
      unordered_set<Item> items;
      for (const auto& it : collection.items_) {
        vector<string> symbols;
        split(it.formula_, symbols);
        if (it.dot_idx_ < symbols.size() && symbols[it.dot_idx_] == term) {
          int new_dot_idx =
              it.dot_idx_ + 1 == symbols.size() ? -1 : it.dot_idx_ + 1;
          Item new_item{it.non_term_, it.formula_, new_dot_idx};
          new_item.symbol_ = it.symbol_;
          items.insert(new_item);
        }
      }
      Closure(items);
      CanonicalCollection new_collection(index, items);
      if (!items.empty()) {
        auto it = canonical_collections_.find(new_collection);
        int next = 0;
        if (it != canonical_collections_.end()) {  // 已经存在相关的表项
          next = it->idx_;
        } else {
          next = index;
          canonical_collections_.emplace(new_collection);
          que.emplace(index++);
        }
        Reflect reflect;
        reflect.first_ = id;
        reflect.symbol_ = term;
        reflect.next_ = next;
        reflects_.emplace_back(reflect);
      }
    }
    que.pop();
  }
}

void LRParseTable::Init() {
  for (const auto& cl : parser_->canonical_collections_) {
    int id = cl.idx_;
    TableItem a;
    a.state_ = TableItem::ActionState::ERROR;
    for (const auto& vt : parser_->terminals_) {
      table_[id][vt] = a;
    }
    for (const auto& vn : parser_->non_terminals_) {
      table_[id][vn] = a;
    }
  }
}
/**
 * 1.遍历项集族，找到 dot_idx_ == -1 的项，对应accept和reduce
 * 2.遍历状态转移集合reflects_, 非终结符对应goto， 终结符对应action 的shift
 */
void LRParseTable::Build() {
  for (const auto& cl :
       parser_->canonical_collections_) {  // accept action 和 reduce 项
    int id = cl.idx_;
    const auto& items = cl.items_;
    for (const auto& item : items) {
      if (item.dot_idx_ == -1) {  // accept action
        TableItem action;
        if (item.non_term_ == parser_->start_symbol_ + "'") {
          action.state_ = TableItem::ActionState::ACCEPT;
          if (table_[id]["$"].state_ != TableItem::ActionState::ERROR) {
            parser_->handler_.Concat(-1, ErrorHandler::ErrorType::NOT_LR, "");
          }
          table_[id]["$"] = action;
        } else {
          action.state_ = TableItem::ActionState::REDUCE;
          action.sid_ = parser_->grammer_[item.non_term_][item.formula_];
          for (const auto& symbol : item.symbol_) {
            if (table_[id][symbol].state_ != TableItem::ActionState::ERROR) {
              parser_->handler_.Concat(-1, ErrorHandler::ErrorType::NOT_LR, "");
            }
            table_[id][symbol] = action;
          }
        }
      }
    }
  }
  for (const auto& reflect : parser_->reflects_) {  // 移入项和GOTO项
    TableItem action;
    if (parser_->non_terminals_.find(reflect.symbol_) !=
        parser_->non_terminals_.end()) {
      action.state_ = TableItem::ActionState::GOTO;
    } else {
      action.state_ = TableItem::ActionState::SHIFT;
    }
    action.sid_ = reflect.next_;
    if (table_[reflect.first_][reflect.symbol_].state_ !=
        TableItem::ActionState::ERROR) {
      parser_->handler_.Concat(-1, ErrorHandler::ErrorType::NOT_LR, "");
    }
    table_[reflect.first_][reflect.symbol_] = action;
  }
}

/**
 * 工具函数处理
 */
void split_token(const string& input, vector<Token>& tokens) {
  stringstream input_ss(input);
  string line;
  bool need_input = true;
  int ln = 1;
  while (getline(input_ss, line)) {
    if (line.empty()) {
      continue;
    }
    stringstream line_ss(line);
    string word;
    while (line_ss >> word) {
      Token token(word, ln);
      tokens.emplace_back(token);
    }
    ++ln;
  }
}

void LRParser::Parse() {
  vector<Token> tokens;
  split_token(input_, tokens);
  buffer_.emplace(Token("$", -1));
  for (auto it = tokens.rbegin(); it != tokens.rend(); ++it) {
    buffer_.emplace(*it);
  }
  state_.emplace(0);
  int cur_line = 1;
  while (!buffer_.empty()) {
    const auto& top_state = state_.top();
    const auto& top_token = buffer_.top();
    const auto& op = table_.table_[top_state][top_token.value_];

    // cout << top_token.value_ << " " << top_state << endl;
    // op.Output();
    if (op.state_ == TableItem::ActionState::ACCEPT) {
      break;
    }
    if (op.state_ == TableItem::ActionState::SHIFT) {  // 移位
      stack_.push(top_token);
      buffer_.pop();
      state_.push(op.sid_);
    }
    if (op.state_ == TableItem::ActionState::REDUCE) {
      reduce_action_.emplace_back(op);
      vector<string> right;  // 产生式右部
      split(idx_grammer_[op.sid_].second, right);
      for (int i = 0; i < right.size(); i++) {
        if (right[i] == "E") {
          continue;
        }
        stack_.pop();
        state_.pop();
      }
      const auto& left = idx_grammer_[op.sid_].first;
      stack_.emplace(left, top_token.ln_);

      const auto& new_op = table_.table_[state_.top()][left];
      state_.emplace(new_op.sid_);
      if (new_op.state_ == TableItem::ActionState::ACCEPT) {
        break;
      }
    }

    if (op.state_ == TableItem::ActionState::ERROR) {
      buffer_.emplace(";", cur_line);
      handler_.Concat(cur_line, ErrorHandler::ErrorType::UNEXPECTED_SYMBOL,
                      ";");
    }
    cur_line = top_token.ln_;
  }
}

void LRParser::Output() {
  ErrorOutput();
  cout << start_symbol_ << " ";
  vector<string> output;
  string s;
  output.emplace_back(start_symbol_);
  for (auto it = reduce_action_.rbegin(); it != reduce_action_.rend(); ++it) {
    output.pop_back();  // 弹出末尾元素
    int sid = it->sid_;
    vector<string> right;
    split(idx_grammer_[sid].second, right);
    const string& left = idx_grammer_[sid].first;
    for (const auto& s : right) {
      output.emplace_back(s);
    }
    while (!output.empty()) {
      if (terminals_.find(output.back()) == terminals_.end()) {
        break;
      }
      string s_add;
      if (output.back() != "E") {
        s_add = output.back() + " ";
      }
      s = s_add + s;
      output.pop_back();
    }
    cout << "=> \n";
    for (const auto& o : output) {
      cout << o << " ";
    }
    cout << s;
  }
}

void Analysis() {
  string prog;
  read_prog(prog);
  /* 骚年们 请开始你们的表演 */
  /********* Begin *********/
  LRParser lr_parser(prog, "program");

  lr_parser.Parse();
  lr_parser.Output();
  /********* End *********/
}