// translation schema
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
  //   char c;
  //   while (scanf("%c", &c) != EOF) {
  //     prog += c;
  //   }
  ifstream prog_input;
  prog_input.open("input.txt", ios::in);
  char c;
  while ((c = prog_input.get()) != EOF) {
    prog += c;
  }
}
/* 你可以添加其他函数 */

// Token 的类型
enum class TokenType { REAL, INT, ID, Terminal, MARK };
// 产生式的类型
// 控制语义
enum class ProductionType {
  EMPTY = 0,   // 空
  ASSIGN,      // 赋值
  BOOL,        // bool
  ARITH,       // 算术表达式
  IF,          // IF 语句块
  WHILE,       // WHILE 语句块
  DECLARE,     // 声明
  ARITHPRIME,  // +-算式
  INSTANT,     // 数
  IDVALUE,     // id值
  BOOLOP,      // bool算子
  OTHER        // 其他
};

// 终结符
unordered_set<string> terminates{
    "{",  "}",      "if",      "(",  ")",  "then", "else", "while", "=",
    ">",  "<",      ">=",      "<=", "==", "+",    "-",    "*",     "/",
    "ID", "INTNUM", "REALNUM", "E",  ";",  "$",    "#",    "int",   "real"};

// 文法
string grammer(
    "program -> decls compoundstmt \n"
    "decls -> decl ; decls | E \n"
    "decl -> int ID = INTNUM | real ID = REALNUM\n"
    "stmt-> ifstmt | whilestmt | assgstmt | compoundstmt \n"
    "compoundstmt -> { stmts } \n"
    "stmts -> stmt stmts | E \n"
    "ifstmt -> if ( boolexpr ) then stmt else stmt \n"
    "whilestmt->while ( boolexpr ) stmt \n"
    "assgstmt-> ID = arithexpr ; \n"
    "boolexpr-> arithexpr boolop arithexpr \n"
    "boolop -> < | > | <= | >= | == \n"
    "arithexpr-> multexpr arithexprprime \n"
    "arithexprprime-> + multexpr arithexprprime | - multexpr arithexprprime | "
    "E "
    "\n"
    "multexpr->simpleexpr multexprprime \n"
    "multexprprime->* simpleexpr multexprprime | / simpleexpr multexprprime | "
    "E \n"
    "simpleexpr -> ID | INTNUM | REALNUM | ( arithexpr )");

unordered_map<int, ProductionType> synax_rules = {
    {3, ProductionType::EMPTY},       {4, ProductionType::DECLARE},
    {5, ProductionType::DECLARE},     {12, ProductionType::EMPTY},
    {13, ProductionType::IF},         {14, ProductionType::WHILE},
    {15, ProductionType::ASSIGN},     {16, ProductionType::BOOL},
    {17, ProductionType::BOOLOP},     {18, ProductionType::BOOLOP},
    {19, ProductionType::BOOLOP},     {20, ProductionType::BOOLOP},
    {21, ProductionType::BOOLOP},     {23, ProductionType::ARITHPRIME},
    {24, ProductionType::ARITHPRIME}, {25, ProductionType::EMPTY},
    {27, ProductionType::ARITHPRIME}, {28, ProductionType::ARITHPRIME},
    {29, ProductionType::EMPTY},      {30, ProductionType::IDVALUE},
    {31, ProductionType::INSTANT},    {32, ProductionType::INSTANT},
    {33, ProductionType::ARITH},
};

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
 * 辅助函数：划分字符串为vector
 */
void split(const string& formula, vector<string>& symbols) {
  stringstream ss(formula);
  string word;
  while (ss >> word) {
    symbols.emplace_back(word);
  }
}

/**
 * 错误处理
 */
class ErrorHandler {
 public:
  enum class ErrorType {
    NOT_LR,
    REAL_TO_INT,
    DIVISION_ZERO,
    UNDECLEARED_ID,
    UNDECLARED_ID_ASSIGN
  };
  explicit ErrorHandler() = default;
  explicit ErrorHandler(const ErrorHandler& handler)
      : error_msg_(handler.error_msg_.str()) {}
  void Concat(int line_number, ErrorType error_type, string reason);
  void Output();
  inline bool Empty() { return error_msg_.str().empty(); }

 private:
  stringstream error_msg_;
};

void ErrorHandler::Concat(int line_number, ErrorType error_type,
                          string symbol) {
  error_msg_ << "error message:line " << line_number;
  switch (error_type) {
    case ErrorType::NOT_LR:
      error_msg_ << "文法错误" << endl;
      break;
    case ErrorType::REAL_TO_INT:
      error_msg_ << ",realnum can not be translated into int type" << endl;
      break;
    case ErrorType::DIVISION_ZERO:
      error_msg_ << ",division by zero" << endl;
      break;
    case ErrorType::UNDECLEARED_ID:
      error_msg_ << ",cannot get the value of undeclared id" << endl;
      break;
    case ErrorType::UNDECLARED_ID_ASSIGN:
      cout << "cannot assign value to undeclared id" << endl;
      break;
    default:
      error_msg_ << ",unknown error" << endl;
  }
}

void ErrorHandler::Output() { cout << error_msg_.str(); }

/**
 * LR项
 */
struct Item {
  using NonTerminal = string;
  int dot_idx_;
  int prod_id_;
  NonTerminal non_term_;
  vector<string> formula_;
  // 点的位置
  unordered_set<string> symbol_;  // 展望串
  Item() = default;
  Item(const Item& item) = default;
  Item(const NonTerminal& non_term, const vector<string>& formula, int dot_idx,
       int prod_id)
      : non_term_(non_term),
        formula_(formula),
        dot_idx_(dot_idx),
        prod_id_(prod_id) {}

  bool operator==(const Item& rhs) const {
    return dot_idx_ == rhs.dot_idx_ && non_term_ == rhs.non_term_ &&
           formula_ == rhs.formula_ && symbol_ == rhs.symbol_ &&
           prod_id_ == rhs.prod_id_;
  }
};

namespace std {
template <>
class hash<Item> {  // Item 的hash函数
 public:
  size_t operator()(const Item& item) const {
    size_t seed = 0;
    for (const auto& right : item.formula_) {
      seed ^= hash<string>()(right) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }
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

// 词法单元
struct Token {
  string str_;
  double value_{0};
  int ln_{-1};
  string name_;
  TokenType type_;

  Token() = default;
  Token(const Token& token) = default;
  Token(const string& str, double value, TokenType type, int ln)
      : str_(str), value_(value), type_(type), ln_(ln) {}

  Token(const string& str, string name, TokenType type, int ln)
      : str_(str), name_(name), type_(type), ln_(ln) {}
};

/**
 * 产生式
 */
struct Production {
  Production() = default;
  Production(const Production& prod) = default;
  Production(const string& left, const vector<string>& rights, int id)
      : left_(left), rights_(rights), id_(id) {}

  string left_;
  vector<string> rights_;
  int id_;
};

/**
 * 计算表达式
 */
struct Expression {
  vector<Token> tokens_;
  TableItem item_;

  explicit Expression() = default;
  explicit Expression(const Expression& expr) = default;
  explicit Expression(const vector<Token>& tokens, const TableItem& item)
      : tokens_(tokens), item_(item) {}
};

class LRParser;
/**
 * LR(1) 预测表
 *
 */
class LRParseTable {
  friend class LRParser;  // 解析器友元

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

  friend class LRParseTable;  // 解析表友元

 public:
  explicit LRParser(const string& input, const NonTerminal& start_symbol)
      : input_(input),
        start_symbol_(start_symbol),
        table_(this),
        synax_rules_(synax_rules) {
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

  // Translation Schema
  void Translate();

 private:
  // 将输入转换成分词
  void SplitToken(const string& input, vector<Token>& tokens);
  // 递归函数计算First集合
  void CalculateFirst(const NonTerminal& left);
  // 递归将A->aB形式的A的follow集放入B的follow集中
  void InsertDependsFollow(
      const NonTerminal& left,
      unordered_map<NonTerminal, unordered_set<NonTerminal>> depends,
      unordered_set<NonTerminal> flag);

  // 计算闭包 (LR)
  void Closure(unordered_set<Item>& items);

  // 将单词转换成Token(TS)
  Token StrToToken(const string& str, int ln);

  // 变量
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
  unordered_map<NonTerminal, vector<Production>> grammer_;
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
  unordered_map<NonTerminal, Production> empty_grammer_;
  // LR(1) 语法分析表
  LRParseTable table_;

  // Translation schema
  vector<Token> tokens_;
  vector<Expression> exprs_;
  unordered_map<int, ProductionType> synax_rules_;
  unordered_map<string, TokenType> id_type_;
  unordered_map<string, double> id_value_;
};

void LRParser::GenerateGrammer(const string& raw_grammer) {
  stringstream input(raw_grammer);
  string line;

  // 放置增广文法
  Production aug_prod = Production(start_symbol_ + "'", {start_symbol_}, 0);
  grammer_[start_symbol_ + "'"].emplace_back(aug_prod);
  idx_grammer_[0] = aug_prod;
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
          vector<string> rights;
          split(trim(right), rights);  // 将产生式划分
          Production prod{left, rights, gid};
          idx_grammer_[gid] = prod;
          ++gid;
          grammer_[left].emplace_back(prod);
          non_terminals_.emplace(left);
          right.clear();
          continue;
        } else {
          right.push_back(c);
        }
      }
    }
    if (!right.empty()) {
      vector<string> rights;
      split(trim(right), rights);  // 将产生式划分
      Production prod{left, rights, gid};
      idx_grammer_[gid] = prod;
      ++gid;
      grammer_[left].emplace_back(prod);
      non_terminals_.emplace(left);
    }
  }
  // 非终结符
  terminals_ = terminates;

  // for (const auto& g : grammer_) {
  //   for (const auto& p : g.second) {
  //     cout << p.id_ << " " << p.left_ << "->";
  //     for (const auto& r : p.rights_) {
  //       cout << r << " ";
  //     }
  //     cout << endl;
  //   }
  // }
}

void LRParser::CalculateFirst(const NonTerminal& left) {
  if (first_.find(left) !=
      first_.end()) {  // 如果已经有First集合那么说明已经计算过
    return;
  }
  const auto& prods = grammer_[left];  // 产生式左部对应的所有产生式
  for (const auto& prod : prods) {     // 遍历产生式
    for (const auto& right : prod.rights_) {  // 遍历所有右部符号
      if (terminates.find(right) !=
          terminates.end()) {  // 如果起始是终结符，加入first集合直接结束
        first_[left].emplace(right);
        if (right == "E") {  // 插入到可空的表达式中
          empty_grammer_[left] = prod;
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
          empty_grammer_[left] = prod;
        }
      }
      if (!may_empty) {
        break;
      }
    }
  }
}

void LRParser::GenerateFist() {
  // 迭代所有产生式, 计算其左部First
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
    const auto& prods = grammer.second;
    for (const auto& prod : prods) {
      const auto& rights = prod.rights_;
      // 计算所有产生式左部和右部结尾的依赖关系
      for (auto it = rights.rbegin(); it != rights.rend();
           it++) {  // 反向迭代产生式右部
        if (terminates.find(*it) == terminates.end()) {
          if (*it != left) {  // 自身不能相等
            depends[*it].emplace(left);
          }
          if (empty_grammer_.find(*it) == empty_grammer_.end()) {
            // 如果可能为空还需要考虑 A->aBb 且b为空的情况，
            // A的所有follow需要添加到B中
            break;
          }
        } else {
          break;
        }
      }
      // 将S->AB 形式的产生式中的B的first集放入A的follow集中
      for (int i = 0; i < rights.size(); i++) {
        if (terminates.find(rights[i]) == terminates.end()) {
          for (int j = i + 1; j < rights.size(); j++) {
            bool may_empty = false;
            if (terminates.find(rights[j]) != terminates.end() &&
                rights[j] != "E") {  // 终结符非空直接进入follow集
              follow_[rights[i]].emplace(rights[j]);
              break;
            }
            // B的first集合
            for (const auto& f : first_[rights[j]]) {
              if (f == "E") {
                may_empty = true;
              } else {
                follow_[rights[i]].emplace(f);
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

// 闭包计算
// 采用 A->alpha B.beta
void LRParser::Closure(unordered_set<Item>& items) {
  queue<Item> que;
  for (const auto& item : items) {  // 通过BFS的形式计算闭包
    que.emplace(item);
  }
  while (!que.empty()) {
    auto& item = que.front();
    if (item.dot_idx_ != -1) {
      // 解析产生式右部
      // 分析.后的符号是否为非终结符
      string B;
      B = item.formula_[item.dot_idx_];
      // 如果B是非终结符
      if (non_terminals_.find(B) != non_terminals_.end()) {
        for (const auto& grammer : grammer_[B]) {
          Item new_item{B, grammer.rights_, 0, grammer.id_};
          if (grammer.rights_[0] == "E") {
            new_item.dot_idx_ = -1;
          }
          if (item.dot_idx_ + 1 == item.formula_.size()) {  // beta 为空
            new_item.symbol_ = item.symbol_;
          } else {  // 判断first(beta)
            string beta = item.formula_[item.dot_idx_ + 1];
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
  Item item{start_symbol_ + "'", {start_symbol_}, 0, 0};
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
        const auto& symbols = it.formula_;
        if (it.dot_idx_ < symbols.size() && symbols[it.dot_idx_] == term) {
          int new_dot_idx =
              it.dot_idx_ + 1 == symbols.size() ? -1 : it.dot_idx_ + 1;
          Item new_item{it.non_term_, it.formula_, new_dot_idx, it.prod_id_};
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
          action.sid_ = item.prod_id_;
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
 * 将输入str转换成token函数
 */
Token LRParser::StrToToken(const string& str, int ln) {
  if (terminals_.find(str) != terminals_.end()) {  // 终结符
    return Token(str, 0.0, TokenType::Terminal, ln);
  }
  // 读入数字
  if (isdigit(str[0])) {
    vector<double> eval_res;
    int dot_index = str.length();
    for (int i = 0; i < str.length(); ++i) {
      if (str[i] == '.') {
        dot_index = i;
      }
    }
    double val = 0;
    for (int i = 0; i < dot_index; ++i) {
      val = val * 10.0 + str[i] - '0';
    }
    eval_res.emplace_back(val);
    val = 0;
    double pow = 0.1;
    if (dot_index != str.length()) {
      for (int i = dot_index + 1; i < str.length(); ++i) {
        val += (str[i] - '0') * pow;
        pow /= 10;
      }
      eval_res.emplace_back(val);
    }
    TokenType type = (eval_res.size() == 1) ? TokenType::INT : TokenType::REAL;
    if (type == TokenType::REAL) {
      return Token("REALNUM", eval_res[0] + eval_res[1], TokenType::REAL, ln);
    } else {
      return Token("INTNUM", eval_res[0], TokenType::INT, ln);
    }
  }
  return Token("ID", str, TokenType::ID, ln);
}

/**
 * 工具函数处理
 */
void LRParser::SplitToken(const string& input, vector<Token>& tokens) {
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
      tokens.emplace_back(StrToToken(word, ln));
    }
    ++ln;
  }
}

/**
 * 主过程函数
 */
void LRParser::Parse() {
  SplitToken(input_, tokens_);
  buffer_.emplace(Token("$", -1, TokenType::MARK, -1));
  for (auto it = tokens_.rbegin(); it != tokens_.rend(); ++it) {
    buffer_.emplace(*it);
  }
  state_.emplace(0);
  int cur_line = 1;
  int limit = 0;
  while (!buffer_.empty()) {
    auto& top_state = state_.top();
    auto& top_token = buffer_.top();
    auto& op = table_.table_[top_state][top_token.str_];
    // cout << top_token.str_ << " value: " << top_token.value_
    //      << " name: " << top_token.name_ << endl;

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
      vector<Token> tokens;
      reduce_action_.emplace_back(op);
      tokens.emplace_back("#", -1, TokenType::MARK, -1);
      for (int i = 0; i < idx_grammer_[op.sid_].rights_.size(); i++) {
        if (idx_grammer_[op.sid_].rights_[i] == "E") {
          continue;
        }
        tokens.emplace_back(stack_.top());
        stack_.pop();
        state_.pop();
      }
      exprs_.emplace_back(tokens, op);
      const auto& left = idx_grammer_[op.sid_].left_;
      stack_.emplace(left, 0.0, TokenType::ID, top_token.ln_);

      const auto& new_op = table_.table_[state_.top()][left];
      state_.emplace(new_op.sid_);
      if (new_op.state_ == TableItem::ActionState::ACCEPT) {
        break;
      }
    }

    if (op.state_ == TableItem::ActionState::ERROR) {
      if (top_token.str_ == "REALNUM") {
        top_token.str_ = "INTNUM";
        buffer_.pop();
        buffer_.emplace(top_token);
        handler_.Concat(cur_line, ErrorHandler::ErrorType::REAL_TO_INT, "");
      }
    }
    cur_line = top_token.ln_;
  }
}

void LRParser::Translate() {
  vector<string> token_ids;
  double eval_value = 0, op_value = 0;
  bool holding = false;
  int bool_res = 0;
  string compare_op;
  for (const auto& expr : exprs_) {
    const auto& tokens = expr.tokens_;
    switch (synax_rules_[expr.item_.sid_]) {
      case ProductionType::DECLARE: {  // 声明语句
        TokenType type;
        if (tokens.back().str_ == "int") {
          type = TokenType::INT;
        } else {
          type = TokenType::REAL;
        }
        token_ids.emplace_back(tokens[tokens.size() - 2].name_);
        id_type_.emplace(token_ids.back(), type);
        id_value_.emplace(token_ids.back(), tokens[tokens.size() - 4].value_);
        break;
      }
      case ProductionType::INSTANT: {  // 如果是立即数取值
        if (!holding) {
          eval_value = tokens.back().value_;
        } else {
          op_value = tokens.back().value_;
        }
        holding = true;
        break;
      }
      case ProductionType::IDVALUE: {  // 标识符参与计算
        if (id_value_.find(tokens.back().name_) ==
            id_value_.end()) {  // 没有对应的标识符
          handler_.Concat(tokens.back().ln_,
                          ErrorHandler::ErrorType::UNDECLEARED_ID,
                          tokens.back().name_);
        }
        if (!holding) {
          eval_value = id_value_[tokens.back().name_];
        } else {
          op_value = id_value_[tokens.back().name_];
        }
        holding = true;
        break;
      }
      case ProductionType::ASSIGN: {  // 赋值语句给对应的token value进行赋值
        if (id_value_.find(tokens.back().name_) ==
            id_value_.end()) {  // 没有对应的标识符
          handler_.Concat(tokens.back().ln_,
                          ErrorHandler::ErrorType::UNDECLARED_ID_ASSIGN,
                          tokens.back().name_);
        }
        if (bool_res != -1) {
          id_value_[tokens.back().name_] = eval_value;
          holding = false;
        }
        if (bool_res == 1) {
          bool_res = -1;
        } else if (bool_res == -1) {
          bool_res = 0;
        }
        eval_value = op_value = 0;
        holding = false;
        break;
      }
      case ProductionType::
          ARITHPRIME: {  // 加减运算符，根据expr中的符号判断进行的是哪种运算
        switch (tokens[3].str_[0]) {
          case '+':
            eval_value += op_value;
            break;
          case '-':
            eval_value -= op_value;
            break;
          case '*':
            eval_value *= op_value;
            break;
          case '/':
            if (op_value == 0) {
              handler_.Concat(tokens.back().ln_,
                              ErrorHandler::ErrorType::DIVISION_ZERO,
                              tokens.back().name_);
            } else {
              eval_value /= op_value;
            }
            break;
        }
        break;
      }
      case ProductionType::BOOL: {
        if (compare_op == "<=")
          bool_res = (eval_value <= op_value) ? 1 : -1;
        else if (compare_op == ">=")
          bool_res = (eval_value >= op_value) ? 1 : -1;
        else if (compare_op == ">")
          bool_res = (eval_value > op_value) ? 1 : -1;
        else if (compare_op == "<")
          bool_res = (eval_value < op_value) ? 1 : -1;
        else if (compare_op == "==")
          bool_res = (eval_value == op_value) ? 1 : -1;
        eval_value = 0;
        op_value = 0;
        holding = false;
        break;
      }
      case ProductionType::BOOLOP:
        compare_op = tokens[1].str_;
      default:
        break;
    }
  }
  if (handler_.Empty()) {
    for (auto id : token_ids) {
      cout << id << ": " << id_value_[id] << endl;
    }
  } else {
    handler_.Output();
  }
}

void Analysis() {
  string prog;
  read_prog(prog);
  /* 骚年们 请开始你们的表演 */
  /********* Begin *********/
  LRParser lr_parser(prog, "program");

  lr_parser.Parse();
  lr_parser.Translate();
  /********* End *********/
}