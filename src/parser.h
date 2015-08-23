#ifndef __SE_SUSI_JAVASCRIPT_PARSER_H
#define __SE_SUSI_JAVASCRIPT_PARSER_H

#include <iostream>
#include <stdexcept>
#include <vector>

#include <unicode/uchar.h>

static const uint64_t AST_OP = 1L << 62;

constexpr uint64_t op(const char c)
{
  constexpr const char chars[] =
      "!%&()*+,-./:;<=>?[]^abcdefghiklmnoprstuvwxy{|}~";
  for (auto *i = chars; *i; ++i) {
    if (c == *i) return i - chars + 1;
  }
  return 0;
  //  throw std::logic_error("Invalid character in op");
}

constexpr uint64_t op(const char *name)
{
  if (!*name) return 0;
  auto result = op(*name);
  if (!result) return 0;
  for (int i = 1; i <= 10; ++i) {
    if (name[i] == 0) return AST_OP | result;
    auto c = op(name[i]);
    if (!c) return 0;
    result = (result << 6) | c;
  }
  return 0; // throw std::logic_error("String to long");
}

std::string op2str(uint64_t op)
{
  if (!(op & AST_OP)) return "";
  op &= AST_OP - 1;
  std::string result;
  const char chars[] = "!%&()*+,-./:;<=>?[]^abcdefghiklmnoprstuvwxy{|}~";
  for (; op; op >>= 6) {
    result += chars[(op & 63) - 1];
  }
  return {result.rbegin(), result.rend()};
}

using code_point_t = uint16_t;

struct js_parser_t {
  std::vector<code_point_t> buffer;

  using InputIt = decltype(buffer.begin());
  InputIt first, last;

  template <typename It>
  js_parser_t(It f, It l)
      : buffer(f, l), first(buffer.begin()), last(buffer.end())
  {
    ast.emplace_back(0);
    cur().first = first;
  }

  enum AST_TYPE {
    AST_NULL       = 1,
    AST_TRUE       = 2,
    AST_FALSE      = 3,
    AST_NUMERIC    = 4,
    AST_STRING     = 8,
    AST_IDENTIFIER = 16,
    AST_EMPTY      = 32,
    AST_LABEL      = 64
  };

  struct type_t {

    uint64_t value;

    constexpr type_t(const char *str) : value(op(str)) {}
    constexpr type_t(uint64_t value) : value(value) {}

    std::string string() const
    {
      if (value & AST_OP) return op2str(value);
      switch (value) {
      case AST_NULL: return "null";
      case AST_TRUE: return "true";
      case AST_FALSE: return "false";
      case AST_NUMERIC: return "number";
      case AST_STRING: return "string";
      default: return "";
      }
    }
  };

  // struct ast_t {
  //   int op = 0;
  //   std::vector<ast_t> children;
  //   ast_t *parent = nullptr;
  //   InputIt first, last;
  // };

  struct ast_t {
    uint64_t op = 0;
    int size = 1, parent = 0; // parent relative
    InputIt first, last;
    ast_t(uint64_t op) : op(op) {}
  };

  std::vector<ast_t> ast;
  int ast_cur = 0;

  bool done() const { return first == last; }

  ast_t &cur() { return ast[ast_cur]; }

  ast_t &parent() { return ast[ast_cur + cur().parent]; }

  void dump_ast()
  {
    std::cout << "--- ast ---" << std::endl;

    for (const auto &node : ast) {
      std::string source = {node.first, node.first + node.size};
      if (&node == &ast.front() + ast_cur) {
        std::printf("-->");
      }
      else
        std::printf("   ");
      std::printf("op: %llx (%s), size: %d, parent: %d, \"%s\"\n",
                  (node.op & (AST_OP - 1)),
                  js_parser_t::type_t(node.op).string().data(), node.size,
                  node.parent, source.data());
    }
    std::cout << "cur: " << ast_cur << std::endl;
    std::cout << "--- ~ast ---" << std::endl;
  }

  void add(ast_t node)
  {
    log("add: " + type_t(node.op).string() + ", cur: " +
        type_t(cur().op).string());
    node.parent = -cur().size;
    node.first = first;
    ast.push_back(node);
    ast_cur = ast.size() - 1;
    dump_ast();
  }

  void insert(ast_t node)
  {
    log("insert: " + type_t(node.op).string());

    ast_t *last_child = nullptr;
    for (int i = 1; i < cur().size;) {
      last_child = (&cur() + i);
      i += last_child->size;
    }

    node.parent        = last_child->parent;
    node.first         = last_child->first;
    node.size          = 1 + last_child->size;
    last_child->parent = -1;
    cur().size -= last_child->size;
    ast_cur = (last_child - &ast.front());
    ast.insert(ast.begin() + ast_cur, node);
    dump_ast();
  }

  bool commit()
  {
    log("commit: " + type_t(cur().op).string());
    log(std::string{cur().first, first});
    parent().size += cur().size;
    cur().last = first;
    ast_cur += cur().parent;
    cur().last = first;
    dump_ast();
    return true;
  }

  bool rollback()
  {
    log("rollback: " + type_t(cur().op).string());
    ast_t node = cur();
    ast.erase(ast.begin() + ast_cur, ast.begin() + ast_cur + node.size);
    ast_cur += node.parent;
    dump_ast();
    return false;
  }

  void log(const std::string &str) { std::cout << str << std::endl; }
  bool error(const char *str)
  {
    int offset = std::distance(buffer.begin(), first);
    throw std::runtime_error(std::to_string(offset) + ": " + str);
  }

  bool backtrack(InputIt it)
  {
    first = it;
    return false;
  }

  bool no_line_terminator()
  {
    parse_whitespace();
    return parse_line_terminator();
  }

  bool match() { return false; }

  bool match(code_point_t x)
  {
    parse_whitespace();
    if (first == last || *first != x) return false;
    ++first;
    return true;
  }

  bool match(char x)
  {
    parse_whitespace();
    if (first == last || *first != x) return false;
    ++first;
    return true;
  }

  bool match(const char *x)
  {
    parse_whitespace();
    for (auto s = first; *x; ++x, ++first) {
      if (first == last || *first != *x) return backtrack(s);
    }
    return true;
  }

  template <typename Arg, typename... Args>
  bool match(Arg &&arg, Args &&... args)
  {
    return match(std::forward<Arg>(arg)) || match(std::forward<Args>(args)...);
  }

  bool parse(const char *x)
  {
    auto _op = op(x);
    parse_whitespace();
    auto f = first;
    for (auto s = first; *x; ++x, ++first) {
      if (first == last || *first != *x) return backtrack(s);
    }
    add(_op);
    cur().first = f; // bug
    return true;
  }

  template <typename Arg, typename... Args>
  bool parse(Arg &&arg, Args &&... args)
  {
    return parse(std::forward<Arg>(arg)) || parse(std::forward<Args>(args)...);
  }

  bool parse_op(const char *x)
  {
    auto _op = op(x);
    parse_whitespace();
    auto f = first;
    for (auto s = first; *x; ++x, ++first) {
      if (first == last || *first != *x) return backtrack(s);
    }
    insert(_op);
    cur().first = f;
    return true;
  }

  template <typename Arg, typename... Args>
  bool parse_op(Arg &&arg, Args &&... args)
  {
    return parse_op(std::forward<Arg>(arg)) ||
           parse_op(std::forward<Args>(args)...);
  }

  // A.1 Lexical Grammar

  bool parse_source_character()
  {
    if (first == last) return false;
    ++first;
    return true;
  }

  // ...

  bool parse_whitespace()
  {
    if (first == last) return false;
    switch (*first) {
    case 0x0009: // <TAB>
    case 0x000B: // <VT>
    case 0x000C: // <FF>
    case 0x0020: // <SP>
    case 0x00A0: // <NBSP>
    case 0xFEFF: // <BOM>
      break;
    default: // <USP>
      switch (u_charType(*first)) {
      case U_SPACE_SEPARATOR: break;
      default: return false;
      }
    }
    ++first;
    return true;
  }

  bool parse_line_terminator()
  {
    if (first == last) return false;
    switch (*first) {
    case 0x000A: // <LF>
    case 0x000D: // <CR>
    case 0x2028: // <LS>
    case 0x2029: // <PS>
      ++first;
      return true;
    default: return false;
    }
  }

  // 7.6
  bool parse_identifier()
  {
    log("parse_identifier");
    add(AST_IDENTIFIER);
    if (match_reserved_word() || !match_identifier_name()) return rollback();
    return commit();
  }

  // 7.6
  bool match_identifier_name()
  {
    log("match_identifier_name");
    if (match_identifier_start()) {
      while (match_identifer_part())
        ;
      return true;
    }
    return false;
  }

  // 7.6
  bool match_identifier_start()
  {
    log("match_identifier_start");
    // if (match('\\')) {
    //   if (!parse_unicode_escape_sequence())
    //     return error("<UnicodeEscapeSequence>");
    //   return true;
    // }
    return parse_unicode_letter() || match("$", "_");
  }

  // 7.6
  bool match_identifer_part()
  {
    log("match_identifer_part");
    return match_identifier_start() || parse_unicode_combining_mark() ||
           parse_unicode_digit() || parse_unicode_connector_puncturation()
        // || match(0x200C /* ZWNJ */) || match(0x200D /* ZWJ */)
        ;
  }

  bool parse_unicode_letter()
  {
    if (first == last) return false;
    switch (u_charType(*first)) {
    case U_UPPERCASE_LETTER:
    case U_LOWERCASE_LETTER:
    case U_TITLECASE_LETTER:
    case U_MODIFIER_LETTER:
    case U_OTHER_LETTER:
    case U_LETTER_NUMBER: ++first; return true;
    default: return false;
    }
  }

  bool parse_unicode_combining_mark()
  {
    if (first == last) return false;
    switch (u_charType(*first)) {
    case U_NON_SPACING_MARK:
    case U_COMBINING_SPACING_MARK: ++first; return true;
    default: return false;
    }
  }

  bool parse_unicode_digit()
  {
    if (first == last) return false;
    switch (u_charType(*first)) {
    case U_DECIMAL_DIGIT_NUMBER: ++first; return true;
    default: return false;
    }
  }

  bool parse_unicode_connector_puncturation()
  {
    if (first == last) return false;
    switch (u_charType(*first)) {
    case U_CONNECTOR_PUNCTUATION: ++first; return true;
    default: return false;
    }
  }

  bool match_reserved_word()
  {
    log("match_reserved_word");
    return match_keyword() || match_future_reserved_word() ||
           match("null", "true", "false");
  }

  // 7.6.1.1
  bool match_keyword()
  {
    log("match_keyword");
    return match("break", "case", "catch", "continue", "debugger", "default",
                 "delete", "do", "else", "finally", "for", "function", "if",
                 "in", "instanceof", "new", "return", "switch", "this", "throw",
                 "try", "typeof", "var", "void", "while", "with");
  }

  // 7.6.1.2
  bool match_future_reserved_word()
  {
    log("match_future_reserved_word");
    return match("class", "const", "enum", "export", "extends", "import",
                 "super") ||
           // strict mode
           match("implements", "interface", "let", "package", "private",
                 "protected", "public", "static", "yield");
  }

  bool parse_literal()
  {
    log("parse_literal");
    return parse_null_literal() || parse_boolean_literal() ||
           parse_numeric_literal() || parse_string_literal() ||
           parse_regular_expression_literal();
  }

  bool parse_null_literal()
  {
    log("parse_null_literal");
    if (!parse("null")) return false;
    return commit();
  }

  bool parse_boolean_literal()
  {
    log("parse_boolean_literal");
    if (!(parse("true", "false"))) return false;
    return commit();
  }

  bool parse_numeric_literal()
  {
    log("parse_numeric_literal");
    add(AST_NUMERIC);
    if (!(match_decimal_literal() || match_hex_integer_literal()))
      return rollback();
    auto s = first;
    if (match_identifier_start() || match_decimal_digit())
      return error("Unexpected token");
    backtrack(s);
    return commit();
  }

  bool match_decimal_literal()
  {
    if (match(".")) {
      if (!match_decimal_digits()) return error("<DecimalDigits>");
      match_exponent_part();
      return true;
    }
    if (match_decimal_integer_literal()) {
      if (match(".")) {
        match_decimal_digits();
      }
      match_exponent_part();
      return true;
    }
    return false;
  }

  bool match_decimal_integer_literal()
  {
    return match('0') || match_decimal_digits();
  }

  bool match_decimal_digits()
  {
    if (!match_decimal_digit()) return false;
    while (match_decimal_digit())
      ;
    return true;
  }

  bool match_decimal_digit()
  {
    return match('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
  }

  bool match_non_zero_digit()
  {
    return match('1') || match('2') || match('3') || match('4') || match('5') ||
           match('6') || match('7') || match('8') || match('9');
  }

  bool match_exponent_part()
  {
    if (!match_exponent_indicator()) return false;
    if (!match_signed_integer()) return error("<SignedInteger>");
    return true;
  }

  bool match_exponent_indicator() { return match('e') || match('E'); }

  bool match_signed_integer()
  {
    if (match('+') || match('-')) {
      if (!match_decimal_digits()) return error("<DecimalDigits>");
    }
    return match_decimal_digits();
  }

  bool match_hex_integer_literal()
  {
    if (!(match("0x") || match("0X"))) return false;
    if (!match_hex_digit()) return error("<HexDigit>");
    while (match_hex_digit())
      ;
    return true;
  }

  bool match_hex_digit()
  {
    return match('0') || match('1') || match('2') || match('3') || match('4') ||
           match('5') || match('6') || match('7') || match('8') || match('9') ||
           match('a') || match('b') || match('c') || match('d') || match('e') ||
           match('f') || match('A') || match('B') || match('C') || match('D') ||
           match('E') || match('F');
  }

  bool parse_string_literal()
  {
    if (match('"')) {
      parse_double_string_characters();
      if (!match('"')) return error("'\"'");
      return true;
    }
    if (match('\'')) {
      parse_single_string_characters();
      if (!match('\'')) return error("'''");
      return true;
    }
    return false;
  }

  bool parse_double_string_characters()
  {
    if (!parse_double_string_character()) return false;
    while (parse_double_string_character())
      ;
    return true;
  }

  bool parse_single_string_characters()
  {
    if (!parse_single_string_character()) return false;
    while (parse_single_string_character())
      ;
    return true;
  }

  bool parse_double_string_character() { return false; }

  bool parse_single_string_character() { return false; }

  bool parse_line_continuation() { return false; }

  bool parse_escape_sequence() { return false; }

  bool parse_character_escape_sequence()
  {
    return parse_single_escape_character() || parse_non_escape_character();
  }

  // ...

  bool parse_single_escape_character()
  {
    return match('\'') || match('"') || match('\\') || match('b') ||
           match('f') || match('n') || match('r') || match('t') || match('v');
  }

  bool parse_non_escape_character()
  {
    return !parse_escape_character() && !parse_line_terminator() &&
           parse_source_character();
  }

  bool parse_escape_character()
  {
    return parse_single_escape_character() || match_decimal_digit() ||
           match('x') || match('u');
  }

  bool parse_hex_escape_sequence()
  {
    if (!match('x')) return false;
    for (int i = 0; i < 2; ++i) {
      if (!match_hex_digit()) return error("<HexDigit>");
    }
    return true;
  }

  bool parse_unicode_escape_sequence()
  {
    if (!match('u')) return false;
    for (int i = 0; i < 4; ++i) {
      if (!match_hex_digit()) return error("<HexDigit>");
    }
    return true;
  }

  bool parse_regular_expression_literal()
  {
    if (!match('/')) return false;
    if (!parse_regular_expression_body())
      return error("<RegularExpressionBody>");
    if (!match('/')) return error("'/'");
    if (!parse_regular_expression_flags())
      return error("<RegularExpressionFlags");
    return true;
  }

  bool parse_regular_expression_body()
  {
    if (!parse_regular_expression_first_char()) return false;
    if (!parse_regular_expression_chars())
      return error("<RegularExpressionChars>");
    return true;
  }

  bool parse_regular_expression_chars()
  {
    while (parse_regular_expression_char())
      ;
    return true;
  }

  bool parse_regular_expression_first_char() { return false; }

  bool parse_regular_expression_char() { return false; }

  // ...

  bool parse_regular_expression_flags()
  {
    while (match_identifer_part())
      ;
    return true;
  }

  // A.3 Expressions

  bool parse_primary_expression()
  {
    log("parse_primary_expression");
    if (match('(')) {
      if (!parse_expression()) return error("<Expression>");
      if (!match(')')) return error("')'");
      return true;
    }
    return parse_this() || parse_literal() || parse_array_literal() ||
           parse_object_literal() || parse_identifier();
  }

  bool parse_this()
  {
    log("parse_this");
    if (!parse("this")) return false;
    return commit();
  }

  bool parse_array_literal()
  {
    log("parse_array_literal");
    if (!match('[')) return false;
    parse_element_list();
    parse_elision();
    if (!match(']')) return error("']'");
    return true;
  }

  bool parse_element_list()
  {
    log("parse_element_list");
    auto s = first;
    parse_elision();
    if (parse_assignment_expression()) {
      while (match(',')) {
        parse_elision();
        if (!parse_assignment_expression())
          return error("<AssignmentExpression>");
      }
      return true;
    }
    return backtrack(s); // this might not be necessary
  }

  bool parse_elision()
  {
    if (!match(',')) return false;
    while (match(','))
      ;
    return true;
  }

  bool parse_object_literal()
  {
    log("parse_object_literal");
    if (!match('{')) return false;
    parse_property_name_and_value_list();
    match(',');
    if (!match('}')) return error("'}'");
    return true;
  }

  bool parse_property_name_and_value_list()
  {
    if (!parse_property_assignment()) return false;
    while (match(',')) {
      if (!parse_property_assignment()) return error("<PropertyAssignment>");
    }
    return true;
  }

  bool parse_property_assignment()
  {
    if (match("get")) {
      if (!parse_property_name()) return error("<PropertyName>");
      if (!match('(')) return error("(");
      if (!match(')')) return error(")");
      if (!match('{')) return error("{");
      if (!parse_function_body()) return error("<FunctionBody>");
      if (!match('}')) return error("}");
      return true;
    }
    if (match("set")) {
      if (!parse_property_name()) return error("<PropertyName>");
      if (!match('(')) return error("(");
      if (!parse_property_set_parameter_list())
        return error("<PropertySetParameterList>");
      if (!match(')')) return error(")");
      if (!match('{')) return error("{");
      if (!parse_function_body()) return error("<FunctionBody>");
      if (!match('}')) return error("}");
      return true;
    }
    if (!parse_property_name()) return false;
    if (!match(':')) return error("':'");
    if (!parse_assignment_expression()) return error("<AssignmentExpression>");
    return true;
  }

  bool parse_property_name()
  {
    return match_identifier_name() || parse_string_literal() ||
           parse_numeric_literal();
  }

  bool parse_property_set_parameter_list() { return parse_identifier(); }

  bool parse_member_expression()
  {
    log("parse_member_expression");
    if (!(parse_primary_expression() || parse_function_expression()))
      return false;
    while (true) {
      if (match('[')) {
        if (!parse_expression()) return error("<Expression>");
        if (!match(']')) return error("']'");
      }
      else if (match('.')) {
        if (!match_identifier_name()) return error("<IdentifierName>");
      }
      else
        break;
    }
    return true;
  }

  bool parse_new_expression()
  {
    log("parse_new_expression");
    return parse_member_expression();
    // new new_expression
  }

  bool parse_call_expression()
  {
    log("parse_call_expression");
    if (!parse_member_expression()) return false;
    if (!parse_arguments()) return error("<Arguments>");

    while (true) {
      if (parse_arguments())
        ;
      else if (match('[')) {
        if (!parse_expression()) return error("<Expression>");
        if (!match(']')) return error("']'");
      }
      else if (match('.')) {
        if (!match_identifier_name()) return error("<IdentifierName>");
      }
      else
        break;
    }
    return true;
  }

  bool parse_arguments()
  {
    log("parse_arguments");
    if (!match('(')) return false;
    parse_argument_list();
    if (!match(')')) return error("')'");
    return true;
  }

  bool parse_argument_list()
  {
    log("parse_argument_list");
    if (!parse_assignment_expression()) return false;
    while (match(',')) {
      if (!parse_assignment_expression())
        return error("<AssignmentExpression>");
    }
    return true;
  }

  bool parse_left_hand_side_expression()
  {
    log("parse_left_hand_side_expression");
    return parse_new_expression() || parse_call_expression();
  }

  bool parse_postfix_expression()
  {
    log("parse_postfix_expression");
    if (!parse_left_hand_side_expression()) return false;
    if (no_line_terminator()) {
      match("++") || match("--");
    }
    return true;
  }

  bool parse_unary_expression()
  {
    log("parse_unary_expression");
    if (parse_postfix_expression()) return true;
    if (parse_op("delete", "void", "typeof", "++", "--", "+", "-", "~", "!")) {
      if (!parse_unary_expression()) return error("<UnaryExpression>");
      return commit();
    }
    return false;
  }

  bool parse_multiplicative_expression()
  {
    log("parse_multiplicative_expression");
    if (!parse_unary_expression()) return false;
    while (parse_op("*", "/", "%")) {
      if (!parse_unary_expression()) return error("<UnaryExpression>");
      commit();
    }
    return true;
  }

  bool parse_additive_expression()
  {
    log("parse_additive_expression");
    if (!parse_multiplicative_expression()) return false;
    while (parse_op("+", "-")) {
      if (!parse_multiplicative_expression())
        return error("<MultiplicativeExpression>");
      commit();
    }
    return true;
  }

  bool parse_shift_expression()
  {
    log("parse_shift_expression");
    if (!parse_additive_expression()) return false;
    while (parse_op("<<", ">>", ">>>")) {
      if (!parse_additive_expression()) return error("<AdditiveExpression>");
      commit();
    }
    return true;
  }

  bool parse_relational_expression()
  {
    log("parse_relational_expression");
    if (!parse_shift_expression()) return false;
    while (parse_op("<", ">", "<=", ">=", "instanceof", "in")) {
      if (!parse_shift_expression()) return error("<ShiftExpression>");
      commit();
    }
    return true;
  }

  bool parse_relational_expression_no_in()
  {
    log("parse_relational_expression_no_in");
    if (!parse_shift_expression()) return false;
    while (parse_op("<", ">", "<=", ">=", "instanceof")) {
      if (!parse_shift_expression()) return error("<ShiftExpression>");
      commit();
    }
    return true;
  }

  bool parse_equality_expression()
  {
    log("parse_equality_expression");
    if (!parse_relational_expression()) return false;
    while (parse_op("==", "!=", "===", "!==")) {
      if (!parse_relational_expression())
        return error("<RelationalExpression>");
      commit();
    }
    return true;
  }

  bool parse_equality_expression_no_in()
  {
    log("parse_equality_expression_no_in");
    if (!parse_relational_expression_no_in()) return false;
    while (parse_op("==", "!=", "===", "!==")) {
      if (!parse_relational_expression_no_in())
        return error("<RelationalExpressionNoIn>");
      commit();
    }
    return true;
  }

  bool parse_bitwise_and_expression()
  {
    log("parse_bitwise_and_expression");
    if (!parse_equality_expression()) return false;
    while (parse_op("&")) {
      if (!parse_equality_expression()) return error("<EqualityExpression>");
      commit();
    }
    return true;
  }

  bool parse_bitwise_and_expression_no_in()
  {
    log("parse_bitwise_and_expression_no_in");
    if (!parse_equality_expression_no_in()) return false;
    while (parse_op("&")) {
      if (!parse_equality_expression_no_in())
        return error("<EqualityExpressionNoIn>");
      commit();
    }
    return true;
  }

  bool parse_bitwise_xor_expression()
  {
    log("parse_bitwise_xor_expression");
    if (!parse_bitwise_and_expression()) return false;
    while (parse_op("^")) {
      if (!parse_bitwise_and_expression())
        return error("<BitwiseANDExpression>");
      commit();
    }
    return true;
  }

  bool parse_bitwise_xor_expression_no_in()
  {
    log("parse_bitwise_xor_expression_no_in");
    if (!parse_bitwise_and_expression_no_in()) return false;
    while (parse_op("^")) {
      if (!parse_bitwise_and_expression_no_in())
        return error("<BitwiseANDExpressionNoIn>");
      commit();
    }
    return true;
  }

  bool parse_bitwise_or_expression()
  {
    log("parse_bitwise_or_expression");
    if (!parse_bitwise_xor_expression()) return false;
    while (parse_op("|")) {
      if (!parse_bitwise_xor_expression())
        return error("<BitwiseXORExpression>");
      commit();
    }
    return true;
  }

  bool parse_bitwise_or_expression_no_in()
  {
    log("parse_bitwise_or_expression_no_in");
    if (!parse_bitwise_xor_expression_no_in()) return false;
    while (parse_op("|")) {
      if (!parse_bitwise_xor_expression_no_in())
        return error("<BitwiseXORExpressionNoIn>");
      commit();
    }
    return true;
  }

  bool parse_logical_and_expression()
  {
    log("parse_logical_and_expression");
    if (!parse_bitwise_or_expression()) return false;
    while (parse_op("&&")) {
      if (!parse_bitwise_or_expression()) return error("<BitwiseORExpression>");
      commit();
    }
    return true;
  }

  bool parse_logical_and_expression_no_in()
  {
    log("parse_logical_and_expression_no_in");
    if (!parse_bitwise_or_expression_no_in()) return false;
    while (parse_op("&&")) {
      if (!parse_bitwise_or_expression_no_in())
        return error("<BitwiseORExpressionNoIn>");
      commit();
    }
    return true;
  }

  bool parse_logical_or_expression()
  {
    log("parse_logical_or_expression");
    if (!parse_logical_and_expression()) return false;
    while (parse_op("||")) {
      if (!parse_logical_and_expression())
        return error("<LogicalANDExpression>");
      commit();
    }
    return true;
  }

  bool parse_logical_or_expression_no_in()
  {
    log("parse_logical_or_expression_no_in");
    if (!parse_logical_and_expression_no_in()) return false;
    while (parse_op("||")) {
      if (!parse_logical_and_expression_no_in())
        return error("<LogicalANDExpressionNoIn>");
      commit();
    }
    return true;
  }
  bool parse_conditional_expression()
  {
    log("parse_conditional_expression");
    if (!parse_logical_or_expression()) return false;
    if (match('?')) {
      if (!parse_assignment_expression())
        return error("<AssignmentExpression>");
      if (!match(":")) return error("':'");
      if (!parse_assignment_expression())
        return error("<AssignmentExpression>");
    }
    return true;
  }

  bool parse_conditional_expression_no_in()
  {
    log("parse_conditional_expression_no_in");
    if (!parse_logical_or_expression_no_in()) return false;
    if (match('?')) {
      if (!parse_assignment_expression())
        return error("<AssignmentExpression>");
      if (!match(":")) return error("':'");
      if (!parse_assignment_expression_no_in())
        return error("<AssignmentExpressionNoIn>");
    }
    return true;
  }

  bool parse_assignment_expression()
  {
    log("parse_assignment_expression");
    if (!parse_conditional_expression()) return false;
    // TODO check typeof lhs == LeftHandSideExpression
    if (match('=') || parse_assignment_operator()) {
      if (!parse_assignment_expression())
        return error("<AssignmentExpression>");
    }
    return true;
  }

  bool parse_assignment_expression_no_in()
  {
    log("parse_assignment_expression_no_in");
    if (!parse_conditional_expression_no_in()) return false;
    // TODO check typeof lhs == LeftHandSideExpression
    if (match('=') || parse_assignment_operator()) {
      if (!parse_assignment_expression_no_in())
        return error("<AssignmentExpressionNoIn>");
    }
    return true;
  }

  bool parse_assignment_operator()
  {
    log("parse_assignment_operator");
    return match("*=") || match("/=") || match("%=") || match("+=") ||
           match("-=") || match("<<=") || match(">>=") || match(">>>=") ||
           match("&=") || match("^=") || match("|=");
  }

  bool parse_expression()
  {
    log("parse_expression");
    if (!parse_assignment_expression()) return false;
    while (parse_op(",")) {
      if (!parse_assignment_expression())
        return error("<AssignmentExpression>");
      commit();
    }
    return true;
  }

  bool parse_expression_no_in()
  {
    log("parse_expression_no_in");
    if (!parse_assignment_expression_no_in()) return false;
    while (parse_op(",")) {
      if (!parse_assignment_expression_no_in())
        return error("<AssignmentExpressionNoIn>");
      commit();
    }
    return true;
  }

  // A.4 Statements

  bool parse_statement()
  {
    log("parse_statement");
    return parse_block() || parse_variable_statement() ||
           parse_empty_statement() || // parse_expression_statement() ||
           parse_if_statement() || parse_iteration_statement() ||
           parse_continue_statement() || parse_break_statement() ||
           parse_return_statement() || parse_with_statement() ||
           parse_labelled_statement() || parse_switch_statement() ||
           parse_throw_statement() || parse_try_statement() ||
           parse_debugger_statement();
  }

  bool parse_block()
  {
    log("parse_block");
    auto s = first;
    if (!match('{')) return false;
    parse_statement_list();
    if (!match('}')) return backtrack(s);
    return true;
  }

  bool parse_statement_list()
  {
    log("parse_statement_list");
    if (!parse_statement()) return false;
    while (parse_statement())
      ;
    return true;
  }

  bool parse_variable_statement()
  {
    log("parse_variable_statement");
    auto s = first;
    if (!match("var")) return false;
    if (!parse_variable_declaration_list()) return backtrack(s);
    return true;
  }

  bool parse_variable_declaration_list()
  {
    log("parse_variable_declaration_list");
    if (!parse_variable_declaration()) return false;
    while (parse_variable_declaration())
      ;
    return true;
  }

  bool parse_variable_declaration_list_no_in()
  {
    log("parse_variable_declaration_list_no_in");
    if (!parse_variable_declaration_no_in()) return false;
    while (parse_variable_declaration_no_in())
      ;
    return true;
  }

  bool parse_variable_declaration()
  {
    log("parse_variable_declaration");
    if (!parse_identifier()) return false;
    parse_initialiser();
    return true;
  }

  bool parse_variable_declaration_no_in()
  {
    log("parse_variable_declaration_no_in");
    if (!parse_identifier()) return false;
    parse_initialiser_no_in();
    return true;
  }

  bool parse_initialiser()
  {
    log("parse_initialiser");
    auto s = first;
    if (!match('=')) return false;
    if (!parse_assignment_expression()) return backtrack(s);
    return true;
  }

  bool parse_initialiser_no_in()
  {
    log("parse_initialiser_no_in");
    auto s = first;
    if (!match('=')) return false;
    if (!parse_assignment_expression_no_in()) return backtrack(s);
    return true;
  }

  bool parse_empty_statement()
  {
    log("parse_empty_statement");
    if (!match(';')) return false;
    add(AST_EMPTY);
    return commit();
  }

  bool parse_expression_statement()
  {
    log("parse_expression_statement");
    auto s = first;
    if (match('{') || match("function")) return backtrack(s);
    if (!parse_expression()) return false;
    if (!match(';')) return backtrack(s);
    return true;
  }

  bool parse_if_statement()
  {
    log("parse_if_statement");
    if (!parse("if")) return false;
    if (!match('(')) return error("'('");
    if (!parse_expression()) return error("<expression>");
    if (!match(')')) return error("')'");
    if (!parse_statement()) return error("<statement>");
    if (match("else") && !parse_statement()) return error("<statement>");
    return commit();
  }

  bool parse_iteration_statement()
  {
    log("parse_iteration_statement");
    if (parse("do")) {
      if (!parse_statement()) return error("<Statement>");
      if (!match("while")) return error("'while'");
      if (!match('(')) return error("'('");
      if (!parse_expression()) return error("<Expression>");
      if (!match(')')) return error(")");
      if (!match(';')) return error(";");
      return commit();
    }
    else if (parse("while")) {
      if (!match('(')) return error("'('");
      if (!parse_expression()) return error("<Expression>");
      if (!match(')')) return error(")");
      if (!parse_statement()) return error("<Statement>");
      return commit();
    }
    else if (parse("for")) {
      if (!match('(')) return error("'('");
      parse_expression_no_in();
      if (match(';')) {
        parse_expression();
        if (!match(';')) return error(";");
        parse_expression();
      }
      else if (match("var") && parse_variable_declaration_list_no_in()) {
        if (!match(';')) return error(";");
        parse_expression();
        if (!match(';')) return error(";");
        parse_expression();
      }
      else if (parse_left_hand_side_expression()) {
        if (!match("in")) return error("'in'");
        parse_expression();
      }
      else if (match("var") && parse_variable_declaration_no_in()) {
        if (!match("in")) return error("'in'");
        parse_expression();
      }
      if (!match(')')) return error(")");
      if (!parse_statement()) return error("<Statement>");
      return commit();
    }
    return false;
  }

  bool parse_continue_statement()
  {
    log("parse_continue_statement");
    if (!parse("continue")) return false;
    if (no_line_terminator()) parse_identifier();
    if (!match(';')) return error(";");
    return commit();
  }

  bool parse_break_statement()
  {
    log("parse_break_statement");
    if (!parse("break")) return false;
    if (no_line_terminator()) parse_identifier();
    if (!match(';')) return error(";");
    return commit();
  }

  bool parse_return_statement()
  {
    log("parse_return_statement");
    if (!parse("return")) return false;
    if (no_line_terminator()) parse_expression();
    if (!match(';')) return error(";");
    return commit();
  }

  bool parse_with_statement()
  {
    log("parse_with_statement");
    if (!parse("with")) return false;
    if (!match('(')) return error("'('");
    if (!parse_expression()) return error("<Expression>");
    if (!match(')')) return error(")");
    if (!parse_statement()) return error("<Statement>");
    return commit();
  }

  bool parse_switch_statement()
  {
    log("parse_switch_statement");
    if (!parse("switch")) return false;
    if (!match('(')) return error("'('");
    if (!parse_expression()) return error("<Expression>");
    if (!match(')')) return error(")");
    if (!parse_case_block()) return error("<CaseBlock>");
    return commit();
  }

  bool parse_case_block()
  {
    log("parse_case_block");
    if (!match('{')) return false;
    parse_case_clauses();
    if (parse_default_clause()) {
      parse_case_clauses();
    }
    if (!match('}')) return error("'}'");
    return true;
  }

  bool parse_case_clauses()
  {
    log("parse_case_clauses");
    if (!parse_case_clause()) return false;
    while (parse_case_clause())
      ;
    return true;
  }

  bool parse_case_clause()
  {
    log("parse_case_clause");
    if (!parse("case")) return false;
    if (!parse_expression()) return error("<Expression>");
    if (!match(':')) return error("':'");
    parse_statement_list();
    return commit();
  }

  bool parse_default_clause()
  {
    log("parse_default_clause");
    if (!parse("default")) return false;
    if (!match(':')) return error("':'");
    parse_statement_list();
    return commit();
  }

  bool parse_labelled_statement()
  {
    log("parse_labelled_statement");
    add(AST_LABEL);
    if (!parse_identifier()) return rollback();
    if (!match(':')) return error("':'");
    if (!parse_statement()) return error("<Statement>");
    return commit();
  }

  bool parse_throw_statement()
  {
    log("parse_throw_statement");
    if (!parse("throw")) return false;
    if (no_line_terminator()) parse_expression();
    if (!match(';')) return error("';'");
    return commit();
  }

  bool parse_try_statement()
  {
    log("parse_try_statement");
    if (!parse("try")) return false;
    commit();
    if (!parse_block()) return error("<Block>");
    parse_catch();
    parse_finally();
    return commit();
  }

  bool parse_catch()
  {
    log("parse_catch");
    if (!parse("catch")) return false;
    if (!match('(')) return error("'('");
    if (!parse_identifier()) return error("<Identifier>");
    if (!match(')')) return error(")");
    if (!parse_block()) return error("<Block>");
    return commit();
  }

  bool parse_finally()
  {
    log("parse_finally");
    if (!parse("finally")) return false;
    if (!parse_block()) return error("<Block>");
    return commit();
  }

  bool parse_debugger_statement()
  {
    log("parse_debugger_statement");
    if (!parse("debugger")) return false;
    if (!match(';')) return error("';'");
    return commit();
  }

  // A.5 Functions and Programs

  bool parse_function_declaration()
  {
    if (!parse("function")) return false;
    parse_identifier();
    if (!match('(')) return error("'('");
    parse_formal_parameter_list();
    if (!match(')')) return error("')'");
    if (!match('{')) return error("'{'");
    if (!parse_function_body()) return error("<FunctionBody>");
    if (!match('}')) return error("'}'");
    return commit();
  }

  bool parse_function_expression()
  {
    if (!parse("function")) return false;
    parse_identifier();
    if (!match('(')) return error("'('");
    parse_formal_parameter_list();
    if (!match(')')) return error("')'");
    if (!match('{')) return error("'{'");
    if (!parse_function_body()) return error("<FunctionBody>");
    if (!match('}')) return error("'}'");
    return commit();
  }

  bool parse_formal_parameter_list()
  {
    if (parse_identifier()) {
      while (match(',')) {
        if (!parse_identifier()) return error("<Identifier>");
      }
      return true;
    }
    return false;
  }

  bool parse_function_body()
  {
    parse_source_elements();
    return true;
  }

  bool parse_program()
  {
    parse_source_elements();
    return true;
  }

  bool parse_source_elements()
  {
    if (parse_source_element()) {
      while (parse_source_element())
        ;
      return true;
    }
    return false;
  }

  bool parse_source_element()
  {
    return /*parse_statement() ||*/ parse_function_declaration();
  }
};

template <typename T> void print(const T &node, int indent = 0)
{
  for (int i = 0; i < indent; ++i) std::cout << "  ";
  std::string source = {node.first, node.last};
  std::printf("op: %llx (%s), size: %d, \"%s\"\n", (node.op & (AST_OP - 1)),
              js_parser_t::type_t(node.op).string().data(), node.size,
              source.data());

  for (int i = 1; i < node.size;) {
    const auto &child_node = *(&node + i);
    print(child_node, indent + 1);
    i += child_node.size;
  }
}

template <typename T> void print_js(const T &node)
{
  std::string source = {node.first, node.last};

  if (node.size == 1) {
    std::cout << source;
  }
  else {
    std::cout << "(";
    for (int i = 1; i < node.size;) {
      const auto &child_node = *(&node + i);
      print_js(child_node);
      i += child_node.size;
      if (i < node.size) std::cout << js_parser_t::type_t(node.op).string();
    }
    std::cout << ")";
  }
}

template <typename InputIt>
// require value_type(InputIt) == code_point_t
void eval(InputIt first, InputIt last)
{
  auto parser = js_parser_t{first, last};

  parser.parse_program();

  std::cout << "---" << std::endl;
  std::cout << "done? " << parser.done() << std::endl;
  print(parser.ast.front());

  for (auto node : parser.ast) {
    std::string source = {node.first, node.last};
    std::printf(
        "op: %llx (%s), size: %d, \"%s\" %ld-%ld\n", (node.op & (AST_OP - 1)),
        js_parser_t::type_t(node.op).string().data(), node.size, source.data(),
        node.first - parser.buffer.begin(), node.last - parser.buffer.begin());
  }

  print_js(parser.ast.front());
}

std::ostream &operator<<(std::ostream &out, const js_parser_t::type_t &value)
{
  return out << value.string();
}

#endif
