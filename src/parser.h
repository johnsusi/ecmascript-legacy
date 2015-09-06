#ifndef __SE_SUSI_JAVASCRIPT_PARSER_H
#define __SE_SUSI_JAVASCRIPT_PARSER_H

#include <functional>
#include <iostream>
#include <stdexcept>
#include <vector>

#include "lexer.h"

namespace ast {

#define TAGBODY(INDEX, X)                                                      \
  {                                                                            \
    static constexpr std::size_t index() { return INDEX; }                     \
    static constexpr const char *name() { return #X; }                         \
  }

#define TAG0(INDEX, X) struct X##_tag TAGBODY(INDEX, X)
#define TAG1(INDEX, X, Y) struct X##_tag : Y##_tag TAGBODY(INDEX, X)
#define TAGX(_1, _2, _3, NAME, ...) NAME
#define TAG(...) TAGX(__VA_ARGS__, TAG1, TAG0, _)(__VA_ARGS__)

TAG(0x01, literal);
TAG(0x02, null_literal, literal);
TAG(0x03, boolean_literal, literal);
TAG(0x04, numeric_literal, literal);
TAG(0x05, string_literal, literal);
TAG(0x06, identifier);

TAG(0x10, assignment_expression);
TAG(0x11, unary_expression);
TAG(0x12, binary_expression);

TAG(0x13, sequence_expression);
TAG(0x14, member_expression);
TAG(0x15, new_expression);
TAG(0x16, call_expression);
TAG(0x17, conditional_expression);

TAG(0x0100, statement);
TAG(0x0101, variable_statement, statement);
TAG(0x0102, empty_statement, statement);
TAG(0x0103, expression_statement, statement);
TAG(0x0104, if_statement, statement);
TAG(0x0105, do_while_statement, statement);
TAG(0x0106, while_statement, statement);
TAG(0x0107, for_statement, statement);
TAG(0x0108, for_in_statement, statement);
TAG(0x0109, continue_statement, statement);
TAG(0x010A, break_statement, statement);
TAG(0x010B, return_statement, statement);
TAG(0x010C, with_statement, statement);
TAG(0x010D, switch_statement, statement);
TAG(0x010E, labelled_statement, statement);
TAG(0x010F, throw_statement, statement);
TAG(0x0110, try_statement, statement);
TAG(0x0111, debugger_statement, statement);

TAG(0x0121, variable_declaration);

TAG(0x0201, program);
TAG(0x0202, function);
TAG(0x0203, function_expression);

struct Node {
  std::size_t type;
  std::size_t size;
  Lexer::Token token;
  Node(std::size_t type, std::size_t size = 1) : type(type), size(size) {}
};
}

template <typename T> constexpr std::size_t type() {
  return T::index();
}

constexpr const char *name(std::size_t index) {
  switch (index) {
  case ast::literal_tag::index(): return ast::literal_tag::name();
  case ast::null_literal_tag::index(): return ast::null_literal_tag::name();
  case ast::boolean_literal_tag::index():
    return ast::boolean_literal_tag::name();
  case ast::numeric_literal_tag::index():
    return ast::numeric_literal_tag::name();
  case ast::string_literal_tag::index(): return ast::string_literal_tag::name();
  case ast::identifier_tag::index(): return ast::identifier_tag::name();
  case ast::assignment_expression_tag::index():
    return ast::assignment_expression_tag::name();
  case ast::unary_expression_tag::index():
    return ast::unary_expression_tag::name();
  case ast::binary_expression_tag::index():
    return ast::binary_expression_tag::name();
  case ast::sequence_expression_tag::index():
    return ast::sequence_expression_tag::name();
  case ast::member_expression_tag::index():
    return ast::member_expression_tag::name();
  case ast::new_expression_tag::index(): return ast::new_expression_tag::name();
  case ast::call_expression_tag::index():
    return ast::call_expression_tag::name();
  case ast::conditional_expression_tag::index():
    return ast::conditional_expression_tag::name();

  case ast::statement_tag::index(): return ast::statement_tag::name();
  case ast::variable_statement_tag::index():
    return ast::variable_statement_tag::name();
  case ast::empty_statement_tag::index():
    return ast::empty_statement_tag::name();
  case ast::expression_statement_tag::index():
    return ast::expression_statement_tag::name();
  case ast::if_statement_tag::index(): return ast::if_statement_tag::name();
  case ast::do_while_statement_tag::index():
    return ast::do_while_statement_tag::name();
  case ast::while_statement_tag::index():
    return ast::while_statement_tag::name();
  case ast::for_statement_tag::index(): return ast::for_statement_tag::name();
  case ast::for_in_statement_tag::index():
    return ast::for_in_statement_tag::name();
  case ast::continue_statement_tag::index():
    return ast::continue_statement_tag::name();
  case ast::break_statement_tag::index():
    return ast::break_statement_tag::name();
  case ast::return_statement_tag::index():
    return ast::return_statement_tag::name();
  case ast::with_statement_tag::index(): return ast::with_statement_tag::name();
  case ast::switch_statement_tag::index():
    return ast::switch_statement_tag::name();
  case ast::labelled_statement_tag::index():
    return ast::labelled_statement_tag::name();
  case ast::throw_statement_tag::index():
    return ast::throw_statement_tag::name();
  case ast::try_statement_tag::index(): return ast::try_statement_tag::name();
  case ast::debugger_statement_tag::index():
    return ast::debugger_statement_tag::name();

  case ast::variable_declaration_tag::index():
    return ast::variable_declaration_tag::name();

  case ast::program_tag::index(): return ast::program_tag::name();
  case ast::function_tag::index(): return ast::function_tag::name();
  case ast::function_expression_tag::index():
    return ast::function_expression_tag::name();
  default: return "<unknown>";
  }
}

template <typename Visitor> void accept(std::size_t type, Visitor visitor) {
  switch (type) {
  case 1: visitor(ast::literal_tag{}); break;
  case ast::program_tag::index(): break;
  }
}

struct Parser {

  Lexer lexer;
  std::vector<ast::Node> ast;

  std::function<void(const char *)> trace = [](auto) {};
  std::function<void(const std::vector<ast::Node> &, std::size_t)> debug_ast =
      [](auto, auto) {};

  Parser(Lexer lexer) : lexer(lexer) {}
  template <typename... Args>
  Parser(Args &&... args)
      : lexer(std::forward<Args>(args)...) {
    lexer.set_trace_strategy([&](auto what) { trace(what); });
  }

  [[noreturn]] bool error(const char *message) {
    using namespace std::string_literals;
    auto lines = std::count_if(lexer.buffer.begin(), lexer.cur,
                               Lexer::is_line_terminator) +
                 1;
    throw std::runtime_error("Syntax error: "s + message + " at line " +
                             std::to_string(lines));
  }

  template <typename Strategy> void set_trace_strategy(Strategy strategy) {
    trace = strategy;
  }

  template <typename Strategy> void set_debug_ast_strategy(Strategy strategy) {
    debug_ast = strategy;
  }

  template <typename T> void add() {
    ast.emplace_back(type<T>(), 1);
    ast.back().token = lexer.token();
    debug_ast(ast, ast.size() - 1);
  }

  template <typename T>
  void insert(std::size_t offset, Lexer::Token token = {}) {

    auto pos    = ast.begin() + offset;
    auto node   = ast.emplace(pos, type<T>(), std::distance(pos, ast.end()) + 1);
    node->token = token;
    //    auto parent  = node - 1;
    //    parent->size = node->size + 1;
    debug_ast(ast, offset);
  }

  template <typename T, typename InputIt>
  void insert(std::size_t offset, InputIt first, InputIt last) {

    auto pos    = ast.begin() + offset;
    auto node   = ast.emplace(pos, type<T>(), std::distance(pos, ast.end()) + 1);
    node->token = {TokenType::EMPTY, first, last};
    //    auto parent  = node - 1;
    //    parent->size = node->size + 1;
    debug_ast(ast, offset);
  }

  void commit(std::size_t offset) {
    auto node  = ast.begin() + offset;
    node->size = std::distance(node, ast.end());
    debug_ast(ast, offset);
  }

  bool parse() const noexcept { return false; }
  template <typename Arg, typename... Args>
  bool parse(Arg &&arg, Args &&... args) {
    return lexer.match(std::forward<Arg>(arg)) ||
           parse(std::forward<Args>(args)...);
  }

  // bool parse_primary_expression() {
  //   trace("parse_primary_expression");
  //   if (lexer.parse("(")) {
  //     parse_expression();
  //     if (!lexer.parse(")")) return error("missing ) in parenthetical");
  //     return true;
  //   }
  //   if (lexer.parse("this")) {
  //     trace("parse_this");
  //     // add<this_tag>(ast);
  //     return true;
  //   }
  //   if (lexer.parse(TokenType::IDENTIFIER)) {
  //     trace("prase_identifier");
  //     // add<ast::identifier_tag>(ast);
  //     return true;
  //   }
  //   if (lexer.parse(TokenGroup::LITERAL)) {
  //     trace("parse_literal");
  //     add<ast::literal_tag>();
  //     return true;
  //   }
  //   trace("nope");
  //
  //   return parse_array_literal() || parse_object_literal();
  // }

  bool parse_primary_expression() {
    trace("parse_primary_expression");
    if (parse("(")) {
      if (!parse_expression()) return error("<Expression>");
      if (!parse(")")) return error("')'");
      return true;
    }
    return parse_this() || parse_literal() || parse_array_literal() ||
           parse_object_literal() || parse_identifier();
  }

  bool parse_this() {
    trace("parse_this");
    if (!parse("this")) return false;
    add<ast::literal_tag>();
    return true;
  }

  bool parse_literal() {
    trace("parse_literal");
    if (!parse(TokenGroup::LITERAL)) return false;
    add<ast::literal_tag>();
    return true;
  }

  bool parse_identifier() {
    if (!parse(TokenType::IDENTIFIER)) return false;
    add<ast::identifier_tag>();
    return true;
  }

  bool parse_array_literal() {
    trace("parse_array_literal");
    if (!parse("[")) return false;
    parse_element_list();
    parse_elision();
    if (!parse("]")) return error("']'");
    return true;
  }

  bool parse_element_list() {
    trace("parse_element_list");
    parse_elision();
    if (parse_assignment_expression()) {
      while (parse(",")) {
        parse_elision();
        if (!parse_assignment_expression())
          return error("<AssignmentExpression>");
      }
      return true;
    }
    return false;
    // return backtrack(s); // this might not be necessary
  }

  bool parse_elision() {
    if (!parse(",")) return false;
    while (parse(","))
      ;
    return true;
  }

  bool parse_object_literal() {
    trace("parse_object_literal");
    if (!parse("{")) return false;
    parse_property_name_and_value_list();
    parse(",");
    if (!parse("}")) return error("'}'");
    return true;
  }

  bool parse_property_name_and_value_list() {
    if (!parse_property_assignment()) return false;
    while (parse(",")) {
      if (!parse_property_assignment()) return error("<PropertyAssignment>");
    }
    return true;
  }

  bool parse_property_assignment() {
    if (parse("get")) {
      if (!lexer.lookahead(':')) {
        if (!parse_property_name()) return error("<PropertyName>");
        if (!parse("(")) return error("(");
        if (!parse(")")) return error(")");
        if (!parse("{")) return error("{");
        if (!parse_function_body()) return error("<FunctionBody>");
        if (!parse("}")) return error("}");
        return true;
      }
    } else if (parse("set")) {
      if (!lexer.lookahead(':')) {
        if (!parse_property_name()) return error("<PropertyName>");
        if (!parse("(")) return error("(");
        if (!parse_property_set_parameter_list())
          return error("<PropertySetParameterList>");
        if (!parse(")")) return error(")");
        if (!parse("{")) return error("{");
        if (!parse_function_body()) return error("<FunctionBody>");
        if (!parse("}")) return error("}");
        return true;
      }
    } else if (!parse_property_name())
      return false;
    if (!parse(":")) return error("':'");
    if (!parse_assignment_expression()) return error("<AssignmentExpression>");
    return true;
  }

  bool parse_property_name() {
    return parse(TokenType::IDENTIFIER, TokenType::STRING_LITERAL,
                 TokenType::NUMERIC_LITERAL);
  }

  bool parse_property_set_parameter_list() { return parse_identifier(); }

  bool parse_member_expression() {
    trace("parse_member_expression");
    auto offset = ast.size();
    if (parse("new")) {
      auto token = lexer.token();
      if (parse_member_expression() && parse_arguments()) {
        insert<ast::new_expression_tag>(offset);
      } else {
        lexer.rollback(token);
        return false;
      }
    } else if (!(parse_primary_expression() || parse_function_expression()))
      return false;

    while (true) {
      if (parse("[")) {
        if (!parse_expression()) return error("<Expression>");
        if (!parse("]")) return error("']'");
      } else if (parse(".")) {
        if (!parse_identifier()) return error("<IdentifierName>");
      } else
        break;
      insert<ast::member_expression_tag>(offset);
    }
    return true;
  }

  bool parse_new_expression() {
    trace("parse_new_expression");
    return parse_member_expression();
    // new new_expression
  }

  bool parse_call_expression() {
    trace("parse_call_expression");
    auto offset = ast.size();
    if (!parse_member_expression()) return false;
    if (!parse_arguments()) return error("<Arguments>");

    insert<ast::call_expression_tag>(offset);

    while (true) {
      if (parse_arguments())
        ;
      /*else if (parse('[')) {
        if (!parse_expression()) return error("<Expression>");
        if (!parse(']')) return error("']'");
      } */ else if (parse(".")) {
        if (!parse_identifier()) return error("<IdentifierName>");
      } else
        break;
    }
    return true;
  }

  bool parse_arguments() {
    trace("parse_arguments");
    if (!parse("(")) return false;
    parse_argument_list();
    if (!parse(")")) return error("')'");
    return true;
  }

  bool parse_argument_list() {
    trace("parse_argument_list");
    if (!parse_assignment_expression()) return false;
    while (parse(",")) {
      if (!parse_assignment_expression())
        return error("<AssignmentExpression>");
    }
    return true;
  }

  bool parse_left_hand_side_expression() {
    trace("parse_left_hand_side_expression");
    auto offset = ast.size();
    if (!parse_new_expression()) return false;
    while (true) {
      if (parse_arguments()) {
        // TODO hack
        ast[offset].type = type<ast::call_expression_tag>();
        ast[offset].size = ast.size() - offset;
      } else if (parse("[")) {
        if (!parse_expression()) return error("<Expression>");
        if (!parse("]")) return error("']'");
        insert<ast::member_expression_tag>(offset);
      } else if (parse(".")) {
        if (!parse_identifier()) return error("<IdentifierName>");
        insert<ast::member_expression_tag>(offset);
      } else
        break;
    }

    return true;
  }

  bool parse_postfix_expression() {
    trace("parse_postfix_expression");
    auto offset = ast.size();
    if (!parse_left_hand_side_expression()) return false;
    if (!parse(TokenType::LINE_TERMINATOR) && (parse("++") || parse("--"))) {
      insert<ast::unary_expression_tag>(offset, lexer.token());
    }
    return true;
  }

  bool parse_unary_expression() {
    trace("parse_unary_expression");
    auto offset = ast.size();
    if (parse_postfix_expression()) return true;
    if (parse("delete", "void", "typeof", "++", "--", "+", "-", "~", "!")) {
      auto token = lexer.token();
      if (!parse_unary_expression()) return error("<ast::unaryExpression>");
      insert<ast::unary_expression_tag>(offset, token);
      return true;
    }
    return false;
  }

  bool parse_multiplicative_expression() {
    trace("parse_multiplicative_expression");
    auto offset = ast.size();
    if (!parse_unary_expression()) return false;
    while (parse("*", "/", "%")) {
      auto token = lexer.token();
      if (!parse_unary_expression()) return error("<ast::unaryExpression>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_additive_expression() {
    trace("parse_additive_expression");
    auto offset = ast.size();
    if (!parse_multiplicative_expression()) return false;
    while (parse("+", "-")) {
      auto token = lexer.token();
      if (!parse_multiplicative_expression())
        return error("<MultiplicativeExpression>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_shift_expression() {
    trace("parse_shift_expression");
    auto offset = ast.size();
    if (!parse_additive_expression()) return false;
    while (parse("<<", ">>", ">>>")) {
      auto token = lexer.token();
      if (!parse_additive_expression()) return error("<AdditiveExpression>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_relational_expression() {
    trace("parse_relational_expression");
    auto offset = ast.size();
    if (!parse_shift_expression()) return false;
    while (parse("<", ">", "<=", ">=", "instanceof", "in")) {
      auto token = lexer.token();
      if (!parse_shift_expression()) return error("<ShiftExpression>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_relational_expression_no_in() {
    trace("parse_relational_expression_no_in");
    auto offset = ast.size();
    if (!parse_shift_expression()) return false;
    while (parse("<", ">", "<=", ">=", "instanceof")) {
      auto token = lexer.token();
      if (!parse_shift_expression()) return error("<ShiftExpression>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_equality_expression() {
    trace("parse_equality_expression");
    auto offset = ast.size();
    if (!parse_relational_expression()) return false;
    while (parse("==", "!=", "===", "!==")) {
      auto token = lexer.token();
      if (!parse_relational_expression())
        return error("<RelationalExpression>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_equality_expression_no_in() {
    trace("parse_equality_expression_no_in");
    auto offset = ast.size();
    if (!parse_relational_expression_no_in()) return false;
    while (parse("==", "!=", "===", "!==")) {
      auto token = lexer.token();
      if (!parse_relational_expression_no_in())
        return error("<RelationalExpressionNoIn>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_bitwise_and_expression() {
    trace("parse_bitwise_and_expression");
    auto offset = ast.size();
    if (!parse_equality_expression()) return false;
    while (parse("&")) {
      auto token = lexer.token();
      if (!parse_equality_expression()) return error("<EqualityExpression>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_bitwise_and_expression_no_in() {
    trace("parse_bitwise_and_expression_no_in");
    auto offset = ast.size();
    if (!parse_equality_expression_no_in()) return false;
    while (parse("&")) {
      auto token = lexer.token();
      if (!parse_equality_expression_no_in())
        return error("<EqualityExpressionNoIn>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_bitwise_xor_expression() {
    trace("parse_bitwise_xor_expression");
    auto offset = ast.size();
    if (!parse_bitwise_and_expression()) return false;
    while (parse("^")) {
      auto token = lexer.token();
      if (!parse_bitwise_and_expression())
        return error("<BitwiseANDExpression>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_bitwise_xor_expression_no_in() {
    trace("parse_bitwise_xor_expression_no_in");
    auto offset = ast.size();
    if (!parse_bitwise_and_expression_no_in()) return false;
    while (parse("^")) {
      auto token = lexer.token();
      if (!parse_bitwise_and_expression_no_in())
        return error("<BitwiseANDExpressionNoIn>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_bitwise_or_expression() {
    trace("parse_bitwise_or_expression");
    auto offset = ast.size();
    if (!parse_bitwise_xor_expression()) return false;
    while (parse("|")) {
      auto token = lexer.token();
      if (!parse_bitwise_xor_expression())
        return error("<BitwiseXORExpression>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_bitwise_or_expression_no_in() {
    trace("parse_bitwise_or_expression_no_in");
    auto offset = ast.size();
    if (!parse_bitwise_xor_expression_no_in()) return false;
    while (parse("|")) {
      auto token = lexer.token();
      if (!parse_bitwise_xor_expression_no_in())
        return error("<BitwiseXORExpressionNoIn>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_logical_and_expression() {
    trace("parse_logical_and_expression");
    auto offset = ast.size();
    if (!parse_bitwise_or_expression()) return false;
    while (parse("&&")) {
      auto token = lexer.token();
      if (!parse_bitwise_or_expression()) return error("<BitwiseORExpression>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_logical_and_expression_no_in() {
    trace("parse_logical_and_expression_no_in");
    auto offset = ast.size();
    if (!parse_bitwise_or_expression_no_in()) return false;
    while (parse("&&")) {
      auto token = lexer.token();
      if (!parse_bitwise_or_expression_no_in())
        return error("<BitwiseORExpressionNoIn>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_logical_or_expression() {
    trace("parse_logical_or_expression");
    auto offset = ast.size();
    if (!parse_logical_and_expression()) return false;
    while (parse("||")) {
      auto token = lexer.token();
      if (!parse_logical_and_expression())
        return error("<LogicalANDExpression>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_logical_or_expression_no_in() {
    trace("parse_logical_or_expression_no_in");
    auto offset = ast.size();
    if (!parse_logical_and_expression_no_in()) return false;
    while (parse("||")) {
      auto token = lexer.token();
      if (!parse_logical_and_expression_no_in())
        return error("<LogicalANDExpressionNoIn>");
      insert<ast::binary_expression_tag>(offset, token);
    }
    return true;
  }
  bool parse_conditional_expression() {
    trace("parse_conditional_expression");
    auto offset = ast.size();
    if (!parse_logical_or_expression()) return false;
    if (parse("?")) {
      if (!parse_assignment_expression())
        return error("<AssignmentExpression>");
      if (!parse(":")) return error("':'");
      if (!parse_assignment_expression())
        return error("<AssignmentExpression>");
      insert<ast::conditional_expression_tag>(offset);
    }
    return true;
  }

  bool parse_conditional_expression_no_in() {
    trace("parse_conditional_expression_no_in");
    auto offset = ast.size();
    if (!parse_logical_or_expression_no_in()) return false;
    if (parse("?")) {
      if (!parse_assignment_expression())
        return error("<AssignmentExpression>");
      if (!parse(":")) return error("':'");
      if (!parse_assignment_expression_no_in())
        return error("<AssignmentExpressionNoIn>");
      insert<ast::conditional_expression_tag>(offset);
    }
    return true;
  }

  bool parse_assignment_expression() {
    trace("parse_assignment_expression");
    auto offset = ast.size();
    if (!parse_conditional_expression()) return false;
    // TODO check typeof lhs == LeftHandSideExpression
    if (parse("=") || parse_assignment_operator()) {
      auto token = lexer.token();
      if (!parse_assignment_expression())
        return error("<AssignmentExpression>");
      insert<ast::assignment_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_assignment_expression_no_in() {
    trace("parse_assignment_expression_no_in");
    auto offset = ast.size();
    if (!parse_conditional_expression_no_in()) return false;
    // TODO check typeof lhs == LeftHandSideExpression
    if (parse("=") || parse_assignment_operator()) {
      auto token = lexer.token();
      if (!parse_assignment_expression_no_in())
        return error("<AssignmentExpressionNoIn>");
      insert<ast::assignment_expression_tag>(offset, token);
    }
    return true;
  }

  bool parse_assignment_operator() {
    trace("parse_assignment_operator");
    return parse("*=") || parse("/=") || parse("%=") || parse("+=") ||
           parse("-=") || parse("<<=") || parse(">>=") || parse(">>>=") ||
           parse("&=") || parse("^=") || parse("|=");
  }

  bool parse_expression() {
    trace("parse_expression");
    auto offset = ast.size();
    if (!parse_assignment_expression()) return false;
    if (parse(",")) {
      insert<ast::sequence_expression_tag>(offset, lexer.token());
      do {
        if (!parse_assignment_expression())
          return error("<AssignmentExpression>");
      } while (parse(","));
      auto node  = ast.begin() + offset;
      node->size = std::distance(node, ast.end());
    }
    return true;
  }

  bool parse_expression_no_in() {
    trace("parse_expression_no_in");
    auto offset = ast.size();
    if (!parse_assignment_expression_no_in()) return false;
    if (parse(",")) {
      insert<ast::sequence_expression_tag>(offset, lexer.token());
      do {
        if (!parse_assignment_expression_no_in())
          return error("<AssignmentExpressionNoIn>");
      } while (parse(","));
      auto node  = ast.begin() + offset;
      node->size = std::distance(node, ast.end());
    }
    return true;
  }

  // A.4

  bool parse_statement() {
    trace("parse_statement");
    return parse_block() || parse_variable_statement() ||
           parse_empty_statement() || parse_expression_statement() ||
           parse_if_statement() || parse_iteration_statement() ||
           parse_continue_statement() || parse_break_statement() ||
           parse_return_statement() || parse_with_statement() ||
           parse_labelled_statement() || parse_switch_statement() ||
           parse_throw_statement() || parse_try_statement() ||
           parse_debugger_statement();
  }

  bool parse_block() {
    trace("parse_block");
    if (!parse("{")) return false;
    parse_statement_list();
    if (!parse("}")) return error("missing } in compund statement");
    return true;
  }

  bool parse_statement_list() {
    trace("parse_statement_list");
    if (!parse_statement()) return false;
    while (parse_statement())
      ;
    return true;
  }

  bool parse_variable_statement() {
    trace("parse_variable_statement");
    if (!parse("var")) return false;
    auto offset = ast.size();
    add<ast::variable_statement_tag>();
    if (!parse_variable_declaration_list())
      return error("missing variable name");
    commit(offset);
    return true;
  }

  bool parse_variable_declaration_list() {
    trace("parse_variable_declaration_list");
    if (!parse_variable_declaration()) return false;
    while (parse(",")) {
      if (!parse_variable_declaration()) return error("missing variable name");
      ;
    }
    return true;
  }

  bool parse_variable_declaration_list_no_in() {
    trace("parse_variable_declaration_list_no_in");
    if (!parse_variable_declaration_no_in()) return false;
    while (parse_variable_declaration_no_in())
      ;
    return true;
  }

  bool parse_variable_declaration() {
    trace("parse_variable_declaration");
    auto offset = ast.size();
    if (!parse_identifier()) return false;
    parse_initialiser();
    insert<ast::variable_declaration_tag>(offset);
    return true;
  }

  bool parse_variable_declaration_no_in() {
    trace("parse_variable_declaration_no_in");
    auto offset = ast.size();
    if (!parse_identifier()) return false;
    parse_initialiser_no_in();
    insert<ast::variable_declaration_tag>(offset);
    return true;
  }

  bool parse_initialiser() {
    trace("parse_initialiser");
    if (!parse("=")) return false;
    if (!parse_assignment_expression()) return error("syntax error");
    return true;
  }

  bool parse_initialiser_no_in() {
    trace("parse_initialiser_no_in");
    if (!parse("=")) return false;
    if (!parse_assignment_expression_no_in()) return error("syntax error");
    return true;
  }

  bool parse_empty_statement() {
    trace("parse_empty_statement");
    auto offset = ast.size();
    if (!parse(";")) return false;
    insert<ast::empty_statement_tag>(offset);
    return true;
  }

  bool parse_expression_statement() {
    trace("parse_expression_statement");
    auto offset = ast.size();
    if (lexer.lookahead('{') || lexer.lookahead("function")) return false;
    if (!parse_expression()) return false;
    if (!parse(";")) return false;
    insert<ast::expression_statement_tag>(offset);
    return true;
  }

  bool parse_if_statement() {
    trace("parse_if_statement");
    auto offset = ast.size();
    if (!parse("if")) return false;
    auto token = lexer.token();
    if (!parse("(")) return error("'('");
    if (!parse_expression()) return error("<expression>");
    if (!parse(")")) return error("')'");
    if (!parse_statement()) return error("<statement>");
    if (parse("else") && !parse_statement()) return error("<statement>");
    insert<ast::if_statement_tag>(offset, token.first, lexer.token().last);
    return true;
  }

  bool parse_iteration_statement() {
    trace("parse_iteration_statement");
    auto offset = ast.size();
    if (parse("do")) {
      if (!parse_statement()) return error("<Statement>");
      if (!parse("while")) return error("'while'");
      if (!parse("(")) return error("'('");
      if (!parse_expression()) return error("<Expression>");
      if (!parse(")")) return error(")");
      if (!parse(";")) return error(";");
      return true;
    } else if (parse("while")) {
      if (!parse("(")) return error("'('");
      if (!parse_expression()) return error("<Expression>");
      if (!parse(")")) return error(")");
      if (!parse_statement()) return error("<Statement>");
      return true;
    } else if (parse("for")) {
      if (!parse("(")) return error("'('");

      if (parse("var")) {
        if (parse_variable_declaration_no_in()) {
          if (!lexer.lookahead("in")) {
            if (parse(",")) parse_variable_declaration_list_no_in();
            if (parse(";")) {
              parse_expression();
              if (parse(";")) {
                parse_expression();
                if (parse(")") && parse_statement()) {
                  insert<ast::for_statement_tag>(offset);
                }
              }
            }
          } else if (parse("in")) {
            if (parse_expression() && parse(")") && parse_statement()) {
              insert<ast::for_in_statement_tag>(offset);
              return true;
            }
          }
        }
      } else {
        parse_expression_no_in();
        if (parse(";")) {
          parse_expression();
          if (parse(";")) {
            parse_expression();
            if (parse(")") && parse_statement()) {
              insert<ast::for_statement_tag>(offset);
              return true;
            }
          }
        } else if (parse("in")) {
          if (parse_expression() && parse(")") && parse_statement()) {
            insert<ast::for_in_statement_tag>(offset);
            return true;
          }
        }
      }
      return error("");
    }
    return false;
  }

  bool parse_continue_statement() {
    trace("parse_continue_statement");
    auto offset = ast.size();
    if (!parse("continue")) return false;
    auto token = lexer.token();
    if (!parse(TokenType::LINE_TERMINATOR)) parse_identifier();
    if (!parse(";")) return error(";");
    insert<ast::continue_statement_tag>(offset, token);
    return true;
  }

  bool parse_break_statement() {
    trace("parse_break_statement");
    auto offset = ast.size();
    if (!parse("break")) return false;
    auto token = lexer.token();
    if (!parse(TokenType::LINE_TERMINATOR)) parse_identifier();
    if (!parse(";")) return error(";");
    insert<ast::break_statement_tag>(offset, token);
    return true;
  }

  bool parse_return_statement() {
    trace("parse_return_statement");
    auto offset = ast.size();
    if (!parse("return")) return false;
    auto token = lexer.token();
    if (!parse(TokenType::LINE_TERMINATOR)) parse_expression();
    if (!parse(";")) return error(";");
    insert<ast::return_statement_tag>(offset, token);
    return true;
  }

  bool parse_with_statement() {
    trace("parse_with_statement");
    auto offset = ast.size();
    if (!parse("with")) return false;
    if (!parse("(")) return error("'('");
    if (!parse_expression()) return error("<Expression>");
    if (!parse(")")) return error(")");
    if (!parse_statement()) return error("<Statement>");
    insert<ast::with_statement_tag>(offset);
    return true;
  }

  bool parse_switch_statement() {
    trace("parse_switch_statement");
    auto offset = ast.size();
    if (!parse("switch")) return false;
    if (!parse("(")) return error("'('");
    if (!parse_expression()) return error("<Expression>");
    if (!parse(")")) return error(")");
    if (!parse_case_block()) return error("<CaseBlock>");
    insert<ast::switch_statement_tag>(offset);
    return true;
  }

  bool parse_case_block() {
    trace("parse_case_block");
    if (!parse("{")) return false;
    parse_case_clauses();
    if (parse_default_clause()) { parse_case_clauses(); }
    if (!parse("}")) return error("'}'");
    return true;
  }

  bool parse_case_clauses() {
    trace("parse_case_clauses");
    if (!parse_case_clause()) return false;
    while (parse_case_clause())
      ;
    return true;
  }

  bool parse_case_clause() {
    trace("parse_case_clause");
    if (!parse("case")) return false;
    if (!parse_expression()) return error("<Expression>");
    if (!parse(":")) return error("':'");
    parse_statement_list();
    return true;
  }

  bool parse_default_clause() {
    trace("parse_default_clause");
    if (!parse("default")) return false;
    if (!parse(":")) return error("':'");
    parse_statement_list();
    return true;
  }

  bool parse_labelled_statement() {
    trace("parse_labelled_statement");
    auto offset = ast.size();
    if (!parse(TokenType::IDENTIFIER)) return false;
    if (!parse(":")) return error("':'");
    if (!parse_statement()) return error("<Statement>");
    insert<ast::labelled_statement_tag>(offset);
    return true;
  }

  bool parse_throw_statement() {
    trace("parse_throw_statement");
    auto offset = ast.size();
    if (!parse("throw")) return false;
    if (!parse(TokenType::LINE_TERMINATOR)) parse_expression();
    if (!parse(";")) return error("';'");
    insert<ast::throw_statement_tag>(offset);
    return true;
  }

  bool parse_try_statement() {
    trace("parse_try_statement");
    auto offset = ast.size();
    if (!parse("try")) return false;
    if (!parse_block()) return error("<Block>");
    parse_catch();
    parse_finally();
    insert<ast::try_statement_tag>(offset);
    return true;
  }

  bool parse_catch() {
    trace("parse_catch");
    if (!parse("catch")) return false;
    if (!parse("(")) return error("'('");
    if (!parse_identifier()) return error("<Identifier>");
    if (!parse(")")) return error(")");
    if (!parse_block()) return error("<Block>");
    return true;
  }

  bool parse_finally() {
    trace("parse_finally");
    if (!parse("finally")) return false;
    if (!parse_block()) return error("<Block>");
    return true;
  }

  bool parse_debugger_statement() {
    trace("parse_debugger_statement");
    auto offset = ast.size();
    if (!parse("debugger")) return false;
    if (!parse(";")) return error("';'");
    insert<ast::debugger_statement_tag>(offset);
    return true;
  }

  // A.5 Functions and Programs

  bool parse_function_declaration() {
    if (!parse("function")) return false;
    auto offset = ast.size();
    parse_identifier();
    if (!parse("(")) return error("'('");
    parse_formal_parameter_list();
    if (!parse(")")) return error("')'");
    if (!parse("{")) return error("'{'");
    if (!parse_function_body()) return error("<FunctionBody>");
    if (!parse("}")) return error("'}'");
    insert<ast::function_tag>(offset);
    return true;
  }

  bool parse_function_expression() {
    auto offset = ast.size();
    if (!parse("function")) return false;
    parse_identifier();
    if (!parse("(")) return error("'('");
    parse_formal_parameter_list();
    if (!parse(")")) return error("')'");
    if (!parse("{")) return error("'{'");
    if (!parse_function_body()) return error("<FunctionBody>");
    if (!parse("}")) return error("'}'");
    insert<ast::function_expression_tag>(offset);
    return true;
  }

  bool parse_formal_parameter_list() {
    if (parse_identifier()) {
      while (parse(",")) {
        if (!parse_identifier()) return error("<Identifier>");
      }
      return true;
    }
    return false;
  }

  bool parse_function_body() {
    parse_source_elements();
    return true;
  }

  // bool parse_statement() {
  //   return /*parse_block() || parse_variable_statement() ||
  //          parse_empty_statement() ||*/ parse_expression_statement() /*||
  //          parse_if_statement() || parse_iteration_statement() ||
  //          parse_continue_statement() || parse_break_statement() ||
  //          parse_return_statement() || parse_with_statement() ||
  //          parse_labelled_statement() || parse_switch_statement() ||
  //          parse_throw_statement() || parse_try_statement() ||
  //          parse_debugger_statement()*/;
  // }
  //
  // // 12.4
  // bool parse_expression_statement() {
  //   trace("parse_expression_statement");
  //   if (lexer.lookahead("{") || lexer.lookahead("function")) return false;
  //   auto offset = ast.size();
  //   if (parse_expression()) {
  //     parse(";");
  //     insert<ast::expression_statement_tag>(offset);
  //   }
  //   return true;
  // }

  // A.5

  // bool parse_function_declaration() { return false; }

  bool parse_program() {
    auto offset = ast.size();
    add<ast::program_tag>();
    parse_source_elements();
    commit(offset);
    return true;
  }

  bool parse_source_elements() {
    if (!parse_source_element()) return false;
    while (parse_source_element())
      ;
    return true;
  }

  bool parse_source_element() {
    return parse_statement() || parse_function_declaration();
  }
};

#endif
