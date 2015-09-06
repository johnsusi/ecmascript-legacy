#ifndef __SE_SUSI_JAVASCRIPT_LEXER_H
#define __SE_SUSI_JAVASCRIPT_LEXER_H

#include <algorithm>
#include <functional>
#include <string>
#include <typeinfo>
#include <typeindex>
#include <unordered_map>
#include <vector>

#include <unicode/uchar.h>

//#include "token.h"

enum class TokenGroup : uint64_t { RESERVED_WORD = 0, LITERAL = 0x100 };

enum class TokenType : uint64_t {
  EMPTY = 0,
  WHITE_SPACE,
  LINE_TERMINATOR,
  COMMENT,
  KEYWORD,
  FUTURUE_RESERVED_WORD,
  PUNCTUATOR,
  DIV_PUNCTUATOR,
  NULL_LITERAL = 0x100 + 1,
  BOOLEAN_LITERAL,
  NUMERIC_LITERAL,
  STRING_LITERAL,
  REGULAR_EXPRESSION_LITERAL,
  IDENTIFIER = 0x200
};

struct Lexer {

  using code_point_t = uint16_t;

  std::vector<code_point_t> buffer;
  using InputIt = decltype(buffer.begin());

  std::unordered_map<std::string, TokenType> reserved_words;

  InputIt first, cur, last;

  struct Token {
    TokenType type;
    InputIt first, last;
  };

  Token current_matched_token = {TokenType::EMPTY}, last_matched_token;
  std::function<void(const char *)> trace_strategy;

  template <typename InputIt>
  Lexer(InputIt first, InputIt last)
      : buffer{first, last}, first(buffer.begin()), cur(buffer.begin()),
        last(buffer.end()) {
    reserved_words = {{"break", TokenType::KEYWORD},
                      {"case", TokenType::KEYWORD},
                      {"catch", TokenType::KEYWORD},
                      {"continue", TokenType::KEYWORD},
                      {"debugger", TokenType::KEYWORD},
                      {"default", TokenType::KEYWORD},
                      {"delete", TokenType::KEYWORD},
                      {"do", TokenType::KEYWORD},
                      {"else", TokenType::KEYWORD},
                      {"finally", TokenType::KEYWORD},
                      {"for", TokenType::KEYWORD},
                      {"function", TokenType::KEYWORD},
                      {"if", TokenType::KEYWORD},
                      {"in", TokenType::KEYWORD},
                      {"instanceof", TokenType::KEYWORD},
                      {"new", TokenType::KEYWORD},
                      {"return", TokenType::KEYWORD},
                      {"switch", TokenType::KEYWORD},
                      {"this", TokenType::KEYWORD},
                      {"throw", TokenType::KEYWORD},
                      {"try", TokenType::KEYWORD},
                      {"typeof", TokenType::KEYWORD},
                      {"var", TokenType::KEYWORD},
                      {"void", TokenType::KEYWORD},
                      {"while", TokenType::KEYWORD},
                      {"with", TokenType::KEYWORD},
                      {"class", TokenType::FUTURUE_RESERVED_WORD},
                      {"const", TokenType::FUTURUE_RESERVED_WORD},
                      {"enum", TokenType::FUTURUE_RESERVED_WORD},
                      {"export", TokenType::FUTURUE_RESERVED_WORD},
                      {"extends", TokenType::FUTURUE_RESERVED_WORD},
                      {"import", TokenType::FUTURUE_RESERVED_WORD},
                      {"super", TokenType::FUTURUE_RESERVED_WORD},
                      {"implements", TokenType::FUTURUE_RESERVED_WORD},
                      {"interface", TokenType::FUTURUE_RESERVED_WORD},
                      {"yield", TokenType::FUTURUE_RESERVED_WORD},
                      {"let", TokenType::FUTURUE_RESERVED_WORD},
                      {"package", TokenType::FUTURUE_RESERVED_WORD},
                      {"private", TokenType::FUTURUE_RESERVED_WORD},
                      {"protected", TokenType::FUTURUE_RESERVED_WORD},
                      {"public", TokenType::FUTURUE_RESERVED_WORD},
                      {"static", TokenType::FUTURUE_RESERVED_WORD},
                      {"{", TokenType::PUNCTUATOR},
                      {"}", TokenType::PUNCTUATOR},
                      {"(", TokenType::PUNCTUATOR},
                      {")", TokenType::PUNCTUATOR},
                      {"[", TokenType::PUNCTUATOR},
                      {"]", TokenType::PUNCTUATOR},
                      {".", TokenType::PUNCTUATOR},
                      {";", TokenType::PUNCTUATOR},
                      {",", TokenType::PUNCTUATOR},
                      {"<", TokenType::PUNCTUATOR},
                      {">", TokenType::PUNCTUATOR},
                      {"<=", TokenType::PUNCTUATOR},
                      {">=", TokenType::PUNCTUATOR},
                      {"==", TokenType::PUNCTUATOR},
                      {"!=", TokenType::PUNCTUATOR},
                      {"===", TokenType::PUNCTUATOR},
                      {"!==", TokenType::PUNCTUATOR},
                      {"+", TokenType::PUNCTUATOR},
                      {"-", TokenType::PUNCTUATOR},
                      {"*", TokenType::PUNCTUATOR},
                      {"%", TokenType::PUNCTUATOR},
                      {"++", TokenType::PUNCTUATOR},
                      {"--", TokenType::PUNCTUATOR},
                      {"<<", TokenType::PUNCTUATOR},
                      {">>", TokenType::PUNCTUATOR},
                      {">>>", TokenType::PUNCTUATOR},
                      {"&", TokenType::PUNCTUATOR},
                      {"|", TokenType::PUNCTUATOR},
                      {"^", TokenType::PUNCTUATOR},
                      {"!", TokenType::PUNCTUATOR},
                      {"~", TokenType::PUNCTUATOR},
                      {"&&", TokenType::PUNCTUATOR},
                      {"||", TokenType::PUNCTUATOR},
                      {"?", TokenType::PUNCTUATOR},
                      {":", TokenType::PUNCTUATOR},
                      {"=", TokenType::PUNCTUATOR},
                      {"+=", TokenType::PUNCTUATOR},
                      {"-=", TokenType::PUNCTUATOR},
                      {"*=", TokenType::PUNCTUATOR},
                      {"%=", TokenType::PUNCTUATOR},
                      {"<<=", TokenType::PUNCTUATOR},
                      {">>=", TokenType::PUNCTUATOR},
                      {">>>=", TokenType::PUNCTUATOR},
                      {"&=", TokenType::PUNCTUATOR},
                      {"|=", TokenType::PUNCTUATOR},
                      {"^=", TokenType::PUNCTUATOR},
                      {"/", TokenType::DIV_PUNCTUATOR},
                      {"/=", TokenType::DIV_PUNCTUATOR},
                      {"null", TokenType::NULL_LITERAL},
                      {"true", TokenType::BOOLEAN_LITERAL},
                      {"false", TokenType::BOOLEAN_LITERAL}};
  }

  template <typename T>
  Lexer(const T &source)
      : Lexer(std::begin(source), std::end(source)) {}

  Token token() const { return last_matched_token; }

  bool done() const { return first == last; }

  bool next(bool no_div = false, bool match_line_terminator = false) {

    while (true) {
      switch (current_matched_token.type) {
      case TokenType::EMPTY:
        if (!scan_input_element_div())
          return false;
        else
          continue;
      case TokenType::WHITE_SPACE: consume(); continue;
      case TokenType::COMMENT:
        // TODO handle match_line_terminator case
        consume();
        continue;
      case TokenType::LINE_TERMINATOR:
        consume();
        if (match_line_terminator) return true;
        continue;
      case TokenType::DIV_PUNCTUATOR:
        if (no_div) {
          first = cur = current_matched_token.first;
          if (!scan_regular_expression_literal()) return false;
        }
        return true;
      default: return true;
      }
    }
  }

  bool match(const char *token) {
    return (next() &&
            std::string{current_matched_token.first,
                        current_matched_token.last} == token) ?
               consume() :
               false;
  }
  bool lookahead_impl(const char *token) {
    return (next() &&
            std::string{current_matched_token.first,
                        current_matched_token.last} == token) ?
               true :
               false;
  }

  bool match(const TokenType &type) {

    return (next(false, type == TokenType::LINE_TERMINATOR) &&
            current_matched_token.type == type) ?
               consume() :
               false;
  }

  bool match(const TokenGroup &type) {
    return (next(type == TokenGroup::LITERAL) &&
            (static_cast<uint64_t>(current_matched_token.type) &
             static_cast<uint64_t>(type)) == static_cast<uint64_t>(type)) ?
               consume() :
               false;
  }

  void set_trace_strategy(std::function<void(const char *)> strategy) {
    trace_strategy = strategy;
  }

  void trace(const char *str) {
    std::string source = {cur -
                              std::min(2L, std::distance(buffer.begin(), cur)),
                          cur + std::min(2L, std::distance(cur, last))};
    if (std::min(2L, std::distance(buffer.begin(), cur)) < 2L) {
      source = std::string(2 - std::min(2L, std::distance(buffer.begin(), cur)),
                           ' ') +
               source;
    }
    source.resize(4, ' ');
    std::transform(source.begin(), source.end(), source.begin(), [](auto c) {
      if (c == '\n')
        return ' ';
      else
        return c;
    });
    auto buffer = std::string("lexer: [") + source + "] " + str;
    trace_strategy(buffer.data());
  }

  template <typename... Args> void trace(const char *format, Args &&... args) {
    char buffer[81];
    std::snprintf(buffer, sizeof(buffer), format, std::forward<Args>(args)...);
    trace_strategy(buffer);
  }

  [[noreturn]] bool error(const char *str) {
    auto line = std::count(buffer.begin(), cur, '\n') + 1;
    throw std::runtime_error(std::string{str} + "\n" + std::string{first, cur} +
                             " at line " + std::to_string(line));
  }

  bool lookahead_impl(code_point_t c) const noexcept {
    auto next = cur;
    return next != last && *next == c;
  }

  bool lookahead() { return false; }

  template <typename T, typename... Ts> bool lookahead(T &&c, Ts &&... tail) {
    return lookahead_impl(c) || lookahead(tail...);
  }
  bool scan_impl(code_point_t c) {
    if (cur == last || *cur != c) return false;
    return ++cur, true;
  }

  bool scan_impl(const char *str) {
    auto mark = cur;
    while (*str) {
      if (!scan_impl(*str++)) return cur = mark, false;
    }
    return true;
  }

  bool scan() { return false; }

  template <typename T, typename... Ts> bool scan(T &&c, Ts &&... tail) {
    return scan_impl(c) || scan(tail...);
  }

  bool consume() {
    last_matched_token    = current_matched_token;
    current_matched_token = {TokenType::EMPTY};
    first                 = cur;
    return true;
  }

  bool commit(TokenType type) {
    trace(":commit: 0x%016llx: %s", type, std::string{first, cur}.data());
    current_matched_token = {type, first, cur};
    return true;
  }

  bool rollback() {
    cur = first;
    return false;
  }

  bool rollback(const Token &token) {
    first = cur = token.last;
    last_matched_token    = {TokenType::EMPTY};
    current_matched_token = token;
    return false;
  }

  // 6
  bool scan_source_character() { return cur != last ? ++cur, true : false; }

  // 7
  bool scan_input_element_div() {
    trace("scan_input_element_div");
    return scan_white_space() || scan_line_terminator() || scan_comment() ||
           scan_token() || scan_div_punctuator();
  }

  bool scan_input_element_reg_exp() {
    trace("scan_input_element_reg_exp");
    return scan_white_space() || scan_line_terminator() || scan_comment() ||
           scan_token() || scan_regular_expression_literal();
  }

  // 7.2
  bool scan_white_space() {
    trace("scan_white_space");
    if (cur == last) return false;
    switch (*cur) {
    case 0x0009: // <TAB>
    case 0x000B: // <VT>
    case 0x000C: // <FF>
    case 0x0020: // <SP>
    case 0x00A0: // <NBSP>
    case 0xFEFF: // <BOM>
      break;
    default:
      switch (u_charType(*cur)) {
      case U_SPACE_SEPARATOR: break; // <USP>
      default: return false;
      }
    }
    ++cur;
    return commit(TokenType::WHITE_SPACE);
  }

  // 7.3
  bool scan_line_terminator() {
    trace("scan_line_terminator");
    if (cur == last) return false;
    switch (*cur) {
    case 0x000D: // <CR>
      if (*(++cur) != 0x000A) break;
    case 0x000A: // <LF>
    case 0x2028: // <LS>
    case 0x2029: // <PS>
      ++cur;
      break;
    default: return false;
    }
    return commit(TokenType::LINE_TERMINATOR);
  }

  static bool is_line_terminator(const code_point_t &c) {
    switch (c) {
    case 0x000D: // <CR>
    case 0x000A: // <LF>
    case 0x2028: // <LS>
    case 0x2029: // <PS>
      return true;
    default: return false;
    }
  }

  // 7.4
  bool scan_comment() {
    trace("scan_comment");
    return scan_multi_line_comment() || scan_single_line_comment();
  }

  bool scan_multi_line_comment() {
    trace("scan_multi_line_comment");
    if (!scan("/*")) return false;
    scan_multi_line_comment_chars();
    if (!scan("*/")) return error("unterminated comment");
    return commit(TokenType::COMMENT);
  }

  bool scan_multi_line_comment_chars() {
    trace("scan_multi_line_comment_chars");
    if (scan_multi_line_not_asterisk_char()) {
      scan_multi_line_comment_chars();
      return true;
    }
    trace("checking *");
    auto mark = cur;
    if (scan("*")) {
      if (lookahead('/')) {
        trace("found /, rollback");
        return cur = mark, false;
      }
      scan_post_asterisk_comment_chars();
      return true;
    }
    return false;
  }

  bool scan_post_asterisk_comment_chars() {
    trace("scan_post_asterisk_comment_chars");
    if (scan_multi_line_not_forward_slash_or_asterisk_char()) {
      scan_multi_line_comment_chars();
      return true;
    }
    auto mark = cur;
    if (scan("*")) {
      if (lookahead('/')) return cur = mark, false;
      scan_post_asterisk_comment_chars();
      return true;
    }
    return false;
  }

  bool scan_multi_line_not_asterisk_char() {
    trace("scan_multi_line_not_asterisk_char");
    return (cur != last && *cur != '*') ? ++cur, true : false;
  }

  bool scan_multi_line_not_forward_slash_or_asterisk_char() {
    trace("scan_multi_line_not_forward_slash_or_asterisk_char");
    return (cur != last && *cur != '*' && *cur != '/') ? ++cur, true : false;
  }

  bool scan_single_line_comment() {
    trace("scan_single_line_comment");
    if (!scan("//")) return false;
    scan_single_line_comment_chars();
    return commit(TokenType::COMMENT);
  }

  bool scan_single_line_comment_chars() {
    trace("scan_single_line_comment_chars");
    if (!scan_single_line_comment_char()) return false;
    while (scan_single_line_comment_char())
      ;
    return true;
  }

  bool scan_single_line_comment_char() {
    auto mark = cur;
    if (scan_line_terminator()) return cur = mark, false;
    return ++cur, true;
  }

  // 7.5
  bool scan_token() {
    trace("scan_token");
    return scan_identifier_name() || scan_punctuator() ||
           scan_numeric_literal() || scan_string_literal();
  }

  // 7.6
  bool scan_identifier_name() {
    trace("scan_identifier_name");
    if (!scan_identifier_start()) return false;
    while (scan_identifier_part()) continue;

    auto identifier_name = std::string{first, cur};
    auto it              = reserved_words.find(identifier_name);

    if (it != reserved_words.end()) return commit(it->second);

    return commit(TokenType::IDENTIFIER);
  }

  bool scan_identifier_start() {
    trace("scan_identifier_start");
    if (cur == last) return false;
    switch (*cur) {
    case '$':
    case '_': return ++cur, true;
    case '\\': ++cur; return scan_unicode_escape_sequence();
    default: return scan_unicode_letter();
    }
    return false;
  }

  bool scan_identifier_part() {
    trace("scan_identifier_part");
    return scan_identifier_start() || scan_unicode_combining_mark() ||
           scan_unicode_digit() || scan_unicode_connector_puncturation();
    /* || <ZWNJ> || <ZWJ> */
  }

  bool scan_unicode_letter() {
    trace("scan_unicode_letter");
    if (cur == last) return false;
    switch (u_charType(*cur)) {
    case U_UPPERCASE_LETTER:
    case U_LOWERCASE_LETTER:
    case U_TITLECASE_LETTER:
    case U_MODIFIER_LETTER:
    case U_OTHER_LETTER:
    case U_LETTER_NUMBER: ++cur; return true;
    default: return false;
    }
  }

  bool scan_unicode_combining_mark() {
    trace("scan_unicode_combining_mark");
    if (cur == last) return false;
    switch (u_charType(*cur)) {
    case U_NON_SPACING_MARK:
    case U_COMBINING_SPACING_MARK: ++cur; return true;
    default: return false;
    }
  }

  bool scan_unicode_digit() {
    trace("scan_unicode_digit");
    if (cur == last) return false;
    switch (u_charType(*cur)) {
    case U_DECIMAL_DIGIT_NUMBER: ++cur; return true;
    default: return false;
    }
  }

  bool scan_unicode_connector_puncturation() {
    trace("scan_unicode_connector_puncturation");
    if (cur == last) return false;
    switch (u_charType(*cur)) {
    case U_CONNECTOR_PUNCTUATION: ++cur; return true;
    default: return false;
    }
  }

  // 7.7
  bool scan_punctuator() {
    trace("scan_punctuator");
    if (cur == last) return false;
    switch (*cur) {
    case '{':
    case '}':
    case '(':
    case ')':
    case '[':
    case ']':
    case '.':
    case ';':
    case ',':
    case '~':
    case '?':
    case ':': ++cur; break;
    case '<':
      if (++cur == last) break;
      switch (*cur) {
      case '=': ++cur; break; // <=
      case '<':               // <<
        if (++cur == last) break;
        switch (*cur) {
        case '=': ++cur; break; // <<=
        }
      }
      break;
    case '>':
      if (++cur == last) break;
      switch (*cur) {
      case '=': ++cur; break; // >=
      case '>':               // >>
        if (++cur == last) break;
        switch (*cur) {
        case '=': ++cur; break; // >>=
        case '>':               // >>>
          if (++cur == last) break;
          switch (*cur) {
          case '=': ++cur; break; // >>>=
          }
        }
      }
      return commit(TokenType::PUNCTUATOR);
    case '=':
      if (++cur == last) break;
      switch (*cur) {
      case '=': // ==
        if (++cur == last) break;
        switch (*cur) {
        case '=': ++cur; break; // ===
        }
      }
      break;
    case '!':
      if (++cur == last) break;
      switch (*cur) {
      case '=': // !=
        if (++cur == last) break;
        switch (*cur) {
        case '=': ++cur; break; // !==
        }
      }
      break;
    case '+':
      if (++cur == last) break;
      switch (*cur) {
      case '+':               // ++
      case '=': ++cur; break; // +=
      }
      break;
    case '-':
      if (++cur == last) break;
      switch (*cur) {
      case '-':               // --
      case '=': ++cur; break; // -=
      }
      break;
    case '*':
      if (++cur == last) break;
      switch (*cur) {
      case '=': ++cur; break; // *=
      }
      break;
    case '%':
      if (++cur == last) break;
      switch (*cur) {
      case '=': ++cur; break; // %=
      }
      break;
    case '&':
      if (++cur == last) break;
      switch (*cur) {
      case '&':               // &&
      case '=': ++cur; break; // &=
      }
      break;
    case '|':
      if (++cur == last) break;
      switch (*cur) {
      case '|':               // ||
      case '=': ++cur; break; // |=
      }
      break;
    case '^':
      if (++cur == last) break;
      switch (*cur) {
      case '=': ++cur; break; // ^=
      }
      break;
    default: return false;
    }
    return commit(TokenType::PUNCTUATOR);
  }

  bool scan_div_punctuator() {
    trace("scan_div_punctuator");
    if (!scan('/')) return false;
    scan('=');
    return commit(TokenType::DIV_PUNCTUATOR);
  }

  // 7.8
  bool scan_literal() {
    trace("scan_literal");
    return scan_null_literal() || scan_boolean_literal() ||
           scan_numeric_literal() || scan_string_literal() ||
           scan_regular_expression_literal();
  }

  // 7.8.1
  bool scan_null_literal() {
    trace("scan_null_literal");
    return scan("null");
  }

  // 7.8.2
  bool scan_boolean_literal() {
    trace("scan_boolean_literal");
    return scan("true") || scan("false");
  }

  // 7.8.3
  bool scan_numeric_literal() {
    trace("scan_numeric_literal");
    auto mark = cur;
    if (scan_decimal_literal() && !scan_identifier_start() &&
        !scan_decimal_digit()) {
      return commit(TokenType::NUMERIC_LITERAL);
    }
    cur = mark;
    if (scan_hex_integer_literal() && !scan_identifier_start() &&
        !scan_decimal_digit()) {
      trace("Committing hex");
      return commit(TokenType::NUMERIC_LITERAL);
    }
    cur = mark;
    return false;
  }

  bool scan_decimal_literal() {
    trace("scan_decimal_literal");
    if (scan_decimal_integer_literal()) {
      if (scan('.')) {
        scan_decimal_digits();
        scan_exponent_part();
      }
      return true;
    } else if (scan('.')) {
      if (!scan_decimal_digits()) return rollback();
      scan_exponent_part();
      return true;
    }

    return false;
  }

  bool scan_decimal_integer_literal() {
    trace("scan_decimal_integer_literal");
    if (scan_non_zero_digit()) {
      scan_decimal_digits();
      return true;
    }
    return scan('0') && !lookahead('x');
  }

  bool scan_decimal_digits() {
    trace("scan_decimal_digits");
    if (!scan_decimal_digit()) return false;
    while (scan_decimal_digit())
      ;
    return true;
  }

  bool scan_decimal_digit() {
    trace("scan_decimal_digit");
    return scan('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
  }

  bool scan_non_zero_digit() {
    trace("scan_non_zero_digit");
    return scan('1', '2', '3', '4', '5', '6', '7', '8', '9');
  }

  bool scan_exponent_part() {
    trace("scan_exponent_part");
    if (!scan_exponent_indicator()) return false;
    if (!scan_signed_integer()) return error("missing exponent");
    return true;
  }

  bool scan_exponent_indicator() {
    trace("scan_exponent_indicator");
    return scan('e', 'E');
  }

  bool scan_signed_integer() {
    trace("scan_signed_integer");
    if (scan('+', '-') && !scan_decimal_digits())
      return error("missing exponent");
    else
      return scan_decimal_digits();
  }

  bool scan_hex_integer_literal() {
    trace("scan_hex_integer_literal");
    if (!scan("0x", "0X")) return false;
    if (!scan_hex_digit())
      return error("missing hexadecimal digits after '0x'");
    while (scan_hex_digit())
      ;
    return true;
  }

  bool scan_hex_digit() {
    trace("scan_hex_digit");
    return scan('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c',
                'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F');
  }

  // 7.8.4
  bool scan_string_literal() {
    trace("scan_string_literal");

    if (scan('"')) {
      scan_double_string_characters();
      if (!scan('"')) return error("unterminated string literal");
      return commit(TokenType::STRING_LITERAL);
    } else if (scan('\'')) {
      scan_single_string_characters();
      if (!scan('\'')) return error("unterminated string literal");
      return commit(TokenType::STRING_LITERAL);
    }

    return false;
  }

  bool scan_double_string_characters() {
    trace("scan_double_string_characters");
    if (!scan_double_string_character()) return false;
    while (scan_double_string_character())
      ;
    return true;
  }

  bool scan_single_string_characters() {
    trace("scan_single_string_characters");
    if (!scan_single_string_character()) return false;
    while (scan_single_string_character())
      ;
    return true;
  }

  bool scan_double_string_character() {
    trace("scan_double_string_character");
    auto mark = cur;
    if (scan('"')) return cur = mark, false;
    if (scan('\\')) {
      if (!scan_escape_sequence()) return error(""); // TODO
      return true;
    } else if (scan_line_continuation())
      return true;
    return ++cur, true;
  }

  bool scan_single_string_character() {
    trace("scan_single_string_character");
    auto mark = cur;
    if (scan('\'')) return cur = mark, false;
    if (scan('\\')) {
      if (!scan_escape_sequence()) return error(""); // TODO
    }
    if (scan_line_continuation()) return true;
    return ++cur, true;
  }

  bool scan_line_continuation() {
    trace("scan_line_continuation");
    auto mark = cur;
    if (!scan('\\')) return false;
    if (!scan_line_terminator()) return cur = mark, false;
    return true;
  }

  bool scan_escape_sequence() {
    trace("scan_escape_sequence");
    return scan_character_escape_sequence() ||
           (scan('0') &&
            !lookahead('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')) ||
           scan_hex_escape_sequence() || scan_unicode_escape_sequence();
  }

  bool scan_character_escape_sequence() {
    trace("scan_character_escape_sequence");
    return scan_single_escape_character() || scan_non_escape_character();
  }

  bool scan_single_escape_character() {
    trace("scan_single_escape_character");
    return scan('\'', '"', '\\', 'b', 'f', 'n', 'r', 't', 'v');
  }

  bool scan_non_escape_character() {
    // TODO
    trace("scan_non_escape_character");
    return ++cur, true;
  }

  bool scan_escape_character() {
    return scan_single_escape_character() || scan_decimal_digit() ||
           scan('x', 'u');
  }

  bool scan_hex_escape_sequence() {
    trace("scan_hex_escape_sequence");
    auto mark = cur;
    if (!scan('x')) return false;
    for (int i = 0; i < 2; ++i)
      if (!scan_hex_digit()) return cur = mark, false;
    return true;
  }

  bool scan_unicode_escape_sequence() {
    trace("scan_unicode_escape_sequence");
    auto mark = cur;
    if (!scan('u')) return false;
    for (int i = 0; i < 4; ++i)
      if (!scan_hex_digit()) return cur = mark, false;
    return true;
  }

  // 7.8.5
  bool scan_regular_expression_literal() {
    trace("scan_regular_expression_literal");
    if (!scan('/')) return false;
    return scan_regular_expression_body() && scan('/') &&
                   scan_regular_expression_flags() ?
               commit(TokenType::REGULAR_EXPRESSION_LITERAL) :
               error("unterminated regular expression literal");
  }

  bool scan_regular_expression_body() {
    trace("scan_regular_expression_body");
    return scan_regular_expression_first_char() &&
           scan_regular_expression_chars();
  }

  bool scan_regular_expression_chars() {
    trace("scan_regular_expression_chars");
    while (scan_regular_expression_char())
      ;
    return true;
  }

  bool scan_regular_expression_first_char() {
    trace("scan_regular_expression_first_char");
    return (!lookahead('*') && !lookahead('\\') && !lookahead('/') &&
            !lookahead('[') && scan_regular_expression_non_terminator()) ||
           scan_regular_expression_backslash_sequence() ||
           scan_regular_expression_class();
  }

  bool scan_regular_expression_char() {
    trace("scan_regular_expression_char");
    return (!lookahead('\\') && !lookahead('/') && !lookahead('[') &&
            scan_regular_expression_non_terminator()) ||
           scan_regular_expression_backslash_sequence() ||
           scan_regular_expression_class();
  }

  bool scan_regular_expression_backslash_sequence() {
    trace("scan_regular_expression_backslash_sequence");
    auto mark = cur;
    return !(scan('\\') && scan_regular_expression_non_terminator()) ?
           cur = mark,
           false : true;
  }

  bool scan_regular_expression_non_terminator() {
    trace("scan_regular_expression_non_terminator");
    if (scan_line_terminator())
      return error("unterminated regular expression literal");
    return scan_source_character();
  }

  bool scan_regular_expression_class() {
    trace("scan_regular_expression_class");
    if (!scan('[')) return false;
    if (!scan_regular_expression_class_chars() || !scan(']'))
      return error("unterminated regular expression literal");
    return true;
  }

  bool scan_regular_expression_class_chars() {
    trace("scan_regular_expression_class_chars");
    while (scan_regular_expression_class_char())
      ;
    return true;
  }

  bool scan_regular_expression_class_char() {
    trace("scan_regular_expression_class_char");
    return (!lookahead(']') && !lookahead('\\') &&
            scan_regular_expression_non_terminator()) ||
           scan_regular_expression_backslash_sequence();
  }

  bool scan_regular_expression_flags() {
    trace("scan_regular_expression_flags");
    while (scan_identifier_part())
      ;
    return true;
  }
};

#endif
