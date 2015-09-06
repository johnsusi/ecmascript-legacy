#include "catch.hpp"

#include "lexer.h"

// TEST_CASE("Keywords")
// {
//   auto test = [](auto source) {
//     Lexer ll(source);
//     if (!ll.scan_input_element_reg_exp()) return false;
//     switch (ll.current_matched_type) {
//     case token("break"):
//     case token("case"):
//     case token("catch"):
//     case token("continue"):
//     case token("debugger"):
//     case token("default"):
//     case token("delete"):
//     case token("do"):
//     case token("else"):
//     case token("finally"):
//     case token("for"):
//     case token("function"):
//     case token("if"):
//     case token("in"):
//     case token("instanceof"):
//     case token("typeof"):
//     case token("new"):
//     case token("var"):
//     case token("return"):
//     case token("void"):
//     case token("switch"):
//     case token("while"):
//     case token("this"):
//     case token("with"):
//     case token("throw"):
//     case token("try"): return true;
//     default: return false;
//     }
//   };
//   REQUIRE(test("break"));
// }

TEST_CASE("Lexer")
{

  std::string source = "if (true){\n\tthrow new Error('hello');\n}\nelse "
                       "return \"world\";\n\n";

  Lexer lexer(source);

  REQUIRE(lexer.match("if"));
  REQUIRE(lexer.match("("));
  REQUIRE(lexer.match("true"));
  REQUIRE(lexer.match(")"));
  REQUIRE(lexer.match("{"));
  REQUIRE(lexer.match("throw"));
  REQUIRE(lexer.match("new"));
  REQUIRE(lexer.match(TOKEN_IDENTIFIER /*"Error"*/));
  REQUIRE(lexer.match("(")); //
  REQUIRE(lexer.match(TOKEN_STRING_LITERAL /*"'hello'"*/));
  REQUIRE(lexer.match(")"));
  REQUIRE(lexer.match(";"));
  REQUIRE(lexer.match("}"));
  REQUIRE(lexer.match("else"));
  REQUIRE(lexer.match("return"));
  REQUIRE(lexer.match(TOKEN_STRING_LITERAL /*"\"world\""*/));
  REQUIRE(lexer.match(";"));

  SECTION("LineTerminator")
  {
    REQUIRE(Lexer("\n").match(TOKEN_LINE_TERMINATOR, false));
    REQUIRE(Lexer("\r\n").match(TOKEN_LINE_TERMINATOR, false));
    REQUIRE(Lexer(" \n").match(TOKEN_LINE_TERMINATOR, false));
    REQUIRE(Lexer(" \r\n").match(TOKEN_LINE_TERMINATOR, false));
  }
}
