#include "lexer.h"
#include "parser.h"

#include <cstring>
#include <fstream>
#include <iostream>

#include <docopt.h>
static const char USAGE[] =
    R"(js

    Usage: js [--trace] [--debug-ast] [--verify-ast] [--print-ast|--tokens] (<file>|-e <string>)...
           js (-h | --help)
           js --version

    Options:
      --trace       Print verbose debuging information
      --debug-ast   Print AST transforms
      --verify-ast  Do some sanity checks on the AST
      -h --help     Show this screen.
      --version     Show version.
)";

int main(int argc, const char **argv) {
  auto args = docopt::docopt(USAGE, {argv + 1, argv + argc}, true, "js 1.0");

  for (auto arg : args)
    std::cout << arg.first << ": " << arg.second << std::endl;

  std::string source;

  for (auto file : args["<file>"].asStringList()) {
    std::ifstream in(file, std::ios::in | std::ios::binary);
    if (!in) throw std::runtime_error(std::strerror(errno));
    source += std::string{std::istreambuf_iterator<char>(in),
                          std::istreambuf_iterator<char>()};
  }

  for (auto string : args["<string>"].asStringList()) {
    source += std::string{string.begin(), string.end()};
  }

  auto parser = Parser{source.begin(), source.end()};
  if (args["--trace"].asBool()) {
    parser.set_trace_strategy(
        [](auto what) { std::cerr << "trace: " << what << std::endl; });
  }

  auto print_ast = [](auto ast, auto offset) {
    std::cerr << "-- ast" << std::endl;
    for (const auto &node : ast) {
      if (&node == &ast.front() + offset) {
        std::printf("-->");
      } else
        std::printf("   ");

      auto source = std::string{node.token.first, node.token.last};
      std::transform(source.begin(), source.end(), source.begin(), [](auto c) {
        if (c == '\n')
          return ' ';
        else
          return c;
      });

      if (source.size() > 24) {
        source.resize(21);
        source.resize(24, '.');
      }

      char buffer[81];
      std::snprintf(buffer, sizeof(buffer),
                    "type: 0x%04lx, size: %4lu %24s : %s", node.type, node.size,
                    name(node.type), source.data());
      std::cerr << buffer << std::endl;
    }
  };

  if (args["--debug-ast"].asBool()) {
    parser.set_debug_ast_strategy(print_ast);
  }

  if (!parser.parse_program())
    ;

  if (args["--verify-ast"].asBool()) {}
  if (args["--print-ast"].asBool()) { print_ast(parser.ast, 0); }
  return 0;
}
