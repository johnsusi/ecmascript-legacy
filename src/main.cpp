#include "parser.h"

#include <cstring>
#include <fstream>
#include <iostream>

#include <docopt.h>
static const char USAGE[] =
    R"(js

    Usage: js (<file>|-e <string>)...
           js (-h | --help)
           js --version

    Options:
      -h --help     Show this screen.
      --version     Show version.
)";

int main(int argc, const char **argv)
{
  auto args = docopt::docopt(USAGE, {argv + 1, argv + argc}, true, "js 1.0");

  for (auto arg : args)
    std::cout << arg.first << ": " << arg.second << std::endl;

  for (auto file : args["<file>"].asStringList()) {
    std::ifstream in(file, std::ios::in | std::ios::binary);
    if (!in) throw std::runtime_error(std::strerror(errno));
    eval(std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>());
  }

  for (auto string : args["<string>"].asStringList()) {
    eval(string.begin(), string.end());
  }

  return 0;
}
