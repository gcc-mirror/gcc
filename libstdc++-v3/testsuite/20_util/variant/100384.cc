// { dg-do compile { target c++17 } }

#include <variant>

int visit(int*, std::true_type) { return 0; }

const std::true_type dat;

int i = visit(nullptr, dat);
