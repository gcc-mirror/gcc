// { dg-do compile { target c++17 } }
// PR libstdc++/110853
// Bad interaction between deduction guide with decay and constraints
// (CTAD, std::pair and function lvalue)

#include <utility>

void func() {}
std::pair p(1, func);
std::pair<int, void (*)()>& r = p;
