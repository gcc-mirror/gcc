// { dg-do compile { target c++20 } }

#include <barrier>

// PR 118395 Constructor of std::barrier is not constexpr
constinit std::barrier<> b(std::barrier<>::max());
