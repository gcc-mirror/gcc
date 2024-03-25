// { dg-do compile { target c++20 } }

// PR libstdc++/112473 - integer_sequence accepts non-integer types

#include <utility>

std::integer_sequence<std::pair<int, int>, std::pair<int, int>{0, 0}> ic;
// { dg-error "static assertion failed" "" { target *-*-* } 0 }
