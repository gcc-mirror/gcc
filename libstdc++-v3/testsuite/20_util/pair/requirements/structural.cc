// { dg-do compile { target c++20 } }

#include <utility>

// C++20 20.4.2 [pairs.pair]
// pair<T, U> is a structural type (13.2) if T and U are both structural types.

template<std::pair<int, int>> struct S; // PR libstdc++/97930
