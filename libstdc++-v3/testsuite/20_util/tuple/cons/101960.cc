// { dg-do compile { target c++11 } }
#include <tuple>
std::tuple<int[1]> t;
auto tt = std::move(t); // PR libstdc++/101960
