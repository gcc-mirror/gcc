// { dg-do compile { target c++11 } }
#include <tuple>

// PR libstdc++/101960

std::tuple<int[1]> t;
auto tt = std::move(t);

std::tuple<int[1], int> t2;
auto tt2 = std::move(t2);

std::tuple<int[1], int[2], int[3]> t3;
auto tt3 = std::move(t3);
