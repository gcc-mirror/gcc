// { dg-do compile { target c++11 } }
// PR libstdc++/104174 unordered_map<const T, U, H> fails
#include <unordered_map>
std::unordered_map<const int, int, std::hash<int>> m;
