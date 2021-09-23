// { dg-do compile { target c++17 } }
// PR libstdc++/101034 - wrong constraint in std::any's constructor

#include <any>

struct S {
  S(std::initializer_list<int>&, int) {}
};

std::any a(std::in_place_type<S>, {0}, 0);
S& s = a.emplace<S>({0}, 0);
