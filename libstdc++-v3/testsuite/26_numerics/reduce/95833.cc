// { dg-do compile { target c++17 } }
// PR libstdc++/95833 - Incorrect static_assert in std::reduce overload

#include <numeric>

struct A { };
struct B { };

struct binop
{
  template<typename T, typename U>
    A operator()(T&&, U&&) const { return A{}; }
};

B b;
A a = std::reduce(&b, &b + 1, A{}, binop{});
