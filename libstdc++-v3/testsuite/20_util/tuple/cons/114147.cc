// { dg-do compile { target c++11 } }

// PR libstdc++/114147
// tuple allocator-extended ctor requires non-explicit default ctor

#include <tuple>
#include <memory>

struct X { explicit X(); };

std::allocator<int> a;
std::tuple<X> t0(std::allocator_arg, a);
std::tuple<int, X> t1(std::allocator_arg, a);
std::tuple<X, int> t2(std::allocator_arg, a);
std::tuple<int, X, int> t3(std::allocator_arg, a);
