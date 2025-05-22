// { dg-do compile { target c++17 } }

#include <memory>

// PR libstdc++/120397
// std::uninitialized_value_construct cannot create arrays of non-trivially
// destructible types

struct X { X() { } ~X() { } };

void def(X (*x)[1])
{
  std::uninitialized_default_construct(x, x+1);
}

void def_n(X (*x)[1])
{
  std::uninitialized_default_construct_n(x, 1);
}
