// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

#include <functional>

struct Foo
{
  void func() {}
};

void bar() { }

// PR libstdc++/107784
static_assert( sizeof(std::bind_front(&Foo::func)) == sizeof(&Foo::func) );
static_assert( sizeof(std::bind_front(&bar)) == sizeof(&bar) );
