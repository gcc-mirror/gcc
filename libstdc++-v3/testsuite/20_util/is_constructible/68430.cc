// { dg-do compile { target c++11 } }

#include <type_traits>

template<class T> struct Foo { Foo(T = nullptr) {} };
static_assert(!std::is_constructible<Foo<int>>::value, "");
