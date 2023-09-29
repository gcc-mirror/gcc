// { dg-do compile { target c++23 } }
#include <functional>

int f();

template<typename R>
concept can_invoke = requires (int (&f)()) { std::invoke_r<R>(f); };

static_assert( not can_invoke<const int&> );
static_assert( not can_invoke<int&&> );
static_assert( not can_invoke<const long&> );
