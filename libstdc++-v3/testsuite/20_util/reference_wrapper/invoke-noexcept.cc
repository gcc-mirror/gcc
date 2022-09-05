// { dg-do compile { target c++11 } }

// C++11 20.8.3.4 reference_wrapper invocation [refwrap.invoke]

#include <functional>

struct F
{
  int operator()() noexcept(true) { return 1; }
  int operator()() const noexcept(false) { return 2; }
};

F f;
static_assert( noexcept(std::ref(f)()) );
static_assert( ! noexcept(std::cref(f)()) );
