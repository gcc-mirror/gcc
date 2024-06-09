// { dg-do compile { target c++20 } }

// PR libstdc++/114152
// Wrong exception specifiers for LFTSv3 scope guard destructors

#include <experimental/scope>

using namespace std::experimental;

struct F {
  void operator()() noexcept(false);
};

static_assert( noexcept(std::declval<scope_exit<F>&>().~scope_exit()) );
static_assert( noexcept(std::declval<scope_fail<F>&>().~scope_fail()) );
static_assert( ! noexcept(std::declval<scope_success<F>&>().~scope_success()) );

struct G {
  void operator()() noexcept(true);
};

static_assert( noexcept(std::declval<scope_exit<G>&>().~scope_exit()) );
static_assert( noexcept(std::declval<scope_fail<G>&>().~scope_fail()) );
static_assert( noexcept(std::declval<scope_success<G>&>().~scope_success()) );
