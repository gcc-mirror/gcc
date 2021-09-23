// { dg-do compile { target c++14 } }

// LWG 3413
// propagate_const's swap's noexcept specification needs to be constrained
// and use a trait

#include <experimental/propagate_const>

using std::experimental::propagate_const;

propagate_const<int*> i;
static_assert( noexcept(i.swap(i)), "member swap is noexcept" );
static_assert( noexcept(swap(i, i)), "non-member swap is noexcept" );

struct P
{
  int i = 0;
  int& operator*() const;
};

void swap(P&, P&) noexcept(false);

propagate_const<P> p;
static_assert( ! noexcept(p.swap(p)), "member swap is conditionally noexcept" );
static_assert( ! noexcept(swap(p, p)), "non-member swap is conditionally noexcept" );

// std::is_swappable not available for -std=c++14
#if __cplusplus > 201402L || !defined(__STRICT_ANSI__)
struct Q
{
  int i = 0;
  int& operator*() const;

  Q& operator=(Q&&) = delete;
};

static_assert( ! std::is_swappable<Q>::value, "" );

static_assert( ! std::is_swappable<propagate_const<Q>>::value,
	       "non-member swap is constrained" );
#endif
