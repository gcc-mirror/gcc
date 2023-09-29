// { dg-do compile { target c++20 } }

#include <utility>

// This is a GCC extension. std::exchange is not required to be noexcept.

static_assert( noexcept( std::exchange(std::declval<int&>(), 1) ) );

struct X
{
  X(const X&);
  X(X&&) noexcept;
  X& operator=(const X&);
  X& operator=(X&&) noexcept;
  X& operator=(int);
};

extern X x, x2;
static_assert( noexcept( std::exchange(x, std::move(x2)) ) );
static_assert( ! noexcept( std::exchange(x, 1) ) );

struct Y
{
  Y(Y&&) noexcept;
  Y& operator=(Y&&);
};

extern Y y, y2;
static_assert( ! noexcept( std::exchange(y, std::move(y2)) ) );

struct Z
{
  Z(Z&&)noexcept;
  Z& operator=(Z&&) ;
};

extern Z z, z2;
static_assert( ! noexcept( std::exchange(z, std::move(z2)) ) );
