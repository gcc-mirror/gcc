// { dg-do compile { target c++20 } }

#include <vector>
#include <testsuite_hooks.h>

constexpr bool
test_empty()
{
  std::vector<int> v;
  VERIFY( v.empty() );
  v = {1};
  VERIFY( !v.empty() );

  return true;
}

static_assert( test_empty() );

constexpr bool
test_size()
{
  std::vector<int> v;
  VERIFY( v.size() == 0 );
  v = {1};
  VERIFY( v.size() == 1 );

  VERIFY( v.max_size() != 0 );

  return true;
}

static_assert( test_size() );

constexpr bool
test_capacity()
{
  std::vector<int> v;
  VERIFY( v.size() == 0 );
  VERIFY( v.capacity() == v.size() );
  v = {1, 2, 3};
  VERIFY( v.size() == 3 );
  VERIFY( v.capacity() == v.size() );

  return true;
}

static_assert( test_capacity() );

constexpr bool
test_resize()
{
  std::vector<int> v;
  v.reserve(9);
  VERIFY( v.size() == 0 );
  VERIFY( v.capacity() == 9 );
  v.resize(5);
  VERIFY( v.size() == 5 );
  VERIFY( v.capacity() == 9 );
  v.resize(15, 6);
  VERIFY( v.size() == 15 );
  VERIFY( v[10] == 6 );

  return true;
}

static_assert( test_resize() );

constexpr bool
test_reserve()
{
  std::vector<int> v;
  v.reserve(9);
  VERIFY( v.size() == 0 );
  VERIFY( v.capacity() == 9 );
  v.resize(2);
  VERIFY( v.size() == 2 );
  VERIFY( v.capacity() == 9 );

  return true;
}

static_assert( test_reserve() );

constexpr bool
test_shrink_to_fit()
{
  std::vector<int> v;
  v.reserve(9);
  v.shrink_to_fit();
#if __cpp_exceptions
  VERIFY( v.capacity() == 0 );
#else
  VERIFY( v.capacity() == 9 );
#endif
  v.reserve(9);
  v.resize(5);
  v.shrink_to_fit();
#if __cpp_exceptions
  VERIFY( v.capacity() == v.size() );
#else
  VERIFY( v.capacity() == 9 );
#endif

  return true;
}

static_assert( test_shrink_to_fit() );
