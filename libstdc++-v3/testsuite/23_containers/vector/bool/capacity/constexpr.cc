// { dg-do compile { target c++20 } }

#include <vector>
#include <testsuite_hooks.h>

constexpr std::size_t
capacity_for(std::size_t n)
{
  std::size_t N = std::vector<bool>(1).capacity();
  if (auto r = n % N)
    return n - r + N;
  return n;
}

constexpr bool
test_empty()
{
  std::vector<bool> v;
  VERIFY( v.empty() );
  v = {1};
  VERIFY( !v.empty() );

  return true;
}

static_assert( test_empty() );

constexpr bool
test_size()
{
  std::vector<bool> v;
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
  std::vector<bool> v;
  VERIFY( v.size() == 0 );
  VERIFY( v.capacity() == v.size() );
  v = {false, false, false};
  VERIFY( v.size() == 3 );
  VERIFY( v.capacity() >= v.size() );

  return true;
}

static_assert( test_capacity() );

constexpr bool
test_resize()
{
  std::vector<bool> v;
  v.reserve(9);
  VERIFY( v.size() == 0 );
  VERIFY( v.capacity() == capacity_for(9) );
  v.resize(5);
  VERIFY( v.size() == 5 );
  VERIFY( v.capacity() == capacity_for(9) );
  v.resize(900, true);
  VERIFY( v.size() == 900 );
  VERIFY( v.capacity() == capacity_for(900) );
  VERIFY( v[10] == true );

  return true;
}

static_assert( test_resize() );

constexpr bool
test_reserve()
{
  std::vector<bool> v;
  v.reserve(9);
  VERIFY( v.size() == 0 );
  VERIFY( v.capacity() == capacity_for(9) );
  v.resize(2);
  VERIFY( v.size() == 2 );
  VERIFY( v.capacity() == capacity_for(9) );
  v.resize(300);
  v.resize(100);
  VERIFY( v.size() == 100 );
  VERIFY( v.capacity() == capacity_for(300) );

  return true;
}

static_assert( test_reserve() );

constexpr bool
test_shrink_to_fit()
{
  std::vector<bool> v;
  v.reserve(9);
  v.shrink_to_fit();
  VERIFY( v.capacity() == 0 );
  v.reserve(9);
  v.resize(5);
  v.shrink_to_fit();
  VERIFY( v.capacity() == capacity_for(v.size()) );

  return true;
}

static_assert( test_shrink_to_fit() );
