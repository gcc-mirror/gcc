// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }
// { dg-xfail-if "not supported" { debug_mode } }

#include <vector>
#include <testsuite_hooks.h>

constexpr bool
test_iterators()
{
  std::vector<int> v;
  VERIFY( v.begin() == v.end() );
  v.reserve(1);
  VERIFY( v.begin() == v.end() );
  v.resize(2);
  VERIFY( v.begin() != v.end() );
  VERIFY( v.cbegin() == v.begin() );
  VERIFY( v.crbegin() == v.rbegin() );
  VERIFY( v.cend() == v.end() );
  VERIFY( v.crend() == v.rend() );

  auto it = v.begin();
  VERIFY( &*it == &v.front() );
  VERIFY( it++ == v.begin() );
  VERIFY( ++it == v.end() );
  VERIFY( (it - 2) == v.begin() );
  it -= 2;
  it += 1;
  VERIFY( (it + 1) == v.end() );

  auto rit = v.rbegin();
  VERIFY( &*rit == &v.back() );
  VERIFY( rit++ == v.rbegin() );
  VERIFY( ++rit == v.rend() );
  VERIFY( (rit - 2) == v.rbegin() );
  rit -= 2;
  rit += 1;
  VERIFY( (rit + 1) == v.rend() );

  return true;
}

static_assert(test_iterators());

constexpr bool
test_access()
{
  std::vector<int> v{1, 2, 3};
  VERIFY( v.at(1) == 2 );
  VERIFY( v[2] == 3 );
  VERIFY( &v[2] == &v.at(2) );
  VERIFY( &v.front() == &v[0] );
  VERIFY( &v.back() == &v[2] );

  const auto& vc = v;
  VERIFY( vc.at(1) == 2 );
  VERIFY( &vc.at(1) == &v.at(1) );
  VERIFY( &vc.at(1) == &vc[1] );
  VERIFY( &vc.front() == &vc[0] );
  VERIFY( &vc.back() == &vc[2] );

  return true;
}

static_assert(test_access());

template<typename T = int>
  constexpr std::false_type
  access_empty() { return {}; }

template<typename T = int>
  requires (std::bool_constant<(std::vector<T>().at(0), true)>::value)
  constexpr std::true_type
  access_empty() { return {}; }

template<typename T = int>
  requires (std::bool_constant<(std::vector<T>()[0], true)>::value)
  constexpr std::true_type
  access_empty() { return {}; }

template<typename T = int>
  requires (std::bool_constant<(std::vector<T>().front(), true)>::value)
  constexpr std::true_type
  access_empty() { return {}; }

template<typename T = int>
  requires (std::bool_constant<(std::vector<T>().back(), true)>::value)
  constexpr std::true_type
  access_empty() { return {}; }

static_assert( ! access_empty() );
