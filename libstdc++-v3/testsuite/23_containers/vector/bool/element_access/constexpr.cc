// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }
// { dg-xfail-if "not supported" { debug_mode } }

#include <vector>
#include <testsuite_hooks.h>

constexpr bool
test_iterators()
{
  std::vector<bool> v;
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
  VERIFY( *it == v.front() );
  VERIFY( it++ == v.begin() );
  VERIFY( ++it == v.end() );
  VERIFY( (it - 2) == v.begin() );
  it -= 2;
  it += 1;
  VERIFY( (it + 1) == v.end() );

  auto rit = v.rbegin();
  VERIFY( *rit == v.back() );
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
  std::vector<bool> v{1, 1, 0, 0, 1, 0, 1, 0, 1};
  std::vector<bool>::reference r1 = v.at(1);
  VERIFY( r1 );
  std::vector<bool>::reference r2 = v[2];
  VERIFY( ! r2 );
  r1 = r2;
  VERIFY( ! r1 );
  VERIFY( ! v[1] );
  r2 = true;
  VERIFY( r2 );
  VERIFY( v[2] );

  const auto& vc = v;
  VERIFY( vc.at(1) == false );
  VERIFY( vc.at(1) == v.at(1) );
  VERIFY( vc.at(1) == vc[1] );
  VERIFY( vc.front() == vc[0] );
  VERIFY( vc.back() == vc[2] );

  return true;
}

static_assert(test_access());

template<typename T = bool>
  constexpr std::false_type
  access_empty() { return {}; }

template<typename T = bool>
  requires (std::bool_constant<(std::vector<T>().at(0), true)>::value)
  constexpr std::true_type
  access_empty() { return {}; }

template<typename T = bool>
  requires (std::bool_constant<(std::vector<T>().back(), true)>::value)
  constexpr std::true_type
  access_empty() { return {}; }

static_assert( ! access_empty() );

template<typename T = bool>
  constexpr std::false_type
  access_empty_front() { return {}; }

template<typename T = bool>
  requires (std::bool_constant<(std::vector<T>()[0], true)>::value)
  constexpr std::true_type
  access_empty_front() { return {}; }

template<typename T = bool>
  requires (std::bool_constant<(std::vector<T>().front(), true)>::value)
  constexpr std::true_type
  access_empty_front() { return {}; }

static_assert( ! access_empty_front() ); // { dg-error "ambiguous" "PR 103191" { target { ! debug_mode } } }
