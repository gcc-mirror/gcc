// { dg-do compile { target c++20 } }

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
  VERIFY( it[0] == 0 );
  VERIFY( &*it == &v.front() );
  VERIFY( &it[1] == &v[1] );
  VERIFY( it++ == v.begin() );
  VERIFY( ++it == v.end() );
  VERIFY( (it - 2) == v.begin() );
  VERIFY( (it - v.begin()) == 2 );
  it -= 2;
  it += 1;
  VERIFY( (it + 1) == v.end() );
  VERIFY( (1 + it) == v.end() );
  it = it + 1;
  auto it2 = v.begin();
  std::swap(it, it2);
  VERIFY( it == v.begin() );
  VERIFY( it2 == v.end() );

  auto rit = v.rbegin();
  VERIFY( rit[0] == 0 );
  VERIFY( &*rit == &v.back() );
  VERIFY( &rit[1] == &v[0] );
  VERIFY( rit++ == v.rbegin() );
  VERIFY( ++rit == v.rend() );
  VERIFY( (rit - 2) == v.rbegin() );
  VERIFY( (rit - v.rbegin()) == 2 );
  rit -= 2;
  rit += 1;
  VERIFY( (rit + 1) == v.rend() );
  VERIFY( (1 + rit) == v.rend() );
  rit = rit + 1;
  auto rit2 = v.rbegin();
  std::swap(rit, rit2);
  VERIFY( rit == v.rbegin() );
  VERIFY( rit2 == v.rend() );

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
  requires (std::bool_constant<&std::vector<T>().at(0) != nullptr>::value)
  constexpr std::true_type
  access_empty() { return {}; }

template<typename T = int>
  requires (std::bool_constant<&std::vector<T>()[0] != nullptr>::value)
  constexpr std::true_type
  access_empty() { return {}; }

template<typename T = int>
  requires (std::bool_constant<&std::vector<T>().front() != nullptr>::value)
  constexpr std::true_type
  access_empty() { return {}; }

template<typename T = int>
  requires (std::bool_constant<&std::vector<T>().back() != nullptr>::value)
  constexpr std::true_type
  access_empty() { return {}; }

static_assert( ! access_empty() );

template<typename T = int>
  constexpr std::false_type
  access_past_the_end() { return {}; }

template<typename T = int>
  requires (std::bool_constant<&std::vector<T>(3).at(3) != nullptr>::value)
  constexpr std::true_type
  access_past_the_end() { return {}; }

template<typename T = int>
  requires (std::bool_constant<&std::vector<T>(3)[3] != nullptr>::value)
  constexpr std::true_type
  access_past_the_end() { return {}; }

static_assert( ! access_past_the_end() );
