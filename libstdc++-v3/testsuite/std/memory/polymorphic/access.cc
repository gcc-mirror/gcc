// { dg-do run { target c++26 } }

#include <memory>
#include <vector>

#include <testsuite_hooks.h>

template<template<typename> class Polymorhpic>
constexpr void
test_access()
{
  const std::vector<int> src{1, 2, 3, 4, 5};
  Polymorhpic<std::vector<int>> i(src);
  auto const& ci = i;
  VERIFY( *i == src );
  VERIFY( *ci == src );
  VERIFY( *std::move(ci) == src );

  auto&& vr = *std::move(i);
  static_assert( std::is_same_v<decltype(vr), std::vector<int>&> );
  VERIFY( vr == src );
  VERIFY( *i == src );
}

template<typename T>
struct PublicBase : std::polymorphic<T>
{
  using std::polymorphic<T>::polymorphic;
};

template<typename T>
class PrivateBase : std::polymorphic<T>
{
public:	
  using std::polymorphic<T>::polymorphic;
  using std::polymorphic<T>::operator*;
  using std::polymorphic<T>::operator->;
};

constexpr bool
test_all()
{
  test_access<std::polymorphic>();
  test_access<PublicBase>();
  test_access<PrivateBase>();
  return true;
}

int main()
{
  test_all();
//  static_assert(test_all());
}
