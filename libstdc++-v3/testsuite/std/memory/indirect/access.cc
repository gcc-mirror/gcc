// { dg-do run { target c++26 } }

#include <memory>
#include <vector>

#include <testsuite_hooks.h>

template<template<typename> class Indirect>
constexpr void
test_access()
{
  const std::vector<int> src{1, 2, 3, 4, 5};
  Indirect<std::vector<int>> i(src);
  auto const& ci = i;
  VERIFY( *i == src );
  VERIFY( *ci == src );
  VERIFY( *std::move(ci) == src );

  std::vector<int>&& vr = *std::move(i);
  VERIFY( vr == src );
  VERIFY( *i == src );

  std::vector<int> vc = *std::move(i);
  VERIFY( vc == src );
  VERIFY( vr.empty() );
  VERIFY( i->empty() );
  VERIFY( ci->empty() );
}

template<typename T>
struct PublicBase : std::indirect<T>
{
  using std::indirect<T>::indirect;
};

template<typename T>
class PrivateBase : std::indirect<T>
{
public:	
  using std::indirect<T>::indirect;
  using std::indirect<T>::operator*;
  using std::indirect<T>::operator->;
};

constexpr bool
test_all()
{
  test_access<std::indirect>();
  test_access<PublicBase>();
  test_access<PrivateBase>();
  return true;
}

int main()
{
  test_all();
  static_assert(test_all());
}
