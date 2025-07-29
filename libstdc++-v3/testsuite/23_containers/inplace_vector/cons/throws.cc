// { dg-do run { target c++26 } }

#include <inplace_vector>

#include <span>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_allocator.h>

struct CopyFailed {};

struct Thrower
{
  static inline size_t throw_after = 0;
  static inline size_t incontainer = 0;

  Thrower() {}
  Thrower(int x) {}
  Thrower(const Thrower&)
  {
    if (incontainer >= throw_after)
      throw CopyFailed();
    ++incontainer;
  }

  ~Thrower()
  { --incontainer; }
};

template<template<class TT> class ItType>
void
do_test_it()
{
  // The vector's value_type.
  using V = Thrower;

  V a[]{1,2,3,4,5,6,7,8,9};
  using It = ItType<V>;

  auto bounds = typename It::ContainerType(a, a+9);
  Thrower::throw_after = 100;
  Thrower::incontainer = 0;
  try
  {
    std::inplace_vector<V, 5> v9(It(a, &bounds), It(a+9, &bounds));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( Thrower::incontainer == 0 );

  bounds = typename It::ContainerType(a, a+9);
  Thrower::throw_after = 2;
  Thrower::incontainer = 0;
  try
  {
    std::inplace_vector<V, 5> v2(It(a, &bounds), It(a+3, &bounds));
    VERIFY(false);
  }
  catch (CopyFailed const&)
  {
  }
  VERIFY( Thrower::incontainer == 0 );
}

bool
test_iterators()
{
  using namespace __gnu_test;
  do_test_it<input_iterator_wrapper>();
  do_test_it<forward_iterator_wrapper>();
  do_test_it<random_access_iterator_wrapper>();
  return true;
}

template<typename Range>
void
do_test_r()
{
  // The vector's value_type.
  using V = Thrower;

  V a[]{1,2,3,4,5,6,7,8,9};

  Thrower::throw_after = 100;
  Thrower::incontainer = 0;
  try
  {
    std::inplace_vector<V, 5> v9(std::from_range, Range(a, a+9));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( Thrower::incontainer == 0 );

  Thrower::throw_after = 2;
  Thrower::incontainer = 0;
  try
  {
    std::inplace_vector<V, 5> v9(std::from_range, Range(a, a+3));
    VERIFY(false);
  }
  catch (CopyFailed const&)
  {
  }
  VERIFY( Thrower::incontainer == 0 );
}

bool
test_ranges()
{
  using namespace __gnu_test;
  do_test_r<test_forward_range<Thrower>>();
  do_test_r<test_sized_range_sized_sent<Thrower, forward_iterator_wrapper>>();

  do_test_r<test_input_range<Thrower>>();
  do_test_r<test_input_sized_range<Thrower>>();
  do_test_r<test_sized_range_sized_sent<Thrower, input_iterator_wrapper>>();
  return true;
}

int main()
{
  test_iterators();
  test_ranges();
}

