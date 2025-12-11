// { dg-do run { target c++26 } }

#include <inplace_vector>

#include <span>
#include <stdexcept>
#include <testsuite_hooks.h>
#include <utility>

template<size_t N, typename T>
constexpr void
test_out_of_capacity()
{
  std::inplace_vector<T, N> v;

#ifdef __cpp_exceptions
#ifndef __cpp_lib_constexpr_exceptions
  if consteval {
    return;
  }
#endif

  try
  {
    (void)v.at(N + 2);
    VERIFY(false);
  }
  catch (std::out_of_range const&)
  {
  }

  try
  {
    (void)as_const(v).at(N + 2);
    VERIFY(false);
  }
  catch (std::out_of_range const&)
  {
  }
#endif
}


template<bool Const, typename T, size_t N>
using InplaceVector = std::conditional_t<Const, 
					 const std::inplace_vector<T, N>,
					 std::inplace_vector<T, N>>;

template<bool Const, typename T>
constexpr void
test_access()
{
  InplaceVector<Const, T, 10> v{1, 2, 3, 4, 5};

  auto& e0a = v[0];
  auto& e0b = v.at(0);
  auto& e0c = v.front();
  VERIFY( &e0a == &e0b );
  VERIFY( &e0a == &e0c );
  VERIFY( &e0a == &v.begin()[0] );
  VERIFY( &e0a == &v.cbegin()[0] );
  VERIFY( &e0a == v.data() );
  VERIFY( e0a == T(1) );

  auto& e3a = v[2];
  auto& e3b = v.at(2);
  VERIFY( &e3a == &e3b );
  VERIFY( &e3a == &v.begin()[2] );
  VERIFY( &e3a == &v.cbegin()[2] );
  VERIFY( &e3a == v.data() + 2 );
  VERIFY( e3a == T(3) );

  auto& e4a = v[4];
  auto& e4b = v.at(4);
  auto& e4c = v.back();
  VERIFY( &e4a == &e4b );
  VERIFY( &e4a == &e4c );
  VERIFY( &e4a == &v.begin()[4] );
  VERIFY( &e4a == &v.cbegin()[4] );
  VERIFY( &e4a == v.data() + 4 );
  VERIFY( e4a == T(5) );

#ifdef __cpp_exceptions
#ifndef __cpp_lib_constexpr_exceptions
  if consteval {
    return;
  }
#endif

  try
  {
    (void)v.at(7);
    VERIFY(false);
  }
  catch (std::out_of_range const&)
  {
  }
#endif
}

int main()
{
  auto test_all = [] {
    test_out_of_capacity<0, int>();
    test_out_of_capacity<4, int>();
    test_access<true, int>();
    test_access<false, int>();
    return true;
  };

  test_all();
  static_assert(test_all());;
}
