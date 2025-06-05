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
#ifdef __cpp_lib_constexpr_exceptions
#error remove the consteval check
#endif
  if consteval {
    return;
  }

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

template<typename T>
constexpr void
test_access()
{
  std::inplace_vector<T, 10> v{1, 2, 3, 4, 5};

  auto& e3a = v[2];
  auto& e3b = std::as_const(v).at(2);
  VERIFY( &e3a == &e3b );
  VERIFY( &e3a == &v.begin()[2] );
  VERIFY( &e3a == std::as_const(v).data() + 2 );
  VERIFY( e3a == T(3) );

  auto& e4a = as_const(v)[4];
  auto& e4b = v.at(4);
  VERIFY( &e4a == &e4b );
  VERIFY( &e4a == &v.cbegin()[4] );
  VERIFY( &e4a == v.data() + 4 );
  VERIFY( e4a == T(5) );

#ifdef __cpp_exceptions
#ifdef __cpp_lib_constexpr_exceptions
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  try
  {
    (void)v.at(7);
    VERIFY(false);
  }
  catch (std::out_of_range const&)
  {
  }

  try
  {
    (void)as_const(v).at(7);
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
    test_access<int>();
    return true;
  };

  test_all();
  static_assert(test_all());;
}
