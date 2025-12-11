// { dg-do run { target c++26 } }

#include <inplace_vector>

#include <span>
#include <testsuite_hooks.h>

template<size_t N, typename T>
constexpr void
test_reserve()
{
  std::inplace_vector<T, N> v;

  static_assert(v.max_size() == N);
  static_assert(v.capacity() == N);

  // static methods
  v.shrink_to_fit();
  v.reserve(0);
  v.reserve(N);

#ifdef __cpp_exceptions
#ifndef __cpp_lib_constexpr_exceptions
  if consteval {
    return;
  }
#endif

  try
  {
    v.reserve(N + 2);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
#endif
}

int main()
{
  auto test_all = [] {
    test_reserve<0, int>();
    test_reserve<4, int>();
    return true;
  };

  test_all();
  static_assert(test_all());;
}
