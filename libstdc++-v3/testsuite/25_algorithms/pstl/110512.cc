// { dg-do compile { target c++17 } }

// Bug 110512 - C++20 random access iterators run sequentially with PSTL

#include <algorithm>
#include <execution>
#include <ranges>
#include <testsuite_iterators.h>

using InputIter = __gnu_test::input_iterator_wrapper<int>;
using FwdIter = __gnu_test::forward_iterator_wrapper<long>;
using RAIter = __gnu_test::random_access_iterator_wrapper<float>;

template<typename... Iter>
constexpr bool all_random_access
  = __pstl::__internal::__are_random_access_iterators<Iter...>::value;

using __pstl::__internal::__are_random_access_iterators;
static_assert( all_random_access<RAIter> );
static_assert( all_random_access<int*, RAIter, const long*> );
static_assert( ! all_random_access<RAIter, FwdIter> );
static_assert( ! all_random_access<FwdIter, InputIter, RAIter> );

#if __cpp_lib_ranges
using IotaIter = std::ranges::iterator_t<std::ranges::iota_view<int, int>>;
static_assert( std::random_access_iterator<IotaIter> );
static_assert( all_random_access<IotaIter> );
static_assert( all_random_access<IotaIter, RAIter> );
static_assert( all_random_access<RAIter, IotaIter> );
static_assert( ! all_random_access<RAIter, IotaIter, FwdIter> );
#endif
