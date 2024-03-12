// { dg-do compile { target c++20 } }
// { dg-add-options no_pch }

// Verify P2520R0 changes to move_iterator's iterator_concept, which we treat
// as a DR against C++20.

#include <iterator>
#if __cpp_lib_move_iterator_concept != 202207L
# error "Feature-test macro __cpp_lib_move_iterator_concept has wrong value in <iterator>"
#endif

#undef __cpp_lib_move_iterator_concept
#include <version>
#if __cpp_lib_move_iterator_concept != 202207L
# error "Feature-test macro __cpp_lib_move_iterator_concept has wrong value in <version>"
#endif

#include <testsuite_iterators.h>

using __gnu_test::test_input_range;
using __gnu_test::test_forward_range;
using __gnu_test::test_bidirectional_range;
using __gnu_test::test_random_access_range;

using ty1 = std::move_iterator<decltype(std::declval<test_input_range<int>&>().begin())>;
static_assert(std::same_as<ty1::iterator_concept, std::input_iterator_tag>);

using ty2 = std::move_iterator<decltype(std::declval<test_forward_range<int>&>().begin())>;
static_assert(std::same_as<ty2::iterator_concept, std::forward_iterator_tag>);

using ty3 = std::move_iterator<decltype(std::declval<test_bidirectional_range<int>&>().begin())>;
static_assert(std::same_as<ty3::iterator_concept, std::bidirectional_iterator_tag>);

using ty4 = std::move_iterator<decltype(std::declval<test_random_access_range<int>&>().begin())>;
static_assert(std::same_as<ty4::iterator_concept, std::random_access_iterator_tag>);

static_assert(std::random_access_iterator<std::move_iterator<int*>>);
