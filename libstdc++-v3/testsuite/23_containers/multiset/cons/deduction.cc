// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <set>
#include <testsuite_allocator.h>
#include <testsuite_iterators.h>

using __gnu_test::SimpleAllocator;
using value_type = std::multiset<int>::value_type;

static_assert(std::is_same_v<
	      decltype(std::multiset{1, 2, 3}),
	      std::multiset<int>>);

static_assert(std::is_same_v<
	      decltype(std::multiset{1, 2, 3}),
	      std::multiset<int>>);

static_assert(std::is_same_v<
	      decltype(std::multiset{{1, 2, 3}, std::less<int>{}, {}}),
	      std::multiset<int>>);

static_assert(std::is_same_v<
	      decltype(std::multiset{{1, 2, 3}, {}}),
	      std::multiset<int>>);

static_assert(std::is_same_v<
	      decltype(std::multiset{{1, 2, 3}, SimpleAllocator<int>{}}),
	      std::multiset<int, std::less<int>, SimpleAllocator<int>>>);

static_assert(std::is_same_v<
	      decltype(std::multiset{{1, 2, 3}, {}, SimpleAllocator<int>{}}),
	      std::multiset<int, std::less<int>, SimpleAllocator<int>>>);

void f()
{
  std::multiset<int> x;

  static_assert(std::is_same_v<
		decltype(std::multiset(x.begin(), x.end())),
		std::multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::multiset{x.begin(), x.end(),
		      std::less<int>{},
		      std::allocator<int>{}}),
		std::multiset<int>>);
  
  static_assert(std::is_same_v<
		decltype(std::multiset{x.begin(), x.end(),
		      std::less<int>{}, {}}),
		std::multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::multiset(x.begin(), x.end(),
				  {})),
		std::multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::multiset{x.begin(), x.end(),
				       std::allocator<int>{}}),
		std::multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::multiset{x.begin(), x.end(),
				       SimpleAllocator<int>{}}),
		std::multiset<int, std::less<int>, SimpleAllocator<int>>>);

  static_assert(std::is_same_v<
		decltype(std::multiset{x.begin(), x.end(),
		      {},
		      std::allocator<int>{}}),
		std::multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::multiset{x.begin(), x.end(),
		      {},
		      SimpleAllocator<int>{}}),
		std::multiset<int, std::less<int>, SimpleAllocator<int>>>);
}

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;

void g()
{
  value_type array[1];
  test_container<value_type, input_iterator_wrapper> x(array);

  static_assert(std::is_same_v<
		decltype(std::multiset(x.begin(), x.end())),
		std::multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::multiset{x.begin(), x.end(),
				  std::less<int>{},
				  std::allocator<value_type>{}}),
		std::multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::multiset{x.begin(), x.end(),
				  std::less<int>{}, {}}),
		std::multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::multiset(x.begin(), x.end(),
				  {})),
		std::multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::multiset{x.begin(), x.end(),
				  std::allocator<value_type>{}}),
		std::multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::multiset{x.begin(), x.end(),
				  SimpleAllocator<value_type>{}}),
		std::multiset<int, std::less<int>,
			 SimpleAllocator<value_type>>>);

  static_assert(std::is_same_v<
		decltype(std::multiset{x.begin(), x.end(),
				  {},
				  std::allocator<value_type>{}}),
		std::multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::multiset{x.begin(), x.end(),
				  {},
				  SimpleAllocator<value_type>{}}),
		std::multiset<int, std::less<int>,
			 SimpleAllocator<value_type>>>);
}
