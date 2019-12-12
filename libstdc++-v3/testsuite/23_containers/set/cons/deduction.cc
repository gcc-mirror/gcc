// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <set>
#include <testsuite_allocator.h>
#include <testsuite_iterators.h>

using __gnu_test::SimpleAllocator;
using value_type = std::set<int>::value_type;

static_assert(std::is_same_v<
	      decltype(std::set{1, 2, 3}),
	      std::set<int>>);

static_assert(std::is_same_v<
	      decltype(std::set{1, 2, 3}),
	      std::set<int>>);

static_assert(std::is_same_v<
	      decltype(std::set{{1, 2, 3},
		    std::less<int>{}, {}}),
	      std::set<int>>);

static_assert(std::is_same_v<
	      decltype(std::set{{1, 2, 3},
		    {}}),
	      std::set<int>>);

static_assert(std::is_same_v<
	      decltype(std::set{{1, 2, 3},
		    SimpleAllocator<int>{}}),
	      std::set<int, std::less<int>,
	      SimpleAllocator<int>>>);

static_assert(std::is_same_v<
	      decltype(std::set{{1, 2, 3},
		    {}, SimpleAllocator<int>{}}),
	      std::set<int, std::less<int>,
	      SimpleAllocator<int>>>);

void f()
{
  std::set<int> x;

  static_assert(std::is_same_v<
		decltype(std::set(x.begin(), x.end())),
		std::set<int>>);

  static_assert(std::is_same_v<
		decltype(std::set{x.begin(), x.end(),
		      std::less<int>{},
		      std::allocator<int>{}}),
		std::set<int>>);
  
  static_assert(std::is_same_v<
		decltype(std::set{x.begin(), x.end(),
		      std::less<int>{}, {}}),
		std::set<int>>);

  static_assert(std::is_same_v<
		decltype(std::set(x.begin(), x.end(),
				  {})),
		std::set<int>>);

  static_assert(std::is_same_v<
		decltype(std::set{x.begin(), x.end(),
		      {},
		      std::allocator<int>{}}),
		std::set<int>>);

  static_assert(std::is_same_v<
		decltype(std::set{x.begin(), x.end(),
				  SimpleAllocator<int>{}}),
		std::set<int, std::less<int>, SimpleAllocator<int>>>);

  static_assert(std::is_same_v<
		decltype(std::set{x.begin(), x.end(),
		      {},
		      SimpleAllocator<int>{}}),
		std::set<int, std::less<int>, SimpleAllocator<int>>>);
}

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;

void g()
{
  value_type array[1];
  test_container<value_type, input_iterator_wrapper> x(array);

  static_assert(std::is_same_v<
		decltype(std::set(x.begin(), x.end())),
		std::set<int>>);

  static_assert(std::is_same_v<
		decltype(std::set{x.begin(), x.end(),
				  std::less<int>{},
				  std::allocator<value_type>{}}),
		std::set<int>>);

  static_assert(std::is_same_v<
		decltype(std::set{x.begin(), x.end(),
				  std::less<int>{}, {}}),
		std::set<int>>);

  static_assert(std::is_same_v<
		decltype(std::set(x.begin(), x.end(),
				  {})),
		std::set<int>>);

  static_assert(std::is_same_v<
		decltype(std::set{x.begin(), x.end(),
				  std::allocator<value_type>{}}),
		std::set<int>>);

  static_assert(std::is_same_v<
		decltype(std::set{x.begin(), x.end(),
				  SimpleAllocator<value_type>{}}),
		std::set<int, std::less<int>,
			 SimpleAllocator<value_type>>>);

  static_assert(std::is_same_v<
		decltype(std::set{x.begin(), x.end(),
				  {},
				  std::allocator<value_type>{}}),
		std::set<int>>);

  static_assert(std::is_same_v<
		decltype(std::set{x.begin(), x.end(),
				  {},
				  SimpleAllocator<value_type>{}}),
		std::set<int, std::less<int>,
			 SimpleAllocator<value_type>>>);
}
