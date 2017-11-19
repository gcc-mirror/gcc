// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <set>
#include <testsuite_allocator.h>

using __gnu_test::SimpleAllocator;

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
		      {},
		      SimpleAllocator<int>{}}),
		std::set<int, std::less<int>, SimpleAllocator<int>>>);
}
