// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <set>
#include <testsuite_allocator.h>

using __gnu_test::SimpleAllocator;

static_assert(std::is_same_v<
	      decltype(std::multiset{1, 2, 3}),
	      std::multiset<int>>);

static_assert(std::is_same_v<
	      decltype(std::multiset{1, 2, 3}),
	      std::multiset<int>>);

static_assert(std::is_same_v<
	      decltype(std::multiset{{1, 2, 3},
		    std::less<int>{}, {}}),
	      std::multiset<int>>);

static_assert(std::is_same_v<
	      decltype(std::multiset{{1, 2, 3},
		    {}}),
	      std::multiset<int>>);

static_assert(std::is_same_v<
	      decltype(std::multiset{{1, 2, 3},
		    {}, SimpleAllocator<int>{}}),
	      std::multiset<int, std::less<int>,
	      SimpleAllocator<int>>>);

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
		      {},
		      std::allocator<int>{}}),
		std::multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::multiset{x.begin(), x.end(),
		      {},
		      SimpleAllocator<int>{}}),
		std::multiset<int, std::less<int>, SimpleAllocator<int>>>);
}
