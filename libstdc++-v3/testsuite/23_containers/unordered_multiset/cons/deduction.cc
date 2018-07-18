// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <unordered_set>
#include <testsuite_allocator.h>

using __gnu_test::SimpleAllocator;

static_assert(std::is_same_v<
	      decltype(std::unordered_multiset{1, 2, 3}),
	      std::unordered_multiset<int>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_multiset{1, 2, 3}),
	      std::unordered_multiset<int>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_multiset{{1, 2, 3},
		    0, std::hash<int>{}, {}}),
	      std::unordered_multiset<int>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_multiset{{1, 2, 3},
		    {}}),
	      std::unordered_multiset<int>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_multiset{{1, 2, 3},
		    {}, {}, {}, std::allocator<int>{}}),
	      std::unordered_multiset<int>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_multiset{{1, 2, 3},
		    {}, {}, {}, SimpleAllocator<int>{}}),
	      std::unordered_multiset<int, std::hash<int>,
	      std::equal_to<int>,
	      SimpleAllocator<int>>>);

void f()
{
  std::unordered_multiset<int> x;

  static_assert(std::is_same_v<
		decltype(std::unordered_multiset(x.begin(), x.end())),
		std::unordered_multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_multiset{x.begin(), x.end(),
		      {},
		      std::hash<int>{},
		      std::equal_to<int>{},
		      std::allocator<int>{}}),
		std::unordered_multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_multiset{x.begin(), x.end(),
		      {}, std::hash<int>{}, {}}),
		std::unordered_multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_multiset(x.begin(), x.end(),
				  {})),
		std::unordered_multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_multiset{x.begin(), x.end(),
		      {}, {}, {},
		      std::allocator<int>{}}),
		std::unordered_multiset<int>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_multiset{x.begin(), x.end(),
		      {}, {}, {},
		      SimpleAllocator<int>{}}),
		std::unordered_multiset<int, std::hash<int>,
		std::equal_to<int>,
		SimpleAllocator<int>>>);
}
