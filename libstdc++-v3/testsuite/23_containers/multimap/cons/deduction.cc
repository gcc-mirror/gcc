// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <map>
#include <testsuite_allocator.h>

using __gnu_test::SimpleAllocator;

static_assert(std::is_same_v<
	      decltype(std::multimap{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}}),
	      std::multimap<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::multimap{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}}}),
	      std::multimap<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::multimap{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}},
		    std::less<int>{}, {}}),
	      std::multimap<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::multimap{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}},
		    {}}),
	      std::multimap<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::multimap{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}},
		    {}, SimpleAllocator<std::pair<const int, double>>{}}),
	      std::multimap<int, double, std::less<int>,
	      SimpleAllocator<std::pair<const int, double>>>>);

void f()
{
  std::multimap<int, double> x;
  static_assert(std::is_same_v<
		decltype(std::multimap(x.begin(), x.end())),
		std::multimap<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::multimap{x.begin(), x.end(),
		      std::less<int>{},
		      std::allocator<std::pair<const int, double>>{}}),
		std::multimap<int, double>>);
  
  static_assert(std::is_same_v<
		decltype(std::multimap{x.begin(), x.end(),
		      std::less<int>{}, {}}),
		std::multimap<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::multimap(x.begin(), x.end(),
				  {})),
		std::multimap<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::multimap{x.begin(), x.end(),
		      {},
		      std::allocator<std::pair<const int, double>>{}}),
		std::multimap<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::multimap{x.begin(), x.end(),
		      {},
		      SimpleAllocator<std::pair<const int, double>>{}}),
		std::multimap<int, double, std::less<int>,
		SimpleAllocator<std::pair<const int, double>>>>);
}
