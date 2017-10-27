// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <map>
#include <testsuite_allocator.h>

using __gnu_test::SimpleAllocator;

static_assert(std::is_same_v<
	      decltype(std::map{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}}),
	      std::map<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::map{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}}}),
	      std::map<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::map{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}},
		    std::less<int>{}, {}}),
	      std::map<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::map{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}},
		    {}}),
	      std::map<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::map{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}},
		    {}, SimpleAllocator<std::pair<const int, double>>{}}),
	      std::map<int, double, std::less<int>,
	      SimpleAllocator<std::pair<const int, double>>>>);

void f()
{
  std::map<int, double> x;
  static_assert(std::is_same_v<
		decltype(std::map(x.begin(), x.end())),
		std::map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::map{x.begin(), x.end(),
		      std::less<int>{},
		      std::allocator<std::pair<const int, double>>{}}),
		std::map<int, double>>);
  
  static_assert(std::is_same_v<
		decltype(std::map{x.begin(), x.end(),
		      std::less<int>{}, {}}),
		std::map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::map(x.begin(), x.end(),
				  {})),
		std::map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::map{x.begin(), x.end(),
		      {},
		      std::allocator<std::pair<const int, double>>{}}),
		std::map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::map{x.begin(), x.end(),
		      {},
		      SimpleAllocator<std::pair<const int, double>>{}}),
		std::map<int, double, std::less<int>,
		SimpleAllocator<std::pair<const int, double>>>>);
}
