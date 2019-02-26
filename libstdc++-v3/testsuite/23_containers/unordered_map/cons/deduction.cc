// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <unordered_map>
#include <testsuite_allocator.h>

using __gnu_test::SimpleAllocator;

static_assert(std::is_same_v<
	      decltype(std::unordered_map{std::pair{1, 2.0},
		    {2, 3.0}, {3, 4.0}}),
	      std::unordered_map<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_map{{std::pair{1, 2.0},
		      {2, 3.0}, {3, 4.0}}}),
	      std::unordered_map<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_map{
		{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}},
		1}),
	      std::unordered_map<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_map{{std::pair{1, 2.0},
		      {2, 3.0}, {3, 4.0}},
		    {}, std::hash<int>{}, {}}),
	      std::unordered_map<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_map{{std::pair{1, 2.0},
		      {2, 3.0}, {3, 4.0}},
		    {}}),
	      std::unordered_map<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_map{{std::pair{1, 2.0},
		      {2, 3.0}, {3, 4.0}},
		    {}, {}, {},
		    SimpleAllocator<std::pair<const int, double>>{}}),
	      std::unordered_map<int, double, std::hash<int>,
	      std::equal_to<int>,
	      SimpleAllocator<std::pair<const int, double>>>>);


void f()
{
  std::unordered_map<int, double> x;

  static_assert(std::is_same_v<
		decltype(std::unordered_map(x.begin(), x.end())),
		std::unordered_map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_map{x.begin(), x.end(),
		      {}, std::hash<int>{}, {},
		      std::allocator<std::pair<const int, double>>{}}),
		std::unordered_map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_map{x.begin(), x.end(),
		      {}, std::hash<int>{}, {}}),
		std::unordered_map<int, double>>);
  
  static_assert(std::is_same_v<
		decltype(std::unordered_map(x.begin(), x.end(),
		      {})),
		std::unordered_map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_map{x.begin(), x.end(), 1}),
		std::unordered_map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_map{x.begin(), x.end(),
		      1,
		      std::allocator<std::pair<const int, double>>{}}),
		std::unordered_map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_map{x.begin(), x.end(),
		      1,
		      SimpleAllocator<std::pair<const int, double>>{}}),
		std::unordered_map<int, double, std::hash<int>,
		std::equal_to<int>,
		SimpleAllocator<std::pair<const int, double>>>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_map{x.begin(), x.end(),
		      1, std::hash<int>{},
		      std::allocator<std::pair<const int, double>>{}}),
		std::unordered_map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_map{x.begin(), x.end(),
		      1, std::hash<int>{},
		      SimpleAllocator<std::pair<const int, double>>{}}),
		std::unordered_map<int, double, std::hash<int>,
		std::equal_to<int>,
		SimpleAllocator<std::pair<const int, double>>>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_map{x.begin(), x.end(),
		      {}, {}, {},
		      std::allocator<std::pair<const int, double>>{}}),
		std::unordered_map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_map{x.begin(), x.end(),
		      {}, {}, {},
		      SimpleAllocator<std::pair<const int, double>>{}}),
		std::unordered_map<int, double, std::hash<int>,
		std::equal_to<int>,
		SimpleAllocator<std::pair<const int, double>>>>);
}
