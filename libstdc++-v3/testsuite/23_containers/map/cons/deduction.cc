// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <map>
#include <testsuite_allocator.h>
#include <testsuite_iterators.h>

using __gnu_test::SimpleAllocator;
using value_type = std::map<int, double>::value_type;

static_assert(std::is_same_v<
	      decltype(std::map{value_type{1, 2.0}, {2, 3.0}, {3, 4.0}}),
	      std::map<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::map{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}}),
	      std::map<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::map{{value_type{1, 2.0}, {2, 3.0}, {3, 4.0}}}),
	      std::map<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::map{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}}}),
	      std::map<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::map{{value_type{1, 2.0}, {2, 3.0}, {3, 4.0}},
				std::less<int>{}, {}}),
	      std::map<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::map{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}},
				std::less<int>{}, {}}),
	      std::map<int, double>>);

/* This is not deducible, {} could be deduced as _Compare or _Allocator.
static_assert(std::is_same_v<
	      decltype(std::map{{value_type{1, 2.0}, {2, 3.0}, {3, 4.0}}, {}}),
	      std::map<int, double>>);
*/

static_assert(std::is_same_v<
	      decltype(std::map{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}}, {}}),
	      std::map<int, double>>);

/* This is not deducible, ambiguous candidates:
 * map(initializer_list<value_type>, const Compare&, const _Allocator& = {})
 * map(initializer_list<value_type>, const _Allocator&)
 * map(initializer_list<pair<Key, T>>, const _Allocator&) -> map
static_assert(std::is_same_v<
	      decltype(std::map{{value_type{1, 2.0}, {2, 3.0}, {3, 4.0}},
				SimpleAllocator<value_type>{}}),
	      std::map<int, double, std::less<int>,
		       SimpleAllocator<value_type>>>);
*/

static_assert(std::is_same_v<
	      decltype(std::map{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}},
				SimpleAllocator<value_type>{}}),
	      std::map<int, double, std::less<int>,
		       SimpleAllocator<value_type>>>);

static_assert(std::is_same_v<
	      decltype(std::map{{value_type{1, 2.0}, {2, 3.0}, {3, 4.0}},
				{}, SimpleAllocator<value_type>{}}),
	      std::map<int, double, std::less<int>,
		       SimpleAllocator<value_type>>>);

static_assert(std::is_same_v<
	      decltype(std::map{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}},
				{}, SimpleAllocator<value_type>{}}),
	      std::map<int, double, std::less<int>,
		       SimpleAllocator<value_type>>>);

void f()
{
  std::map<int, double> x;
  static_assert(std::is_same_v<
		decltype(std::map(x.begin(), x.end())),
		std::map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::map{x.begin(), x.end(),
				  std::less<int>{},
				  std::allocator<value_type>{}}),
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
				  std::allocator<value_type>{}}),
		std::map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::map{x.begin(), x.end(),
				  SimpleAllocator<value_type>{}}),
		std::map<int, double, std::less<int>,
			 SimpleAllocator<value_type>>>);

  static_assert(std::is_same_v<
		decltype(std::map{x.begin(), x.end(),
				  {},
				  std::allocator<value_type>{}}),
		std::map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::map{x.begin(), x.end(),
				  {},
				  SimpleAllocator<value_type>{}}),
		std::map<int, double, std::less<int>,
			 SimpleAllocator<value_type>>>);
}

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;

void g()
{
  value_type array[1];
  test_container<value_type, input_iterator_wrapper> x(array);

  static_assert(std::is_same_v<
		decltype(std::map(x.begin(), x.end())),
		std::map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::map{x.begin(), x.end(),
				  std::less<int>{},
				  std::allocator<value_type>{}}),
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
				  std::allocator<value_type>{}}),
		std::map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::map{x.begin(), x.end(),
				  SimpleAllocator<value_type>{}}),
		std::map<int, double, std::less<int>,
			 SimpleAllocator<value_type>>>);

  static_assert(std::is_same_v<
		decltype(std::map{x.begin(), x.end(),
				  {},
				  std::allocator<value_type>{}}),
		std::map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::map{x.begin(), x.end(),
				  {},
				  SimpleAllocator<value_type>{}}),
		std::map<int, double, std::less<int>,
			 SimpleAllocator<value_type>>>);
}

void h()
{
  std::pair<int, double> array[1];
  test_container<std::pair<int, double>, input_iterator_wrapper> x(array);

  static_assert(std::is_same_v<
		decltype(std::map(x.begin(), x.end())),
		std::map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::map{x.begin(), x.end(),
				  std::less<int>{},
				  std::allocator<value_type>{}}),
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
				  std::allocator<value_type>{}}),
		std::map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::map{x.begin(), x.end(),
				  SimpleAllocator<value_type>{}}),
		std::map<int, double, std::less<int>,
			 SimpleAllocator<value_type>>>);

  static_assert(std::is_same_v<
		decltype(std::map{x.begin(), x.end(),
				  {},
				  std::allocator<value_type>{}}),
		std::map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::map{x.begin(), x.end(),
				  {},
				  SimpleAllocator<value_type>{}}),
		std::map<int, double, std::less<int>,
			 SimpleAllocator<value_type>>>);
}
