// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <map>
#include <testsuite_allocator.h>
#include <testsuite_iterators.h>

using __gnu_test::SimpleAllocator;
using value_type = std::multimap<int, double>::value_type;

static_assert(std::is_same_v<
	      decltype(std::multimap{value_type{1, 2.0}, {2, 3.0}, {3, 4.0}}),
	      std::multimap<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::multimap{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}}),
	      std::multimap<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::multimap{{value_type{1, 2.0}, {2, 3.0}, {3, 4.0}}}),
	      std::multimap<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::multimap{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}}}),
	      std::multimap<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::multimap{{value_type{1, 2.0}, {2, 3.0}, {3, 4.0}},
				std::less<int>{}, {}}),
	      std::multimap<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::multimap{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}},
		    std::less<int>{}, {}}),
	      std::multimap<int, double>>);

/* This is not deducible, {} could be deduced as _Compare or _Allocator.
static_assert(std::is_same_v<
	      decltype(std::multimap{{value_type{1, 2.0}, {2, 3.0}, {3, 4.0}},
				     {}}),
	      std::multimap<int, double>>);
*/

static_assert(std::is_same_v<
	      decltype(std::multimap{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}},
		    {}}),
	      std::multimap<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::multimap{{value_type{1, 2.0}, {2, 3.0}, {3, 4.0}},
				     {}, SimpleAllocator<value_type>{}}),
	      std::multimap<int, double, std::less<int>,
		       SimpleAllocator<value_type>>>);

static_assert(std::is_same_v<
	      decltype(std::multimap{{std::pair{1, 2.0}, {2, 3.0}, {3, 4.0}},
				     {}, SimpleAllocator<value_type>{}}),
	      std::multimap<int, double, std::less<int>,
			    SimpleAllocator<value_type>>>);

void f()
{
  std::multimap<int, double> x;
  static_assert(std::is_same_v<
		decltype(std::multimap(x.begin(), x.end())),
		std::multimap<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::multimap{x.begin(), x.end(),
				       std::less<int>{},
				       std::allocator<value_type>{}}),
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
		      std::allocator<value_type>{}}),
		std::multimap<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::multimap{x.begin(), x.end(),
				       {},
				       SimpleAllocator<value_type>{}}),
		std::multimap<int, double, std::less<int>,
			      SimpleAllocator<value_type>>>);
}

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;

void g()
{
  value_type array[1];
  test_container<value_type, input_iterator_wrapper> x(array);

  static_assert(std::is_same_v<
		decltype(std::multimap(x.begin(), x.end())),
		std::multimap<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::multimap{x.begin(), x.end(),
				       std::less<int>{},
				       std::allocator<value_type>{}}),
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
				       std::allocator<value_type>{}}),
		std::multimap<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::multimap{x.begin(), x.end(),
				       {},
				       SimpleAllocator<value_type>{}}),
		std::multimap<int, double, std::less<int>,
			 SimpleAllocator<value_type>>>);
}

void h()
{
  std::pair<int, double> array[1];
  test_container<std::pair<int, double>, input_iterator_wrapper> x(array);

  static_assert(std::is_same_v<
		decltype(std::multimap(x.begin(), x.end())),
		std::multimap<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::multimap{x.begin(), x.end(),
				       std::less<int>{},
				       std::allocator<value_type>{}}),
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
				       std::allocator<value_type>{}}),
		std::multimap<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::multimap{x.begin(), x.end(),
				       {},
				       SimpleAllocator<value_type>{}}),
		std::multimap<int, double, std::less<int>,
			      SimpleAllocator<value_type>>>);
}
