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
		    std::less<int>{}}),
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
				  std::less<int>{})),
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
				  std::less<int>{})),
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

template<typename T, typename U> struct require_same;
template<typename T> struct require_same<T, T> { using type = void; };

template<typename T, typename U>
  typename require_same<T, U>::type
  check_type(U&) { }

struct Pool;

template<typename T>
struct Alloc : __gnu_test::SimpleAllocator<T>
{
  Alloc(Pool*) { }

  template<typename U>
    Alloc(const Alloc<U>&) { }
};

void
test_p1518r2()
{
  // P1518R2 - Stop overconstraining allocators in container deduction guides.
  // This is a C++23 feature but we support it for C++17 too.

  using Set = std::set<unsigned, std::greater<>, Alloc<unsigned>>;
  Pool* p = nullptr;
  Set s(p);

  std::set s1(s, p);
  check_type<Set>(s1);

  std::set s2(std::move(s), p);
  check_type<Set>(s2);
}
