// { dg-do compile { target c++17 } }

#include <unordered_set>
#include <testsuite_allocator.h>

using __gnu_test::SimpleAllocator;

static_assert(std::is_same_v<
	      decltype(std::unordered_set{1, 2, 3}),
	      std::unordered_set<int>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_set{{1, 2, 3},
		    0, std::hash<int>{}, std::equal_to<int>{}}),
	      std::unordered_set<int>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_set{{1, 2, 3},
		    0, std::hash<int>{}, std::allocator<int>{}}),
	      std::unordered_set<int>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_set{{1, 2, 3},
		    {}}),
	      std::unordered_set<int>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_set{{1, 2, 3},
		    1, std::allocator<int>{}}),
	      std::unordered_set<int>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_set{{1, 2, 3},
		    1, SimpleAllocator<int>{}}),
	      std::unordered_set<int, std::hash<int>,
	      std::equal_to<int>,
	      SimpleAllocator<int>>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_set{{1, 2, 3},
		    1, std::hash<int>{}, std::allocator<int>{}}),
	      std::unordered_set<int>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_set{{1, 2, 3},
		    1, std::hash<int>{}, SimpleAllocator<int>{}}),
	      std::unordered_set<int, std::hash<int>,
	      std::equal_to<int>,
	      SimpleAllocator<int>>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_set{{1, 2, 3},
		    {}, {}, {}, std::allocator<int>{}}),
	      std::unordered_set<int>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_set{{1, 2, 3},
		    {}, {}, {}, SimpleAllocator<int>{}}),
	      std::unordered_set<int, std::hash<int>,
	      std::equal_to<int>,
	      SimpleAllocator<int>>>);

void f()
{
  std::unordered_set<int> x;

  static_assert(std::is_same_v<
		decltype(std::unordered_set(x.begin(), x.end())),
		std::unordered_set<int>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_set{x.begin(), x.end(),
		      {},
		      std::hash<int>{},
		      std::equal_to<int>{},
		      std::allocator<int>{}}),
		std::unordered_set<int>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_set{x.begin(), x.end(),
		      {}, std::hash<int>{}, std::equal_to<int>{}}),
		std::unordered_set<int>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_set{x.begin(), x.end(),
		      {}, std::hash<int>{}, std::allocator<int>{}}),
		std::unordered_set<int>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_set(x.begin(), x.end(),
		      {})),
		std::unordered_set<int>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_set{x.begin(), x.end(), 1}),
		std::unordered_set<int>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_set{x.begin(), x.end(),
		      1,
		      std::allocator<int>{}}),
		std::unordered_set<int>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_set{x.begin(), x.end(),
		      1,
		      SimpleAllocator<int>{}}),
		std::unordered_set<int, std::hash<int>,
		std::equal_to<int>,
		SimpleAllocator<int>>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_set{x.begin(), x.end(),
		      1, std::hash<int>{},
		      std::allocator<int>{}}),
		std::unordered_set<int>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_set{x.begin(), x.end(),
		      1, std::hash<int>{},
		      SimpleAllocator<int>{}}),
		std::unordered_set<int, std::hash<int>,
		std::equal_to<int>,
		SimpleAllocator<int>>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_set{x.begin(), x.end(),
		      {}, {}, {},
		      std::allocator<int>{}}),
		std::unordered_set<int>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_set{x.begin(), x.end(),
		      {}, {}, {},
		      SimpleAllocator<int>{}}),
		std::unordered_set<int, std::hash<int>,
		std::equal_to<int>,
		SimpleAllocator<int>>>);
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

  using Hash = std::hash<unsigned long>;
  using Eq = std::equal_to<>;
  using USet = std::unordered_set<unsigned, Hash, Eq, Alloc<unsigned>>;
  Pool* p = nullptr;
  USet s(p);

  std::unordered_set s1(s, p);
  check_type<USet>(s1);

  std::unordered_set s2(std::move(s), p);
  check_type<USet>(s2);
}
