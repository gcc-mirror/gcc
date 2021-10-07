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
		    {}, std::hash<int>{}, std::equal_to<int>{}}),
	      std::unordered_map<int, double>>);

static_assert(std::is_same_v<
	      decltype(std::unordered_map{{std::pair{1, 2.0},
		      {2, 3.0}, {3, 4.0}},
		    {}, std::hash<int>{}, std::allocator<std::pair<const int, double>>{}}),
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
		      {}, std::hash<int>{}, std::equal_to<int>{}}),
		std::unordered_map<int, double>>);

  static_assert(std::is_same_v<
		decltype(std::unordered_map{x.begin(), x.end(),
		      {}, std::hash<int>{}, std::allocator<std::pair<const int, double>>{}}),
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

  using PairAlloc = Alloc<std::pair<const unsigned, void*>>;
  using Hash = std::hash<unsigned long>;
  using Eq = std::equal_to<>;
  using UMap = std::unordered_map<unsigned, void*, Hash, Eq, PairAlloc>;
  Pool* p = nullptr;
  UMap m(p);

  std::unordered_map s1(m, p);
  check_type<UMap>(s1);

  std::unordered_map s2(std::move(m), p);
  check_type<UMap>(s2);
}
