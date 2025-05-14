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
		    std::less<int>{}}),
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
				       std::less<int>{})),
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
				       std::less<int>{})),
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
				       std::less<int>{})),
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
  using MMap = std::multimap<unsigned, void*, std::greater<>, PairAlloc>;
  Pool* p = nullptr;
  MMap m(p);

  std::multimap s1(m, p);
  check_type<MMap>(s1);

  std::multimap s2(std::move(m), p);
  check_type<MMap>(s2);
}

struct MyPred
{
  template<typename T, typename U>
  bool operator()(T const&, U const&) const;
};

template<typename K, typename V>
constexpr bool test_lwg4223()
{
  using KD = std::remove_cv_t<std::remove_reference_t<K>>;
  using VD = std::remove_cv_t<std::remove_reference_t<V>>;
  using Alloc = __gnu_test::SimpleAllocator<std::pair<const KD, VD>>;

  std::initializer_list<std::pair<K, V>> il = {};
  Alloc a;
  MyPred p;

  // The remove_cvref_t is not applied here.
  // static_assert(std::is_same_v<
  //   decltype(std::multimap(il)),
  //   std::multimap<KD, VD>>);

  static_assert(std::is_same_v<
    decltype(std::multimap(il.begin(), il.end())),
    std::multimap<KD, VD>>);

  static_assert(std::is_same_v<
    decltype(std::multimap(il.begin(), il.end(), p)),
    std::multimap<KD, VD, MyPred>>);

  static_assert(std::is_same_v<
    decltype(std::multimap(il.begin(), il.end(), a)),
    std::multimap<KD, VD, std::less<KD>, Alloc>>);

  static_assert(std::is_same_v<
    decltype(std::multimap(il.begin(), il.end(), p, a)),
    std::multimap<KD, VD, MyPred, Alloc>>);

  return true;
}

static_assert(test_lwg4223<const int, const float>());
static_assert(test_lwg4223<int&, float&>());
static_assert(test_lwg4223<int&&, float&&>());
static_assert(test_lwg4223<const int&, const float&>());
