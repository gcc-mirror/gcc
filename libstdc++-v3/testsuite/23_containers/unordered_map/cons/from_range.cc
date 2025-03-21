// { dg-do run { target c++23 } }

#include <unordered_map>

#if __cpp_lib_containers_ranges != 202202L
# error "Feature-test macro __cpp_lib_containers_ranges has wrong value in <unordered_map>"
#endif

#include <algorithm>
#include <span>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

struct StateHash {
  int state = 17;

  template<typename T>
  size_t operator()(T const& t) const {
    return std::hash<T>()(t) + 43;
  }
};

struct StateEq {
  int state = 7;

  template<typename T, typename U>
  bool operator()(T const& l, U const & r) const {
    return l == r;
  }
};

void
test_deduction_guide()
{
  __gnu_test::test_input_range<std::pair<long, float>> r(0, 0);
  std::unordered_map m(std::from_range, r);
  static_assert(std::is_same_v<decltype(m), std::unordered_map<long, float>>);

  std::unordered_map m2(std::from_range, r, 0);
  static_assert(std::is_same_v<decltype(m2), std::unordered_map<long, float>>);

  StateHash hf;
  std::unordered_map m3(std::from_range, r, 0, hf);
  static_assert(std::is_same_v<
    decltype(m3),
    std::unordered_map<long, float, StateHash>>);

  StateEq eq;
  std::unordered_map m4(std::from_range, r, 0, hf, eq);
  static_assert(std::is_same_v<
    decltype(m4),
    std::unordered_map<long, float, StateHash, StateEq>>);

  using Alloc = __gnu_test::SimpleAllocator<std::pair<const long, float>>;
  Alloc alloc;
  std::unordered_map m5(std::from_range, r, alloc);
  static_assert(std::is_same_v<
    decltype(m5),
    std::unordered_map<long, float,
		       std::hash<long>, std::equal_to<long>, Alloc>>);

  std::unordered_map m6(std::from_range, r, 0, alloc);
  static_assert(std::is_same_v<
    decltype(m6),
    std::unordered_map<long, float,
		       std::hash<long>, std::equal_to<long>, Alloc>>);

  std::unordered_map m7(std::from_range, r, 0, hf, alloc);
  static_assert(std::is_same_v<
    decltype(m7),
    std::unordered_map<long, float, StateHash, std::equal_to<long>, Alloc>>);

  std::unordered_map m8(std::from_range, r, 0, hf, eq, alloc);
  static_assert(std::is_same_v<
    decltype(m8),
    std::unordered_map<long, float, StateHash, StateEq, Alloc>>);

  __gnu_test::test_input_range<std::pair<const long, const float>> r2(0, 0);
  std::unordered_map m9(std::from_range, r2);
  static_assert(std::is_same_v<
    decltype(m9),
    std::unordered_map<long, const float>>);

  // LWG4223: deduces map<const long&, float&>
  // __gnu_test::test_input_range<std::pair<const long&, float&>> r3(0, 0);
  // std::unordered_map m10(std::from_range, r3);

  __gnu_test::test_input_range<std::tuple<long, float>> r4(0, 0);
  std::unordered_map m11(std::from_range, r4);
  static_assert(std::is_same_v<decltype(m11), std::unordered_map<long, float>>);
  std::unordered_map it11(r4.begin(), r4.begin());
  static_assert(std::is_same_v<decltype(it11), std::unordered_map<long, float>>);
}

template<typename T, typename U>
constexpr bool is_equal(std::hash<T>, std::hash<U>)
{ return true; }

template<typename T, typename U>
constexpr bool is_equal(std::equal_to<T>, std::equal_to<U>)
{ return true; }

constexpr bool is_equal(StateHash lhs, StateHash rhs)
{ return lhs.state = rhs.state; }

constexpr bool is_equal(StateEq lhs, StateEq rhs)
{ return lhs.state = rhs.state; }

template<typename Range, typename Alloc, typename Hash, typename Equal>
constexpr void
do_test(Alloc alloc, Hash hf, Equal eqf)
{
  // The map's value_type, key_type and mapped_type.
  using P = typename Alloc::value_type;
  using K = typename P::first_type;
  using V = typename P::second_type;

  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{{1,2},{2,3},{3,4},{4,5},{5,6},{6,7},{7,8},{8,9},{9,0},
	{1,1},{2,2},{3,3},{4,4},{5,5}};

  auto eq = [&](std::unordered_map<K, V, Hash, Equal, Alloc> const& l,
		std::span<T> r) {
    if (l.size() != r.size())
      return false;

    return std::ranges::is_permutation(l, r);
  };

  std::unordered_map<K, V, Hash, Equal, Alloc>
    m0(std::from_range, Range(a, a+0));
  VERIFY( m0.empty() );
  VERIFY( is_equal(m0.hash_function(), Hash()) );
  VERIFY( is_equal(m0.key_eq(), Equal()) );
  VERIFY( m0.get_allocator() == Alloc() );

  std::unordered_map<K, V, Hash, Equal, Alloc>
    m4(std::from_range, Range(a, a+4), 2);
  VERIFY( eq(m4, {a, 4}) );
  VERIFY( m4.bucket_count() >= 2 );
  VERIFY( is_equal(m4.hash_function(), Hash()) );
  VERIFY( is_equal(m4.key_eq(), Equal()) );
  VERIFY( m4.get_allocator() == Alloc() );

  std::unordered_map<K, V, Hash, Equal, Alloc>
    m7(std::from_range, Range(a, a+7), 3, hf);
  VERIFY( eq(m7, {a, 7}) );
  VERIFY( m7.bucket_count() >= 3 );
  VERIFY( is_equal(m7.hash_function(), hf) );
  VERIFY( is_equal(m7.key_eq(), Equal()) );
  VERIFY( m7.get_allocator() == Alloc() );

  std::unordered_map<K, V, Hash, Equal, Alloc>
    m9(std::from_range, Range(a, a+9), 5, hf, eqf);
  VERIFY( eq(m9, {a, 9}) );
  VERIFY( m9.bucket_count() >= 5 );
  VERIFY( m9.get_allocator() == Alloc() );
  VERIFY( is_equal(m9.hash_function(), hf) );
  VERIFY( is_equal(m9.key_eq(), eqf) );

  std::unordered_map<K, V, Hash, Equal, Alloc>
    ma1(std::from_range, Range(a, a+14), alloc);
  VERIFY( eq(ma1, {a, 9}) );
  VERIFY( is_equal(ma1.hash_function(), Hash()) );
  VERIFY( is_equal(ma1.key_eq(), Equal()) );
  VERIFY( ma1.get_allocator() == alloc );

  std::unordered_map<K, V, Hash, Equal, Alloc>
    ma2(std::from_range, Range(a, a+14), 2, alloc);
  VERIFY( eq(ma2, {a, 9}) );
  VERIFY( ma2.bucket_count() >= 2 );
  VERIFY( is_equal(ma2.hash_function(), Hash()) );
  VERIFY( is_equal(ma2.key_eq(), Equal()) );
  VERIFY( ma2.get_allocator() == alloc );

  std::unordered_map<K, V, Hash, Equal, Alloc>
    ma3(std::from_range, Range(a, a+14), 3, hf, alloc);
  VERIFY( eq(ma3, {a, 9}) );
  VERIFY( ma3.bucket_count() >= 3 );
  VERIFY( is_equal(ma3.hash_function(), hf) );
  VERIFY( is_equal(ma3.key_eq(), Equal()) );
  VERIFY( ma3.get_allocator() == alloc );

  std::unordered_map<K, V, Hash, Equal, Alloc>
    ma4(std::from_range, Range(a, a+14), 5, hf, eqf, alloc);
  VERIFY( eq(ma4, {a, 9}) );
  VERIFY( ma4.bucket_count() >= 5 );
  VERIFY( is_equal(ma4.hash_function(), hf) );
  VERIFY( is_equal(ma4.key_eq(), eqf) );
  VERIFY( ma4.get_allocator() == alloc );
}

template<typename Range>
void
do_test_ahe()
{
  do_test<Range>(std::allocator<std::pair<const int, double>>(),
		 std::hash<int>(), std::equal_to<int>());
  do_test<Range>(std::allocator<std::pair<const int, double>>(),
		 StateHash{27}, StateEq{17});
  do_test<Range>(__gnu_test::uneq_allocator<std::pair<const int, double>>(42),
		 std::hash<int>(), std::equal_to<int>());
  do_test<Range>(__gnu_test::uneq_allocator<std::pair<const int, double>>(42),
		 StateHash{27}, StateEq{17});
}

struct MyPair {
  long x;
  long y;

  constexpr operator std::pair<int const, double>() const
  { return {x, y}; }

  friend bool operator==(MyPair, MyPair) = default;
  constexpr friend bool operator==(MyPair lhs, std::pair<int const, double> rhs)
  { return (lhs.x == rhs.first) && (lhs.y == rhs.second); }
};

bool
test_ranges()
{
  using namespace __gnu_test;

  do_test_ahe<test_forward_range<std::pair<int, double>>>();
  do_test_ahe<test_forward_range<std::pair<short, float>>>();
  do_test_ahe<test_forward_range<std::tuple<int, double>>>();
  do_test_ahe<test_forward_range<MyPair>>();

  return true;
}

int main()
{
  test_ranges();
}
