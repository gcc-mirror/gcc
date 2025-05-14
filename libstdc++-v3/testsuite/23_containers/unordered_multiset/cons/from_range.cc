// { dg-do run { target c++23 } }

#include <unordered_set>

#if __cpp_lib_containers_ranges != 202202L
# error "Feature-test macro __cpp_lib_containers_ranges has wrong value in <unordered_set>"
#endif

#include <algorithm>
#include <ranges>
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
test_deduction_guide(long* p)
{
  __gnu_test::test_input_range<long> r(p, p);
  std::unordered_multiset s(std::from_range, r);
  static_assert(std::is_same_v<decltype(s), std::unordered_multiset<long>>);

  std::unordered_multiset s2(std::from_range, r, 0);
  static_assert(std::is_same_v<decltype(s2), std::unordered_multiset<long>>);

  StateHash hf;
  std::unordered_multiset s3(std::from_range, r, 0, hf);
  static_assert(std::is_same_v<
    decltype(s3),
    std::unordered_multiset<long, StateHash>>);

  StateEq eq;
  std::unordered_multiset s4(std::from_range, r, 0, hf, eq);
  static_assert(std::is_same_v<
    decltype(s4),
    std::unordered_multiset<long, StateHash, StateEq>>);

  using Alloc = __gnu_test::SimpleAllocator<long>;
  Alloc alloc;
  std::unordered_multiset s5(std::from_range, r, alloc);
  static_assert(std::is_same_v<
    decltype(s5),
    std::unordered_multiset<long, std::hash<long>, std::equal_to<long>, Alloc>>);

  std::unordered_multiset s6(std::from_range, r, 0, alloc);
  static_assert(std::is_same_v<
    decltype(s6),
    std::unordered_multiset<long, std::hash<long>, std::equal_to<long>, Alloc>>);

  std::unordered_multiset s7(std::from_range, r, 0, hf, alloc);
  static_assert(std::is_same_v<
    decltype(s7),
    std::unordered_multiset<long, StateHash, std::equal_to<long>, Alloc>>);

  std::unordered_multiset s8(std::from_range, r, 0, hf, eq, alloc);
  static_assert(std::is_same_v<
    decltype(s8),
    std::unordered_multiset<long, StateHash, StateEq, Alloc>>);
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
  // The unordered_multiset's value_typ.
  using V = typename Alloc::value_type;

  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9,1,2,3,4,5};

  auto eq = [&](std::unordered_multiset<V, Hash, Equal, Alloc> const& l,
		std::span<T> r) {
    if (l.size() != r.size())
      return false;

    return std::ranges::is_permutation(l, r, eqf);
  };

  std::unordered_multiset<V, Hash, Equal, Alloc>
    s0(std::from_range, Range(a, a+0));
  VERIFY( s0.empty() );
  VERIFY( is_equal(s0.hash_function(), Hash()) );
  VERIFY( is_equal(s0.key_eq(), Equal()) );
  VERIFY( s0.get_allocator() == Alloc() );

  std::unordered_multiset<V, Hash, Equal, Alloc>
    s4(std::from_range, Range(a, a+4), 2);
  VERIFY( eq(s4, {a, 4}) );
  VERIFY( s4.bucket_count() >= 2 );
  VERIFY( is_equal(s4.hash_function(), Hash()) );
  VERIFY( is_equal(s4.key_eq(), Equal()) );
  VERIFY( s4.get_allocator() == Alloc() );

  std::unordered_multiset<V, Hash, Equal, Alloc>
    s7(std::from_range, Range(a, a+7), 3, hf);
  VERIFY( eq(s7, {a, 7}) );
  VERIFY( s7.bucket_count() >= 3 );
  VERIFY( is_equal(s7.hash_function(), hf) );
  VERIFY( is_equal(s7.key_eq(), Equal()) );
  VERIFY( s7.get_allocator() == Alloc() );

  std::unordered_multiset<V, Hash, Equal, Alloc>
    s9(std::from_range, Range(a, a+9), 5, hf, eqf);
  VERIFY( eq(s9, {a, 9}) );
  VERIFY( s9.bucket_count() >= 5 );
  VERIFY( s9.get_allocator() == Alloc() );
  VERIFY( is_equal(s9.hash_function(), hf) );
  VERIFY( is_equal(s9.key_eq(), eqf) );

  std::unordered_multiset<V, Hash, Equal, Alloc>
    sa1(std::from_range, Range(a, a+14), alloc);
  VERIFY( eq(sa1, {a, 14}) );
  VERIFY( is_equal(sa1.hash_function(), Hash()) );
  VERIFY( is_equal(sa1.key_eq(), Equal()) );
  VERIFY( sa1.get_allocator() == alloc );

  std::unordered_multiset<V, Hash, Equal, Alloc>
    sa2(std::from_range, Range(a, a+14), 2, alloc);
  VERIFY( eq(sa2, {a, 14}) );
  VERIFY( sa2.bucket_count() >= 2 );
  VERIFY( is_equal(sa2.hash_function(), Hash()) );
  VERIFY( is_equal(sa2.key_eq(), Equal()) );
  VERIFY( sa2.get_allocator() == alloc );

  std::unordered_multiset<V, Hash, Equal, Alloc>
    sa3(std::from_range, Range(a, a+14), 3, hf, alloc);
  VERIFY( eq(sa3, {a, 14}) );
  VERIFY( sa3.bucket_count() >= 3 );
  VERIFY( is_equal(sa3.hash_function(), hf) );
  VERIFY( is_equal(sa3.key_eq(), Equal()) );
  VERIFY( sa3.get_allocator() == alloc );

  std::unordered_multiset<V, Hash, Equal, Alloc>
    sa4(std::from_range, Range(a, a+14), 5, hf, eqf, alloc);
  VERIFY( eq(sa4, {a, 14}) );
  VERIFY( sa4.bucket_count() >= 5 );
  VERIFY( is_equal(sa4.hash_function(), hf) );
  VERIFY( is_equal(sa4.key_eq(), eqf) );
  VERIFY( sa4.get_allocator() == alloc );
}

template<typename Range>
void
do_test_ahe()
{
  do_test<Range>(std::allocator<int>(),
		 std::hash<int>(), std::equal_to<int>());
  do_test<Range>(std::allocator<int>(),
		 StateHash{27}, StateEq{17});
  do_test<Range>(__gnu_test::uneq_allocator<int>(42),
		 std::hash<int>(), std::equal_to<int>());
  do_test<Range>(__gnu_test::uneq_allocator<int>(42),
		 StateHash{27}, StateEq{17});
}

bool
test_ranges()
{
  using namespace __gnu_test;

  do_test_ahe<test_forward_range<int>>();
  do_test_ahe<test_range_nocopy<int, input_iterator_wrapper_nocopy>>();
  do_test_ahe<test_forward_range<short>>();

  return true;
}

void test_PR119358() {
#ifdef __SIZEOF_INT128__
  auto r = std::views::iota(__int128(0))
	 | std::views::take(5);
  std::unordered_multiset<__int128> s(std::from_range, r);
  VERIFY( std::ranges::is_permutation(s, r) );
#endif
}

int main()
{
  test_ranges();
  test_PR119358();
}
