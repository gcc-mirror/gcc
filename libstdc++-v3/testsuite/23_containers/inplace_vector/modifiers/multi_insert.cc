// { dg-do run { target c++26 } }

#include <inplace_vector>

#include <span>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

struct X
{
  X() = default;
  constexpr X(int p) : v(p) {}
  constexpr X(const X& o) : v(o.v) { } // not trivial
  constexpr X& operator=(const X& o) // not trivial
  { v = o.v; return *this; }

  int v;

  friend auto operator<=>(const X&, const X&) = default;
};

template<typename T, typename V, size_t N>
constexpr bool
eq(const std::inplace_vector<V, N>& l, std::span<const T> r) {
  if (l.size() != r.size())
    return false;
  for (auto i = 0u; i < l.size(); ++i)
    if (l[i] != r[i])
      return false;
  return true;
};

template<typename T, typename V, size_t N>
constexpr bool
prefix(const std::inplace_vector<V, N>& l, std::span<const T> r) {
  if (l.size() < r.size())
    return false;
  for (auto i = 0u; i < r.size(); ++i)
    if (l[i] != r[i])
      return false;
  return true;
};

template<size_t N, typename T, template<class TT> class ItType>
constexpr void
test_add_to_full_it()
{
  using namespace __gnu_test;

  T a[]{1,2,3,4,5,6,7,8,9};
  using It = ItType<T>;
  using Range = test_range<T, ItType>;
  using SizedRange = test_sized_range<T, ItType>;

  std::inplace_vector<T, N> v(std::from_range, std::span(a, a+N));

  Range r1(a, a);
  auto rit1 = v.try_append_range(r1);
  VERIFY( eq<T>(v, {a, N}) );
  VERIFY( rit1.base() == a );

  SizedRange r2(a, a);
  auto rit2 = v.try_append_range(r2);
  VERIFY( eq<T>(v, {a, N}) );
  VERIFY( rit2.base() == a );

  v.append_range(Range(a, a));
  VERIFY( eq<T>(v, {a, N}) );
  v.append_range(SizedRange(a, a));
  VERIFY( eq<T>(v, {a, N}) );

  auto it = v.insert_range(v.end(), Range(a, a));
  VERIFY( eq<T>(v, {a, N}) );
  VERIFY( it == v.end() );
  it = v.insert_range(v.end(), SizedRange(a, a));
  VERIFY( eq<T>(v, {a, N}) );
  VERIFY( it == v.end() );

  auto bounds = typename It::ContainerType(a, a+9);
  it = v.insert(v.end(), It(a, &bounds), It(a, &bounds));
  VERIFY( eq<T>(v, {a, N}) );
  VERIFY( it == v.end() );

  it = v.insert_range(v.begin(), SizedRange(a, a));
  VERIFY( eq<T>(v, {a, N}) );
  VERIFY( it == v.begin() );
  it = v.insert_range(v.begin(), Range(a, a));
  VERIFY( eq<T>(v, {a, N}) );
  VERIFY( it == v.begin() );

  bounds = typename It::ContainerType(a, a+9);
  it = v.insert(v.begin(), It(a, &bounds), It(a, &bounds));
  VERIFY( eq<T>(v, {a, N}) );
  VERIFY( it == v.begin() );

  // Inserting non-empty range
  Range r3(a+3, a+5);
  auto rit3 = v.try_append_range(r3);
  VERIFY( eq<T>(v, {a, N}) );
  VERIFY( rit3.base() == a+3 );

  SizedRange r4(a+2, a+5);
  auto rit4 = v.try_append_range(r4);
  VERIFY( eq<T>(v, {a, N}) );
  VERIFY( rit4.base() == a+2 );

#ifdef __cpp_exceptions
#ifdef __cpp_lib_constexpr_exceptions
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  try
  {
    v.append_range(Range(a, a + 5));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );

  try
  {
    v.append_range(SizedRange(a, a + 5));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );

  try
  {
    v.insert_range(v.begin(), SizedRange(a, a+5));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );

  try
  {
    v.insert_range(v.begin(), Range(a, a+5));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  auto gn = std::ranges::sized_range<Range> || std::ranges::forward_range<Range> ? N : 0;
  VERIFY( prefix<T>(v, {a, gn}) );

  v = std::inplace_vector<T, N>(std::from_range, std::span(a, a+N));
  try
  {
    v.insert_range(v.begin(), Range(a, a+5));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  gn = std::forward_iterator<It> ? N : 0;
  VERIFY( prefix<T>(v, {a, gn}) );
#endif
}

template<size_t N, typename T>
constexpr void
test_add_to_full_other()
{
  using namespace __gnu_test;

  T a[]{1,2,3,4,5,6,7,8,9};
  std::inplace_vector<T, N> v(std::from_range, std::span(a, a+N));

  auto it = v.insert(v.end(), {});
  VERIFY( eq<T>(v, {a, N}) );
  VERIFY( it == v.end() );
  it = v.insert(v.end(), 0u, T(2));
  VERIFY( eq<T>(v, {a, N}) );
  VERIFY( it == v.end() );

  it = v.insert(v.begin(), {});
  VERIFY( eq<T>(v, {a, N}) );
  VERIFY( it == v.begin() );
  it = v.insert(v.begin(), 0u, T(2));
  VERIFY( eq<T>(v, {a, N}) );
  VERIFY( it == v.begin() );

#ifdef __cpp_exceptions
#ifdef __cpp_lib_constexpr_exceptions
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  v = std::inplace_vector<T, N>(std::from_range, std::span(a, a+N));
  try
  {
    v.insert(v.begin(), {T(1), T(2), T(3)});
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );

  try
  {
    v.insert(v.begin(), 4u, T(3));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );
#endif
}


template<size_t N, typename T>
constexpr void
test_add_to_full()
{
  using namespace __gnu_test;
  test_add_to_full_it<N, T, input_iterator_wrapper>();
  test_add_to_full_it<N, T, forward_iterator_wrapper>();
  test_add_to_full_it<N, T, random_access_iterator_wrapper>();

  test_add_to_full_other<N, T>();
}

template<typename Range>
constexpr void
test_append_range()
{
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

  std::inplace_vector<T, 20> v;
  v.append_range(Range(a,a+10));
  VERIFY( eq<T>(v, {a, 10}) );

  v.append_range(Range(a+10, a+15));
  VERIFY( eq<T>(v, {a, 15}) );

#ifdef __cpp_exceptions
#ifdef __cpp_lib_constexpr_exceptions
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  try
  {
    v.append_range(Range(a, a+10));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( prefix<T>(v, {a, 15}) );
#endif
}

template<typename Range>
constexpr void
test_try_append_range()
{
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25};

  std::inplace_vector<T, 20> v;
  Range r1 = Range(a, a+10);
  auto it1 = v.try_append_range(r1);
  VERIFY( eq<T>(v, {a, 10}) );
  VERIFY( it1.base() == a+10 );

  Range r2 = Range(a+10, a+15);
  auto it2 = v.try_append_range(r2);
  VERIFY( eq<T>(v, {a, 15}) );
  VERIFY( it2.base() == a+15 );

  Range r3 = Range(a+15, a+25);
  auto it3 = v.try_append_range(r3);
  VERIFY( eq<T>(v, {a, 20}) );
  VERIFY( it3.base() == a+20 );
}

template<typename Range>
constexpr void
test_insert_range()
{
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

  std::inplace_vector<T, 20> v;
  auto it = v.insert_range(v.begin(), Range(a+10,a+15));
  VERIFY( eq<T>(v, {a+10, 5}) );
  VERIFY( it == v.begin() );

  it = v.insert_range(v.begin(), Range(a, a+5));
  VERIFY( prefix<T>(v, {a, 5}) );
  VERIFY( it == v.begin() );

  it = v.insert_range(v.begin() + 5, Range(a+5, a+10));
  VERIFY( eq<T>(v, {a, 15}) );
  VERIFY( it == v.begin() + 5 );

#ifdef __cpp_exceptions
#ifdef __cpp_lib_constexpr_exceptions
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  const bool seg = std::ranges::sized_range<Range> || std::ranges::forward_range<Range>;
  auto vc = v;
  try
  {
    vc.insert_range(vc.begin(), Range(a, a+10));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( prefix<T>(vc, {a, seg ? 15 : 0}) );

  vc = v;
  try
  {
    vc.insert_range(vc.begin()+5, Range(a, a+10));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( prefix<T>(vc, {a, seg ? 15 : 5}) );

  vc = v;
  try
  {
    vc.insert_range(vc.end(), Range(a, a+10));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( prefix<T>(vc, {a, 15}) );
#endif
}

template<typename Range>
constexpr void
do_test_ranges()
{
  test_append_range<Range>();
  test_try_append_range<Range>();
  test_insert_range<Range>();
}

template<typename T, template<class TT> class ItType>
constexpr void
test_insert_iterators()
{
  T a[]{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  using It = ItType<T>;

  std::inplace_vector<T, 20> v;

  auto bounds = typename It::ContainerType(a, a+15);
  auto it = v.insert(v.begin(), It(a+10, &bounds), It(a+15, &bounds));
  VERIFY( eq<T>(v, {a+10, 5}) );
  VERIFY( it == v.begin() );

  bounds = typename It::ContainerType(a, a+15);
  it = v.insert(v.begin(), It(a, &bounds), It(a+5, &bounds));
  VERIFY( prefix<T>(v, {a, 5}) );
  VERIFY( it == v.begin() );

  bounds = typename It::ContainerType(a, a+15);
  it = v.insert(v.begin() + 5, It(a+5, &bounds), It(a+10, &bounds));
  VERIFY( eq<T>(v, {a, 15}) );
  VERIFY( it == v.begin() + 5 );

#ifdef __cpp_exceptions
#ifdef __cpp_lib_constexpr_exceptions
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  const bool seg = std::forward_iterator<It>;
  auto vc = v;
  bounds = typename It::ContainerType(a, a+15);
  try
  {
    vc.insert(vc.begin(), It(a, &bounds), It(a+10, &bounds));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( prefix<T>(vc, {a, seg ? 15 : 0}) );

  vc = v;
  bounds = typename It::ContainerType(a, a+15);
  try
  {
    vc.insert(vc.begin()+5, It(a, &bounds), It(a+10, &bounds));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( prefix<T>(vc, {a, seg ? 15 : 5}) );

  vc = v;
  bounds = typename It::ContainerType(a, a+15);
  try
  {
    vc.insert(vc.end(), It(a, &bounds), It(a+10, &bounds));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( prefix<T>(vc, {a, 15}) );
#endif
}

template<typename T>
constexpr void
test_insert_initializer_list()
{
  T a[]{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

  std::inplace_vector<T, 20> v;

  auto it = v.insert(v.begin(), {T(11), T(12), T(13), T(14), T(15)});
  VERIFY( eq<T>(v, {a+10, 5}) );
  VERIFY( it == v.begin() );

  it = v.insert(v.begin(), {T(1), T(2), T(3), T(4), T(5)});
  VERIFY( prefix<T>(v, {a, 5}) );
  VERIFY( it == v.begin() );

  it = v.insert(v.begin() + 5, {T(6), T(7), T(8), T(9), T(10)});
  VERIFY( eq<T>(v, {a, 15}) );
  VERIFY( it == v.begin() + 5 );

#ifdef __cpp_exceptions
#ifdef __cpp_lib_constexpr_exceptions
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  std::initializer_list<T> il
  = {T(0), T(1), T(2), T(3), T(4), T(5), T(6), T(7), T(8), T(9)};

  try
  {
    v.insert(v.begin(), il);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, 15}) );

  try
  {
    v.insert(v.begin()+5, il);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, 15}) );

  try
  {
    v.insert(v.end(), il);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, 15}) );
#endif
}

template<typename T>
constexpr void
test_insert_repeated()
{
  T a[]{5,5,5,5,5,6,6,6,6,6,7,7,7,7,7};

  std::inplace_vector<T, 20> v;

  auto it = v.insert(v.begin(), 5, T(7));
  VERIFY( eq<T>(v, {a+10, 5}) );
  VERIFY( it == v.begin() );

  it = v.insert(v.begin(), 5, T(5));
  VERIFY( prefix<T>(v, {a, 5}) );
  VERIFY( it == v.begin() );

  it = v.insert(v.begin() + 5, 5, T(6));
  VERIFY( eq<T>(v, {a, 15}) );
  VERIFY( it == v.begin() + 5 );

#ifdef __cpp_exceptions
#ifdef __cpp_lib_constexpr_exceptions
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  try
  {
    v.insert(v.begin(), 10u, T(6));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, 15}) );

  try
  {
    v.insert(v.begin()+5, 10u, T(6));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, 15}) );

  try
  {
    v.insert(v.end(), 10u, T(6));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, 15}) );
#endif
}

template<typename T>
constexpr void
test_inserts()
{
  using namespace __gnu_test;
  do_test_ranges<test_forward_range<int>>();
  do_test_ranges<test_sized_range_sized_sent<int, forward_iterator_wrapper>>();

  do_test_ranges<test_input_range<int>>();
  do_test_ranges<test_input_sized_range<int>>();
  do_test_ranges<test_sized_range_sized_sent<int, input_iterator_wrapper>>();

  do_test_ranges<test_range<int, input_iterator_wrapper_nocopy>>();
  do_test_ranges<test_sized_range<int, input_iterator_wrapper_nocopy>>();
  do_test_ranges<test_sized_range_sized_sent<int, input_iterator_wrapper_nocopy>>();

  test_insert_iterators<T, input_iterator_wrapper>();
  test_insert_iterators<T, forward_iterator_wrapper>();
  test_insert_iterators<T, random_access_iterator_wrapper>();

test_insert_initializer_list<T>();
test_insert_repeated<T>();
}

int main()
{
auto test_all = []{
  test_add_to_full<0, int>();
  test_add_to_full<0, X>();
  test_add_to_full<4, int>();

    test_inserts<int>();
#ifdef __cpp_lib_constexpr_inplace_vector
#error uncomemnt test_inserts<X>()
#endif
    if !consteval {
      test_add_to_full<4, X>();
      test_inserts<X>();
    }
    return true;
  };

  test_all();
  static_assert(test_all());
}
