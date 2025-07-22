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

template<size_t N, typename T, template<class TT> class ItType>
constexpr void
test_assign_empty_it()
{
  using namespace __gnu_test;

  T a[]{1,2,3,4,5,6,7,8,9,10};
  using It = ItType<T>;
  using Range = test_range<T, ItType>;
  using SizedRange = test_sized_range<T, ItType>;

  const std::inplace_vector<T, N> src(std::from_range, std::span(a, N));
  std::inplace_vector<T, N> v;

  v = src;
  v.assign_range(Range(a, a));
  VERIFY( v.empty() );
  v.assign_range(Range(a, a));
  VERIFY( v.empty() );

  v = src;
  v.assign_range(SizedRange(a, a));
  VERIFY( v.empty() );

  v = src;
  auto bounds = typename It::ContainerType(a, a+9);
  v.assign(It(a, &bounds), It(a, &bounds));
  VERIFY( v.empty() );

#ifdef __cpp_exceptions
#ifdef __cpp_lib_constexpr_exceptions
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  static_assert(N < 9);

  v = src;
  try
  {
    v.assign_range(Range(a, a+9));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  if constexpr (std::ranges::sized_range<Range> || std::ranges::forward_range<Range>)
    VERIFY( eq<T>(v, {a, N}) );

  v = src;
  try
  {
    v.assign_range(SizedRange(a, a+9));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );

  v = src;
  bounds = typename It::ContainerType(a, a+9);
  try
  {
    v.assign(It(a, &bounds), It(a+9, &bounds));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  if constexpr(std::forward_iterator<It>)
    VERIFY( eq<T>(v, {a, N}) );
#endif
}

template<size_t N, typename T>
constexpr void
test_assign_empty_other()
{
  T a[]{1,2,3,4,5,6,7,8,9,10};
  const std::inplace_vector<T, N> src(std::from_range, std::span(a, N));
  std::inplace_vector<T, N> v;

  v = src;
  v.assign(0, T(4));
  VERIFY( v.empty() );

  v = src;
  v.assign({});
  VERIFY( v.empty() );

  v = src;
  v = {};
  VERIFY( v.empty() );

  v = src;
  v.resize(0, T(3));
  VERIFY( v.empty() );

  v = src;
  v.resize(0);
  VERIFY( v.empty() );

#ifdef __cpp_exceptions
#ifdef __cpp_lib_constexpr_exceptions
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  static_assert(N < 9);

  v = src;
  try
  {
    v.assign(9, T(4));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );

  std::initializer_list<T> il =
  {T(0), T(1), T(2), T(3), T(4), T(5), T(6), T(7), T(8), T(9), T(10)};
  try
  {
    v.assign(il);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );

  try
  {
    v = il;
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );

  try
  {
    v.resize(9, T(3));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );

  try
  {
    v.resize(9);
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
test_assign_empty()
{
  using namespace __gnu_test;
  test_assign_empty_it<N, T, input_iterator_wrapper>();
  test_assign_empty_it<N, T, forward_iterator_wrapper>();
  test_assign_empty_it<N, T, random_access_iterator_wrapper>();

  test_assign_empty_other<N, T>;
}

template<typename Range>
constexpr void
test_assign_range()
{
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

  std::inplace_vector<T, 10> v;
  v.assign_range(Range(a,a+5));
  VERIFY( eq<T>(v, {a, 5}) );

  v.assign_range(Range(a,a+7));
  VERIFY( eq<T>(v, {a, 7}) );

  v.assign_range(Range(a,a+3));
  VERIFY( eq<T>(v, {a, 3}) );

  v.assign_range(Range(a,a+10));
  VERIFY( eq<T>(v, {a, 10}) );
}

template<typename T, template<class TT> class ItType>
constexpr void
test_assign_iterators()
{
  T a[]{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  using It = ItType<T>;

  std::inplace_vector<T, 10> v;

  auto bounds = typename It::ContainerType(a, a+15);
  v.assign(It(a, &bounds), It(a+5, &bounds));
  VERIFY( eq<T>(v, {a, 5}) );

  bounds = typename It::ContainerType(a, a+15);
  v.assign(It(a, &bounds), It(a+7, &bounds));
  VERIFY( eq<T>(v, {a, 7}) );

  bounds = typename It::ContainerType(a, a+15);
  v.assign(It(a, &bounds), It(a+3, &bounds));
  VERIFY( eq<T>(v, {a, 3}) );

  bounds = typename It::ContainerType(a, a+15);
  v.assign(It(a, &bounds), It(a+10, &bounds));
  VERIFY( eq<T>(v, {a, 10}) );
}

template<typename T>
constexpr void
test_assign_initializer_list()
{
  T a[]{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

  std::inplace_vector<T, 10> v;

  v.assign({T(1), T(2), T(3), T(4), T(5)});
  VERIFY( eq<T>(v, {a, 5}) );

  v = {T(1), T(2), T(3), T(4), T(5), T(6), T(7)};
  VERIFY( eq<T>(v, {a, 7}) );

  v.assign({T(1), T(2), T(3)});
  VERIFY( eq<T>(v, {a, 3}) );

  v = {T(1), T(2), T(3), T(4), T(5), T(6), T(7), T(8), T(9), T(10)};
  VERIFY( eq<T>(v, {a, 10}) );
}

template<typename T>
constexpr void
test_assign_repeated()
{
  auto rep = [](const std::inplace_vector<T, 10>& v, size_t c, const T& t)
  {
    if (v.size() != c)
      return false;
    for (const T& o : v)
      if (o != t)
	return false;
    return true;
  };

  std::inplace_vector<T, 10> v;

  v.assign(5, T(1));
  VERIFY( rep(v, 5, T(1)) );

  v.assign(7, T(2));
  VERIFY( rep(v, 7, T(2)) );

  v.assign(3, T(4));
  VERIFY( rep(v, 3, T(4)) );

  v.assign(10, T(8));
  VERIFY( rep(v, 10, T(8)) );
}

template<typename T>
constexpr void
test_resize()
{
  T a[]{1,1,1,1,2,2,2,0,0,0};

  std::inplace_vector<T, 10> v;

  v.resize(4, T(1));
  VERIFY( eq<T>(v, {a, 4}) );

  v.resize(7, T(2));
  VERIFY( eq<T>(v, {a, 7}) );

  v.resize(10);
  VERIFY( eq<T>(v, {a, 10}) );

  v.resize(6, T(1));
  VERIFY( eq<T>(v, {a, 6}) );
}

template<typename T>
constexpr void
test_assigns()
{
  using namespace __gnu_test;
  test_assign_range<test_forward_range<int>>();
  test_assign_range<test_sized_range_sized_sent<int, forward_iterator_wrapper>>();

  test_assign_range<test_input_range<int>>();
  test_assign_range<test_input_sized_range<int>>();
  test_assign_range<test_sized_range_sized_sent<int, input_iterator_wrapper>>();

  test_assign_range<test_range<int, input_iterator_wrapper_nocopy>>();
  test_assign_range<test_sized_range<int, input_iterator_wrapper_nocopy>>();
  test_assign_range<test_sized_range_sized_sent<int, input_iterator_wrapper_nocopy>>();

  test_assign_iterators<T, input_iterator_wrapper>();
  test_assign_iterators<T, forward_iterator_wrapper>();
  test_assign_iterators<T, random_access_iterator_wrapper>();

  test_assign_initializer_list<T>();
  test_assign_repeated<T>();
  test_resize<T>();
}

int main()
{
  auto test_all = [] {
    test_assign_empty<0, int>();
    test_assign_empty<0, X>();
    test_assign_empty<2, int>();

    test_assigns<int>();
#ifdef __cpp_lib_constexpr_inplace_vector
#error uncomemnt test_inserts<X>()
#endif
    if !consteval {
      test_assign_empty<2, X>();
      test_assigns<X>();
    }
    return true;
  };


  test_all();
  static_assert(test_all());
}
