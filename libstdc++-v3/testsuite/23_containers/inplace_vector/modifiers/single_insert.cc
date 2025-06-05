// { dg-do run { target c++26 } }

#include <inplace_vector>

#include <span>
#include <testsuite_hooks.h>

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

template<size_t N, typename T>
constexpr void
test_add_to_full()
{
  using namespace __gnu_test;

  T a[]{1,2,3,4,5,6,7,8,9};
  const T c(10);

  std::inplace_vector<T, N> v(std::from_range, std::span(a, a+N));

  VERIFY( v.try_emplace_back(1) == nullptr );
  VERIFY( eq<T>(v, {a, N}) );
  VERIFY( v.try_push_back(T(1)) == nullptr );
  VERIFY( eq<T>(v, {a, N}) );
  VERIFY( v.try_push_back(c) == nullptr );
  VERIFY( eq<T>(v, {a, N}) );

#ifdef __cpp_exceptions
#ifdef __cpp_lib_constexpr_exceptions
#error remove the consteval check
#endif
  if consteval {
    return;
  }

  try
  {
    v.emplace_back(1);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );

  try
  {
    v.push_back(T(1));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );

  try
  {
    v.push_back(c);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );

  try
  {
    v.insert(v.end(), T(1));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );

  try
  {
    v.insert(v.begin(), c);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );

  try
  {
    v.emplace(v.end(), c);
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );

  try
  {
    v.emplace(v.begin(), T(2));
    VERIFY(false);
  }
  catch (std::bad_alloc const&)
  {
  }
  VERIFY( eq<T>(v, {a, N}) );
#endif
}

template<typename T>
constexpr void
test_inserts()
{
  T a[]{3,14,13,1,2,3,4,5,3,7,8,3,10,11,3};
  const T c(3);

  std::inplace_vector<T, 20> v;

  v.emplace_back(1);
  VERIFY( eq<T>(v, {a+3, 1}) );
  v.push_back(T(2));
  VERIFY( eq<T>(v, {a+3, 2}) );
  v.push_back(c);
  VERIFY( eq<T>(v, {a+3, 3}) );

  v.unchecked_emplace_back(4);
  VERIFY( eq<T>(v, {a+3, 4}) );
  v.unchecked_push_back(T(5));
  VERIFY( eq<T>(v, {a+3, 5}) );
  v.unchecked_push_back(c);
  VERIFY( eq<T>(v, {a+3, 6}) );

  T* ptr = v.try_emplace_back(7);
  VERIFY( eq<T>(v, {a+3, 7}) );
  VERIFY( ptr = &v.back() );
  ptr = v.try_push_back(T(8));
  VERIFY( eq<T>(v, {a+3, 8}) );
  VERIFY( ptr = &v.back() );
  ptr = v.try_push_back(c);
  VERIFY( eq<T>(v, {a+3, 9}) );
  VERIFY( ptr = &v.back() );

  auto it = v.emplace(v.end(), 10);
  VERIFY( eq<T>(v, {a+3, 10}) );
  VERIFY( it == v.end()-1 );
  it = v.insert(v.end(), T(11));
  VERIFY( eq<T>(v, {a+3, 11}) );
  VERIFY( it == v.end()-1 );
  it = v.insert(v.end(), c);
  VERIFY( eq<T>(v, {a+3, 12}) );
  VERIFY( it == v.end()-1 );

  it = v.emplace(v.begin(), 13);
  VERIFY( eq<T>(v, {a+2, 13}) );
  VERIFY( it == v.begin() );
  it = v.insert(v.begin(), T(14));
  VERIFY( eq<T>(v, {a+1, 14}) );
  VERIFY( it == v.begin() );
  it = v.insert(v.begin(), c);
  VERIFY( eq<T>(v, {a+0, 15}) );
  VERIFY( it == v.begin() );

  it = v.emplace(v.begin()+2, 22);
  VERIFY( *it == 22 );
  VERIFY( it == v.begin()+2 );
  it = v.insert(v.begin()+6, T(24));
  VERIFY( *it == 24 );
  VERIFY( it == v.begin()+6 );
  it = v.insert(v.begin()+13, c);
  VERIFY( *it == 3 );
  VERIFY( it == v.begin()+13 );
}

int main()
{
  auto test_all = [] {
    test_add_to_full<0, int>();
    test_add_to_full<0, X>();

    test_add_to_full<4, int>();

    test_inserts<int>();
#ifdef __cpp_lib_constexpr_inplace_vector
#error uncomemnt test_inserts<X>()
#endif
    if ! consteval {
      test_add_to_full<4, X>();
      test_inserts<X>();
    }
    return true;
  };

  test_all();
  static_assert(test_all());;
}
