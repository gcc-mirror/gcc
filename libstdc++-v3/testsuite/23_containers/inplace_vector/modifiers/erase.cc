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

template<typename T, typename V, size_t N>
constexpr bool
eq(const std::inplace_vector<V, N>& l, std::initializer_list<T> r)
{ return eq<T>(l, std::span<const T>(r)); }

template<size_t N, typename T>
constexpr void
test_erase_all_or_none()
{
  using namespace __gnu_test;

  T a[]{1,2,3,4,5,6,7,8,9};
  const T c(10);

  std::inplace_vector<T, N> src(std::from_range, std::span(a, a+N));
  std::inplace_vector<T, N> v;

  v = src;
  auto it = v.erase(v.begin(), v.begin());
  VERIFY( it == v.begin() );
  VERIFY( eq<T>(v, {a, N}) );

  it = v.erase(v.end(), v.end());
  VERIFY( it == v.end() );
  VERIFY( eq<T>(v, {a, N}) );

  it = v.erase(v.begin(), v.end());
  VERIFY( it == v.begin() );
  VERIFY( v.empty() );

  v = src;
  v.clear();
  VERIFY( v.empty() );
}

template<typename T>
constexpr void
test_erase()
{
  std::inplace_vector<T, 10> v{T(1), T(2), T(3), T(4), T(5), T(6), T(7)};

  auto it = v.erase(v.begin());
  VERIFY( eq<T>(v, {T(2), T(3), T(4), T(5), T(6), T(7)}) );
  VERIFY( it == v.begin() );

  it = v.erase(v.end()-1);
  VERIFY( eq<T>(v, {T(2), T(3), T(4), T(5), T(6)}) );
  VERIFY( it == v.end() );

  it = v.erase(v.begin()+2, v.begin()+4);
  VERIFY( eq<T>(v, {T(2), T(3), T(6)}) );
  VERIFY( it == v.begin()+2 );

  it = v.erase(v.end()-1, v.end());
  VERIFY( eq<T>(v, {T(2), T(3)}) );
  VERIFY( it == v.end() );

  it = v.erase(v.begin(), v.begin()+1);
  VERIFY( eq<T>(v, {T(3)}) );
  VERIFY( it == v.begin() );

  v.pop_back();
  VERIFY( v.empty() );
}

int main()
{
  auto test_all = [] {
    test_erase_all_or_none<0, int>();
    test_erase_all_or_none<0, X>();

    test_erase_all_or_none<4, int>();

    test_erase<int>();
#ifdef __cpp_lib_constexpr_inplace_vector
#error uncomemnt test_inserts<X>()
#endif
    if ! consteval {
      test_erase_all_or_none<4, X>();
    }
    return true;
  };

  test_all();
  static_assert(test_all());;
}
