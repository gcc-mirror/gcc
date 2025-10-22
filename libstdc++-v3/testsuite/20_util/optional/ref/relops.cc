// { dg-do run { target c++26 }  }

#include <optional>
#include <functional>
#include <testsuite_hooks.h>
#include <type_traits>

template<typename T, typename H = std::hash<T>>
constexpr bool has_disabled_hash
 = !std::is_default_constructible_v<H>
 && !std::is_copy_constructible_v<H>
 && !std::is_move_constructible_v<H>
 && !std::is_copy_assignable_v<H>
 && !std::is_move_assignable_v<H>;

static_assert(has_disabled_hash<std::optional<int&>>);
static_assert(has_disabled_hash<std::optional<const int&>>);

template<typename T, typename V>
constexpr void
test_compare_val(V& l, V& h)
{
  std::optional<T> t;

  VERIFY( !(t == l) );
  VERIFY(  (t != l) );
  VERIFY(  (t <  l) );
  VERIFY(  (t <= l) );
  VERIFY( !(t >  l) );
  VERIFY( !(t >= l) );
  VERIFY( (t <=> l) < 0 );

  VERIFY( !(l == t) );
  VERIFY(  (l != t) );
  VERIFY( !(l <  t) );
  VERIFY( !(l <= t) );
  VERIFY(  (l >  t) );
  VERIFY(  (l >= t) );
  VERIFY( (l <=> t) > 0 );

  t.emplace(l);
  VERIFY(  (t == l) );
  VERIFY( !(t != l) );
  VERIFY( !(t <  l) );
  VERIFY(  (t <= l) );
  VERIFY( !(t >  l) );
  VERIFY(  (t >= l) );
  VERIFY( (t <=> l) == 0 );

  VERIFY(  (l == t) );
  VERIFY( !(l != t) );
  VERIFY( !(l <  t) );
  VERIFY(  (l <= t) );
  VERIFY( !(l >  t) );
  VERIFY(  (l >= t) );
  VERIFY( (t <=> l) == 0 );

  t.emplace(h);
  VERIFY( !(t == l) );
  VERIFY(  (t != l) );
  VERIFY( !(t <  l) );
  VERIFY( !(t <= l) );
  VERIFY(  (t >  l) );
  VERIFY(  (t >= l) );
  VERIFY( (t <=> l) > 0 );

  VERIFY( !(l == t) );
  VERIFY(  (l != t) );
  VERIFY(  (l <  t) );
  VERIFY(  (l <= t) );
  VERIFY( !(l >  t) );
  VERIFY( !(l >= t) );
  VERIFY( (l <=> t) < 0 );
}

template<typename T, typename U, typename V>
constexpr void
test_compare_opts(V& l, V& h)
{
  std::optional<T> t;
  std::optional<U> u;

  VERIFY(  (t == u) );
  VERIFY( !(t != u) );
  VERIFY( !(t <  u) );
  VERIFY(  (t <= u) );
  VERIFY( !(t >  u) );
  VERIFY(  (t >= u) );
  VERIFY( (t <=> u) == 0 );

  t.emplace(l);
  VERIFY( !(t == u) );
  VERIFY(  (t != u) );
  VERIFY( !(t <  u) );
  VERIFY( !(t <= u) );
  VERIFY(  (t >  u) );
  VERIFY(  (t >= u) );
  VERIFY( (t <=> u) > 0 );

  u.emplace(l);
  VERIFY(  (t == u) );
  VERIFY( !(t != u) );
  VERIFY( !(t <  u) );
  VERIFY(  (t <= u) );
  VERIFY( !(t >  u) );
  VERIFY(  (t <= u) );
  VERIFY( (t <=> u) == 0 );

  u.emplace(h);
  VERIFY( !(t == u) );
  VERIFY(  (t != u) );
  VERIFY(  (t <  u) );
  VERIFY(  (t <= u) );
  VERIFY( !(t >  u) );
  VERIFY( !(t >= u) );
  VERIFY( (t <=> u) < 0 );

  t.reset();
  u.emplace(l);
  VERIFY( !(t == u) );
  VERIFY(  (t != u) );
  VERIFY(  (t <  u) );
  VERIFY(  (t <= u) );
  VERIFY( !(t >  u) );
  VERIFY( !(t >= u) );
  VERIFY( (t <=> u) < 0 );

  t.emplace(h);
  VERIFY( !(t == u) );
  VERIFY(  (t != u) );
  VERIFY( !(t <  u) );
  VERIFY( !(t <= u) );
  VERIFY(  (t >  u) );
  VERIFY(  (t >= u) );
  VERIFY( (t <=> u) > 0 );
}

template<typename V>
constexpr void
test_compare(V l, V h)
{
  test_compare_val<V&>(l, h);
  test_compare_val<const V&>(l, h);

  test_compare_opts<V&, V&>(l, h);
  test_compare_opts<V, V&>(l, h);
  test_compare_opts<V&, V>(l, h);

  test_compare_opts<const V&, const V&>(l, h);
  test_compare_opts<V, const V&>(l, h);
  test_compare_opts<const V&, V>(l, h);

  test_compare_opts<V&, const V&>(l, h);
  test_compare_opts<const V&, V&>(l, h);
}

struct TreeWay
{
  int v;
  friend auto operator<=>(TreeWay, TreeWay) = default;
};

struct Other
{
  int v;

  constexpr Other(int p) : v(p) {}
  constexpr Other(TreeWay p) : v(p.v) {}

  friend bool operator==(Other, Other) = default;
  friend auto operator<=>(Other, Other) = default;

  friend constexpr bool
  operator==(const Other& lhs, const TreeWay& rhs)
  { return lhs.v == rhs.v; }

  friend constexpr std::strong_ordering
  operator<=>(const Other& lhs, const TreeWay& rhs)
  { return lhs.v <=> rhs.v; }
};

constexpr void
test_heterogeneus_cmp()
{
  TreeWay l{10};
  Other h{20};

  std::optional<TreeWay&> t;
  std::optional<const Other&> u;

  VERIFY(  (t == u) );
  VERIFY( !(t != u) );
  VERIFY( !(t <  u) );
  VERIFY(  (t <= u) );
  VERIFY( !(t >  u) );
  VERIFY(  (t >= u) );
  VERIFY( (t <=> u) == 0 );

  t.emplace(l);
  VERIFY( !(t == u) );
  VERIFY(  (t != u) );
  VERIFY( !(t <  u) );
  VERIFY( !(t <= u) );
  VERIFY(  (t >  u) );
  VERIFY(  (t >= u) );
  VERIFY( (t <=> u) > 0 );

  u.emplace(h);
  VERIFY( !(t == u) );
  VERIFY(  (t != u) );
  VERIFY(  (t <  u) );
  VERIFY(  (t <= u) );
  VERIFY( !(t >  u) );
  VERIFY( !(t >= u) );
  VERIFY( (t <=> u) < 0 );
}

int main()
{
  auto test_all = [] {
    test_compare(2, 5);
    test_compare(TreeWay{11}, TreeWay{12});
    test_heterogeneus_cmp();
    return true;
  };

  test_all();
  static_assert(test_all());
}
