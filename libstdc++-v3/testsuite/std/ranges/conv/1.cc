// { dg-do run { target c++23 } }

// C++23 26.5.7 Range conversions [range.utility.conv]

#include <ranges>
#include <vector>
#include <string>
#include <deque>
#include <list>
#include <forward_list>
#include <map>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>
#include <testsuite_iterators.h>

void
test_p1206r7_examples()
{
  using Alloc = __gnu_test::uneq_allocator<int>;
  const Alloc alloc(303);
  const std::map<int, const char*> m{{1, "one"}, {2, "two"}, {3, "three"}};
  namespace ranges = std::ranges;

  auto l = std::views::iota(1, 10);
  // create a vector with the elements of l
  auto vec = ranges::to<std::vector<int>>(l); // or vector{std::from_range, l};
  //Specify an allocator
  auto b = ranges::to<std::vector<int, Alloc>>(l, alloc); // or vector{std::from_range, l, alloc};
  //deducing value_type
  auto c = ranges::to<std::vector>(l);
  // explicit conversion int -> long
  auto d = ranges::to<std::vector<long>>(l);
  //Supports converting associative container to sequence containers
  auto f = ranges::to<std::vector>(m);
  //Supports converting sequence containers to associative ones
  auto g = ranges::to<std::map>(f);
  //Pipe syntax
  auto g2 = l | ranges::views::take(42) | ranges::to<std::vector>();
  //Pipe syntax with allocator
  auto h = l | ranges::views::take(42) | ranges::to<std::vector>(alloc);
  //The pipe syntax also support specifying the type and conversions
  auto i = l | ranges::views::take(42) | ranges::to<std::vector<long>>();
  // Nested ranges
  std::list<std::forward_list<int>> lst = {{0, 1, 2, 3}, {4, 5, 6, 7}};
  auto vec1 = ranges::to<std::vector<std::vector<int>>>(lst);
  auto vec2 = ranges::to<std::vector<std::deque<double>>>(lst);

  VERIFY( vec == std::vector<int>(std::ranges::begin(l), std::ranges::end(l)) );
  static_assert(std::is_same_v<decltype(b), std::vector<int, Alloc>>);
  VERIFY( b == (std::vector<int, Alloc>(vec.begin(), vec.end())) );
  VERIFY( b.get_allocator() == alloc );
  static_assert(std::is_same_v<decltype(c), std::vector<int>>);
  VERIFY( c == vec );
  static_assert(std::is_same_v<decltype(d), std::vector<long>>);
  VERIFY( d == std::vector<long>(vec.begin(), vec.end()) );
  VERIFY( g == m );
  static_assert(std::is_same_v<decltype(g2), std::vector<int>>);
  VERIFY( g2 == vec );
  static_assert(std::is_same_v<decltype(h), std::vector<int, Alloc>>);
  VERIFY( h == b );
  VERIFY( h.get_allocator() == alloc );
  VERIFY( i == d );
  static_assert(std::is_same_v<decltype(vec1), std::vector<std::vector<int>>>);
  VERIFY( vec1[1][1] == 5 );
  static_assert(std::is_same_v<decltype(vec2), std::vector<std::deque<double>>>);
  VERIFY( vec2[1][2] == 6.0 );
}

void
test_example_1()
{
  using namespace std;
  using ranges::to;

  // Example 1 from C++23 [range.utility.conv.general]
  string_view str = "the quick brown fox";
  auto words = views::split(str, ' ') | to<vector<string>>();

  VERIFY( (is_same_v<decltype(words), vector<string>>) );
  VERIFY( words == vector<string>({"the", "quick", "brown", "fox"}) );
}

template<typename C>
struct Cont1
{
  template<typename R, typename... Args>
    requires std::constructible_from<C, R&, Args&...>
    Cont1(R&& r, Args&&... args)
    : c(r, args...)
    { }

  C c;
};

void
test_2_1_1()
{
  // (2.1.1) constructible_from<C, R, Args...>

  std::vector<int> v{1, 2, 3, 4};
  auto v2 = std::ranges::to<std::vector<int>>(v);
  static_assert(std::is_same_v<decltype(v2), decltype(v)>);
  VERIFY( v2 == v );

  std::initializer_list<int> il{5, 6, 7, 8};
  v2 = std::ranges::to<std::vector<int>>(il);
  VERIFY( v2 == std::vector<int>(il) );

  v2 = std::ranges::to<std::vector<int>>(il, std::allocator<int>{});
  VERIFY( v2 == std::vector<int>(il) );

  using Alloc = __gnu_test::uneq_allocator<int>;
  using V = std::vector<int, Alloc>;

  V v3({10, 11, 12, 13}, Alloc(14));
  auto v4 = std::ranges::to<V>(v3);
  static_assert(std::is_same_v<decltype(v4), V>);
  VERIFY( v4 == v3 );
  VERIFY( v4.get_allocator() == v3.get_allocator() );

  auto v5 = std::ranges::to<V>(v3, Alloc(33));
  VERIFY( v5 == v3 );
  VERIFY( v5.get_allocator() == Alloc(33) );

  auto v6 = std::ranges::to<V>(il, Alloc(44));
  VERIFY( v6 == V(il) );
  VERIFY( v6.get_allocator() == Alloc(44) );

  auto c = std::ranges::to<Cont1<V>>(V{1, 2, 3});
  static_assert(std::is_same_v<decltype(c), Cont1<V>>);
  VERIFY( c.c == V({1, 2, 3}) );

  auto c2 = std::ranges::to<Cont1<V>>(V{4, 5, 6}, Alloc(55));
  static_assert(std::is_same_v<decltype(c2), Cont1<V>>);
  VERIFY( c2.c == V({4, 5, 6}) );
  VERIFY( c2.c.get_allocator() == Alloc(55) );

  auto c3 = std::ranges::to<Cont1<V>>(il, Alloc(66));
  static_assert(std::is_same_v<decltype(c2), Cont1<V>>);
  VERIFY( c3.c == V(v2.begin(), v2.end()) );
  VERIFY( c3.c.get_allocator() == Alloc(66) );
}

template<typename C>
struct Cont2
{
  template<typename R, typename... Args>
    requires std::constructible_from<C, R&, Args&...>
    Cont2(std::from_range_t, R&& r, Args&&... args)
    : c(r, args...)
    { }

  C c;
};

void
test_2_1_2()
{
  // (2.1.2) constructible_from<C, from_range_t, R, Args...>

  using Alloc = __gnu_test::uneq_allocator<int>;
  using V = std::vector<int, Alloc>;
  auto c = std::ranges::to<Cont2<V>>(V{1, 2, 3});
  static_assert(std::is_same_v<decltype(c), Cont2<V>>);
  VERIFY( c.c == V({1, 2, 3}) );

  auto c2 = std::ranges::to<Cont2<V>>(V{4, 5, 6}, Alloc(7));
  static_assert(std::is_same_v<decltype(c2), Cont2<V>>);
  VERIFY( c2.c == V({4, 5, 6}) );
  VERIFY( c2.c.get_allocator() == Alloc(7) );
}

template<typename C>
struct Cont3
{
  template<typename It, typename Sent, typename... Args>
    requires std::same_as<It, Sent>
    && std::constructible_from<C, It&, Sent&, Args&...>
    Cont3(It first, Sent last, Args&&... args)
    : c(first, last, args...)
    { }

  C c;
};

void
test_2_1_3()
{
  // (2.1.3) constructible_from<C, iterator_t<R>, sentinel_t<R<, Args...>

  using Alloc = __gnu_test::uneq_allocator<int>;
  using V = std::vector<int, Alloc>;

  std::list<unsigned> l{1u, 2u, 3u};
  auto c = std::ranges::to<Cont3<V>>(l);
  static_assert(std::is_same_v<decltype(c), Cont3<V>>);
  VERIFY( c.c == V(l.begin(), l.end()) );

  std::list<long> l2{4l, 5l, 6l};
  auto c2 = std::ranges::to<Cont3<V>>(l2, Alloc(78));
  static_assert(std::is_same_v<decltype(c2), Cont3<V>>);
  VERIFY( c2.c == V(l2.begin(), l2.end()) );
  VERIFY( c2.c.get_allocator() == Alloc(78) );
}

enum AppendKind { None, EmplaceBack, PushBack, Emplace, Insert };

template<typename C, AppendKind Kind>
struct Cont4
{
  // Only support construction with no args or an allocator.
  // This forces the use of either emplace_back, push_back, emplace or insert.
  Cont4() { }
  Cont4(typename C::allocator_type a) : c(a) { }

  template<typename T>
    requires (Kind <= EmplaceBack)
    && requires(C& c, T&& t) { c.emplace_back(std::forward<T>(t)); }
    void
    emplace_back(T&& t)
    {
      kind = EmplaceBack;
      c.emplace_back(std::forward<T>(t));
    }

  template<typename T>
    requires (Kind <= PushBack)
    && requires(C& c, T&& t) { c.push_back(std::forward<T>(t)); }
    void
    push_back(T&& t)
    {
      kind = PushBack;
      c.push_back(std::forward<T>(t));
    }

  template<typename T>
    requires (Kind <= Emplace)
    && requires(C& c, T&& t) { c.emplace(c.end(), std::forward<T>(t)); }
    void
    emplace(typename C::iterator pos, T&& t)
    {
      kind = Emplace;
      c.emplace(pos, std::forward<T>(t));
    }

  template<typename T>
    void
    insert(typename C::iterator pos, T&& t)
    {
      kind = Insert;
      c.insert(pos, std::forward<T>(t));
    }

  // Required to satisfy reservable-container
  void
  reserve(typename C::size_type n) requires requires(C& c) { c.reserve(n); }
  {
    c.reserve(n);
    used_reserve = true;
  }

  // Satisfying sized_range is required to satisfy reservable-container
  typename C::iterator begin() { return c.begin(); }
  typename C::iterator end() { return c.end(); }
  auto size() const { return c.size(); }

  // Required to satisfy reservable-container
  auto capacity() const requires requires(C& c) { c.capacity(); }
  { return c.capacity(); }

  // Required to satisfy reservable-container
  auto max_size() const { return c.max_size(); }

  C c;
  AppendKind kind{};
  bool used_reserve = false;
};

void
test_2_1_4()
{
  // (2.1.4) constructible_from<C, Args...> and
  // container-insertable<C, range_reference_t<R>>

  using Alloc = __gnu_test::uneq_allocator<int>;
  using Alloc2 = __gnu_test::uneq_allocator<short>;
  using V = std::vector<int, Alloc>;
  using List = std::list<short, Alloc2>;

  std::list<unsigned> l{1u, 2u, 3u};
  std::list<long> l2{4l, 5l, 6l};

  // use vector::emplace_back and vector::reserve
  auto c = std::ranges::to<Cont4<V, EmplaceBack>>(l);
  static_assert(std::is_same_v<decltype(c), Cont4<V, EmplaceBack>>);
  VERIFY( c.c == V(l.begin(), l.end()) );
  VERIFY( c.kind == EmplaceBack );
  VERIFY( c.used_reserve );

  // use vector::emplace_back and vector::reserve
  auto c2 = std::ranges::to<Cont4<V, EmplaceBack>>(l2, Alloc(78));
  static_assert(std::is_same_v<decltype(c2), Cont4<V, EmplaceBack>>);
  VERIFY( c2.c == V(l2.begin(), l2.end()) );
  VERIFY( c2.c.get_allocator() == Alloc(78) );
  VERIFY( c2.kind == EmplaceBack );
  VERIFY( c2.used_reserve );

  // use list::emplace_back
  auto c3 = std::ranges::to<Cont4<List, EmplaceBack>>(c.c, Alloc2(99));
  static_assert(std::is_same_v<decltype(c3), Cont4<List, EmplaceBack>>);
  VERIFY( c3.c == List(l.begin(), l.end()) );
  VERIFY( c3.c.get_allocator() == Alloc(99) );
  VERIFY( c3.kind == EmplaceBack );
  VERIFY( ! c3.used_reserve );

  // use vector::push_back and vector::reserve
  auto c4 = std::ranges::to<Cont4<V, PushBack>>(l);
  static_assert(std::is_same_v<decltype(c4), Cont4<V, PushBack>>);
  VERIFY( c4.c == V(l.begin(), l.end()) );
  VERIFY( c4.kind == PushBack );
  VERIFY( c4.used_reserve );

  // use vector::push_back and vector::reserve
  auto c5 = std::ranges::to<Cont4<V, PushBack>>(l2, Alloc(78));
  static_assert(std::is_same_v<decltype(c5), Cont4<V, PushBack>>);
  VERIFY( c5.c == V(l2.begin(), l2.end()) );
  VERIFY( c5.c.get_allocator() == Alloc(78) );
  VERIFY( c5.kind == PushBack );
  VERIFY( c5.used_reserve );

  // use list::push_back
  auto c6 = std::ranges::to<Cont4<List, PushBack>>(c.c, Alloc2(99));
  static_assert(std::is_same_v<decltype(c6), Cont4<List, PushBack>>);
  VERIFY( c6.c == List(l.begin(), l.end()) );
  VERIFY( c6.c.get_allocator() == Alloc(99) );
  VERIFY( c6.kind == PushBack );
  VERIFY( ! c6.used_reserve );

  // use vector::emplace and vector::reserve
  auto c7 = std::ranges::to<Cont4<V, Emplace>>(l);
  static_assert(std::is_same_v<decltype(c7), Cont4<V, Emplace>>);
  VERIFY( c7.c == V(l.begin(), l.end()) );
  VERIFY( c7.kind == Emplace );
  VERIFY( c7.used_reserve );

  // use vector::emplace and vector::reserve
  auto c8 = std::ranges::to<Cont4<V, Emplace>>(l2, Alloc(78));
  static_assert(std::is_same_v<decltype(c8), Cont4<V, Emplace>>);
  VERIFY( c8.c == V(l2.begin(), l2.end()) );
  VERIFY( c8.c.get_allocator() == Alloc(78) );
  VERIFY( c8.kind == Emplace );
  VERIFY( c8.used_reserve );

  // use list::emplace
  auto c9 = std::ranges::to<Cont4<List, Emplace>>(c.c, Alloc2(99));
  static_assert(std::is_same_v<decltype(c9), Cont4<List, Emplace>>);
  VERIFY( c9.c == List(l.begin(), l.end()) );
  VERIFY( c9.c.get_allocator() == Alloc(99) );
  VERIFY( c9.kind == Emplace );
  VERIFY( ! c9.used_reserve );

  // use vector::insert and vector::reserve
  auto c10 = std::ranges::to<Cont4<V, Insert>>(l);
  static_assert(std::is_same_v<decltype(c10), Cont4<V, Insert>>);
  VERIFY( c10.c == V(l.begin(), l.end()) );
  VERIFY( c10.kind == Insert );
  VERIFY( c10.used_reserve );

  // use vector::insert and vector::reserve
  auto c11 = std::ranges::to<Cont4<V, Insert>>(l2, Alloc(78));
  static_assert(std::is_same_v<decltype(c11), Cont4<V, Insert>>);
  VERIFY( c11.c == V(l2.begin(), l2.end()) );
  VERIFY( c11.c.get_allocator() == Alloc(78) );
  VERIFY( c11.kind == Insert );
  VERIFY( c11.used_reserve );

  // use list::insert
  auto c12 = std::ranges::to<Cont4<List, Insert>>(c.c, Alloc2(99));
  static_assert(std::is_same_v<decltype(c12), Cont4<List, Insert>>);
  VERIFY( c12.c == List(l.begin(), l.end()) );
  VERIFY( c12.c.get_allocator() == Alloc(99) );
  VERIFY( c12.kind == Insert );
  VERIFY( ! c12.used_reserve );

  struct NoCopyPls
  {
    NoCopyPls(int) { }
    NoCopyPls(const NoCopyPls&) { throw; }
  };

  // Uses list::emplace_back(const int&) not list::push_back(NoCopyPls&&).
  (void) std::ranges::to<std::list<NoCopyPls>>(l);
}

void
test_2_2()
{
  // (2.2) input_range<range_reference_t<R>>

  std::string s1[]{ "one", "two", "three", "four" };
  std::string s2[]{ "V", "VI", "VII", "VIII" };
  std::string s3[]{ "0x09", "0x0a", "0x0b", "0x0c" };
  using R = __gnu_test::test_input_range<std::string>;
  R input_ranges[]{R(s1), R(s2), R(s3)};
  __gnu_test::test_input_range<R> rr(input_ranges);
  namespace pmr = std::pmr;
  __gnu_test::memory_resource res;
#if _GLIBCXX_USE_CXX11_ABI
  auto vvs = std::ranges::to<pmr::vector<pmr::vector<pmr::string>>>(rr, &res);
  auto str_alloc = pmr::polymorphic_allocator<char>(&res);
#else
  auto vvs = std::ranges::to<pmr::vector<pmr::vector<std::string>>>(rr, &res);
  auto str_alloc = std::allocator<char>();
#endif
  VERIFY( vvs[1][1] == "VI" );
  VERIFY( vvs[2][2] == "0x0b" );
  VERIFY( vvs[0].get_allocator().resource() == &res );
  VERIFY( vvs[2][2].get_allocator() == str_alloc );
}

void
test_lwg3984()
{
  std::vector<std::vector<int>> v;
  auto r = std::views::all(std::move(v));
  auto l = std::ranges::to<std::list<std::list<int>>>(r);
  VERIFY(l.empty());
}

void
test_nodiscard()
{
  std::vector<int> v;
  std::ranges::to<std::vector<long>>(v); // { dg-warning "ignoring return" }
  std::ranges::to<std::vector>(v);       // { dg-warning "ignoring return" }
  std::ranges::to<std::vector<long>>();  // { dg-warning "ignoring return" }
  std::ranges::to<std::vector>();        // { dg-warning "ignoring return" }
}

void
test_constexpr()
{
  constexpr int x = [](int i) {
    auto c1 = std::views::iota(1, i) | std::ranges::to<std::vector<int>>();
    auto c2 = std::views::iota(i, 10) | std::ranges::to<std::vector>();
    return c1[0] + c2[0];
  }(5);
  static_assert(x == 6);
}

void
test_sfinae()
{
  // PR libstdc++/112802
  [](auto x) {
    static_assert(!requires { std::ranges::to<std::vector<int>>()(x); });
    static_assert(!requires { std::ranges::to<std::vector>()(x); });
  }(0);
}

void
test_composition()
{
  // PR libstdc++/113068
  auto adaptor = std::ranges::to<std::string>() | std::ranges::to<std::string>();
  auto str = adaptor(" ");
}

int main()
{
  test_p1206r7_examples();
  test_example_1();
  test_2_1_1();
  test_2_1_2();
  test_2_1_3();
  test_2_1_4();
  test_2_2();
  test_lwg3984();
  test_nodiscard();
  test_constexpr();
  test_sfinae();
  test_composition();
}
