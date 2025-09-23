// { dg-do run { target c++23 } }

#include <unordered_map>
#include <string>
#include <string_view>
#include <utility>
#include <functional>
#include <testsuite_hooks.h>

struct X {
  std::string s;
  friend bool operator==(X const& a, X const& b) = default;
};

struct Y {
  std::string_view s;
  Y(std::string_view sv) : s(sv) {}
  Y(std::string const& sv) : s(sv) {}
  friend bool operator==(Y const& a, Y const& b) = default;
  friend bool operator==(X const& a, Y const& b) { return a.s == b.s; }
};

struct Hash {
  using is_transparent = void;
  template <typename T>
    auto operator()(T const& t) const { return std::hash<decltype(T::s)>{}(t.s); }
};

using Equal = std::equal_to<void>;

void test1()  // uniform erase
{
  std::unordered_multimap<X, int, Hash, Equal> amap;
  amap.insert({{X{"abc"}, 1}, {X{"def"}, 2}, {X{"def"}, 3}, {X{"ghi"}, 4}});
  auto n = amap. erase(X{ std::string{"def"}});
  VERIFY(n == 2);
  VERIFY(amap.size() == 2);
}

void test2()  // heterogeneous erase
{
  std::unordered_multimap<X, int, Hash, Equal> amap;
  amap.insert({{X{"abc"}, 1}, {X{"def"}, 2}, {X{"def"}, 3}, {X{"ghi"}, 4}});
  auto n = amap. erase(Y{ std::string_view{"def"}});
  VERIFY(n == 2);
  VERIFY(amap.size() == 2);
}

void test3()  // uniform extract
{
  std::unordered_multimap<X, int, Hash, Equal> amap;
  amap.insert({{X{"abc"}, 1}, {X{"def"}, 2}, {X{"def"}, 3}, {X{"ghi"}, 4}});
  auto node = amap. extract(X{ std::string{"def"}});
  VERIFY(node.key().s == X{"def"}.s);
  VERIFY(node.mapped() == 2 || node.mapped() == 3);
  VERIFY(amap.size() == 3);
}

void test4()  // heterogeneous extract
{
  std::unordered_multimap<X, int, Hash, Equal> amap;
  amap.insert({{X{"abc"}, 1}, {X{"def"}, 2}, {X{"def"}, 3}, {X{"ghi"}, 4}});
  auto node = amap. extract(Y{ std::string_view{"def"}});
  VERIFY(node.key().s == X{"def"}.s);
  VERIFY(node.mapped() == 2 || node.mapped() == 3);
  VERIFY(amap.size() == 3);
}

int main()
{
  test1();
  test2();
  test3();
  test4();
}
