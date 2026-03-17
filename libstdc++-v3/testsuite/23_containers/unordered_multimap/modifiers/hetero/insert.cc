// { dg-do run { target c++26 } }

#include <unordered_map>
#include <string>
#include <string_view>
#include <utility>
#include <functional>
#include <testsuite_hooks.h>

struct Y;

struct X {
  std::string s;
  X(std::string_view str) : s(str) {}
  X(Y&& y);
  X(const Y& y);
  friend bool operator==(X const& a, X const& b) = default;
};

struct Y {
  std::string s;
  Y() = default;
  Y(Y&& y) : s(std::move(y.s)) { y.s.clear(); }
  Y(const Y& y) = default;
  Y& operator=(Y&& y) { s = std::move(y.s); y.s.clear(); return *this; }
  Y& operator=(const Y& y) = default;
  Y(std::string_view sv) : s(sv) {}
  Y(int a, int b = 0) : s(std::string('a', a + b)) {}
  friend bool operator==(Y const& a, Y const& b) = default;
  friend bool operator==(X const& a, Y const& b) { return a.s == b.s; }
};

X::X(Y&& y) : s(std::move(y.s)) { y.s.clear(); }
X::X(const Y& y) : s(y.s) {}

struct Hash {
  using is_transparent = void;
  template <typename T>
    auto operator()(T const& t) const
    { return std::hash<decltype(T::s)>{}(t.s); }
};

using Equal = std::equal_to<void>;

void test_bucket()
{
  std::unordered_multimap<X, Y, Hash, Equal> amap;
  amap.insert({{X{"abc"}, 1}, {X{"def"}, 2}, {X{"def"}, 3}, {X{"ghi"}, 3}});

  auto const& amapr{amap};
  VERIFY(amapr.bucket(X{"def"}) == amapr.bucket(Y{"def"}));
}

int main()
{
  test_bucket();
}
