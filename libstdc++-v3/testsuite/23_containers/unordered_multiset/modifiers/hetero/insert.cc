// { dg-do run { target c++26 } }

#include <unordered_set>
#include <string>
#include <string_view>
#include <utility>
#include <functional>
#include <testsuite_hooks.h>

struct Y;

struct X {
  std::string s;
  X(Y&& y);
  X(const Y& y);
  X(std::string_view str) : s(str) {}
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
  std::unordered_multiset<X, Hash, Equal> aset{};
  aset.insert({X{"abc"}, X{"def"}, X{"def"}, X{"ghi"}});

  auto const& asetr{aset};
  VERIFY(asetr.bucket(X{"def"}) == asetr.bucket(Y{"def"}));
}

int main()
{
  test_bucket();
}
