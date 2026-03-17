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
  Y(int a, int b = 0) : s(std::string('a', a + b)) {}
  Y(std::string_view sv) : s(sv) {}
  friend bool operator==(Y const& a, Y const& b) = default;
  friend bool operator==(X const& a, Y const& b) { return a.s == b.s; }
};

X::X(Y&& y) : s(std::move(y.s)) { y.s.clear(); }
X::X(const Y& y) : s(y.s) {}

struct Hash {
  using is_transparent = void;
  template <typename T>
    auto operator()(T const& t) const { return std::hash<decltype(T::s)>{}(t.s); }
};

using Equal = std::equal_to<void>;

void test_insert()
{
  std::unordered_set<X, Hash, Equal> aset;
  aset.insert({X{"abc"}, X{"def"}, X{"ghi"}});

  { // Fail
    auto a = aset;
    Y y{"def"};
    auto [it, res] = a.insert(y);
    VERIFY(!res);
    VERIFY(a.size() == 3);
    VERIFY(it->s == "def");
    VERIFY(y.s.size() == 3);  // not moved
  }
  { // Fail, move
    auto a = aset;
    Y y{"def"};
    auto [it, res] = a.insert(std::move(y));
    VERIFY(!res);
    VERIFY(a.size() == 3);
    VERIFY(it->s == "def");
    VERIFY(y.s.size() == 3);  // not moved
  }
  { // Succeed
    auto a = aset;
    Y y{"deg"};
    auto [it, res] = a.insert(y);
    VERIFY(res);
    VERIFY(a.size() == 4);
    VERIFY(it->s == "deg");
    VERIFY(y.s.size() == 3);  // not moved
  }
  { // Succeed, move
    auto a = aset;
    Y y{"deg"};
    auto [it, res] = a.insert(std::move(y));
    VERIFY(res);
    VERIFY(a.size() == 4);
    VERIFY(it->s == "deg");
    VERIFY(y.s.empty());  // moved
  }


  { // Hinted, fail
    auto a = aset;
    Y y{"def"};
    auto it = a.insert(a.begin(), y);
    VERIFY(a.size() == 3);
    VERIFY(it->s == "def");
    VERIFY(y.s.size() == 3);  // not moved
  }
  { // Hinted, fail, move
    auto a = aset;
    Y y{"def"};
    auto it = a.insert(a.begin(), std::move(y));
    VERIFY(a.size() == 3);
    VERIFY(it->s == "def");
    VERIFY(y.s.size() == 3);  // not moved
  }
  { // Hinted, succeed
    auto a = aset;
    Y y{"deh"};
    auto it = a.insert(a.begin(), y);
    VERIFY(a.size() == 4);
    VERIFY(it->s == "deh");
    VERIFY(y.s.size() == 3);  // not moved
  }
  { // Hinted, succeed, move
    auto a = aset;
    Y y{"deh"};
    auto it = a.insert(a.begin(), std::move(y));
    VERIFY(a.size() == 4);
    VERIFY(it->s == "deh");
    VERIFY(y.s.empty());  // moved
  }
}

void test_bucket()
{
  std::unordered_set<X, Hash, Equal> aset{};
  aset.insert({X{"abc"}, X{"def"}, X{"ghi"}});

  auto const& asetr{aset};
  VERIFY(asetr.bucket(X{"def"}) == asetr.bucket(Y{"def"}));
}

int main()
{
  test_insert();
  test_bucket();
}
