// { dg-do run { target c++23 } }

#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <compare>
#include <cstring>
#include <testsuite_hooks.h>

struct X {
  std::string s;
  friend auto operator<=>(X const& a, X const& b) = default;
};

struct Y {
  std::string_view s;
  Y(std::string_view sv) : s(sv) {}
  Y(std::string const& sv) : s(sv) {}
  friend auto operator<=>(Y const& a, Y const& b) = default;
  friend auto operator<=>(X const& a, Y const& b) { return a.s <=> b.s; }
};

using cmp = std::less<void>;

void test1() // uniform erase
{
  std::multiset<X, cmp> aset{cmp{}};
  aset.insert({X{"abc"}, X{"def"}, X{"def"}, X{"ghi"}});
  auto n = aset.erase(X{std::string{"def"}});
  VERIFY(n == 2);
  VERIFY(aset.size() == 2);
}

void test2() // heterogeneous erase
{
  std::multiset<X, cmp> aset{cmp{}};
  aset.insert({X{"abc"}, X{"def"}, X{"def"}, X{"ghi"}});
  auto n = aset.erase(Y{std::string_view{"def"}});
  VERIFY(n == 2);
  VERIFY(aset.size() == 2);
}

void test3() // uniform extract
{
  std::multiset<X, cmp> aset{cmp{}};
  aset.insert({X{"abc"}, X{"def"}, X{"def"}, X{"ghi"}});
  const auto node = aset.extract(X{std::string{"def"}});
  VERIFY(node.value().s == "def");
  VERIFY(aset.size() == 3);
}

void test4() // heterogeneous extract
{
  std::multiset<X, cmp> aset{cmp{}};
  aset.insert({X{"abc"}, X{"def"}, X{"def"}, X{"ghi"}});
  const auto node = aset.extract(Y{std::string_view{"def"}});
  VERIFY(node.value().s == "def");
  VERIFY(aset.size() == 3);
}

struct Z {
  std::string_view s;
  friend auto operator<=>(X const& a, Z const& b) {
    int cmp = std::memcmp(a.s.data(), b.s.data(), 3);
    return cmp < 0 ? std::strong_ordering::less :
	   cmp == 0 ? std::strong_ordering::equal :
	   std::strong_ordering::greater;
  }
};

void test5()
{
  std::multiset<X, cmp> aset{cmp{}};
  aset.insert({X{"abcdef"}, X{"defghi"}, X{"defjkl"}, X{"jklmno"}});
  auto n = aset.erase(Z{std::string_view{"def"}});
  VERIFY(n == 2);
  VERIFY(aset.size() == 2);
}

int main()
{
  test1();
  test2();
  test3();
  test4();
  test5();
}
