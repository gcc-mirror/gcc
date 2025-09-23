// { dg-do run { target c++23 } }

#include <map>
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

// uniform erase
void test1()
{
  std::map<X, int, cmp> amap{cmp{}};
  amap.insert({{X{"abc"}, 1}, {X{"def"}, 2}, {X{"ghi"}, 3}});
  auto n = amap.erase(X{std::string{"def"}});
  VERIFY(n == 1);
  VERIFY(amap.size() == 2);
}

// heterogeneous erase
void test2()
{
  std::map<X, int, cmp> amap{cmp{}};
  amap.insert({{X{"abc"}, 1}, {X{"def"}, 2}, {X{"ghi"}, 3}});
  auto n = amap.erase(Y{std::string_view{"def"}});
  VERIFY(n == 1);
  VERIFY(amap.size() == 2);
}

// uniform extract
void test3()
{
  std::map<X, int, cmp> amap{cmp{}};
  amap.insert({{X{"abc"}, 1}, {X{"def"}, 2}, {X{"ghi"}, 3}});
  auto node = amap.extract(X{std::string{"def"}});
  VERIFY(node.key().s == X{"def"}.s);
  VERIFY(node.mapped() == 2);
  VERIFY(amap.size() == 2);
}

// heterogeneous extract
void test4()
{
  std::map<X, int, cmp> amap{cmp{}};
  amap.insert({{X{"abc"}, 1}, {X{"def"}, 2}, {X{"ghi"}, 3}});
  auto node = amap.extract(Y{std::string_view{"def"}});
  VERIFY(node.key().s == X{"def"}.s);
  VERIFY(node.mapped() == 2);
  VERIFY(amap.size() == 2);
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
  std::map<X, int, cmp> amap{cmp{}};
  amap.insert(
    {{X{"abcdef"}, 1}, {X{"defghi"}, 2}, {X{"defjkl"}, 3}, {X{"jklmno"}, 4}});
  auto n = amap.erase(Z{std::string_view{"def"}});
  VERIFY(n == 2);
  VERIFY(amap.size() == 2);
}

int main()
{
  test1();
  test2();
  test3();
  test4();
  test5();
}
