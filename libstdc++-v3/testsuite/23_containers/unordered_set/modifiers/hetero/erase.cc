// { dg-do run { target c++23 } }

#include <unordered_set>
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
  std::unordered_set<X, Hash, Equal> aset;
  aset.insert({X{"abc"}, X{"def"}, X{"ghi"}});
  auto n = aset.erase(X{std::string{"def"}});
  VERIFY(n == 1);
  VERIFY(aset.size() == 2);
}

void test2()  // heterogeneous erase
{
  std::unordered_set<X, Hash, Equal> aset;
  aset.insert({X{"abc"}, X{"def"}, X{"ghi"}});
  auto n = aset.erase(Y{std::string_view{"def"}});
  VERIFY(n == 1);
  VERIFY(aset.size() == 2);
}

void test3() // uniform extract
{
  std::unordered_set<X, Hash, Equal> aset;
  aset.insert({X{"abc"}, X{"def"}, X{"ghi"}});
  auto node = aset.extract(X{std::string{"def"}});
  VERIFY(node.value().s == X{"def"}.s);
  VERIFY(aset.size() == 2);
}

void test4()  // heterogeneous extract
{
  std::unordered_set<X, Hash, Equal> aset;
  aset.insert({X{"abc"}, X{"def"}, X{"ghi"}});
  auto node = aset.extract(Y{std::string_view{"def"}});
  VERIFY(node.value().s == X{"def"}.s);
  VERIFY(aset.size() == 2);
}

int main()
{
  test1();
  test2();
  test3();
  test4();
}
