// { dg-do run { target c++26 } }

#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <compare>
#include <cstring>
#include <testsuite_hooks.h>

struct Y;
struct Z;

struct X {
  std::string s;
  X(std::string_view str) : s(str) {}
  X(Y&& y);
  X(const Y& y);
  X(Z&& z);
  X(const Z& z);

  friend auto operator<=>(X const& a, X const& b) = default;
};

struct Y {
  std::string s;
  Y() = default;
  Y(std::string_view sv) : s(sv) {}
  Y(int a, int b = 0) : s(std::string('a', a + b)) {}
  Y(Y&& y) : s(std::move(y.s)) { y.s.clear(); }
  Y(const Y& y) = default;
  Y& operator=(Y&& y) { s = std::move(y.s); y.s.clear(); return *this; }
  Y& operator=(const Y& y) = default;
  friend auto operator<=>(Y const& a, Y const& b) = default;
  friend auto operator<=>(X const& a, Y const& b) { return a.s <=> b.s; }
};

X::X(Y&& y) : s(std::move(y.s)) { y.s.clear(); }
X::X(const Y& y) : s(y.s) {}

using cmp = std::less<void>;

void test_root()
{
  std::set<X, cmp> aset{cmp{}};
  aset.insert(Y{"def"});
  VERIFY(aset.size() == 1);
  VERIFY(aset.contains(X{"def"}));
  aset.insert(Y{"abc"});
  VERIFY(aset.size() == 2);
  VERIFY(aset.contains(X{"abc"}));
  aset.insert(Y{"ghi"});
  VERIFY(aset.size() == 3);
  VERIFY(aset.contains(X{"ghi"}));
}

void test_insert()
{
  std::set<X, cmp> aset{cmp{}};
  aset.insert({Y{"abc"}, Y{"def"}, Y{"ghi"}});

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

struct Z {
  std::string s;
  mutable int compares = 0;

  Z() = default;
  Z(Z&& z) : s(std::move(z.s)) { z.s.clear(); }
  Z(const Z& z) = default;
  Z& operator=(Z&& z) { s = std::move(z.s); z.s.clear(); return *this; }
  Z& operator=(const Z& z) = default;
  Z(std::string_view sv) : s(sv) {}
  Z(int n) : s(std::string(n, 'a')) {}
  friend auto operator<=>(Z const& a, Z const& b) = default;
  friend auto operator<=>(X const& a, Z const& b)
    { return ++b.compares, a.s.substr(0, b.s.size()) <=> b.s; }
};

X::X(Z&& z) : s(std::move(z.s)) { z.s.clear(); }
X::X(const Z& z) : s(z.s) {}

// A heterogeneous key type like Z here that compares equal
// if it matches just the first part of the key is allowed,
// which affects op[], try_emplace, and insert_or_assign.

auto populate(auto a)
{
  const std::string vs[] = { "dec", "ded", "dee", "def", "deg", "deh", "dei" };
  for (auto const& v : vs)
    a.insert(Y{v});
  return a;
}

void test_insert_prefix()
{
  std::set<X, cmp> aset{cmp{}};
  aset.insert({Y{"abc"}, Y{"def"}, Y{"ghi"}});
  {
    { // Already there, fail.
      Z z{"de"};
      auto a = populate(aset);
      auto [it, res] = a.insert(std::move(z));
      VERIFY(!res);
      VERIFY(a.size() == 9);
      VERIFY(*it == X{"dec"});
      VERIFY(!a.contains(Y{"de"}));
      VERIFY(a.contains(Y{"dec"}));
      VERIFY(a.contains(Y{"ded"}));
      VERIFY(a.contains(Y{"dee"}));
      VERIFY(a.contains(Y{"def"}));
      VERIFY(a.contains(Y{"deg"}));
      VERIFY(a.contains(Y{"deh"}));
      VERIFY(a.contains(Y{"dei"}));
      VERIFY(z.s.size() == 2);   // not moved from
      VERIFY(z.compares > 3);
    }
  }
  {
    {  // hinted, succeed
      Z z{"aaa"};
      auto a = populate(aset);
      auto it = a.insert(a.begin(), std::move(z));
      VERIFY(a.size() == 10);
      VERIFY(*it == X{"aaa"});
      VERIFY(a.contains(Y{"dec"}));
      VERIFY(a.contains(Y{"ded"}));
      VERIFY(a.contains(Y{"dee"}));
      VERIFY(a.contains(Y{"def"}));
      VERIFY(a.contains(Y{"deg"}));
      VERIFY(a.contains(Y{"deh"}));
      VERIFY(a.contains(Y{"dei"}));
      VERIFY(z.s.empty());  // moved from
      VERIFY(z.compares == 1);
    }
  }
  { // Hinted, already there, fail.
    {
      Z z{"de"};
      auto a = populate(aset);
      auto it = a.insert(a.begin(), std::move(z));
      VERIFY(a.size() == 9);
      VERIFY(*it == X{"dec"});
      VERIFY(a.contains(Y{"dec"}));
      VERIFY(a.contains(Y{"ded"}));
      VERIFY(a.contains(Y{"dee"}));
      VERIFY(a.contains(Y{"def"}));
      VERIFY(a.contains(Y{"deg"}));
      VERIFY(a.contains(Y{"deh"}));
      VERIFY(a.contains(Y{"dei"}));
      VERIFY(z.s.size() == 2);
      VERIFY(z.compares > 3);
    }
    {
      Z z{"de"};
      auto a = populate(aset);
      auto it = a.insert(a.find(X{"dec"}), std::move(z));
      VERIFY(a.size() == 9);
      VERIFY(*it == X{"dec"});
      VERIFY(a.contains(Y{"dec"}));
      VERIFY(a.contains(Y{"ded"}));
      VERIFY(a.contains(Y{"dee"}));
      VERIFY(a.contains(Y{"def"}));
      VERIFY(a.contains(Y{"deg"}));
      VERIFY(a.contains(Y{"deh"}));
      VERIFY(a.contains(Y{"dei"}));
      VERIFY(z.s.size() == 2);
      VERIFY(z.compares == 3);
    }
    {
      Z z{"de"};
      auto a = populate(aset);
      auto it = a.insert(a.find(X{"def"}), std::move(z));
      VERIFY(a.size() == 9);
      VERIFY(*it == X{"dec"});
      VERIFY(a.contains(Y{"dec"}));
      VERIFY(a.contains(Y{"ded"}));
      VERIFY(a.contains(Y{"dee"}));
      VERIFY(a.contains(Y{"def"}));
      VERIFY(a.contains(Y{"deg"}));
      VERIFY(a.contains(Y{"deh"}));
      VERIFY(a.contains(Y{"dei"}));
      VERIFY(z.s.size() == 2);
      VERIFY(z.compares > 3);
    }
    {
      Z z{"de"};
      auto a = populate(aset);
      auto it = a.insert(a.find(X{"dei"}), std::move(z));
      VERIFY(a.size() == 9);
      VERIFY(*it == X{"dec"});
      VERIFY(a.contains(Y{"dec"}));
      VERIFY(a.contains(Y{"ded"}));
      VERIFY(a.contains(Y{"dee"}));
      VERIFY(a.contains(Y{"def"}));
      VERIFY(a.contains(Y{"deg"}));
      VERIFY(a.contains(Y{"deh"}));
      VERIFY(a.contains(Y{"dei"}));
      VERIFY(z.s.size() == 2);
      VERIFY(z.compares > 3);
    }
    {
      Z z{"df"};
      auto a = populate(aset);
      auto it = a.insert(a.find(X{"dei"}), std::move(z));
      VERIFY(a.size() == 10);
      VERIFY(*it == X{"df"});
      VERIFY(a.contains(Y{"dec"}));
      VERIFY(a.contains(Y{"ded"}));
      VERIFY(a.contains(Y{"dee"}));
      VERIFY(a.contains(Y{"def"}));
      VERIFY(a.contains(Y{"deg"}));
      VERIFY(a.contains(Y{"deh"}));
      VERIFY(a.contains(Y{"dei"}));
      VERIFY(z.s.empty());  // moved from
      VERIFY(z.compares == 3);
    }
    {
      Z z{"de"};
      auto a = populate(aset);
      auto it = a.insert(a.find(X{"ghi"}), std::move(z));
      VERIFY(a.size() == 9);
      VERIFY(*it == X{"dec"});
      VERIFY(a.contains(Y{"dec"}));
      VERIFY(a.contains(Y{"ded"}));
      VERIFY(a.contains(Y{"dee"}));
      VERIFY(a.contains(Y{"def"}));
      VERIFY(a.contains(Y{"deg"}));
      VERIFY(a.contains(Y{"deh"}));
      VERIFY(a.contains(Y{"dei"}));
      VERIFY(z.s.size() == 2);
      VERIFY(z.compares > 3);
    }
    {
      Z z{"df"};
      auto a = populate(aset);
      auto it = a.insert(a.find(X{"ghi"}), std::move(z));
      VERIFY(a.size() == 10);
      VERIFY(*it == X{"df"});
      VERIFY(a.contains(Y{"dec"}));
      VERIFY(a.contains(Y{"ded"}));
      VERIFY(a.contains(Y{"dee"}));
      VERIFY(a.contains(Y{"def"}));
      VERIFY(a.contains(Y{"deg"}));
      VERIFY(a.contains(Y{"deh"}));
      VERIFY(a.contains(Y{"dei"}));
      VERIFY(z.s.empty());  // moved from
      VERIFY(z.compares == 2);
    }
    {
      Z z{"de"};
      auto a = populate(aset);
      auto it = a.insert(a.end(), std::move(z));
      VERIFY(a.size() == 9);
      VERIFY(*it == X{"dec"});
      VERIFY(a.contains(Y{"dec"}));
      VERIFY(a.contains(Y{"ded"}));
      VERIFY(a.contains(Y{"dee"}));
      VERIFY(a.contains(Y{"def"}));
      VERIFY(a.contains(Y{"deg"}));
      VERIFY(a.contains(Y{"deh"}));
      VERIFY(a.contains(Y{"dei"}));
      VERIFY(z.s.size() == 2);
      VERIFY(z.compares > 3);
    }
    {
      Z z{"jkl"};
      auto a = populate(aset);
      auto it = a.insert(a.find(X{"ghi"}), std::move(z));
      VERIFY(a.size() == 10);
      VERIFY(*it == X{"jkl"});
      VERIFY(a.contains(Y{"dec"}));
      VERIFY(a.contains(Y{"ded"}));
      VERIFY(a.contains(Y{"dee"}));
      VERIFY(a.contains(Y{"def"}));
      VERIFY(a.contains(Y{"deg"}));
      VERIFY(a.contains(Y{"deh"}));
      VERIFY(a.contains(Y{"dei"}));
      VERIFY(z.s.empty());  // moved from
      VERIFY(z.compares == 2);
    }
    {
      Z z{"jkl"};
      auto a = populate(aset);
      auto it = a.insert(a.end(), std::move(z));
      VERIFY(a.size() == 10);
      VERIFY(*it == X{"jkl"});
      VERIFY(a.contains(Y{"dec"}));
      VERIFY(a.contains(Y{"ded"}));
      VERIFY(a.contains(Y{"dee"}));
      VERIFY(a.contains(Y{"def"}));
      VERIFY(a.contains(Y{"deg"}));
      VERIFY(a.contains(Y{"deh"}));
      VERIFY(a.contains(Y{"dei"}));
      VERIFY(z.s.empty());  // moved from
      VERIFY(z.compares == 1);
    }
  }
}

int main()
{
  test_root();
  test_insert();
  test_insert_prefix();
}
