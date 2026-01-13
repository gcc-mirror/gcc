// { dg-do run { target c++26 } }

#include <map>
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
  X(Y&&);
  X(const Y&);
  X(Z&&);
  X(const Z&);
  friend auto operator<=>(X const& a, X const& b) = default;
};

struct Y {
  std::string s;
  Y() = default;
  Y(Y&& y) : s(std::move(y.s)) { y.s.clear(); }
  Y(const Y& y) = default;
  Y& operator=(Y&& y) { s = std::move(y.s); y.s.clear(); return *this; }
  Y& operator=(const Y& y) = default;
  Y(std::string_view sv) : s(sv) {}
  Y(int n) : s(std::string(n, 'a')) {}
  Y(const Y& a, const Y& b) : s(a.s + "1" + b.s) { }
  Y(const Y& a, Y&& b)      : s(a.s + "2" + b.s) { b.s.clear(); }
  Y(Y&& a, const Y& b)      : s(a.s + "3" + b.s) { a.s.clear(); }
  Y(Y&& a, Y&& b)           : s(a.s + "4" + b.s) { a.s.clear(), b.s.clear(); }
  friend auto operator<=>(Y const& a, Y const& b) = default;
  friend auto operator<=>(X const& a, Y const& b) { return a.s <=> b.s; }
};

X::X(Y&& y) : s(std::move(y.s)) { y.s.clear(); }
X::X(const Y& y) : s(y.s) {}

using cmp = std::less<void>;

void test_at()
{
  std::map<X, Y, cmp> amap{cmp{}};
  amap.insert( {{Y{"abc"}, 1},
    {Y{"dee"}, 2}, {Y{"def"}, 3}, {Y{"deg"}, 4},
    {Y{"ghi"}, 5}});

  Y y{"def"};
  try
    {
      VERIFY(3 == amap.at(y));
      VERIFY(amap.size() == 5);
      VERIFY(4 == (amap.at(std::move(y)) = 4));
      VERIFY(amap.size() == 5);
      VERIFY(y.s.size() == 3);  // not moved from
    }
  catch(...) { VERIFY(false); }
  try
  {
    amap.at(Y{"dea"}) = 4;
    VERIFY(false);  // Should have thrown.
  }
  catch (std::out_of_range&) { VERIFY(amap.size() == 5); }
  catch (...) { VERIFY(false); } // Wrong exception.

  auto const& amapr{amap};
  Y z{"deh"};
  try
    {
      amapr.at(std::move(z));
      VERIFY(false);  // Should have thrown.
    }
  catch (std::out_of_range&) {  }
  catch (...) { VERIFY(false); } // Wrong exception.
  VERIFY(amapr.size() == 5);
  VERIFY(z.s.size() == 3);  // not moved from
}

void test_op_bracket()
{
  std::map<X, Y, cmp> amap{cmp{}};
  amap.insert({{Y{"abc"}, 1}, {Y{"def"}, 2}, {Y{"ghi"}, 3}});
  {
    Y z{"deg"};
    amap[z] = 4;
    VERIFY(amap.size() == 4);
    VERIFY(z.s.size() == 3);  // pass by value, not moved from.
    amap[std::move(z)] = Y{5};
    VERIFY(amap.size() == 4);
    VERIFY(amap.at(z) == Y{5});
    VERIFY(z.s.size() == 3);  // already there, not moved from.
  }
  {
    Y y{"deh"};
    Y& v = amap[std::move(y)];
    VERIFY(v == Y{});
    VERIFY(amap.size() == 5);
    VERIFY(amap.at(Y{"deh"}) == Y{});
    VERIFY(y.s.empty());      // moved from.
  }
  {
    Y x{"dei"};
    amap[std::move(x)] = Y{7};
    VERIFY(amap.size() == 6);
    VERIFY(amap.at(Y{"dei"}) == Y{7});
    VERIFY(x.s.empty());      // moved from
  }
}

void test_try_emplace()
{
  std::map<X, Y, cmp> amap{cmp{}};
  amap.insert({{Y{"abc"}, 1}, {Y{"def"}, 2}, {Y{"ghi"}, 3}});

  { // Fail, already there
    auto a = amap;
    auto [it, res] = a.try_emplace(Y{"def"}, Y{"xyz"});
    VERIFY(!res);
    VERIFY(a.size() == 3);
    VERIFY(a.at(Y{"def"}) == Y{2}); // unchanged
  }
  { // Fail, already there, move
    auto a = amap;
    Y x{"def"};
    Y y{"xyz"};
    auto [it, res] = a.try_emplace(std::move(x), std::move(y));
    VERIFY(!res);
    VERIFY(a.size() == 3);
    VERIFY(a.at(Y{"def"}) == Y{2}); // unchanged
    VERIFY(x.s.size() == 3);  // not moved from
    VERIFY(y.s.size() == 3);  // not moved from
  }
  { // Succeed, construct
    auto a = amap;
    {
      Y m{"m"}, n{"n"};
      auto [it, res] = a.try_emplace(Y{"deg"}, m, n);
      VERIFY(res);
      VERIFY(a.size() == 4);
      VERIFY(it->first == X{"deg"});
      VERIFY(it->second == Y{"m1n"});
      VERIFY(m.s.size() == 1);
      VERIFY(n.s.size() == 1);
    }
    {
      Y m{"m"}, n{"n"};
      auto [it, res] = a.try_emplace(Y{"deh"}, m, std::move(n));
      VERIFY(res);
      VERIFY(a.size() == 5);
      VERIFY(it->first == X{"deh"});
      VERIFY(it->second == Y{"m2n"});
      VERIFY(m.s.size() == 1);
      VERIFY(n.s.empty());
    }
    {
      Y m{"m"}, o{"o"};
      auto [it, res] = a.try_emplace(Y{"dei"}, std::move(m), o);
      VERIFY(res);
      VERIFY(a.size() == 6);
      VERIFY(it->first == X{"dei"});
      VERIFY(it->second == Y{"m3o"});
      VERIFY(m.s.empty());
      VERIFY(o.s.size() == 1);
    }
    {
      Y o{"o"}, p{"p"};
      auto [it, res] = a.try_emplace(Y{"dej"}, std::move(o), std::move(p));
      VERIFY(res);
      VERIFY(a.size() == 7);
      VERIFY(it->first == X{"dej"});
      VERIFY(it->second == Y{"o4p"});
      VERIFY(o.s.empty());
      VERIFY(p.s.empty());
    }
    {
      Y dek{"dek"};
      auto [it, res] = a.try_emplace(std::move(dek), Y("q"), Y("r"));
      VERIFY(res);
      VERIFY(a.size() == 8);
      VERIFY(dek.s.empty());
      VERIFY(it->first == X{"dek"});
      VERIFY(it->second == Y{"q4r"});
    }
  }
  { // Succeed, move
    auto a = amap;
    Y y{"tuv"}, z{"xyz"};
    auto [it, res] = a.try_emplace(std::move(y), std::move(z));
    VERIFY(res);
    VERIFY(it->first == X{"tuv"});
    VERIFY(it->second == Y{"xyz"});
    VERIFY(a.size() == 4);
    VERIFY(y.s.empty()); // moved from
    VERIFY(z.s.empty()); // moved from
  }
  { // Hinted, fail
    auto a = amap;
    Y y{"def"}, z{"xyz"};
    auto it = a.try_emplace(a.begin(), std::move(y), std::move(z));
    VERIFY(a.size() == 3);
    VERIFY(it->first == X{"def"});
    VERIFY(it->second == Y{2});
    VERIFY(y.s.size() == 3);  // not moved from
    VERIFY(z.s.size() == 3);  // not moved from
  }
  { // Hinted, fail, move
    auto a = amap;
    Y y{"def"}, z{"xyz"};
    auto it = a.try_emplace(a.begin(), std::move(y), std::move(z));
    VERIFY(a.size() == 3);
    VERIFY(it->first == X{"def"});
    VERIFY(it->second == Y{2});
    VERIFY(y.s.size() == 3);  // not moved from
    VERIFY(z.s.size() == 3);  // not moved from
  }
  { // Hinted, succeed, construct
    auto a = amap;
    {
      Y m("m"), n("n");
      auto it = a.try_emplace(a.begin(), Y{"deg"}, m, n);
      VERIFY(a.size() == 4);
      VERIFY(it->first == X{"deg"});
      VERIFY(it->second == Y{"m1n"});
      VERIFY(m.s.size() == 1);
      VERIFY(n.s.size() == 1);
    }
    {
      Y m("m"), n("n");
      auto it = a.try_emplace(a.begin(), Y{"deh"}, m, std::move(n));
      VERIFY(a.size() == 5);
      VERIFY(it->first == X{"deh"});
      VERIFY(it->second == Y{"m2n"});
      VERIFY(m.s.size() == 1);
      VERIFY(n.s.empty());
    }
    {
      Y m("m"), o("o");
      auto it = a.try_emplace(a.begin(), Y{"dei"}, std::move(m), o);
      VERIFY(a.size() == 6);
      VERIFY(it->first == X{"dei"});
      VERIFY(it->second == Y{"m3o"});
      VERIFY(m.s.empty());
      VERIFY(o.s.size() == 1);
    }
    {
      Y o("o"), p("p");
      auto it = a.try_emplace(a.begin(), Y{"dej"}, std::move(o), std::move(p));
      VERIFY(a.size() == 7);
      VERIFY(it->first == X{"dej"});
      VERIFY(it->second == Y{"o4p"});
      VERIFY(o.s.empty());
      VERIFY(p.s.empty());
    }
    {
      Y dek("dek");
      auto it = a.try_emplace(a.begin(), std::move(dek), Y("q"), Y("r"));
      VERIFY(a.size() == 8);
      VERIFY(dek.s.empty());
      VERIFY(it->first == X{"dek"});
      VERIFY(it->second == Y{"q4r"});
    }
  }
  {  // Hinted, succeed, move
    auto a = amap;
    Y y{"tuv"}, z{"xyz"};
    auto it = a.try_emplace(a.begin(), std::move(y), std::move(z));
    VERIFY(it->first == X{"tuv"});
    VERIFY(it->second == Y{"xyz"});
    VERIFY(a.size() == 4);
    VERIFY(y.s.empty()); // moved from
    VERIFY(z.s.empty());  // moved from
  }
}

void test_insert_or_assign()
{
  std::map<X, Y, cmp> amap{cmp{}};
  amap.insert({{Y{"abc"}, 1}, {Y{"def"}, 2}, {Y{"ghi"}, 3}});

  { // Already there, replace
    auto a = amap;
    Y y{"def"}, z{"xyz"};
    auto [it, res] = a.insert_or_assign(y, z);
    VERIFY(!res);
    VERIFY(a.size() == 3);
    VERIFY(a.at(Y{"def"}) == Y{"xyz"});
    VERIFY(y.s.size() == 3);  // not moved from
    VERIFY(z.s.size() == 3);  // not moved from
  }
  { // Already there, move
    auto a = amap;
    Y y{"def"}, z{"xyz"};
    auto [it, res] = a.insert_or_assign(std::move(y), std::move(z));
    VERIFY(!res);
    VERIFY(a.size() == 3);
    VERIFY(a.at(Y{"def"}) == Y{"xyz"});
    VERIFY(y.s.size() == 3);  // not moved from
    VERIFY(z.s.empty());      // moved from
  }
  { // Succeed, move
    auto a = amap;
    Y y{"tuv"}, z{"xyz"};
    auto [it, res] = a.insert_or_assign(std::move(y), std::move(z));
    VERIFY(res);
    VERIFY(it->first == X{"tuv"});
    VERIFY(it->second == Y{"xyz"});
    VERIFY(a.size() == 4);
    VERIFY(y.s.empty()); // moved from
    VERIFY(z.s.empty()); // moved from
  }
  { // Hinted, already there, replace
    auto a = amap;
    Y y{"def"}, z{"xyz"};
    auto it = a.insert_or_assign(a.begin(), y, z);
    VERIFY(a.size() == 3);
    VERIFY(it->first == X{"def"});
    VERIFY(it->second == Y{"xyz"});
    VERIFY(y.s.size() == 3);  // not moved from
    VERIFY(z.s.size() == 3);  // not moved from
  }
  { // Hinted, already there, move
    auto a = amap;
    Y y{"def"}, z{"xyz"};
    auto it = a.insert_or_assign(a.begin(), std::move(y), std::move(z));
    VERIFY(a.size() == 3);
    VERIFY(it->first == X{"def"});
    VERIFY(it->second == Y{"xyz"});
    VERIFY(y.s.size() == 3);  // not moved from
    VERIFY(z.s.empty());      // moved from
  }
  {  // Hinted, succeed
    auto a = amap;
    Y y{"tuv"}, z{"xyz"};
    auto it = a.insert_or_assign(a.begin(), y, z);
    VERIFY(it->first == X{"tuv"});
    VERIFY(it->second == Y{"xyz"});
    VERIFY(a.size() == 4);
    VERIFY(y.s.size() == 3);  // not moved from
    VERIFY(z.s.size() == 3);  // not moved from
  }
  {  // Hinted, succeed, move
    auto a = amap;
    Y y{"tuv"}, z{"xyz"};
    auto it = a.insert_or_assign(a.begin(), std::move(y), std::move(z));
    VERIFY(it->first == X{"tuv"});
    VERIFY(it->second == Y{"xyz"});
    VERIFY(a.size() == 4);
    VERIFY(y.s.empty());  // moved from
    VERIFY(z.s.empty());  // moved from
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
    a[Y{v}] = Y{v};
  return a;
}

void test_op_bracket_prefix()
{
  std::map<X, Y, cmp> amap{cmp{}};
  amap.insert({{Y{"abc"}, 1}, {Y{"def"}, 2}, {Y{"ghi"}, 3}});
  { // Already there, multiple matches
    auto a = populate(amap);
    VERIFY(a.size() == 9);
    Z z{"de"};
    a[std::move(z)] = Y{5};
    VERIFY(a.size() == 9);
    VERIFY(a.at(X{"dec"}) == Y{5});  // lower_bound match changed
    VERIFY(a.at(X{"ded"}) == Y{"ded"});
    VERIFY(a.at(X{"dee"}) == Y{"dee"});
    VERIFY(a.at(X{"def"}) == Y{"def"});
    VERIFY(a.at(X{"deg"}) == Y{"deg"});
    VERIFY(a.at(X{"deh"}) == Y{"deh"});
    VERIFY(a.at(X{"dei"}) == Y{"dei"});
    VERIFY(z.s.size() == 2);  // not moved from
  }
}

void test_try_emplace_prefix()
{
  std::map<X, Y, cmp> amap{cmp{}};
  amap.insert({{Y{"abc"}, 1}, {Y{"def"}, 2}, {Y{"ghi"}, 3}});
  {
    auto a = populate(amap);
    VERIFY(a.size() == 9);
    {
      Z z{"de"};
      auto [it, res] = a.try_emplace(std::move(z), 5);
      VERIFY(a.size() == 9);
      VERIFY(!res);
      VERIFY(it->first == X{"dec"});
      VERIFY(it->second == Y{"dec"});
      VERIFY(a.at(X{"dec"}) == Y{"dec"});  // lower_bound match unchanged
      VERIFY(a.at(X{"ded"}) == Y{"ded"});  // unchanged
      VERIFY(a.at(X{"dee"}) == Y{"dee"});  // unchanged
      VERIFY(a.at(X{"def"}) == Y{"def"});  // unchanged
      VERIFY(a.at(X{"deg"}) == Y{"deg"});  // unchanged
      VERIFY(a.at(X{"deh"}) == Y{"deh"});  // unchanged
      VERIFY(a.at(X{"dei"}) == Y{"dei"});  // unchanged
      VERIFY(z.s.size() == 2);  // not moved from
    }
    {
      Z z{"df"};
      auto [it, res] = a.try_emplace(std::move(z), Y{6});
      VERIFY(a.size() == 10);
      VERIFY(res);
      VERIFY(it->first == X{"df"});
      VERIFY(it->second == Y{6});
      VERIFY(z.s.size() == 0);  // moved from
      auto it2 = a.find(X{"dei"});
      VERIFY(it2 != a.end());
      ++it2;
      VERIFY(it2 != a.end());
      VERIFY(it2->first == X{"df"});
    }
  }
  { // hinted
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"aaa"};
      auto it = a.try_emplace(a.begin(), std::move(z), 5);
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"aaa"});
      VERIFY(it->second == Y{5});
      VERIFY(a.at(X{"dec"}) == Y{"dec"});  // unchanged
      VERIFY(a.at(X{"ded"}) == Y{"ded"});  // unchanged
      VERIFY(a.at(X{"dee"}) == Y{"dee"});  // unchanged
      VERIFY(a.at(X{"def"}) == Y{"def"});  // unchanged
      VERIFY(a.at(X{"deg"}) == Y{"deg"});  // unchanged
      VERIFY(a.at(X{"deh"}) == Y{"deh"});  // unchanged
      VERIFY(a.at(X{"dei"}) == Y{"dei"});  // unchanged
      VERIFY(z.s.empty());  // moved from
      VERIFY(z.compares == 1);
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"de"};
      auto it = a.try_emplace(a.find(X{"dec"}), std::move(z), 5);
      VERIFY(a.size() == 9);
      VERIFY(it->first == X{"dec"});
      VERIFY(it->second == Y{"dec"});
      VERIFY(a.at(X{"dec"}) == Y{"dec"});  // unchanged
      VERIFY(a.at(X{"ded"}) == Y{"ded"});  // unchanged
      VERIFY(a.at(X{"dee"}) == Y{"dee"});  // unchanged
      VERIFY(a.at(X{"def"}) == Y{"def"});  // unchanged
      VERIFY(a.at(X{"deg"}) == Y{"deg"});  // unchanged
      VERIFY(a.at(X{"deh"}) == Y{"deh"});  // unchanged
      VERIFY(a.at(X{"dei"}) == Y{"dei"});  // unchanged
      VERIFY(z.s.size() == 2);  // not moved from
      VERIFY(z.compares == 3);
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"df"};
      auto it = a.try_emplace(a.begin(), std::move(z), Y{6});
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"df"});
      VERIFY(it->second == Y{6});
      VERIFY(z.s.size() == 0);  // moved from
      VERIFY(z.compares > 3);
      auto it2 = a.find(X{"dei"});
      VERIFY(it2 != a.end());
      ++it2;
      VERIFY(it2 != a.end());
      VERIFY(it2->first == X{"df"});
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"df"};
      auto it = a.try_emplace(std::next(a.begin()), std::move(z), Y{6});
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"df"});
      VERIFY(it->second == Y{6});
      VERIFY(z.s.size() == 0);  // moved from
      VERIFY(z.compares > 3);
      auto it2 = a.find(X{"dei"});
      VERIFY(it2 != a.end());
      ++it2;
      VERIFY(it2 != a.end());
      VERIFY(it2->first == X{"df"});
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"df"};
      auto it = a.try_emplace(std::prev(a.end()), std::move(z), Y{6});
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"df"});
      VERIFY(it->second == Y{6});
      VERIFY(z.s.size() == 0);  // moved from
      VERIFY(z.compares == 2);
      auto it2 = a.find(X{"dei"});
      VERIFY(it2 != a.end());
      ++it2;
      VERIFY(it2 != a.end());
      VERIFY(it2->first == X{"df"});
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"df"};
      auto it = a.try_emplace(a.end(), std::move(z), Y{6});
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"df"});
      VERIFY(it->second == Y{6});
      VERIFY(z.s.size() == 0);  // moved from
      VERIFY(z.compares > 3);
      auto it2 = a.find(X{"dei"});
      VERIFY(it2 != a.end());
      ++it2;
      VERIFY(it2 != a.end());
      VERIFY(it2->first == X{"df"});
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"df"};
      auto it = a.try_emplace(a.find(X{"dei"}), std::move(z), Y{6});
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"df"});
      VERIFY(it->second == Y{6});
      VERIFY(z.s.size() == 0);  // moved from
      VERIFY(z.compares == 3);
      auto it2 = a.find(X{"dei"});
      VERIFY(it2 != a.end());
      ++it2;
      VERIFY(it2 != a.end());
      VERIFY(it2->first == X{"df"});
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"df"};
      auto it = a.try_emplace(a.find(X{"dec"}), std::move(z), Y{6});
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"df"});
      VERIFY(it->second == Y{6});
      VERIFY(z.s.size() == 0);  // moved from
      VERIFY(z.compares > 3);
      auto it2 = a.find(X{"dei"});
      VERIFY(it2 != a.end());
      ++it2;
      VERIFY(it2 != a.end());
      VERIFY(it2->first == X{"df"});
    }
  }
  { // hinted
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"de"};
      auto it = a.try_emplace(a.find(X{"dec"}), std::move(z), 5);
      VERIFY(a.size() == 9);
      VERIFY(it->first == X{"dec"});
      VERIFY(it->second == Y{"dec"});
      VERIFY(a.at(X{"dec"}) == Y{"dec"});  // unchanged
      VERIFY(a.at(X{"ded"}) == Y{"ded"});  // unchanged
      VERIFY(a.at(X{"dee"}) == Y{"dee"});  // unchanged
      VERIFY(a.at(X{"def"}) == Y{"def"});  // unchanged
      VERIFY(a.at(X{"deg"}) == Y{"deg"});  // unchanged
      VERIFY(a.at(X{"deh"}) == Y{"deh"});  // unchanged
      VERIFY(a.at(X{"dei"}) == Y{"dei"});  // unchanged
      VERIFY(z.s.size() == 2);  // not moved from
      VERIFY(z.compares == 3);
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"de"};
      auto it = a.try_emplace(a.begin(), std::move(z), 5);
      VERIFY(a.size() == 9);
      VERIFY(it->first == X{"dec"});
      VERIFY(it->second == Y{"dec"});
      VERIFY(a.at(X{"dec"}) == Y{"dec"});  // unchanged
      VERIFY(a.at(X{"ded"}) == Y{"ded"});  // unchanged
      VERIFY(a.at(X{"dee"}) == Y{"dee"});  // unchanged
      VERIFY(a.at(X{"def"}) == Y{"def"});  // unchanged
      VERIFY(a.at(X{"deg"}) == Y{"deg"});  // unchanged
      VERIFY(a.at(X{"deh"}) == Y{"deh"});  // unchanged
      VERIFY(a.at(X{"dei"}) == Y{"dei"});  // unchanged
      VERIFY(z.s.size() == 2);  // not moved from
      VERIFY(z.compares > 3);
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"de"};
      auto it = a.try_emplace(a.find(X{"dei"}), std::move(z), 5);
      VERIFY(a.size() == 9);
      VERIFY(it->first == X{"dec"});
      VERIFY(it->second == Y{"dec"});
      VERIFY(a.at(X{"dec"}) == Y{"dec"});  // unchanged
      VERIFY(a.at(X{"ded"}) == Y{"ded"});  // unchanged
      VERIFY(a.at(X{"dee"}) == Y{"dee"});  // unchanged
      VERIFY(a.at(X{"def"}) == Y{"def"});  // unchanged
      VERIFY(a.at(X{"deg"}) == Y{"deg"});  // unchanged
      VERIFY(a.at(X{"deh"}) == Y{"deh"});  // unchanged
      VERIFY(a.at(X{"dei"}) == Y{"dei"});  // unchanged
      VERIFY(z.s.size() == 2);  // not moved from
      VERIFY(z.compares > 3);
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"de"};
      auto it = a.try_emplace(a.find(X{"ghi"}), std::move(z), 5);
      VERIFY(a.size() == 9);
      VERIFY(it->first == X{"dec"});
      VERIFY(it->second == Y{"dec"});
      VERIFY(a.at(X{"dec"}) == Y{"dec"});  // unchanged
      VERIFY(a.at(X{"ded"}) == Y{"ded"});  // unchanged
      VERIFY(a.at(X{"dee"}) == Y{"dee"});  // unchanged
      VERIFY(a.at(X{"def"}) == Y{"def"});  // unchanged
      VERIFY(a.at(X{"deg"}) == Y{"deg"});  // unchanged
      VERIFY(a.at(X{"deh"}) == Y{"deh"});  // unchanged
      VERIFY(a.at(X{"dei"}) == Y{"dei"});  // unchanged
      VERIFY(z.s.size() == 2);  // not moved from
      VERIFY(z.compares > 3);
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"de"};
      auto it = a.try_emplace(a.end(), std::move(z), 5);
      VERIFY(a.size() == 9);
      VERIFY(it->first == X{"dec"});
      VERIFY(it->second == Y{"dec"});
      VERIFY(a.at(X{"dec"}) == Y{"dec"});  // unchanged
      VERIFY(a.at(X{"ded"}) == Y{"ded"});  // unchanged
      VERIFY(a.at(X{"dee"}) == Y{"dee"});  // unchanged
      VERIFY(a.at(X{"def"}) == Y{"def"});  // unchanged
      VERIFY(a.at(X{"deg"}) == Y{"deg"});  // unchanged
      VERIFY(a.at(X{"deh"}) == Y{"deh"});  // unchanged
      VERIFY(a.at(X{"dei"}) == Y{"dei"});  // unchanged
      VERIFY(z.s.size() == 2);  // not moved from
      VERIFY(z.compares > 3);
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"df"};
      auto it = a.try_emplace(a.begin(), std::move(z), Y{6});
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"df"});
      VERIFY(it->second == Y{6});
      VERIFY(z.s.size() == 0);  // moved from
      VERIFY(z.compares > 3);
      auto it2 = a.find(X{"dei"});
      VERIFY(it2 != a.end());
      ++it2;
      VERIFY(it2 != a.end());
      VERIFY(it2->first == X{"df"});
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"df"};
      auto it = a.try_emplace(std::next(a.begin()), std::move(z), Y{6});
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"df"});
      VERIFY(it->second == Y{6});
      VERIFY(z.s.size() == 0);  // moved from
      VERIFY(z.compares > 3);
      auto it2 = a.find(X{"dei"});
      VERIFY(it2 != a.end());
      ++it2;
      VERIFY(it2 != a.end());
      VERIFY(it2->first == X{"df"});
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"df"};
      auto it = a.try_emplace(a.find(X{"dei"}), std::move(z), Y{6});
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"df"});
      VERIFY(it->second == Y{6});
      VERIFY(z.s.size() == 0);  // moved from
      VERIFY(z.compares == 3);
      auto it2 = a.find(X{"dei"});
      VERIFY(it2 != a.end());
      ++it2;
      VERIFY(it2 != a.end());
      VERIFY(it2->first == X{"df"});
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"df"};
      auto it = a.try_emplace(a.find(X{"dec"}), std::move(z), Y{6});
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"df"});
      VERIFY(it->second == Y{6});
      VERIFY(z.s.size() == 0);  // moved from
      VERIFY(z.compares > 3);
      auto it2 = a.find(X{"dei"});
      VERIFY(it2 != a.end());
      ++it2;
      VERIFY(it2 != a.end());
      VERIFY(it2->first == X{"df"});
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"df"};
      auto it = a.try_emplace(std::prev(a.end()), std::move(z), Y{6});
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"df"});
      VERIFY(it->second == Y{6});
      VERIFY(z.s.size() == 0);  // moved from
      VERIFY(z.compares == 2);
      auto it2 = a.find(X{"dei"});
      VERIFY(it2 != a.end());
      ++it2;
      VERIFY(it2 != a.end());
      VERIFY(it2->first == X{"df"});
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"df"};
      auto it = a.try_emplace(a.end(), std::move(z), Y{6});
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"df"});
      VERIFY(it->second == Y{6});
      VERIFY(z.s.size() == 0);  // moved from
      VERIFY(z.compares > 3);
      auto it2 = a.find(X{"dei"});
      VERIFY(it2 != a.end());
      ++it2;
      VERIFY(it2 != a.end());
      VERIFY(it2->first == X{"df"});
    }
  }
}

void test_insert_or_assign_prefix()
{
  std::map<X, Y, cmp> amap{cmp{}};
  amap.insert({{Y{"abc"}, 1}, {Y{"def"}, 2}, {Y{"ghi"}, 3}});

  { // Already there, replace
    {
      Z z{"de"};
      auto a = populate(amap);
      auto [it, res] = a.insert_or_assign(z, Y{5});
      VERIFY(!res);
      VERIFY(a.size() == 9);
      VERIFY(it->first == X{"dec"});
      VERIFY(it->second == Y{5});
      VERIFY(a.at(Y{"dec"}) == Y{5});  // lower_bound changed
      VERIFY(a.at(Y{"ded"}) == Y{"ded"});  // unchanged
      VERIFY(a.at(Y{"dee"}) == Y{"dee"});  // unchanged
      VERIFY(a.at(Y{"def"}) == Y{"def"});  // unchanged
      VERIFY(a.at(Y{"deg"}) == Y{"deg"});  // unchanged
      VERIFY(a.at(Y{"deh"}) == Y{"deh"});  // unchanged
      VERIFY(a.at(Y{"dei"}) == Y{"dei"});  // unchanged
      VERIFY(z.s.size() == 2);   // not moved from
    }
    {
      Z z{"de"};
      auto a = populate(amap);
      auto [it, res] = a.insert_or_assign(std::move(z), Y{5});
      VERIFY(!res);
      VERIFY(a.size() == 9);
      VERIFY(it->first == X{"dec"});
      VERIFY(it->second == Y{5});
      VERIFY(a.at(Y{"dec"}) == Y{5}); // lower_bound changed
      VERIFY(a.at(Y{"ded"}) == Y{"ded"});  // unchanged
      VERIFY(a.at(Y{"dee"}) == Y{"dee"});  // unchanged
      VERIFY(a.at(Y{"def"}) == Y{"def"});  // unchanged
      VERIFY(a.at(Y{"deg"}) == Y{"deg"});  // unchanged
      VERIFY(a.at(Y{"deh"}) == Y{"deh"});  // unchanged
      VERIFY(a.at(Y{"dei"}) == Y{"dei"});  // unchanged
      VERIFY(z.s.size() == 2);   // not moved from
    }
  }
  { // Hinted, already there, replace
    {
      Z z{"de"};
      auto a = populate(amap);
      auto it = a.insert_or_assign(a.find(X{"dec"}), z, Y{5});
      VERIFY(a.size() == 9);
      VERIFY(it->first == X{"dec"});
      VERIFY(it->second == Y{5});
      VERIFY(a.at(Y{"dec"}) == Y{5}); // lower_bound changed
      VERIFY(a.at(Y{"ded"}) == Y{"ded"});  // unchanged
      VERIFY(a.at(Y{"dee"}) == Y{"dee"});  // unchanged
      VERIFY(a.at(Y{"def"}) == Y{"def"});  // unchanged
      VERIFY(a.at(Y{"deg"}) == Y{"deg"});  // unchanged
      VERIFY(a.at(Y{"deh"}) == Y{"deh"});  // unchanged
      VERIFY(a.at(Y{"dei"}) == Y{"dei"});  // unchanged
      VERIFY(z.s.size() == 2);   // not moved from
      VERIFY(z.compares == 3);
    }
    {
      Z z{"de"};
      auto a = populate(amap);
      auto it = a.insert_or_assign(a.end(), std::move(z), Y{5});
      VERIFY(a.size() == 9);
      VERIFY(it->first == X{"dec"});
      VERIFY(it->second == Y{5});
      VERIFY(a.at(Y{"dec"}) == Y{5});   // lower_bound changed
      VERIFY(a.at(Y{"ded"}) == Y{"ded"});  // unchanged
      VERIFY(a.at(Y{"dee"}) == Y{"dee"});  // unchanged
      VERIFY(a.at(Y{"def"}) == Y{"def"});  // unchanged
      VERIFY(a.at(Y{"deg"}) == Y{"deg"});  // unchanged
      VERIFY(a.at(Y{"deh"}) == Y{"deh"});  // unchanged
      VERIFY(a.at(Y{"dei"}) == Y{"dei"});  // unchanged
      VERIFY(z.s.size() == 2);   // not moved from
      VERIFY(z.compares > 3);
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"df"};
      auto it = a.try_emplace(a.find(X{"dei"}), std::move(z), 5);
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"df"});
      VERIFY(it->second == Y{5});
      VERIFY(a.at(X{"dec"}) == Y{"dec"});  // unchanged
      VERIFY(a.at(X{"ded"}) == Y{"ded"});  // unchanged
      VERIFY(a.at(X{"dee"}) == Y{"dee"});  // unchanged
      VERIFY(a.at(X{"def"}) == Y{"def"});  // unchanged
      VERIFY(a.at(X{"deg"}) == Y{"deg"});  // unchanged
      VERIFY(a.at(X{"deh"}) == Y{"deh"});  // unchanged
      VERIFY(a.at(X{"dei"}) == Y{"dei"});  // unchanged
      VERIFY(z.s.empty());  // moved from
      VERIFY(z.compares == 3);
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"df"};
      auto it = a.try_emplace(a.find(X{"ghi"}), std::move(z), 5);
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"df"});
      VERIFY(it->second == Y{5});
      VERIFY(a.at(X{"dec"}) == Y{"dec"});  // unchanged
      VERIFY(a.at(X{"ded"}) == Y{"ded"});  // unchanged
      VERIFY(a.at(X{"dee"}) == Y{"dee"});  // unchanged
      VERIFY(a.at(X{"def"}) == Y{"def"});  // unchanged
      VERIFY(a.at(X{"deg"}) == Y{"deg"});  // unchanged
      VERIFY(a.at(X{"deh"}) == Y{"deh"});  // unchanged
      VERIFY(a.at(X{"dei"}) == Y{"dei"});  // unchanged
      VERIFY(z.s.empty());  // moved from
      VERIFY(z.compares == 2);
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"jkl"};
      auto it = a.try_emplace(a.find(X{"ghi"}), std::move(z), 5);
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"jkl"});
      VERIFY(it->second == Y{5});
      VERIFY(a.at(X{"dec"}) == Y{"dec"});  // unchanged
      VERIFY(a.at(X{"ded"}) == Y{"ded"});  // unchanged
      VERIFY(a.at(X{"dee"}) == Y{"dee"});  // unchanged
      VERIFY(a.at(X{"def"}) == Y{"def"});  // unchanged
      VERIFY(a.at(X{"deg"}) == Y{"deg"});  // unchanged
      VERIFY(a.at(X{"deh"}) == Y{"deh"});  // unchanged
      VERIFY(a.at(X{"dei"}) == Y{"dei"});  // unchanged
      VERIFY(z.s.empty());  // moved from
      VERIFY(z.compares == 2);
    }
    {
      auto a = populate(amap);
      VERIFY(a.size() == 9);
      Z z{"jkl"};
      auto it = a.try_emplace(a.end(), std::move(z), 5);
      VERIFY(a.size() == 10);
      VERIFY(it->first == X{"jkl"});
      VERIFY(it->second == Y{5});
      VERIFY(a.at(X{"dec"}) == Y{"dec"});  // unchanged
      VERIFY(a.at(X{"ded"}) == Y{"ded"});  // unchanged
      VERIFY(a.at(X{"dee"}) == Y{"dee"});  // unchanged
      VERIFY(a.at(X{"def"}) == Y{"def"});  // unchanged
      VERIFY(a.at(X{"deg"}) == Y{"deg"});  // unchanged
      VERIFY(a.at(X{"deh"}) == Y{"deh"});  // unchanged
      VERIFY(a.at(X{"dei"}) == Y{"dei"});  // unchanged
      VERIFY(z.s.empty());  // moved from
      VERIFY(z.compares == 1);
    }
  }
}

int main()
{
  test_at();
  test_op_bracket();
  test_try_emplace();
  test_insert_or_assign();
  test_op_bracket_prefix();
  test_try_emplace_prefix();
  test_insert_or_assign_prefix();
}
