// { dg-do run { target c++26 } }

#include <unordered_map>
#include <string>
#include <string_view>
#include <utility>
#include <functional>
#include <compare>
#include <exception>
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
  mutable bool copied_from = false;  // [PR126800]
  Y() = default;
  Y(Y&& y) : s(std::move(y.s)) { y.s.clear(); }
  Y(const Y& y) : s(y.s) { y.copied_from = true; }
  Y& operator=(Y&& y) { s = std::move(y.s); y.s.clear(); return *this; }
  Y& operator=(const Y& y) { s = y.s; y.copied_from = true; return *this; }
  Y(std::string_view sv) : s(sv) {}
  Y(int n) : s(std::string('a', n)) {}
  Y(const Y& a, const Y& b) : s(a.s + "1" + b.s) { }
  Y(const Y& a, Y&& b)      : s(a.s + "2" + b.s) { b.s.clear(); }
  Y(Y&& a, const Y& b)      : s(a.s + "3" + b.s) { a.s.clear(); }
  Y(Y&& a, Y&& b)           : s(a.s + "4" + b.s) { a.s.clear(), b.s.clear(); }
  friend bool operator==(Y const& a, Y const& b) = default;
  friend bool operator==(X const& a, Y const& b) { return a.s == b.s; }
};

X::X(Y&& y) : s(std::move(y.s)) { y.s.clear(); }
X::X(const Y& y) : s(y.s) { y.copied_from = true; }

struct Hash {
  using is_transparent = void;
  template <typename T>
    auto operator()(T const& t) const
    { return std::hash<decltype(T::s)>{}(t.s); }
};

using Equal = std::equal_to<void>;

void test_op_bracket()
{
  std::unordered_map<X, Y, Hash, Equal> amap;
  amap.insert({{X{"abc"}, 1}, {X{"def"}, 2}, {X{"ghi"}, 3}});

  Y z{"deg"};
  amap[z] = Y{4};
  VERIFY(amap.size() == 4);
  VERIFY(z.s.size() == 3);  // not moved from.
  VERIFY(z.copied_from);
  VERIFY(amap[Y{"deg"}] == Y{4});

  Y zz{"deg"};
  amap[std::move(zz)] = Y{5};
  VERIFY(amap.size() == 4);
  VERIFY(zz.s.size() == 3);  // not moved from.
  VERIFY(!zz.copied_from);
  VERIFY(amap[Y{"deg"}] == Y{5});

  Y y{"deh"};
  amap[y] = Y{6};
  VERIFY(amap.size() == 5);
  VERIFY(y.s.size() == 3);      // not moved from.
  VERIFY(y.copied_from);
  VERIFY(amap[Y{"deh"}] == Y{6});

  Y x{"dei"};
  amap[std::move(x)] = Y{7};
  VERIFY(amap.size() == 6);
  VERIFY(x.s.empty());      // moved from
  VERIFY(!x.copied_from);
  VERIFY(amap[Y{"dei"}] == Y{7});
}

void test_at()
{
  std::unordered_map<X, Y, Hash, Equal> amap;
  amap.insert({{X{"abc"}, 1}, {X{"def"}, 2}, {X{"ghi"}, 3}});

  Y x{"def"};
  try
    {
      VERIFY(2 == amap.at(x));
      VERIFY(amap.size() == 3);
      VERIFY(x.s.size() == 3);   // not moved from
      VERIFY(!x.copied_from);
      VERIFY(4 == (amap.at(x) = 4));
      VERIFY(amap.size() == 3);
      VERIFY(x.s.size() == 3);   // not moved from
      VERIFY(!x.copied_from);
    }
  catch(...) { VERIFY(false); }

  Y z{"deg"};
  try
  {
    amap.at(z) = Y{1};
    VERIFY(false);  // Should have thrown.
  }
  catch (std::out_of_range&) { VERIFY(amap.size() == 3); }
  catch (...) { VERIFY(false); } // Wrong exception.
  VERIFY(z.s.size() == 3);   // not moved from
  VERIFY(!z.copied_from);

  Y y{"deh"};
  auto const& amapr{amap};
  try
    {
      amapr.at(y);
      VERIFY(false);  // Should have thrown.
    }
  catch (std::out_of_range&) {  }
  catch (...) { VERIFY(false); } // Wrong exception.
  VERIFY(amapr.size() == 3);
  VERIFY(y.s.size() == 3);  // not moved from
  VERIFY(!y.copied_from);
}

void test_try_emplace()
{
  std::unordered_map<X, Y, Hash, Equal> amap;
  amap.insert({{X{"abc"}, 1}, {X{"def"}, 2}, {X{"ghi"}, 3}});

  { // Fail, already there
    auto a = amap;
    Y y{"def"}, z{"xyz"};
    auto [it, res] = a.try_emplace(std::move(y), z);
    VERIFY(!res);
    VERIFY(a.size() == 3);
    VERIFY(a.at(Y{"def"}) == Y{2});
    VERIFY(y.s.size() == 3); // not moved from
    VERIFY(!y.copied_from);
    VERIFY(z.s.size() == 3); // not moved from
    VERIFY(!z.copied_from);
  }
  { // Fail, already there, move
    auto a = amap;
    Y y{"def"}, z{"xyz"};
    auto [it, res] = a.try_emplace(std::move(y), std::move(z));
    VERIFY(!res);
    VERIFY(a.size() == 3);
    VERIFY(a.at(Y{"def"}) == Y{2});
    VERIFY(y.s.size() == 3);  // not moved from
    VERIFY(!y.copied_from);
    VERIFY(z.s.size() == 3);  // not moved from
    VERIFY(!z.copied_from);
  }
  { // Succeed, construct
    auto a = amap;
    Y m("m"), n("n"), o("o"), p("p");
    {
      Y y{"deg"};
      auto [it, res] = a.try_emplace(std::move(y), m, n);
      VERIFY(res);
      VERIFY(a.size() == 4);
      VERIFY(it->first == X{"deg"});
      VERIFY(it->second == Y{"m1n"});
      VERIFY(m.s.size() == 1);
      VERIFY(n.s.size() == 1);
    }
    {
      Y y{"deh"};
      auto [it, res] = a.try_emplace(std::move(y), m, std::move(n));
      VERIFY(res);
      VERIFY(a.size() == 5);
      VERIFY(it->first == X{"deh"});
      VERIFY(it->second == Y{"m2n"});
      VERIFY(m.s.size() == 1);
      VERIFY(n.s.empty());
      VERIFY(!y.copied_from);
    }
    {
      Y y{"dei"};
      auto [it, res] = a.try_emplace(std::move(y), std::move(m), o);
      VERIFY(res);
      VERIFY(a.size() == 6);
      VERIFY(it->first == X{"dei"});
      VERIFY(it->second == Y{"m3o"});
      VERIFY(m.s.empty());
      VERIFY(o.s.size() == 1);
      VERIFY(!y.copied_from);
    }
    {
      Y y{"dej"};
      auto [it, res] = a.try_emplace(std::move(y), std::move(o), std::move(p));
      VERIFY(res);
      VERIFY(a.size() == 7);
      VERIFY(it->first == X{"dej"});
      VERIFY(it->second == Y{"o4p"});
      VERIFY(o.s.empty());
      VERIFY(p.s.empty());
      VERIFY(!y.copied_from);
    }
    {
      Y y{"dek"};
      auto [it, res] = a.try_emplace(std::move(y), Y("q"), Y("r"));
      VERIFY(res);
      VERIFY(a.size() == 8);
      VERIFY(y.s.empty());
      VERIFY(it->first == X{"dek"});
      VERIFY(it->second == Y{"q4r"});
      VERIFY(!y.copied_from);
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
    VERIFY(!y.copied_from);
    VERIFY(!z.copied_from);
  }
  { // Hinted, fail
    auto a = amap;
    Y y{"def"}, z{"xyz"};
    auto it = a.try_emplace(a.begin(), std::move(y), z);
    VERIFY(a.size() == 3);
    VERIFY(it->first == X{"def"});
    VERIFY(it->second == Y{2});
    VERIFY(y.s.size() == 3);  // not moved from
    VERIFY(z.s.size() == 3);  // not moved from
    VERIFY(!y.copied_from);
    VERIFY(!z.copied_from);
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
    VERIFY(!y.copied_from);
    VERIFY(!z.copied_from);
  }
  { // Hinted, succeed, construct
    auto a = amap;
    Y m("m"), n("n"), o("o"), p("p");
    {
      Y y{"deg"};
      auto it = a.try_emplace(a.begin(), y, m, n);
      VERIFY(a.size() == 4);
      VERIFY(it->first == X{"deg"});
      VERIFY(it->second == Y{"m1n"});
      VERIFY(m.s.size() == 1);
      VERIFY(n.s.size() == 1);
      VERIFY(y.s.size() == 3);
      VERIFY(y.copied_from);
    }
    {
      Y y{"deh"};
      auto it = a.try_emplace(a.begin(), y, m, std::move(n));
      VERIFY(a.size() == 5);
      VERIFY(it->first == X{"deh"});
      VERIFY(it->second == Y{"m2n"});
      VERIFY(m.s.size() == 1);
      VERIFY(n.s.empty());
      VERIFY(y.s.size() == 3);
      VERIFY(y.copied_from);
    }
    {
      Y y{"dei"};
      auto it = a.try_emplace(a.begin(), y, std::move(m), o);
      VERIFY(a.size() == 6);
      VERIFY(it->first == X{"dei"});
      VERIFY(it->second == Y{"m3o"});
      VERIFY(m.s.empty());
      VERIFY(o.s.size() == 1);
      VERIFY(y.s.size() == 3);
      VERIFY(y.copied_from);
    }
    {
      Y y{"dej"};
      auto it = a.try_emplace(a.begin(), y, std::move(o), std::move(p));
      VERIFY(a.size() == 7);
      VERIFY(it->first == X{"dej"});
      VERIFY(it->second == Y{"o4p"});
      VERIFY(o.s.empty());
      VERIFY(p.s.empty());
      VERIFY(y.s.size() == 3);
      VERIFY(y.copied_from);
    }
    {
      Y y{"dek"};
      auto it = a.try_emplace(a.begin(), std::move(y), Y("q"), Y("r"));
      VERIFY(a.size() == 8);
      VERIFY(y.s.empty());
      VERIFY(it->first == X{"dek"});
      VERIFY(it->second == Y{"q4r"});
      VERIFY(it->second == Y{"q4r"});
      VERIFY(y.s.empty()); // moved from
      VERIFY(!y.copied_from);
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
    VERIFY(!y.copied_from);
    VERIFY(z.s.empty());  // moved from
    VERIFY(!z.copied_from);
  }
}

void test_insert_or_assign()
{
  std::unordered_map<X, Y, Hash, Equal> amap;
  amap.insert({{X{"abc"}, 1}, {X{"def"}, 2}, {X{"ghi"}, 3}});

  { // Already there, replace
    auto a = amap;
    Y y{"def"}, z{"xyz"};
    auto [it, res] = a.insert_or_assign(y, z);
    VERIFY(!res);
    VERIFY(a.size() == 3);
    VERIFY(a.at(Y{"def"}) == Y{"xyz"});
    VERIFY(y.s.size() == 3);  // not moved from
    VERIFY(!y.copied_from);
    VERIFY(z.s.size() == 3);  // not moved from
    VERIFY(z.copied_from);
  }
  { // Already there, move
    auto a = amap;
    Y y{"def"}, z{"xyz"};
    auto [it, res] = a.insert_or_assign(std::move(y), std::move(z));
    VERIFY(!res);
    VERIFY(a.size() == 3);
    VERIFY(a.at(Y{"def"}) == Y{"xyz"});
    VERIFY(y.s.size() == 3);  // not moved from
    VERIFY(!y.copied_from);
    VERIFY(z.s.empty());      // moved from
    VERIFY(!z.copied_from);
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
    VERIFY(!y.copied_from);
    VERIFY(z.s.empty()); // moved from
    VERIFY(!z.copied_from);
  }
  { // Hinted, already there, replace
    auto a = amap;
    Y y{"def"}, z{"xyz"};
    auto it = a.insert_or_assign(a.begin(), y, z);
    VERIFY(a.size() == 3);
    VERIFY(it->first == X{"def"});
    VERIFY(it->second == Y{"xyz"});
    VERIFY(y.s.size() == 3);  // not moved from
    VERIFY(!y.copied_from);
    VERIFY(z.s.size() == 3);  // not moved from
    VERIFY(z.copied_from);
  }
  { // Hinted, already there, move
    auto a = amap;
    Y y{"def"}, z{"xyz"};
    auto it = a.insert_or_assign(a.begin(), std::move(y), std::move(z));
    VERIFY(a.size() == 3);
    VERIFY(it->first == X{"def"});
    VERIFY(it->second == Y{"xyz"});
    VERIFY(y.s.size() == 3);  // not moved from
    VERIFY(!y.copied_from);
    VERIFY(z.s.empty());      // moved from
    VERIFY(!z.copied_from);
  }
  {  // Hinted, succeed
    auto a = amap;
    Y y{"tuv"}, z{"xyz"};
    auto it = a.insert_or_assign(a.begin(), y, z);
    VERIFY(it->first == X{"tuv"});
    VERIFY(it->second == Y{"xyz"});
    VERIFY(a.size() == 4);
    VERIFY(y.s.size() == 3);  // not moved from
    VERIFY(y.copied_from);
    VERIFY(z.s.size() == 3);  // not moved from
    VERIFY(z.copied_from);
  }
  {  // Hinted, succeed, move
    auto a = amap;
    Y y{"tuv"}, z{"xyz"};
    auto it = a.insert_or_assign(a.begin(), std::move(y), std::move(z));
    VERIFY(it->first == X{"tuv"});
    VERIFY(it->second == Y{"xyz"});
    VERIFY(a.size() == 4);
    VERIFY(y.s.empty());  // moved from
    VERIFY(!y.copied_from);
    VERIFY(z.s.empty());  // moved from
    VERIFY(!z.copied_from);
  }
}

int main()
{
  test_op_bracket();
  test_at();
  test_try_emplace();
  test_insert_or_assign();
}
