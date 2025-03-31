// { dg-options "-O2 -Werror=stringop-overflow -Werror=array-bounds" }
// { dg-do compile { target c++11 } }

// Bug libstdc++/114945
// Sporadic std::vector::resize() -Wstringop-overflow or -Warray-bounds warning

#include <stdint.h>
#include <vector>
template <typename a> struct b {
  void resize(std::size_t c) { d.resize(c); }
  template <typename e> void f(a, e);
  std::vector<char> d;
};
#include <regex>
std::regex g;
uint64_t h;
uint32_t i;
struct s {
  enum class j : size_t;
  void k();
  using l = b<j>;
  std::vector<l> m;
};
enum class s::j : size_t { n };
void o() { g = ""; }
void s::k() {
  l p;
  auto q = uint32_t(), r = uint32_t();
  if (h)
    r = i;
  b<size_t> t;
  if (q || r)
    p.f(j::n, 5);
  t.resize(4);
  m.push_back(p);
}
