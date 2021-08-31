// { dg-options "-std=gnu++2a" }
// { dg-do run { xfail *-*-* } }
// { dg-require-effective-target c++2a }

#undef _GLIBCXX_DEBUG
#define _GLIBCXX_DEBUG
#include <span>
#include <vector>

int main()
{
  std::vector<int> v(2), w(1);
  std::span<int, std::dynamic_extent> s(v.begin(), w.end());
}
