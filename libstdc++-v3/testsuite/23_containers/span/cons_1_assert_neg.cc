// { dg-do run { target c++20 xfail *-*-* } }
// { dg-add-options no_pch }

#undef _GLIBCXX_DEBUG
#define _GLIBCXX_DEBUG
#include <span>
#include <vector>

int main()
{
  std::vector<int> v(2);
  std::span<int, std::dynamic_extent> s(v.begin(), 3);
}
