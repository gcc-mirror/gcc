#include <iterator>
#include <algorithm>
#include <vector>
#ifndef _GLIBCXX_DEBUG
#include <debug/vector>
#endif

struct S { };

int main()
{
  S s[1];
  std::vector<S> v(1);
  std::copy(s, s, v.rbegin());
#if __cplusplus >= 201103L
  std::copy(s, s, std::make_move_iterator(v.begin()));
  std::copy(s, s, std::make_move_iterator(v.rbegin()));
#endif

#ifndef _GLIBCXX_DEBUG
  __gnu_debug::vector<S> dv(1);
  std::copy(s, s, dv.rbegin());
#if __cplusplus >= 201103L
  std::copy(s, s, std::make_move_iterator(dv.begin()));
  std::copy(s, s, std::make_move_iterator(dv.rbegin()));
#endif
#endif
}

