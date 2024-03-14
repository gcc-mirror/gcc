// { dg-do run { target c++11 } }
// { dg-require-debug-mode "" }

// PR libstdc++/114316

#include <set>
#include <algorithm>

#include <testsuite_hooks.h>

int main()
{
  std::set<int>::iterator it{};
  VERIFY( std::find(it, it, 0) == it );
  return 0;
}
