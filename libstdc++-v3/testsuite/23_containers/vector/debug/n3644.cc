// { dg-do run { target c++11 } }
// { dg-require-debug-mode "" }

#include <vector>
#include <algorithm>

#include <testsuite_hooks.h>

int main()
{
  std::vector<int>::iterator it{};
  auto cpy = it;
  std::advance(it, 0);
  VERIFY( it == cpy );
  return 0;
}
