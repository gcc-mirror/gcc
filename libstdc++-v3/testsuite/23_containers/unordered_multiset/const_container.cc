// { dg-do compile { target c++11 } }

#include <unordered_set>

#include <testsuite_hooks.h>

// PR c++/116369
const std::unordered_multiset<int> ums
  { 0, 0, 1, 1, 2, 2 };

int main()
{
  VERIFY( ums.size() == 6 );
  VERIFY( ums.find(0) != ums.end() );
}
