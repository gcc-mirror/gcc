// { dg-do compile { target c++11 } }

#include <unordered_set>

#include <testsuite_hooks.h>

// PR c++/116369
const std::unordered_set<int> us { 0, 1, 2 };

int main()
{
  VERIFY( us.size() == 3 );
  VERIFY( us.find(0) != us.end() );
}
