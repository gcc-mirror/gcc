// { dg-do compile { target c++11 } }

#include <unordered_map>

#include <testsuite_hooks.h>

// PR c++/116369
const std::unordered_multimap<int, int> umm
  {
    { 0, 1 },
    { 0, 1 },
    { 2, 3 },
    { 2, 3 },
    { 4, 5 },
    { 4, 5 }
  };

int main()
{
  VERIFY( umm.size() == 6 );
  VERIFY( umm.find(0) != umm.end() );
}
