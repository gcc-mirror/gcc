// { dg-do compile { target c++11 } }

#include <unordered_map>

#include <testsuite_hooks.h>

// PR c++/116369
const std::unordered_map<int, int> um
  {
    { 0, 1 },
    { 2, 3 },
    { 4, 5 }
  };

int main()
{
  VERIFY( um.size() == 3 );
  VERIFY( um.find(0) != um.end() );
}
