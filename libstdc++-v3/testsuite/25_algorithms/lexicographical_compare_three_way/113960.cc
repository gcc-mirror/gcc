// { dg-do run { target c++20 } }

// PR libstdc++/113960
// std::map with std::vector as input overwrites itself with c++20, on s390x

#include <algorithm>
#include <testsuite_hooks.h>

int main()
{
  unsigned short a1[] { 1, 2, 3 };
  unsigned short a2[] { 1, 2, 4 };
  // Incorrect memcmp comparison for big endian targets.
  VERIFY( std::lexicographical_compare_three_way(a1, a1+3, a2, a2+3) < 0 );
}
