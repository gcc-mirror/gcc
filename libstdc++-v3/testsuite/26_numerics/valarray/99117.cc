// { dg-do run { target c++11 } }

// PR libstdc++/99117 cannot accumulate std::valarray

#include <valarray>
#include <vector>
#include <testsuite_hooks.h>

int main()
{
    std::vector<std::valarray<int>> v = {{1,1}, {2,2}};
    std::valarray<int> sum(2);
    for (const auto& e : v)
      sum = sum + e;
    VERIFY(sum[0]==3);
    VERIFY(sum[1]==3);
}
