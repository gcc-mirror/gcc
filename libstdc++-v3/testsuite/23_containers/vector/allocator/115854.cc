// { dg-do compile { target c++11 } }

#include <vector>
#include <testsuite_allocator.h>

__gnu_test::ExplicitConsAlloc<int> alloc;
std::vector<int, __gnu_test::ExplicitConsAlloc<int>> v;
std::vector<int, __gnu_test::ExplicitConsAlloc<int>> v1(alloc);
std::vector<int, __gnu_test::ExplicitConsAlloc<int>> v2(v1, alloc);
std::vector<int, __gnu_test::ExplicitConsAlloc<int>> v3(std::move(v1), alloc);
