// { dg-do compile { target c++11 } }

#include <vector>
#include <testsuite_allocator.h>

__gnu_test::ExplicitConsAlloc<bool> alloc;
std::vector<bool, __gnu_test::ExplicitConsAlloc<bool>> v;
std::vector<bool, __gnu_test::ExplicitConsAlloc<bool>> v1(alloc);
std::vector<bool, __gnu_test::ExplicitConsAlloc<bool>> v2(v1, alloc);
std::vector<bool, __gnu_test::ExplicitConsAlloc<bool>> v3(std::move(v1), alloc);
