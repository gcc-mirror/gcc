// { dg-options "-O0" }
// { dg-do compile { target c++17 } }
// { dg-final { scan-assembler "memcmp" } }

#include <algorithm>
#include <cstddef>

bool eq(std::byte const* p, std::byte const* q, unsigned n)
{
  return std::equal(p, p + n, q);
}
