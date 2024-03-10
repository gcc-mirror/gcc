// { dg-do compile }
// { dg-options "-O3 -g0" }
// { dg-require-normal-mode "" }
// { dg-final { scan-assembler-not "_Znw" } }
// GCC should be able to optimize away the paths involving reallocation.

#include <vector>

void fill(std::vector<int>& vec)
{
  vec.assign(vec.size(), 0);
}

void fill_val(std::vector<int>& vec, int i)
{
  vec.assign(vec.size(), i);
}

void fill_range(std::vector<int>& vec, const int* first)
{
  vec.assign(first, first + vec.size());
}
