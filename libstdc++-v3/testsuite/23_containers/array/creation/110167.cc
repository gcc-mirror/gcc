// { dg-do compile { target c++20 } }

// PR libstdc++/110167 - excessive compile time when optimizing std::to_array

#include <array>

constexpr int N = 512 * 512;

std::array<int, N>
make_std_array(int (&a)[N])
{
  return std::to_array(a);
}
