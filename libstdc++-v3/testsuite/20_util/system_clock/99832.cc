// { dg-options "-O0 -g0" }
// { dg-do compile { target c++20 } }
// { dg-final { scan-assembler-not "system_clock9to_time_t" } }

// Bug libstdc++/99832
// std::chrono::system_clock::to_time_t needs ABI tag for 32-bit time_t

#include <chrono>

std::time_t
test_pr99832(std::chrono::system_clock::time_point t)
{
  return std::chrono::system_clock::to_time_t(t);
}
