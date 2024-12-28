// { dg-do compile { target c++23 } }
#include <generator>

std::generator<int>
generator();

void
try_reassigning()
{
  auto gen = generator();
  auto gen2 = generator();
  gen = std::move(gen2);
}
