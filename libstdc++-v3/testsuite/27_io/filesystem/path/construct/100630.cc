// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <filesystem>

void f(bool) { }
void f(const std::filesystem::path&) { }

void
test_100630()
{
  volatile bool b = true;
  f(b);
}
