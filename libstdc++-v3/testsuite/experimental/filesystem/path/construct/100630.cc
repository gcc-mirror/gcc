// { dg-do compile { target c++11 } }
// { dg-require-filesystem-ts "" }

#include <experimental/filesystem>

void f(bool) { }
void f(const std::experimental::filesystem::path&) { }

void
test_100630()
{
  volatile bool b = true;
  f(b);
}
