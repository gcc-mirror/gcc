// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }

#include <experimental/filesystem>

void
test01()
{
  using std::experimental::filesystem::path;
  path p;
  path::string_type s(p);
}
