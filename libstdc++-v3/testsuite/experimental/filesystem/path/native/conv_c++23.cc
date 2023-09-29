// { dg-do compile { target c++23 } }
// { dg-require-filesystem-ts "" }

#include <experimental/filesystem>

void
test01()
{
  using std::experimental::filesystem::path;
  path p;
  path::string_type s(p);
}
