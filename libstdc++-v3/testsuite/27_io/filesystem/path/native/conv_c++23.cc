// { dg-do compile { target c++23 } }

#include <filesystem>

void
test01()
{
  using std::filesystem::path;
  path p;
  path::string_type s(p);
}
