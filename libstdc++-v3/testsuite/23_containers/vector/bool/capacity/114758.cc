// { dg-options "-O3 -Werror=stringop-overread -fno-assume-sane-operators-new-delete" }
// { dg-do compile }

// Bug libstdc++/114758 The layout of a std::vector<bool> reports a warning

#include <vector>

void pr114758(std::vector<bool>& v)
{
  v.resize(3);
  v = std::vector<bool>(3, false);
}
