// { dg-options "-O2" }
// { dg-do compile { target c++11 } }

// Bug 110807
// Copy list initialisation of a vector<bool> raises a warning with -O2

#include <vector>

std::vector<bool> byCallSpread;

void f()
{
  byCallSpread = {true};
}
