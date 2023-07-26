// { dg-options "-O2" }
// { dg-do compile }

// Bug 110807
// Copy list initialisation of a vector<bool> raises a warning with -O2

#include <vector>

std::vector<bool> byCallSpread;

void f()
{
  byCallSpread = {true};
}
