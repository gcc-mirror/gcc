// { dg-options "-Wdeprecated" }
// { dg-do compile }
// { dg-require-normal-mode "" }

#include <vector>

void
test01()
{
  std::vector<bool> v;
  v.insert(v.begin(), false);
  v.insert(v.begin());  // { dg-warning "deprecated" }
}
