// { dg-do compile { target c++11 } }

#include <vector>

struct S
{
  explicit operator bool() &&;
};

void
test_emplace_back()
{
  S s;
  std::vector<bool> v;
  v.emplace_back(std::move(s));
}

void
test_emplace()
{
  S s;
  std::vector<bool> v;
  v.emplace(v.begin(), std::move(s));
}
