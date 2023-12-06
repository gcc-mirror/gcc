// { dg-do compile { target c++20 } }

#include <vector>
#include <utility>
#include <testsuite_hooks.h>

constexpr bool
test_std_swap()
{
  std::vector<int> v1 = {1, 2, 3};
  std::vector<int> v2 = {0, 1};

  std::swap(v1, v2);

  VERIFY(v1.size() == 2);
  VERIFY(v1.at(0) == 0);
  VERIFY(v1.at(1) == 1);

  VERIFY(v2.size() == 3);
  VERIFY(v2[0] == 1);
  VERIFY(v2[1] == 2);
  VERIFY(v2[2] == 3);

  return true;
}

static_assert(test_std_swap());

constexpr bool
test_member_swap()
{
  std::vector<int> v1 = {1, 2, 3};
  std::vector<int> v2 = {0, 1};

  v1.swap(v2);

  VERIFY(v1.size() == 2);
  VERIFY(v1.at(0) == 0);
  VERIFY(v1.at(1) == 1);

  VERIFY(v2.size() == 3);
  VERIFY(v2[0] == 1);
  VERIFY(v2[1] == 2);
  VERIFY(v2[2] == 3);

  return true;
}

static_assert(test_member_swap());
