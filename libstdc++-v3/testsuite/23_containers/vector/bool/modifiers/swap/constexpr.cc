// { dg-do compile { target c++20 } }

#include <vector>
#include <utility>
#include <testsuite_hooks.h>

constexpr bool
test_std_swap()
{
  std::vector<bool> v1 = {true, false, true};
  std::vector<bool> v2 = {false, true};

  std::swap(v1, v2);

  VERIFY(v1.size() == 2);
  VERIFY(v1.at(0) == false);
  VERIFY(v1.at(1) == true);

  VERIFY(v2.size() == 3);
  VERIFY(v2[0]);
  VERIFY(!v2[1]);
  VERIFY(v2[2]);

  return true;
}

static_assert(test_std_swap());

constexpr bool
test_member_swap()
{
  std::vector<bool> v1 = {true, false, true};
  std::vector<bool> v2 = {false, true};

  v1.swap(v2);

  VERIFY(v1.size() == 2);
  VERIFY(v1.at(0) == false);
  VERIFY(v1.at(1) == true);

  VERIFY(v2.size() == 3);
  VERIFY(v2[0]);
  VERIFY(!v2[1]);
  VERIFY(v2[2]);

  return true;
}

static_assert(test_member_swap());

#ifndef _GLIBCXX_DEBUG
constexpr bool
test_reference_swap()
{
  std::vector<bool> v1 = {true, false, true};
  std::vector<bool>::swap(v1[0], v1[1]);

  VERIFY(v1[0] == false);
  VERIFY(v1[1] == true);
  VERIFY(v1[2] == true);

  return true;
}

static_assert(test_reference_swap());
#endif
