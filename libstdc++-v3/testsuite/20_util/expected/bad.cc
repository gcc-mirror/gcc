// { dg-do compile { target c++23 } }

#include <expected>

struct E {
  E() = default;
  E(E&&) = default;
};

void
test_pr105146()
{
  std::bad_expected_access(E{});
}
