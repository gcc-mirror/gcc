// { dg-do run { target c++23 } }

// LWG 4366. Heterogeneous comparison of expected may be ill-formed

#include <expected>
#include <testsuite_hooks.h>

struct Bool
{
  operator bool() const { return true; }
  explicit operator bool() { throw; }
};

struct E1 {
  friend Bool operator==(E1, E1) { return {}; }
} e1;

struct E2 {
  friend Bool operator==(E1, E2) { return {}; }
} e2;

int main()
{
  std::expected<int, E1> u1(std::unexpect, e1);
  VERIFY(u1 == u1);

  std::unexpected<E2> u2(e2);
  VERIFY(u1 == u2);

  std::expected<void, E1> u3(std::unexpect, e1);
  VERIFY(u3 == u3);
  VERIFY(u3 == u2);
}
