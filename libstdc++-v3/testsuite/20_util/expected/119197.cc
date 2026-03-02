// { dg-do compile { target c++23 } }

// PR libstdc++/119197 - std::expected should be nodiscard

#include <expected>

std::expected<int, int> f();
std::expected<void, int> g();

void
test_nodiscard()
{
  f();                         // { dg-warning "ignoring return" }
  std::expected<int, int>(42); // PR c++/85973: should warn "ignoring temporary"
  (void)f();                   // OK

  g();                         // { dg-warning "ignoring return" }
  std::expected<void, int>();  // PR c++/85973: should warn "ignoring temporary"
  (void)g();                   // OK
}
