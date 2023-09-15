// { dg-do compile { target c++20 } }
// { dg-require-effective-target hosted }

#include <memory>

struct move_only
{
  move_only(move_only&&);
};

using P = std::pair<move_only, move_only>;

void
test_lwg3527(std::pair<move_only&&, move_only&&> rvals)
{
  // LWG 3527. uses_allocator_construction_args handles rvalue pairs of
  // rvalue references incorrectly.
  // PR libstdc++/108952 Regression in uses_allocator_construction_args
  // for pair of rvalue references
  std::allocator<move_only> a;
  (void) std::uses_allocator_construction_args<P>(a, std::move(rvals));
}
