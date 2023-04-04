// { dg-do compile { target c++11 } }
// { dg-require-effective-target hosted }

#include <scoped_allocator>

struct move_only
{
  move_only(move_only&&);
};

using P = std::pair<move_only, move_only>;

void
test_pr108952(std::pair<move_only&&, move_only&&> rvals)
{
  // LWG 3527. uses_allocator_construction_args handles rvalue pairs of
  // rvalue references incorrectly.
  // PR libstdc++/108952 Regression in uses_allocator_construction_args
  // for pair of rvalue references
  std::scoped_allocator_adaptor<std::allocator<P>> a;
  auto p = a.allocate(1);
  a.construct(p, std::move(rvals));
  a.deallocate(p, 1);
}
