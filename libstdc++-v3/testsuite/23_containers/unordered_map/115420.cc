// { dg-do compile { target c++11 } }

#include <unordered_map>

struct S { };

void
test_pr115420()
{
  std::unordered_map<S, int> m; // { dg-error "here" }
}

// { dg-error "hash function must be copy constructible" "" { target *-*-* } 0 }
// { dg-prune-output "use of deleted function" }
// { dg-prune-output "is private" }
// { dg-prune-output "no matching function" }
