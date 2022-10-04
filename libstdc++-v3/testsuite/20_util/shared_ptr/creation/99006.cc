// { dg-do compile { target { c++11 } } }
// { dg-require-effective-target hosted }

#include <memory>

auto p = std::make_shared<int[]>(2); // { dg-error "here" "" { target c++17_down } }
auto q = std::make_shared<int[2]>(1, 2); // { dg-error "here" }

// { dg-prune-output "enable_if<false" }
// { dg-prune-output "template constraint failure" }
// { dg-prune-output "no matching function" }
