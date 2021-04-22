// FIXME: This should use { target { ! c++20 } }
// { dg-do compile }

#include <memory>

auto p = std::make_shared<int[]>(2); // { dg-error "here" }
auto q = std::make_shared<int[2]>(1, 2); // { dg-error "here" }

// { dg-prune-output "static assertion failed" }
