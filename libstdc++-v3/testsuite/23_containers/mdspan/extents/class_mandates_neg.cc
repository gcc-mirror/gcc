// { dg-do compile { target c++23 } }
#include<mdspan>

std::extents<char, size_t(1) << 9> e1; // { dg-error "from here" }
std::extents<double, 1> e2;            // { dg-error "from here" }
// { dg-prune-output "dynamic or representable as _IndexType" }
// { dg-prune-output "must be integral" }
// { dg-prune-output "invalid use of incomplete type" }
