// { dg-do compile { target c++23 } }
#include<mdspan>

#include <cstdint>

std::extents<uint8_t, size_t(1) << 9> e1; // { dg-error "from here" }
std::extents<char, 1> e2;                 // { dg-error "from here" }
std::extents<bool, 1> e3;                 // { dg-error "from here" }
std::extents<double, 1> e4;               // { dg-error "from here" }

// { dg-prune-output "dynamic or representable as IndexType" }
// { dg-prune-output "signed or unsigned integer" }
// { dg-prune-output "invalid use of incomplete type" }
// { dg-prune-output "non-constant condition for static assertion" }
// { dg-prune-output "integer constants in boolean context" }
// { dg-prune-output "__gnu_cxx::__numeric_traits_integer" }
// { dg-prune-output "static assertion failed" }
