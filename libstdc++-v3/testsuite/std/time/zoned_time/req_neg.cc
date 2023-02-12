// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }
// { dg-require-effective-target cxx11_abi }

#include <chrono>

std::chrono::zoned_time<std::chrono::year> z; // { dg-error "here" }
// { dg-error "static assertion failed" "" { target *-*-* } 0 }
// { dg-prune-output "common_type" }
