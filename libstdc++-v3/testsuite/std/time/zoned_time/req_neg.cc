// { dg-do compile { target c++20 } }

#include <chrono>

struct TZ; // custom zone so this works even for freestanding and old ABI

std::chrono::zoned_time<std::chrono::year, const TZ*> z; // { dg-error "here" }
// { dg-error "static assertion failed" "" { target *-*-* } 0 }
// { dg-prune-output "common_type" }
