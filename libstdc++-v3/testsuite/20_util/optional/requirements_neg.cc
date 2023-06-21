// { dg-do compile { target c++17 } }

#include <optional>

// T shall be a type other than cv in_place_t or cv nullopt_t
// that meets the Cpp17Destructible requirements

std::optional<std::nullopt_t> o1;        // { dg-error "here" }
std::optional<const std::nullopt_t> o2;  // { dg-error "here" }
std::optional<std::in_place_t> o3;       // { dg-error "here" }
std::optional<const std::in_place_t> o4; // { dg-error "here" }
std::optional<int&> o5;                  // { dg-error "here" }
std::optional<int[1]> o6;                // { dg-error "here" }
std::optional<int[]> o7;                 // { dg-error "here" }
std::optional<int()> o8;                 // { dg-error "here" }

// { dg-error "static assertion failed" "" { target *-*-* } 0 }

// { dg-prune-output "forming pointer to reference type" }
// { dg-prune-output "union may not have reference type" }
// { dg-prune-output "function returning an array" }
// { dg-prune-output "flexible array member .* in union" }
// { dg-prune-output "function returning a function" }
// { dg-prune-output "invalidly declared function type" }
