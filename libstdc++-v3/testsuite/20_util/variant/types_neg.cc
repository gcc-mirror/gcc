// { dg-do compile { target c++17 } }

# include <variant>

std::variant<> v0; // { dg-error "here" }
// { dg-error "must have at least one alternative" "" { target *-*-* } 0 }
std::variant<int, void> v1; // { dg-error "here" }
std::variant<int, const void> v2; // { dg-error "here" }
std::variant<int, int&> v3; // { dg-error "here" }
std::variant<int, void()> v4; // { dg-error "here" }
std::variant<int, int[]> v5; // { dg-error "here" }
std::variant<int, int[1]> v6; // { dg-error "here" }
// { dg-error "must be non-array object types" "" { target *-*-* } 0 }

// All of variant's base classes are instantiated before checking any
// static_assert, so we get lots of errors before the expected errors above.
// { dg-excess-errors "" }
