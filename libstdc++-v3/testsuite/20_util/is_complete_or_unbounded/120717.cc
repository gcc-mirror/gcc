// PR libstdc++/120717
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wsfinae-incomplete" }

#include <type_traits>

// Verify __is_complete_or_unbounded doesn't try to instantiate the underlying
// type of a reference or array of unknown bound.
template<class T> struct A { static_assert(false, "do not instantiate"); };
static_assert(std::__is_complete_or_unbounded(std::__type_identity<A<int>&>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<A<int>&&>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<A<int>[]>{}), "");

// Verify __is_complete_or_unbounded doesn't produce unexpected
// -Wsfinae-incomplete warnings.
struct B;
static_assert(std::__is_complete_or_unbounded(std::__type_identity<B&>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<B&&>{}), "");
static_assert(std::__is_complete_or_unbounded(std::__type_identity<B[]>{}), "");
struct B { }; // { dg-bogus "-Wsfinae-incomplete" }
