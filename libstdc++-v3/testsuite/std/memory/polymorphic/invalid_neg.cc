// { dg-do compile { target c++26 } }

#include <memory>

// In every specialization polymorphic<T, Allocator>, if the type
// allocator_traits<Allocator>::value_type is not the same type as T,
// the program is ill-formed.
using T1 = std::polymorphic<int, std::allocator<long>>::value_type; // { dg-error "here" }

// A program that instantiates the definition of the template
// polymorphic<T, Allocator> with a type for the T parameter that is
// a non-object type, an array type, in_place_t,
// a specialization of in_place_type_t, or a cv-qualified type is ill-formed.

using T2 = std::polymorphic<int&>::value_type; // { dg-error "here" }

using T3 = std::polymorphic<int[1]>::value_type; // { dg-error "here" }

using T4 = std::polymorphic<std::in_place_t>::value_type; // { dg-error "here" }

using T5 = std::polymorphic<std::in_place_type_t<int>>::value_type; // { dg-error "here" }

using T6 = std::polymorphic<const int>::value_type; // { dg-error "here" }

using T7 = std::polymorphic<volatile int>::value_type; // { dg-error "here" }

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
// { dg-prune-output "forming pointer to reference" }
