// P2655R3 - common_reference_t of reference_wrapper Should Be a Reference Type
// Implemented as a DR against C++20
// { dg-do compile { target c++20 } }

#include <type_traits>

#if __cpp_lib_common_reference != 202302L
# error "Feature-test macro __cpp_lib_common_reference has wrong value in <type_traits>"
#endif

struct A { };
struct B { operator A&() const; };

static_assert( std::is_same_v<std::common_reference_t<A&, const B&>, A&> );
static_assert( std::is_same_v<std::common_reference_t<const B&, A&>, A&> );
