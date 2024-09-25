// { dg-do preprocess { target c++17 } }
// { dg-add-options no_pch }

#include <scoped_allocator>

#if __cpp_lib_allocator_traits_is_always_equal != 201411L
# error "Feature-test macro __cpp_lib_allocator_traits_is_always_equal has wrong value in <version>"
#endif
