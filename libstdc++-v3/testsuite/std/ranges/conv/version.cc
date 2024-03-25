// { dg-do preprocess { target c++23 } }
// { dg-add-options no_pch }

#include <ranges>

#ifndef __cpp_lib_ranges_to_container
# error "Feature test macro for ranges_to_container is missing in <ranges>"
#elif __cpp_lib_ranges_to_container < 202202L
# error "Feature test macro for ranges_to_container has wrong value in <ranges>"
#endif

#undef __cpp_lib_ranges_to_container
#include <version>

#ifndef __cpp_lib_ranges_to_container
# error "Feature test macro for ranges_to_container is missing in <version>"
#elif __cpp_lib_ranges_to_container < 202202L
# error "Feature test macro for ranges_to_container has wrong value in <version>"
#endif
