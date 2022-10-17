// { dg-options "-std=gnu++20" }
// { dg-do preprocess { target c++20 } }
// { dg-require-effective-target hosted }

// C++20 20.11.3.7 shared_ptr Creation [util.smartptr.shared.create]

#include <version>

#ifndef __cpp_lib_shared_ptr_arrays
# error "Feature-test macro for make_shared arrays missing in <version>"
#elif __cpp_lib_shared_ptr_arrays < 201707L
# error "Feature-test macro for make_shared arrays has wrong value in <version>"
#endif

#ifndef __cpp_lib_smart_ptr_for_overwrite
# error "Feature-test macro for make_shared_for_overwrite missing in <version>"
#elif __cpp_lib_smart_ptr_for_overwrite < 202002L
# error "Feature-test macro for make_shared_for_overwrite has wrong value in <version>"
#endif
