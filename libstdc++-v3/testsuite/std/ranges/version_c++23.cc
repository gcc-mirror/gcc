// { dg-options "-std=gnu++23" }
// { dg-do preprocess { target c++23 } }

#include <version>

#if __STDC_HOSTED__
# if __cpp_lib_ranges != 202110L
#  error "Feature-test macro __cpp_lib_ranges has wrong value in <version>"
# endif
#endif

#if __cpp_lib_ranges_zip != 202110L
# error "Feature-test macro __cpp_lib_ranges_zip has wrong value in <version>"
#endif

#if __cpp_lib_ranges_chunk != 202202L
# error "Feature-test macro __cpp_lib_ranges_chunk has wrong value in <version>"
#endif

#if __cpp_lib_ranges_slide != 202202L
# error "Feature-test macro __cpp_lib_ranges_slide has wrong value in <version>"
#endif

#if __cpp_lib_ranges_chunk_by != 202202L
# error "Feature-test macro __cpp_lib_ranges_chunk_by has wrong value in <version>"
#endif

#if __cpp_lib_ranges_join_with != 202202L
# error "Feature-test macro __cpp_lib_ranges_join_with has wrong value in <version>"
#endif

#if __cpp_lib_ranges_repeat != 202207L
# error "Feature-test macro __cpp_lib_ranges_repeat has wrong value in <version>"
#endif

#if __cpp_lib_ranges_stride != 202207L
# error "Feature-test macro __cpp_lib_ranges_stride has wrong value in <version>"
#endif

#if __cpp_lib_ranges_cartesian_product != 202207L
# error "Feature-test macro __cpp_lib_ranges_cartesian_product has wrong value in <version>"
#endif

#if __cpp_lib_ranges_as_rvalue != 202207L
# error "Feature-test macro __cpp_lib_ranges_as_rvalue has wrong value in <version>"
#endif
