// { dg-options "-std=gnu++14" }
// { dg-do compile }

#include <utility>
#include <tuple>
#include <memory>
#include <functional>
#include <type_traits>
#include <chrono>
#include <string>
#include <complex>
#include <iomanip>
#include <shared_mutex>
#include <map>
#include <set>

#ifndef  __cpp_lib_integer_sequence
#  error "__cpp_lib_integer_sequence"
#elif  __cpp_lib_integer_sequence != 201304
#  error "__cpp_lib_integer_sequence != 201304"
#endif

#ifndef  __cpp_lib_exchange_function
#  error "__cpp_lib_exchange_function"
#elif  __cpp_lib_exchange_function != 201304
#  error "__cpp_lib_exchange_function != 201304"
#endif

#ifndef  __cpp_lib_tuples_by_type
#  error "__cpp_lib_tuples_by_type"
#elif  __cpp_lib_tuples_by_type != 201304
#  error "__cpp_lib_tuples_by_type != 201304"
#endif

#ifndef  __cpp_lib_make_unique
#  error "__cpp_lib_make_unique"
#elif  __cpp_lib_make_unique != 201304
#  error "__cpp_lib_make_unique != 201304"
#endif

#ifndef  __cpp_lib_transparent_operators
#  error "__cpp_lib_transparent_operators"
#elif  __cpp_lib_transparent_operators != 201210
#  error "__cpp_lib_transparent_operators != 201210"
#endif

#ifndef  __cpp_lib_result_of_sfinae
#  error "__cpp_lib_result_of_sfinae"
#elif  __cpp_lib_result_of_sfinae != 201210
#  error "__cpp_lib_result_of_sfinae != 201210"
#endif

#ifndef  __cpp_lib_integral_constant_callable
#  error "__cpp_lib_integral_constant_callable"
#elif  __cpp_lib_integral_constant_callable != 201304
#  error "__cpp_lib_integral_constant_callable != 201304"
#endif

#ifndef  __cpp_lib_transformation_trait_aliases
#  error "__cpp_lib_transformation_trait_aliases"
#elif  __cpp_lib_transformation_trait_aliases != 201304
#  error "__cpp_lib_transformation_trait_aliases != 201304"
#endif

#ifndef  __cpp_lib_chrono_udls
#  error "__cpp_lib_chrono_udls"
#elif  __cpp_lib_chrono_udls != 201304
#  error "__cpp_lib_chrono_udls != 201304"
#endif

#ifndef  __cpp_lib_string_udls
#  error "__cpp_lib_string_udls"
#elif  __cpp_lib_string_udls != 201304
#  error "__cpp_lib_string_udls != 201304"
#endif

#ifndef __cpp_lib_complex_udls
#  error "__cpp_lib_complex_udls"
#elif  __cpp_lib_complex_udls != 201309
#  error "__cpp_lib_complex_udls != 201309"
#endif

#ifndef  __cpp_lib_generic_associative_lookup
#  error "__cpp_lib_generic_associative_lookup"
#elif  __cpp_lib_generic_associative_lookup != 201304
#  error "__cpp_lib_generic_associative_lookup != 201304"
#endif

//#ifndef  __cpp_lib_null_iterators
//#  error "__cpp_lib_null_iterators"
//#elif  __cpp_lib_null_iterators != 201304
//#  error "__cpp_lib_null_iterators != 201304"
//#endif

#ifndef  __cpp_lib_robust_nonmodifying_seq_ops
#  error "__cpp_lib_robust_nonmodifying_seq_ops"
#elif  __cpp_lib_robust_nonmodifying_seq_ops != 201304
#  error "__cpp_lib_robust_nonmodifying_seq_ops != 201304"
#endif

#ifndef  __cpp_lib_quoted_string_io
#  error "__cpp_lib_quoted_string_io"
#elif  __cpp_lib_quoted_string_io != 201304
#  error "__cpp_lib_quoted_string_io != 201304"
#endif

#if !__has_include(<shared_mutex>)
#  error "<shared_mutex>"
#endif

#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)
#  ifndef  __cpp_lib_shared_timed_mutex
#    error "__cpp_lib_shared_timed_mutex"
#  elif  __cpp_lib_shared_timed_mutex != 201402
#    error "__cpp_lib_shared_timed_mutex != 201402"
#  endif
#endif

#ifndef  __cpp_lib_is_final
#  error "__cpp_lib_is_final"
#elif  __cpp_lib_is_final != 201402
#  error "__cpp_lib_is_final != 201402"
#endif

#ifndef  __cpp_lib_is_null_pointer
#  error "__cpp_lib_is_null_pointer"
#elif  __cpp_lib_is_null_pointer != 201309
#  error "__cpp_lib_is_null_pointer != 201309"
#endif

#ifndef  __cpp_lib_make_reverse_iterator
#  error "__cpp_lib_make_reverse_iterator"
#elif  __cpp_lib_make_reverse_iterator != 201402
#  error "__cpp_lib_make_reverse_iterator != 201402"
#endif
