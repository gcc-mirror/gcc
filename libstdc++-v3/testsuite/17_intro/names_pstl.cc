// { dg-do compile { target c++17 } }
// { dg-require-effective-target tbb_backend }
// { dg-add-options no_pch }

// The TBB headers use non-reserved names (because they're not part of the
// implementation) so we need to include them before the macro definitions
// in names.cc:
#if __has_include(<tbb/tbb.h>)
# define TBB_SUPPRESS_DEPRECATED_MESSAGES 1
# include <tbb/tbb.h>
#endif
// Now we can define the macros to poison uses of non-reserved names:
#include "names.cc"
// And finally, include all the headers that have PSTL content:
#include <execution>
#include <algorithm>
#include <memory>
#include <numeric>
