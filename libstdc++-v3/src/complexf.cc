#define FLT float
// Used in complex.cc to work around GCC's buggy __complex__ float support.
#define _GLIBCPP_FLOAT_SPECIALIZATION 1
#include "complex.cc"
