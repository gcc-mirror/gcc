#include <bits/c++config.h>
#ifdef _GLIBCPP_USE_LONG_DOUBLE
/* We compile these functions only when we have the long double functions
   available.  */
#define FLT long double
#include "complex.cc"
#endif
