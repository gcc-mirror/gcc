#include <bits/c++config.h>
#ifdef _GLIBCPP_HAVE_SINL
/* We compile these functions only when we have the long double functions
   available.  */
#define FLT long double
#define FCT(name) ::name##l
#include "complex.cc"
#endif
