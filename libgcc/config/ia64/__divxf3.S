#ifdef SHARED
#define __divtf3 __divtf3_compat
#endif

#define L__divxf3
#include "config/ia64/lib1funcs.S"

#ifdef SHARED
#undef __divtf3
.symver __divtf3_compat, __divtf3@GCC_3.0
#endif
