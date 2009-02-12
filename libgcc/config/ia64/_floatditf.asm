#ifdef SHARED
#define __floattitf __floattitf_compat
#endif

#define L_floatditf
#include "config/ia64/lib1funcs.asm"

#ifdef SHARED
#undef __floattitf
.symver __floattitf_compat, __floattitf@GCC_3.0
#endif
