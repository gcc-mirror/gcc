#ifdef SHARED
#define __fixtfti __fixtfti_compat
#endif

#define L_fixtfdi
#include "config/ia64/lib1funcs.asm"

#ifdef SHARED
#undef __fixtfti
.symver __fixtfti_compat, __fixtfti@GCC_3.0
#endif
