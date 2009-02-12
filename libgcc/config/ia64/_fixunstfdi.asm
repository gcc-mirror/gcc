#ifdef SHARED
#define __fixunstfti __fixunstfti_compat
#endif

#define L_fixunstfdi
#include "config/ia64/lib1funcs.asm"

#ifdef SHARED
#undef __fixunstfti
.symver __fixunstfti_compat, __fixunstfti@GCC_3.0
#endif
