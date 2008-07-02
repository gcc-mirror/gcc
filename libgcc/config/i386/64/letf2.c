#ifdef SHARED
#define __lttf2 __lttf2_shared
#endif

#include "config/soft-fp/letf2.c"

#ifdef SHARED
#undef __lttf2
strong_alias (__lttf2_shared, __lttf2_compat);

asm (".symver __lttf2_compat,__lttf2@GCC_3.0");
asm (".symver __lttf2_shared,__lttf2@@GCC_4.3.0");
#endif
