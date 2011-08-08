#ifdef SHARED
#define __netf2 __netf2_shared
#endif

#include "soft-fp/eqtf2.c"

#ifdef SHARED
#undef __netf2
strong_alias (__netf2_shared, __netf2_compat);

#ifndef _WIN32
asm (".symver __netf2_compat,__netf2@GCC_3.0");
asm (".symver __netf2_shared,__netf2@@GCC_4.3.0");
#endif
#endif
