#ifdef SHARED
#define __gttf2 __gttf2_shared
#endif

#include "config/soft-fp/getf2.c"

#ifdef SHARED
#undef __gttf2
strong_alias (__gttf2_shared, __gttf2_compat);

#ifndef _WIN32
asm (".symver __gttf2_compat,__gttf2@GCC_3.0");
asm (".symver __gttf2_shared,__gttf2@@GCC_4.3.0");
#endif
#endif
