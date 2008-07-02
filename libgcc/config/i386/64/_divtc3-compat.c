#ifdef SHARED
#define __divtc3 __divtc3_shared
#endif

#define L_divtc3
#include "libgcc2.c"

#ifdef SHARED
#undef __divtc3
extern __typeof__ (__divtc3_shared) __divtc3_compat __attribute__((alias ("__divtc3_shared")));

asm (".symver __divtc3_compat,__divtc3@GCC_4.0.0");
asm (".symver __divtc3_shared,__divtc3@@GCC_4.3.0");
#endif
