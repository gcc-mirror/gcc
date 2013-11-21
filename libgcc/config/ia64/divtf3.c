#ifdef SHARED
#define __divtf3 __divtf3_shared
#endif

#include "soft-fp/divtf3.c"

#ifdef SHARED
asm (".symver __divtf3_shared, __divtf3@@GCC_4.4.0");
#endif
