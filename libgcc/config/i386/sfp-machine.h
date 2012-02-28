#ifdef __MINGW32__
  /* Make sure we are using gnu-style bitfield handling.  */
#define _FP_STRUCT_LAYOUT  __attribute__ ((gcc_struct))
#endif

#ifdef __x86_64__
#include "config/i386/64/sfp-machine.h"
#else
#include "config/i386/32/sfp-machine.h"
#endif
