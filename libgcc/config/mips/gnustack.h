#include "config.h"
#if defined(__ELF__) && defined(__linux__)
#if defined (TARGET_LIBC_GNUSTACK) || defined (__mips_soft_float)
	.section .note.GNU-stack,"",%progbits
	.previous
#endif
#endif
