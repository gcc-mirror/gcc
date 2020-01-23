#include "config.h"
#if defined(__ELF__) && defined(__linux__)
#if defined (__mips_soft_float)
	.section .note.GNU-stack,"",%progbits
	.previous
#endif
#endif
