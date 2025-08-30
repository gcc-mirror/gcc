#ifndef _SOFT_FLOAT
/* Get the rounding mode.  */
#define DFP_GET_ROUNDMODE						\
  unsigned int _frnd_orig;						\
  do									\
    {									\
      __asm__ __volatile__ ("fnstcw\t%0" : "=m" (_frnd_orig));		\
      _frnd_orig &= FP_RND_MASK;					\
    }									\
  while (0);

/* Set the rounding mode.  */
#define DFP_SET_ROUNDMODE(round)					\
  do									\
    {									\
      unsigned int _cw;							\
      __asm__ __volatile__ ("fnstcw\t%0" : "=m" (_cw));			\
      _cw &= ~FP_RND_MASK;						\
      _cw |= round;							\
      __asm__ __volatile__ ("fldcw\t%0" :: "m" (_cw));			\
      if (__builtin_cpu_supports ("sse"))				\
	{								\
	  __asm__ __volatile__ ("%vstmxcsr\t%0" : "=m" (_cw));		\
	  _cw &= ~0x6000;						\
	  _cw |= round << 3;						\
	  __asm__ __volatile__ ("%vldmxcsr\t%0" :: "m" (_cw));		\
	}								\
    }									\
  while (0);
#endif
