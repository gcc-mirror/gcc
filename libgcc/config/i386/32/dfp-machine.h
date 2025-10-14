#ifndef _SOFT_FLOAT
/* Get the rounding mode.  */
#define DFP_GET_ROUNDMODE						\
  unsigned short _frnd_orig;						\
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
      unsigned short _fcw;						\
      __asm__ __volatile__ ("fnstcw\t%0" : "=m" (_fcw));		\
      _fcw &= ~FP_RND_MASK;						\
      _fcw |= round;							\
      __asm__ __volatile__ ("fldcw\t%0" :: "m" (_fcw));			\
      if (__builtin_cpu_supports ("sse"))				\
	{								\
	  unsigned int _xcw;						\
	  __asm__ __volatile__ ("%vstmxcsr\t%0" : "=m" (_xcw));		\
	  _xcw &= ~0x6000;						\
	  _xcw |= round << 3;						\
	  __asm__ __volatile__ ("%vldmxcsr\t%0" :: "m" (_xcw));		\
	}								\
    }									\
  while (0);
#endif
