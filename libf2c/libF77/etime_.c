#include "time.h"

#ifdef MSDOS
#undef USE_CLOCK
#define USE_CLOCK
#endif

#ifndef USE_CLOCK
#define _INCLUDE_POSIX_SOURCE	/* for HP-UX */
#define _INCLUDE_XOPEN_SOURCE	/* for HP-UX */
#include "sys/types.h"
#include "sys/times.h"
#endif

#undef Hz
#ifdef CLK_TCK
#define Hz CLK_TCK
#else
#ifdef HZ
#define Hz HZ
#else
#define Hz 60
#endif
#endif

double
etime_ (float *tarray)
{
#ifdef USE_CLOCK
#ifndef CLOCKS_PER_SECOND
#define CLOCKS_PER_SECOND Hz
#endif
  double t = clock ();
  tarray[1] = 0;
  return tarray[0] = t / CLOCKS_PER_SECOND;
#else
  struct tms t;

  times (&t);
  return (tarray[0] = (double) t.tms_utime / Hz)
    + (tarray[1] = (double) t.tms_stime / Hz);
#endif
}
