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
#ifdef KR_headers
dtime_(tarray) float *tarray;
#else
dtime_(float *tarray)
#endif
{
#ifdef USE_CLOCK
#ifndef CLOCKS_PER_SECOND
#define CLOCKS_PER_SECOND Hz
#endif
	static double t0;
	double t = clock();
	tarray[1] = 0;
	tarray[0] = (t - t0) / CLOCKS_PER_SECOND;
	t0 = t;
	return tarray[0];
#else
	struct tms t;
	static struct tms t0;

	times(&t);
	tarray[0] = (double)(t.tms_utime - t0.tms_utime) / Hz;
	tarray[1] = (double)(t.tms_stime - t0.tms_stime) / Hz;
	t0 = t;
	return tarray[0] + tarray[1];
#endif
	}
