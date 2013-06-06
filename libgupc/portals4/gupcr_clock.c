/* Copyright (C) 2012-2013 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/**
 * @file gupcr_clock.c
 * GUPC Clock routines.
 */

/**
 * @addtogroup UPCCLOCK UPC Clock Functions
 * @{
 */

#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_utils.h"

static double gupcr_clock_rez;
static double gupcr_clock_base;

#if HAVE_CLOCK_GETTIME

#if defined(CLOCK_MONOTONIC_RAW) && defined(CLOCK_MONOTONIC)
#define GUPCR_CLOCK_ID CLOCK_MONOTONIC_RAW
/* On some RHEL/CentOS systems, the timer resolution returned for
   CLOCK_MONOTONIC_RAW is incorrect.  Use CLOCK_MONOTONIC instead.  */
#define GUPCR_CLOCK_REZ_ID CLOCK_MONOTONIC
#elif defined(CLOCK_MONOTONIC)
#define GUPCR_CLOCK_ID CLOCK_MONOTONIC
#define GUPCR_CLOCK_REZ_ID CLOCK_MONOTONIC
#else
#error missing system clock name definition.
#endif

double
gupcr_clock (void)
{
  struct timespec ts;
  double t;
  gupcr_syscall (clock_gettime, (GUPCR_CLOCK_ID, &ts));
  t = (double) ts.tv_sec + (double) ts.tv_nsec * 1.0e-9;
  t -= gupcr_clock_base;
  return t;
}

double
gupcr_clock_resolution (void)
{
  return gupcr_clock_rez;
}

void
gupcr_clock_init (void)
{
  struct timespec clock_rez;
  gupcr_syscall (clock_getres, (GUPCR_CLOCK_REZ_ID, &clock_rez));
  gupcr_assert (clock_rez.tv_sec == 0);
  gupcr_clock_rez = clock_rez.tv_nsec * 1.0e-9;
  gupcr_clock_base = gupcr_clock ();
}

#else /* Use gettimeofday().  */

double
gupcr_clock (void)
{
  struct timeval tv;
  double t;
  gupcr_syscall (gettimeofday, (&tv, NULL));
  t = (double) tv.tv_sec + (double) tv.tv_usec * 1.0e-6;
  t -= gupcr_clock_base;
  return t;
}

double
gupcr_clock_resolution (void)
{
  return gupcr_clock_rez;
}

void
gupcr_clock_init (void)
{
  int i;
  gupcr_clock_base = gupcr_clock ();
  gupcr_clock_rez = 1.0;
  for (i = 1; i <= 10; ++i)
    {
      double t1, t2, diff;
      t1 = gupcr_clock ();
      do
	{
	  t2 = gupcr_clock ();
	}
      while (t1 == t2);
      diff = t2 - t1;
      if (diff < gupcr_clock_rez)
	gupcr_clock_rez = diff;
    }
  /* Round the clock resolution to some common values
     if it is within range of one of them.  */
  if (gupcr_clock_rez > 0.9e-6 && gupcr_clock_rez < 1.1e-6)
    gupcr_clock_rez = 1.0e-6;
  else if (gupcr_clock_rez > 0.9e-3 && gupcr_clock_rez < 1.1e-3)
    gupcr_clock_rez = 1.0e-3;
  else if (gupcr_clock_rez > 0.9e-2 && gupcr_clock_rez < 1.1e-2)
    gupcr_clock_rez = 1.0e-2;
  else if (gupcr_clock_rez > 1.63e-2 && gupcr_clock_rez < 1.69e-2)
    gupcr_clock_rez = 1.0 / 60.0;
  else if (gupcr_clock_rez > 1.95e-2 && gupcr_clock_rez < 2.05e-2)
    gupcr_clock_rez = 1.0 / 50.0;
}

#endif
/** @} */
