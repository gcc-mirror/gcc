/* Implementation of the IRAND, RAND, and SRAND intrinsics.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.
   Contributed by Steven G. Kargl <kargls@comcast.net>.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Simple multiplicative congruent algorithm.
   The period of this generator is approximately 2^31-1, which means that
   it should not be used for anything serious.  The implementation here
   is based of an algorithm from  S.K. Park and K.W. Miller, Comm. ACM,
   31, 1192-1201 (1988).  It is also provided solely for compatibility 
   with G77.  */

#include "config.h"
#include "libgfortran.h"
#include "../io/io.h"

#define GFC_RAND_A	16807
#define GFC_RAND_M	2147483647
#define GFC_RAND_M1	(GFC_RAND_M - 1)

static GFC_UINTEGER_8 rand_seed = 1;
#ifdef __GTHREAD_MUTEX_INIT
static __gthread_mutex_t rand_seed_lock = __GTHREAD_MUTEX_INIT;
#else
static __gthread_mutex_t rand_seed_lock;
#endif


/* Set the seed of the irand generator.  Note 0 is a bad seed.  */

static void
srand_internal (GFC_INTEGER_8 i)
{
  rand_seed = i ? i : 123459876;
}

extern void PREFIX(srand) (GFC_INTEGER_4 *i);
export_proto_np(PREFIX(srand));

void
PREFIX(srand) (GFC_INTEGER_4 *i)
{
  __gthread_mutex_lock (&rand_seed_lock);
  srand_internal (*i);
  __gthread_mutex_unlock (&rand_seed_lock);
}

/* Return an INTEGER in the range [1,GFC_RAND_M-1].  */

extern GFC_INTEGER_4 irand (GFC_INTEGER_4 *);
iexport_proto(irand);

GFC_INTEGER_4
irand (GFC_INTEGER_4 *i)
{
  GFC_INTEGER_4 j;
  if (i)
    j = *i;
  else
    j = 0;

  __gthread_mutex_lock (&rand_seed_lock);

  switch (j)
  {
    /* Return the next RN. */
    case 0:
      break;

    /* Reset the RN sequence to system-dependent sequence and return the
       first value.  */
    case 1:
      srand_internal (0);
      break;
    
    /* Seed the RN sequence with j and return the first value.  */
    default:
      srand_internal (j);
      break;
   }

   rand_seed = GFC_RAND_A * rand_seed % GFC_RAND_M;
   j = (GFC_INTEGER_4) rand_seed;

  __gthread_mutex_unlock (&rand_seed_lock);

   return j;
}
iexport(irand);


/*  Return a random REAL in the range [0,1).  */

extern GFC_REAL_4 PREFIX(rand) (GFC_INTEGER_4 *i);
export_proto_np(PREFIX(rand));

GFC_REAL_4
PREFIX(rand) (GFC_INTEGER_4 *i)
{
  return normalize_r4_i4 (irand (i) - 1, GFC_RAND_M1 - 1);
}

#ifndef __GTHREAD_MUTEX_INIT
static void __attribute__((constructor))
init (void)
{
  __GTHREAD_MUTEX_INIT_FUNCTION (&rand_seed_lock);
}
#endif
