/* Implementation of the IRAND, RAND, and SRAND intrinsics.
   Copyright (C) 2004 Free Software Foundation, Inc.
   Contributed by Steven G. Kargl <kargls@comcast.net>.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfor; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Simple multiplicative congruent algorithm.
   The period of this generator is approximately 2^31-1, which means that
   it should not be used for anything serious.  The implementation here
   is based of an algorithm from  S.K. Park and K.W. Miller, Comm. ACM,
   31, 1192-1201 (1988).  It is also provided solely for compatibility 
   with G77.  */

#include "config.h"
#include "libgfortran.h"

#define GFC_RAND_A	16807
#define GFC_RAND_M	2147483647
#define GFC_RAND_M1	(GFC_RAND_M - 1)

static GFC_UINTEGER_8 rand_seed = 1;


/* Set the seed of the irand generator.  Note 0 is a bad seed.  */

void
prefix(srand) (GFC_INTEGER_4 *i)
{
  rand_seed = (GFC_UINTEGER_8) (*i != 0) ? *i : 123459876;
}


/* Return an INTEGER in the range [1,GFC_RAND_M-1].  */

GFC_INTEGER_4
prefix(irand) (GFC_INTEGER_4 *i)
{
  
  GFC_INTEGER_4 j = *i;

  switch (j)
  {
    /* Return the next RN. */
    case 0:
      break;

    /* Reset the RN sequence to system-dependent sequence and return the
       first value.  */
    case 1:
      j = 0;
      prefix(srand) (&j);
      break;
    
    /* Seed the RN sequence with j and return the first value.  */
    default:
      prefix(srand) (&j);
   }

   rand_seed = GFC_RAND_A * rand_seed % GFC_RAND_M;

   return (GFC_INTEGER_4) rand_seed;
}


/*  Return a random REAL in the range [0,1).  */

GFC_REAL_4
prefix(rand) (GFC_INTEGER_4 *i)
{
  return normalize_r4_i4 (prefix(irand) (i) - 1, GFC_RAND_M1 - 1);
}
