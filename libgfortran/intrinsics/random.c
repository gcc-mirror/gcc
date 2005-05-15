/* Implementation of the RANDOM intrinsics
   Copyright 2002, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Lars Segerlund <seger@linuxmail.org>
   and Steve Kargl.

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

Ligbfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "libgfortran.h"

extern void random_r4 (GFC_REAL_4 *);
iexport_proto(random_r4);

extern void random_r8 (GFC_REAL_8 *);
iexport_proto(random_r8);

extern void arandom_r4 (gfc_array_r4 *);
export_proto(arandom_r4);

extern void arandom_r8 (gfc_array_r8 *);
export_proto(arandom_r8);

#if 0

/*  The Mersenne Twister code is currently commented out due to

    (1) Simple user specified seeds lead to really bad sequences for
        nearly 100000 random numbers.
    (2) open(), read(), and close() are not properly declared via header
        files.
    (3) The global index i is abused and causes unexpected behavior with
        GET and PUT.
    (4) See PR 15619.

  The algorithm was taken from the paper :

	Mersenne Twister:	623-dimensionally equidistributed
				uniform pseudorandom generator.

	by:	Makoto Matsumoto
		Takuji Nishimura

	Which appeared in the: ACM Transactions on Modelling and Computer
	Simulations: Special Issue on Uniform Random Number
	Generation. ( Early in 1998 ).  */


#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/*Use the 'big' generator by default ( period -> 2**19937 ).  */

#define MT19937

/* Define the necessary constants for the algorithm.  */

#ifdef  MT19937
enum constants
{
  N = 624, M = 397, R = 19, TU = 11, TS = 7, TT = 15, TL = 17
};
#define M_A	0x9908B0DF
#define T_B	0x9D2C5680
#define T_C	0xEFC60000
#else
enum constants
{
  N = 351, M = 175, R = 19, TU = 11, TS = 7, TT = 15, TL = 17
};
#define M_A	0xE4BD75F5
#define T_B	0x655E5280
#define T_C	0xFFD58000
#endif

static int i = N;
static unsigned int seed[N];

/* This is the routine which handles the seeding of the generator,
   and also reading and writing of the seed.  */

void
random_seed (GFC_INTEGER_4 *size, gfc_array_i4 *put, gfc_array_i4 *get)
{
  /* Initialize the seed in system dependent manner.  */
  if (get == NULL && put == NULL && size == NULL)
    {
      int fd;
      fd = open ("/dev/urandom", O_RDONLY);
      if (fd == 0)
	{
	  /* We dont have urandom.  */
	  GFC_UINTEGER_4 s = (GFC_UINTEGER_4) seed;
	  for (i = 0; i < N; i++)
	    {
	      s = s * 29943829 - 1;
	      seed[i] = s;
	    }
	}
      else
	{
	  /* Using urandom, might have a length issue.  */
	  read (fd, &seed[0], sizeof (GFC_UINTEGER_4) * N);
	  close (fd);
	}
      return;
    }

  /* Return the size of the seed */
  if (size != NULL)
    {
      *size = N;
      return;
    }

  /* if we have gotten to this pount we have a get or put
   * now we check it the array fulfills the demands in the standard .
   */

  /* Set the seed to PUT data */
  if (put != NULL)
    {
      /* if the rank of the array is not 1 abort */
      if (GFC_DESCRIPTOR_RANK (put) != 1)
	abort ();

      /* if the array is too small abort */
      if (((put->dim[0].ubound + 1 - put->dim[0].lbound)) < N)
	abort ();

      /* If this is the case the array is a temporary */
      if (put->dim[0].stride == 0)
	return;

      /*  This code now should do correct strides. */
      for (i = 0; i < N; i++)
	seed[i] = put->data[i * put->dim[0].stride];
    }

  /* Return the seed to GET data */
  if (get != NULL)
    {
      /* if the rank of the array is not 1 abort */
      if (GFC_DESCRIPTOR_RANK (get) != 1)
	abort ();

      /* if the array is too small abort */
      if (((get->dim[0].ubound + 1 - get->dim[0].lbound)) < N)
	abort ();

      /* If this is the case the array is a temporary */
      if (get->dim[0].stride == 0)
	return;

      /*  This code now should do correct strides. */
      for (i = 0; i < N; i++)
	get->data[i * get->dim[0].stride] = seed[i];
    }
}
iexport(random_seed);

/* Here is the internal routine which generates the random numbers
   in 'batches' based upon the need for a new batch.
   It's an integer based routine known as 'Mersenne Twister'.
   This implementation still lacks 'tempering' and a good verification,
   but gives very good metrics.  */

static void
random_generate (void)
{
  /* 32 bits.  */
  GFC_UINTEGER_4 y;

  /* Generate batch of N.  */
  int k, m;
  for (k = 0, m = M; k < N - 1; k++)
    {
      y = (seed[k] & (-1 << R)) | (seed[k + 1] & ((1u << R) - 1));
      seed[k] = seed[m] ^ (y >> 1) ^ (-(GFC_INTEGER_4) (y & 1) & M_A);
      if (++m >= N)
	m = 0;
    }

  y = (seed[N - 1] & (-1 << R)) | (seed[0] & ((1u << R) - 1));
  seed[N - 1] = seed[M - 1] ^ (y >> 1) ^ (-(GFC_INTEGER_4) (y & 1) & M_A);
  i = 0;
}

/* A routine to return a REAL(KIND=4).  */

void
random_r4 (GFC_REAL_4 * harv)
{
  /* Regenerate if we need to.  */
  if (i >= N)
    random_generate ();

  /* Convert uint32 to REAL(KIND=4).  */
  *harv = (GFC_REAL_4) ((GFC_REAL_4) (GFC_UINTEGER_4) seed[i++] /
			(GFC_REAL_4) (~(GFC_UINTEGER_4) 0));
}
iexport(random_r4);

/* A routine to return a REAL(KIND=8).  */

void
random_r8 (GFC_REAL_8 * harv)
{
  /* Regenerate if we need to, may waste one 32-bit value.  */
  if ((i + 1) >= N)
    random_generate ();

  /* Convert two uint32 to a REAL(KIND=8).  */
  *harv = ((GFC_REAL_8) ((((GFC_UINTEGER_8) seed[i+1]) << 32) + seed[i])) /
	  (GFC_REAL_8) (~(GFC_UINTEGER_8) 0);
  i += 2;
}
iexport(random_r8);

/* Code to handle arrays will follow here.  */

/* REAL(KIND=4) REAL array.  */

void
arandom_r4 (gfc_array_r4 * harv)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  GFC_REAL_4 *dest;
  int n;

  dest = harv->data;

  if (harv->dim[0].stride == 0)
    harv->dim[0].stride = 1;

  dim = GFC_DESCRIPTOR_RANK (harv);

  for (n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = harv->dim[n].stride;
      extent[n] = harv->dim[n].ubound + 1 - harv->dim[n].lbound;
      if (extent[n] <= 0)
	return;
    }

  stride0 = stride[0];

  while (dest)
    {
      /* Set the elements.  */

      /* regenerate if we need to */
      if (i >= N)
	random_generate ();

      /* Convert uint32 to float in a hopefully g95 compiant manner */
      *dest = (GFC_REAL_4) ((GFC_REAL_4) (GFC_UINTEGER_4) seed[i++] /
			    (GFC_REAL_4) (~(GFC_UINTEGER_4) 0));

      /* Advance to the next element.  */
      dest += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      n = 0;
      while (count[n] == extent[n])
	{
	  /* When we get to the end of a dimension,
	     reset it and increment
	     the next dimension.  */
	  count[n] = 0;
	  /* We could precalculate these products,
	     but this is a less
	     frequently used path so proabably not worth it.  */
	  dest -= stride[n] * extent[n];
	  n++;
	  if (n == dim)
	    {
	      dest = NULL;
	      break;
	    }
	  else
	    {
	      count[n]++;
	      dest += stride[n];
	    }
	}
    }
}

/* REAL(KIND=8) array.  */

void
arandom_r8 (gfc_array_r8 * harv)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  GFC_REAL_8 *dest;
  int n;

  dest = harv->data;

  if (harv->dim[0].stride == 0)
    harv->dim[0].stride = 1;

  dim = GFC_DESCRIPTOR_RANK (harv);

  for (n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = harv->dim[n].stride;
      extent[n] = harv->dim[n].ubound + 1 - harv->dim[n].lbound;
      if (extent[n] <= 0)
	return;
    }

  stride0 = stride[0];

  while (dest)
    {
      /* Set the elements.  */

      /* regenerate if we need to, may waste one 32-bit value */
      if ((i + 1) >= N)
	random_generate ();

      /* Convert two uint32 to a REAL(KIND=8).  */
      *dest = ((GFC_REAL_8) ((((GFC_UINTEGER_8) seed[i+1]) << 32) + seed[i])) /
	      (GFC_REAL_8) (~(GFC_UINTEGER_8) 0);
      i += 2;

      /* Advance to the next element.  */
      dest += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      n = 0;
      while (count[n] == extent[n])
	{
	  /* When we get to the end of a dimension,
	     reset it and increment
	     the next dimension.  */
	  count[n] = 0;
	  /* We could precalculate these products,
	     but this is a less
	     frequently used path so proabably not worth it.  */
	  dest -= stride[n] * extent[n];
	  n++;
	  if (n == dim)
	    {
	      dest = NULL;
	      break;
	    }
	  else
	    {
	      count[n]++;
	      dest += stride[n];
	    }
	}
    }
}

#else

/* George Marsaglia's KISS (Keep It Simple Stupid) random number generator.

   This PRNG combines:

   (1) The congruential generator x(n)=69069*x(n-1)+1327217885 with a period
       of 2^32,
   (2) A 3-shift shift-register generator with a period of 2^32-1,
   (3) Two 16-bit multiply-with-carry generators with a period of
       597273182964842497 > 2^59.

   The overall period exceeds 2^123.

   http://www.ciphersbyritter.com/NEWS4/RANDC.HTM#369F6FCA.74C7C041@stat.fsu.edu

   The above web site has an archive of a newsgroup posting from George
   Marsaglia with the statement:

   Subject: Random numbers for C: Improvements.
   Date: Fri, 15 Jan 1999 11:41:47 -0500
   From: George Marsaglia <geo@stat.fsu.edu>
   Message-ID: <369F6FCA.74C7C041@stat.fsu.edu>
   References: <369B5E30.65A55FD1@stat.fsu.edu>
   Newsgroups: sci.stat.math,sci.math,sci.math.numer-analysis
   Lines: 93

   As I hoped, several suggestions have led to
   improvements in the code for RNG's I proposed for
   use in C. (See the thread "Random numbers for C: Some
   suggestions" in previous postings.) The improved code
   is listed below.

   A question of copyright has also been raised.  Unlike
   DIEHARD, there is no copyright on the code below. You
   are free to use it in any way you want, but you may
   wish to acknowledge the source, as a courtesy.

"There is no copyright on the code below." included the original
KISS algorithm.  */

#define GFC_SL(k, n)	((k)^((k)<<(n)))
#define GFC_SR(k, n)	((k)^((k)>>(n)))

static const GFC_INTEGER_4 kiss_size = 4;
#define KISS_DEFAULT_SEED {123456789, 362436069, 521288629, 916191069}
static const GFC_UINTEGER_4 kiss_default_seed[4] = KISS_DEFAULT_SEED;
static GFC_UINTEGER_4 kiss_seed[4] = KISS_DEFAULT_SEED;

/* kiss_random_kernel() returns an integer value in the range of
   (0, GFC_UINTEGER_4_HUGE].  The distribution of pseudorandom numbers
   should be uniform.  */

static GFC_UINTEGER_4
kiss_random_kernel(void)
{
  GFC_UINTEGER_4 kiss;

  kiss_seed[0] = 69069 * kiss_seed[0] + 1327217885;
  kiss_seed[1] = GFC_SL(GFC_SR(GFC_SL(kiss_seed[1],13),17),5);
  kiss_seed[2] = 18000 * (kiss_seed[2] & 65535) + (kiss_seed[2] >> 16);
  kiss_seed[3] = 30903 * (kiss_seed[3] & 65535) + (kiss_seed[3] >> 16);
  kiss = kiss_seed[0] + kiss_seed[1] + (kiss_seed[2] << 16) + kiss_seed[3];

  return kiss;
}

/*  This function produces a REAL(4) value from the uniform distribution
    with range [0,1).  */

void
random_r4 (GFC_REAL_4 *x)
{
  GFC_UINTEGER_4 kiss;

  kiss = kiss_random_kernel ();
  /* Burn a random number, so the REAL*4 and REAL*8 functions
     produce similar sequences of random numbers.  */
  kiss_random_kernel ();
  *x = normalize_r4_i4 (kiss, ~(GFC_UINTEGER_4) 0);
}
iexport(random_r4);

/*  This function produces a REAL(8) value from the uniform distribution
    with range [0,1).  */

void
random_r8 (GFC_REAL_8 *x)
{
  GFC_UINTEGER_8 kiss;

  kiss = ((GFC_UINTEGER_8)kiss_random_kernel ()) << 32;
  kiss += kiss_random_kernel ();
  *x = normalize_r8_i8 (kiss, ~(GFC_UINTEGER_8) 0);
}
iexport(random_r8);

/*  This function fills a REAL(4) array with values from the uniform
    distribution with range [0,1).  */

void
arandom_r4 (gfc_array_r4 *x)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  GFC_REAL_4 *dest;
  int n;

  dest = x->data;

  if (x->dim[0].stride == 0)
    x->dim[0].stride = 1;

  dim = GFC_DESCRIPTOR_RANK (x);

  for (n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = x->dim[n].stride;
      extent[n] = x->dim[n].ubound + 1 - x->dim[n].lbound;
      if (extent[n] <= 0)
        return;
    }

  stride0 = stride[0];

  while (dest)
    {
      random_r4 (dest);

      /* Advance to the next element.  */
      dest += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so probably not worth it.  */
          dest -= stride[n] * extent[n];
          n++;
          if (n == dim)
            {
              dest = NULL;
              break;
            }
          else
            {
              count[n]++;
              dest += stride[n];
            }
        }
    }
}

/*  This function fills a REAL(8) array with values from the uniform
    distribution with range [0,1).  */

void
arandom_r8 (gfc_array_r8 *x)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  GFC_REAL_8 *dest;
  int n;

  dest = x->data;

  if (x->dim[0].stride == 0)
    x->dim[0].stride = 1;

  dim = GFC_DESCRIPTOR_RANK (x);

  for (n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = x->dim[n].stride;
      extent[n] = x->dim[n].ubound + 1 - x->dim[n].lbound;
      if (extent[n] <= 0)
        return;
    }

  stride0 = stride[0];

  while (dest)
    {
      random_r8 (dest);

      /* Advance to the next element.  */
      dest += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so probably not worth it.  */
          dest -= stride[n] * extent[n];
          n++;
          if (n == dim)
            {
              dest = NULL;
              break;
            }
          else
            {
              count[n]++;
              dest += stride[n];
            }
        }
    }
}

/* random_seed is used to seed the PRNG with either a default
   set of seeds or user specified set of seeds.  random_seed
   must be called with no argument or exactly one argument.  */

void
random_seed (GFC_INTEGER_4 *size, gfc_array_i4 *put, gfc_array_i4 *get)
{
  int i;

  if (size == NULL && put == NULL && get == NULL)
    {
      /* From the standard: "If no argument is present, the processor assigns
         a processor-dependent value to the seed."  */
      kiss_seed[0] = kiss_default_seed[0];
      kiss_seed[1] = kiss_default_seed[1];
      kiss_seed[2] = kiss_default_seed[2];
      kiss_seed[3] = kiss_default_seed[3];
    }

  if (size != NULL)
    *size = kiss_size;

  if (put != NULL)
    {
      /* If the rank of the array is not 1, abort.  */
      if (GFC_DESCRIPTOR_RANK (put) != 1)
        runtime_error ("Array rank of PUT is not 1.");

      /* If the array is too small, abort.  */
      if (((put->dim[0].ubound + 1 - put->dim[0].lbound)) < kiss_size)
        runtime_error ("Array size of PUT is too small.");

      if (put->dim[0].stride == 0)
	put->dim[0].stride = 1;

      /*  This code now should do correct strides.  */
      for (i = 0; i < kiss_size; i++)
        kiss_seed[i] =(GFC_UINTEGER_4) put->data[i * put->dim[0].stride];
    }

  /* Return the seed to GET data.  */
  if (get != NULL)
    {
      /* If the rank of the array is not 1, abort.  */
      if (GFC_DESCRIPTOR_RANK (get) != 1)
	runtime_error ("Array rank of GET is not 1.");

      /* If the array is too small, abort.  */
      if (((get->dim[0].ubound + 1 - get->dim[0].lbound)) < kiss_size)
	runtime_error ("Array size of GET is too small.");

      if (get->dim[0].stride == 0)
	get->dim[0].stride = 1;

      /*  This code now should do correct strides.  */
      for (i = 0; i < kiss_size; i++)
        get->data[i * get->dim[0].stride] = (GFC_INTEGER_4) kiss_seed[i];
    }
}
iexport(random_seed);

#endif /* mersenne twister */
