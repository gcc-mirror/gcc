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
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include "libgfortran.h"
#include "../io/io.h"

extern void random_r4 (GFC_REAL_4 *);
iexport_proto(random_r4);

extern void random_r8 (GFC_REAL_8 *);
iexport_proto(random_r8);

extern void arandom_r4 (gfc_array_r4 *);
export_proto(arandom_r4);

extern void arandom_r8 (gfc_array_r8 *);
export_proto(arandom_r8);

#ifdef HAVE_GFC_REAL_10

extern void random_r10 (GFC_REAL_10 *);
iexport_proto(random_r10);

extern void arandom_r10 (gfc_array_r10 *);
export_proto(arandom_r10);

#endif

#ifdef HAVE_GFC_REAL_16

extern void random_r16 (GFC_REAL_16 *);
iexport_proto(random_r16);

extern void arandom_r16 (gfc_array_r16 *);
export_proto(arandom_r16);

#endif

#ifdef __GTHREAD_MUTEX_INIT
static __gthread_mutex_t random_lock = __GTHREAD_MUTEX_INIT;
#else
static __gthread_mutex_t random_lock;
#endif

/* Helper routines to map a GFC_UINTEGER_* to the corresponding
   GFC_REAL_* types in the range of [0,1).  If GFC_REAL_*_RADIX are 2
   or 16, respectively, we mask off the bits that don't fit into the
   correct GFC_REAL_*, convert to the real type, then multiply by the
   correct offset.
*/


static inline void
rnumber_4 (GFC_REAL_4 *f, GFC_UINTEGER_4 v)
{
  GFC_UINTEGER_4 mask;
#if GFC_REAL_4_RADIX == 2
  mask = ~ (GFC_UINTEGER_4) 0u << (32 - GFC_REAL_4_DIGITS);
#elif GFC_REAL_4_RADIX == 16
  mask = ~ (GFC_UINTEGER_4) 0u << ((8 - GFC_REAL_4_DIGITS) * 4);
#else
#error "GFC_REAL_4_RADIX has unknown value"
#endif
  v = v & mask;
  *f = (GFC_REAL_4) v * (GFC_REAL_4) 0x1.p-32f;
}

static inline void
rnumber_8 (GFC_REAL_8 *f, GFC_UINTEGER_8 v)
{
  GFC_UINTEGER_8 mask;
#if GFC_REAL_8_RADIX == 2
  mask = ~ (GFC_UINTEGER_8) 0u << (64 - GFC_REAL_8_DIGITS);
#elif GFC_REAL_8_RADIX == 16
  mask = ~ (GFC_UINTEGER_8) 0u << (16 - GFC_REAL_8_DIGITS) * 4);
#else
#error "GFC_REAL_8_RADIX has unknown value"
#endif
  v = v & mask;
  *f = (GFC_REAL_8) v * (GFC_REAL_8) 0x1.p-64;
}

#ifdef HAVE_GFC_REAL_10

static inline void
rnumber_10 (GFC_REAL_10 *f, GFC_UINTEGER_8 v)
{
  GFC_UINTEGER_8 mask;
#if GFC_REAL_10_RADIX == 2
  mask = ~ (GFC_UINTEGER_8) 0u << (64 - GFC_REAL_10_DIGITS);
#elif GFC_REAL_10_RADIX == 16
  mask = ~ (GFC_UINTEGER_10) 0u << ((16 - GFC_REAL_10_DIGITS) * 4);
#else
#error "GFC_REAL_10_RADIX has unknown value"
#endif
  v = v & mask;
  *f = (GFC_REAL_10) v * (GFC_REAL_10) 0x1.p-64;
}
#endif

#ifdef HAVE_GFC_REAL_16

/* For REAL(KIND=16), we only need to mask off the lower bits.  */

static inline void
rnumber_16 (GFC_REAL_16 *f, GFC_UINTEGER_8 v1, GFC_UINTEGER_8 v2)
{
  GFC_UINTEGER_8 mask;
#if GFC_REAL_16_RADIX == 2
  mask = ~ (GFC_UINTEGER_8) 0u << (128 - GFC_REAL_16_DIGITS);
#elif GFC_REAL_16_RADIX == 16
  mask = ~ (GFC_UINTEGER_8) 0u << ((32 - GFC_REAL_16_DIGITS) * 4);
#else
#error "GFC_REAL_16_RADIX has unknown value"
#endif
  v2 = v2 & mask;
  *f = (GFC_REAL_16) v1 * (GFC_REAL_16) 0x1.p-64
    + (GFC_REAL_16) v2 * (GFC_REAL_16) 0x1.p-128;
}
#endif
/* libgfortran previously had a Mersenne Twister, taken from the paper:
  
	Mersenne Twister:	623-dimensionally equidistributed
				uniform pseudorandom generator.

	by Makoto Matsumoto & Takuji Nishimura
	which appeared in the: ACM Transactions on Modelling and Computer
	Simulations: Special Issue on Uniform Random Number
	Generation. ( Early in 1998 ).

   The Mersenne Twister code was replaced due to

    (1) Simple user specified seeds lead to really bad sequences for
        nearly 100000 random numbers.
    (2) open(), read(), and close() were not properly declared via header
        files.
    (3) The global index i was abused and caused unexpected behavior with
        GET and PUT.
    (4) See PR 15619.


   libgfortran currently uses George Marsaglia's KISS (Keep It Simple Stupid)
   random number generator.  This PRNG combines:

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

/* We use three KISS random number generators, with different
   seeds.
   As a matter of Quality of Implementation, the random numbers
   we generate for different REAL kinds, starting from the same
   seed, are always the same up to the precision of these types.
   We do this by using three generators with different seeds, the
   first one always for the most significant bits, the second one
   for bits 33..64 (if present in the REAL kind), and the third one
   (called twice) for REAL(16).
*/

#define GFC_SL(k, n)	((k)^((k)<<(n)))
#define GFC_SR(k, n)	((k)^((k)>>(n)))

/*   Reference for the seed:
   From: "George Marsaglia" <g...@stat.fsu.edu>
   Newsgroups: sci.math
   Message-ID: <e7CcnWxczriWssCjXTWc3A@comcast.com>
  
   The KISS RNG uses four seeds, x, y, z, c,
   with 0<=x<2^32, 0<y<2^32, 0<=z<2^32, 0<=c<698769069
   except that the two pairs
   z=0,c=0 and z=2^32-1,c=698769068
   should be avoided.
*/

#define KISS_DEFAULT_SEED_1 123456789, 362436069, 521288629, 316191069
#define KISS_DEFAULT_SEED_2 987654321, 458629013, 582859209, 438195021
#ifdef HAVE_GFC_REAL_16
#define KISS_DEFAULT_SEED_3 573658661, 185639104, 582619469, 296736107
#endif

static GFC_UINTEGER_4 kiss_seed[] = {
  KISS_DEFAULT_SEED_1,
  KISS_DEFAULT_SEED_2,
#ifdef HAVE_GFC_REAL_16
  KISS_DEFAULT_SEED_3
#endif
};

static GFC_UINTEGER_4 kiss_default_seed[] = {
  KISS_DEFAULT_SEED_1,
  KISS_DEFAULT_SEED_2,
#ifdef HAVE_GFC_REAL_16
  KISS_DEFAULT_SEED_3
#endif
};

static const GFC_INTEGER_4 kiss_size = sizeof(kiss_seed)/sizeof(kiss_seed[0]);

static GFC_UINTEGER_4 * const kiss_seed_1 = kiss_seed;
static GFC_UINTEGER_4 * const kiss_seed_2 = kiss_seed + 4;

#ifdef HAVE_GFC_REAL_16
static GFC_UINTEGER_4 * const kiss_seed_3 = kiss_seed + 8;
#endif

/* kiss_random_kernel() returns an integer value in the range of
   (0, GFC_UINTEGER_4_HUGE].  The distribution of pseudorandom numbers
   should be uniform.  */

static GFC_UINTEGER_4
kiss_random_kernel(GFC_UINTEGER_4 * seed)
{
  GFC_UINTEGER_4 kiss;

  seed[0] = 69069 * seed[0] + 1327217885;
  seed[1] = GFC_SL(GFC_SR(GFC_SL(seed[1],13),17),5);
  seed[2] = 18000 * (seed[2] & 65535) + (seed[2] >> 16);
  seed[3] = 30903 * (seed[3] & 65535) + (seed[3] >> 16);
  kiss = seed[0] + seed[1] + (seed[2] << 16) + seed[3];

  return kiss;
}

/*  This function produces a REAL(4) value from the uniform distribution
    with range [0,1).  */

void
random_r4 (GFC_REAL_4 *x)
{
  GFC_UINTEGER_4 kiss;

  __gthread_mutex_lock (&random_lock);
  kiss = kiss_random_kernel (kiss_seed_1);
  rnumber_4 (x, kiss);
  __gthread_mutex_unlock (&random_lock);
}
iexport(random_r4);

/*  This function produces a REAL(8) value from the uniform distribution
    with range [0,1).  */

void
random_r8 (GFC_REAL_8 *x)
{
  GFC_UINTEGER_8 kiss;

  __gthread_mutex_lock (&random_lock);
  kiss = ((GFC_UINTEGER_8) kiss_random_kernel (kiss_seed_1)) << 32;
  kiss += kiss_random_kernel (kiss_seed_2);
  rnumber_8 (x, kiss);
  __gthread_mutex_unlock (&random_lock);
}
iexport(random_r8);

#ifdef HAVE_GFC_REAL_10

/*  This function produces a REAL(10) value from the uniform distribution
    with range [0,1).  */

void
random_r10 (GFC_REAL_10 *x)
{
  GFC_UINTEGER_8 kiss;

  __gthread_mutex_lock (&random_lock);
  kiss = ((GFC_UINTEGER_8) kiss_random_kernel (kiss_seed_1)) << 32;
  kiss += kiss_random_kernel (kiss_seed_2);
  rnumber_10 (x, kiss);
  __gthread_mutex_unlock (&random_lock);
}
iexport(random_r10);

#endif

/*  This function produces a REAL(16) value from the uniform distribution
    with range [0,1).  */

#ifdef HAVE_GFC_REAL_16

void
random_r16 (GFC_REAL_16 *x)
{
  GFC_UINTEGER_8 kiss1, kiss2;

  __gthread_mutex_lock (&random_lock);
  kiss1 = ((GFC_UINTEGER_8) kiss_random_kernel (kiss_seed_1)) << 32;
  kiss1 += kiss_random_kernel (kiss_seed_2);

  kiss2 = ((GFC_UINTEGER_8) kiss_random_kernel (kiss_seed_3)) << 32;
  kiss2 += kiss_random_kernel (kiss_seed_3);

  rnumber_16 (x, kiss1, kiss2);
  __gthread_mutex_unlock (&random_lock);
}
iexport(random_r16);


#endif
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
  GFC_UINTEGER_4 kiss;
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

  __gthread_mutex_lock (&random_lock);

  while (dest)
    {
      /* random_r4 (dest); */
      kiss = kiss_random_kernel (kiss_seed_1);
      rnumber_4 (dest, kiss);

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
  __gthread_mutex_unlock (&random_lock);
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
  GFC_UINTEGER_8 kiss;
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

  __gthread_mutex_lock (&random_lock);

  while (dest)
    {
      /* random_r8 (dest); */
      kiss = ((GFC_UINTEGER_8) kiss_random_kernel (kiss_seed_1)) << 32;
      kiss += kiss_random_kernel (kiss_seed_2);
      rnumber_8 (dest, kiss);

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
  __gthread_mutex_unlock (&random_lock);
}

#ifdef HAVE_GFC_REAL_10

/*  This function fills a REAL(10) array with values from the uniform
    distribution with range [0,1).  */

void
arandom_r10 (gfc_array_r10 *x)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  GFC_REAL_10 *dest;
  GFC_UINTEGER_8 kiss;
  int n;

  dest = x->data;

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

  __gthread_mutex_lock (&random_lock);

  while (dest)
    {
      /* random_r10 (dest); */
      kiss = ((GFC_UINTEGER_8) kiss_random_kernel (kiss_seed_1)) << 32;
      kiss += kiss_random_kernel (kiss_seed_2);
      rnumber_10 (dest, kiss);

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
  __gthread_mutex_unlock (&random_lock);
}

#endif

#ifdef HAVE_GFC_REAL_16

/*  This function fills a REAL(16) array with values from the uniform
    distribution with range [0,1).  */

void
arandom_r16 (gfc_array_r16 *x)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  GFC_REAL_16 *dest;
  GFC_UINTEGER_8 kiss1, kiss2;
  int n;

  dest = x->data;

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

  __gthread_mutex_lock (&random_lock);

  while (dest)
    {
      /* random_r16 (dest); */
      kiss1 = ((GFC_UINTEGER_8) kiss_random_kernel (kiss_seed_1)) << 32;
      kiss1 += kiss_random_kernel (kiss_seed_2);

      kiss2 = ((GFC_UINTEGER_8) kiss_random_kernel (kiss_seed_3)) << 32;
      kiss2 += kiss_random_kernel (kiss_seed_3);

      rnumber_16 (dest, kiss1, kiss2);

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
  __gthread_mutex_unlock (&random_lock);
}

#endif

/* random_seed is used to seed the PRNG with either a default
   set of seeds or user specified set of seeds.  random_seed
   must be called with no argument or exactly one argument.  */

void
random_seed (GFC_INTEGER_4 *size, gfc_array_i4 *put, gfc_array_i4 *get)
{
  int i;

  __gthread_mutex_lock (&random_lock);

  if (size == NULL && put == NULL && get == NULL)
    {
      /* From the standard: "If no argument is present, the processor assigns
         a processor-dependent value to the seed."  */

      for (i=0; i<kiss_size; i++)
	kiss_seed[i] = kiss_default_seed[i];

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

  __gthread_mutex_unlock (&random_lock);
}
iexport(random_seed);


#ifndef __GTHREAD_MUTEX_INIT
static void __attribute__((constructor))
init (void)
{
  __GTHREAD_MUTEX_INIT_FUNCTION (&random_lock);
}
#endif
