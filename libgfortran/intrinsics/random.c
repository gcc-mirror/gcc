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

#ifdef __GTHREAD_MUTEX_INIT
static __gthread_mutex_t random_lock = __GTHREAD_MUTEX_INIT;
#else
static __gthread_mutex_t random_lock;
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

  __gthread_mutex_lock (&random_lock);
  kiss = kiss_random_kernel ();
  /* Burn a random number, so the REAL*4 and REAL*8 functions
     produce similar sequences of random numbers.  */
  kiss_random_kernel ();
  *x = normalize_r4_i4 (kiss, ~(GFC_UINTEGER_4) 0);
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
  kiss = ((GFC_UINTEGER_8)kiss_random_kernel ()) << 32;
  kiss += kiss_random_kernel ();
  *x = normalize_r8_i8 (kiss, ~(GFC_UINTEGER_8) 0);
  __gthread_mutex_unlock (&random_lock);
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
      kiss = kiss_random_kernel ();
      /* Burn a random number, so the REAL*4 and REAL*8 functions
	 produce similar sequences of random numbers.  */
      kiss_random_kernel ();
      *dest = normalize_r4_i4 (kiss, ~(GFC_UINTEGER_4) 0);

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
      kiss = ((GFC_UINTEGER_8)kiss_random_kernel ()) << 32;
      kiss += kiss_random_kernel ();
      *dest = normalize_r8_i8 (kiss, ~(GFC_UINTEGER_8) 0);

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
