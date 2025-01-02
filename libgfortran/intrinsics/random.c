/* Implementation of the RANDOM intrinsics
   Copyright (C) 2002-2025 Free Software Foundation, Inc.
   Contributed by Lars Segerlund <seger@linuxmail.org>,
   Steve Kargl and Janne Blomqvist.

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Ligbfortran is distributed in the hope that it will be useful,
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

/* For rand_s.  */
#define _CRT_RAND_S

#include "libgfortran.h"
#include <gthr.h>
#include <string.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/stat.h>
#include <fcntl.h>
#include "time_1.h"
#ifdef HAVE_SYS_RANDOM_H
#include <sys/random.h>
#endif

#ifdef __MINGW32__
#define HAVE_GETPID 1
#include <process.h>
#include <_mingw.h> /* For __MINGW64_VERSION_MAJOR  */
#endif

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

#ifdef HAVE_GFC_REAL_17

extern void random_r17 (GFC_REAL_17 *);
iexport_proto(random_r17);

extern void arandom_r17 (gfc_array_r17 *);
export_proto(arandom_r17);

#endif

extern void random_m1 (GFC_UINTEGER_1 *);
export_proto (random_m1);

extern void random_m2 (GFC_UINTEGER_2 *);
export_proto (random_m2);

extern void random_m4 (GFC_UINTEGER_4 *);
export_proto (random_m4);

extern void random_m8 (GFC_UINTEGER_8 *);
export_proto (random_m8);

#ifdef  HAVE_GFC_UINTEGER_16
extern void random_m16 (GFC_UINTEGER_16 *);
export_proto (random_m16);

#endif

extern void arandom_m1 (gfc_array_m1 *);
export_proto (arandom_m1);

extern void arandom_m2 (gfc_array_m2 *);
export_proto (arandom_m2);

extern void arandom_m4 (gfc_array_m4 *);
export_proto (arandom_m4);

extern void arandom_m8 (gfc_array_m8 *);
export_proto (arandom_m8);

#ifdef HAVE_GFC_UINTEGER_16

extern void arandom_m16 (gfc_array_m16 *);
export_proto (arandom_m16);

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
   correct offset.  */


static void
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
  *f = (GFC_REAL_4) v * GFC_REAL_4_LITERAL(0x1.p-32);
}

static void
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
  *f = (GFC_REAL_8) v * GFC_REAL_8_LITERAL(0x1.p-64);
}

#ifdef HAVE_GFC_REAL_10

static void
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
  *f = (GFC_REAL_10) v * GFC_REAL_10_LITERAL(0x1.p-64);
}
#endif

#ifdef HAVE_GFC_REAL_16

/* For REAL(KIND=16), we only need to mask off the lower bits.  */

static void
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
  *f = (GFC_REAL_16) v1 * GFC_REAL_16_LITERAL(0x1.p-64)
    + (GFC_REAL_16) v2 * GFC_REAL_16_LITERAL(0x1.p-128);
}
#endif

#ifdef HAVE_GFC_REAL_17

/* For REAL(KIND=16), we only need to mask off the lower bits.  */

static void
rnumber_17 (GFC_REAL_17 *f, GFC_UINTEGER_8 v1, GFC_UINTEGER_8 v2)
{
  GFC_UINTEGER_8 mask;
#if GFC_REAL_17_RADIX == 2
  mask = ~ (GFC_UINTEGER_8) 0u << (128 - GFC_REAL_17_DIGITS);
#elif GFC_REAL_17_RADIX == 16
  mask = ~ (GFC_UINTEGER_8) 0u << ((32 - GFC_REAL_17_DIGITS) * 4);
#else
#error "GFC_REAL_17_RADIX has unknown value"
#endif
  v2 = v2 & mask;
  *f = (GFC_REAL_17) v1 * GFC_REAL_17_LITERAL(0x1.p-64)
    + (GFC_REAL_17) v2 * GFC_REAL_17_LITERAL(0x1.p-128);
}
#endif


/*

   We use the xoshiro256** generator, a fast high-quality generator
   that:

   - passes TestU1 without any failures

   - provides a "jump" function making it easy to provide many
     independent parallel streams.

   - Long period of 2**256 - 1

   A description can be found at

   http://prng.di.unimi.it/

   or

   https://arxiv.org/abs/1805.01407

   The paper includes public domain source code which is the basis for
   the implementation below.

*/
typedef struct
{
  bool init;
  uint64_t s[4];
}
prng_state;


/* master_state is the only variable protected by random_lock.  */
static prng_state master_state = { .init = false, .s = {
    0xad63fa1ed3b55f36ULL, 0xd94473e78978b497ULL, 0xbc60592a98172477ULL,
    0xa3de7c6e81265301ULL }
};


static __gthread_key_t rand_state_key;

static prng_state*
get_rand_state (void)
{
  /* For single threaded apps.  */
  static prng_state rand_state;

  if (__gthread_active_p ())
    {
      void* p = __gthread_getspecific (rand_state_key);
      if (!p)
	{
	  p = xcalloc (1, sizeof (prng_state));
	  __gthread_setspecific (rand_state_key, p);
	}
      return p;
    }
  else
    return &rand_state;
}

static inline uint64_t
rotl (const uint64_t x, int k)
{
	return (x << k) | (x >> (64 - k));
}


static uint64_t
prng_next (prng_state* rs)
{
  const uint64_t result = rotl(rs->s[1] * 5, 7) * 9;

  const uint64_t t = rs->s[1] << 17;

  rs->s[2] ^= rs->s[0];
  rs->s[3] ^= rs->s[1];
  rs->s[1] ^= rs->s[2];
  rs->s[0] ^= rs->s[3];

  rs->s[2] ^= t;

  rs->s[3] = rotl(rs->s[3], 45);

  return result;
}


/* This is the jump function for the generator. It is equivalent to
   2^128 calls to prng_next(); it can be used to generate 2^128
   non-overlapping subsequences for parallel computations. */

static void
jump (prng_state* rs)
{
  static const uint64_t JUMP[] = { 0x180ec6d33cfd0aba, 0xd5a61266f0c9392c, 0xa9582618e03fc9aa, 0x39abdc4529b1661c };

  uint64_t s0 = 0;
  uint64_t s1 = 0;
  uint64_t s2 = 0;
  uint64_t s3 = 0;
  for(size_t i = 0; i < sizeof JUMP / sizeof *JUMP; i++)
    for(int b = 0; b < 64; b++) {
      if (JUMP[i] & UINT64_C(1) << b) {
	s0 ^= rs->s[0];
	s1 ^= rs->s[1];
	s2 ^= rs->s[2];
	s3 ^= rs->s[3];
      }
      prng_next (rs);
    }

  rs->s[0] = s0;
  rs->s[1] = s1;
  rs->s[2] = s2;
  rs->s[3] = s3;
}


/* Splitmix64 recommended by xoshiro author for initializing.  After
   getting one uint64_t value from the OS, this is used to fill in the
   rest of the xoshiro state.  */

static uint64_t
splitmix64 (uint64_t x)
{
  uint64_t z = (x += 0x9e3779b97f4a7c15);
  z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9;
  z = (z ^ (z >> 27)) * 0x94d049bb133111eb;
  return z ^ (z >> 31);
}


/* Get some bytes from the operating system in order to seed
   the PRNG.  */

static int
getosrandom (void *buf, size_t buflen)
{
  /* rand_s is available in MinGW-w64 but not plain MinGW.  */
#if defined(__MINGW64_VERSION_MAJOR)
  unsigned int* b = buf;
  for (size_t i = 0; i < buflen / sizeof (unsigned int); i++)
    rand_s (&b[i]);
  return buflen;
#else
#ifdef HAVE_GETENTROPY
  if (getentropy (buf, buflen) == 0)
    return buflen;
#endif
  int flags = O_RDONLY;
#ifdef O_CLOEXEC
  flags |= O_CLOEXEC;
#endif
  int fd = open("/dev/urandom", flags);
  if (fd != -1)
    {
      int res = read(fd, buf, buflen);
      close (fd);
      return res;
    }
  uint64_t seed = 0x047f7684e9fc949dULL;
  time_t secs;
  long usecs;
  if (gf_gettime (&secs, &usecs) == 0)
    {
      seed ^= secs;
      seed ^= usecs;
    }
#ifdef HAVE_GETPID
  pid_t pid = getpid();
  seed ^= pid;
#endif
  size_t size = buflen < sizeof (uint64_t) ? buflen : sizeof (uint64_t);
  memcpy (buf, &seed, size);
  return size;
#endif /* __MINGW64_VERSION_MAJOR  */
}


/* Initialize the random number generator for the current thread,
   using the master state and the number of times we must jump.  */

static void
init_rand_state (prng_state* rs, const bool locked)
{
  if (!locked)
    __gthread_mutex_lock (&random_lock);
  if (!master_state.init)
    {
      uint64_t os_seed;
      getosrandom (&os_seed, sizeof (os_seed));
      for (uint64_t i = 0; i < sizeof (master_state.s) / sizeof (uint64_t); i++)
	{
          os_seed = splitmix64 (os_seed);
          master_state.s[i] = os_seed;
        }
      master_state.init = true;
    }
  memcpy (&rs->s, master_state.s, sizeof (master_state.s));
  jump (&master_state);
  if (!locked)
    __gthread_mutex_unlock (&random_lock);
  rs->init = true;
}


/*  This function produces a REAL(4) value from the uniform distribution
    with range [0,1).  */

void
random_r4 (GFC_REAL_4 *x)
{
  prng_state* rs = get_rand_state();

  if (unlikely (!rs->init))
    init_rand_state (rs, false);
  uint64_t r = prng_next (rs);
  /* Take the higher bits, ensuring that a stream of real(4), real(8),
     and real(10) will be identical (except for precision).  */
  uint32_t high = (uint32_t) (r >> 32);
  rnumber_4 (x, high);
}
iexport(random_r4);

/*  This function produces a REAL(8) value from the uniform distribution
    with range [0,1).  */

void
random_r8 (GFC_REAL_8 *x)
{
  GFC_UINTEGER_8 r;
  prng_state* rs = get_rand_state();

  if (unlikely (!rs->init))
    init_rand_state (rs, false);
  r = prng_next (rs);
  rnumber_8 (x, r);
}
iexport(random_r8);

#ifdef HAVE_GFC_REAL_10

/*  This function produces a REAL(10) value from the uniform distribution
    with range [0,1).  */

void
random_r10 (GFC_REAL_10 *x)
{
  GFC_UINTEGER_8 r;
  prng_state* rs = get_rand_state();

  if (unlikely (!rs->init))
    init_rand_state (rs, false);
  r = prng_next (rs);
  rnumber_10 (x, r);
}
iexport(random_r10);

#endif

/*  This function produces a REAL(16) value from the uniform distribution
    with range [0,1).  */

#ifdef HAVE_GFC_REAL_16

void
random_r16 (GFC_REAL_16 *x)
{
  GFC_UINTEGER_8 r1, r2;
  prng_state* rs = get_rand_state();

  if (unlikely (!rs->init))
    init_rand_state (rs, false);
  r1 = prng_next (rs);
  r2 = prng_next (rs);
  rnumber_16 (x, r1, r2);
}
iexport(random_r16);


#endif

/*  This function produces a REAL(16) value from the uniform distribution
    with range [0,1).  */

#ifdef HAVE_GFC_REAL_17

void
random_r17 (GFC_REAL_17 *x)
{
  GFC_UINTEGER_8 r1, r2;
  prng_state* rs = get_rand_state();

  if (unlikely (!rs->init))
    init_rand_state (rs, false);
  r1 = prng_next (rs);
  r2 = prng_next (rs);
  rnumber_17 (x, r1, r2);
}
iexport(random_r17);


#endif

/* Versions for unsigned numbers.  */

/* Returns a random byte.  */

void
random_m1 (GFC_UINTEGER_1 *x)
{
  prng_state* rs = get_rand_state();

  if (unlikely (!rs->init))
    init_rand_state (rs, false);
  GFC_UINTEGER_8 r = prng_next (rs);

  *x = r >> 56;
}

/* A random 16-bit number.  */

void
random_m2 (GFC_UINTEGER_2 *x)
{
  prng_state* rs = get_rand_state();

  if (unlikely (!rs->init))
    init_rand_state (rs, false);
  GFC_UINTEGER_8 r = prng_next (rs);

  *x = r >> 48;
}

/* A random 32-bit number.  */

void
random_m4 (GFC_UINTEGER_4 *x)
{
  prng_state* rs = get_rand_state();

  if (unlikely (!rs->init))
    init_rand_state (rs, false);
  GFC_UINTEGER_8 r = prng_next (rs);

  *x = r >> 32;
}

/* A random 64-bit number.  */

void
random_m8 (GFC_UINTEGER_8 *x)
{
  prng_state* rs = get_rand_state();

  if (unlikely (!rs->init))
    init_rand_state (rs, false);
  GFC_UINTEGER_8 r = prng_next (rs);

  *x = r;
}

/* ... and a random 128-bit number, if we have the type.  */

#ifdef HAVE_GFC_UINTEGER_16
void
random_m16 (GFC_UINTEGER_16 *x)
{
  prng_state* rs = get_rand_state();

  if (unlikely (!rs->init))
    init_rand_state (rs, false);
  GFC_UINTEGER_8 r1 = prng_next (rs);
  GFC_UINTEGER_8 r2 = prng_next (rs);

  *x = (((GFC_UINTEGER_16) r1) << 64) | (GFC_UINTEGER_16) r2;
}
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
  prng_state* rs = get_rand_state();

  dest = x->base_addr;

  dim = GFC_DESCRIPTOR_RANK (x);

  for (index_type n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE(x,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(x,n);
      if (extent[n] <= 0)
        return;
    }

  stride0 = stride[0];

  if (unlikely (!rs->init))
    init_rand_state (rs, false);

  while (dest)
    {
      /* random_r4 (dest);  */
      uint64_t r = prng_next (rs);
      uint32_t high = (uint32_t) (r >> 32);
      rnumber_4 (dest, high);

      /* Advance to the next element.  */
      dest += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      index_type n = 0;
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
  prng_state* rs = get_rand_state();

  dest = x->base_addr;

  dim = GFC_DESCRIPTOR_RANK (x);

  for (index_type n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE(x,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(x,n);
      if (extent[n] <= 0)
        return;
    }

  stride0 = stride[0];

  if (unlikely (!rs->init))
    init_rand_state (rs, false);

  while (dest)
    {
      /* random_r8 (dest);  */
      uint64_t r = prng_next (rs);
      rnumber_8 (dest, r);

      /* Advance to the next element.  */
      dest += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      index_type n = 0;
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
  prng_state* rs = get_rand_state();

  dest = x->base_addr;

  dim = GFC_DESCRIPTOR_RANK (x);

  for (index_type n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE(x,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(x,n);
      if (extent[n] <= 0)
        return;
    }

  stride0 = stride[0];

  if (unlikely (!rs->init))
    init_rand_state (rs, false);

  while (dest)
    {
      /* random_r10 (dest);  */
      uint64_t r = prng_next (rs);
      rnumber_10 (dest, r);

      /* Advance to the next element.  */
      dest += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      index_type n = 0;
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
  prng_state* rs = get_rand_state();

  dest = x->base_addr;

  dim = GFC_DESCRIPTOR_RANK (x);

  for (index_type n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE(x,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(x,n);
      if (extent[n] <= 0)
        return;
    }

  stride0 = stride[0];

  if (unlikely (!rs->init))
    init_rand_state (rs, false);

  while (dest)
    {
      /* random_r16 (dest);  */
      uint64_t r1 = prng_next (rs);
      uint64_t r2 = prng_next (rs);
      rnumber_16 (dest, r1, r2);

      /* Advance to the next element.  */
      dest += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      index_type n = 0;
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

#endif

#ifdef HAVE_GFC_REAL_17

/*  This function fills a REAL(16) array with values from the uniform
    distribution with range [0,1).  */

void
arandom_r17 (gfc_array_r17 *x)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  GFC_REAL_17 *dest;
  prng_state* rs = get_rand_state();

  dest = x->base_addr;

  dim = GFC_DESCRIPTOR_RANK (x);

  for (index_type n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE(x,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(x,n);
      if (extent[n] <= 0)
        return;
    }

  stride0 = stride[0];

  if (unlikely (!rs->init))
    init_rand_state (rs, false);

  while (dest)
    {
      /* random_r17 (dest);  */
      uint64_t r1 = prng_next (rs);
      uint64_t r2 = prng_next (rs);
      rnumber_17 (dest, r1, r2);

      /* Advance to the next element.  */
      dest += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      index_type n = 0;
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

#endif

/* Fill an unsigned array with random bytes.  */

void
arandom_m1 (gfc_array_m1 *x)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  GFC_UINTEGER_1 *dest;
  prng_state* rs = get_rand_state();

  dest = x->base_addr;

  dim = GFC_DESCRIPTOR_RANK (x);

  for (index_type n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE(x,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(x,n);
      if (extent[n] <= 0)
	return;
    }

  stride0 = stride[0];

  if (unlikely (!rs->init))
    init_rand_state (rs, false);

  while (dest)
    {
      /* random_m1 (dest);  */
      uint64_t r = prng_next (rs);
      *dest = r >> 56;

      /* Advance to the next element.  */
      dest += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      index_type n = 0;
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

/* Fill an unsigned array with random 16-bit unsigneds.  */

void
arandom_m2 (gfc_array_m2 *x)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  GFC_UINTEGER_2 *dest;
  prng_state* rs = get_rand_state();

  dest = x->base_addr;

  dim = GFC_DESCRIPTOR_RANK (x);

  for (index_type n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE(x,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(x,n);
      if (extent[n] <= 0)
	return;
    }

  stride0 = stride[0];

  if (unlikely (!rs->init))
    init_rand_state (rs, false);

  while (dest)
    {
      /* random_m1 (dest);  */
      uint64_t r = prng_next (rs);
      *dest = r >> 48;

      /* Advance to the next element.  */
      dest += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      index_type n = 0;
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

/* Fill an array with random 32-bit unsigneds.  */

void
arandom_m4 (gfc_array_m4 *x)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  GFC_UINTEGER_4 *dest;
  prng_state* rs = get_rand_state();

  dest = x->base_addr;

  dim = GFC_DESCRIPTOR_RANK (x);

  for (index_type n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE(x,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(x,n);
      if (extent[n] <= 0)
	return;
    }

  stride0 = stride[0];

  if (unlikely (!rs->init))
    init_rand_state (rs, false);

  while (dest)
    {
      /* random_m4 (dest);  */
      uint64_t r = prng_next (rs);
      *dest = r >> 32;

      /* Advance to the next element.  */
      dest += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      index_type n = 0;
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

/* Fill an array with random 64-bit unsigneds.  */

void
arandom_m8 (gfc_array_m8 *x)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  GFC_UINTEGER_8 *dest;
  prng_state* rs = get_rand_state();

  dest = x->base_addr;

  dim = GFC_DESCRIPTOR_RANK (x);

  for (index_type n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE(x,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(x,n);
      if (extent[n] <= 0)
	return;
    }

  stride0 = stride[0];

  if (unlikely (!rs->init))
    init_rand_state (rs, false);

  while (dest)
    {
      /* random_m8 (dest);  */
      uint64_t r = prng_next (rs);
      *dest = r;

      /* Advance to the next element.  */
      dest += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      index_type n = 0;
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

#ifdef GFC_HAVE_GFC_UINTEGER_16

/* Fill an unsigned array with random bytes.  */

void
arandom_m16 (gfc_array_m16 *x)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  GFC_UINTEGER_16 *dest;
  prng_state* rs = get_rand_state();

  dest = x->base_addr;

  dim = GFC_DESCRIPTOR_RANK (x);

  for (index_type n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE(x,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(x,n);
      if (extent[n] <= 0)
	return;
    }

  stride0 = stride[0];

  if (unlikely (!rs->init))
    init_rand_state (rs, false);

  while (dest)
    {
      /* random_m16 (dest);  */
      uint64_t r1 = prng_next (rs), r2 = prng_next (rs);
      *dest = (((GFC_UINTEGER_16) r1) << 64) | (GFC_UINTEGER_16) r2;

      /* Advance to the next element.  */
      dest += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      index_type n = 0;
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

#endif

/* Number of elements in master_state array.  */
#define SZU64 (sizeof (master_state.s) / sizeof (uint64_t))

/* Equivalent number of elements in an array of GFC_INTEGER_{4,8}.  */
#define SZ_IN_INT_4 (SZU64 * (sizeof (uint64_t) / sizeof (GFC_INTEGER_4)))
#define SZ_IN_INT_8 (SZU64 * (sizeof (uint64_t) / sizeof (GFC_INTEGER_8)))

/* Keys for scrambling the seed in order to avoid poor seeds.  */

static const uint64_t xor_keys[] = {
  0xbd0c5b6e50c2df49ULL, 0xd46061cd46e1df38ULL, 0xbb4f4d4ed6103544ULL,
  0x114a583d0756ad39ULL
};


/* Since a XOR cipher is symmetric, we need only one routine, and we
   can use it both for encryption and decryption.  */

static void
scramble_seed (uint64_t *dest, const uint64_t *src)
{
  for (size_t i = 0; i < SZU64; i++)
    dest[i] = src[i] ^ xor_keys[i];
}


/* random_seed is used to seed the PRNG with either a default
   set of seeds or user specified set of seeds.  random_seed
   must be called with no argument or exactly one argument.  */

void
random_seed_i4 (GFC_INTEGER_4 *size, gfc_array_i4 *put, gfc_array_i4 *get)
{
  uint64_t seed[SZU64];

  /* Check that we only have one argument present.  */
  if ((size ? 1 : 0) + (put ? 1 : 0) + (get ? 1 : 0) > 1)
    runtime_error ("RANDOM_SEED should have at most one argument present.");

  if (size != NULL)
    *size = SZ_IN_INT_4;

  prng_state* rs = get_rand_state();

  /* Return the seed to GET data.  */
  if (get != NULL)
    {
      /* If the rank of the array is not 1, abort.  */
      if (GFC_DESCRIPTOR_RANK (get) != 1)
	runtime_error ("Array rank of GET is not 1.");

      /* If the array is too small, abort.  */
      if (GFC_DESCRIPTOR_EXTENT(get,0) < (index_type) SZ_IN_INT_4)
	runtime_error ("Array size of GET is too small.");

      if (!rs->init)
	init_rand_state (rs, false);

      /* Unscramble the seed.  */
      scramble_seed (seed, rs->s);

      /*  Then copy it back to the user variable.  */
      for (size_t i = 0; i < SZ_IN_INT_4 ; i++)
	memcpy (&(get->base_addr[(SZ_IN_INT_4 - 1 - i) *
				 GFC_DESCRIPTOR_STRIDE(get,0)]),
		(unsigned char*) seed + i * sizeof(GFC_UINTEGER_4),
               sizeof(GFC_UINTEGER_4));
    }

  else
    {
  __gthread_mutex_lock (&random_lock);

  /* From the standard: "If no argument is present, the processor assigns
     a processor-dependent value to the seed."  */
  if (size == NULL && put == NULL && get == NULL)
    {
      master_state.init = false;
      init_rand_state (rs, true);
    }

  if (put != NULL)
    {
      /* If the rank of the array is not 1, abort.  */
      if (GFC_DESCRIPTOR_RANK (put) != 1)
        runtime_error ("Array rank of PUT is not 1.");

      /* If the array is too small, abort.  */
      if (GFC_DESCRIPTOR_EXTENT(put,0) < (index_type) SZ_IN_INT_4)
        runtime_error ("Array size of PUT is too small.");

      /*  We copy the seed given by the user.  */
      for (size_t i = 0; i < SZ_IN_INT_4; i++)
	memcpy ((unsigned char*) seed + i * sizeof(GFC_UINTEGER_4),
		&(put->base_addr[(SZ_IN_INT_4 - 1 - i) *
				 GFC_DESCRIPTOR_STRIDE(put,0)]),
		sizeof(GFC_UINTEGER_4));

      /* We put it after scrambling the bytes, to paper around users who
	 provide seeds with quality only in the lower or upper part.  */
      scramble_seed (master_state.s, seed);
      master_state.init = true;
      init_rand_state (rs, true);
    }

  __gthread_mutex_unlock (&random_lock);
    }
}
iexport(random_seed_i4);


void
random_seed_i8 (GFC_INTEGER_8 *size, gfc_array_i8 *put, gfc_array_i8 *get)
{
  uint64_t seed[SZU64];

  /* Check that we only have one argument present.  */
  if ((size ? 1 : 0) + (put ? 1 : 0) + (get ? 1 : 0) > 1)
    runtime_error ("RANDOM_SEED should have at most one argument present.");

  if (size != NULL)
    *size = SZ_IN_INT_8;

  prng_state* rs = get_rand_state();

  /* Return the seed to GET data.  */
  if (get != NULL)
    {
      /* If the rank of the array is not 1, abort.  */
      if (GFC_DESCRIPTOR_RANK (get) != 1)
	runtime_error ("Array rank of GET is not 1.");

      /* If the array is too small, abort.  */
      if (GFC_DESCRIPTOR_EXTENT(get,0) < (index_type) SZ_IN_INT_8)
	runtime_error ("Array size of GET is too small.");

      if (!rs->init)
	init_rand_state (rs, false);

      /* Unscramble the seed.  */
      scramble_seed (seed, rs->s);

      /*  This code now should do correct strides.  */
      for (size_t i = 0; i < SZ_IN_INT_8; i++)
	memcpy (&(get->base_addr[i * GFC_DESCRIPTOR_STRIDE(get,0)]), &seed[i],
		sizeof (GFC_UINTEGER_8));
    }

  else
    {
  __gthread_mutex_lock (&random_lock);

  /* From the standard: "If no argument is present, the processor assigns
     a processor-dependent value to the seed."  */
  if (size == NULL && put == NULL && get == NULL)
    {
      master_state.init = false;
      init_rand_state (rs, true);
    }

  if (put != NULL)
    {
      /* If the rank of the array is not 1, abort.  */
      if (GFC_DESCRIPTOR_RANK (put) != 1)
        runtime_error ("Array rank of PUT is not 1.");

      /* If the array is too small, abort.  */
      if (GFC_DESCRIPTOR_EXTENT(put,0) < (index_type) SZ_IN_INT_8)
        runtime_error ("Array size of PUT is too small.");

      /*  This code now should do correct strides.  */
      for (size_t i = 0; i < SZ_IN_INT_8; i++)
	memcpy (&seed[i], &(put->base_addr[i * GFC_DESCRIPTOR_STRIDE(put,0)]),
		sizeof (GFC_UINTEGER_8));

      scramble_seed (master_state.s, seed);
      master_state.init = true;
      init_rand_state (rs, true);
     }


  __gthread_mutex_unlock (&random_lock);
    }
}
iexport(random_seed_i8);


#if !defined __GTHREAD_MUTEX_INIT || defined __GTHREADS
static void __attribute__((constructor))
constructor_random (void)
{
#ifndef __GTHREAD_MUTEX_INIT
  __GTHREAD_MUTEX_INIT_FUNCTION (&random_lock);
#endif
  if (__gthread_active_p ())
    __gthread_key_create (&rand_state_key, &free);
}
#endif

#ifdef __GTHREADS
static void __attribute__((destructor))
destructor_random (void)
{
  if (__gthread_active_p ())
    __gthread_key_delete (rand_state_key);
}
#endif
