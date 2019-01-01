/* Implementation of the RANDOM intrinsics
   Copyright (C) 2002-2019 Free Software Foundation, Inc.
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


/*

   We use the xorshift1024* generator, a fast high-quality generator
   that:

   - passes TestU1 without any failures

   - provides a "jump" function making it easy to provide many
     independent parallel streams.

   - Long period of 2**1024 - 1

   A description can be found at

   http://vigna.di.unimi.it/ftp/papers/xorshift.pdf

   or

   http://arxiv.org/abs/1402.6246

   The paper includes public domain source code which is the basis for
   the implementation below.

*/
typedef struct
{
  bool init;
  int p;
  uint64_t s[16];
}
xorshift1024star_state;


/* master_init, njumps, and master_state are the only variables
   protected by random_lock.  */
static bool master_init;
static unsigned njumps; /* How many times we have jumped.  */
static uint64_t master_state[] = {
  0xad63fa1ed3b55f36ULL, 0xd94473e78978b497ULL, 0xbc60592a98172477ULL,
  0xa3de7c6e81265301ULL, 0x586640c5e785af27ULL, 0x7a2a3f63b67ce5eaULL,
  0x9fde969f922d9b82ULL, 0xe6fe34379b3f3822ULL, 0x6c277eac3e99b6c2ULL,
  0x9197290ab0d3f069ULL, 0xdb227302f6c25576ULL, 0xee0209aee527fae9ULL,
  0x675666a793cd05b9ULL, 0xd048c99fbc70c20fULL, 0x775f8c3dba385ef5ULL,
  0x625288bc262faf33ULL
};


static __gthread_key_t rand_state_key;

static xorshift1024star_state*
get_rand_state (void)
{
  /* For single threaded apps.  */
  static xorshift1024star_state rand_state;

  if (__gthread_active_p ())
    {
      void* p = __gthread_getspecific (rand_state_key);
      if (!p)
	{
	  p = xcalloc (1, sizeof (xorshift1024star_state));
	  __gthread_setspecific (rand_state_key, p);
	}
      return p;
    }
  else
    return &rand_state;
}


static uint64_t
xorshift1024star (xorshift1024star_state* rs)
{
  int p = rs->p;
  const uint64_t s0 = rs->s[p];
  uint64_t s1 = rs->s[p = (p + 1) & 15];
  s1 ^= s1 << 31;
  rs->s[p] = s1 ^ s0 ^ (s1 >> 11) ^ (s0 >> 30);
  rs->p = p;
  return rs->s[p] * UINT64_C(1181783497276652981);
}


/* This is the jump function for the generator. It is equivalent to
   2^512 calls to xorshift1024star(); it can be used to generate 2^512
   non-overlapping subsequences for parallel computations. */

static void
jump (xorshift1024star_state* rs)
{
  static const uint64_t JUMP[] = {
    0x84242f96eca9c41dULL, 0xa3c65b8776f96855ULL, 0x5b34a39f070b5837ULL,
    0x4489affce4f31a1eULL, 0x2ffeeb0a48316f40ULL, 0xdc2d9891fe68c022ULL,
    0x3659132bb12fea70ULL, 0xaac17d8efa43cab8ULL, 0xc4cb815590989b13ULL,
    0x5ee975283d71c93bULL, 0x691548c86c1bd540ULL, 0x7910c41d10a1e6a5ULL,
    0x0b5fc64563b3e2a8ULL, 0x047f7684e9fc949dULL, 0xb99181f2d8f685caULL,
    0x284600e3f30e38c3ULL
  };

  uint64_t t[16] = { 0 };
  for(size_t i = 0; i < sizeof JUMP / sizeof *JUMP; i++)
    for(int b = 0; b < 64; b++)
      {
	if (JUMP[i] & 1ULL << b)
	  for(int j = 0; j < 16; j++)
	    t[j] ^= rs->s[(j + rs->p) & 15];
	xorshift1024star (rs);
      }
  for(int j = 0; j < 16; j++)
    rs->s[(j + rs->p) & 15] = t[j];
}


/* Super-simple LCG generator used in getosrandom () if /dev/urandom
   doesn't exist.  */

#define M 2147483647 /* 2^31 - 1 (A large prime number) */
#define A 16807      /* Prime root of M, passes statistical tests and produces a full cycle */
#define Q 127773 /* M / A (To avoid overflow on A * seed) */
#define R 2836   /* M % A (To avoid overflow on A * seed) */

__attribute__((unused)) static uint32_t
lcg_parkmiller(uint32_t seed)
{
    uint32_t hi = seed / Q;
    uint32_t lo = seed % Q;
    int32_t test = A * lo - R * hi;
    if (test <= 0)
        test += M;
    return test;
}

#undef M
#undef A
#undef Q
#undef R


/* Get some random bytes from the operating system in order to seed
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
    return 0;
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
  uint32_t seed = 1234567890;
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
  uint32_t* ub = buf;
  for (size_t i = 0; i < buflen / sizeof (uint32_t); i++)
    {
      ub[i] = seed;
      seed = lcg_parkmiller (seed);
    }
  return buflen;
#endif /* __MINGW64_VERSION_MAJOR  */
}


/* Initialize the random number generator for the current thread,
   using the master state and the number of times we must jump.  */

static void
init_rand_state (xorshift1024star_state* rs, const bool locked)
{
  if (!locked)
    __gthread_mutex_lock (&random_lock);
  if (!master_init)
    {
      getosrandom (master_state, sizeof (master_state));
      njumps = 0;
      master_init = true;
    }
  memcpy (&rs->s, master_state, sizeof (master_state));
  unsigned n = njumps++;
  if (!locked)
    __gthread_mutex_unlock (&random_lock);
  for (unsigned i = 0; i < n; i++)
    jump (rs);
  rs->init = true;
}


/*  This function produces a REAL(4) value from the uniform distribution
    with range [0,1).  */

void
random_r4 (GFC_REAL_4 *x)
{
  xorshift1024star_state* rs = get_rand_state();

  if (unlikely (!rs->init))
    init_rand_state (rs, false);
  uint64_t r = xorshift1024star (rs);
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
  xorshift1024star_state* rs = get_rand_state();

  if (unlikely (!rs->init))
    init_rand_state (rs, false);
  r = xorshift1024star (rs);
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
  xorshift1024star_state* rs = get_rand_state();

  if (unlikely (!rs->init))
    init_rand_state (rs, false);
  r = xorshift1024star (rs);
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
  xorshift1024star_state* rs = get_rand_state();

  if (unlikely (!rs->init))
    init_rand_state (rs, false);
  r1 = xorshift1024star (rs);
  r2 = xorshift1024star (rs);
  rnumber_16 (x, r1, r2);
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
  xorshift1024star_state* rs = get_rand_state();

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
      uint64_t r = xorshift1024star (rs);
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
  xorshift1024star_state* rs = get_rand_state();

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
      uint64_t r = xorshift1024star (rs);
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
  xorshift1024star_state* rs = get_rand_state();

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
      uint64_t r = xorshift1024star (rs);
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
  xorshift1024star_state* rs = get_rand_state();

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
      uint64_t r1 = xorshift1024star (rs);
      uint64_t r2 = xorshift1024star (rs);
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


/* Number of elements in master_state array.  */
#define SZU64 (sizeof (master_state) / sizeof (uint64_t))


/* Keys for scrambling the seed in order to avoid poor seeds.  */

static const uint64_t xor_keys[] = {
  0xbd0c5b6e50c2df49ULL, 0xd46061cd46e1df38ULL, 0xbb4f4d4ed6103544ULL,
  0x114a583d0756ad39ULL, 0x4b5ad8623d0aaab6ULL, 0x3f2ed7afbe0c0f21ULL,
  0xdec83fd65f113445ULL, 0x3824f8fbc4f10d24ULL, 0x5d9025af05878911ULL,
  0x500bc46b540340e9ULL, 0x8bd53298e0d00530ULL, 0x57886e40a952e06aULL,
  0x926e76c88e31cdb6ULL, 0xbd0724dac0a3a5f9ULL, 0xc5c8981b858ab796ULL,
  0xbb12ab2694c2b32cULL
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
#define SZ (sizeof (master_state) / sizeof (GFC_INTEGER_4))

  /* Check that we only have one argument present.  */
  if ((size ? 1 : 0) + (put ? 1 : 0) + (get ? 1 : 0) > 1)
    runtime_error ("RANDOM_SEED should have at most one argument present.");

  if (size != NULL)
    *size = SZ + 1;

  xorshift1024star_state* rs = get_rand_state();

  /* Return the seed to GET data.  */
  if (get != NULL)
    {
      /* If the rank of the array is not 1, abort.  */
      if (GFC_DESCRIPTOR_RANK (get) != 1)
	runtime_error ("Array rank of GET is not 1.");

      /* If the array is too small, abort.  */
      if (GFC_DESCRIPTOR_EXTENT(get,0) < (index_type) SZ + 1)
	runtime_error ("Array size of GET is too small.");

      if (!rs->init)
	init_rand_state (rs, false);

      /* Unscramble the seed.  */
      scramble_seed (seed, rs->s);

      /*  Then copy it back to the user variable.  */
      for (size_t i = 0; i < SZ ; i++)
	memcpy (&(get->base_addr[(SZ - 1 - i) * GFC_DESCRIPTOR_STRIDE(get,0)]),
		(unsigned char*) seed + i * sizeof(GFC_UINTEGER_4),
               sizeof(GFC_UINTEGER_4));

      /* Finally copy the value of p after the seed.  */
      get->base_addr[SZ * GFC_DESCRIPTOR_STRIDE(get, 0)] = rs->p;
    }

  else
    {
  __gthread_mutex_lock (&random_lock);

  /* From the standard: "If no argument is present, the processor assigns
     a processor-dependent value to the seed."  */
  if (size == NULL && put == NULL && get == NULL)
    {
      master_init = false;
      init_rand_state (rs, true);
    }

  if (put != NULL)
    {
      /* If the rank of the array is not 1, abort.  */
      if (GFC_DESCRIPTOR_RANK (put) != 1)
        runtime_error ("Array rank of PUT is not 1.");

      /* If the array is too small, abort.  */
      if (GFC_DESCRIPTOR_EXTENT(put,0) < (index_type) SZ + 1)
        runtime_error ("Array size of PUT is too small.");

      /*  We copy the seed given by the user.  */
      for (size_t i = 0; i < SZ; i++)
	memcpy ((unsigned char*) seed + i * sizeof(GFC_UINTEGER_4),
		&(put->base_addr[(SZ - 1 - i) * GFC_DESCRIPTOR_STRIDE(put,0)]),
		sizeof(GFC_UINTEGER_4));

      /* We put it after scrambling the bytes, to paper around users who
	 provide seeds with quality only in the lower or upper part.  */
      scramble_seed (master_state, seed);
      njumps = 0;
      master_init = true;
      init_rand_state (rs, true);

      rs->p = put->base_addr[SZ * GFC_DESCRIPTOR_STRIDE(put, 0)] & 15;
    }

  __gthread_mutex_unlock (&random_lock);
    }
#undef SZ
}
iexport(random_seed_i4);


void
random_seed_i8 (GFC_INTEGER_8 *size, gfc_array_i8 *put, gfc_array_i8 *get)
{
  uint64_t seed[SZU64];

  /* Check that we only have one argument present.  */
  if ((size ? 1 : 0) + (put ? 1 : 0) + (get ? 1 : 0) > 1)
    runtime_error ("RANDOM_SEED should have at most one argument present.");

#define SZ (sizeof (master_state) / sizeof (GFC_INTEGER_8))
  if (size != NULL)
    *size = SZ + 1;

  xorshift1024star_state* rs = get_rand_state();

  /* Return the seed to GET data.  */
  if (get != NULL)
    {
      /* If the rank of the array is not 1, abort.  */
      if (GFC_DESCRIPTOR_RANK (get) != 1)
	runtime_error ("Array rank of GET is not 1.");

      /* If the array is too small, abort.  */
      if (GFC_DESCRIPTOR_EXTENT(get,0) < (index_type) SZ + 1)
	runtime_error ("Array size of GET is too small.");

      if (!rs->init)
	init_rand_state (rs, false);

      /* Unscramble the seed.  */
      scramble_seed (seed, rs->s);

      /*  This code now should do correct strides.  */
      for (size_t i = 0; i < SZ; i++)
	memcpy (&(get->base_addr[i * GFC_DESCRIPTOR_STRIDE(get,0)]), &seed[i],
		sizeof (GFC_UINTEGER_8));

      get->base_addr[SZ * GFC_DESCRIPTOR_STRIDE(get, 0)] = rs->p;
    }

  else
    {
  __gthread_mutex_lock (&random_lock);

  /* From the standard: "If no argument is present, the processor assigns
     a processor-dependent value to the seed."  */
  if (size == NULL && put == NULL && get == NULL)
    {
      master_init = false;
      init_rand_state (rs, true);
    }

  if (put != NULL)
    {
      /* If the rank of the array is not 1, abort.  */
      if (GFC_DESCRIPTOR_RANK (put) != 1)
        runtime_error ("Array rank of PUT is not 1.");

      /* If the array is too small, abort.  */
      if (GFC_DESCRIPTOR_EXTENT(put,0) < (index_type) SZ + 1)
        runtime_error ("Array size of PUT is too small.");

      /*  This code now should do correct strides.  */
      for (size_t i = 0; i < SZ; i++)
	memcpy (&seed[i], &(put->base_addr[i * GFC_DESCRIPTOR_STRIDE(put,0)]),
		sizeof (GFC_UINTEGER_8));

      scramble_seed (master_state, seed);
      njumps = 0;
      master_init = true;
      init_rand_state (rs, true);
      rs->p = put->base_addr[SZ * GFC_DESCRIPTOR_STRIDE(put, 0)] & 15;
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
