/* More subroutines needed by GCC output code on some machines.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"

#ifdef HAVE_GAS_HIDDEN
#define ATTRIBUTE_HIDDEN  __attribute__ ((__visibility__ ("hidden")))
#else
#define ATTRIBUTE_HIDDEN
#endif

/* Work out the largest "word" size that we can deal with on this target.  */
#if MIN_UNITS_PER_WORD > 4
# define LIBGCC2_MAX_UNITS_PER_WORD 8
#elif (MIN_UNITS_PER_WORD > 2 \
       || (MIN_UNITS_PER_WORD > 1 && __SIZEOF_LONG_LONG__ > 4))
# define LIBGCC2_MAX_UNITS_PER_WORD 4
#else
# define LIBGCC2_MAX_UNITS_PER_WORD MIN_UNITS_PER_WORD
#endif

/* Work out what word size we are using for this compilation.
   The value can be set on the command line.  */
#ifndef LIBGCC2_UNITS_PER_WORD
#define LIBGCC2_UNITS_PER_WORD LIBGCC2_MAX_UNITS_PER_WORD
#endif

#if LIBGCC2_UNITS_PER_WORD <= LIBGCC2_MAX_UNITS_PER_WORD

#include "libgcc2.h"

#ifdef DECLARE_LIBRARY_RENAMES
  DECLARE_LIBRARY_RENAMES
#endif

#if defined (L_negdi2)
DWtype
__negdi2 (DWtype u)
{
  const DWunion uu = {.ll = u};
  const DWunion w = { {.low = -uu.s.low,
		       .high = -uu.s.high - ((UWtype) -uu.s.low > 0) } };

  return w.ll;
}
#endif

#ifdef L_addvsi3
Wtype
__addvSI3 (Wtype a, Wtype b)
{
  Wtype w;

  if (__builtin_add_overflow (a, b, &w))
    abort ();

  return w;
}
#ifdef COMPAT_SIMODE_TRAPPING_ARITHMETIC
SItype
__addvsi3 (SItype a, SItype b)
{
  SItype w;

  if (__builtin_add_overflow (a, b, &w))
    abort ();

  return w;
}
#endif /* COMPAT_SIMODE_TRAPPING_ARITHMETIC */
#endif

#ifdef L_addvdi3
DWtype
__addvDI3 (DWtype a, DWtype b)
{
  DWtype w;

  if (__builtin_add_overflow (a, b, &w))
    abort ();

  return w;
}
#endif

#ifdef L_subvsi3
Wtype
__subvSI3 (Wtype a, Wtype b)
{
  Wtype w;

  if (__builtin_sub_overflow (a, b, &w))
    abort ();

  return w;
}
#ifdef COMPAT_SIMODE_TRAPPING_ARITHMETIC
SItype
__subvsi3 (SItype a, SItype b)
{
  SItype w;

  if (__builtin_sub_overflow (a, b, &w))
    abort ();

  return w;
}
#endif /* COMPAT_SIMODE_TRAPPING_ARITHMETIC */
#endif

#ifdef L_subvdi3
DWtype
__subvDI3 (DWtype a, DWtype b)
{
  DWtype w;

  if (__builtin_sub_overflow (a, b, &w))
    abort ();

  return w;
}
#endif

#ifdef L_mulvsi3
Wtype
__mulvSI3 (Wtype a, Wtype b)
{
  Wtype w;

  if (__builtin_mul_overflow (a, b, &w))
    abort ();

  return w;
}
#ifdef COMPAT_SIMODE_TRAPPING_ARITHMETIC
SItype
__mulvsi3 (SItype a, SItype b)
{
  SItype w;

  if (__builtin_mul_overflow (a, b, &w))
    abort ();

  return w;
}
#endif /* COMPAT_SIMODE_TRAPPING_ARITHMETIC */
#endif

#ifdef L_negvsi2
Wtype
__negvSI2 (Wtype a)
{
  Wtype w;

  if (__builtin_sub_overflow (0, a, &w))
    abort ();

  return w;
}
#ifdef COMPAT_SIMODE_TRAPPING_ARITHMETIC
SItype
__negvsi2 (SItype a)
{
  SItype w;

  if (__builtin_sub_overflow (0, a, &w))
    abort ();

  return w;
}
#endif /* COMPAT_SIMODE_TRAPPING_ARITHMETIC */
#endif

#ifdef L_negvdi2
DWtype
__negvDI2 (DWtype a)
{
  DWtype w;

  if (__builtin_sub_overflow (0, a, &w))
    abort ();

  return w;
}
#endif

#ifdef L_absvsi2
Wtype
__absvSI2 (Wtype a)
{
  const Wtype v = 0 - (a < 0);
  Wtype w;

  if (__builtin_add_overflow (a, v, &w))
    abort ();

  return v ^ w;
}
#ifdef COMPAT_SIMODE_TRAPPING_ARITHMETIC
SItype
__absvsi2 (SItype a)
{
  const SItype v = 0 - (a < 0);
  SItype w;

  if (__builtin_add_overflow (a, v, &w))
    abort ();

  return v ^ w;
}
#endif /* COMPAT_SIMODE_TRAPPING_ARITHMETIC */
#endif

#ifdef L_absvdi2
DWtype
__absvDI2 (DWtype a)
{
  const DWtype v = 0 - (a < 0);
  DWtype w;

  if (__builtin_add_overflow (a, v, &w))
    abort ();

  return v ^ w;
}
#endif

#ifdef L_mulvdi3
DWtype
__mulvDI3 (DWtype u, DWtype v)
{
  /* The unchecked multiplication needs 3 Wtype x Wtype multiplications,
     but the checked multiplication needs only two.  */
  const DWunion uu = {.ll = u};
  const DWunion vv = {.ll = v};

  if (__builtin_expect (uu.s.high == uu.s.low >> (W_TYPE_SIZE - 1), 1))
    {
      /* u fits in a single Wtype.  */
      if (__builtin_expect (vv.s.high == vv.s.low >> (W_TYPE_SIZE - 1), 1))
	{
	  /* v fits in a single Wtype as well.  */
	  /* A single multiplication.  No overflow risk.  */
	  return (DWtype) uu.s.low * (DWtype) vv.s.low;
	}
      else
	{
	  /* Two multiplications.  */
	  DWunion w0 = {.ll = (UDWtype) (UWtype) uu.s.low
			* (UDWtype) (UWtype) vv.s.low};
	  DWunion w1 = {.ll = (UDWtype) (UWtype) uu.s.low
			* (UDWtype) (UWtype) vv.s.high};

	  if (vv.s.high < 0)
	    w1.s.high -= uu.s.low;
	  if (uu.s.low < 0)
	    w1.ll -= vv.ll;
	  w1.ll += (UWtype) w0.s.high;
	  if (__builtin_expect (w1.s.high == w1.s.low >> (W_TYPE_SIZE - 1), 1))
	    {
	      w0.s.high = w1.s.low;
	      return w0.ll;
	    }
	}
    }
  else
    {
      if (__builtin_expect (vv.s.high == vv.s.low >> (W_TYPE_SIZE - 1), 1))
	{
	  /* v fits into a single Wtype.  */
	  /* Two multiplications.  */
	  DWunion w0 = {.ll = (UDWtype) (UWtype) uu.s.low
			* (UDWtype) (UWtype) vv.s.low};
	  DWunion w1 = {.ll = (UDWtype) (UWtype) uu.s.high
			* (UDWtype) (UWtype) vv.s.low};

	  if (uu.s.high < 0)
	    w1.s.high -= vv.s.low;
	  if (vv.s.low < 0)
	    w1.ll -= uu.ll;
	  w1.ll += (UWtype) w0.s.high;
	  if (__builtin_expect (w1.s.high == w1.s.low >> (W_TYPE_SIZE - 1), 1))
	    {
	      w0.s.high = w1.s.low;
	      return w0.ll;
	    }
	}
      else
	{
	  /* A few sign checks and a single multiplication.  */
	  if (uu.s.high >= 0)
	    {
	      if (vv.s.high >= 0)
		{
		  if (uu.s.high == 0 && vv.s.high == 0)
		    {
		      const DWtype w = (UDWtype) (UWtype) uu.s.low
			* (UDWtype) (UWtype) vv.s.low;
		      if (__builtin_expect (w >= 0, 1))
			return w;
		    }
		}
	      else
		{
		  if (uu.s.high == 0 && vv.s.high == (Wtype) -1)
		    {
		      DWunion ww = {.ll = (UDWtype) (UWtype) uu.s.low
				    * (UDWtype) (UWtype) vv.s.low};

		      ww.s.high -= uu.s.low;
		      if (__builtin_expect (ww.s.high < 0, 1))
			return ww.ll;
		    }
		}
	    }
	  else
	    {
	      if (vv.s.high >= 0)
		{
		  if (uu.s.high == (Wtype) -1 && vv.s.high == 0)
		    {
		      DWunion ww = {.ll = (UDWtype) (UWtype) uu.s.low
				    * (UDWtype) (UWtype) vv.s.low};

		      ww.s.high -= vv.s.low;
		      if (__builtin_expect (ww.s.high < 0, 1))
			return ww.ll;
		    }
		}
	      else
		{
		  if ((uu.s.high & vv.s.high) == (Wtype) -1
		      && (uu.s.low | vv.s.low) != 0)
		    {
		      DWunion ww = {.ll = (UDWtype) (UWtype) uu.s.low
				    * (UDWtype) (UWtype) vv.s.low};

		      ww.s.high -= uu.s.low;
		      ww.s.high -= vv.s.low;
		      if (__builtin_expect (ww.s.high >= 0, 1))
			return ww.ll;
		    }
		}
	    }
	}
    }

  /* Overflow.  */
  abort ();
}
#endif


/* Unless shift functions are defined with full ANSI prototypes,
   parameter b will be promoted to int if shift_count_type is smaller than an int.  */
#ifdef L_lshrdi3
DWtype
__lshrdi3 (DWtype u, shift_count_type b)
{
  if (b == 0)
    return u;

  const DWunion uu = {.ll = u};
  const shift_count_type bm = W_TYPE_SIZE - b;
  DWunion w;

  if (bm <= 0)
    {
      w.s.high = 0;
      w.s.low = (UWtype) uu.s.high >> -bm;
    }
  else
    {
      const UWtype carries = (UWtype) uu.s.high << bm;

      w.s.high = (UWtype) uu.s.high >> b;
      w.s.low = ((UWtype) uu.s.low >> b) | carries;
    }

  return w.ll;
}
#endif

#ifdef L_ashldi3
DWtype
__ashldi3 (DWtype u, shift_count_type b)
{
  if (b == 0)
    return u;

  const DWunion uu = {.ll = u};
  const shift_count_type bm = W_TYPE_SIZE - b;
  DWunion w;

  if (bm <= 0)
    {
      w.s.low = 0;
      w.s.high = (UWtype) uu.s.low << -bm;
    }
  else
    {
      const UWtype carries = (UWtype) uu.s.low >> bm;

      w.s.low = (UWtype) uu.s.low << b;
      w.s.high = ((UWtype) uu.s.high << b) | carries;
    }

  return w.ll;
}
#endif

#ifdef L_ashrdi3
DWtype
__ashrdi3 (DWtype u, shift_count_type b)
{
  if (b == 0)
    return u;

  const DWunion uu = {.ll = u};
  const shift_count_type bm = W_TYPE_SIZE - b;
  DWunion w;

  if (bm <= 0)
    {
      /* w.s.high = 1..1 or 0..0 */
      w.s.high = uu.s.high >> (W_TYPE_SIZE - 1);
      w.s.low = uu.s.high >> -bm;
    }
  else
    {
      const UWtype carries = (UWtype) uu.s.high << bm;

      w.s.high = uu.s.high >> b;
      w.s.low = ((UWtype) uu.s.low >> b) | carries;
    }

  return w.ll;
}
#endif

#ifdef L_bswapsi2
SItype
__bswapsi2 (SItype u)
{
  return ((((u) & 0xff000000u) >> 24)
	  | (((u) & 0x00ff0000u) >>  8)
	  | (((u) & 0x0000ff00u) <<  8)
	  | (((u) & 0x000000ffu) << 24));
}
#endif
#ifdef L_bswapdi2
DItype
__bswapdi2 (DItype u)
{
  return ((((u) & 0xff00000000000000ull) >> 56)
	  | (((u) & 0x00ff000000000000ull) >> 40)
	  | (((u) & 0x0000ff0000000000ull) >> 24)
	  | (((u) & 0x000000ff00000000ull) >>  8)
	  | (((u) & 0x00000000ff000000ull) <<  8)
	  | (((u) & 0x0000000000ff0000ull) << 24)
	  | (((u) & 0x000000000000ff00ull) << 40)
	  | (((u) & 0x00000000000000ffull) << 56));
}
#endif
#ifdef L_ffssi2
#undef int
int
__ffsSI2 (UWtype u)
{
  UWtype count;

  if (u == 0)
    return 0;

  count_trailing_zeros (count, u);
  return count + 1;
}
#endif

#ifdef L_ffsdi2
#undef int
int
__ffsDI2 (DWtype u)
{
  const DWunion uu = {.ll = u};
  UWtype word, count, add;

  if (uu.s.low != 0)
    word = uu.s.low, add = 0;
  else if (uu.s.high != 0)
    word = uu.s.high, add = W_TYPE_SIZE;
  else
    return 0;

  count_trailing_zeros (count, word);
  return count + add + 1;
}
#endif

#ifdef L_muldi3
DWtype
__muldi3 (DWtype u, DWtype v)
{
  const DWunion uu = {.ll = u};
  const DWunion vv = {.ll = v};
  DWunion w = {.ll = __umulsidi3 (uu.s.low, vv.s.low)};

  w.s.high += ((UWtype) uu.s.low * (UWtype) vv.s.high
	       + (UWtype) uu.s.high * (UWtype) vv.s.low);

  return w.ll;
}
#endif

#if (defined (L_udivdi3) || defined (L_divdi3) || \
     defined (L_umoddi3) || defined (L_moddi3))
#if defined (sdiv_qrnnd)
#define L_udiv_w_sdiv
#endif
#endif

#ifdef L_udiv_w_sdiv
#if defined (sdiv_qrnnd)
#if (defined (L_udivdi3) || defined (L_divdi3) || \
     defined (L_umoddi3) || defined (L_moddi3))
static inline __attribute__ ((__always_inline__))
#endif
UWtype
__udiv_w_sdiv (UWtype *rp, UWtype a1, UWtype a0, UWtype d)
{
  UWtype q, r;
  UWtype c0, c1, b1;

  if ((Wtype) d >= 0)
    {
      if (a1 < d - a1 - (a0 >> (W_TYPE_SIZE - 1)))
	{
	  /* Dividend, divisor, and quotient are nonnegative.  */
	  sdiv_qrnnd (q, r, a1, a0, d);
	}
      else
	{
	  /* Compute c1*2^32 + c0 = a1*2^32 + a0 - 2^31*d.  */
	  sub_ddmmss (c1, c0, a1, a0, d >> 1, d << (W_TYPE_SIZE - 1));
	  /* Divide (c1*2^32 + c0) by d.  */
	  sdiv_qrnnd (q, r, c1, c0, d);
	  /* Add 2^31 to quotient.  */
	  q += (UWtype) 1 << (W_TYPE_SIZE - 1);
	}
    }
  else
    {
      b1 = d >> 1;			/* d/2, between 2^30 and 2^31 - 1 */
      c1 = a1 >> 1;			/* A/2 */
      c0 = (a1 << (W_TYPE_SIZE - 1)) + (a0 >> 1);

      if (a1 < b1)			/* A < 2^32*b1, so A/2 < 2^31*b1 */
	{
	  sdiv_qrnnd (q, r, c1, c0, b1); /* (A/2) / (d/2) */

	  r = 2*r + (a0 & 1);		/* Remainder from A/(2*b1) */
	  if ((d & 1) != 0)
	    {
	      if (r >= q)
		r = r - q;
	      else if (q - r <= d)
		{
		  r = r - q + d;
		  q--;
		}
	      else
		{
		  r = r - q + 2*d;
		  q -= 2;
		}
	    }
	}
      else if (c1 < b1)			/* So 2^31 <= (A/2)/b1 < 2^32 */
	{
	  c1 = (b1 - 1) - c1;
	  c0 = ~c0;			/* logical NOT */

	  sdiv_qrnnd (q, r, c1, c0, b1); /* (A/2) / (d/2) */

	  q = ~q;			/* (A/2)/b1 */
	  r = (b1 - 1) - r;

	  r = 2*r + (a0 & 1);		/* A/(2*b1) */

	  if ((d & 1) != 0)
	    {
	      if (r >= q)
		r = r - q;
	      else if (q - r <= d)
		{
		  r = r - q + d;
		  q--;
		}
	      else
		{
		  r = r - q + 2*d;
		  q -= 2;
		}
	    }
	}
      else				/* Implies c1 = b1 */
	{				/* Hence a1 = d - 1 = 2*b1 - 1 */
	  if (a0 >= -d)
	    {
	      q = -1;
	      r = a0 + d;
	    }
	  else
	    {
	      q = -2;
	      r = a0 + 2*d;
	    }
	}
    }

  *rp = r;
  return q;
}
#else
/* If sdiv_qrnnd doesn't exist, define dummy __udiv_w_sdiv.  */
UWtype
__udiv_w_sdiv (UWtype *rp __attribute__ ((__unused__)),
	       UWtype a1 __attribute__ ((__unused__)),
	       UWtype a0 __attribute__ ((__unused__)),
	       UWtype d __attribute__ ((__unused__)))
{
  return 0;
}
#endif
#endif

#if (defined (L_udivdi3) || defined (L_divdi3) || \
     defined (L_umoddi3) || defined (L_moddi3) || \
     defined (L_divmoddi4))
#define L_udivmoddi4
#endif

#ifdef L_clz
const UQItype __clz_tab[256] =
{
  0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
};
#endif

#ifdef L_clzsi2
#undef int
int
__clzSI2 (UWtype x)
{
  Wtype ret;

  count_leading_zeros (ret, x);

  return ret;
}
#endif

#ifdef L_clzdi2
#undef int
int
__clzDI2 (UDWtype x)
{
  const DWunion uu = {.ll = x};
  UWtype word;
  Wtype ret, add;

  if (uu.s.high)
    word = uu.s.high, add = 0;
  else
    word = uu.s.low, add = W_TYPE_SIZE;

  count_leading_zeros (ret, word);
  return ret + add;
}
#endif

#ifdef L_ctzsi2
#undef int
int
__ctzSI2 (UWtype x)
{
  Wtype ret;

  count_trailing_zeros (ret, x);

  return ret;
}
#endif

#ifdef L_ctzdi2
#undef int
int
__ctzDI2 (UDWtype x)
{
  const DWunion uu = {.ll = x};
  UWtype word;
  Wtype ret, add;

  if (uu.s.low)
    word = uu.s.low, add = 0;
  else
    word = uu.s.high, add = W_TYPE_SIZE;

  count_trailing_zeros (ret, word);
  return ret + add;
}
#endif

#ifdef L_clrsbsi2
#undef int
int
__clrsbSI2 (Wtype x)
{
  Wtype ret;

  if (x < 0)
    x = ~x;
  if (x == 0)
    return W_TYPE_SIZE - 1;
  count_leading_zeros (ret, x);
  return ret - 1;
}
#endif

#ifdef L_clrsbdi2
#undef int
int
__clrsbDI2 (DWtype x)
{
  const DWunion uu = {.ll = x};
  UWtype word;
  Wtype ret, add;

  if (uu.s.high == 0)
    word = uu.s.low, add = W_TYPE_SIZE;
  else if (uu.s.high == -1)
    word = ~uu.s.low, add = W_TYPE_SIZE;
  else if (uu.s.high >= 0)
    word = uu.s.high, add = 0;
  else
    word = ~uu.s.high, add = 0;

  if (word == 0)
    ret = W_TYPE_SIZE;
  else
    count_leading_zeros (ret, word);

  return ret + add - 1;
}
#endif

#ifdef L_popcount_tab
const UQItype __popcount_tab[256] =
{
    0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8
};
#endif

#if defined(L_popcountsi2) || defined(L_popcountdi2)
#define POPCOUNTCST2(x) (((UWtype) x << __CHAR_BIT__) | x)
#define POPCOUNTCST4(x) (((UWtype) x << (2 * __CHAR_BIT__)) | x)
#define POPCOUNTCST8(x) (((UWtype) x << (4 * __CHAR_BIT__)) | x)
#if W_TYPE_SIZE == __CHAR_BIT__
#define POPCOUNTCST(x) x
#elif W_TYPE_SIZE == 2 * __CHAR_BIT__
#define POPCOUNTCST(x) POPCOUNTCST2 (x)
#elif W_TYPE_SIZE == 4 * __CHAR_BIT__
#define POPCOUNTCST(x) POPCOUNTCST4 (POPCOUNTCST2 (x))
#elif W_TYPE_SIZE == 8 * __CHAR_BIT__
#define POPCOUNTCST(x) POPCOUNTCST8 (POPCOUNTCST4 (POPCOUNTCST2 (x)))
#endif
#endif

#ifdef L_popcountsi2
#undef int
int
__popcountSI2 (UWtype x)
{
  /* Force table lookup on targets like AVR and RL78 which only
     pretend they have LIBGCC2_UNITS_PER_WORD 4, but actually
     have 1, and other small word targets.  */
#if __SIZEOF_INT__ > 2 && defined (POPCOUNTCST) && __CHAR_BIT__ == 8
  x = x - ((x >> 1) & POPCOUNTCST (0x55));
  x = (x & POPCOUNTCST (0x33)) + ((x >> 2) & POPCOUNTCST (0x33));
  x = (x + (x >> 4)) & POPCOUNTCST (0x0F);
  return (x * POPCOUNTCST (0x01)) >> (W_TYPE_SIZE - __CHAR_BIT__);
#else
  int i, ret = 0;

  for (i = 0; i < W_TYPE_SIZE; i += 8)
    ret += __popcount_tab[(x >> i) & 0xff];

  return ret;
#endif
}
#endif

#ifdef L_popcountdi2
#undef int
int
__popcountDI2 (UDWtype x)
{
  /* Force table lookup on targets like AVR and RL78 which only
     pretend they have LIBGCC2_UNITS_PER_WORD 4, but actually
     have 1, and other small word targets.  */
#if __SIZEOF_INT__ > 2 && defined (POPCOUNTCST) && __CHAR_BIT__ == 8
  const DWunion uu = {.ll = x};
  UWtype x1 = uu.s.low, x2 = uu.s.high;
  x1 = x1 - ((x1 >> 1) & POPCOUNTCST (0x55));
  x2 = x2 - ((x2 >> 1) & POPCOUNTCST (0x55));
  x1 = (x1 & POPCOUNTCST (0x33)) + ((x1 >> 2) & POPCOUNTCST (0x33));
  x2 = (x2 & POPCOUNTCST (0x33)) + ((x2 >> 2) & POPCOUNTCST (0x33));
  x1 = (x1 + (x1 >> 4)) & POPCOUNTCST (0x0F);
  x2 = (x2 + (x2 >> 4)) & POPCOUNTCST (0x0F);
  x1 += x2;
  return (x1 * POPCOUNTCST (0x01)) >> (W_TYPE_SIZE - __CHAR_BIT__);
#else
  int i, ret = 0;

  for (i = 0; i < 2*W_TYPE_SIZE; i += 8)
    ret += __popcount_tab[(x >> i) & 0xff];

  return ret;
#endif
}
#endif

#ifdef L_paritysi2
#undef int
int
__paritySI2 (UWtype x)
{
#if W_TYPE_SIZE > 64
# error "fill out the table"
#endif
#if W_TYPE_SIZE > 32
  x ^= x >> 32;
#endif
#if W_TYPE_SIZE > 16
  x ^= x >> 16;
#endif
  x ^= x >> 8;
  x ^= x >> 4;
  x &= 0xf;
  return (0x6996 >> x) & 1;
}
#endif

#ifdef L_paritydi2
#undef int
int
__parityDI2 (UDWtype x)
{
  const DWunion uu = {.ll = x};
  UWtype nx = uu.s.low ^ uu.s.high;

#if W_TYPE_SIZE > 64
# error "fill out the table"
#endif
#if W_TYPE_SIZE > 32
  nx ^= nx >> 32;
#endif
#if W_TYPE_SIZE > 16
  nx ^= nx >> 16;
#endif
  nx ^= nx >> 8;
  nx ^= nx >> 4;
  nx &= 0xf;
  return (0x6996 >> nx) & 1;
}
#endif

#ifdef L_udivmoddi4
#ifdef TARGET_HAS_NO_HW_DIVIDE

#if (defined (L_udivdi3) || defined (L_divdi3) || \
     defined (L_umoddi3) || defined (L_moddi3) || \
     defined (L_divmoddi4))
static inline __attribute__ ((__always_inline__))
#endif
UDWtype
__udivmoddi4 (UDWtype n, UDWtype d, UDWtype *rp)
{
  UDWtype q = 0, r = n, y = d;
  UWtype lz1, lz2, i, k;

  /* Implements align divisor shift dividend method. This algorithm
     aligns the divisor under the dividend and then perform number of
     test-subtract iterations which shift the dividend left. Number of
     iterations is k + 1 where k is the number of bit positions the
     divisor must be shifted left to align it under the dividend.
     quotient bits can be saved in the rightmost positions of the dividend
     as it shifts left on each test-subtract iteration. */

  if (y <= r)
    {
      lz1 = __builtin_clzll (d);
      lz2 = __builtin_clzll (n);

      k = lz1 - lz2;
      y = (y << k);

      /* Dividend can exceed 2 ^ (width - 1) - 1 but still be less than the
	 aligned divisor. Normal iteration can drops the high order bit
	 of the dividend. Therefore, first test-subtract iteration is a
	 special case, saving its quotient bit in a separate location and
	 not shifting the dividend. */
      if (r >= y)
	{
	  r = r - y;
	  q =  (1ULL << k);
	}

      if (k > 0)
	{
	  y = y >> 1;

	  /* k additional iterations where k regular test subtract shift
	    dividend iterations are done.  */
	  i = k;
	  do
	    {
	      if (r >= y)
		r = ((r - y) << 1) + 1;
	      else
		r =  (r << 1);
	      i = i - 1;
	    } while (i != 0);

	  /* First quotient bit is combined with the quotient bits resulting
	     from the k regular iterations.  */
	  q = q + r;
	  r = r >> k;
	  q = q - (r << k);
	}
    }

  if (rp)
    *rp = r;
  return q;
}
#else

#if (defined (L_udivdi3) || defined (L_divdi3) || \
     defined (L_umoddi3) || defined (L_moddi3) || \
     defined (L_divmoddi4))
static inline __attribute__ ((__always_inline__))
#endif
UDWtype
__udivmoddi4 (UDWtype n, UDWtype d, UDWtype *rp)
{
  const DWunion nn = {.ll = n};
  const DWunion dd = {.ll = d};
  DWunion rr;
  UWtype d0, d1, n0, n1, n2;
  UWtype q0, q1;
  UWtype b, bm;

  d0 = dd.s.low;
  d1 = dd.s.high;
  n0 = nn.s.low;
  n1 = nn.s.high;

#if !UDIV_NEEDS_NORMALIZATION
  if (d1 == 0)
    {
      if (d0 > n1)
	{
	  /* 0q = nn / 0D */

	  udiv_qrnnd (q0, n0, n1, n0, d0);
	  q1 = 0;

	  /* Remainder in n0.  */
	}
      else
	{
	  /* qq = NN / 0d */

	  if (d0 == 0)
	    d0 = 1 / d0;	/* Divide intentionally by zero.  */

	  udiv_qrnnd (q1, n1, 0, n1, d0);
	  udiv_qrnnd (q0, n0, n1, n0, d0);

	  /* Remainder in n0.  */
	}

      if (rp != 0)
	{
	  rr.s.low = n0;
	  rr.s.high = 0;
	  *rp = rr.ll;
	}
    }

#else /* UDIV_NEEDS_NORMALIZATION */

  if (d1 == 0)
    {
      if (d0 > n1)
	{
	  /* 0q = nn / 0D */

	  count_leading_zeros (bm, d0);

	  if (bm != 0)
	    {
	      /* Normalize, i.e. make the most significant bit of the
		 denominator set.  */

	      d0 = d0 << bm;
	      n1 = (n1 << bm) | (n0 >> (W_TYPE_SIZE - bm));
	      n0 = n0 << bm;
	    }

	  udiv_qrnnd (q0, n0, n1, n0, d0);
	  q1 = 0;

	  /* Remainder in n0 >> bm.  */
	}
      else
	{
	  /* qq = NN / 0d */

	  if (d0 == 0)
	    d0 = 1 / d0;	/* Divide intentionally by zero.  */

	  count_leading_zeros (bm, d0);

	  if (bm == 0)
	    {
	      /* From (n1 >= d0) /\ (the most significant bit of d0 is set),
		 conclude (the most significant bit of n1 is set) /\ (the
		 leading quotient digit q1 = 1).

		 This special case is necessary, not an optimization.
		 (Shifts counts of W_TYPE_SIZE are undefined.)  */

	      n1 -= d0;
	      q1 = 1;
	    }
	  else
	    {
	      /* Normalize.  */

	      b = W_TYPE_SIZE - bm;

	      d0 = d0 << bm;
	      n2 = n1 >> b;
	      n1 = (n1 << bm) | (n0 >> b);
	      n0 = n0 << bm;

	      udiv_qrnnd (q1, n1, n2, n1, d0);
	    }

	  /* n1 != d0...  */

	  udiv_qrnnd (q0, n0, n1, n0, d0);

	  /* Remainder in n0 >> bm.  */
	}

      if (rp != 0)
	{
	  rr.s.low = n0 >> bm;
	  rr.s.high = 0;
	  *rp = rr.ll;
	}
    }
#endif /* UDIV_NEEDS_NORMALIZATION */

  else
    {
      if (d1 > n1)
	{
	  /* 00 = nn / DD */

	  q0 = 0;
	  q1 = 0;

	  /* Remainder in n1n0.  */
	  if (rp != 0)
	    {
	      rr.s.low = n0;
	      rr.s.high = n1;
	      *rp = rr.ll;
	    }
	}
      else
	{
	  /* 0q = NN / dd */

	  count_leading_zeros (bm, d1);
	  if (bm == 0)
	    {
	      /* From (n1 >= d1) /\ (the most significant bit of d1 is set),
		 conclude (the most significant bit of n1 is set) /\ (the
		 quotient digit q0 = 0 or 1).

		 This special case is necessary, not an optimization.  */

	      /* The condition on the next line takes advantage of that
		 n1 >= d1 (true due to program flow).  */
	      if (n1 > d1 || n0 >= d0)
		{
		  q0 = 1;
		  sub_ddmmss (n1, n0, n1, n0, d1, d0);
		}
	      else
		q0 = 0;

	      q1 = 0;

	      if (rp != 0)
		{
		  rr.s.low = n0;
		  rr.s.high = n1;
		  *rp = rr.ll;
		}
	    }
	  else
	    {
	      UWtype m1, m0;
	      /* Normalize.  */

	      b = W_TYPE_SIZE - bm;

	      d1 = (d1 << bm) | (d0 >> b);
	      d0 = d0 << bm;
	      n2 = n1 >> b;
	      n1 = (n1 << bm) | (n0 >> b);
	      n0 = n0 << bm;

	      udiv_qrnnd (q0, n1, n2, n1, d1);
	      umul_ppmm (m1, m0, q0, d0);

	      if (m1 > n1 || (m1 == n1 && m0 > n0))
		{
		  q0--;
		  sub_ddmmss (m1, m0, m1, m0, d1, d0);
		}

	      q1 = 0;

	      /* Remainder in (n1n0 - m1m0) >> bm.  */
	      if (rp != 0)
		{
		  sub_ddmmss (n1, n0, n1, n0, m1, m0);
		  rr.s.low = (n1 << b) | (n0 >> bm);
		  rr.s.high = n1 >> bm;
		  *rp = rr.ll;
		}
	    }
	}
    }

  const DWunion ww = {{.low = q0, .high = q1}};
  return ww.ll;
}
#endif
#endif

#ifdef L_divdi3
DWtype
__divdi3 (DWtype u, DWtype v)
{
  Wtype c = 0;
  DWunion uu = {.ll = u};
  DWunion vv = {.ll = v};
  DWtype w;

  if (uu.s.high < 0)
    c = ~c,
    uu.ll = -uu.ll;
  if (vv.s.high < 0)
    c = ~c,
    vv.ll = -vv.ll;

  w = __udivmoddi4 (uu.ll, vv.ll, (UDWtype *) 0);
  if (c)
    w = -w;

  return w;
}
#endif

#ifdef L_moddi3
DWtype
__moddi3 (DWtype u, DWtype v)
{
  Wtype c = 0;
  DWunion uu = {.ll = u};
  DWunion vv = {.ll = v};
  DWtype w;

  if (uu.s.high < 0)
    c = ~c,
    uu.ll = -uu.ll;
  if (vv.s.high < 0)
    vv.ll = -vv.ll;

  (void) __udivmoddi4 (uu.ll, vv.ll, (UDWtype*)&w);
  if (c)
    w = -w;

  return w;
}
#endif

#ifdef L_divmoddi4
DWtype
__divmoddi4 (DWtype u, DWtype v, DWtype *rp)
{
  Wtype c1 = 0, c2 = 0;
  DWunion uu = {.ll = u};
  DWunion vv = {.ll = v};
  DWtype w;
  DWtype r;

  if (uu.s.high < 0)
    c1 = ~c1, c2 = ~c2,
    uu.ll = -uu.ll;
  if (vv.s.high < 0)
    c1 = ~c1,
    vv.ll = -vv.ll;

  w = __udivmoddi4 (uu.ll, vv.ll, (UDWtype*)&r);
  if (c1)
    w = -w;
  if (c2)
    r = -r;

  *rp = r;
  return w;
}
#endif

#ifdef L_umoddi3
UDWtype
__umoddi3 (UDWtype u, UDWtype v)
{
  UDWtype w;

  (void) __udivmoddi4 (u, v, &w);

  return w;
}
#endif

#ifdef L_udivdi3
UDWtype
__udivdi3 (UDWtype n, UDWtype d)
{
  return __udivmoddi4 (n, d, (UDWtype *) 0);
}
#endif

#if (defined(__BITINT_MAXWIDTH__) \
     && (defined(L_mulbitint3) || defined(L_divmodbitint4)))
/* _BitInt support.  */

/* If *P is zero or sign extended (the latter only for PREC < 0) from
   some narrower _BitInt value, reduce precision.  */

static inline __attribute__((__always_inline__)) SItype
bitint_reduce_prec (const UBILtype **p, SItype prec)
{
  UWtype mslimb;
  SItype i;
  if (prec < 0)
    {
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
      i = 0;
#else
      i = ((USItype) -1 - prec) / W_TYPE_SIZE;
#endif
      mslimb = (*p)[i];
      if (mslimb & ((UWtype) 1 << (((USItype) -1 - prec) % W_TYPE_SIZE)))
	{
	  SItype n = ((USItype) -prec) % W_TYPE_SIZE;
	  if (n)
	    {
	      mslimb |= ((UWtype) -1 << (((USItype) -1 - prec) % W_TYPE_SIZE));
	      if (mslimb == (UWtype) -1)
		{
		  prec += n;
		  if (prec >= -1)
		    return -2;
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
		  ++p;
#else
		  --i;
#endif
		  mslimb = (*p)[i];
		  n = 0;
		}
	    }
	  while (mslimb == (UWtype) -1)
	    {
	      prec += W_TYPE_SIZE;
	      if (prec >= -1)
		return -2;
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
	      ++p;
#else
	      --i;
#endif
	      mslimb = (*p)[i];
	    }
	  if (n == 0)
	    {
	      if ((Wtype) mslimb >= 0)
		{
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
		  --p;
#endif
		  return prec - 1;
		}
	    }
	  return prec;
	}
      else
	prec = -prec;
    }
  else
    {
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
      i = 0;
#else
      i = ((USItype) prec - 1) / W_TYPE_SIZE;
#endif
      mslimb = (*p)[i];
    }
  SItype n = ((USItype) prec) % W_TYPE_SIZE;
  if (n)
    {
      mslimb &= ((UWtype) 1 << (((USItype) prec) % W_TYPE_SIZE)) - 1;
      if (mslimb == 0)
	{
	  prec -= n;
	  if (prec == 0)
	    return 1;
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
	  ++p;
#else
	  --i;
#endif
	  mslimb = (*p)[i];
	}
    }
  while (mslimb == 0)
    {
      prec -= W_TYPE_SIZE;
      if (prec == 0)
	return 1;
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
      ++p;
#else
      --i;
#endif
      mslimb = (*p)[i];
    }
  return prec;
}

#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
# define BITINT_INC -1
# define BITINT_END(be, le) (be)
#else
# define BITINT_INC 1
# define BITINT_END(be, le) (le)
#endif

#ifdef L_mulbitint3
/* D = S * L.  */

static UWtype
bitint_mul_1 (UBILtype *d, const UBILtype *s, UWtype l, SItype n)
{
  UWtype sv, hi, lo, c = 0;
  do
    {
      sv = *s;
      s += BITINT_INC;
      umul_ppmm (hi, lo, sv, l);
      c = __builtin_add_overflow (lo, c, &lo) + hi;
      *d = lo;
      d += BITINT_INC;
    }
  while (--n);
  return c;
}

/* D += S * L.  */

static UWtype
bitint_addmul_1 (UBILtype *d, const UBILtype *s, UWtype l, SItype n)
{
  UWtype sv, hi, lo, c = 0;
  do
    {
      sv = *s;
      s += BITINT_INC;
      umul_ppmm (hi, lo, sv, l);
      hi += __builtin_add_overflow (lo, *d, &lo);
      c = __builtin_add_overflow (lo, c, &lo) + hi;
      *d = lo;
      d += BITINT_INC;
    }
  while (--n);
  return c;
}

/* If XPREC is positive, it is precision in bits
   of an unsigned _BitInt operand (which has XPREC/W_TYPE_SIZE
   full limbs and if Xprec%W_TYPE_SIZE one partial limb.
   If Xprec is negative, -XPREC is precision in bits
   of a signed _BitInt operand.  RETPREC should be always
   positive.  */

void
__mulbitint3 (UBILtype *ret, SItype retprec,
	      const UBILtype *u, SItype uprec,
	      const UBILtype *v, SItype vprec)
{
  uprec = bitint_reduce_prec (&u, uprec);
  vprec = bitint_reduce_prec (&v, vprec);
  USItype auprec = uprec < 0 ? -uprec : uprec;
  USItype avprec = vprec < 0 ? -vprec : vprec;

  /* Prefer non-negative U.
     Otherwise make sure V doesn't have higher precision than U.  */
  if ((uprec < 0 && vprec >= 0)
      || (avprec > auprec && !(uprec >= 0 && vprec < 0)))
    {
      SItype p;
      const UBILtype *t;
      p = uprec; uprec = vprec; vprec = p;
      p = auprec; auprec = avprec; avprec = p;
      t = u; u = v; v = t;
    }

  USItype un = auprec / W_TYPE_SIZE;
  USItype un2 = (auprec + W_TYPE_SIZE - 1) / W_TYPE_SIZE;
  USItype vn = avprec / W_TYPE_SIZE;
  USItype vn2 = (avprec + W_TYPE_SIZE - 1) / W_TYPE_SIZE;
  USItype retn = ((USItype) retprec + W_TYPE_SIZE - 1) / W_TYPE_SIZE;
  USItype retidx, uidx, vidx;
  UWtype vv;
  /* Indexes of least significant limb.  */
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
  retidx = retn - 1;
  uidx = un2 - 1;
  vidx = vn2 - 1;
#else
  retidx = 0;
  uidx = 0;
  vidx = 0;
#endif
  if (__builtin_expect (auprec <= W_TYPE_SIZE, 0) && vprec < 0)
    {
      UWtype uu = u[uidx];
      if (__builtin_expect (auprec < W_TYPE_SIZE, 0))
	uu &= ((UWtype) 1 << (auprec % W_TYPE_SIZE)) - 1;
      if (uu == 0)
	{
	  /* 0 * negative would be otherwise mishandled below, so
	     handle it specially.  */
	  __builtin_memset (ret, 0, retn * sizeof (UWtype));
	  return;
	}
    }
  vv = v[vidx];
  if (__builtin_expect (avprec < W_TYPE_SIZE, 0))
    {
      if (vprec > 0)
	vv &= ((UWtype) 1 << (avprec % W_TYPE_SIZE)) - 1;
      else
	vv |= (UWtype) -1 << (avprec % W_TYPE_SIZE);
    }

  USItype n = un > retn ? retn : un;
  USItype n2 = n;
  USItype retidx2 = retidx + n * BITINT_INC;
  UWtype c = 0, uv = 0;
  if (n)
    c = bitint_mul_1 (ret + retidx, u + uidx, vv, n);
  if (retn > un && un2 != un)
    {
      UWtype hi, lo;
      uv = u[uidx + n * BITINT_INC];
      if (uprec > 0)
	uv &= ((UWtype) 1 << (auprec % W_TYPE_SIZE)) - 1;
      else
	uv |= (UWtype) -1 << (auprec % W_TYPE_SIZE);
      umul_ppmm (hi, lo, uv, vv);
      c = __builtin_add_overflow (lo, c, &lo) + hi;
      ret[retidx2] = lo;
      retidx2 += BITINT_INC;
      ++n2;
    }
  if (retn > un2)
    {
      if (uprec < 0)
	{
	  while (n2 < retn)
	    {
	      if (n2 >= un2 + vn2)
		break;
	      UWtype hi, lo;
	      umul_ppmm (hi, lo, (UWtype) -1, vv);
	      c = __builtin_add_overflow (lo, c, &lo) + hi;
	      ret[retidx2] = lo;
	      retidx2 += BITINT_INC;
	      ++n2;
	    }
	}
      else
	{
	  ret[retidx2] = c;
	  retidx2 += BITINT_INC;
	  ++n2;
	}
      /* If RET has more limbs than U after precision reduction,
	 fill in the remaining limbs.  */
      while (n2 < retn)
	{
	  if (n2 < un2 + vn2 || (uprec ^ vprec) >= 0)
	    c = 0;
	  else
	    c = (UWtype) -1;
	  ret[retidx2] = c;
	  retidx2 += BITINT_INC;
	  ++n2;
	}
    }
  /* N is now number of possibly non-zero limbs in RET (ignoring
     limbs above UN2 + VN2 which if any have been finalized already).  */
  USItype end = vprec < 0 ? un2 + vn2 : vn2;
  if (retn > un2 + vn2) retn = un2 + vn2;
  if (end > retn) end = retn;
  for (USItype m = 1; m < end; ++m)
    {
      retidx += BITINT_INC;
      vidx += BITINT_INC;
      if (m < vn2)
	{
	  vv = v[vidx];
	  if (__builtin_expect (m == vn, 0))
	    {
	      if (vprec > 0)
		vv &= ((UWtype) 1 << (avprec % W_TYPE_SIZE)) - 1;
	      else
		vv |= (UWtype) -1 << (avprec % W_TYPE_SIZE);
	    }
	}
      else
	vv = (UWtype) -1;
      if (m + n > retn)
	n = retn - m;
      c = 0;
      if (n)
	c = bitint_addmul_1 (ret + retidx, u + uidx, vv, n);
      n2 = m + n;
      retidx2 = retidx + n * BITINT_INC;
      if (n2 < retn && un2 != un)
	{
	  UWtype hi, lo;
	  umul_ppmm (hi, lo, uv, vv);
	  hi += __builtin_add_overflow (lo, ret[retidx2], &lo);
	  c = __builtin_add_overflow (lo, c, &lo) + hi;
	  ret[retidx2] = lo;
	  retidx2 += BITINT_INC;
	  ++n2;
	}
      if (uprec < 0)
	while (n2 < retn)
	  {
	    UWtype hi, lo;
	    umul_ppmm (hi, lo, (UWtype) -1, vv);
	    hi += __builtin_add_overflow (lo, ret[retidx2], &lo);
	    c = __builtin_add_overflow (lo, c, &lo) + hi;
	    ret[retidx2] = lo;
	    retidx2 += BITINT_INC;
	    ++n2;
	  }
      else if (n2 < retn)
	{
	  ret[retidx2] = c;
	  retidx2 += BITINT_INC;
	}
    }
}
#endif

#ifdef L_divmodbitint4
/* D = -S.  */

static UWtype
bitint_negate (UBILtype *d, const UBILtype *s, SItype n)
{
  UWtype c = 1;
  UWtype r = 0;
  do
    {
      UWtype sv = *s, lo;
      r |= sv;
      s += BITINT_INC;
      c = __builtin_add_overflow (~sv, c, &lo);
      *d = lo;
      d += BITINT_INC;
    }
  while (--n);
  return r;
}

/* D -= S * L.  */

static UWtype
bitint_submul_1 (UBILtype *d, const UBILtype *s, UWtype l, SItype n)
{
  UWtype sv, hi, lo, c = 0;
  do
    {
      sv = *s;
      s += BITINT_INC;
      umul_ppmm (hi, lo, sv, l);
      hi += __builtin_sub_overflow (*d, lo, &lo);
      c = __builtin_sub_overflow (lo, c, &lo) + hi;
      *d = lo;
      d += BITINT_INC;
    }
  while (--n);
  return c;
}

/* If XPREC is positive, it is precision in bits
   of an unsigned _BitInt operand (which has XPREC/W_TYPE_SIZE
   full limbs and if Xprec%W_TYPE_SIZE one partial limb.
   If Xprec is negative, -XPREC is precision in bits
   of a signed _BitInt operand.  QPREC and RPREC should be
   always non-negative.  If either Q or R is NULL (at least
   one should be non-NULL), then corresponding QPREC or RPREC
   should be 0.  */

void
__divmodbitint4 (UBILtype *q, SItype qprec,
		 UBILtype *r, SItype rprec,
		 const UBILtype *u, SItype uprec,
		 const UBILtype *v, SItype vprec)
{
  uprec = bitint_reduce_prec (&u, uprec);
  vprec = bitint_reduce_prec (&v, vprec);
  USItype auprec = uprec < 0 ? -uprec : uprec;
  USItype avprec = vprec < 0 ? -vprec : vprec;
  USItype un = (auprec + W_TYPE_SIZE - 1) / W_TYPE_SIZE;
  USItype vn = (avprec + W_TYPE_SIZE - 1) / W_TYPE_SIZE;
  USItype qn = ((USItype) qprec + W_TYPE_SIZE - 1) / W_TYPE_SIZE;
  USItype rn = ((USItype) rprec + W_TYPE_SIZE - 1) / W_TYPE_SIZE;
  USItype up = auprec % W_TYPE_SIZE;
  USItype vp = avprec % W_TYPE_SIZE;
  /* If vprec < 0 and the top limb of v is all ones and the second most
     significant limb has most significant bit clear, then just decrease
     vn/avprec/vp, because after negation otherwise v2 would have most
     significant limb clear.  */
  if (vprec < 0
      && ((v[BITINT_END (0, vn - 1)] | (vp ? ((UWtype) -1 << vp) : 0))
	  == (UWtype) -1)
      && vn > 1
      && (Wtype) v[BITINT_END (1, vn - 2)] >= 0)
    {
      /* Unless all bits below the most significant limb are zero.  */
      SItype vn2;
      for (vn2 = vn - 2; vn2 >= 0; --vn2)
	if (v[BITINT_END (vn - 1 - vn2, vn2)])
	  {
	    vp = 0;
	    --vn;
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
	    ++v;
#endif
	    break;
	  }
    }
  if (__builtin_expect (un < vn, 0))
    {
      /* q is 0 and r is u.  */
      if (q)
	__builtin_memset (q, 0, qn * sizeof (UWtype));
      if (r == NULL)
	return;
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
      r += rn - 1;
      u += un - 1;
#endif
      if (up)
	--un;
      if (rn < un)
	un = rn;
      for (rn -= un; un; --un)
	{
	  *r = *u;
	  r += BITINT_INC;
	  u += BITINT_INC;
	}
      if (!rn)
	return;
      if (up)
	{
	  if (uprec > 0)
	    *r = *u & (((UWtype) 1 << up) - 1);
	  else
	    *r = *u | ((UWtype) -1 << up);
	  r += BITINT_INC;
	  if (!--rn)
	    return;
	}
      UWtype c = uprec < 0 ? (UWtype) -1 : (UWtype) 0;
      for (; rn; --rn)
	{
	  *r = c;
	  r += BITINT_INC;
	}
      return;
    }
  USItype qn2 = un - vn + 1;
  if (qn >= qn2)
    qn2 = 0;
  USItype sz = un + 1 + vn + qn2;
  UBILtype *buf = __builtin_alloca (sz * sizeof (UWtype));
  USItype uidx, vidx;
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
  uidx = un - 1;
  vidx = vn - 1;
#else
  uidx = 0;
  vidx = 0;
#endif
  if (uprec < 0)
    bitint_negate (buf + BITINT_END (uidx + 1, 0), u + uidx, un);
  else
    __builtin_memcpy (buf + BITINT_END (1, 0), u, un * sizeof (UWtype));
  if (up)
    buf[BITINT_END (1, un - 1)] &= (((UWtype) 1 << up) - 1);
  if (vprec < 0)
    bitint_negate (buf + un + 1 + vidx, v + vidx, vn);
  else
    __builtin_memcpy (buf + un + 1, v, vn * sizeof (UWtype));
  if (vp)
    buf[un + 1 + BITINT_END (0, vn - 1)] &= (((UWtype) 1 << vp) - 1);
  UBILtype *u2 = buf;
  UBILtype *v2 = u2 + un + 1;
  UBILtype *q2 = v2 + vn;
  if (!qn2)
    q2 = q + BITINT_END (qn - (un - vn + 1), 0);

  /* Knuth's algorithm.  See also ../gcc/wide-int.cc (divmod_internal_2).  */

#ifndef UDIV_NEEDS_NORMALIZATION
  /* Handle single limb divisor first.  */
  if (vn == 1)
    {
      UWtype vv = v2[0];
      if (vv == 0)
	vv = 1 / vv; /* Divide intentionally by zero.  */
      UWtype k = 0;
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
      for (SItype i = 0; i <= un - 1; ++i)
#else
      for (SItype i = un - 1; i >= 0; --i)
#endif
	udiv_qrnnd (q2[i], k, k, u2[BITINT_END (i + 1, i)], vv);
      if (r != NULL)
	r[BITINT_END (rn - 1, 0)] = k;
    }
  else
#endif
    {
      SItype s;
#ifdef UDIV_NEEDS_NORMALIZATION
      if (vn == 1 && v2[0] == 0)
	s = 0;
      else
#endif
      if (sizeof (0U) == sizeof (UWtype))
	s = __builtin_clz (v2[BITINT_END (0, vn - 1)]);
      else if (sizeof (0UL) == sizeof (UWtype))
	s = __builtin_clzl (v2[BITINT_END (0, vn - 1)]);
      else
	s = __builtin_clzll (v2[BITINT_END (0, vn - 1)]);
      if (s)
	{
	  /* Normalize by shifting v2 left so that it has msb set.  */
	  const SItype n = sizeof (UWtype) * __CHAR_BIT__;
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
	  for (SItype i = 0; i < vn - 1; ++i)
#else
	  for (SItype i = vn - 1; i > 0; --i)
#endif
	    v2[i] = (v2[i] << s) | (v2[i - BITINT_INC] >> (n - s));
	  v2[vidx] = v2[vidx] << s;
	  /* And shift u2 left by the same amount.  */
	  u2[BITINT_END (0, un)] = u2[BITINT_END (1, un - 1)] >> (n - s);
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
	  for (SItype i = 1; i < un; ++i)
#else
	  for (SItype i = un - 1; i > 0; --i)
#endif
	    u2[i] = (u2[i] << s) | (u2[i - BITINT_INC] >> (n - s));
	  u2[BITINT_END (un, 0)] = u2[BITINT_END (un, 0)] << s;
	}
      else
	u2[BITINT_END (0, un)] = 0;
#ifdef UDIV_NEEDS_NORMALIZATION
      /* Handle single limb divisor first.  */
      if (vn == 1)
	{
	  UWtype vv = v2[0];
	  if (vv == 0)
	    vv = 1 / vv; /* Divide intentionally by zero.  */
	  UWtype k = u2[BITINT_END (0, un)];
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
	  for (SItype i = 0; i <= un - 1; ++i)
#else
	  for (SItype i = un - 1; i >= 0; --i)
#endif
	    udiv_qrnnd (q2[i], k, k, u2[BITINT_END (i + 1, i)], vv);
	  if (r != NULL)
	    r[BITINT_END (rn - 1, 0)] = k >> s;
	}
      else
#endif
	{
	  UWtype vv1 = v2[BITINT_END (0, vn - 1)];
	  UWtype vv0 = v2[BITINT_END (1, vn - 2)];
	  /* Main loop.  */
	  for (SItype j = un - vn; j >= 0; --j)
	    {
	      /* Compute estimate in qhat.  */
	      UWtype uv1 = u2[BITINT_END (un - j - vn, j + vn)];
	      UWtype uv0 = u2[BITINT_END (un - j - vn + 1, j + vn - 1)];
	      UWtype qhat, rhat, hi, lo, c;
	      if (uv1 >= vv1)
		{
		  /* udiv_qrnnd doesn't support quotients which don't
		     fit into UWtype, while Knuth's algorithm originally
		     uses a double-word by word to double-word division.
		     Fortunately, the algorithm guarantees that uv1 <= vv1,
		     because if uv1 > vv1, then even if v would have all
		     bits in all words below vv1 set, the previous iteration
		     would be supposed to use qhat larger by 1 and subtract
		     v.  With uv1 == vv1 and uv0 >= vv1 the double-word
		     qhat in Knuth's algorithm would be 1 in the upper word
		     and 1 in the lower word, say for
		     uv1 0x8000000000000000ULL
		     uv0 0xffffffffffffffffULL
		     vv1 0x8000000000000000ULL
		     0x8000000000000000ffffffffffffffffuwb
		     / 0x8000000000000000uwb == 0x10000000000000001uwb, and
		     exactly like that also for any other value
		     > 0x8000000000000000ULL in uv1 and vv1 and uv0 >= uv1.
		     So we need to subtract one or at most two vv1s from
		     uv1:uv0 (qhat because of that decreases by 1 or 2 and
		     is then representable in UWtype) and need to increase
		     rhat by vv1 once or twice because of that.  Now, if
		     we need to subtract 2 vv1s, i.e. if
		     uv1 == vv1 && uv0 >= vv1, then rhat (which is uv0 - vv1)
		     + vv1 computation can't overflow, because it is equal
		     to uv0 and therefore the original algorithm in that case
		     performs goto again, but the second vv1 addition must
		     overflow already because vv1 has msb set from the
		     canonicalization.  */
		  uv1 -= __builtin_sub_overflow (uv0, vv1, &uv0);
		  if (uv1 >= vv1)
		    {
		      uv1 -= __builtin_sub_overflow (uv0, vv1, &uv0);
		      udiv_qrnnd (qhat, rhat, uv1, uv0, vv1);
		      rhat += 2 * vv1;
		    }
		  else
		    {
		      udiv_qrnnd (qhat, rhat, uv1, uv0, vv1);
		      if (!__builtin_add_overflow (rhat, vv1, &rhat))
			goto again;
		    }
		}
	      else
		{
		  udiv_qrnnd (qhat, rhat, uv1, uv0, vv1);
		again:
		  umul_ppmm (hi, lo, qhat, vv0);
		  if (hi > rhat
		      || (hi == rhat
			  && lo > u2[BITINT_END (un - j - vn + 2,
						 j + vn - 2)]))
		    {
		      --qhat;
		      if (!__builtin_add_overflow (rhat, vv1, &rhat))
			goto again;
		    }
		}

	      c = bitint_submul_1 (u2 + BITINT_END (un - j, j),
				   v2 + BITINT_END (vn - 1, 0), qhat, vn);
	      u2[BITINT_END (un - j - vn, j + vn)] -= c;
	      /* If we've subtracted too much, decrease qhat and
		 and add back.  */
	      if ((Wtype) u2[BITINT_END (un - j - vn, j + vn)] < 0)
		{
		  --qhat;
		  c = 0;
		  for (USItype i = 0; i < vn; ++i)
		    {
		      UWtype s = v2[BITINT_END (vn - 1 - i, i)];
		      UWtype d = u2[BITINT_END (un - i - j, i + j)];
		      UWtype c1 = __builtin_add_overflow (d, s, &d);
		      UWtype c2 = __builtin_add_overflow (d, c, &d);
		      c = c1 + c2;
		      u2[BITINT_END (un - i - j, i + j)] = d;
		    }
		  u2[BITINT_END (un - j - vn, j + vn)] += c;
		}
	      q2[BITINT_END (un - vn - j, j)] = qhat;
	    }
	  if (r != NULL)
	    {
	      if (s)
		{
		  const SItype n = sizeof (UWtype) * __CHAR_BIT__;
		  /* Unnormalize remainder.  */
		  USItype i;
		  for (i = 0; i < vn && i < rn; ++i)
		    r[BITINT_END (rn - 1 - i, i)]
		      = ((u2[BITINT_END (un - i, i)] >> s)
			 | (u2[BITINT_END (un - i - 1, i + 1)] << (n - s)));
		  if (i < rn)
		    r[BITINT_END (rn - vn, vn - 1)]
		      = u2[BITINT_END (un - vn + 1, vn - 1)] >> s;
		}
	      else if (rn > vn)
		__builtin_memcpy (&r[BITINT_END (rn - vn, 0)],
				  &u2[BITINT_END (un + 1 - vn, 0)],
				  vn * sizeof (UWtype));
	      else
		__builtin_memcpy (&r[0], &u2[BITINT_END (un + 1 - rn, 0)],
				  rn * sizeof (UWtype));
	    }
	}
    }
  if (q != NULL)
    {
      if ((uprec < 0) ^ (vprec < 0))
	{
	  /* Negative quotient.  */
	  USItype n;
	  if (un - vn + 1 > qn)
	    n = qn;
	  else
	    n = un - vn + 1;
	  SItype c = bitint_negate (q + BITINT_END (qn - 1, 0),
				    q2 + BITINT_END (un - vn, 0), n) ? -1 : 0;
	  if (qn > n)
	    __builtin_memset (q + BITINT_END (0, n), c,
			      (qn - n) * sizeof (UWtype));
	}
      else
	{
	  /* Positive quotient.  */
	  if (qn2)
	    __builtin_memcpy (q, q2 + BITINT_END (un - vn + 1 - qn, 0),
			      qn * sizeof (UWtype));
	  else if (qn > un - vn + 1)
	    __builtin_memset (q + BITINT_END (0, un - vn + 1), 0,
			      (qn - (un - vn + 1)) * sizeof (UWtype));
	}
    }
  if (r != NULL)
    {
      if (uprec < 0)
	{
	  /* Negative remainder.  */
	  SItype c = bitint_negate (r + BITINT_END (rn - 1, 0),
				    r + BITINT_END (rn - 1, 0),
				    rn > vn ? vn : rn) ? -1 : 0;
	  if (rn > vn)
	    __builtin_memset (r + BITINT_END (0, vn), c,
			      (rn - vn) * sizeof (UWtype));
	}
      else
	{
	  /* Positive remainder.  */
	  if (rn > vn)
	    __builtin_memset (r + BITINT_END (0, vn), 0,
			      (rn - vn) * sizeof (UWtype));
	}
    }
}
#endif
#endif

#ifdef L_cmpdi2
cmp_return_type
__cmpdi2 (DWtype a, DWtype b)
{
  return (a > b) - (a < b) + 1;
}
#endif

#ifdef L_ucmpdi2
cmp_return_type
__ucmpdi2 (UDWtype a, UDWtype b)
{
  return (a > b) - (a < b) + 1;
}
#endif

#if defined(L_fixunstfdi) && LIBGCC2_HAS_TF_MODE
UDWtype
__fixunstfDI (TFtype a)
{
  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  const TFtype b = (a / Wtype_MAXp1_F);
  /* Convert that to fixed (but not to DWtype!),
     and shift it into the high word.  */
  UDWtype v = (UWtype) b;
  v <<= W_TYPE_SIZE;
  /* Remove high part from the TFtype, leaving the low part as flonum.  */
  a -= (TFtype)v;
  /* Convert that to fixed (but not to DWtype!) and add it in.
     Sometimes A comes out negative.  This is significant, since
     A has more bits than a long int does.  */
  if (a < 0)
    v -= (UWtype) (- a);
  else
    v += (UWtype) a;
  return v;
}
#endif

#if defined(L_fixtfdi) && LIBGCC2_HAS_TF_MODE
DWtype
__fixtfdi (TFtype a)
{
  if (a < 0)
    return - __fixunstfDI (-a);
  return __fixunstfDI (a);
}
#endif

#if defined(L_fixunsxfdi) && LIBGCC2_HAS_XF_MODE
UDWtype
__fixunsxfDI (XFtype a)
{
  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  const XFtype b = (a / Wtype_MAXp1_F);
  /* Convert that to fixed (but not to DWtype!),
     and shift it into the high word.  */
  UDWtype v = (UWtype) b;
  v <<= W_TYPE_SIZE;
  /* Remove high part from the XFtype, leaving the low part as flonum.  */
  a -= (XFtype)v;
  /* Convert that to fixed (but not to DWtype!) and add it in.
     Sometimes A comes out negative.  This is significant, since
     A has more bits than a long int does.  */
  if (a < 0)
    v -= (UWtype) (- a);
  else
    v += (UWtype) a;
  return v;
}
#endif

#if defined(L_fixxfdi) && LIBGCC2_HAS_XF_MODE
DWtype
__fixxfdi (XFtype a)
{
  if (a < 0)
    return - __fixunsxfDI (-a);
  return __fixunsxfDI (a);
}
#endif

#if defined(L_fixunsdfdi) && LIBGCC2_HAS_DF_MODE
UDWtype
__fixunsdfDI (DFtype a)
{
  /* Get high part of result.  The division here will just moves the radix
     point and will not cause any rounding.  Then the conversion to integral
     type chops result as desired.  */
  const UWtype hi = a / Wtype_MAXp1_F;

  /* Get low part of result.  Convert `hi' to floating type and scale it back,
     then subtract this from the number being converted.  This leaves the low
     part.  Convert that to integral type.  */
  const UWtype lo = a - (DFtype) hi * Wtype_MAXp1_F;

  /* Assemble result from the two parts.  */
  return ((UDWtype) hi << W_TYPE_SIZE) | lo;
}
#endif

#if defined(L_fixdfdi) && LIBGCC2_HAS_DF_MODE
DWtype
__fixdfdi (DFtype a)
{
  if (a < 0)
    return - __fixunsdfDI (-a);
  return __fixunsdfDI (a);
}
#endif

#if defined(L_fixunssfdi) && LIBGCC2_HAS_SF_MODE
UDWtype
__fixunssfDI (SFtype a)
{
#if LIBGCC2_HAS_DF_MODE
  /* Convert the SFtype to a DFtype, because that is surely not going
     to lose any bits.  Some day someone else can write a faster version
     that avoids converting to DFtype, and verify it really works right.  */
  const DFtype dfa = a;

  /* Get high part of result.  The division here will just moves the radix
     point and will not cause any rounding.  Then the conversion to integral
     type chops result as desired.  */
  const UWtype hi = dfa / Wtype_MAXp1_F;

  /* Get low part of result.  Convert `hi' to floating type and scale it back,
     then subtract this from the number being converted.  This leaves the low
     part.  Convert that to integral type.  */
  const UWtype lo = dfa - (DFtype) hi * Wtype_MAXp1_F;

  /* Assemble result from the two parts.  */
  return ((UDWtype) hi << W_TYPE_SIZE) | lo;
#elif FLT_MANT_DIG < W_TYPE_SIZE
  if (a < 1)
    return 0;
  if (a < Wtype_MAXp1_F)
    return (UWtype)a;
  if (a < Wtype_MAXp1_F * Wtype_MAXp1_F)
    {
      /* Since we know that there are fewer significant bits in the SFmode
	 quantity than in a word, we know that we can convert out all the
	 significant bits in one step, and thus avoid losing bits.  */

      /* ??? This following loop essentially performs frexpf.  If we could
	 use the real libm function, or poke at the actual bits of the fp
	 format, it would be significantly faster.  */

      UWtype shift = 0, counter;
      SFtype msb;

      a /= Wtype_MAXp1_F;
      for (counter = W_TYPE_SIZE / 2; counter != 0; counter >>= 1)
	{
	  SFtype counterf = (UWtype)1 << counter;
	  if (a >= counterf)
	    {
	      shift |= counter;
	      a /= counterf;
	    }
	}

      /* Rescale into the range of one word, extract the bits of that
	 one word, and shift the result into position.  */
      a *= Wtype_MAXp1_F;
      counter = a;
      return (DWtype)counter << shift;
    }
  return -1;
#else
# error
#endif
}
#endif

#if defined(L_fixsfdi) && LIBGCC2_HAS_SF_MODE
DWtype
__fixsfdi (SFtype a)
{
  if (a < 0)
    return - __fixunssfDI (-a);
  return __fixunssfDI (a);
}
#endif

#if defined(L_floatdixf) && LIBGCC2_HAS_XF_MODE
XFtype
__floatdixf (DWtype u)
{
#if W_TYPE_SIZE > __LIBGCC_XF_MANT_DIG__
# error
#endif
  XFtype d = (Wtype) (u >> W_TYPE_SIZE);
  d *= Wtype_MAXp1_F;
  d += (UWtype)u;
  return d;
}
#endif

#if defined(L_floatundixf) && LIBGCC2_HAS_XF_MODE
XFtype
__floatundixf (UDWtype u)
{
#if W_TYPE_SIZE > __LIBGCC_XF_MANT_DIG__
# error
#endif
  XFtype d = (UWtype) (u >> W_TYPE_SIZE);
  d *= Wtype_MAXp1_F;
  d += (UWtype)u;
  return d;
}
#endif

#if defined(L_floatditf) && LIBGCC2_HAS_TF_MODE
TFtype
__floatditf (DWtype u)
{
#if W_TYPE_SIZE > __LIBGCC_TF_MANT_DIG__
# error
#endif
  TFtype d = (Wtype) (u >> W_TYPE_SIZE);
  d *= Wtype_MAXp1_F;
  d += (UWtype)u;
  return d;
}
#endif

#if defined(L_floatunditf) && LIBGCC2_HAS_TF_MODE
TFtype
__floatunditf (UDWtype u)
{
#if W_TYPE_SIZE > __LIBGCC_TF_MANT_DIG__
# error
#endif
  TFtype d = (UWtype) (u >> W_TYPE_SIZE);
  d *= Wtype_MAXp1_F;
  d += (UWtype)u;
  return d;
}
#endif

#if (defined(L_floatdisf) && LIBGCC2_HAS_SF_MODE)	\
     || (defined(L_floatdidf) && LIBGCC2_HAS_DF_MODE)
#define DI_SIZE (W_TYPE_SIZE * 2)
#define F_MODE_OK(SIZE) \
  (SIZE < DI_SIZE							\
   && SIZE > (DI_SIZE - SIZE + FSSIZE)					\
   && !AVOID_FP_TYPE_CONVERSION(SIZE))
#if defined(L_floatdisf)
#define FUNC __floatdisf
#define FSTYPE SFtype
#define FSSIZE __LIBGCC_SF_MANT_DIG__
#else
#define FUNC __floatdidf
#define FSTYPE DFtype
#define FSSIZE __LIBGCC_DF_MANT_DIG__
#endif

FSTYPE
FUNC (DWtype u)
{
#if FSSIZE >= W_TYPE_SIZE
  /* When the word size is small, we never get any rounding error.  */
  FSTYPE f = (Wtype) (u >> W_TYPE_SIZE);
  f *= Wtype_MAXp1_F;
  f += (UWtype)u;
  return f;
#elif (LIBGCC2_HAS_DF_MODE && F_MODE_OK (__LIBGCC_DF_MANT_DIG__))	\
     || (LIBGCC2_HAS_XF_MODE && F_MODE_OK (__LIBGCC_XF_MANT_DIG__))	\
     || (LIBGCC2_HAS_TF_MODE && F_MODE_OK (__LIBGCC_TF_MANT_DIG__))

#if (LIBGCC2_HAS_DF_MODE && F_MODE_OK (__LIBGCC_DF_MANT_DIG__))
# define FSIZE __LIBGCC_DF_MANT_DIG__
# define FTYPE DFtype
#elif (LIBGCC2_HAS_XF_MODE && F_MODE_OK (__LIBGCC_XF_MANT_DIG__))
# define FSIZE __LIBGCC_XF_MANT_DIG__
# define FTYPE XFtype
#elif (LIBGCC2_HAS_TF_MODE && F_MODE_OK (__LIBGCC_TF_MANT_DIG__))
# define FSIZE __LIBGCC_TF_MANT_DIG__
# define FTYPE TFtype
#else
# error
#endif

#define REP_BIT ((UDWtype) 1 << (DI_SIZE - FSIZE))

  /* Protect against double-rounding error.
     Represent any low-order bits, that might be truncated by a bit that
     won't be lost.  The bit can go in anywhere below the rounding position
     of the FSTYPE.  A fixed mask and bit position handles all usual
     configurations.  */
  if (! (- ((DWtype) 1 << FSIZE) < u
	 && u < ((DWtype) 1 << FSIZE)))
    {
      if ((UDWtype) u & (REP_BIT - 1))
	{
	  u &= ~ (REP_BIT - 1);
	  u |= REP_BIT;
	}
    }

  /* Do the calculation in a wider type so that we don't lose any of
     the precision of the high word while multiplying it.  */
  FTYPE f = (Wtype) (u >> W_TYPE_SIZE);
  f *= Wtype_MAXp1_F;
  f += (UWtype)u;
  return (FSTYPE) f;
#else
#if FSSIZE >= W_TYPE_SIZE - 2
# error
#endif
  /* Finally, the word size is larger than the number of bits in the
     required FSTYPE, and we've got no suitable wider type.  The only
     way to avoid double rounding is to special case the
     extraction.  */

  /* If there are no high bits set, fall back to one conversion.  */
  if ((Wtype)u == u)
    return (FSTYPE)(Wtype)u;

  /* Otherwise, find the power of two.  */
  Wtype hi = u >> W_TYPE_SIZE;
  if (hi < 0)
    hi = -(UWtype) hi;

  UWtype count, shift;
#if !defined (COUNT_LEADING_ZEROS_0) || COUNT_LEADING_ZEROS_0 != W_TYPE_SIZE
  if (hi == 0)
    count = W_TYPE_SIZE;
  else
#endif
  count_leading_zeros (count, hi);

  /* No leading bits means u == minimum.  */
  if (count == 0)
    return Wtype_MAXp1_F * (FSTYPE) (hi | ((UWtype) u != 0));

  shift = 1 + W_TYPE_SIZE - count;

  /* Shift down the most significant bits.  */
  hi = u >> shift;

  /* If we lost any nonzero bits, set the lsb to ensure correct rounding.  */
  if ((UWtype)u << (W_TYPE_SIZE - shift))
    hi |= 1;

  /* Convert the one word of data, and rescale.  */
  FSTYPE f = hi, e;
  if (shift == W_TYPE_SIZE)
    e = Wtype_MAXp1_F;
  /* The following two cases could be merged if we knew that the target
     supported a native unsigned->float conversion.  More often, we only
     have a signed conversion, and have to add extra fixup code.  */
  else if (shift == W_TYPE_SIZE - 1)
    e = Wtype_MAXp1_F / 2;
  else
    e = (Wtype)1 << shift;
  return f * e;
#endif
}
#endif

#if (defined(L_floatundisf) && LIBGCC2_HAS_SF_MODE)	\
     || (defined(L_floatundidf) && LIBGCC2_HAS_DF_MODE)
#define DI_SIZE (W_TYPE_SIZE * 2)
#define F_MODE_OK(SIZE) \
  (SIZE < DI_SIZE							\
   && SIZE > (DI_SIZE - SIZE + FSSIZE)					\
   && !AVOID_FP_TYPE_CONVERSION(SIZE))
#if defined(L_floatundisf)
#define FUNC __floatundisf
#define FSTYPE SFtype
#define FSSIZE __LIBGCC_SF_MANT_DIG__
#else
#define FUNC __floatundidf
#define FSTYPE DFtype
#define FSSIZE __LIBGCC_DF_MANT_DIG__
#endif

FSTYPE
FUNC (UDWtype u)
{
#if FSSIZE >= W_TYPE_SIZE
  /* When the word size is small, we never get any rounding error.  */
  FSTYPE f = (UWtype) (u >> W_TYPE_SIZE);
  f *= Wtype_MAXp1_F;
  f += (UWtype)u;
  return f;
#elif (LIBGCC2_HAS_DF_MODE && F_MODE_OK (__LIBGCC_DF_MANT_DIG__))	\
     || (LIBGCC2_HAS_XF_MODE && F_MODE_OK (__LIBGCC_XF_MANT_DIG__))	\
     || (LIBGCC2_HAS_TF_MODE && F_MODE_OK (__LIBGCC_TF_MANT_DIG__))

#if (LIBGCC2_HAS_DF_MODE && F_MODE_OK (__LIBGCC_DF_MANT_DIG__))
# define FSIZE __LIBGCC_DF_MANT_DIG__
# define FTYPE DFtype
#elif (LIBGCC2_HAS_XF_MODE && F_MODE_OK (__LIBGCC_XF_MANT_DIG__))
# define FSIZE __LIBGCC_XF_MANT_DIG__
# define FTYPE XFtype
#elif (LIBGCC2_HAS_TF_MODE && F_MODE_OK (__LIBGCC_TF_MANT_DIG__))
# define FSIZE __LIBGCC_TF_MANT_DIG__
# define FTYPE TFtype
#else
# error
#endif

#define REP_BIT ((UDWtype) 1 << (DI_SIZE - FSIZE))

  /* Protect against double-rounding error.
     Represent any low-order bits, that might be truncated by a bit that
     won't be lost.  The bit can go in anywhere below the rounding position
     of the FSTYPE.  A fixed mask and bit position handles all usual
     configurations.  */
  if (u >= ((UDWtype) 1 << FSIZE))
    {
      if ((UDWtype) u & (REP_BIT - 1))
	{
	  u &= ~ (REP_BIT - 1);
	  u |= REP_BIT;
	}
    }

  /* Do the calculation in a wider type so that we don't lose any of
     the precision of the high word while multiplying it.  */
  FTYPE f = (UWtype) (u >> W_TYPE_SIZE);
  f *= Wtype_MAXp1_F;
  f += (UWtype)u;
  return (FSTYPE) f;
#else
#if FSSIZE == W_TYPE_SIZE - 1
# error
#endif
  /* Finally, the word size is larger than the number of bits in the
     required FSTYPE, and we've got no suitable wider type.  The only
     way to avoid double rounding is to special case the
     extraction.  */

  /* If there are no high bits set, fall back to one conversion.  */
  if ((UWtype)u == u)
    return (FSTYPE)(UWtype)u;

  /* Otherwise, find the power of two.  */
  UWtype hi = u >> W_TYPE_SIZE;

  UWtype count, shift;
  count_leading_zeros (count, hi);

  shift = W_TYPE_SIZE - count;

  /* Shift down the most significant bits.  */
  hi = u >> shift;

  /* If we lost any nonzero bits, set the lsb to ensure correct rounding.  */
  if ((UWtype)u << (W_TYPE_SIZE - shift))
    hi |= 1;

  /* Convert the one word of data, and rescale.  */
  FSTYPE f = hi, e;
  if (shift == W_TYPE_SIZE)
    e = Wtype_MAXp1_F;
  /* The following two cases could be merged if we knew that the target
     supported a native unsigned->float conversion.  More often, we only
     have a signed conversion, and have to add extra fixup code.  */
  else if (shift == W_TYPE_SIZE - 1)
    e = Wtype_MAXp1_F / 2;
  else
    e = (Wtype)1 << shift;
  return f * e;
#endif
}
#endif

#if defined(L_fixunsxfsi) && LIBGCC2_HAS_XF_MODE
UWtype
__fixunsxfSI (XFtype a)
{
  if (a >= - (DFtype) Wtype_MIN)
    return (Wtype) (a + Wtype_MIN) - Wtype_MIN;
  return (Wtype) a;
}
#endif

#if defined(L_fixunsdfsi) && LIBGCC2_HAS_DF_MODE
UWtype
__fixunsdfSI (DFtype a)
{
  if (a >= - (DFtype) Wtype_MIN)
    return (Wtype) (a + Wtype_MIN) - Wtype_MIN;
  return (Wtype) a;
}
#endif

#if defined(L_fixunssfsi) && LIBGCC2_HAS_SF_MODE
UWtype
__fixunssfSI (SFtype a)
{
  if (a >= - (SFtype) Wtype_MIN)
    return (Wtype) (a + Wtype_MIN) - Wtype_MIN;
  return (Wtype) a;
}
#endif

/* Integer power helper used from __builtin_powi for non-constant
   exponents.  */

#if (defined(L_powisf2) && LIBGCC2_HAS_SF_MODE) \
    || (defined(L_powidf2) && LIBGCC2_HAS_DF_MODE) \
    || (defined(L_powixf2) && LIBGCC2_HAS_XF_MODE) \
    || (defined(L_powitf2) && LIBGCC2_HAS_TF_MODE)
# if defined(L_powisf2)
#  define TYPE SFtype
#  define NAME __powisf2
# elif defined(L_powidf2)
#  define TYPE DFtype
#  define NAME __powidf2
# elif defined(L_powixf2)
#  define TYPE XFtype
#  define NAME __powixf2
# elif defined(L_powitf2)
#  define TYPE TFtype
#  define NAME __powitf2
# endif

#undef int
#undef unsigned
TYPE
NAME (TYPE x, int m)
{
  unsigned int n = m < 0 ? -(unsigned int) m : (unsigned int) m;
  TYPE y = n % 2 ? x : 1;
  while (n >>= 1)
    {
      x = x * x;
      if (n % 2)
	y = y * x;
    }
  return m < 0 ? 1/y : y;
}

#endif

#if((defined(L_mulhc3) || defined(L_divhc3)) && LIBGCC2_HAS_HF_MODE) \
    || ((defined(L_mulsc3) || defined(L_divsc3)) && LIBGCC2_HAS_SF_MODE) \
    || ((defined(L_muldc3) || defined(L_divdc3)) && LIBGCC2_HAS_DF_MODE) \
    || ((defined(L_mulxc3) || defined(L_divxc3)) && LIBGCC2_HAS_XF_MODE) \
    || ((defined(L_multc3) || defined(L_divtc3)) && LIBGCC2_HAS_TF_MODE)

#undef float
#undef double
#undef long

#if defined(L_mulhc3) || defined(L_divhc3)
# define MTYPE	HFtype
# define CTYPE	HCtype
# define AMTYPE SFtype
# define MODE	hc
# define CEXT	__LIBGCC_HF_FUNC_EXT__
# define NOTRUNC (!__LIBGCC_HF_EXCESS_PRECISION__)
#elif defined(L_mulsc3) || defined(L_divsc3)
# define MTYPE	SFtype
# define CTYPE	SCtype
# define AMTYPE DFtype
# define MODE	sc
# define CEXT	__LIBGCC_SF_FUNC_EXT__
# define NOTRUNC (!__LIBGCC_SF_EXCESS_PRECISION__)
# define RBIG	(__LIBGCC_SF_MAX__ / 2)
# define RMIN	(__LIBGCC_SF_MIN__)
# define RMIN2	(__LIBGCC_SF_EPSILON__)
# define RMINSCAL (1 / __LIBGCC_SF_EPSILON__)
# define RMAX2	(RBIG * RMIN2)
#elif defined(L_muldc3) || defined(L_divdc3)
# define MTYPE	DFtype
# define CTYPE	DCtype
# define MODE	dc
# define CEXT	__LIBGCC_DF_FUNC_EXT__
# define NOTRUNC (!__LIBGCC_DF_EXCESS_PRECISION__)
# define RBIG	(__LIBGCC_DF_MAX__ / 2)
# define RMIN	(__LIBGCC_DF_MIN__)
# define RMIN2	(__LIBGCC_DF_EPSILON__)
# define RMINSCAL (1 / __LIBGCC_DF_EPSILON__)
# define RMAX2  (RBIG * RMIN2)
#elif defined(L_mulxc3) || defined(L_divxc3)
# define MTYPE	XFtype
# define CTYPE	XCtype
# define MODE	xc
# define CEXT	__LIBGCC_XF_FUNC_EXT__
# define NOTRUNC (!__LIBGCC_XF_EXCESS_PRECISION__)
# define RBIG	(__LIBGCC_XF_MAX__ / 2)
# define RMIN	(__LIBGCC_XF_MIN__)
# define RMIN2	(__LIBGCC_XF_EPSILON__)
# define RMINSCAL (1 / __LIBGCC_XF_EPSILON__)
# define RMAX2	(RBIG * RMIN2)
#elif defined(L_multc3) || defined(L_divtc3)
# define MTYPE	TFtype
# define CTYPE	TCtype
# define MODE	tc
# define CEXT	__LIBGCC_TF_FUNC_EXT__
# define NOTRUNC (!__LIBGCC_TF_EXCESS_PRECISION__)
# if __LIBGCC_TF_MANT_DIG__ == 106
#  define RBIG	(__LIBGCC_DF_MAX__ / 2)
#  define RMIN	(__LIBGCC_DF_MIN__)
#  define RMIN2  (__LIBGCC_DF_EPSILON__)
#  define RMINSCAL (1 / __LIBGCC_DF_EPSILON__)
# else
#  define RBIG	(__LIBGCC_TF_MAX__ / 2)
#  define RMIN	(__LIBGCC_TF_MIN__)
#  define RMIN2	(__LIBGCC_TF_EPSILON__)
#  define RMINSCAL (1 / __LIBGCC_TF_EPSILON__)
# endif
# define RMAX2	(RBIG * RMIN2)
#else
# error
#endif

#define CONCAT3(A,B,C)	_CONCAT3(A,B,C)
#define _CONCAT3(A,B,C)	A##B##C

#define CONCAT2(A,B)	_CONCAT2(A,B)
#define _CONCAT2(A,B)	A##B

#define isnan(x)	__builtin_isnan (x)
#define isfinite(x)	__builtin_isfinite (x)
#define isinf(x)	__builtin_isinf (x)

#undef INFINITY
#define INFINITY	CONCAT2(__builtin_huge_val, CEXT) ()
#define I		1i

/* Helpers to make the following code slightly less gross.  */
#define COPYSIGN	CONCAT2(__builtin_copysign, CEXT)
#define FABS		CONCAT2(__builtin_fabs, CEXT)

/* Verify that MTYPE matches up with CEXT.  */
extern void *compile_type_assert[sizeof(INFINITY) == sizeof(MTYPE) ? 1 : -1];

/* Ensure that we've lost any extra precision.  */
#if NOTRUNC
# define TRUNC(x)
#else
# define TRUNC(x)	__asm__ ("" : "=m"(x) : "m"(x))
#endif

#if defined(L_mulhc3) || defined(L_mulsc3) || defined(L_muldc3) \
    || defined(L_mulxc3) || defined(L_multc3)

CTYPE
CONCAT3(__mul,MODE,3) (MTYPE a, MTYPE b, MTYPE c, MTYPE d)
{
  MTYPE ac, bd, ad, bc, x, y;
  CTYPE res;

  ac = a * c;
  bd = b * d;
  ad = a * d;
  bc = b * c;

  TRUNC (ac);
  TRUNC (bd);
  TRUNC (ad);
  TRUNC (bc);

  x = ac - bd;
  y = ad + bc;

  if (isnan (x) && isnan (y))
    {
      /* Recover infinities that computed as NaN + iNaN.  */
      _Bool recalc = 0;
      if (isinf (a) || isinf (b))
	{
	  /* z is infinite.  "Box" the infinity and change NaNs in
	     the other factor to 0.  */
	  a = COPYSIGN (isinf (a) ? 1 : 0, a);
	  b = COPYSIGN (isinf (b) ? 1 : 0, b);
	  if (isnan (c)) c = COPYSIGN (0, c);
	  if (isnan (d)) d = COPYSIGN (0, d);
          recalc = 1;
	}
     if (isinf (c) || isinf (d))
	{
	  /* w is infinite.  "Box" the infinity and change NaNs in
	     the other factor to 0.  */
	  c = COPYSIGN (isinf (c) ? 1 : 0, c);
	  d = COPYSIGN (isinf (d) ? 1 : 0, d);
	  if (isnan (a)) a = COPYSIGN (0, a);
	  if (isnan (b)) b = COPYSIGN (0, b);
	  recalc = 1;
	}
     if (!recalc
	  && (isinf (ac) || isinf (bd)
	      || isinf (ad) || isinf (bc)))
	{
	  /* Recover infinities from overflow by changing NaNs to 0.  */
	  if (isnan (a)) a = COPYSIGN (0, a);
	  if (isnan (b)) b = COPYSIGN (0, b);
	  if (isnan (c)) c = COPYSIGN (0, c);
	  if (isnan (d)) d = COPYSIGN (0, d);
	  recalc = 1;
	}
      if (recalc)
	{
	  x = INFINITY * (a * c - b * d);
	  y = INFINITY * (a * d + b * c);
	}
    }

  __real__ res = x;
  __imag__ res = y;
  return res;
}
#endif /* complex multiply */

#if defined(L_divhc3) || defined(L_divsc3) || defined(L_divdc3) \
    || defined(L_divxc3) || defined(L_divtc3)

CTYPE
CONCAT3(__div,MODE,3) (MTYPE a, MTYPE b, MTYPE c, MTYPE d)
{
#if defined(L_divhc3)						\
  || (defined(L_divsc3) && defined(__LIBGCC_HAVE_HWDBL__) )

  /* Half precision is handled with float precision.
     float is handled with double precision when double precision
     hardware is available.
     Due to the additional precision, the simple complex divide
     method (without Smith's method) is sufficient to get accurate
     answers and runs slightly faster than Smith's method.  */

  AMTYPE aa, bb, cc, dd;
  AMTYPE denom;
  MTYPE x, y;
  CTYPE res;
  aa = a;
  bb = b;
  cc = c;
  dd = d;

  denom = (cc * cc) + (dd * dd);
  x = ((aa * cc) + (bb * dd)) / denom;
  y = ((bb * cc) - (aa * dd)) / denom;

#else
  MTYPE denom, ratio, x, y;
  CTYPE res;

  /* double, extended, long double have significant potential
     underflow/overflow errors that can be greatly reduced with
     a limited number of tests and adjustments.  float is handled
     the same way when no HW double is available.
  */

  /* Scale by max(c,d) to reduce chances of denominator overflowing.  */
  if (FABS (c) < FABS (d))
    {
      /* Prevent underflow when denominator is near max representable.  */
      if (FABS (d) >= RBIG)
	{
	  a = a / 2;
	  b = b / 2;
	  c = c / 2;
	  d = d / 2;
	}
      /* Avoid overflow/underflow issues when c and d are small.
	 Scaling up helps avoid some underflows.
	 No new overflow possible since c&d < RMIN2.  */
      if (FABS (d) < RMIN2)
	{
	  a = a * RMINSCAL;
	  b = b * RMINSCAL;
	  c = c * RMINSCAL;
	  d = d * RMINSCAL;
	}
      else
	{
	  if (((FABS (a) < RMIN) && (FABS (b) < RMAX2) && (FABS (d) < RMAX2))
	      || ((FABS (b) < RMIN) && (FABS (a) < RMAX2)
		  && (FABS (d) < RMAX2)))
	    {
	      a = a * RMINSCAL;
	      b = b * RMINSCAL;
	      c = c * RMINSCAL;
	      d = d * RMINSCAL;
	    }
	}
      ratio = c / d;
      denom = (c * ratio) + d;
      /* Choose alternate order of computation if ratio is subnormal.  */
      if (FABS (ratio) > RMIN)
	{
	  x = ((a * ratio) + b) / denom;
	  y = ((b * ratio) - a) / denom;
	}
      else
	{
	  x = ((c * (a / d)) + b) / denom;
	  y = ((c * (b / d)) - a) / denom;
	}
    }
  else
    {
      /* Prevent underflow when denominator is near max representable.  */
      if (FABS (c) >= RBIG)
	{
	  a = a / 2;
	  b = b / 2;
	  c = c / 2;
	  d = d / 2;
	}
      /* Avoid overflow/underflow issues when both c and d are small.
	 Scaling up helps avoid some underflows.
	 No new overflow possible since both c&d are less than RMIN2.  */
      if (FABS (c) < RMIN2)
	{
	  a = a * RMINSCAL;
	  b = b * RMINSCAL;
	  c = c * RMINSCAL;
	  d = d * RMINSCAL;
	}
      else
	{
	  if (((FABS (a) < RMIN) && (FABS (b) < RMAX2) && (FABS (c) < RMAX2))
	      || ((FABS (b) < RMIN) && (FABS (a) < RMAX2)
		  && (FABS (c) < RMAX2)))
	    {
	      a = a * RMINSCAL;
	      b = b * RMINSCAL;
	      c = c * RMINSCAL;
	      d = d * RMINSCAL;
	    }
	}
      ratio = d / c;
      denom = (d * ratio) + c;
      /* Choose alternate order of computation if ratio is subnormal.  */
      if (FABS (ratio) > RMIN)
	{
	  x = ((b * ratio) + a) / denom;
	  y = (b - (a * ratio)) / denom;
	}
      else
	{
	  x = (a + (d * (b / c))) / denom;
	  y = (b - (d * (a / c))) / denom;
	}
    }
#endif

  /* Recover infinities and zeros that computed as NaN+iNaN; the only
     cases are nonzero/zero, infinite/finite, and finite/infinite.  */
  if (isnan (x) && isnan (y))
    {
      if (c == 0.0 && d == 0.0 && (!isnan (a) || !isnan (b)))
	{
	  x = COPYSIGN (INFINITY, c) * a;
	  y = COPYSIGN (INFINITY, c) * b;
	}
      else if ((isinf (a) || isinf (b)) && isfinite (c) && isfinite (d))
	{
	  a = COPYSIGN (isinf (a) ? 1 : 0, a);
	  b = COPYSIGN (isinf (b) ? 1 : 0, b);
	  x = INFINITY * (a * c + b * d);
	  y = INFINITY * (b * c - a * d);
	}
      else if ((isinf (c) || isinf (d)) && isfinite (a) && isfinite (b))
	{
	  c = COPYSIGN (isinf (c) ? 1 : 0, c);
	  d = COPYSIGN (isinf (d) ? 1 : 0, d);
	  x = 0.0 * (a * c + b * d);
	  y = 0.0 * (b * c - a * d);
	}
    }

  __real__ res = x;
  __imag__ res = y;
  return res;
}
#endif /* complex divide */

#endif /* all complex float routines */

/* From here on down, the routines use normal data types.  */

#define SItype bogus_type
#define USItype bogus_type
#define DItype bogus_type
#define UDItype bogus_type
#define SFtype bogus_type
#define DFtype bogus_type
#undef Wtype
#undef UWtype
#undef HWtype
#undef UHWtype
#undef DWtype
#undef UDWtype

#undef char
#undef short
#undef int
#undef long
#undef unsigned
#undef float
#undef double

#ifdef L__gcc_bcmp

/* Like bcmp except the sign is meaningful.
   Result is negative if S1 is less than S2,
   positive if S1 is greater, 0 if S1 and S2 are equal.  */

int
__gcc_bcmp (const unsigned char *s1, const unsigned char *s2, size_t size)
{
  while (size > 0)
    {
      const unsigned char c1 = *s1++, c2 = *s2++;
      if (c1 != c2)
	return c1 - c2;
      size--;
    }
  return 0;
}

#endif

/* __eprintf used to be used by GCC's private version of <assert.h>.
   We no longer provide that header, but this routine remains in libgcc.a
   for binary backward compatibility.  Note that it is not included in
   the shared version of libgcc.  */
#ifdef L_eprintf
#ifndef inhibit_libc

#undef NULL /* Avoid errors if stdio.h and our stddef.h mismatch.  */
#include <stdio.h>

void
__eprintf (const char *string, const char *expression,
	   unsigned int line, const char *filename)
{
  fprintf (stderr, string, expression, line, filename);
  fflush (stderr);
  abort ();
}

#endif
#endif


#ifdef L_clear_cache
/* Clear part of an instruction cache.  */

void
__clear_cache (void *beg __attribute__((__unused__)),
	       void *end __attribute__((__unused__)))
{
#ifdef CLEAR_INSN_CACHE
  /* Cast the void* pointers to char* as some implementations
     of the macro assume the pointers can be subtracted from
     one another.  */
  CLEAR_INSN_CACHE ((char *) beg, (char *) end);
#endif /* CLEAR_INSN_CACHE */
}

#endif /* L_clear_cache */

#ifdef L_trampoline

/* Jump to a trampoline, loading the static chain address.  */

#if defined(WINNT) && ! defined(__CYGWIN__)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
int getpagesize (void);
int mprotect (char *,int, int);

int
getpagesize (void)
{
#ifdef _ALPHA_
  return 8192;
#else
  return 4096;
#endif
}

int
mprotect (char *addr, int len, int prot)
{
  DWORD np, op;

  if (prot == 7)
    np = 0x40;
  else if (prot == 5)
    np = 0x20;
  else if (prot == 4)
    np = 0x10;
  else if (prot == 3)
    np = 0x04;
  else if (prot == 1)
    np = 0x02;
  else if (prot == 0)
    np = 0x01;
  else
    return -1;

  if (VirtualProtect (addr, len, np, &op))
    return 0;
  else
    return -1;
}

#endif /* WINNT && ! __CYGWIN__ */

#ifdef TRANSFER_FROM_TRAMPOLINE
TRANSFER_FROM_TRAMPOLINE
#endif
#endif /* L_trampoline */

#ifndef __CYGWIN__
#ifdef L__main

#include "gbl-ctors.h"

/* Some systems use __main in a way incompatible with its use in gcc, in these
   cases use the macros NAME__MAIN to give a quoted symbol and SYMBOL__MAIN to
   give the same symbol without quotes for an alternative entry point.  You
   must define both, or neither.  */
#ifndef NAME__MAIN
#define NAME__MAIN "__main"
#define SYMBOL__MAIN __main
#endif

#if defined (__LIBGCC_INIT_SECTION_ASM_OP__) \
    || defined (__LIBGCC_INIT_ARRAY_SECTION_ASM_OP__)
#undef HAS_INIT_SECTION
#define HAS_INIT_SECTION
#endif

#if !defined (HAS_INIT_SECTION) || !defined (OBJECT_FORMAT_ELF)

/* Some ELF crosses use crtstuff.c to provide __CTOR_LIST__, but use this
   code to run constructors.  In that case, we need to handle EH here, too.
   But MINGW32 is special because it handles CRTSTUFF and EH on its own.  */

#ifdef __MINGW32__
#undef __LIBGCC_EH_FRAME_SECTION_NAME__
#endif

#ifdef __LIBGCC_EH_FRAME_SECTION_NAME__
#include "unwind-dw2-fde.h"
extern unsigned char __EH_FRAME_BEGIN__[];
#endif

/* Run all the global destructors on exit from the program.  */

void
__do_global_dtors (void)
{
#ifdef DO_GLOBAL_DTORS_BODY
  DO_GLOBAL_DTORS_BODY;
#else
  static func_ptr *p = __DTOR_LIST__ + 1;
  while (*p)
    {
      p++;
      (*(p-1)) ();
    }
#endif
#if defined (__LIBGCC_EH_FRAME_SECTION_NAME__) && !defined (HAS_INIT_SECTION)
  {
    static int completed = 0;
    if (! completed)
      {
	completed = 1;
	__deregister_frame_info (__EH_FRAME_BEGIN__);
      }
  }
#endif
}
#endif

#ifndef HAS_INIT_SECTION
/* Run all the global constructors on entry to the program.  */

void
__do_global_ctors (void)
{
#ifdef __LIBGCC_EH_FRAME_SECTION_NAME__
  {
    static struct object object;
    __register_frame_info (__EH_FRAME_BEGIN__, &object);
  }
#endif
  DO_GLOBAL_CTORS_BODY;
  atexit (__do_global_dtors);
}
#endif /* no HAS_INIT_SECTION */

#if !defined (HAS_INIT_SECTION) || defined (INVOKE__main)
/* Subroutine called automatically by `main'.
   Compiling a global function named `main'
   produces an automatic call to this function at the beginning.

   For many systems, this routine calls __do_global_ctors.
   For systems which support a .init section we use the .init section
   to run __do_global_ctors, so we need not do anything here.  */

extern void SYMBOL__MAIN (void);
void
SYMBOL__MAIN (void)
{
  /* Support recursive calls to `main': run initializers just once.  */
  static int initialized;
  if (! initialized)
    {
      initialized = 1;
      __do_global_ctors ();
    }
}
#endif /* no HAS_INIT_SECTION or INVOKE__main */

#endif /* L__main */
#endif /* __CYGWIN__ */

#ifdef L_ctors

#include "gbl-ctors.h"

/* Provide default definitions for the lists of constructors and
   destructors, so that we don't get linker errors.  These symbols are
   intentionally bss symbols, so that gld and/or collect will provide
   the right values.  */

/* We declare the lists here with two elements each,
   so that they are valid empty lists if no other definition is loaded.

   If we are using the old "set" extensions to have the gnu linker
   collect ctors and dtors, then we __CTOR_LIST__ and __DTOR_LIST__
   must be in the bss/common section.

   Long term no port should use those extensions.  But many still do.  */
#if !defined(__LIBGCC_INIT_SECTION_ASM_OP__)
#if defined (TARGET_ASM_CONSTRUCTOR) || defined (USE_COLLECT2)
func_ptr __CTOR_LIST__[2] = {0, 0};
func_ptr __DTOR_LIST__[2] = {0, 0};
#else
func_ptr __CTOR_LIST__[2];
func_ptr __DTOR_LIST__[2];
#endif
#endif /* no __LIBGCC_INIT_SECTION_ASM_OP__ */
#endif /* L_ctors */
#endif /* LIBGCC2_UNITS_PER_WORD <= MIN_UNITS_PER_WORD */
