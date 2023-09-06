/* Software floating-point emulation.
   Compute powers of 10 into _BitInt.

   Copyright (C) 2023 Free Software Foundation, Inc.

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

#include "soft-fp.h"
#include "bitint.h"

#ifdef __BITINT_MAXWIDTH__
# define BIL_VAL(x) ((UBILtype) (x))
# if BIL_TYPE_SIZE == 64
#  define BIL_PAIR(x, y) ((BIL_VAL (x) << 32) | BIL_VAL (y))
#  define BIL_OFF(x, y) (x)
# elif BIL_TYPE_SIZE == 32
#  if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
#   define BIL_PAIR(x, y) BIL_VAL (x), BIL_VAL (y)
#  else
#   define BIL_PAIR(x, y) BIL_VAL (y), BIL_VAL (x)
#  endif
#  define BIL_OFF(x, y) (y)
# else
#  error Unsupported _BitInt limb size
# endif
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
# define BIL_SET2(a, b) a, b
# define BIL_SET3(a, b, c) a, b, c
# define BIL_SET4(a, b, c, d) a, b, c, d
# define BIL_SET5(a, b, c, d, e) a, b, c, d, e
# define BIL_SET6(a, b, c, d, e, f) a, b, c, d, e, f
# define BIL_SET7(a, b, c, d, e, f, g) a, b, c, d, e, f, g
# define BIL_SET8(a, b, c, d, e, f, g, h) a, b, c, d, e, f, g, h
# define BIL_SET9(a, b, c, d, e, f, g, h, i) a, b, c, d, e, f, g, h, i
# define BIL_SET10(a, b, c, d, e, f, g, h, i, j) a, b, c, d, e, f, g, h, i, j
# define BIL_SET11(a, b, c, d, e, f, g, h, i, j, k) \
  a, b, c, d, e, f, g, h, i, j, k
# define BIL_SET12(a, b, c, d, e, f, g, h, i, j, k, l) \
  a, b, c, d, e, f, g, h, i, j, k, l
# define BIL_SET13(a, b, c, d, e, f, g, h, i, j, k, l, m) \
  a, b, c, d, e, f, g, h, i, j, k, l, m
# define BIL_SET14(a, b, c, d, e, f, g, h, i, j, k, l, m, n) \
  a, b, c, d, e, f, g, h, i, j, k, l, m, n
# define BIL_SET15(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) \
  a, b, c, d, e, f, g, h, i, j, k, l, m, n, o
#else
# define BIL_SET2(a, b) b, a
# define BIL_SET3(a, b, c) c, b, a
# define BIL_SET4(a, b, c, d) d, c, b, a
# define BIL_SET5(a, b, c, d, e) e, d, c, b, a
# define BIL_SET6(a, b, c, d, e, f) f, e, d, c, b, a
# define BIL_SET7(a, b, c, d, e, f, g) g, f, e, d, c, b, a
# define BIL_SET8(a, b, c, d, e, f, g, h) h, g, f, e, d, c, b, a
# define BIL_SET9(a, b, c, d, e, f, g, h, i) i, h, g, f, e, d, c, b, a
# define BIL_SET10(a, b, c, d, e, f, g, h, i, j) j, i, h, g, f, e, d, c, b, a
# define BIL_SET11(a, b, c, d, e, f, g, h, i, j, k) \
  k, j, i, h, g, f, e, d, c, b, a
# define BIL_SET12(a, b, c, d, e, f, g, h, i, j, k, l) \
  l, k, j, i, h, g, f, e, d, c, b, a
# define BIL_SET13(a, b, c, d, e, f, g, h, i, j, k, l, m) \
  m, l, k, j, i, h, g, f, e, d, c, b, a
# define BIL_SET14(a, b, c, d, e, f, g, h, i, j, k, l, m, n) \
  n, m, l, k, j, i, h, g, f, e, d, c, b, a
# define BIL_SET15(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) \
  o, n, m, l, k, j, i, h, g, f, e, d, c, b, a
#endif

#include "bitintpow10.h"

/* Set r (_BitInt limbs with rprec bits) to pow10 (n),
   where n is in [0, 6111].  Returns number of least significant
   limbs with just 0s in it.  */

USItype
__bid_pow10bitint (UBILtype *r, SItype rprec, USItype n)
{
  USItype rn = ((USItype) rprec + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
  if (n <= 256)
    {
      /* No need to multiply anything, just copy it from pow10_limbs
	 array.  */
      USItype low_zeros = (n / 64) * (64 / BIL_TYPE_SIZE);
      UBILtype *p = &pow10_limbs[pow10_offs[n]];
      USItype cnt = pow10_offs[n + 1] - pow10_offs[n];
      if (low_zeros)
	__builtin_memset (r + BITINT_END (rn - low_zeros, 0), '\0',
			  low_zeros * sizeof (UBILtype));
      __builtin_memcpy (r + BITINT_END (rn - low_zeros - cnt, low_zeros),
			p, cnt * sizeof (UBILtype));
      if (rn > low_zeros + cnt)
	__builtin_memset (r + BITINT_END (0, low_zeros + cnt), '\0',
			  (rn - low_zeros - cnt) * sizeof (UBILtype));
      return low_zeros;
    }
  else
    {
      USItype m = n / 256;
      n &= 255;
      USItype low_zeros = ((n / 64) + (m * 4)) * (64 / BIL_TYPE_SIZE);
      UBILtype *pm = &pow10_limbs[pow10_offs[m + 255]];
      USItype cntm = pow10_offs[m + 256] - pow10_offs[m + 255];
      UBILtype *pn = &pow10_limbs[pow10_offs[n]];
      USItype cntn = pow10_offs[n + 1] - pow10_offs[n];
      if (low_zeros)
	__builtin_memset (r + BITINT_END (rn - low_zeros, 0), '\0',
			  low_zeros * sizeof (UBILtype));
      __mulbitint3 (r + BITINT_END (0, low_zeros),
		    rprec - low_zeros * BIL_TYPE_SIZE,
		    pm, cntm * BIL_TYPE_SIZE, pn, cntn * BIL_TYPE_SIZE);
      return low_zeros;
    }
}
#endif
