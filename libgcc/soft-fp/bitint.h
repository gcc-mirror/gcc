/* Software floating-point emulation.
   Definitions for _BitInt implementation details.

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

#ifndef GCC_SOFT_FP_BITINT_H
#define GCC_SOFT_FP_BITINT_H

#ifdef __BITINT_MAXWIDTH__
#define BIL_UNITS_PER_WORD (__LIBGCC_BITINT_LIMB_WIDTH__ / __CHAR_BIT__)

#if BIL_UNITS_PER_WORD == 8
#define BIL_TYPE_SIZE (8 * __CHAR_BIT__)
#define BILtype		DItype
typedef UDItype __attribute__ ((__may_alias__)) UBILtype;
#elif BIL_UNITS_PER_WORD == 4
#define BIL_TYPE_SIZE (4 * __CHAR_BIT__)
#define BILtype		SItype
typedef USItype __attribute__ ((__may_alias__)) UBILtype;
#elif BIL_UNITS_PER_WORD == 2
#define BIL_TYPE_SIZE (2 * __CHAR_BIT__)
#define BILtype		HItype
typedef UHItype __attribute__ ((__may_alias__)) UBILtype;
#else
#define BIL_TYPE_SIZE __CHAR_BIT__
#define BILtype		QItype
typedef UQItype __attribute__ ((__may_alias__)) UBILtype;
#endif

/* If *P is zero or sign extended (the latter only for PREC < 0) from
   some narrower _BitInt value, reduce precision.  */

static inline __attribute__((__always_inline__)) SItype
bitint_reduce_prec (const UBILtype **p, SItype prec)
{
  UBILtype mslimb;
  SItype i;
  if (prec < 0)
    {
#if __LIBGCC_BITINT_ORDER__ == __ORDER_BIG_ENDIAN__
      i = 0;
#else
      i = ((USItype) -1 - prec) / BIL_TYPE_SIZE;
#endif
      mslimb = (*p)[i];
      if (mslimb & ((UBILtype) 1 << (((USItype) -1 - prec) % BIL_TYPE_SIZE)))
	{
	  SItype n = ((USItype) -prec) % BIL_TYPE_SIZE;
	  if (n)
	    {
	      mslimb |= ((UBILtype) -1 << (((USItype) -1 - prec) % BIL_TYPE_SIZE));
	      if (mslimb == (UBILtype) -1)
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
	  while (mslimb == (UBILtype) -1)
	    {
	      prec += BIL_TYPE_SIZE;
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
	      if ((BILtype) mslimb >= 0)
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
      i = ((USItype) prec - 1) / BIL_TYPE_SIZE;
#endif
      mslimb = (*p)[i];
    }
  SItype n = ((USItype) prec) % BIL_TYPE_SIZE;
  if (n)
    {
      mslimb &= ((UBILtype) 1 << (((USItype) prec) % BIL_TYPE_SIZE)) - 1;
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
      prec -= BIL_TYPE_SIZE;
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

/* Negate N limbs from S into D.  D and S should point to
   the least significant limb.  */

static inline __attribute__((__always_inline__)) void
bitint_negate (UBILtype *d, const UBILtype *s, SItype n)
{
  UBILtype c = 1;
  do
    {
      UBILtype sv = *s, lo;
      s += BITINT_INC;
      c = __builtin_add_overflow (~sv, c, &lo);
      *d = lo;
      d += BITINT_INC;
    }
  while (--n);
}

/* Common final part of __fix?fbitint conversion functions.
   The A floating point value should have been converted using
   soft-fp macros into RV, U##DI##type DI##_BITS precise normal
   integral type and SHIFT, how many bits should that value be
   shifted to the left.  R is pointer to limbs array passed to the
   function, RN number of limbs in it, ARPREC absolute value of
   RPREC argument passed to it, RSIZE number of significant bits in RV.
   RSIGNED is non-zero if the result is signed bit-precise integer,
   otherwise zero.  If OVF is true, instead of storing RV shifted left
   by SHIFT bits and zero or sign extended store minimum or maximum
   of the signed or unsigned bit-precise integer type or zero depending on if
   RV contains the minimum or maximum signed or unsigned value or zero.  */

#define FP_TO_BITINT(r, rn, arprec, shift, rv, rsize, rsigned, ovf, DI) \
  if (ovf)								\
    {									\
      if ((rv & 1) != 0)						\
	__builtin_memset (r, -1, rn * sizeof (UBILtype));		\
      else								\
	__builtin_memset (r, 0, rn * sizeof (UBILtype));		\
      if (rv & (((U##DI##type) 1) << (rsize - 1)))			\
	r[BITINT_END (0, rn - 1)]					\
	  |= (UBILtype) -1 << ((arprec - 1) % BIL_TYPE_SIZE);		\
      else								\
	r[BITINT_END (0, rn - 1)]					\
	  &= ~((UBILtype) -1 << ((arprec - 1) % BIL_TYPE_SIZE));	\
    }									\
  else									\
    {									\
      USItype shiftl = shift / BIL_TYPE_SIZE;				\
      rsize = DI##_BITS;						\
      if (rsigned && (DI##type) rv >= 0)				\
	rsigned = 0;							\
      if (shift + DI##_BITS > arprec)					\
	rsize = arprec - shift;						\
      USItype shiftr = shift % BIL_TYPE_SIZE;				\
      if (shiftl)							\
	__builtin_memset (r + BITINT_END (rn - shiftl, 0), 0,		\
			  shiftl * sizeof (UBILtype));			\
      USItype idx = BITINT_END (rn - shiftl - 1, shiftl);		\
      DI##type rvs = rv;						\
      if (shiftr)							\
	{								\
	  r[idx] = (rsigned ? (UBILtype) rvs : (UBILtype) rv) << shiftr;\
	  idx += BITINT_INC;						\
	  if (rsize > BIL_TYPE_SIZE - shiftr)				\
	    {								\
	      rv >>= BIL_TYPE_SIZE - shiftr;				\
	      rvs >>= BIL_TYPE_SIZE - shiftr;				\
	      rsize -= BIL_TYPE_SIZE - shiftr;				\
	    }								\
	  else								\
	    rsize = 0;							\
	}								\
      while (rsize)							\
	{								\
	  r[idx] = rsigned ? (UBILtype) rvs : (UBILtype) rv;		\
	  idx += BITINT_INC;						\
	  if (rsize <= BIL_TYPE_SIZE)					\
	    break;							\
	  rv >>= (DI##_BITS > BIL_TYPE_SIZE ? BIL_TYPE_SIZE : 0);	\
	  rvs >>= (DI##_BITS > BIL_TYPE_SIZE ? BIL_TYPE_SIZE : 0);	\
	  rsize -= BIL_TYPE_SIZE;					\
	}								\
      if (idx < rn)							\
	__builtin_memset (r + BITINT_END (0, idx), rsigned ? -1 : 0,	\
			  BITINT_END (idx + 1, rn - idx)		\
			  * sizeof (UBILtype));				\
    }

/* Common initial part of __floatbitint?f conversion functions.
   I and IPREC are arguments passed to those functions, convert that
   into a pair of DI##type IV integer and SHIFT, such that converting
   IV to floating point and multiplicating that by pow (2, SHIFT)
   gives the expected result.  IV size needs to be chosen such that
   it is larger than number of bits in floating-point mantissa and
   contains there even at least a two bits below the mantissa for
   rounding purposes.  If any of the SHIFT bits shifted out is non-zero,
   the least significant bit should be non-zero.  */

#define FP_FROM_BITINT(i, iprec, iv, shift, DI)				\
  do									\
    {									\
      iprec = bitint_reduce_prec (&i, iprec);				\
      USItype aiprec = iprec < 0 ? -iprec : iprec;			\
      USItype in = (aiprec + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;	\
      USItype idx = BITINT_END (0, in - 1);				\
      UBILtype msb = i[idx];						\
      SItype n = 0;							\
      if (aiprec % BIL_TYPE_SIZE)					\
	{								\
	  if (iprec > 0)						\
	    msb &= ((UBILtype) 1 << (aiprec % BIL_TYPE_SIZE)) - 1;	\
	  else								\
	    msb |= (UBILtype) -1 << (aiprec % BIL_TYPE_SIZE);		\
	}								\
      if (iprec < 0)							\
	{								\
	  if (msb == (UBILtype) -1)					\
	    n = 1;							\
	  else								\
	    n = (sizeof (0ULL) * __CHAR_BIT__ + 1			\
		 - __builtin_clzll (~msb));				\
	  if (BIL_TYPE_SIZE > DI##_BITS && n > DI##_BITS)		\
	    {								\
	      iv = msb >> (n - DI##_BITS);				\
	      shift = n - DI##_BITS;					\
	      n = 0;							\
	    }								\
	  else								\
	    {								\
	      iv = (BILtype) msb;					\
	      n = DI##_BITS - n;					\
	    }								\
	}								\
      /* bitint_reduce_prec guarantees that if msb is 0, then whole	\
	 i must be zero, otherwise it would have reduced the		\
	 precision.  */							\
      else if (msb == 0)						\
	iv = 0;								\
      else								\
	{								\
	  n = sizeof (0ULL) * __CHAR_BIT__ - __builtin_clzll (msb);	\
	  if (BIL_TYPE_SIZE >= DI##_BITS && n >= DI##_BITS)		\
	    {								\
	      iv = msb >> (n - DI##_BITS + 1);				\
	      shift = n - DI##_BITS + 1;				\
	      n = 0;							\
	    }								\
	  else								\
	    {								\
	      iv = msb;							\
	      n = DI##_BITS - 1 - n;					\
	    }								\
	}								\
      while (n && BITINT_END (idx < in - 1, idx))			\
	{								\
	  idx -= BITINT_INC;						\
	  msb = i[idx];							\
	  if (BIL_TYPE_SIZE < DI##_BITS && n >= BIL_TYPE_SIZE)		\
	    {								\
	      iv = (U##DI##type) iv << (BIL_TYPE_SIZE < DI##_BITS	\
					? BIL_TYPE_SIZE : 0);		\
	      iv |= msb;						\
	      n -= BIL_TYPE_SIZE;					\
	    }								\
	  else								\
	    {								\
	      iv = (U##DI##type) iv << n;				\
	      iv |= msb >> (BIL_TYPE_SIZE - n);				\
	      shift = BIL_TYPE_SIZE - n;				\
	      break;							\
	    }								\
	}								\
									\
      UBILtype low_bits = 0;						\
      if (shift)							\
	low_bits = msb & (((UBILtype) 1 << shift) - 1);			\
      shift += BITINT_END (in - 1 - idx, idx) * BIL_TYPE_SIZE;		\
      while (!low_bits && BITINT_END (idx < in - 1, idx))		\
	{								\
	  idx -= BITINT_INC;						\
	  low_bits |= i[idx];						\
	}								\
      iv |= (low_bits != 0);						\
    }									\
  while (0)

extern void __mulbitint3 (UBILtype *, SItype, const UBILtype *, SItype,
			  const UBILtype *, SItype);
extern void __divmodbitint4 (UBILtype *, SItype, UBILtype *, SItype,
			     const UBILtype *, SItype,
			     const UBILtype *, SItype);

extern USItype __bid_pow10bitint (UBILtype *, SItype, USItype);

#endif /* __BITINT_MAXWIDTH__ */

#endif /* GCC_SOFT_FP_BITINT_H */
