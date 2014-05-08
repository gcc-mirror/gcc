/* Operations with very long integers.
   Copyright (C) 2012-2013 Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "hwint.h"
#include "wide-int.h"
#include "tree.h"
#include "dumpfile.h"

#if GCC_VERSION >= 3000
#define W_TYPE_SIZE HOST_BITS_PER_WIDE_INT
typedef unsigned HOST_HALF_WIDE_INT UHWtype;
typedef unsigned HOST_WIDE_INT UWtype;
typedef unsigned int UQItype __attribute__ ((mode (QI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));
typedef unsigned int UTItype __attribute__ ((mode (TI)));
#if W_TYPE_SIZE == 32
# define UDWtype       UDItype
#elif W_TYPE_SIZE == 64
# define UDWtype       UTItype
#endif
#include "longlong.h"
#endif

static const HOST_WIDE_INT zeros[WIDE_INT_MAX_ELTS] = {};

/*
 * Internal utilities.
 */

/* Quantities to deal with values that hold half of a wide int.  Used
   in multiply and divide.  */
#define HALF_INT_MASK (((HOST_WIDE_INT) 1 << HOST_BITS_PER_HALF_WIDE_INT) - 1)

#define BLOCK_OF(TARGET) ((TARGET) / HOST_BITS_PER_WIDE_INT)
#define BLOCKS_NEEDED(PREC) \
  (PREC ? (((PREC) + HOST_BITS_PER_WIDE_INT - 1) / HOST_BITS_PER_WIDE_INT) : 1)
#define SIGN_MASK(X) ((HOST_WIDE_INT) (X) < 0 ? -1 : 0)

/* Return the value a VAL[I] if I < LEN, otherwise, return 0 or -1
   based on the top existing bit of VAL. */

static unsigned HOST_WIDE_INT
safe_uhwi (const HOST_WIDE_INT *val, unsigned int len, unsigned int i)
{
  return i < len ? val[i] : val[len - 1] < 0 ? (HOST_WIDE_INT) -1 : 0;
}

/* Convert the integer in VAL to canonical form, returning its new length.
   LEN is the number of blocks currently in VAL and PRECISION is the number
   of bits in the integer it represents.

   This function only changes the representation, not the value.  */
static unsigned int
canonize (HOST_WIDE_INT *val, unsigned int len, unsigned int precision)
{
  unsigned int blocks_needed = BLOCKS_NEEDED (precision);
  HOST_WIDE_INT top;
  int i;

  if (len > blocks_needed)
    len = blocks_needed;

  if (len == 1)
    return len;

  top = val[len - 1];
  if (len * HOST_BITS_PER_WIDE_INT > precision)
    val[len - 1] = top = sext_hwi (top, precision % HOST_BITS_PER_WIDE_INT);
  if (top != 0 && top != (HOST_WIDE_INT)-1)
    return len;

  /* At this point we know that the top is either 0 or -1.  Find the
     first block that is not a copy of this.  */
  for (i = len - 2; i >= 0; i--)
    {
      HOST_WIDE_INT x = val[i];
      if (x != top)
	{
	  if (SIGN_MASK (x) == top)
	    return i + 1;

	  /* We need an extra block because the top bit block i does
	     not match the extension.  */
	  return i + 2;
	}
    }

  /* The number is 0 or -1.  */
  return 1;
}

/*
 * Conversion routines in and out of wide_int.
 */

/* Copy XLEN elements from XVAL to VAL.  If NEED_CANON, canonize the
   result for an integer with precision PRECISION.  Return the length
   of VAL (after any canonization.  */
unsigned int
wi::from_array (HOST_WIDE_INT *val, const HOST_WIDE_INT *xval,
		unsigned int xlen, unsigned int precision, bool need_canon)
{
  for (unsigned i = 0; i < xlen; i++)
    val[i] = xval[i];
  return need_canon ? canonize (val, xlen, precision) : xlen;
}

/* Construct a wide int from a buffer of length LEN.  BUFFER will be
   read according to byte endianess and word endianess of the target.
   Only the lower BUFFER_LEN bytes of the result are set; the remaining
   high bytes are cleared.  */
wide_int
wi::from_buffer (const unsigned char *buffer, unsigned int buffer_len)
{
  unsigned int precision = buffer_len * BITS_PER_UNIT;
  wide_int result = wide_int::create (precision);
  unsigned int words = buffer_len / UNITS_PER_WORD;

  /* We have to clear all the bits ourself, as we merely or in values
     below.  */
  unsigned int len = BLOCKS_NEEDED (precision);
  HOST_WIDE_INT *val = result.write_val ();
  for (unsigned int i = 0; i < len; ++i)
    val[i] = 0;

  for (unsigned int byte = 0; byte < buffer_len; byte++)
    {
      unsigned int offset;
      unsigned int index;
      unsigned int bitpos = byte * BITS_PER_UNIT;
      unsigned HOST_WIDE_INT value;

      if (buffer_len > UNITS_PER_WORD)
	{
	  unsigned int word = byte / UNITS_PER_WORD;

	  if (WORDS_BIG_ENDIAN)
	    word = (words - 1) - word;

	  offset = word * UNITS_PER_WORD;

	  if (BYTES_BIG_ENDIAN)
	    offset += (UNITS_PER_WORD - 1) - (byte % UNITS_PER_WORD);
	  else
	    offset += byte % UNITS_PER_WORD;
	}
      else
	offset = BYTES_BIG_ENDIAN ? (buffer_len - 1) - byte : byte;

      value = (unsigned HOST_WIDE_INT) buffer[offset];

      index = bitpos / HOST_BITS_PER_WIDE_INT;
      val[index] |= value << (bitpos % HOST_BITS_PER_WIDE_INT);
    }

  result.set_len (canonize (val, len, precision));

  return result;
}

/* Sets RESULT from X, the sign is taken according to SGN.  */
void
wi::to_mpz (const wide_int_ref &x, mpz_t result, signop sgn)
{
  int len = x.get_len ();
  const HOST_WIDE_INT *v = x.get_val ();
  int excess = len * HOST_BITS_PER_WIDE_INT - x.get_precision ();

  if (wi::neg_p (x, sgn))
    {
      /* We use ones complement to avoid -x80..0 edge case that -
	 won't work on.  */
      HOST_WIDE_INT *t = XALLOCAVEC (HOST_WIDE_INT, len);
      for (int i = 0; i < len; i++)
	t[i] = ~v[i];
      if (excess > 0)
	t[len - 1] = (unsigned HOST_WIDE_INT) t[len - 1] << excess >> excess;
      mpz_import (result, len, -1, sizeof (HOST_WIDE_INT), 0, 0, t);
      mpz_com (result, result);
    }
  else if (excess > 0)
    {
      HOST_WIDE_INT *t = XALLOCAVEC (HOST_WIDE_INT, len);
      for (int i = 0; i < len - 1; i++)
	t[i] = v[i];
      t[len - 1] = (unsigned HOST_WIDE_INT) v[len - 1] << excess >> excess;
      mpz_import (result, len, -1, sizeof (HOST_WIDE_INT), 0, 0, t);
    }
  else
    mpz_import (result, len, -1, sizeof (HOST_WIDE_INT), 0, 0, v);
}

/* Returns X converted to TYPE.  If WRAP is true, then out-of-range
   values of VAL will be wrapped; otherwise, they will be set to the
   appropriate minimum or maximum TYPE bound.  */
wide_int
wi::from_mpz (const_tree type, mpz_t x, bool wrap)
{
  size_t count, numb;
  int prec = TYPE_PRECISION (type);
  wide_int res = wide_int::create (prec);

  if (!wrap)
    {
      mpz_t min, max;

      mpz_init (min);
      mpz_init (max);
      get_type_static_bounds (type, min, max);

      if (mpz_cmp (x, min) < 0)
	mpz_set (x, min);
      else if (mpz_cmp (x, max) > 0)
	mpz_set (x, max);

      mpz_clear (min);
      mpz_clear (max);
    }

  /* Determine the number of unsigned HOST_WIDE_INTs that are required
     for representing the value.  The code to calculate count is
     extracted from the GMP manual, section "Integer Import and Export":
     http://gmplib.org/manual/Integer-Import-and-Export.html  */
  numb = 8 * sizeof(HOST_WIDE_INT);
  count = (mpz_sizeinbase (x, 2) + numb - 1) / numb;
  HOST_WIDE_INT *val = res.write_val ();
  mpz_export (val, &count, -1, sizeof (HOST_WIDE_INT), 0, 0, x);
  if (count < 1)
    {
      val[0] = 0;
      count = 1;
    }
  res.set_len (count);

  if (mpz_sgn (x) < 0)
    res = -res;

  return res;
}

/*
 * Largest and smallest values in a mode.
 */

/* Return the largest SGNed number that is representable in PRECISION bits.

   TODO: There is still code from the double_int era that trys to
   make up for the fact that double int's could not represent the
   min and max values of all types.  This code should be removed
   because the min and max values can always be represented in
   wide_ints and int-csts.  */
wide_int
wi::max_value (unsigned int precision, signop sgn)
{
  gcc_checking_assert (precision != 0);
  if (sgn == UNSIGNED)
    /* The unsigned max is just all ones.  */
    return shwi (-1, precision);
  else
    /* The signed max is all ones except the top bit.  This must be
       explicitly represented.  */
    return mask (precision - 1, false, precision);
}

/* Return the largest SGNed number that is representable in PRECISION bits.  */
wide_int
wi::min_value (unsigned int precision, signop sgn)
{
  gcc_checking_assert (precision != 0);
  if (sgn == UNSIGNED)
    return uhwi (0, precision);
  else
    /* The signed min is all zeros except the top bit.  This must be
       explicitly represented.  */
    return wi::set_bit_in_zero (precision - 1, precision);
}

/*
 * Public utilities.
 */

/* Convert the number represented by XVAL, XLEN and XPRECISION, which has
   signedness SGN, to an integer that has PRECISION bits.  Store the blocks
   in VAL and return the number of blocks used.

   This function can handle both extension (PRECISION > XPRECISION)
   and truncation (PRECISION < XPRECISION).  */
unsigned int
wi::force_to_size (HOST_WIDE_INT *val, const HOST_WIDE_INT *xval,
		   unsigned int xlen, unsigned int xprecision,
		   unsigned int precision, signop sgn)
{
  unsigned int blocks_needed = BLOCKS_NEEDED (precision);
  unsigned int len = blocks_needed < xlen ? blocks_needed : xlen;
  for (unsigned i = 0; i < len; i++)
    val[i] = xval[i];

  if (precision > xprecision)
    {
      unsigned int small_xprecision = xprecision % HOST_BITS_PER_WIDE_INT;

      /* Expanding.  */
      if (sgn == UNSIGNED)
	{
	  if (small_xprecision && len == BLOCKS_NEEDED (xprecision))
	    val[len - 1] = zext_hwi (val[len - 1], small_xprecision);
	  else if (val[len - 1] < 0)
	    {
	      while (len < BLOCKS_NEEDED (xprecision))
		val[len++] = -1;
	      if (small_xprecision)
		val[len - 1] = zext_hwi (val[len - 1], small_xprecision);
	      else
		val[len++] = 0;
	    }
	}
      else
	{
	  if (small_xprecision && len == BLOCKS_NEEDED (xprecision))
	    val[len - 1] = sext_hwi (val[len - 1], small_xprecision);
	}
    }
  len = canonize (val, len, precision);

  return len;
}

/* This function hides the fact that we cannot rely on the bits beyond
   the precision.  This issue comes up in the relational comparisions
   where we do allow comparisons of values of different precisions.  */
static inline HOST_WIDE_INT
selt (const HOST_WIDE_INT *a, unsigned int len,
      unsigned int blocks_needed, unsigned int small_prec,
      unsigned int index, signop sgn)
{
  HOST_WIDE_INT val;
  if (index < len)
    val = a[index];
  else if (index < blocks_needed || sgn == SIGNED)
    /* Signed or within the precision.  */
    val = SIGN_MASK (a[len - 1]);
  else
    /* Unsigned extension beyond the precision. */
    val = 0;

  if (small_prec && index == blocks_needed - 1)
    return (sgn == SIGNED
	    ? sext_hwi (val, small_prec)
	    : zext_hwi (val, small_prec));
  else
    return val;
}

/* Find the highest bit represented in a wide int.  This will in
   general have the same value as the sign bit.  */
static inline HOST_WIDE_INT
top_bit_of (const HOST_WIDE_INT *a, unsigned int len, unsigned int prec)
{
  int excess = len * HOST_BITS_PER_WIDE_INT - prec;
  unsigned HOST_WIDE_INT val = a[len - 1];
  if (excess > 0)
    val <<= excess;
  return val >> (HOST_BITS_PER_WIDE_INT - 1);
}

/*
 * Comparisons, note that only equality is an operator.  The other
 * comparisons cannot be operators since they are inherently signed or
 * unsigned and C++ has no such operators.
 */

/* Return true if OP0 == OP1.  */
bool
wi::eq_p_large (const HOST_WIDE_INT *op0, unsigned int op0len,
		const HOST_WIDE_INT *op1, unsigned int op1len,
		unsigned int prec)
{
  int l0 = op0len - 1;
  unsigned int small_prec = prec & (HOST_BITS_PER_WIDE_INT - 1);

  if (op0len != op1len)
    return false;

  if (op0len == BLOCKS_NEEDED (prec) && small_prec)
    {
      /* It does not matter if we zext or sext here, we just have to
	 do both the same way.  */
      if (zext_hwi (op0 [l0], small_prec) != zext_hwi (op1 [l0], small_prec))
	return false;
      l0--;
    }

  while (l0 >= 0)
    if (op0[l0] != op1[l0])
      return false;
    else
      l0--;

  return true;
}

/* Return true if OP0 < OP1 using signed comparisons.  */
bool
wi::lts_p_large (const HOST_WIDE_INT *op0, unsigned int op0len,
		 unsigned int precision,
		 const HOST_WIDE_INT *op1, unsigned int op1len)
{
  HOST_WIDE_INT s0, s1;
  unsigned HOST_WIDE_INT u0, u1;
  unsigned int blocks_needed = BLOCKS_NEEDED (precision);
  unsigned int small_prec = precision & (HOST_BITS_PER_WIDE_INT - 1);
  int l = MAX (op0len - 1, op1len - 1);

  /* Only the top block is compared as signed.  The rest are unsigned
     comparisons.  */
  s0 = selt (op0, op0len, blocks_needed, small_prec, l, SIGNED);
  s1 = selt (op1, op1len, blocks_needed, small_prec, l, SIGNED);
  if (s0 < s1)
    return true;
  if (s0 > s1)
    return false;

  l--;
  while (l >= 0)
    {
      u0 = selt (op0, op0len, blocks_needed, small_prec, l, SIGNED);
      u1 = selt (op1, op1len, blocks_needed, small_prec, l, SIGNED);

      if (u0 < u1)
	return true;
      if (u0 > u1)
	return false;
      l--;
    }

  return false;
}

/* Returns -1 if OP0 < OP1, 0 if OP0 == OP1 and 1 if OP0 > OP1 using
   signed compares.  */
int
wi::cmps_large (const HOST_WIDE_INT *op0, unsigned int op0len,
		unsigned int precision,
		const HOST_WIDE_INT *op1, unsigned int op1len)
{
  HOST_WIDE_INT s0, s1;
  unsigned HOST_WIDE_INT u0, u1;
  unsigned int blocks_needed = BLOCKS_NEEDED (precision);
  unsigned int small_prec = precision & (HOST_BITS_PER_WIDE_INT - 1);
  int l = MAX (op0len - 1, op1len - 1);

  /* Only the top block is compared as signed.  The rest are unsigned
     comparisons.  */
  s0 = selt (op0, op0len, blocks_needed, small_prec, l, SIGNED);
  s1 = selt (op1, op1len, blocks_needed, small_prec, l, SIGNED);
  if (s0 < s1)
    return -1;
  if (s0 > s1)
    return 1;

  l--;
  while (l >= 0)
    {
      u0 = selt (op0, op0len, blocks_needed, small_prec, l, SIGNED);
      u1 = selt (op1, op1len, blocks_needed, small_prec, l, SIGNED);

      if (u0 < u1)
	return -1;
      if (u0 > u1)
	return 1;
      l--;
    }

  return 0;
}

/* Return true if OP0 < OP1 using unsigned comparisons.  */
bool
wi::ltu_p_large (const HOST_WIDE_INT *op0, unsigned int op0len,
		 unsigned int precision,
		 const HOST_WIDE_INT *op1, unsigned int op1len)
{
  unsigned HOST_WIDE_INT x0;
  unsigned HOST_WIDE_INT x1;
  unsigned int blocks_needed = BLOCKS_NEEDED (precision);
  unsigned int small_prec = precision & (HOST_BITS_PER_WIDE_INT - 1);
  int l = MAX (op0len - 1, op1len - 1);

  while (l >= 0)
    {
      x0 = selt (op0, op0len, blocks_needed, small_prec, l, UNSIGNED);
      x1 = selt (op1, op1len, blocks_needed, small_prec, l, UNSIGNED);
      if (x0 < x1)
	return true;
      if (x0 > x1)
	return false;
      l--;
    }

  return false;
}

/* Returns -1 if OP0 < OP1, 0 if OP0 == OP1 and 1 if OP0 > OP1 using
   unsigned compares.  */
int
wi::cmpu_large (const HOST_WIDE_INT *op0, unsigned int op0len,
		unsigned int precision,
		const HOST_WIDE_INT *op1, unsigned int op1len)
{
  unsigned HOST_WIDE_INT x0;
  unsigned HOST_WIDE_INT x1;
  unsigned int blocks_needed = BLOCKS_NEEDED (precision);
  unsigned int small_prec = precision & (HOST_BITS_PER_WIDE_INT - 1);
  int l = MAX (op0len - 1, op1len - 1);

  while (l >= 0)
    {
      x0 = selt (op0, op0len, blocks_needed, small_prec, l, UNSIGNED);
      x1 = selt (op1, op1len, blocks_needed, small_prec, l, UNSIGNED);
      if (x0 < x1)
	return -1;
      if (x0 > x1)
	return 1;
      l--;
    }

  return 0;
}

/*
 * Extension.
 */

/* Sign-extend the number represented by XVAL and XLEN into VAL,
   starting at OFFSET.  Return the number of blocks in VAL.  Both XVAL
   and VAL have PRECISION bits.  */
unsigned int
wi::sext_large (HOST_WIDE_INT *val, const HOST_WIDE_INT *xval,
		unsigned int xlen, unsigned int precision, unsigned int offset)
{
  unsigned int len = offset / HOST_BITS_PER_WIDE_INT;
  /* Extending beyond the precision is a no-op.  If we have only stored
     OFFSET bits or fewer, the rest are already signs.  */
  if (offset >= precision || len >= xlen)
    {
      for (unsigned i = 0; i < xlen; ++i)
	val[i] = xval[i];
      return xlen;
    }
  unsigned int suboffset = offset % HOST_BITS_PER_WIDE_INT;
  for (unsigned int i = 0; i < len; i++)
    val[i] = xval[i];
  if (suboffset > 0)
    {
      val[len] = sext_hwi (xval[len], suboffset);
      len += 1;
    }
  return canonize (val, len, precision);
}

/* Zero-extend the number represented by XVAL and XLEN into VAL,
   starting at OFFSET.  Return the number of blocks in VAL.  Both XVAL
   and VAL have PRECISION bits.  */
unsigned int
wi::zext_large (HOST_WIDE_INT *val, const HOST_WIDE_INT *xval,
		unsigned int xlen, unsigned int precision, unsigned int offset)
{
  unsigned int len = offset / HOST_BITS_PER_WIDE_INT;
  /* Extending beyond the precision is a no-op.  If we have only stored
     OFFSET bits or fewer, and the upper stored bit is zero, then there
     is nothing to do.  */
  if (offset >= precision || (len >= xlen && xval[xlen - 1] >= 0))
    {
      for (unsigned i = 0; i < xlen; ++i)
	val[i] = xval[i];
      return xlen;
    }
  unsigned int suboffset = offset % HOST_BITS_PER_WIDE_INT;
  for (unsigned int i = 0; i < len; i++)
    val[i] = i < xlen ? xval[i] : -1;
  if (suboffset > 0)
    val[len] = zext_hwi (len < xlen ? xval[len] : -1, suboffset);
  else
    val[len] = 0;
  return canonize (val, len + 1, precision);
}

/*
 * Masking, inserting, shifting, rotating.
 */

/* Insert WIDTH bits from Y into X starting at START.  */
wide_int
wi::insert (const wide_int &x, const wide_int &y, unsigned int start,
	    unsigned int width)
{
  wide_int result;
  wide_int mask;
  wide_int tmp;

  unsigned int precision = x.get_precision ();
  if (start >= precision)
    return x;

  gcc_checking_assert (precision >= width);

  if (start + width >= precision)
    width = precision - start;

  mask = wi::shifted_mask (start, width, false, precision);
  tmp = wi::lshift (wide_int::from (y, precision, UNSIGNED), start);
  result = tmp & mask;

  tmp = wi::bit_and_not (x, mask);
  result = result | tmp;

  return result;
}

/* Copy the number represented by XVAL and XLEN into VAL, setting bit BIT.
   Return the number of blocks in VAL.  Both XVAL and VAL have PRECISION
   bits.  */
unsigned int
wi::set_bit_large (HOST_WIDE_INT *val, const HOST_WIDE_INT *xval,
		   unsigned int xlen, unsigned int precision, unsigned int bit)
{
  unsigned int block = bit / HOST_BITS_PER_WIDE_INT;
  unsigned int subbit = bit % HOST_BITS_PER_WIDE_INT;

  if (block + 1 >= xlen)
    {
      /* The operation either affects the last current block or needs
	 a new block.  */
      unsigned int len = block + 1;
      for (unsigned int i = 0; i < len; i++)
	val[i] = safe_uhwi (xval, xlen, i);
      val[block] |= (unsigned HOST_WIDE_INT) 1 << subbit;

      /* If the bit we just set is at the msb of the block, make sure
	 that any higher bits are zeros.  */
      if (bit + 1 < precision && subbit == HOST_BITS_PER_WIDE_INT - 1)
	val[len++] = 0;
      return len;
    }
  else
    {
      for (unsigned int i = 0; i < xlen; i++)
	val[i] = xval[i];
      val[block] |= (unsigned HOST_WIDE_INT) 1 << subbit;
      return canonize (val, xlen, precision);
    }
}

/* bswap THIS.  */
wide_int
wide_int_storage::bswap () const
{
  wide_int result = wide_int::create (precision);
  unsigned int i, s;
  unsigned int len = BLOCKS_NEEDED (precision);
  unsigned int xlen = get_len ();
  const HOST_WIDE_INT *xval = get_val ();
  HOST_WIDE_INT *val = result.write_val ();

  /* This is not a well defined operation if the precision is not a
     multiple of 8.  */
  gcc_assert ((precision & 0x7) == 0);

  for (i = 0; i < len; i++)
    val[i] = 0;

  /* Only swap the bytes that are not the padding.  */
  for (s = 0; s < precision; s += 8)
    {
      unsigned int d = precision - s - 8;
      unsigned HOST_WIDE_INT byte;

      unsigned int block = s / HOST_BITS_PER_WIDE_INT;
      unsigned int offset = s & (HOST_BITS_PER_WIDE_INT - 1);

      byte = (safe_uhwi (xval, xlen, block) >> offset) & 0xff;

      block = d / HOST_BITS_PER_WIDE_INT;
      offset = d & (HOST_BITS_PER_WIDE_INT - 1);

      val[block] |= byte << offset;
    }

  result.set_len (canonize (val, len, precision));
  return result;
}

/* Fill VAL with a mask where the lower WIDTH bits are ones and the bits
   above that up to PREC are zeros.  The result is inverted if NEGATE
   is true.  Return the number of blocks in VAL.  */
unsigned int
wi::mask (HOST_WIDE_INT *val, unsigned int width, bool negate,
	  unsigned int prec)
{
  if (width >= prec)
    {
      val[0] = negate ? 0 : -1;
      return 1;
    }
  else if (width == 0)
    {
      val[0] = negate ? -1 : 0;
      return 1;
    }

  unsigned int i = 0;
  while (i < width / HOST_BITS_PER_WIDE_INT)
    val[i++] = negate ? 0 : -1;

  unsigned int shift = width & (HOST_BITS_PER_WIDE_INT - 1);
  if (shift != 0)
    {
      HOST_WIDE_INT last = ((unsigned HOST_WIDE_INT) 1 << shift) - 1;
      val[i++] = negate ? ~last : last;
    }
  else
    val[i++] = negate ? -1 : 0;

  return i;
}

/* Fill VAL with a mask where the lower START bits are zeros, the next WIDTH
   bits are ones, and the bits above that up to PREC are zeros.  The result
   is inverted if NEGATE is true.  Return the number of blocks in VAL.  */
unsigned int
wi::shifted_mask (HOST_WIDE_INT *val, unsigned int start, unsigned int width,
		  bool negate, unsigned int prec)
{
  if (start >= prec || width == 0)
    {
      val[0] = negate ? -1 : 0;
      return 1;
    }

  if (width > prec - start)
    width = prec - start;
  unsigned int end = start + width;

  unsigned int i = 0;
  while (i < start / HOST_BITS_PER_WIDE_INT)
    val[i++] = negate ? -1 : 0;

  unsigned int shift = start & (HOST_BITS_PER_WIDE_INT - 1);
  if (shift)
    {
      HOST_WIDE_INT block = ((unsigned HOST_WIDE_INT) 1 << shift) - 1;
      shift += width;
      if (shift < HOST_BITS_PER_WIDE_INT)
	{
	  /* case 000111000 */
	  block = ((unsigned HOST_WIDE_INT) 1 << shift) - block - 1;
	  val[i++] = negate ? ~block : block;
	  return i;
	}
      else
	/* ...111000 */
	val[i++] = negate ? block : ~block;
    }

  while (i < end / HOST_BITS_PER_WIDE_INT)
    /* 1111111 */
    val[i++] = negate ? 0 : -1;

  shift = end & (HOST_BITS_PER_WIDE_INT - 1);
  if (shift != 0)
    {
      /* 000011111 */
      HOST_WIDE_INT block = ((unsigned HOST_WIDE_INT) 1 << shift) - 1;
      val[i++] = negate ? ~block : block;
    }
  else if (end < prec)
    val[i++] = negate ? -1 : 0;

  return i;
}

/*
 * logical operations.
 */

/* Set VAL to OP0 & OP1.  Return the number of blocks used.  */
unsigned int
wi::and_large (HOST_WIDE_INT *val, const HOST_WIDE_INT *op0,
	       unsigned int op0len, const HOST_WIDE_INT *op1,
	       unsigned int op1len, unsigned int prec)
{
  int l0 = op0len - 1;
  int l1 = op1len - 1;
  bool need_canon = true;

  unsigned int len = MAX (op0len, op1len);
  if (l0 > l1)
    {
      HOST_WIDE_INT op1mask = -top_bit_of (op1, op1len, prec);
      if (op1mask == 0)
	{
	  l0 = l1;
	  len = l1 + 1;
	}
      else
	{
	  need_canon = false;
	  while (l0 > l1)
	    {
	      val[l0] = op0[l0];
	      l0--;
	    }
	}
    }
  else if (l1 > l0)
    {
      HOST_WIDE_INT op0mask = -top_bit_of (op0, op0len, prec);
      if (op0mask == 0)
	len = l0 + 1;
      else
	{
	  need_canon = false;
	  while (l1 > l0)
	    {
	      val[l1] = op1[l1];
	      l1--;
	    }
	}
    }

  while (l0 >= 0)
    {
      val[l0] = op0[l0] & op1[l0];
      l0--;
    }

  if (need_canon)
    len = canonize (val, len, prec);

  return len;
}

/* Set VAL to OP0 & ~OP1.  Return the number of blocks used.  */
unsigned int
wi::and_not_large (HOST_WIDE_INT *val, const HOST_WIDE_INT *op0,
		   unsigned int op0len, const HOST_WIDE_INT *op1,
		   unsigned int op1len, unsigned int prec)
{
  wide_int result;
  int l0 = op0len - 1;
  int l1 = op1len - 1;
  bool need_canon = true;

  unsigned int len = MAX (op0len, op1len);
  if (l0 > l1)
    {
      HOST_WIDE_INT op1mask = -top_bit_of (op1, op1len, prec);
      if (op1mask != 0)
	{
	  l0 = l1;
	  len = l1 + 1;
	}
      else
	{
	  need_canon = false;
	  while (l0 > l1)
	    {
	      val[l0] = op0[l0];
	      l0--;
	    }
	}
    }
  else if (l1 > l0)
    {
      HOST_WIDE_INT op0mask = -top_bit_of (op0, op0len, prec);
      if (op0mask == 0)
	len = l0 + 1;
      else
	{
	  need_canon = false;
	  while (l1 > l0)
	    {
	      val[l1] = ~op1[l1];
	      l1--;
	    }
	}
    }

  while (l0 >= 0)
    {
      val[l0] = op0[l0] & ~op1[l0];
      l0--;
    }

  if (need_canon)
    len = canonize (val, len, prec);

  return len;
}

/* Set VAL to OP0 | OP1.  Return the number of blocks used.  */
unsigned int
wi::or_large (HOST_WIDE_INT *val, const HOST_WIDE_INT *op0,
	      unsigned int op0len, const HOST_WIDE_INT *op1,
	      unsigned int op1len, unsigned int prec)
{
  wide_int result;
  int l0 = op0len - 1;
  int l1 = op1len - 1;
  bool need_canon = true;

  unsigned int len = MAX (op0len, op1len);
  if (l0 > l1)
    {
      HOST_WIDE_INT op1mask = -top_bit_of (op1, op1len, prec);
      if (op1mask != 0)
	{
	  l0 = l1;
	  len = l1 + 1;
	}
      else
	{
	  need_canon = false;
	  while (l0 > l1)
	    {
	      val[l0] = op0[l0];
	      l0--;
	    }
	}
    }
  else if (l1 > l0)
    {
      HOST_WIDE_INT op0mask = -top_bit_of (op0, op0len, prec);
      if (op0mask != 0)
	len = l0 + 1;
      else
	{
	  need_canon = false;
	  while (l1 > l0)
	    {
	      val[l1] = op1[l1];
	      l1--;
	    }
	}
    }

  while (l0 >= 0)
    {
      val[l0] = op0[l0] | op1[l0];
      l0--;
    }

  if (need_canon)
    len = canonize (val, len, prec);

  return len;
}

/* Set VAL to OP0 | ~OP1.  Return the number of blocks used.  */
unsigned int
wi::or_not_large (HOST_WIDE_INT *val, const HOST_WIDE_INT *op0,
		  unsigned int op0len, const HOST_WIDE_INT *op1,
		  unsigned int op1len, unsigned int prec)
{
  wide_int result;
  int l0 = op0len - 1;
  int l1 = op1len - 1;
  bool need_canon = true;

  unsigned int len = MAX (op0len, op1len);
  if (l0 > l1)
    {
      HOST_WIDE_INT op1mask = -top_bit_of (op1, op1len, prec);
      if (op1mask == 0)
	{
	  l0 = l1;
	  len = l1 + 1;
	}
      else
	{
	  need_canon = false;
	  while (l0 > l1)
	    {
	      val[l0] = op0[l0];
	      l0--;
	    }
	}
    }
  else if (l1 > l0)
    {
      HOST_WIDE_INT op0mask = -top_bit_of (op0, op0len, prec);
      if (op0mask != 0)
	len = l0 + 1;
      else
	{
	  need_canon = false;
	  while (l1 > l0)
	    {
	      val[l1] = ~op1[l1];
	      l1--;
	    }
	}
    }

  while (l0 >= 0)
    {
      val[l0] = op0[l0] | ~op1[l0];
      l0--;
    }

  if (need_canon)
    len = canonize (val, len, prec);

  return len;
}

/* Set VAL to OP0 ^ OP1.  Return the number of blocks used.  */
unsigned int
wi::xor_large (HOST_WIDE_INT *val, const HOST_WIDE_INT *op0,
	       unsigned int op0len, const HOST_WIDE_INT *op1,
	       unsigned int op1len, unsigned int prec)
{
  wide_int result;
  int l0 = op0len - 1;
  int l1 = op1len - 1;

  unsigned int len = MAX (op0len, op1len);
  if (l0 > l1)
    {
      HOST_WIDE_INT op1mask = -top_bit_of (op1, op1len, prec);
      while (l0 > l1)
	{
	  val[l0] = op0[l0] ^ op1mask;
	  l0--;
	}
    }

  if (l1 > l0)
    {
      HOST_WIDE_INT op0mask = -top_bit_of (op0, op0len, prec);
      while (l1 > l0)
	{
	  val[l1] = op0mask ^ op1[l1];
	  l1--;
	}
    }

  while (l0 >= 0)
    {
      val[l0] = op0[l0] ^ op1[l0];
      l0--;
    }

  return canonize (val, len, prec);
}

/*
 * math
 */

/* Set VAL to OP0 + OP1.  If OVERFLOW is nonnull, record in *OVERFLOW
   whether the result overflows when OP0 and OP1 are treated as having
   signedness SGN.  Return the number of blocks in VAL.  */
unsigned int
wi::add_large (HOST_WIDE_INT *val, const HOST_WIDE_INT *op0,
	       unsigned int op0len, const HOST_WIDE_INT *op1,
	       unsigned int op1len, unsigned int prec,
	       signop sgn, bool *overflow)
{
  unsigned HOST_WIDE_INT o0 = 0;
  unsigned HOST_WIDE_INT o1 = 0;
  unsigned HOST_WIDE_INT x = 0;
  unsigned HOST_WIDE_INT carry = 0;
  unsigned HOST_WIDE_INT old_carry = 0;
  unsigned HOST_WIDE_INT mask0, mask1;
  unsigned int i;

  unsigned int len = MAX (op0len, op1len);
  mask0 = -top_bit_of (op0, op0len, prec);
  mask1 = -top_bit_of (op1, op1len, prec);
  /* Add all of the explicitly defined elements.  */

  for (i = 0; i < len; i++)
    {
      o0 = i < op0len ? (unsigned HOST_WIDE_INT) op0[i] : mask0;
      o1 = i < op1len ? (unsigned HOST_WIDE_INT) op1[i] : mask1;
      x = o0 + o1 + carry;
      val[i] = x;
      old_carry = carry;
      carry = carry == 0 ? x < o0 : x <= o0;
    }

  if (len * HOST_BITS_PER_WIDE_INT < prec)
    {
      val[len] = mask0 + mask1 + carry;
      len++;
      if (overflow)
	*overflow = false;
    }
  else if (overflow)
    {
      unsigned int shift = -prec % HOST_BITS_PER_WIDE_INT;
      if (sgn == SIGNED)
	{
	  unsigned HOST_WIDE_INT x = (val[len - 1] ^ o0) & (val[len - 1] ^ o1);
	  *overflow = (HOST_WIDE_INT) (x << shift) < 0;
	}
      else
	{
	  /* Put the MSB of X and O0 and in the top of the HWI.  */
	  x <<= shift;
	  o0 <<= shift;
	  if (old_carry)
	    *overflow = (x <= o0);
	  else
	    *overflow = (x < o0);
	}
    }

  return canonize (val, len, prec);
}

/* Subroutines of the multiplication and division operations.  Unpack
   the first IN_LEN HOST_WIDE_INTs in INPUT into 2 * IN_LEN
   HOST_HALF_WIDE_INTs of RESULT.  The rest of RESULT is filled by
   uncompressing the top bit of INPUT[IN_LEN - 1].  */
static void
wi_unpack (unsigned HOST_HALF_WIDE_INT *result, const HOST_WIDE_INT *input,
	   unsigned int in_len, unsigned int out_len,
	   unsigned int prec, signop sgn)
{
  unsigned int i;
  unsigned int j = 0;
  unsigned int small_prec = prec & (HOST_BITS_PER_WIDE_INT - 1);
  unsigned int blocks_needed = BLOCKS_NEEDED (prec);
  HOST_WIDE_INT mask;

  if (sgn == SIGNED)
    {
      mask = -top_bit_of ((const HOST_WIDE_INT *) input, in_len, prec);
      mask &= HALF_INT_MASK;
    }
  else
    mask = 0;

  for (i = 0; i < blocks_needed - 1; i++)
    {
      HOST_WIDE_INT x = safe_uhwi (input, in_len, i);
      result[j++] = x;
      result[j++] = x >> HOST_BITS_PER_HALF_WIDE_INT;
    }

  HOST_WIDE_INT x = safe_uhwi (input, in_len, i);
  if (small_prec)
    {
      if (sgn == SIGNED)
	x = sext_hwi (x, small_prec);
      else
	x = zext_hwi (x, small_prec);
    }
  result[j++] = x;
  result[j++] = x >> HOST_BITS_PER_HALF_WIDE_INT;

  /* Smear the sign bit.  */
  while (j < out_len)
    result[j++] = mask;
}

/* The inverse of wi_unpack.  IN_LEN is the the number of input
   blocks.  The number of output blocks will be half this amount.  */
static void
wi_pack (unsigned HOST_WIDE_INT *result,
	 const unsigned HOST_HALF_WIDE_INT *input,
	 unsigned int in_len)
{
  unsigned int i = 0;
  unsigned int j = 0;

  while (i + 2 < in_len)
    {
      result[j++] = (unsigned HOST_WIDE_INT)input[i]
	| ((unsigned HOST_WIDE_INT)input[i + 1]
	   << HOST_BITS_PER_HALF_WIDE_INT);
      i += 2;
    }

  /* Handle the case where in_len is odd.   For this we zero extend.  */
  if (in_len & 1)
    result[j++] = (unsigned HOST_WIDE_INT)input[i];
  else
    result[j++] = (unsigned HOST_WIDE_INT)input[i]
      | ((unsigned HOST_WIDE_INT)input[i + 1] << HOST_BITS_PER_HALF_WIDE_INT);
}

/* Multiply Op1 by Op2.  If HIGH is set, only the upper half of the
   result is returned.  

   If HIGH is not set, throw away the upper half after the check is
   made to see if it overflows.  Unfortunately there is no better way
   to check for overflow than to do this.  If OVERFLOW is nonnull,
   record in *OVERFLOW whether the result overflowed.  SGN controls
   the signedness and is used to check overflow or if HIGH is set.  */
unsigned int
wi::mul_internal (HOST_WIDE_INT *val, const HOST_WIDE_INT *op1val,
		  unsigned int op1len, const HOST_WIDE_INT *op2val,
		  unsigned int op2len, unsigned int prec, signop sgn,
		  bool *overflow, bool high)
{
  unsigned HOST_WIDE_INT o0, o1, k, t;
  unsigned int i;
  unsigned int j;
  unsigned int blocks_needed = BLOCKS_NEEDED (prec);
  unsigned int half_blocks_needed = blocks_needed * 2;
  /* The sizes here are scaled to support a 2x largest mode by 2x
     largest mode yielding a 4x largest mode result.  This is what is
     needed by vpn.  */

  unsigned HOST_HALF_WIDE_INT
    u[4 * MAX_BITSIZE_MODE_ANY_INT / HOST_BITS_PER_HALF_WIDE_INT];
  unsigned HOST_HALF_WIDE_INT
    v[4 * MAX_BITSIZE_MODE_ANY_INT / HOST_BITS_PER_HALF_WIDE_INT];
  /* The '2' in 'R' is because we are internally doing a full
     multiply.  */
  unsigned HOST_HALF_WIDE_INT
    r[2 * 4 * MAX_BITSIZE_MODE_ANY_INT / HOST_BITS_PER_HALF_WIDE_INT];
  HOST_WIDE_INT mask = ((HOST_WIDE_INT)1 << HOST_BITS_PER_HALF_WIDE_INT) - 1;

  /* If the top level routine did not really pass in an overflow, then
     just make sure that we never attempt to set it.  */
  bool needs_overflow = (overflow != 0);
  if (needs_overflow)
    *overflow = false;

  wide_int_ref op1 = wi::storage_ref (op1val, op1len, prec);
  wide_int_ref op2 = wi::storage_ref (op2val, op2len, prec);

  /* This is a surprisingly common case, so do it first.  */
  if (op1 == 0 || op2 == 0)
    {
      val[0] = 0;
      return 1;
    }

#ifdef umul_ppmm
  if (sgn == UNSIGNED)
    {
      /* If the inputs are single HWIs and the output has room for at
	 least two HWIs, we can use umul_ppmm directly.  */
      if (prec >= HOST_BITS_PER_WIDE_INT * 2
	  && wi::fits_uhwi_p (op1)
	  && wi::fits_uhwi_p (op2))
	{
	  umul_ppmm (val[1], val[0], op1.ulow (), op2.ulow ());
	  return 1 + (val[1] != 0 || val[0] < 0);
	}
      /* Likewise if the output is a full single HWI, except that the
	 upper HWI of the result is only used for determining overflow.
	 (We handle this case inline when overflow isn't needed.)  */
      else if (prec == HOST_BITS_PER_WIDE_INT)
	{
	  unsigned HOST_WIDE_INT upper;
	  umul_ppmm (upper, val[0], op1.ulow (), op2.ulow ());
	  if (needs_overflow)
	    *overflow = (upper != 0);
	  return 1;
	}
    }
#endif

  /* Handle multiplications by 1.  */
  if (op1 == 1)
    {
      for (i = 0; i < op2len; i++)
	val[i] = op2val[i];
      return op2len;
    }
  if (op2 == 1)
    {
      for (i = 0; i < op1len; i++)
	val[i] = op1val[i];
      return op1len;
    }

  /* If we need to check for overflow, we can only do half wide
     multiplies quickly because we need to look at the top bits to
     check for the overflow.  */
  if ((high || needs_overflow)
      && (prec <= HOST_BITS_PER_HALF_WIDE_INT))
    {
      unsigned HOST_WIDE_INT r;

      if (sgn == SIGNED)
	{
	  o0 = op1.to_shwi ();
	  o1 = op2.to_shwi ();
	}
      else
	{
	  o0 = op1.to_uhwi ();
	  o1 = op2.to_uhwi ();
	}

      r = o0 * o1;
      if (needs_overflow)
	{
	  if (sgn == SIGNED)
	    {
	      if ((HOST_WIDE_INT) r != sext_hwi (r, prec))
		*overflow = true;
	    }
	  else
	    {
	      if ((r >> prec) != 0)
		*overflow = true;
	    }
	}
      val[0] = high ? r >> prec : r;
      return 1;
    }

  /* We do unsigned mul and then correct it.  */
  wi_unpack (u, op1val, op1len, half_blocks_needed, prec, SIGNED);
  wi_unpack (v, op2val, op2len, half_blocks_needed, prec, SIGNED);

  /* The 2 is for a full mult.  */
  memset (r, 0, half_blocks_needed * 2
	  * HOST_BITS_PER_HALF_WIDE_INT / CHAR_BIT);

  for (j = 0; j < half_blocks_needed; j++)
    {
      k = 0;
      for (i = 0; i < half_blocks_needed; i++)
	{
	  t = ((unsigned HOST_WIDE_INT)u[i] * (unsigned HOST_WIDE_INT)v[j]
	       + r[i + j] + k);
	  r[i + j] = t & HALF_INT_MASK;
	  k = t >> HOST_BITS_PER_HALF_WIDE_INT;
	}
      r[j + half_blocks_needed] = k;
    }

  /* We did unsigned math above.  For signed we must adjust the
     product (assuming we need to see that).  */
  if (sgn == SIGNED && (high || needs_overflow))
    {
      unsigned HOST_WIDE_INT b;
      if (wi::neg_p (op1))
	{
	  b = 0;
	  for (i = 0; i < half_blocks_needed; i++)
	    {
	      t = (unsigned HOST_WIDE_INT)r[i + half_blocks_needed]
		- (unsigned HOST_WIDE_INT)v[i] - b;
	      r[i + half_blocks_needed] = t & HALF_INT_MASK;
	      b = t >> (HOST_BITS_PER_WIDE_INT - 1);
	    }
	}
      if (wi::neg_p (op2))
	{
	  b = 0;
	  for (i = 0; i < half_blocks_needed; i++)
	    {
	      t = (unsigned HOST_WIDE_INT)r[i + half_blocks_needed]
		- (unsigned HOST_WIDE_INT)u[i] - b;
	      r[i + half_blocks_needed] = t & HALF_INT_MASK;
	      b = t >> (HOST_BITS_PER_WIDE_INT - 1);
	    }
	}
    }

  if (needs_overflow)
    {
      HOST_WIDE_INT top;

      /* For unsigned, overflow is true if any of the top bits are set.
	 For signed, overflow is true if any of the top bits are not equal
	 to the sign bit.  */
      if (sgn == UNSIGNED)
	top = 0;
      else
	{
	  top = r[(half_blocks_needed) - 1];
	  top = SIGN_MASK (top << (HOST_BITS_PER_WIDE_INT / 2));
	  top &= mask;
	}

      for (i = half_blocks_needed; i < half_blocks_needed * 2; i++)
	if (((HOST_WIDE_INT)(r[i] & mask)) != top)
	  *overflow = true;
    }

  if (high)
    {
      /* compute [prec] <- ([prec] * [prec]) >> [prec] */
      wi_pack ((unsigned HOST_WIDE_INT *) val,
	       &r[half_blocks_needed], half_blocks_needed);
      return canonize (val, blocks_needed, prec);
    }
  else
    {
      /* compute [prec] <- ([prec] * [prec]) && ((1 << [prec]) - 1) */
      wi_pack ((unsigned HOST_WIDE_INT *) val, r, half_blocks_needed);
      return canonize (val, blocks_needed, prec);
    }
}

/* Compute the population count of X.  */
int
wi::popcount (const wide_int_ref &x)
{
  unsigned int i;
  int count;

  /* The high order block is special if it is the last block and the
     precision is not an even multiple of HOST_BITS_PER_WIDE_INT.  We
     have to clear out any ones above the precision before doing
     popcount on this block.  */
  count = x.precision - x.len * HOST_BITS_PER_WIDE_INT;
  unsigned int stop = x.len;
  if (count < 0)
    {
      count = popcount_hwi (x.uhigh () << -count);
      stop -= 1;
    }
  else
    {
      if (x.sign_mask () >= 0)
	count = 0;
    }

  for (i = 0; i < stop; ++i)
    count += popcount_hwi (x.val[i]);

  return count;
}

/* Set VAL to OP0 - OP1.  If OVERFLOW is nonnull, record in *OVERFLOW
   whether the result overflows when OP0 and OP1 are treated as having
   signedness SGN.  Return the number of blocks in VAL.  */
unsigned int
wi::sub_large (HOST_WIDE_INT *val, const HOST_WIDE_INT *op0,
	       unsigned int op0len, const HOST_WIDE_INT *op1,
	       unsigned int op1len, unsigned int prec,
	       signop sgn, bool *overflow)
{
  unsigned HOST_WIDE_INT o0 = 0;
  unsigned HOST_WIDE_INT o1 = 0;
  unsigned HOST_WIDE_INT x = 0;
  /* We implement subtraction as an in place negate and add.  Negation
     is just inversion and add 1, so we can do the add of 1 by just
     starting the borrow in of the first element at 1.  */
  unsigned HOST_WIDE_INT borrow = 0;
  unsigned HOST_WIDE_INT old_borrow = 0;

  unsigned HOST_WIDE_INT mask0, mask1;
  unsigned int i;

  unsigned int len = MAX (op0len, op1len);
  mask0 = -top_bit_of (op0, op0len, prec);
  mask1 = -top_bit_of (op1, op1len, prec);

  /* Subtract all of the explicitly defined elements.  */
  for (i = 0; i < len; i++)
    {
      o0 = i < op0len ? (unsigned HOST_WIDE_INT)op0[i] : mask0;
      o1 = i < op1len ? (unsigned HOST_WIDE_INT)op1[i] : mask1;
      x = o0 - o1 - borrow;
      val[i] = x;
      old_borrow = borrow;
      borrow = borrow == 0 ? o0 < o1 : o0 <= o1;
    }

  if (len * HOST_BITS_PER_WIDE_INT < prec)
    {
      val[len] = mask0 - mask1 - borrow;
      len++;
      if (overflow)
	*overflow = false;
    }
  else if (overflow)
    {
      unsigned int shift = -prec % HOST_BITS_PER_WIDE_INT;
      if (sgn == SIGNED)
	{
	  unsigned HOST_WIDE_INT x = (o0 ^ o1) & (val[len - 1] ^ o0);
	  *overflow = (HOST_WIDE_INT) (x << shift) < 0;
	}
      else
	{
	  /* Put the MSB of X and O0 and in the top of the HWI.  */
	  x <<= shift;
	  o0 <<= shift;
	  if (old_borrow)
	    *overflow = (x >= o0);
	  else
	    *overflow = (x > o0);
	}
    }

  return canonize (val, len, prec);
}


/*
 * Division and Mod
 */

/* Compute B_QUOTIENT and B_REMAINDER from B_DIVIDEND/B_DIVISOR.  The
   algorithm is a small modification of the algorithm in Hacker's
   Delight by Warren, which itself is a small modification of Knuth's
   algorithm.  M is the number of significant elements of U however
   there needs to be at least one extra element of B_DIVIDEND
   allocated, N is the number of elements of B_DIVISOR.  */
static void
divmod_internal_2 (unsigned HOST_HALF_WIDE_INT *b_quotient,
		   unsigned HOST_HALF_WIDE_INT *b_remainder,
		   unsigned HOST_HALF_WIDE_INT *b_dividend,
		   unsigned HOST_HALF_WIDE_INT *b_divisor,
		   int m, int n)
{
  /* The "digits" are a HOST_HALF_WIDE_INT which the size of half of a
     HOST_WIDE_INT and stored in the lower bits of each word.  This
     algorithm should work properly on both 32 and 64 bit
     machines.  */
  unsigned HOST_WIDE_INT b
    = (unsigned HOST_WIDE_INT)1 << HOST_BITS_PER_HALF_WIDE_INT;
  unsigned HOST_WIDE_INT qhat;   /* Estimate of quotient digit.  */
  unsigned HOST_WIDE_INT rhat;   /* A remainder.  */
  unsigned HOST_WIDE_INT p;      /* Product of two digits.  */
  HOST_WIDE_INT t, k;
  int i, j, s;

  /* Single digit divisor.  */
  if (n == 1)
    {
      k = 0;
      for (j = m - 1; j >= 0; j--)
	{
	  b_quotient[j] = (k * b + b_dividend[j])/b_divisor[0];
	  k = ((k * b + b_dividend[j])
	       - ((unsigned HOST_WIDE_INT)b_quotient[j]
		  * (unsigned HOST_WIDE_INT)b_divisor[0]));
	}
      b_remainder[0] = k;
      return;
    }

  s = clz_hwi (b_divisor[n-1]) - HOST_BITS_PER_HALF_WIDE_INT; /* CHECK clz */

  if (s)
    {
      /* Normalize B_DIVIDEND and B_DIVISOR.  Unlike the published
	 algorithm, we can overwrite b_dividend and b_divisor, so we do
	 that.  */
      for (i = n - 1; i > 0; i--)
	b_divisor[i] = (b_divisor[i] << s)
	  | (b_divisor[i-1] >> (HOST_BITS_PER_HALF_WIDE_INT - s));
      b_divisor[0] = b_divisor[0] << s;

      b_dividend[m] = b_dividend[m-1] >> (HOST_BITS_PER_HALF_WIDE_INT - s);
      for (i = m - 1; i > 0; i--)
	b_dividend[i] = (b_dividend[i] << s)
	  | (b_dividend[i-1] >> (HOST_BITS_PER_HALF_WIDE_INT - s));
      b_dividend[0] = b_dividend[0] << s;
    }

  /* Main loop.  */
  for (j = m - n; j >= 0; j--)
    {
      qhat = (b_dividend[j+n] * b + b_dividend[j+n-1]) / b_divisor[n-1];
      rhat = (b_dividend[j+n] * b + b_dividend[j+n-1]) - qhat * b_divisor[n-1];
    again:
      if (qhat >= b || qhat * b_divisor[n-2] > b * rhat + b_dividend[j+n-2])
	{
	  qhat -= 1;
	  rhat += b_divisor[n-1];
	  if (rhat < b)
	    goto again;
	}

      /* Multiply and subtract.  */
      k = 0;
      for (i = 0; i < n; i++)
	{
	  p = qhat * b_divisor[i];
	  t = b_dividend[i+j] - k - (p & HALF_INT_MASK);
	  b_dividend[i + j] = t;
	  k = ((p >> HOST_BITS_PER_HALF_WIDE_INT)
	       - (t >> HOST_BITS_PER_HALF_WIDE_INT));
	}
      t = b_dividend[j+n] - k;
      b_dividend[j+n] = t;

      b_quotient[j] = qhat;
      if (t < 0)
	{
	  b_quotient[j] -= 1;
	  k = 0;
	  for (i = 0; i < n; i++)
	    {
	      t = (HOST_WIDE_INT)b_dividend[i+j] + b_divisor[i] + k;
	      b_dividend[i+j] = t;
	      k = t >> HOST_BITS_PER_HALF_WIDE_INT;
	    }
	  b_dividend[j+n] += k;
	}
    }
  if (s)
    for (i = 0; i < n; i++)
      b_remainder[i] = (b_dividend[i] >> s)
	| (b_dividend[i+1] << (HOST_BITS_PER_HALF_WIDE_INT - s));
  else
    for (i = 0; i < n; i++)
      b_remainder[i] = b_dividend[i];
}


/* Divide DIVIDEND by DIVISOR, which have signedness SGN, and truncate
   the result.  If QUOTIENT is nonnull, store the value of the quotient
   there and return the number of blocks in it.  The return value is
   not defined otherwise.  If REMAINDER is nonnull, store the value
   of the remainder there and store the number of blocks in
   *REMAINDER_LEN.  If OFLOW is not null, store in *OFLOW whether
   the division overflowed.  */
unsigned int
wi::divmod_internal (HOST_WIDE_INT *quotient, unsigned int *remainder_len,
		     HOST_WIDE_INT *remainder,
		     const HOST_WIDE_INT *dividend_val,
		     unsigned int dividend_len, unsigned int dividend_prec,
		     const HOST_WIDE_INT *divisor_val, unsigned int divisor_len,
		     unsigned int divisor_prec, signop sgn,
		     bool *oflow)
{
  unsigned int dividend_blocks_needed = 2 * BLOCKS_NEEDED (dividend_prec);
  unsigned int divisor_blocks_needed = 2 * BLOCKS_NEEDED (divisor_prec);
  unsigned HOST_HALF_WIDE_INT
    b_quotient[4 * MAX_BITSIZE_MODE_ANY_INT / HOST_BITS_PER_HALF_WIDE_INT];
  unsigned HOST_HALF_WIDE_INT
    b_remainder[4 * MAX_BITSIZE_MODE_ANY_INT / HOST_BITS_PER_HALF_WIDE_INT];
  unsigned HOST_HALF_WIDE_INT
    b_dividend[(4 * MAX_BITSIZE_MODE_ANY_INT / HOST_BITS_PER_HALF_WIDE_INT) + 1];
  unsigned HOST_HALF_WIDE_INT
    b_divisor[4 * MAX_BITSIZE_MODE_ANY_INT / HOST_BITS_PER_HALF_WIDE_INT];
  unsigned int m, n;
  bool dividend_neg = false;
  bool divisor_neg = false;
  bool overflow = false;
  wide_int neg_dividend, neg_divisor;

  wide_int_ref dividend = wi::storage_ref (dividend_val, dividend_len,
					   dividend_prec);
  wide_int_ref divisor = wi::storage_ref (divisor_val, divisor_len,
					  divisor_prec);
  if (divisor == 0)
    overflow = true;

  /* The smallest signed number / -1 causes overflow.  The dividend_len
     check is for speed rather than correctness.  */
  if (sgn == SIGNED
      && dividend_len == BLOCKS_NEEDED (dividend_prec)
      && divisor == -1
      && wi::only_sign_bit_p (dividend))
    overflow = true;

  /* Handle the overflow cases.  Viewed as unsigned value, the quotient of
     (signed min / -1) has the same representation as the orignal dividend.
     We have traditionally made division by zero act as division by one,
     so there too we use the original dividend.  */
  if (overflow)
    {
      if (remainder)
	{
	  *remainder_len = 1;
	  remainder[0] = 0;
	}
      if (oflow != 0)
	*oflow = true;
      if (quotient)
	for (unsigned int i = 0; i < dividend_len; ++i)
	  quotient[i] = dividend_val[i];
      return dividend_len;
    }

  if (oflow)
    *oflow = false;

  /* Do it on the host if you can.  */
  if (sgn == SIGNED
      && wi::fits_shwi_p (dividend)
      && wi::fits_shwi_p (divisor))
    {
      HOST_WIDE_INT o0 = dividend.to_shwi ();
      HOST_WIDE_INT o1 = divisor.to_shwi ();

      if (o0 == HOST_WIDE_INT_MIN && o1 == -1)
	{
	  gcc_checking_assert (dividend_prec > HOST_BITS_PER_WIDE_INT);
	  if (quotient)
	    {
	      quotient[0] = HOST_WIDE_INT_MIN;
	      quotient[1] = 0;
	    }
	  if (remainder)
	    {
	      remainder[0] = 0;
	      *remainder_len = 1;
	    }
	  return 2;
	}
      else
	{
	  if (quotient)
	    quotient[0] = o0 / o1;
	  if (remainder)
	    {
	      remainder[0] = o0 % o1;
	      *remainder_len = 1;
	    }
	  return 1;
	}
    }

  if (sgn == UNSIGNED
      && wi::fits_uhwi_p (dividend)
      && wi::fits_uhwi_p (divisor))
    {
      unsigned HOST_WIDE_INT o0 = dividend.to_uhwi ();
      unsigned HOST_WIDE_INT o1 = divisor.to_uhwi ();

      if (quotient)
	quotient[0] = o0 / o1;
      if (remainder)
	{
	  remainder[0] = o0 % o1;
	  *remainder_len = 1;
	}
      return 1;
    }

  /* Make the divisor and dividend positive and remember what we
     did.  */
  if (sgn == SIGNED)
    {
      if (wi::neg_p (dividend))
	{
	  neg_dividend = -dividend;
	  dividend = neg_dividend;
	  dividend_neg = true;
	}
      if (wi::neg_p (divisor))
	{
	  neg_divisor = -divisor;
	  divisor = neg_divisor;
	  divisor_neg = true;
	}
    }

  wi_unpack (b_dividend, dividend.get_val (), dividend.get_len (),
	     dividend_blocks_needed, dividend_prec, sgn);
  wi_unpack (b_divisor, divisor.get_val (), divisor.get_len (),
	     divisor_blocks_needed, divisor_prec, sgn);

  m = dividend_blocks_needed;
  while (m > 1 && b_dividend[m - 1] == 0)
    m--;

  n = divisor_blocks_needed;
  while (n > 1 && b_divisor[n - 1] == 0)
    n--;

  memset (b_quotient, 0, sizeof (b_quotient));

  divmod_internal_2 (b_quotient, b_remainder, b_dividend, b_divisor, m, n);

  unsigned int quotient_len = 0;
  if (quotient)
    {
      wi_pack ((unsigned HOST_WIDE_INT *) quotient, b_quotient, m);
      quotient_len = canonize (quotient, (m + 1) / 2, dividend_prec);
      /* The quotient is neg if exactly one of the divisor or dividend is
	 neg.  */
      if (dividend_neg != divisor_neg)
	quotient_len = wi::sub_large (quotient, zeros, 1, quotient,
				      quotient_len, dividend_prec,
				      UNSIGNED, 0);
    }

  if (remainder)
    {
      wi_pack ((unsigned HOST_WIDE_INT *) remainder, b_remainder, n);
      *remainder_len = canonize (remainder, (n + 1) / 2, dividend_prec);
      /* The remainder is always the same sign as the dividend.  */
      if (dividend_neg)
	*remainder_len = wi::sub_large (remainder, zeros, 1, remainder,
					*remainder_len, dividend_prec,
					UNSIGNED, 0);
    }

  return quotient_len;
}

/*
 * Shifting, rotating and extraction.
 */

/* Left shift XVAL by SHIFT and store the result in VAL.  Return the
   number of blocks in VAL.  Both XVAL and VAL have PRECISION bits.  */
unsigned int
wi::lshift_large (HOST_WIDE_INT *val, const HOST_WIDE_INT *xval,
		  unsigned int xlen, unsigned int precision,
		  unsigned int shift)
{
  /* Split the shift into a whole-block shift and a subblock shift.  */
  unsigned int skip = shift / HOST_BITS_PER_WIDE_INT;
  unsigned int small_shift = shift % HOST_BITS_PER_WIDE_INT;

  /* The whole-block shift fills with zeros.  */
  unsigned int len = BLOCKS_NEEDED (precision);
  for (unsigned int i = 0; i < skip; ++i)
    val[i] = 0;

  /* It's easier to handle the simple block case specially.  */
  if (small_shift == 0)
    for (unsigned int i = skip; i < len; ++i)
      val[i] = safe_uhwi (xval, xlen, i - skip);
  else
    {
      /* The first unfilled output block is a left shift of the first
	 block in XVAL.  The other output blocks contain bits from two
	 consecutive input blocks.  */
      unsigned HOST_WIDE_INT carry = 0;
      for (unsigned int i = skip; i < len; ++i)
	{
	  unsigned HOST_WIDE_INT x = safe_uhwi (xval, xlen, i - skip);
	  val[i] = (x << small_shift) | carry;
	  carry = x >> (-small_shift % HOST_BITS_PER_WIDE_INT);
	}
    }
  return canonize (val, len, precision);
}

/* Right shift XVAL by SHIFT and store the result in VAL.  Return the
   number of blocks in VAL.  The input has XPRECISION bits and the
   output has XPRECISION - SHIFT bits.  */
static unsigned int
rshift_large_common (HOST_WIDE_INT *val, const HOST_WIDE_INT *xval,
		     unsigned int xlen, unsigned int xprecision,
		     unsigned int shift)
{
  /* Split the shift into a whole-block shift and a subblock shift.  */
  unsigned int skip = shift / HOST_BITS_PER_WIDE_INT;
  unsigned int small_shift = shift % HOST_BITS_PER_WIDE_INT;

  /* Work out how many blocks are needed to store the significant bits
     (excluding the upper zeros or signs).  */
  unsigned int len = BLOCKS_NEEDED (xprecision - shift);

  /* It's easier to handle the simple block case specially.  */
  if (small_shift == 0)
    for (unsigned int i = 0; i < len; ++i)
      val[i] = safe_uhwi (xval, xlen, i + skip);
  else
    {
      /* Each output block but the last is a combination of two input blocks.
	 The last block is a right shift of the last block in XVAL.  */
      unsigned HOST_WIDE_INT curr = safe_uhwi (xval, xlen, skip);
      for (unsigned int i = 0; i < len; ++i)
	{
	  val[i] = curr >> small_shift;
	  curr = safe_uhwi (xval, xlen, i + skip + 1);
	  val[i] |= curr << (-small_shift % HOST_BITS_PER_WIDE_INT);
	}
    }
  return len;
}

/* Logically right shift XVAL by SHIFT and store the result in VAL.
   Return the number of blocks in VAL.  XVAL has XPRECISION bits and
   VAL has PRECISION bits.  */
unsigned int
wi::lrshift_large (HOST_WIDE_INT *val, const HOST_WIDE_INT *xval,
		   unsigned int xlen, unsigned int xprecision,
		   unsigned int precision, unsigned int shift)
{
  unsigned int len = rshift_large_common (val, xval, xlen, xprecision, shift);

  /* The value we just created has precision XPRECISION - SHIFT.
     Zero-extend it to wider precisions.  */
  if (precision > xprecision - shift)
    {
      unsigned int small_prec = (xprecision - shift) % HOST_BITS_PER_WIDE_INT;
      if (small_prec)
	val[len - 1] = zext_hwi (val[len - 1], small_prec);
      else if (val[len - 1] < 0)
	{
	  /* Add a new block with a zero. */
	  val[len++] = 0;
	  return len;
	}
    }
  return canonize (val, len, precision);
}

/* Arithmetically right shift XVAL by SHIFT and store the result in VAL.
   Return the number of blocks in VAL.  XVAL has XPRECISION bits and
   VAL has PRECISION bits.  */
unsigned int
wi::arshift_large (HOST_WIDE_INT *val, const HOST_WIDE_INT *xval,
		   unsigned int xlen, unsigned int xprecision,
		   unsigned int precision, unsigned int shift)
{
  unsigned int len = rshift_large_common (val, xval, xlen, xprecision, shift);

  /* The value we just created has precision XPRECISION - SHIFT.
     Sign-extend it to wider types.  */
  if (precision > xprecision - shift)
    {
      unsigned int small_prec = (xprecision - shift) % HOST_BITS_PER_WIDE_INT;
      if (small_prec)
	val[len - 1] = sext_hwi (val[len - 1], small_prec);
    }
  return canonize (val, len, precision);
}

/* Return the number of leading (upper) zeros in X.  */
int
wi::clz (const wide_int_ref &x)
{
  /* Calculate how many bits there above the highest represented block.  */
  int count = x.precision - x.len * HOST_BITS_PER_WIDE_INT;

  unsigned HOST_WIDE_INT high = x.uhigh ();
  if (count < 0)
    /* The upper -COUNT bits of HIGH are not part of the value.
       Clear them out.  */
    high = (high << -count) >> -count;
  else if (x.sign_mask () < 0)
    /* The upper bit is set, so there are no leading zeros.  */
    return 0;

  /* We don't need to look below HIGH.  Either HIGH is nonzero,
     or the top bit of the block below is nonzero; clz_hwi is
     HOST_BITS_PER_WIDE_INT in the latter case.  */
  return count + clz_hwi (high);
}

/* Return the number of redundant sign bits in X.  (That is, the number
   of bits immediately below the sign bit that have the same value as
   the sign bit.)  */
int
wi::clrsb (const wide_int_ref &x)
{
  /* Calculate how many bits there above the highest represented block.  */
  int count = x.precision - x.len * HOST_BITS_PER_WIDE_INT;

  unsigned HOST_WIDE_INT high = x.uhigh ();
  unsigned HOST_WIDE_INT mask = -1;
  if (count < 0)
    {
      /* The upper -COUNT bits of HIGH are not part of the value.
	 Clear them from both MASK and HIGH.  */
      mask >>= -count;
      high &= mask;
    }

  /* If the top bit is 1, count the number of leading 1s.  If the top
     bit is zero, count the number of leading zeros.  */
  if (high > mask / 2)
    high ^= mask;

  /* There are no sign bits below the top block, so we don't need to look
     beyond HIGH.  Note that clz_hwi is HOST_BITS_PER_WIDE_INT when
     HIGH is 0.  */
  return count + clz_hwi (high) - 1;
}

/* Return the number of trailing (lower) zeros in X.  */
int
wi::ctz (const wide_int_ref &x)
{
  if (x.len == 1 && x.ulow () == 0)
    return x.precision;

  /* Having dealt with the zero case, there must be a block with a
     nonzero bit.  We don't care about the bits above the first 1.  */
  unsigned int i = 0;
  while (x.val[i] == 0)
    ++i;
  return i * HOST_BITS_PER_WIDE_INT + ctz_hwi (x.val[i]);
}

/* If X is an exact power of 2, return the base-2 logarithm, otherwise
   return -1.  */
int
wi::exact_log2 (const wide_int_ref &x)
{
  /* Reject cases where there are implicit -1 blocks above HIGH.  */
  if (x.len * HOST_BITS_PER_WIDE_INT < x.precision && x.sign_mask () < 0)
    return -1;

  /* Set CRUX to the index of the entry that should be nonzero.
     If the top block is zero then the next lowest block (if any)
     must have the high bit set.  */
  unsigned int crux = x.len - 1;
  if (crux > 0 && x.val[crux] == 0)
    crux -= 1;

  /* Check that all lower blocks are zero.  */
  for (unsigned int i = 0; i < crux; ++i)
    if (x.val[i] != 0)
      return -1;

  /* Get a zero-extended form of block CRUX.  */
  unsigned HOST_WIDE_INT hwi = x.val[crux];
  if ((crux + 1) * HOST_BITS_PER_WIDE_INT > x.precision)
    hwi = zext_hwi (hwi, x.precision % HOST_BITS_PER_WIDE_INT);

  /* Now it's down to whether HWI is a power of 2.  */
  int res = ::exact_log2 (hwi);
  if (res >= 0)
    res += crux * HOST_BITS_PER_WIDE_INT;
  return res;
}

/* Return the base-2 logarithm of X, rounding down.  Return -1 if X is 0.  */
int
wi::floor_log2 (const wide_int_ref &x)
{
  return x.precision - 1 - clz (x);
}

/* Return the index of the first (lowest) set bit in X, counting from 1.
   Return 0 if X is 0.  */
int
wi::ffs (const wide_int_ref &x)
{
  return eq_p (x, 0) ? 0 : ctz (x) + 1;
}

/* Return true if sign-extending X to have precision PRECISION would give
   the minimum signed value at that precision.  */
bool
wi::only_sign_bit_p (const wide_int_ref &x, unsigned int precision)
{
  return ctz (x) + 1 == int (precision);
}

/* Return true if X represents the minimum signed value.  */
bool
wi::only_sign_bit_p (const wide_int_ref &x)
{
  return only_sign_bit_p (x, x.precision);
}

/*
 * Private utilities.
 */

void gt_ggc_mx (widest_int *) { }
void gt_pch_nx (widest_int *, void (*) (void *, void *), void *) { }
void gt_pch_nx (widest_int *) { }

template void wide_int::dump () const;
template void generic_wide_int <wide_int_ref_storage <false> >::dump () const;
template void generic_wide_int <wide_int_ref_storage <true> >::dump () const;
template void offset_int::dump () const;
template void widest_int::dump () const;
