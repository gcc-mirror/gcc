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
#include "rtl.h"
#include "tree.h"
#include "dumpfile.h"

/* This is the maximal size of the buffer needed for dump.  */
const int MAX_SIZE = 4 * (MAX_BITSIZE_MODE_ANY_INT / 4
		     + MAX_BITSIZE_MODE_ANY_INT / HOST_BITS_PER_WIDE_INT + 32);

/*
 * Internal utilities.
 */

/* Quantities to deal with values that hold half of a wide int.  Used
   in multiply and divide.  */
#define HALF_INT_MASK (((HOST_WIDE_INT)1 << HOST_BITS_PER_HALF_WIDE_INT) - 1)

#define BLOCK_OF(TARGET) ((TARGET) / HOST_BITS_PER_WIDE_INT)
#define BLOCKS_NEEDED(PREC) \
  (PREC ? (((PREC) + HOST_BITS_PER_WIDE_INT - 1) / HOST_BITS_PER_WIDE_INT) : 1)
#define SIGN_MASK(X) (((HOST_WIDE_INT)X) >> (HOST_BITS_PER_WIDE_INT - 1))

/*
 * Conversion routines in and out of wide-int.
 */

/* Convert OP0 into a wide int of PRECISION.  */
wide_int_ro
wide_int_ro::from_shwi (HOST_WIDE_INT op0,
			unsigned int precision)
{
  wide_int result;

  result.precision = precision;
  result.val[0] = op0;
  result.len = 1;

#ifdef DEBUG_WIDE_INT
  debug_wh ("wide_int::from_shwi %s " HOST_WIDE_INT_PRINT_HEX ")\n",
	    result, op0);
#endif

  return result;
}

/* Convert OP0 into a wide int of PRECISION. */
wide_int_ro
wide_int_ro::from_uhwi (unsigned HOST_WIDE_INT op0,
			unsigned int precision)
{
  wide_int result;

  result.precision = precision;
  result.val[0] = op0;

  /* If the top bit is a 1, we need to add another word of 0s since
     that would not expand the right value since the infinite
     expansion of any unsigned number must have 0s at the top.  */
  if ((HOST_WIDE_INT)op0 < 0 && precision > HOST_BITS_PER_WIDE_INT)
    {
      result.val[1] = 0;
      result.len = 2;
    }
  else
    result.len = 1;

#ifdef DEBUG_WIDE_INT
  debug_wh ("wide_int::from_uhwi %s " HOST_WIDE_INT_PRINT_HEX ")\n",
	    result, op0);
#endif

  return result;
}

/* Create a wide_int from an array of host_wide_ints in OP1 of LEN.
   The result has PRECISION.  */
wide_int_ro
wide_int_ro::from_array (const HOST_WIDE_INT *op1, unsigned int len,
			 unsigned int precision, bool need_canon)
{
  unsigned int i;
  wide_int result;

  result.len = len;
  result.precision = precision;

  for (i=0; i < len; i++)
    result.val[i] = op1[i];

#ifdef DEBUG_WIDE_INT
  debug_wa ("wide_int::from_array %s = %s\n", result, op1, len, precision);
#endif

  if (need_canon)
    result.canonize ();

  return result;
}

/* Convert a double int into a wide int with precision PREC.  */
wide_int_ro
wide_int_ro::from_double_int (double_int di, unsigned int prec)
{
  HOST_WIDE_INT op = di.low;
  wide_int result;

  result.precision = prec;
  result.len = (prec <= HOST_BITS_PER_WIDE_INT) ? 1 : 2;

  if (prec < HOST_BITS_PER_WIDE_INT)
    result.val[0] = sext_hwi (op, prec);
  else
    {
      result.val[0] = op;
      if (prec > HOST_BITS_PER_WIDE_INT)
	{
	  if (prec < HOST_BITS_PER_DOUBLE_INT)
	    result.val[1] = sext_hwi (di.high, prec);
	  else
	    result.val[1] = di.high;
	}
    }

  if (result.len == 2)
    result.canonize ();

  return result;
}

/* Extract a constant integer from the R.  The bits of the integer are
   returned.  */
wide_int_ro
wide_int_ro::from_rtx (const rtx_mode_t r)
{
  const_rtx x = get_rtx (r);
  enum machine_mode mode = get_mode (r);
  wide_int result;
  unsigned int prec = GET_MODE_PRECISION (mode);

  gcc_assert (mode != VOIDmode);

  result.precision = prec;

  switch (GET_CODE (x))
    {
    case CONST_INT:
      result.val[0] = INTVAL (x);
      result.len = 1;
#ifdef DEBUG_WIDE_INT
      debug_wh ("wide_int:: %s = from_rtx ("HOST_WIDE_INT_PRINT_HEX")\n",
		result, INTVAL (x));
#endif
      break;

#if TARGET_SUPPORTS_WIDE_INT
    case CONST_WIDE_INT:
      {
	int i;
	result.len = CONST_WIDE_INT_NUNITS (x);
	
	for (i = 0; i < result.len; i++)
	  result.val[i] = CONST_WIDE_INT_ELT (x, i);
      }
      break;
#else
    case CONST_DOUBLE:
      result.len = 2;
      result.val[0] = CONST_DOUBLE_LOW (x);
      result.val[1] = CONST_DOUBLE_HIGH (x);

#ifdef DEBUG_WIDE_INT
      debug_whh ("wide_int:: %s = from_rtx ("HOST_WIDE_INT_PRINT_HEX" "HOST_WIDE_INT_PRINT_HEX")\n",
		 result, CONST_DOUBLE_HIGH (x), CONST_DOUBLE_LOW (x));
#endif

      break;
#endif

    default:
      gcc_unreachable ();
    }

  return result;
}

/* Construct a wide int from a buffer of length LEN.  BUFFER will be
   read according to byte endianess and word endianess of the target.
   Only the lower LEN bytes of the result are set; the remaining high
   bytes are cleared.  */
wide_int_ro
wide_int_ro::from_buffer (const unsigned char *buffer, int len)
{
  wide_int result = wide_int::zero (len * BITS_PER_UNIT);
  int words = len / UNITS_PER_WORD;

  /* We have to clear all the bits ourself, as we merely or in values
     below.  */
  result.len = BLOCKS_NEEDED (len*BITS_PER_UNIT);
  for (int i = 0; i < result.len; ++i)
    result.val[i] = 0;

  for (int byte = 0; byte < len; byte++)
    {
      int offset;
      int index;
      int bitpos = byte * BITS_PER_UNIT;
      unsigned HOST_WIDE_INT value;

      if (len > UNITS_PER_WORD)
	{
	  int word = byte / UNITS_PER_WORD;

	  if (WORDS_BIG_ENDIAN)
	    word = (words - 1) - word;

	  offset = word * UNITS_PER_WORD;

	  if (BYTES_BIG_ENDIAN)
	    offset += (UNITS_PER_WORD - 1) - (byte % UNITS_PER_WORD);
	  else
	    offset += byte % UNITS_PER_WORD;
	}
      else
	offset = BYTES_BIG_ENDIAN ? (len - 1) - byte : byte;

      value = (unsigned HOST_WIDE_INT) buffer[offset];

      index = bitpos / HOST_BITS_PER_WIDE_INT;
      result.val[index] |= value << (bitpos % HOST_BITS_PER_WIDE_INT);
    }

  result.canonize ();

  return result;
}

/* Sets RESULT from THIS, the sign is taken according to SGN.  */
void
wide_int_ro::to_mpz (mpz_t result, signop sgn) const
{
  bool negative = false;
  wide_int tmp;

  if ((*this).neg_p (sgn))
    {
      negative = true;
      /* We use ones complement to avoid -x80..0 edge case that -
	 won't work on.  */
      tmp = ~(*this);
    }
  else
    tmp = *this;

  mpz_import (result, tmp.len, -1, sizeof (HOST_WIDE_INT), 0, 0, tmp.val);

  if (negative)
    mpz_com (result, result);
}

/* Returns VAL converted to TYPE.  If WRAP is true, then out-of-range
   values of VAL will be wrapped; otherwise, they will be set to the
   appropriate minimum or maximum TYPE bound.  */
wide_int_ro
wide_int_ro::from_mpz (const_tree type, mpz_t val, bool wrap)
{
  size_t count, numb;
  wide_int res;
  unsigned int i;

  if (!wrap)
    {
      mpz_t min, max;

      mpz_init (min);
      mpz_init (max);
      get_type_static_bounds (type, min, max);

      if (mpz_cmp (val, min) < 0)
	mpz_set (val, min);
      else if (mpz_cmp (val, max) > 0)
	mpz_set (val, max);

      mpz_clear (min);
      mpz_clear (max);
    }

  /* Determine the number of unsigned HOST_WIDE_INTs that are required
     for representing the value.  The code to calculate count is
     extracted from the GMP manual, section "Integer Import and Export":
     http://gmplib.org/manual/Integer-Import-and-Export.html  */
  numb = 8*sizeof(HOST_WIDE_INT);
  count = (mpz_sizeinbase (val, 2) + numb-1) / numb;
  if (count < 1)
    count = 1;

  /* Need to initialize the number because it writes nothing for
     zero.  */
  for (i = 0; i < count; i++)
    res.val[i] = 0;

  res.len = count;

  mpz_export (res.val, &count, -1, sizeof (HOST_WIDE_INT), 0, 0, val);

  res.precision = TYPE_PRECISION (type);
  if (mpz_sgn (val) < 0)
    res = -res;

  return res;
}

/*
 * Largest and smallest values in a mode.
 */

/* Produce the largest SGNed number that is represented in TYPE_PREC.
   The resulting number is placed in a wide int of size RESULT_PREC.
   IF RESULT_PREC is 0, answer will have TYPE_PREC precision. */
wide_int_ro
wide_int_ro::max_value (unsigned int type_prec, signop sgn,
			unsigned int result_prec)
{
  unsigned int prec = result_prec ? result_prec : type_prec;

  if (type_prec == 0)
    return wide_int::zero (result_prec
			   ? result_prec
			   : TYPE_PRECISION (integer_type_node));

  if (sgn == UNSIGNED)
    {
      if (prec <= type_prec)
	/* The unsigned max is just all ones, for which the
	   compressed rep is just a single HWI.  */
	return wide_int::minus_one (prec);
      else
	return wide_int::mask (type_prec, false, prec);
    }
  else
    /* The signed max is all ones except the top bit.  This must be
       explicitly represented.  */
    return wide_int::mask (type_prec-1, false, prec);
}

/* Produce the smallest SGNed number that is represented in TYPE_PREC.
   The resulting number is placed in a wide int of size RESULT_PREC.
   IF RESULT_PREC is 0, answer will have TYPE_PREC precision.  */
wide_int_ro
wide_int_ro::min_value (unsigned int type_prec, signop sgn,
			unsigned int result_prec)
{
  if (result_prec == 0)
    result_prec = type_prec;

  if (type_prec == 0)
    return wide_int_ro::zero (result_prec
			      ? result_prec
			      : TYPE_PRECISION (integer_type_node));

  if (sgn == UNSIGNED)
    {
      /* The unsigned min is just all zeros, for which the compressed
	 rep is just a single HWI.  */
      wide_int result;
      result.len = 1;
      result.precision = result_prec;
      result.val[0] = 0;
      return result;
    }
  else
    {
      /* The signed min is all zeros except the top bit.  This must be
	 explicitly represented.  */
      return set_bit_in_zero (type_prec - 1, result_prec);
    }
}

/*
 * Public utilities.
 */

/* Check the upper HOST_WIDE_INTs of src to see if the length can be
   shortened.  An upper HOST_WIDE_INT is unnecessary if it is all ones
   or zeros and the top bit of the next lower word matches.

   This function may change the representation of THIS, but does not
   change the value that THIS represents.  It does not sign extend in
   the case that the size of the mode is less than
   HOST_BITS_PER_WIDE_INT.  */
void
wide_int_ro::canonize ()
{
  int small_prec = precision & (HOST_BITS_PER_WIDE_INT - 1);
  int blocks_needed = BLOCKS_NEEDED (precision);
  HOST_WIDE_INT top;
  int i;

  if (len > blocks_needed)
    len = blocks_needed;

  /* Clean up the top bits for any mode that is not a multiple of a
     HWI and is not compressed.  */
  if (len == blocks_needed && small_prec)
    val[len - 1] = sext_hwi (val[len - 1], small_prec);

  if (len == 1)
    return;

  top = val[len - 1];
  if (top != 0 && top != (HOST_WIDE_INT)-1)
    return;

  /* At this point we know that the top is either 0 or -1.  Find the
     first block that is not a copy of this.  */
  for (i = len - 2; i >= 0; i--)
    {
      HOST_WIDE_INT x = val[i];
      if (x != top)
	{
	  if (SIGN_MASK (x) == top)
	    {
	      len = i + 1;
	      return;
	    }

	  /* We need an extra block because the top bit block i does
	     not match the extension.  */
	  len = i + 2;
	  return;
	}
    }

  /* The number is 0 or -1.  */
  len = 1;
}

/* Copy THIS replacing the precision with PREC.  It can do any of
   truncation, extension or copying.  This function is only available
   with the default wide-int form as the other forms have fixed
   precisions.  */
wide_int_ro
wide_int_ro::force_to_size (unsigned int prec, signop sgn) const
{
  wide_int result;
  int blocks_needed = BLOCKS_NEEDED (prec);
  int i;

  result.precision = prec;
  /* If this is a value that has come in from a hwi, then it does not
     have a proper precision.  However, it is in canonical form, so
     just copy and zap in the precision and return.  */
  if (precision == 0)
    {
      /* Some zero prec numbers take 2 hwi's.  If the target prec is
	 small, we may need to shorten it.  */
      result.len = len;
      if (prec <= HOST_BITS_PER_WIDE_INT)
	result.len = 1;
      for (int i = 0; i < result.len; ++i)
	result.val[i] = val[i];
      return result;
    }

  result.len = blocks_needed < len ? blocks_needed : len;
  for (i = 0; i < result.len; i++)
    result.val[i] = val[i];

  if (prec == precision)
    /* Nothing much to do.  */
    ;
  else if (prec > precision)
    {
      /* Expanding */
      int small_precision = precision & (HOST_BITS_PER_WIDE_INT - 1);

      if (sgn == UNSIGNED)
	{
	  /* The top block in the existing rep must be zero extended.  */
	  if (small_precision
	      /* We need to ensure we only extend the last block of
		 the original number, if the number has not been
		 compressed.  If the number has been compressed, then
		 all the bits are significant.  */
	      && len == BLOCKS_NEEDED (precision))
	    result.val[len-1] = zext_hwi (result.val[len-1], small_precision);
	  else if (len < blocks_needed
		   && small_precision == 0
		   && result.val[result.len - 1] < 0)
	    {
	      /* We need to uncompress the original value first.  */
	      while (result.len < BLOCKS_NEEDED (precision))
		result.val[result.len++] = (HOST_WIDE_INT)-1;
	      /* We need to put the 0 block on top to keep the value
		 from being sign extended.  */
	      if (result.len < blocks_needed)
		result.val[result.len++] = 0;
	    }
	}
      /* We have to do this because we cannot guarantee that there is
	 not trash in the top block of an uncompressed value.  For a
	 compressed value, all the bits are significant.  */
      else if (small_precision
	       && len == BLOCKS_NEEDED (precision))
	result.val[len-1] = sext_hwi (result.val[len-1], small_precision);
    }
  else
    result.canonize ();

#ifdef DEBUG_WIDE_INT
  debug_wwvs ("wide_int:: %s = force_to_size (%s, prec = %d %s)\n",
	      result, *this, prec, sgn==UNSIGNED ? "U" : "S");
#endif

  return result;
}

/* This function hides the fact that we cannot rely on the bits beyond
   the precision.  This issue comes up in the relational comparisions
   where we do allow comparisons of values of different precisions.  */
static inline HOST_WIDE_INT
selt (const HOST_WIDE_INT *a, unsigned int len,
     unsigned int blocks_needed,
     unsigned int small_prec,
     unsigned int index, signop sgn)
{
  if (index >= len)
    {
      if (index < blocks_needed || sgn == SIGNED)
	/* Signed or within the precision.  */
	return SIGN_MASK (a[len - 1]);
      else
	/* Unsigned extension beyond the precision. */
	return 0;
    }

  if (small_prec && index == blocks_needed - 1)
    {
      /* The top block is partially outside of the precision.  */
      if (sgn == SIGNED)
	return sext_hwi (a[index], small_prec);
      else
	return zext_hwi (a[index], small_prec);
    }
  return a[index];
}

/* Find the hignest bit represented in a wide int.  This will in
   general have the same value as the sign bit.  */
static inline HOST_WIDE_INT
top_bit_of (const HOST_WIDE_INT *a, unsigned int len, unsigned int prec)
{
  if (len == BLOCKS_NEEDED (prec)
      && (prec & (HOST_BITS_PER_WIDE_INT - 1)))
    return (a[len - 1] >> (prec & (HOST_BITS_PER_WIDE_INT - 1))) & 1;
  else
    return (a[len - 1] >> (HOST_BITS_PER_WIDE_INT - 1)) & 1;
}

/*
 * Comparisons, note that only equality is an operator.  The other
 * comparisons cannot be operators since they are inherently singed or
 * unsigned and C++ has no such operators.
 */

/* Return true if OP0 == OP1.  */
bool
wide_int_ro::eq_p_large (const HOST_WIDE_INT *op0, unsigned int op0len,
			 unsigned int prec,
			 const HOST_WIDE_INT *op1, unsigned int op1len)
{
  int l0 = op0len - 1;
  unsigned int small_prec = prec & (HOST_BITS_PER_WIDE_INT - 1);

  while (op0len != op1len)
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
wide_int_ro::lts_p_large (const HOST_WIDE_INT *op0, unsigned int op0len,
			  unsigned int p0,
			  const HOST_WIDE_INT *op1, unsigned int op1len,
			  unsigned int p1)
{
  HOST_WIDE_INT s0, s1;
  unsigned HOST_WIDE_INT u0, u1;
  unsigned int blocks_needed0 = BLOCKS_NEEDED (p0);
  unsigned int blocks_needed1 = BLOCKS_NEEDED (p1);
  unsigned int small_prec0 = p0 & (HOST_BITS_PER_WIDE_INT - 1);
  unsigned int small_prec1 = p1 & (HOST_BITS_PER_WIDE_INT - 1);
  int l = MAX (op0len - 1, op1len - 1);

  /* Only the top block is compared as signed.  The rest are unsigned
     comparisons.  */
  s0 = selt (op0, op0len, blocks_needed0, small_prec0, l, SIGNED);
  s1 = selt (op1, op1len, blocks_needed1, small_prec1, l, SIGNED);
  if (s0 < s1)
    return true;
  if (s0 > s1)
    return false;

  l--;
  while (l >= 0)
    {
      u0 = selt (op0, op0len, blocks_needed0, small_prec0, l, SIGNED);
      u1 = selt (op1, op1len, blocks_needed1, small_prec1, l, SIGNED);

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
wide_int_ro::cmps_large (const HOST_WIDE_INT *op0, unsigned int op0len,
			 unsigned int p0,
			 const HOST_WIDE_INT *op1, unsigned int op1len,
			 unsigned int p1)
{
  HOST_WIDE_INT s0, s1;
  unsigned HOST_WIDE_INT u0, u1;
  unsigned int blocks_needed0 = BLOCKS_NEEDED (p0);
  unsigned int blocks_needed1 = BLOCKS_NEEDED (p1);
  unsigned int small_prec0 = p0 & (HOST_BITS_PER_WIDE_INT - 1);
  unsigned int small_prec1 = p1 & (HOST_BITS_PER_WIDE_INT - 1);
  int l = MAX (op0len - 1, op1len - 1);

  /* Only the top block is compared as signed.  The rest are unsigned
     comparisons.  */
  s0 = selt (op0, op0len, blocks_needed0, small_prec0, l, SIGNED);
  s1 = selt (op1, op1len, blocks_needed1, small_prec1, l, SIGNED);
  if (s0 < s1)
    return -1;
  if (s0 > s1)
    return 1;

  l--;
  while (l >= 0)
    {
      u0 = selt (op0, op0len, blocks_needed0, small_prec0, l, SIGNED);
      u1 = selt (op1, op1len, blocks_needed1, small_prec1, l, SIGNED);

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
wide_int_ro::ltu_p_large (const HOST_WIDE_INT *op0, unsigned int op0len, unsigned int p0,
			  const HOST_WIDE_INT *op1, unsigned int op1len, unsigned int p1)
{
  unsigned HOST_WIDE_INT x0;
  unsigned HOST_WIDE_INT x1;
  unsigned int blocks_needed0 = BLOCKS_NEEDED (p0);
  unsigned int blocks_needed1 = BLOCKS_NEEDED (p1);
  unsigned int small_prec0 = p0 & (HOST_BITS_PER_WIDE_INT - 1);
  unsigned int small_prec1 = p1 & (HOST_BITS_PER_WIDE_INT - 1);
  int l = MAX (op0len - 1, op1len - 1);

  while (l >= 0)
    {
      x0 = selt (op0, op0len, blocks_needed0, small_prec0, l, UNSIGNED);
      x1 = selt (op1, op1len, blocks_needed1, small_prec1, l, UNSIGNED);
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
wide_int_ro::cmpu_large (const HOST_WIDE_INT *op0, unsigned int op0len, unsigned int p0,
			 const HOST_WIDE_INT *op1, unsigned int op1len, unsigned int p1)
{
  unsigned HOST_WIDE_INT x0;
  unsigned HOST_WIDE_INT x1;
  unsigned int blocks_needed0 = BLOCKS_NEEDED (p0);
  unsigned int blocks_needed1 = BLOCKS_NEEDED (p1);
  unsigned int small_prec0 = p0 & (HOST_BITS_PER_WIDE_INT - 1);
  unsigned int small_prec1 = p1 & (HOST_BITS_PER_WIDE_INT - 1);
  int l = MAX (op0len - 1, op1len - 1);

  while (l >= 0)
    {
      x0 = selt (op0, op0len, blocks_needed0, small_prec0, l, UNSIGNED);
      x1 = selt (op1, op1len, blocks_needed1, small_prec1, l, UNSIGNED);
      if (x0 < x1)
	return -1;
      if (x0 > x1)
	return 1;
      l--;
    }

  return 0;
}

/* Return true if THIS has the sign bit set to 1 and all other bits are
   zero.  */
bool
wide_int_ro::only_sign_bit_p (unsigned int prec) const
{
  int i;
  HOST_WIDE_INT x;
  int small_prec;
  bool result;

  if (BLOCKS_NEEDED (prec) != len)
    {
      result = false;
      goto ex;
    }

  for (i=0; i < len - 1; i++)
    if (val[i] != 0)
      {
	result = false;
	goto ex;
      }

  x = val[len - 1];
  small_prec = prec & (HOST_BITS_PER_WIDE_INT - 1);
  if (small_prec)
    x = x << (HOST_BITS_PER_WIDE_INT - small_prec);

  result = x == ((HOST_WIDE_INT)1) << (HOST_BITS_PER_WIDE_INT - 1);

 ex:

#ifdef DEBUG_WIDE_INT
  debug_vw ("wide_int:: %d = only_sign_bit_p (%s)\n", result, *this);
#endif
  return result;
}

/* Returns true if THIS fits into range of TYPE.  Signedness of OP0 is
   assumed to be the same as the signedness of TYPE.  */
bool
wide_int_ro::fits_to_tree_p (const_tree type) const
{
  unsigned int type_prec = TYPE_PRECISION (type);

  if (precision <= type_prec)
    return true;

  if (TYPE_SIGN (type) == UNSIGNED)
    return *this == zext (type_prec);
  else
    {
      /* For signed, we can do a couple of quick tests since the
	 compressed rep looks like it was just sign extended.  */
      if (len < BLOCKS_NEEDED (type_prec))
	return true;

      if (len > BLOCKS_NEEDED (type_prec))
	return false;

      return *this == sext (type_prec);
    }
}

/*
 * Extension.
 */

/* Sign extend THIS starting at OFFSET.  The precision of the result
   are the same as THIS.  */
wide_int_ro
wide_int_ro::sext (unsigned int offset) const
{
  wide_int result;
  int off;

  gcc_assert (precision >= offset);

  if (precision <= HOST_BITS_PER_WIDE_INT)
    {
      result.precision = precision;
      if (offset < precision)
	result.val[0] = sext_hwi (val[0], offset);
      else
	/* If offset is greater or equal to precision there is nothing
	   to do since the internal rep is already sign extended.  */
	result.val[0] = val[0];

      result.len = 1;
    }
  else if (precision == offset)
    result = *this;
  else
    {
      result = decompress (offset, precision);

      /* Now we can do the real sign extension.  */
      off = offset & (HOST_BITS_PER_WIDE_INT - 1);
      if (off)
	{
	  int block = BLOCK_OF (offset);
	  result.val[block] = sext_hwi (val[block], off);
	  result.len = block + 1;
	}
      /* We never need an extra element for sign extended values but
	 we may need to compress.  */
      result.canonize ();
    }

#ifdef DEBUG_WIDE_INT
  debug_wwv ("wide_int:: %s = (%s sext %d)\n", result, *this, offset);
#endif

  return result;
}

/* Zero extend THIS starting at OFFSET.  The precision of the result
   are the same as THIS.  */
wide_int_ro
wide_int_ro::zext (unsigned int offset) const
{
  wide_int result;
  int off;
  int block;

  gcc_assert (precision >= offset);

  if (precision <= HOST_BITS_PER_WIDE_INT)
    {
      result.precision = precision;
      if (offset < precision)
	result.val[0] = zext_hwi (val[0], offset);
      else if (offset == precision)
	result.val[0] = val[0];
	/* If offset was greater than the precision we need to zero
	   extend from the old precision since the internal rep was
	   equivalent to sign extended.  */
      else
	result.val[0] = zext_hwi (val[0], precision);
	
      result.len = 1;
    }
  else if (precision == offset)
    result = *this;
  else
    {
      result = decompress (offset, precision);

      /* Now we can do the real zero extension.  */
      off = offset & (HOST_BITS_PER_WIDE_INT - 1);
      block = BLOCK_OF (offset);
      if (off)
	{
	  result.val[block] = zext_hwi (val[block], off);
	  result.len = block + 1;
	}
      else
	/* See if we need an extra zero element to satisfy the
	   compression rule.  */
	if (result.val[block - 1] < 0 && offset < precision)
	  {
	    result.val[block] = 0;
	    result.len += 1;
	  }
      result.canonize ();
    }
#ifdef DEBUG_WIDE_INT
  debug_wwv ("wide_int:: %s = (%s zext %d)\n", result, *this, offset);
#endif
  return result;
}

/*
 * Masking, inserting, shifting, rotating.
 */

/* Return a value with a one bit inserted in THIS at BITPOS.  */
wide_int_ro
wide_int_ro::set_bit (unsigned int bitpos) const
{
  wide_int result;
  int i, j;

  if (bitpos >= precision)
    result = *this;
  else
    {
      result = decompress (bitpos, precision);
      j = bitpos / HOST_BITS_PER_WIDE_INT;
      i = bitpos & (HOST_BITS_PER_WIDE_INT - 1);
      result.val[j] |= ((HOST_WIDE_INT)1) << i;
    }

#ifdef DEBUG_WIDE_INT
  debug_wwv ("wide_int_ro:: %s = (%s set_bit %d)\n", result, *this, bitpos);
#endif
  return result;
}

/* Insert a 1 bit into 0 at BITPOS producing an number with PREC.  */
wide_int_ro
wide_int_ro::set_bit_in_zero (unsigned int bitpos, unsigned int prec)
{
  wide_int result;
  int extra_bit = 0;
  /* We need one extra bit of 0 above the set bit for the compression
     of the bits above the set bit when the bit that is set is the top
     bit of a compressed number.  When setting the actual top bit
     (non-compressed) we can just set it as there are no bits above
     it.  */
  if (bitpos % HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_WIDE_INT-1
      && bitpos+1 != prec)
    extra_bit = 1;
  int blocks_needed = BLOCKS_NEEDED (bitpos + 1 + extra_bit);
  int i, j;

  result.precision = prec;
  if (bitpos >= prec)
    {
      result.len = 1;
      result.val[0] = 0;
    }
  else
    {
      result.len = blocks_needed;
      for (i = 0; i < blocks_needed; i++)
	result.val[i] = 0;

      j = bitpos / HOST_BITS_PER_WIDE_INT;
      i = bitpos & (HOST_BITS_PER_WIDE_INT - 1);
      result.val[j] |= ((HOST_WIDE_INT)1) << i;
    }

#ifdef DEBUG_WIDE_INT
  debug_wv ("wide_int_ro:: %s = set_bit_in_zero (%d)\n", result, bitpos);
#endif

  return result;
}

/* Insert WIDTH bits from OP0 into THIS starting at START.  */
wide_int_ro
wide_int_ro::insert (const wide_int_ro &op0, unsigned int start,
		     unsigned int width) const
{
  wide_int result;
  wide_int mask;
  wide_int tmp;

  if (start >= precision)
    return *this;

  gcc_checking_assert (op0.precision >= width);

  if (start + width >= precision)
    width = precision - start;

  mask = shifted_mask (start, width, false, precision);
  tmp = op0.lshift_widen (start, precision);
  result = tmp & mask;

  tmp = and_not (mask);
  result = result | tmp;

#ifdef DEBUG_WIDE_INT
  debug_wwwvv ("wide_int_ro:: %s = (%s insert %s start = %d width = %d)\n",
	       result, *this, op0, start, width);
#endif

  return result;
}

/* bswap THIS.  */
wide_int_ro
wide_int_ro::bswap () const
{
  wide_int result;
  int i, s;
  int end;
  int len = BLOCKS_NEEDED (precision);

  /* This is not a well defined operation if the precision is not a
     multiple of 8.  */
  gcc_assert ((precision & 0x7) == 0);

  result.precision = precision;
  result.len = len;

  for (i = 0; i < len; i++)
    result.val[i] = 0;

  /* Only swap the bytes that are not the padding.  */
  if ((precision & (HOST_BITS_PER_WIDE_INT - 1))
      && (this->len == len))
    end = precision;
  else
    end = this->len * HOST_BITS_PER_WIDE_INT;

  for (s = 0; s < end; s += 8)
    {
      unsigned int d = precision - s - 8;
      unsigned HOST_WIDE_INT byte;

      int block = s / HOST_BITS_PER_WIDE_INT;
      int offset = s & (HOST_BITS_PER_WIDE_INT - 1);

      byte = (val[block] >> offset) & 0xff;

      block = d / HOST_BITS_PER_WIDE_INT;
      offset = d & (HOST_BITS_PER_WIDE_INT - 1);

      result.val[block] |= byte << offset;
    }

  result.canonize ();

#ifdef DEBUG_WIDE_INT
  debug_ww ("wide_int_ro:: %s = bswap (%s)\n", result, *this);
#endif
  return result;
}

/* Return a result mask where the lower WIDTH bits are ones and the
   bits above that up to the precision are zeros.  The result is
   inverted if NEGATE is true.  The result is made with PREC. */
wide_int_ro
wide_int_ro::mask (unsigned int width, bool negate, unsigned int prec)
{
  wide_int result;
  unsigned int i = 0;
  int shift;

  gcc_assert (width < 4 * MAX_BITSIZE_MODE_ANY_INT);
  gcc_assert (prec <= 4 * MAX_BITSIZE_MODE_ANY_INT);

  if (width == prec)
    {
      if (negate)
	result = wide_int::zero (prec);
      else
	result = wide_int::minus_one (prec);
    }
  else if (width == 0)
    {
      if (negate)
	result = wide_int::minus_one (prec);
      else
	result = wide_int::zero (prec);
    }
  else
    {
      result.precision = prec;

      while (i < width / HOST_BITS_PER_WIDE_INT)
	result.val[i++] = negate ? 0 : (HOST_WIDE_INT)-1;

      shift = width & (HOST_BITS_PER_WIDE_INT - 1);
      if (shift != 0)
	{
	  HOST_WIDE_INT last = (((HOST_WIDE_INT)1) << shift) - 1;
	  result.val[i++] = negate ? ~last : last;
	}
      else
	result.val[i++] = negate ? (HOST_WIDE_INT)-1 : 0;
      result.len = i;
    }

#ifdef DEBUG_WIDE_INT
  debug_wvv ("wide_int_ro:: %s = mask (%d, negate = %d)\n", result, width, negate);
#endif
  return result;
}

/* Return a result mask of WIDTH ones starting at START and the
   bits above that up to the precision are zeros.  The result is
   inverted if NEGATE is true.  */
wide_int_ro
wide_int_ro::shifted_mask (unsigned int start, unsigned int width,
			   bool negate, unsigned int prec)
{
  wide_int result;
  unsigned int i = 0;
  unsigned int shift;
  unsigned int end = start + width;
  HOST_WIDE_INT block;

  gcc_assert (start < 4 * MAX_BITSIZE_MODE_ANY_INT);

  if (start + width > prec)
    width = prec - start;

  if (width == 0)
    {
      if (negate)
	result = wide_int::minus_one (prec);
      else
	result = wide_int::zero (prec);
#ifdef DEBUG_WIDE_INT
      debug_wvvv
	("wide_int:: %s = shifted_mask (start = %d width = %d negate = %d)\n",
	 result, start, width, negate);
#endif
      return result;
    }

  result.precision = prec;

  while (i < start / HOST_BITS_PER_WIDE_INT)
    result.val[i++] = negate ? (HOST_WIDE_INT)-1 : 0;

  shift = start & (HOST_BITS_PER_WIDE_INT - 1);
  if (shift)
    {
      block = (((HOST_WIDE_INT)1) << shift) - 1;
      shift = (end) & (HOST_BITS_PER_WIDE_INT - 1);
      if (shift)
	{
	  /* case 000111000 */
	  block = (((HOST_WIDE_INT)1) << shift) - block - 1;
	  result.val[i++] = negate ? ~block : block;
	  result.len = i;

#ifdef DEBUG_WIDE_INT
	  debug_wvvv
	    ("wide_int_ro:: %s = shifted_mask (start = %d width = %d negate = %d)\n",
	     result, start, width, negate);
#endif
	  return result;
	}
      else
	/* ...111000 */
	result.val[i++] = negate ? block : ~block;
    }

  while (i < end / HOST_BITS_PER_WIDE_INT)
    /* 1111111 */
    result.val[i++] = negate ? 0 : (HOST_WIDE_INT)-1;

  shift = end & (HOST_BITS_PER_WIDE_INT - 1);
  if (shift != 0)
    {
      /* 000011111 */
      block = (((HOST_WIDE_INT)1) << shift) - 1;
      result.val[i++] = negate ? ~block : block;
    }
  else if (end < prec)
    result.val[i++] = negate ? (HOST_WIDE_INT)-1 : 0;

  result.len = i;

#ifdef DEBUG_WIDE_INT
  debug_wvvv
    ("wide_int_ro:: %s = shifted_mask (start = %d width = %d negate = %d)\n",
     result, start, width, negate);
#endif

  return result;
}

/* Ensure there are no undefined bits returned by elt ().  This is
   useful for when we might hash the value returned by elt and want to
   ensure the top undefined bit are in fact, defined.  If sgn is
   UNSIGNED, the bits are zeroed, if sgn is SIGNED, then the bits are
   copies of the top bit (aka sign bit) as determined by
   precision.  */
void
wide_int_ro::clear_undef (signop sgn)
{
  int small_prec = precision % HOST_BITS_PER_WIDE_INT;
  if (small_prec)
    {
      if (len == (precision + HOST_BITS_PER_WIDE_INT - 1) / HOST_BITS_PER_WIDE_INT)
	{
	  if (sgn == UNSIGNED)
	    val[len-1] &= ((unsigned HOST_WIDE_INT)1 << small_prec) - 1;
	  else
	    {
	      int cnt = HOST_BITS_PER_WIDE_INT - small_prec;
	      val[len-1] = (val[len-1] << cnt) >> cnt;
	    }
	}
    }
  /* Do we have a int:0 inside a struct?  */
  else if (precision == 0)
    val[0] = 0;
}


/*
 * logical operations.
 */

/* Return THIS & OP1.  */
wide_int_ro
wide_int_ro::and_large (const HOST_WIDE_INT *op0, unsigned int op0len,
			unsigned int prec,
			const HOST_WIDE_INT *op1, unsigned int op1len)
{
  wide_int result;
  int l0 = op0len - 1;
  int l1 = op1len - 1;
  bool need_canon = true;

  result.len = MAX (op0len, op1len);
  result.precision = prec;

  if (l0 > l1)
    {
      HOST_WIDE_INT op1mask = -top_bit_of (op1, op1len, prec);
      if (op1mask  == 0)
	{
	  l0 = l1;
	  result.len = l1 + 1;
	}
      else
	{
	  need_canon = false;
	  while (l0 > l1)
	    {
	      result.val[l0] = op0[l0];
	      l0--;
	    }
	}
    }
  else if (l1 > l0)
    {
      HOST_WIDE_INT op0mask = -top_bit_of (op0, op0len, prec);
      if (op0mask == 0)
	result.len = l0 + 1;
      else
	{
	  need_canon = false;
	  while (l1 > l0)
	    {
	      result.val[l1] = op1[l1];
	      l1--;
	    }
	}
    }

  while (l0 >= 0)
    {
      result.val[l0] = op0[l0] & op1[l0];
      l0--;
    }

  if (need_canon)
    result.canonize ();

  return result;
}

/* Return THIS & ~OP1.  */
wide_int_ro
wide_int_ro::and_not_large (const HOST_WIDE_INT *op0, unsigned int op0len,
			    unsigned int prec,
			    const HOST_WIDE_INT *op1, unsigned int op1len)
{
  wide_int result;
  int l0 = op0len - 1;
  int l1 = op1len - 1;
  bool need_canon = true;

  result.len = MAX (op0len, op1len);
  result.precision = prec;

  if (l0 > l1)
    {
      HOST_WIDE_INT op1mask = -top_bit_of (op1, op1len, prec);
      if (op1mask != 0)
	{
	  l0 = l1;
	  result.len = l1 + 1;
	}
      else
	{
	  need_canon = false;
	  while (l0 > l1)
	    {
	      result.val[l0] = op0[l0];
	      l0--;
	    }
	}
    }
  else if (l1 > l0)
    {
      HOST_WIDE_INT op0mask = -top_bit_of (op0, op0len, prec);
      if (op0mask == 0)
	result.len = l0 + 1;
      else
	{
	  need_canon = false;
	  while (l1 > l0)
	    {
	      result.val[l1] = ~op1[l1];
	      l1--;
	    }
	}
    }

  while (l0 >= 0)
    {
      result.val[l0] = op0[l0] & ~op1[l0];
      l0--;
    }

  if (need_canon)
    result.canonize ();

  return result;
}

/* Return THIS | OP1.  */
wide_int_ro
wide_int_ro::or_large (const HOST_WIDE_INT *op0, unsigned int op0len,
		       unsigned int prec,
		       const HOST_WIDE_INT *op1, unsigned int op1len)
{
  wide_int result;
  int l0 = op0len - 1;
  int l1 = op1len - 1;
  bool need_canon = true;

  result.len = MAX (op0len, op1len);
  result.precision = prec;

  if (l0 > l1)
    {
      HOST_WIDE_INT op1mask = -top_bit_of (op1, op1len, prec);
      if (op1mask != 0)
	{
	  l0 = l1;
	  result.len = l1 + 1;
	}
      else
	{
	  need_canon = false;
	  while (l0 > l1)
	    {
	      result.val[l0] = op0[l0];
	      l0--;
	    }
	}
    }
  else if (l1 > l0)
    {
      HOST_WIDE_INT op0mask = -top_bit_of (op0, op0len, prec);
      if (op0mask != 0)
	result.len = l0 + 1;
      else
	{
	  need_canon = false;
	  while (l1 > l0)
	    {
	      result.val[l1] = op1[l1];
	      l1--;
	    }
	}
    }

  while (l0 >= 0)
    {
      result.val[l0] = op0[l0] | op1[l0];
      l0--;
    }

  if (need_canon)
    result.canonize ();

  return result;
}

/* Return THIS | ~OP1.  */
wide_int_ro
wide_int_ro::or_not_large (const HOST_WIDE_INT *op0, unsigned int op0len,
			   unsigned int prec,
			   const HOST_WIDE_INT *op1, unsigned int op1len)
{
  wide_int result;
  int l0 = op0len - 1;
  int l1 = op1len - 1;
  bool need_canon = true;

  result.len = MAX (op0len, op1len);
  result.precision = prec;

  if (l0 > l1)
    {
      HOST_WIDE_INT op1mask = -top_bit_of (op1, op1len, prec);
      if (op1mask == 0)
	{
	  l0 = l1;
	  result.len = l1 + 1;
	}
      else
	{
	  need_canon = false;
	  while (l0 > l1)
	    {
	      result.val[l0] = op0[l0];
	      l0--;
	    }
	}
    }
  else if (l1 > l0)
    {
      HOST_WIDE_INT op0mask = -top_bit_of (op0, op0len, prec);
      if (op0mask != 0)
	result.len = l0 + 1;
      else
	{
	  need_canon = false;
	  while (l1 > l0)
	    {
	      result.val[l1] = ~op1[l1];
	      l1--;
	    }
	}
    }

  while (l0 >= 0)
    {
      result.val[l0] = op0[l0] | ~op1[l0];
      l0--;
    }

  if (need_canon)
    result.canonize ();

  return result;
}

/* Return the exclusive ior (xor) of THIS and OP1.  */
wide_int_ro
wide_int_ro::xor_large (const HOST_WIDE_INT *op0, unsigned int op0len,
			unsigned int prec,
			const HOST_WIDE_INT *op1, unsigned int op1len)
{
  wide_int result;
  int l0 = op0len - 1;
  int l1 = op1len - 1;

  result.len = MAX (op0len, op1len);
  result.precision = prec;

  if (l0 > l1)
    {
      HOST_WIDE_INT op1mask = -top_bit_of (op1, op1len, prec);
      while (l0 > l1)
	{
	  result.val[l0] = op0[l0] ^ op1mask;
	  l0--;
	}
    }

  if (l1 > l0)
    {
      HOST_WIDE_INT op0mask = -top_bit_of (op0, op0len, prec);
      while (l1 > l0)
	{
	  result.val[l1] = op0mask ^ op1[l1];
	  l1--;
	}
    }

  while (l0 >= 0)
    {
      result.val[l0] = op0[l0] ^ op1[l0];
      l0--;
    }

  result.canonize ();

#ifdef DEBUG_WIDE_INT
  debug_waa ("wide_int_ro:: %s = (%s ^ %s)\n",
	     result, op0, op0len, prec, op1, op1len, prec);
#endif
  return result;
}

/*
 * math
 */

/* Absolute value of THIS.  */
wide_int_ro
wide_int_ro::abs () const
{
  wide_int result;
  gcc_checking_assert (precision);

  if (sign_mask ())
    result = neg ();
  else
    result = *this;

#ifdef DEBUG_WIDE_INT
  debug_ww ("wide_int_ro:: %s = abs (%s)\n", result, *this);
#endif
  return result;
}

/* Add of THIS and OP1.  No overflow is detected.  */
wide_int_ro
wide_int_ro::add_large (const HOST_WIDE_INT *op0, unsigned int op0len,
			unsigned int prec,
			const HOST_WIDE_INT *op1, unsigned int op1len,
			signop sgn, bool *overflow)
{
  wide_int result;
  unsigned HOST_WIDE_INT o0 = 0;
  unsigned HOST_WIDE_INT o1 = 0;
  unsigned HOST_WIDE_INT x = 0;
  unsigned HOST_WIDE_INT carry = 0;
  unsigned HOST_WIDE_INT old_carry = 0;
  unsigned HOST_WIDE_INT mask0, mask1;
  unsigned int i, small_prec;

  result.precision = prec;
  result.len = MAX (op0len, op1len);
  mask0 = -top_bit_of (op0, op0len, prec);
  mask1 = -top_bit_of (op1, op1len, prec);
  /* Add all of the explicitly defined elements.  */

  for (i = 0; i < result.len; i++)
    {
      o0 = i < op0len ? (unsigned HOST_WIDE_INT)op0[i] : mask0;
      o1 = i < op1len ? (unsigned HOST_WIDE_INT)op1[i] : mask1;
      x = o0 + o1 + carry;
      result.val[i] = x;
      old_carry = carry;
      carry = carry == 0 ? x < o0 : x <= o0;
    }

  if (result.len * HOST_BITS_PER_WIDE_INT < prec)
    {
      result.val[result.len] = mask0 + mask1 + carry;
      result.len++;
      if (overflow)
	*overflow = false;
    }
  else if (overflow)
    {
      if (sgn == SIGNED)
	{
	  int p = (result.len == BLOCKS_NEEDED (prec)
		   ? HOST_BITS_PER_WIDE_INT
		   : prec & (HOST_BITS_PER_WIDE_INT - 1) ) - 1;
	  HOST_WIDE_INT x = (result.val[result.len - 1] ^ o0)
	    & (result.val[result.len - 1] ^ o1);
	  x = (x >> p) & 1;
	  *overflow = (x != 0);
	}
      else
	{
	  if (old_carry)
	    *overflow = ((unsigned HOST_WIDE_INT)result.val[result.len - 1] <= o0);
	  else
	    *overflow = ((unsigned HOST_WIDE_INT)result.val[result.len - 1] < o0);
	}
    }

  small_prec = prec & (HOST_BITS_PER_WIDE_INT - 1);
  if (small_prec != 0 && BLOCKS_NEEDED (prec) == result.len)
    {
      /* Modes with weird precisions.  */
      i = result.len - 1;
      result.val[i] = sext_hwi (result.val[i], small_prec);
    }

  result.canonize ();

  return result;
}


/* Count leading zeros of THIS but only looking at the bits in the
   smallest HWI of size mode.  */
wide_int_ro
wide_int_ro::clz () const
{
  int i;
  int start;
  int count;
  HOST_WIDE_INT v;
  int small_prec = precision & (HOST_BITS_PER_WIDE_INT - 1);

  gcc_checking_assert (precision);

  if (zero_p ())
    {
      enum machine_mode mode = mode_for_size (precision, MODE_INT, 0);
      if (mode == BLKmode)
	mode_for_size (precision, MODE_PARTIAL_INT, 0);

      /* Even if the value at zero is undefined, we have to come up
	 with some replacement.  Seems good enough.  */
      if (mode == BLKmode)
	count = precision;
      else if (!CLZ_DEFINED_VALUE_AT_ZERO (mode, count))
	count = precision;
    }
  else if (neg_p (SIGNED))
    count = 0;
  else
    {
      /* The high order block is special if it is the last block and the
	 precision is not an even multiple of HOST_BITS_PER_WIDE_INT.  We
	 have to clear out any ones above the precision before doing clz
	 on this block.  */
      if (BLOCKS_NEEDED (precision) == len && small_prec)
	{
	  v = zext_hwi (val[len - 1], small_prec);
	  count = clz_hwi (v) - (HOST_BITS_PER_WIDE_INT - small_prec);
	  start = len - 2;
	  if (v != 0)
	    {
#ifdef DEBUG_WIDE_INT
	      debug_vw ("wide_int:: %d = clz (%s)\n", count, *this);
#endif
	      return from_shwi (count, precision);
	    }
	}
      else
	{
	  count = HOST_BITS_PER_WIDE_INT * (BLOCKS_NEEDED (precision) - len);
	  start = len - 1;
	}

      for (i = start; i >= 0; i--)
	{
	  v = elt (i);
	  count += clz_hwi (v);
	  if (v != 0)
	    break;
	}

    }

#ifdef DEBUG_WIDE_INT
  debug_vw ("wide_int_ro:: %d = clz (%s)\n", count, *this);
#endif
  return from_shwi (count, precision);
}

/* Count the number of redundant leading bits of THIS.  Return result
   as a HOST_WIDE_INT.  */
wide_int_ro
wide_int_ro::clrsb () const
{
  gcc_checking_assert (precision);

  if (neg_p (SIGNED))
    return operator ~ ().clz () - 1;

  return clz () - 1;
}

/* Count zeros of THIS.   */
wide_int_ro
wide_int_ro::ctz () const
{
  int i;
  unsigned int count = 0;
  HOST_WIDE_INT v;
  int small_prec = precision & (HOST_BITS_PER_WIDE_INT - 1);
  int end;
  bool more_to_do;

  gcc_checking_assert (precision);

  if (zero_p ())
    {
      enum machine_mode mode = mode_for_size (precision, MODE_INT, 0);
      if (mode == BLKmode)
	mode_for_size (precision, MODE_PARTIAL_INT, 0);

      /* Even if the value at zero is undefined, we have to come up
	 with some replacement.  Seems good enough.  */
      if (mode == BLKmode)
	count = precision;
      else if (!CTZ_DEFINED_VALUE_AT_ZERO (mode, count))
	count = precision;
    }
  else
    {
      /* The high order block is special if it is the last block and the
	 precision is not an even multiple of HOST_BITS_PER_WIDE_INT.  We
	 have to clear out any ones above the precision before doing clz
	 on this block.  */
      if (BLOCKS_NEEDED (precision) == len && small_prec)
	{
	  end = len - 1;
	  more_to_do = true;
	}
      else
	{
	  end = len;
	  more_to_do = false;
	}

      for (i = 0; i < end; i++)
	{
	  v = val[i];
	  count += ctz_hwi (v);
	  if (v != 0)
	    {
#ifdef DEBUG_WIDE_INT
	      debug_vw ("wide_int_ro:: %d = ctz (%s)\n", count, *this);
#endif
	      return wide_int_ro::from_shwi (count, precision);
	    }
	}

      if (more_to_do)
	{
	  v = zext_hwi (val[len - 1], small_prec);
	  count = ctz_hwi (v);
	  /* The top word was all zeros so we have to cut it back to prec,
	     because we are counting some of the zeros above the
	     interesting part.  */
	  if (count > precision)
	    count = precision;
	}
      else
	/* Skip over the blocks that are not represented.  They must be
	   all zeros at this point.  */
	count = precision;
    }

#ifdef DEBUG_WIDE_INT
  debug_vw ("wide_int_ro:: %d = ctz (%s)\n", count, *this);
#endif
  return wide_int_ro::from_shwi (count, precision);
}

/* ffs of THIS.  */
wide_int_ro
wide_int_ro::ffs () const
{
  HOST_WIDE_INT count = ctz ().to_shwi ();

  if (count == precision)
    count = 0;
  else
    count += 1;

#ifdef DEBUG_WIDE_INT
  debug_vw ("wide_int_ro:: %d = ffs (%s)\n", count, *this);
#endif
  return wide_int_ro::from_shwi (count, precision);
}

/* Subroutines of the multiplication and division operations.  Unpack
   the first IN_LEN HOST_WIDE_INTs in INPUT into 2 * IN_LEN
   HOST_HALF_WIDE_INTs of RESULT.  The rest of RESULT is filled by
   uncompressing the top bit of INPUT[IN_LEN - 1].  */
static void
wi_unpack (unsigned HOST_HALF_WIDE_INT *result,
	   const unsigned HOST_WIDE_INT *input,
	   int in_len, int out_len, unsigned int prec, signop sgn)
{
  int i;
  int j = 0;
  int small_prec = prec & (HOST_BITS_PER_WIDE_INT - 1);
  int blocks_needed = BLOCKS_NEEDED (prec);
  HOST_WIDE_INT mask;

  if (sgn == SIGNED)
    {
      mask = -top_bit_of ((const HOST_WIDE_INT *) input, in_len, prec);
      mask &= HALF_INT_MASK;
    }
  else
    mask = 0;

  for (i = 0; i < in_len; i++)
    {
      HOST_WIDE_INT x = input[i];
      if (i == blocks_needed - 1 && small_prec)
	{
	  if (sgn == SIGNED)
	    x = sext_hwi (x, small_prec);
	  else
	    x = zext_hwi (x, small_prec);
	}
      result[j++] = x;
      result[j++] = x >> HOST_BITS_PER_HALF_WIDE_INT;
    }

  /* Smear the sign bit.  */
  while (j < out_len)
    result[j++] = mask;
}

/* The inverse of wi_unpack.  IN_LEN is the the number of input
   blocks.  The number of output blocks will be half this amount.  */
static void
wi_pack (unsigned HOST_WIDE_INT *result,
	 const unsigned HOST_HALF_WIDE_INT *input,
	 int in_len)
{
  int i = 0;
  int j = 0;

  while (i < in_len - 2)
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

/* Return an integer that is the exact log2 of THIS.  */
wide_int_ro
wide_int_ro::exact_log2 () const
{
  int small_prec = precision & (HOST_BITS_PER_WIDE_INT - 1);
  wide_int count;
  wide_int result;

  gcc_checking_assert (precision);
  if (precision <= HOST_BITS_PER_WIDE_INT)
    {
      HOST_WIDE_INT v;
      if (small_prec)
	v = zext_hwi (val[0], small_prec);
      else
	v = val[0];
      result = wide_int_ro::from_shwi (::exact_log2 (v), precision);
    }
  else
    {
      count = ctz ();
      if (clz () + count + 1 == precision)
	result = count;
      else
	result = wide_int_ro::from_shwi (-1, precision);
    }

#ifdef DEBUG_WIDE_INT
  debug_ww ("wide_int_ro:: %s = exact_log2 (%s)\n", result, *this);
#endif
  return result;
}

/* Return an integer that is the floor log2 of THIS.  */
wide_int_ro
wide_int_ro::floor_log2 () const
{
  int small_prec = precision & (HOST_BITS_PER_WIDE_INT - 1);
  wide_int result;

  gcc_checking_assert (precision);
  if (precision <= HOST_BITS_PER_WIDE_INT)
    {
      HOST_WIDE_INT v;
      if (small_prec)
	v = zext_hwi (val[0], small_prec);
      else
	v = val[0];
      result = wide_int_ro::from_shwi (::floor_log2 (v), precision);
    }
  else
    result = wide_int_ro::from_shwi (precision, precision) - 1 - clz ();

#ifdef DEBUG_WIDE_INT
  debug_ww ("wide_int_ro:: %s = floor_log2 (%s)\n", result, *this);
#endif
  return result;
}

/* Multiply Op1 by Op2.  If HIGH is set, only the upper half of the
   result is returned.  If FULL is set, the entire result is returned
   in a mode that is twice the width of the inputs.  However, that
   mode needs to exist if the value is to be usable.  Clients that use
   FULL need to check for this.

   If HIGH or FULL are not set, throw away the upper half after the check
   is made to see if it overflows.  Unfortunately there is no better
   way to check for overflow than to do this.  OVERFLOW is assumed to
   be sticky so it should be initialized.  SGN controls the signedness
   and is used to check overflow or if HIGH or FULL is set.  */
wide_int_ro
wide_int_ro::mul_internal (bool high, bool full,
			   const HOST_WIDE_INT *op1, unsigned int op1len,
			   unsigned int prec,
			   const HOST_WIDE_INT *op2, unsigned int op2len,
			   signop sgn,  bool *overflow,
			   bool needs_overflow)
{
  wide_int result;
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
  if (overflow == 0)
    needs_overflow = false;
  result.precision = prec;

  /* If we need to check for overflow, we can only do half wide
     multiplies quickly because we need to look at the top bits to
     check for the overflow.  */
  if ((high || full || needs_overflow)
      && (prec <= HOST_BITS_PER_HALF_WIDE_INT))
    {
      HOST_WIDE_INT r;
      result.len = 1;

      if (sgn == SIGNED)
	{
	  o0 = sext_hwi (op1[0], prec);
	  o1 = sext_hwi (op2[0], prec);
	}
      else
	{
	  o0 = zext_hwi (op1[0], prec);
	  o1 = zext_hwi (op2[0], prec);
	}

      r = o0 * o1;
      if (needs_overflow)
	{
	  HOST_WIDE_INT upper;
	  HOST_WIDE_INT sm
	    = (r << (HOST_BITS_PER_WIDE_INT - prec))
	    >> (HOST_BITS_PER_WIDE_INT - 1);
	  mask = ((HOST_WIDE_INT)1 << prec) - 1;
	  sm &= mask;
	  upper = (r >> prec) & mask;

	  if (sgn == SIGNED)
	    {
	      if (sm != upper)
		*overflow = true;
	    }
	  else
	    if (upper != 0)
	      *overflow = true;
	}
      if (full)
	{
	  result.val[0] = sext_hwi (r, prec * 2);
	  result.precision = prec * 2;
	}
      else if (high)
	result.val[0] = r >> prec;
      else
	result.val[0] = sext_hwi (r, prec);
#ifdef DEBUG_WIDE_INT
      debug_wvasa ("wide_int_ro:: %s O=%d = (%s *%s %s)\n",
		   result, overflow ? *overflow : 0, op1, op1len, prec,
		   sgn==UNSIGNED ? "U" : "S", op2, op2len, prec);
#endif
      return result;
    }

  /* We do unsigned mul and then correct it.  */
  wi_unpack (u, (const unsigned HOST_WIDE_INT*)op1, op1len,
	     half_blocks_needed, prec, SIGNED);
  wi_unpack (v, (const unsigned HOST_WIDE_INT*)op2, op2len,
	     half_blocks_needed, prec, SIGNED);

  /* The 2 is for a full mult.  */
  memset (r, 0, half_blocks_needed * 2
	  * HOST_BITS_PER_HALF_WIDE_INT / BITS_PER_UNIT);

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
  if (sgn == SIGNED && (full || high || needs_overflow))
    {
      unsigned HOST_WIDE_INT b;
      if (op1[op1len-1] < 0)
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
      if (op2[op2len-1] < 0)
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

  if (full)
    {
      /* compute [2prec] <- [prec] * [prec] */
      wi_pack ((unsigned HOST_WIDE_INT*)result.val, r, 2 * half_blocks_needed);
      result.len = blocks_needed * 2;
      result.precision = prec * 2;
    }
  else if (high)
    {
      /* compute [prec] <- ([prec] * [prec]) >> [prec] */
      wi_pack ((unsigned HOST_WIDE_INT*)&result.val [blocks_needed >> 1],
	       r, half_blocks_needed);
      result.len = blocks_needed;
    }
  else
    {
      /* compute [prec] <- ([prec] * [prec]) && ((1 << [prec]) - 1) */
      wi_pack ((unsigned HOST_WIDE_INT*)result.val, r, half_blocks_needed);
      result.len = blocks_needed;
    }

  result.canonize ();

#ifdef DEBUG_WIDE_INT
  debug_wvasa ("wide_int_ro:: %s O=%d = (%s *%s %s)\n",
	       result, overflow ? *overflow : 0, op1, op1len, prec,
	       sgn==UNSIGNED ? "U" : "S", op2, op2len, prec);
#endif
  return result;
}

/* Compute the parity of THIS.  */
wide_int_ro
wide_int_ro::parity () const
{
  wide_int count = popcount ();
  return count & 1;
}

/* Compute the population count of THIS.  */
wide_int_ro
wide_int_ro::popcount () const
{
  int i;
  int start;
  int count;
  HOST_WIDE_INT v;
  int small_prec = precision & (HOST_BITS_PER_WIDE_INT - 1);
  int blocks_needed = BLOCKS_NEEDED (precision);

  gcc_checking_assert (precision);

  /* The high order block is special if it is the last block and the
     precision is not an even multiple of HOST_BITS_PER_WIDE_INT.  We
     have to clear out any ones above the precision before doing
     popcount on this block.  */
  if (small_prec)
    {
      v = zext_hwi (elt (blocks_needed - 1), small_prec);
      count = popcount_hwi (v);

      if (len == blocks_needed)
	start = len - 2;
      else
	{
	  start = len - 1;
	  blocks_needed--;
	}
    }
  else
    {
      start = len - 1;
      count = 0;
    }

  if (sign_mask ())
    count += HOST_BITS_PER_WIDE_INT * (blocks_needed - len);

  for (i = start; i >= 0; i--)
    {
      v = val[i];
      count += popcount_hwi (v);
    }

#ifdef DEBUG_WIDE_INT
  debug_vw ("wide_int_ro:: %d = popcount (%s)\n", count, *this);
#endif
  return wide_int_ro::from_shwi (count, precision);
}

/* Subtract of THIS and OP1.  If the pointer to OVERFLOW is not 0, set
   OVERFLOW if the value overflows.  */
wide_int_ro
wide_int_ro::sub_large (const HOST_WIDE_INT *op0, unsigned int op0len,
			unsigned int prec,
			const HOST_WIDE_INT *op1, unsigned int op1len,
			signop sgn, bool *overflow)
{
  wide_int result;
  unsigned HOST_WIDE_INT o0 = 0;
  unsigned HOST_WIDE_INT o1 = 0;
  unsigned HOST_WIDE_INT x = 0;
  /* We implement subtraction as an in place negate and add.  Negation
     is just inversion and add 1, so we can do the add of 1 by just
     starting the borrow in of the first element at 1.  */
  unsigned HOST_WIDE_INT borrow = 0;
  unsigned HOST_WIDE_INT old_borrow = 0;

  unsigned HOST_WIDE_INT mask0, mask1;
  unsigned int i, small_prec;

  result.precision = prec;
  result.len = MAX (op0len, op1len);
  mask0 = -top_bit_of (op0, op0len, prec);
  mask1 = -top_bit_of (op1, op1len, prec);

  /* Subtract all of the explicitly defined elements.  */
  for (i = 0; i < result.len; i++)
    {
      o0 = i < op0len ? (unsigned HOST_WIDE_INT)op0[i] : mask0;
      o1 = i < op1len ? (unsigned HOST_WIDE_INT)op1[i] : mask1;
      x = o0 - o1 - borrow;
      result.val[i] = x;
      old_borrow = borrow;
      borrow = borrow == 0 ? o0 < o1 : o0 <= o1;
    }

  if (result.len * HOST_BITS_PER_WIDE_INT < prec)
    {
      result.val[result.len] = mask0 - mask1 - borrow;
      result.len++;
      if (overflow)
	*overflow = false;
    }
  else if (overflow)
    {
      if (sgn == SIGNED)
	{
	  int p = (result.len == BLOCKS_NEEDED (prec)
		   ? HOST_BITS_PER_WIDE_INT
		   : prec & (HOST_BITS_PER_WIDE_INT - 1) ) - 1;
	  HOST_WIDE_INT x
	    = (((o0 ^ o1) & (result.val[result.len - 1] ^ o0)) >> p) & 1;
	  *overflow = (x != 0);
	}
      else
	{
	  if (old_borrow)
	    *overflow = ((unsigned HOST_WIDE_INT)result.val[result.len - 1] >= o0);
	  else
	    *overflow = ((unsigned HOST_WIDE_INT)result.val[result.len - 1] > o0);
	}
    }


  small_prec = prec & (HOST_BITS_PER_WIDE_INT - 1);
  if (small_prec != 0 && BLOCKS_NEEDED (prec) == result.len)
    {
      /* Modes with weird precisions.  */
      i = result.len - 1;
      result.val[i] = sext_hwi (result.val[i], small_prec);
    }

  result.canonize ();

  return result;
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
void
wide_int_ro::divmod_internal_2 (unsigned HOST_HALF_WIDE_INT *b_quotient,
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
  HOST_WIDE_INT s, i, j, t, k;

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


/* Do a truncating divide DIVISOR into DIVIDEND.  The result is the
   same size as the operands.  SIGN is either SIGNED or UNSIGNED.  */
wide_int_ro
wide_int_ro::divmod_internal (bool compute_quotient,
			      const HOST_WIDE_INT *dividend,
			      unsigned int dividend_len,
			      unsigned int dividend_prec,
			      const HOST_WIDE_INT *divisor,
			      unsigned int divisor_len,
			      unsigned int divisor_prec,
			      signop sgn, wide_int_ro *remainder,
			      bool compute_remainder,
			      bool *oflow)
{
  wide_int quotient, u0, u1;
  int dividend_blocks_needed = 2 * BLOCKS_NEEDED (dividend_prec);
  int divisor_blocks_needed = 2 * BLOCKS_NEEDED (divisor_prec);
  unsigned HOST_HALF_WIDE_INT
    b_quotient[4 * MAX_BITSIZE_MODE_ANY_INT / HOST_BITS_PER_HALF_WIDE_INT];
  unsigned HOST_HALF_WIDE_INT
    b_remainder[4 * MAX_BITSIZE_MODE_ANY_INT / HOST_BITS_PER_HALF_WIDE_INT];
  unsigned HOST_HALF_WIDE_INT
    b_dividend[(4 * MAX_BITSIZE_MODE_ANY_INT / HOST_BITS_PER_HALF_WIDE_INT) + 1];
  unsigned HOST_HALF_WIDE_INT
    b_divisor[4 * MAX_BITSIZE_MODE_ANY_INT / HOST_BITS_PER_HALF_WIDE_INT];
  int m, n;
  bool dividend_neg = false;
  bool divisor_neg = false;
  bool overflow = false;

  if (divisor[0] == 0 && divisor_len == 1)
    overflow = true;

  /* The smallest signed number / -1 causes overflow.  */
  if (sgn == SIGNED)
    {
      HOST_WIDE_INT small_prec = dividend_prec & (HOST_BITS_PER_WIDE_INT - 1);
      if (dividend_len == BLOCKS_NEEDED (dividend_prec)
	  && divisor_len == 1
	  && divisor[0] == HOST_WIDE_INT(-1))

	if ((small_prec
	     && ((HOST_WIDE_INT)zext_hwi (dividend[dividend_len - 1],
					  small_prec)
		 == (HOST_WIDE_INT(1) << (small_prec - 1))))
	    || dividend[dividend_len - 1]
	    == HOST_WIDE_INT(1) << (HOST_BITS_PER_WIDE_INT - 1))
	  {
	    /* The smallest neg number is 100...00.  The high word was
	       checked above, now check the rest of the words are
	       zero.  */
	    unsigned int i;
	    bool all_zero = true;
	    for (i = 0; i < dividend_len - 1; i++)
	      if (dividend[i] != 0)
		{
		  all_zero = false;
		  break;
		}
	    if (all_zero)
	      overflow = true;
	  }
    }

  quotient.precision = dividend_prec;
  remainder->precision = dividend_prec;

  /* If overflow is set, just get out.  There will only be grief by
     continuing.  */
  if (overflow)
    {
      if (compute_remainder)
	{
	  remainder->len = 1;
	  remainder->val[0] = 0;
	}
      if (oflow != 0)
	*oflow = true;
      return wide_int::zero (dividend_prec);
    }

  /* Do it on the host if you can.  */
  if (dividend_prec <= HOST_BITS_PER_WIDE_INT
      && divisor_prec <= HOST_BITS_PER_WIDE_INT)
    {
      quotient.len = 1;
      remainder->len = 1;
      if (sgn == SIGNED)
	{
	  HOST_WIDE_INT o0 = sext_hwi (dividend[0], dividend_prec);
	  HOST_WIDE_INT o1 = sext_hwi (divisor[0], divisor_prec);

	  quotient.val[0] = sext_hwi (o0 / o1, dividend_prec);
	  remainder->val[0] = sext_hwi (o0 % o1, dividend_prec);
	}
      else
	{
	  unsigned HOST_WIDE_INT o0 = zext_hwi (dividend[0], dividend_prec);
	  unsigned HOST_WIDE_INT o1 = zext_hwi (divisor[0], divisor_prec);

	  quotient.val[0] = zext_hwi (o0 / o1, dividend_prec);
	  remainder->val[0] = zext_hwi (o0 % o1, dividend_prec);
	}

#ifdef DEBUG_WIDE_INT
      debug_wwasa ("wide_int_ro:: (q = %s) (r = %s) = (%s /%s %s)\n",
		   quotient, *remainder,
		   dividend, dividend_len, dividend_prec,
		   sgn == SIGNED ? "S" : "U",
		     divisor, divisor_len, divisor_prec);
#endif
      return quotient;
    }

  /* Make the divisor and dividend positive and remember what we
     did.  */
  if (sgn == SIGNED)
    {
      if (top_bit_of (dividend, dividend_len, dividend_prec))
	{
	  u0 = sub_large (wide_int (0).val, 1,
			  dividend_prec, dividend, dividend_len, UNSIGNED);
	  dividend = u0.val;
	  dividend_len = u0.len;
	  dividend_neg = true;
	}
      if (top_bit_of (divisor, divisor_len, divisor_prec))
	{
	  u1 = sub_large (wide_int (0).val, 1,
			  divisor_prec, divisor, divisor_len, UNSIGNED);
	  divisor = u1.val;
	  divisor_len = u1.len;
	  divisor_neg = true;
	}
    }

  wi_unpack (b_dividend, (const unsigned HOST_WIDE_INT*)dividend,
	     dividend_len, dividend_blocks_needed, dividend_prec, sgn);
  wi_unpack (b_divisor, (const unsigned HOST_WIDE_INT*)divisor,
	     divisor_len, divisor_blocks_needed, divisor_prec, sgn);

  if (top_bit_of (dividend, dividend_len, dividend_prec) && sgn == SIGNED)
    m = dividend_blocks_needed;
  else
    m = 2 * dividend_len;

  if (top_bit_of (divisor, divisor_len, divisor_prec) && sgn == SIGNED)
    n = divisor_blocks_needed;
  else
    n = 2 * divisor_len;

  /* We need to find the top non zero block of b_divisor.  At most the
     top two blocks are zero.  */
  if (b_divisor[n - 1] == 0)
    n--;
  if (b_divisor[n - 1] == 0)
    n--;

  memset (b_quotient, 0, sizeof (b_quotient));

  divmod_internal_2 (b_quotient, b_remainder, b_dividend, b_divisor, m, n);

  if (compute_quotient)
    {
      wi_pack ((unsigned HOST_WIDE_INT*)quotient.val, b_quotient, m);
      quotient.len = m / 2;
      quotient.canonize ();
      /* The quotient is neg if exactly one of the divisor or dividend is
	 neg.  */
      if (dividend_neg != divisor_neg)
	quotient = -quotient;
    }
  else
    quotient = wide_int::zero (dividend_prec);

  if (compute_remainder)
    {
      wi_pack ((unsigned HOST_WIDE_INT*)remainder->val, b_remainder, n);
      if (n & 1)
	n++;
      remainder->len = n / 2;
      (*remainder).canonize ();
      /* The remainder is always the same sign as the dividend.  */
      if (dividend_neg)
	*remainder = -(*remainder);
    }
  else
    *remainder = wide_int::zero (dividend_prec);

#ifdef DEBUG_WIDE_INT
  debug_wwasa ("wide_int_ro:: (q = %s) (r = %s) = (%s /%s %s)\n",
	       quotient, *remainder,
	       dividend, dividend_len, dividend_prec,
	       sgn == SIGNED ? "S" : "U",
		 divisor, divisor_len, divisor_prec);
#endif
  return quotient;
}


/* Return TRUE iff PRODUCT is an integral multiple of FACTOR, and return
   the multiple in *MULTIPLE.  Otherwise return FALSE and leave *MULTIPLE
   unchanged.  */
bool
wide_int_ro::multiple_of_p (const wide_int_ro &factor,
			    signop sgn, wide_int_ro *multiple) const
{
  wide_int remainder;
  wide_int quotient = divmod_trunc (factor, &remainder, sgn);
  if (remainder.zero_p ())
    {
      *multiple = quotient;
      return true;
    }

  return false;
}

/*
 * Shifting, rotating and extraction.
 */

/* Extract WIDTH bits from THIS starting at OFFSET.  The result is
   assumed to fit in a HOST_WIDE_INT.  This function is safe in that
   it can properly access elements that may not be explicitly
   represented.  */
HOST_WIDE_INT
wide_int_ro::extract_to_hwi (int offset, int width) const
{
  int start_elt, end_elt, shift;
  HOST_WIDE_INT x;

  /* Get rid of the easy cases first.   */
  if (offset >= len * HOST_BITS_PER_WIDE_INT)
    return sign_mask ();
  if (offset + width <= 0)
    return 0;

  shift = offset & (HOST_BITS_PER_WIDE_INT - 1);
  if (offset < 0)
    {
      start_elt = -1;
      end_elt = 0;
      x = 0;
    }
  else
    {
      start_elt = offset / HOST_BITS_PER_WIDE_INT;
      end_elt = (offset + width - 1) / HOST_BITS_PER_WIDE_INT;
      x = start_elt >= len
	? sign_mask ()
	: (unsigned HOST_WIDE_INT)val[start_elt] >> shift;
    }

  if (start_elt != end_elt)
    {
      HOST_WIDE_INT y = end_elt == len
	? sign_mask () : val[end_elt];

      x |= y << (HOST_BITS_PER_WIDE_INT - shift);
    }

  if (width != HOST_BITS_PER_WIDE_INT)
    x &= ((HOST_WIDE_INT)1 << width) - 1;

  return x;
}


/* Left shift THIS by CNT.  See the definition of Op.TRUNC for how to
   set Z.  Since this is used internally, it has the ability to
   specify the BISIZE and PRECISION independently.  This is useful
   when inserting a small value into a larger one.  */
wide_int_ro
wide_int_ro::lshift_large (unsigned int cnt, unsigned int res_prec) const
{
  wide_int result;
  unsigned int i;

  result.precision = res_prec;

  if (cnt >= res_prec)
    {
      result.val[0] = 0;
      result.len = 1;
      return result;
    }

  for (i = 0; i < res_prec; i += HOST_BITS_PER_WIDE_INT)
    result.val[i / HOST_BITS_PER_WIDE_INT]
      = extract_to_hwi (i - cnt, HOST_BITS_PER_WIDE_INT);

  result.len = BLOCKS_NEEDED (res_prec);
  result.canonize ();

  return result;
}

/* Unsigned right shift THIS by CNT.  */
wide_int_ro
wide_int_ro::rshiftu_large (unsigned int cnt) const
{
  wide_int result;
  int i;
  int small_prec = (precision - cnt) & (HOST_BITS_PER_WIDE_INT - 1);

  if (cnt == 0)
    return *this;

  result.precision = precision;

  if (cnt >= precision)
    {
      result.val[0] = 0;
      result.len = 1;
      return result;
    }

  result.len = BLOCKS_NEEDED (precision - cnt);

  for (i = 0; i < result.len; i++)
    result.val[i]
      = extract_to_hwi ((i * HOST_BITS_PER_WIDE_INT) + cnt,
			HOST_BITS_PER_WIDE_INT);

  /* Extract_to_hwi sign extends.  So we need to fix that up.  */
  if (small_prec)
    result.val [result.len - 1]
      = zext_hwi (result.val [result.len - 1], small_prec);
  else if (result.val[result.len - 1] < 0)
    {
      /* Add a new block with a zero. */
      result.val[result.len++] = 0;
      return result;
    }

  result.canonize ();

  return result;
}

/* Signed right shift THIS by CNT.  */
wide_int_ro
wide_int_ro::rshifts_large (unsigned int cnt) const
{
  wide_int result;
  int i;

  if (cnt == 0)
    return *this;

  result.precision = precision;

  if (cnt >= precision)
    {
      HOST_WIDE_INT m = sign_mask ();
      result.val[0] = m;
      result.len = 1;
      return result;
    }

  result.len = BLOCKS_NEEDED (precision - cnt);

  for (i = 0; i < result.len; i++)
    result.val[i]
      = extract_to_hwi ((i * HOST_BITS_PER_WIDE_INT) + cnt,
			HOST_BITS_PER_WIDE_INT);

  result.canonize ();

  return result;
}

/*
 * Private utilities.
 */

/* Decompress THIS for at least TARGET bits into a result with
   precision PREC.  */
wide_int_ro
wide_int_ro::decompress (unsigned int target, unsigned int prec) const
{
  wide_int result;
  int blocks_needed = BLOCKS_NEEDED (target);
  HOST_WIDE_INT mask;
  int len, i;

  result.precision = prec;
  result.len = blocks_needed;

  for (i = 0; i < this->len; i++)
    result.val[i] = val[i];

  len = this->len;

  if (target > result.precision)
    return result;

  /* The extension that we are doing here is not sign extension, it is
     decompression.  */
  mask = sign_mask ();
  while (len < blocks_needed)
    result.val[len++] = mask;

  return result;
}

void gt_ggc_mx(max_wide_int*) { }
void gt_pch_nx(max_wide_int*,void (*)(void*, void*), void*) { }
void gt_pch_nx(max_wide_int*) { }

/*
 * Private debug printing routines.
 */
#ifdef DEBUG_WIDE_INT
/* The debugging routines print results of wide operations into the
   dump files of the respective passes in which they were called.  */
static char *
dumpa (const HOST_WIDE_INT *val, unsigned int len, unsigned int prec, char *buf)
{
  int i;
  int l;
  const char * sep = "";

  l = sprintf (buf, "[%d (", prec);
  for (i = len - 1; i >= 0; i--)
    {
      l += sprintf (&buf[l], "%s" HOST_WIDE_INT_PRINT_HEX, sep, val[i]);
      sep = " ";
    }

  gcc_assert (len != 0);

  l += sprintf (&buf[l], ")]");

  gcc_assert (l < MAX_SIZE);
  return buf;


}
#endif

/* The debugging routines print results of wide operations into the
   dump files of the respective passes in which they were called.  */
char *
wide_int_ro::dump (char* buf) const
{
  int i;
  int l;
  const char * sep = "";

  l = sprintf (buf, "[%d (", precision);
  for (i = len - 1; i >= 0; i--)
    {
      l += sprintf (&buf[l], "%s" HOST_WIDE_INT_PRINT_HEX, sep, val[i]);
      sep = " ";
    }

  gcc_assert (len != 0);

  l += sprintf (&buf[l], ")]");

  gcc_assert (l < MAX_SIZE);
  return buf;
}

#ifdef DEBUG_WIDE_INT

#if 0
#define wide_int_dump_file (dump_file ? dump_file : stdout)
#else
#define wide_int_dump_file (dump_file)
#endif

void
wide_int_ro::debug_vaa (const char* fmt, int r,
			const HOST_WIDE_INT *o0, unsigned int l0, unsigned int p0,
			const HOST_WIDE_INT *o1, unsigned int l1, unsigned int p1)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r,
	     dumpa (o0, l0, p0, buf0),
	     dumpa (o1, l1, p1, buf1));
}

void
wide_int_ro::debug_vw (const char* fmt, int r, const wide_int_ro& o0)
{
  char buf0[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r, o0.dump (buf0));
}

void
wide_int_ro::debug_vwa (const char* fmt, int r, const wide_int_ro &o0,
			const HOST_WIDE_INT *o1, unsigned int l1, unsigned int p1)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r, o0.dump (buf0), dumpa (o1, l1, p1, buf1));
}

void
wide_int_ro::debug_vwh (const char* fmt, int r, const wide_int_ro &o0,
			HOST_WIDE_INT o1)
{
  char buf0[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r, o0.dump (buf0), o1);
}

void
wide_int_ro::debug_vww (const char* fmt, int r, const wide_int_ro &o0,
			const wide_int_ro &o1)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r, o0.dump (buf0), o1.dump (buf1));
}

void
wide_int_ro::debug_wa (const char* fmt, const wide_int_ro &r,
		       const HOST_WIDE_INT *o0, unsigned int l0, unsigned int p0)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0), dumpa (o0, l0, p0, buf1));
}

void
wide_int_ro::debug_waa (const char* fmt, const wide_int_ro &r,
			const HOST_WIDE_INT *o0, unsigned int l0, unsigned int p0,
			const HOST_WIDE_INT *o1, unsigned int l1, unsigned int p1)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  char buf2[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0), dumpa (o0, l0, p0, buf1),
	     dumpa (o1, l1, p1, buf2));
}

void
wide_int_ro::debug_waav (const char* fmt, const wide_int_ro &r,
			 const HOST_WIDE_INT *o0, unsigned int l0, unsigned int p0,
			 const HOST_WIDE_INT *o1, unsigned int l1, unsigned int p1,
			 int s)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  char buf2[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0), dumpa (o0, l0, p0, buf1),
	     dumpa (o1, l1, p1, buf2), s);
}

void
wide_int_ro::debug_wh (const char* fmt, const wide_int_ro &r,
		       HOST_WIDE_INT o1)
{
  char buf0[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0), o1);
}

void
wide_int_ro::debug_whh (const char* fmt, const wide_int_ro &r,
			HOST_WIDE_INT o1, HOST_WIDE_INT o2)
{
  char buf0[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0), o1, o2);
}

void
wide_int_ro::debug_wv (const char* fmt, const wide_int_ro &r, int v0)
{
  char buf0[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0), v0);
}

void
wide_int_ro::debug_wvv (const char* fmt, const wide_int_ro &r,
			int v0, int v1)
{
  char buf0[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0), v0, v1);
}

void
wide_int_ro::debug_wvvv (const char* fmt, const wide_int_ro &r,
			 int v0, int v1, int v2)
{
  char buf0[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0), v0, v1, v2);
}

void
wide_int_ro::debug_wvwa (const char* fmt, const wide_int_ro &r, int v0,
			 const wide_int_ro &o0,
			 const HOST_WIDE_INT *o1, unsigned int l1, unsigned int p1)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  char buf2[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0), v0,
	     o0.dump (buf1), dumpa (o1, l1, p1, buf2));
}

void
wide_int_ro::debug_wvasa (const char* fmt, const wide_int_ro &r, int v0,
			  const HOST_WIDE_INT *o0, unsigned int l0, unsigned int p0,
			  const char* s,
			  const HOST_WIDE_INT *o1, unsigned int l1, unsigned int p1)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  char buf2[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0), v0,
	     dumpa (o0, l0, p0, buf1), s, dumpa (o1, l1, p1, buf2));
}

void
wide_int_ro::debug_wvww (const char* fmt, const wide_int_ro &r, int v0,
			 const wide_int_ro &o0, const wide_int_ro &o1)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  char buf2[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0), v0,
	     o0.dump (buf1), o1.dump (buf2));
}

void
wide_int_ro::debug_ww (const char* fmt, const wide_int_ro &r,
		       const wide_int_ro &o0)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0), o0.dump (buf1));
}

void
wide_int_ro::debug_wwa (const char* fmt, const wide_int_ro &r,
			const wide_int_ro &o0,
			const HOST_WIDE_INT *o1, unsigned int l1, unsigned int p1)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  char buf2[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0), o0.dump (buf1),
	     dumpa (o1, l1, p1, buf2));
}

void
wide_int_ro::debug_wwv (const char* fmt, const wide_int_ro &r,
			const wide_int_ro &o0, int v0)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0), o0.dump (buf1), v0);
}

void
wide_int_ro::debug_wwvs (const char* fmt, const wide_int_ro &r,
			 const wide_int_ro &o0, int v0,
			 const char *s)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0), o0.dump (buf1), v0, s);
}

void
wide_int_ro::debug_wwvvs (const char* fmt, const wide_int_ro &r,
			  const wide_int_ro &o0, int v0, int v1,
			  const char *s)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0), o0.dump (buf1), v0, v1, s);
}

void
wide_int_ro::debug_wwwvv (const char* fmt, const wide_int_ro &r,
			  const wide_int_ro &o0, const wide_int_ro &o1,
			  int v0, int v1)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  char buf2[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0),
	     o0.dump (buf1), o1.dump (buf2), v0, v1);
}

void
wide_int_ro::debug_www (const char* fmt, const wide_int_ro &r,
			const wide_int_ro &o0, const wide_int_ro &o1)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  char buf2[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0),
	     o0.dump (buf1), o1.dump (buf2));
}

void
wide_int_ro::debug_wwasa (const char* fmt, const wide_int_ro &r, const wide_int_ro &o0,
			  const HOST_WIDE_INT *o1, unsigned int l1, unsigned int p1,
			  const char* s,
			  const HOST_WIDE_INT *o2, unsigned int l2, unsigned int p2)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  char buf2[MAX_SIZE];
  char buf3[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0),
	     o0.dump (buf1), dumpa (o1, l1, p1, buf2), s, dumpa (o2, l2, p2, buf3));
}

void
wide_int_ro::debug_wwww (const char* fmt, const wide_int_ro &r,
			 const wide_int_ro &o0, const wide_int_ro &o1,
			 const wide_int_ro &o2)
{
  char buf0[MAX_SIZE];
  char buf1[MAX_SIZE];
  char buf2[MAX_SIZE];
  char buf3[MAX_SIZE];
  if (wide_int_dump_file)
    fprintf (wide_int_dump_file, fmt, r.dump (buf0),
	     o0.dump (buf1), o1.dump (buf2), o2.dump (buf3));
}

#endif

