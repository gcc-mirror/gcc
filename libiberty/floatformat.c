/* IEEE floating point support routines, for GDB, the GNU Debugger.
   Copyright (C) 1991, 1994, 1999, 2000 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "floatformat.h"
#include <math.h>		/* ldexp */
#ifdef __STDC__
#include <stddef.h>
extern void *memcpy (void *s1, const void *s2, size_t n);
extern void *memset (void *s, int c, size_t n);
#else
extern char *memcpy ();
extern char *memset ();
#endif

/* The odds that CHAR_BIT will be anything but 8 are low enough that I'm not
   going to bother with trying to muck around with whether it is defined in
   a system header, what we do if not, etc.  */
#define FLOATFORMAT_CHAR_BIT 8

/* floatformats for IEEE single and double, big and little endian.  */
const struct floatformat floatformat_ieee_single_big =
{
  floatformat_big, 32, 0, 1, 8, 127, 255, 9, 23,
  floatformat_intbit_no,
  "floatformat_ieee_single_big"
};
const struct floatformat floatformat_ieee_single_little =
{
  floatformat_little, 32, 0, 1, 8, 127, 255, 9, 23,
  floatformat_intbit_no,
  "floatformat_ieee_single_little"
};
const struct floatformat floatformat_ieee_double_big =
{
  floatformat_big, 64, 0, 1, 11, 1023, 2047, 12, 52,
  floatformat_intbit_no,
  "floatformat_ieee_double_big"
};
const struct floatformat floatformat_ieee_double_little =
{
  floatformat_little, 64, 0, 1, 11, 1023, 2047, 12, 52,
  floatformat_intbit_no,
  "floatformat_ieee_double_little"
};

/* floatformat for IEEE double, little endian byte order, with big endian word
   ordering, as on the ARM.  */

const struct floatformat floatformat_ieee_double_littlebyte_bigword =
{
  floatformat_littlebyte_bigword, 64, 0, 1, 11, 1023, 2047, 12, 52,
  floatformat_intbit_no,
  "floatformat_ieee_double_little"
};

const struct floatformat floatformat_i387_ext =
{
  floatformat_little, 80, 0, 1, 15, 0x3fff, 0x7fff, 16, 64,
  floatformat_intbit_yes,
  "floatformat_i387_ext"
};
const struct floatformat floatformat_m68881_ext =
{
  /* Note that the bits from 16 to 31 are unused.  */
  floatformat_big, 96, 0, 1, 15, 0x3fff, 0x7fff, 32, 64,
  floatformat_intbit_yes,
  "floatformat_m68881_ext"
};
const struct floatformat floatformat_i960_ext =
{
  /* Note that the bits from 0 to 15 are unused.  */
  floatformat_little, 96, 16, 17, 15, 0x3fff, 0x7fff, 32, 64,
  floatformat_intbit_yes,
  "floatformat_i960_ext"
};
const struct floatformat floatformat_m88110_ext =
{
#ifdef HARRIS_FLOAT_FORMAT
  /* Harris uses raw format 128 bytes long, but the number is just an ieee
     double, and the last 64 bits are wasted. */
  floatformat_big,128, 0, 1, 11,  0x3ff,  0x7ff, 12, 52,
  floatformat_intbit_no,
  "floatformat_m88110_ext(harris)"
#else
  floatformat_big, 80, 0, 1, 15, 0x3fff, 0x7fff, 16, 64,
  floatformat_intbit_yes,
  "floatformat_m88110_ext"
#endif /* HARRIS_FLOAT_FORMAT */
};
const struct floatformat floatformat_arm_ext =
{
  /* Bits 1 to 16 are unused.  */
  floatformat_big, 96, 0, 17, 15, 0x3fff, 0x7fff, 32, 64,
  floatformat_intbit_yes,
  "floatformat_arm_ext"
};

static unsigned long get_field PARAMS ((unsigned char *,
					enum floatformat_byteorders,
					unsigned int,
					unsigned int,
					unsigned int));

/* Extract a field which starts at START and is LEN bytes long.  DATA and
   TOTAL_LEN are the thing we are extracting it from, in byteorder ORDER.  */
static unsigned long
get_field (data, order, total_len, start, len)
     unsigned char *data;
     enum floatformat_byteorders order;
     unsigned int total_len;
     unsigned int start;
     unsigned int len;
{
  unsigned long result;
  unsigned int cur_byte;
  int cur_bitshift;

  /* Start at the least significant part of the field.  */
  cur_byte = (start + len) / FLOATFORMAT_CHAR_BIT;
  if (order == floatformat_little)
    cur_byte = (total_len / FLOATFORMAT_CHAR_BIT) - cur_byte - 1;
  cur_bitshift =
    ((start + len) % FLOATFORMAT_CHAR_BIT) - FLOATFORMAT_CHAR_BIT;
  result = *(data + cur_byte) >> (-cur_bitshift);
  cur_bitshift += FLOATFORMAT_CHAR_BIT;
  if (order == floatformat_little)
    ++cur_byte;
  else
    --cur_byte;

  /* Move towards the most significant part of the field.  */
  while ((unsigned int) cur_bitshift < len)
    {
      if (len - cur_bitshift < FLOATFORMAT_CHAR_BIT)
	/* This is the last byte; zero out the bits which are not part of
	   this field.  */
	result |=
	  (*(data + cur_byte) & ((1 << (len - cur_bitshift)) - 1))
	    << cur_bitshift;
      else
	result |= *(data + cur_byte) << cur_bitshift;
      cur_bitshift += FLOATFORMAT_CHAR_BIT;
      if (order == floatformat_little)
	++cur_byte;
      else
	--cur_byte;
    }
  return result;
}
  
#ifndef min
#define min(a, b) ((a) < (b) ? (a) : (b))
#endif

/* Convert from FMT to a double.
   FROM is the address of the extended float.
   Store the double in *TO.  */

void
floatformat_to_double (fmt, from, to)
     const struct floatformat *fmt;
     char *from;
     double *to;
{
  unsigned char *ufrom = (unsigned char *)from;
  double dto;
  long exponent;
  unsigned long mant;
  unsigned int mant_bits, mant_off;
  int mant_bits_left;
  int special_exponent;		/* It's a NaN, denorm or zero */

  exponent = get_field (ufrom, fmt->byteorder, fmt->totalsize,
			fmt->exp_start, fmt->exp_len);
  /* Note that if exponent indicates a NaN, we can't really do anything useful
     (not knowing if the host has NaN's, or how to build one).  So it will
     end up as an infinity or something close; that is OK.  */

  mant_bits_left = fmt->man_len;
  mant_off = fmt->man_start;
  dto = 0.0;

  special_exponent = exponent == 0 || (unsigned long) exponent == fmt->exp_nan;

  /* Don't bias zero's, denorms or NaNs.  */
  if (!special_exponent)
    exponent -= fmt->exp_bias;

  /* Build the result algebraically.  Might go infinite, underflow, etc;
     who cares. */

  /* If this format uses a hidden bit, explicitly add it in now.  Otherwise,
     increment the exponent by one to account for the integer bit.  */

  if (!special_exponent)
    {
      if (fmt->intbit == floatformat_intbit_no)
	dto = ldexp (1.0, exponent);
      else
	exponent++;
    }

  while (mant_bits_left > 0)
    {
      mant_bits = min (mant_bits_left, 32);

      mant = get_field (ufrom, fmt->byteorder, fmt->totalsize,
			 mant_off, mant_bits);

      dto += ldexp ((double)mant, exponent - mant_bits);
      exponent -= mant_bits;
      mant_off += mant_bits;
      mant_bits_left -= mant_bits;
    }

  /* Negate it if negative.  */
  if (get_field (ufrom, fmt->byteorder, fmt->totalsize, fmt->sign_start, 1))
    dto = -dto;
  *to = dto;
}

static void put_field PARAMS ((unsigned char *, enum floatformat_byteorders,
			       unsigned int,
			       unsigned int,
			       unsigned int,
			       unsigned long));

/* Set a field which starts at START and is LEN bytes long.  DATA and
   TOTAL_LEN are the thing we are extracting it from, in byteorder ORDER.  */
static void
put_field (data, order, total_len, start, len, stuff_to_put)
     unsigned char *data;
     enum floatformat_byteorders order;
     unsigned int total_len;
     unsigned int start;
     unsigned int len;
     unsigned long stuff_to_put;
{
  unsigned int cur_byte;
  int cur_bitshift;

  /* Start at the least significant part of the field.  */
  cur_byte = (start + len) / FLOATFORMAT_CHAR_BIT;
  if (order == floatformat_little)
    cur_byte = (total_len / FLOATFORMAT_CHAR_BIT) - cur_byte - 1;
  cur_bitshift =
    ((start + len) % FLOATFORMAT_CHAR_BIT) - FLOATFORMAT_CHAR_BIT;
  *(data + cur_byte) &=
    ~(((1 << ((start + len) % FLOATFORMAT_CHAR_BIT)) - 1) << (-cur_bitshift));
  *(data + cur_byte) |=
    (stuff_to_put & ((1 << FLOATFORMAT_CHAR_BIT) - 1)) << (-cur_bitshift);
  cur_bitshift += FLOATFORMAT_CHAR_BIT;
  if (order == floatformat_little)
    ++cur_byte;
  else
    --cur_byte;

  /* Move towards the most significant part of the field.  */
  while ((unsigned int) cur_bitshift < len)
    {
      if (len - cur_bitshift < FLOATFORMAT_CHAR_BIT)
	{
	  /* This is the last byte.  */
	  *(data + cur_byte) &=
	    ~((1 << (len - cur_bitshift)) - 1);
	  *(data + cur_byte) |= (stuff_to_put >> cur_bitshift);
	}
      else
	*(data + cur_byte) = ((stuff_to_put >> cur_bitshift)
			      & ((1 << FLOATFORMAT_CHAR_BIT) - 1));
      cur_bitshift += FLOATFORMAT_CHAR_BIT;
      if (order == floatformat_little)
	++cur_byte;
      else
	--cur_byte;
    }
}

/* The converse: convert the double *FROM to an extended float
   and store where TO points.  Neither FROM nor TO have any alignment
   restrictions.  */

void
floatformat_from_double (fmt, from, to)
     const struct floatformat *fmt;
     double *from;
     char *to;
{
  double dfrom;
  int exponent;
  double mant;
  unsigned int mant_bits, mant_off;
  int mant_bits_left;
  unsigned char *uto = (unsigned char *)to;

  memcpy (&dfrom, from, sizeof (dfrom));
  memset (uto, 0, fmt->totalsize / FLOATFORMAT_CHAR_BIT);
  if (dfrom == 0)
    return;			/* Result is zero */
  if (dfrom != dfrom)
    {
      /* From is NaN */
      put_field (uto, fmt->byteorder, fmt->totalsize, fmt->exp_start,
		 fmt->exp_len, fmt->exp_nan);
      /* Be sure it's not infinity, but NaN value is irrel */
      put_field (uto, fmt->byteorder, fmt->totalsize, fmt->man_start,
		 32, 1);
      return;
    }

  /* If negative, set the sign bit.  */
  if (dfrom < 0)
    {
      put_field (uto, fmt->byteorder, fmt->totalsize, fmt->sign_start, 1, 1);
      dfrom = -dfrom;
    }

  /* How to tell an infinity from an ordinary number?  FIXME-someday */

  mant = frexp (dfrom, &exponent);
  put_field (uto, fmt->byteorder, fmt->totalsize, fmt->exp_start, fmt->exp_len,
	     exponent + fmt->exp_bias - 1);

  mant_bits_left = fmt->man_len;
  mant_off = fmt->man_start;
  while (mant_bits_left > 0)
    {
      unsigned long mant_long;
      mant_bits = mant_bits_left < 32 ? mant_bits_left : 32;

      mant *= 4294967296.0;
      mant_long = (unsigned long)mant;
      mant -= mant_long;

      /* If the integer bit is implicit, then we need to discard it.
	 If we are discarding a zero, we should be (but are not) creating
	 a denormalized	number which means adjusting the exponent
	 (I think).  */
      if ((unsigned int) mant_bits_left == fmt->man_len
	  && fmt->intbit == floatformat_intbit_no)
	{
	  mant_long &= 0x7fffffff;
	  mant_bits -= 1;
	}
      else if (mant_bits < 32)
	{
	  /* The bits we want are in the most significant MANT_BITS bits of
	     mant_long.  Move them to the least significant.  */
	  mant_long >>= 32 - mant_bits;
	}

      put_field (uto, fmt->byteorder, fmt->totalsize,
		 mant_off, mant_bits, mant_long);
      mant_off += mant_bits;
      mant_bits_left -= mant_bits;
    }
}


#ifdef IEEE_DEBUG

/* This is to be run on a host which uses IEEE floating point.  */

void
ieee_test (n)
     double n;
{
  double result;
  char exten[16];

  floatformat_to_double (&floatformat_ieee_double_big, &n, &result);
  if (n != result)
    printf ("Differ(to): %.20g -> %.20g\n", n, result);
  floatformat_from_double (&floatformat_ieee_double_big, &n, &result);
  if (n != result)
    printf ("Differ(from): %.20g -> %.20g\n", n, result);

  floatformat_from_double (&floatformat_m68881_ext, &n, exten);
  floatformat_to_double (&floatformat_m68881_ext, exten, &result);
  if (n != result)
    printf ("Differ(to+from): %.20g -> %.20g\n", n, result);

#if IEEE_DEBUG > 1
  /* This is to be run on a host which uses 68881 format.  */
  {
    long double ex = *(long double *)exten;
    if (ex != n)
      printf ("Differ(from vs. extended): %.20g\n", n);
  }
#endif
}

int
main ()
{
  ieee_test (0.5);
  ieee_test (256.0);
  ieee_test (0.12345);
  ieee_test (234235.78907234);
  ieee_test (-512.0);
  ieee_test (-0.004321);
  return 0;
}
#endif
