/* Copyright (C) 2002, 2003, 2005, 2007, 2008, 2009 Free Software Foundation, Inc.
   Contributed by Andy Vaught
   F2003 I/O support contributed by Jerry DeLisle

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Libgfortran is distributed in the hope that it will be useful,
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

#include "io.h"
#include "fbuf.h"
#include "format.h"
#include "unix.h"
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>

typedef unsigned char uchar;

/* read.c -- Deal with formatted reads */


/* set_integer()-- All of the integer assignments come here to
 * actually place the value into memory.  */

void
set_integer (void *dest, GFC_INTEGER_LARGEST value, int length)
{
  switch (length)
    {
#ifdef HAVE_GFC_INTEGER_16
/* length=10 comes about for kind=10 real/complex BOZ, cf. PR41711. */
    case 10:
    case 16:
      {
	GFC_INTEGER_16 tmp = value;
	memcpy (dest, (void *) &tmp, length);
      }
      break;
#endif
    case 8:
      {
	GFC_INTEGER_8 tmp = value;
	memcpy (dest, (void *) &tmp, length);
      }
      break;
    case 4:
      {
	GFC_INTEGER_4 tmp = value;
	memcpy (dest, (void *) &tmp, length);
      }
      break;
    case 2:
      {
	GFC_INTEGER_2 tmp = value;
	memcpy (dest, (void *) &tmp, length);
      }
      break;
    case 1:
      {
	GFC_INTEGER_1 tmp = value;
	memcpy (dest, (void *) &tmp, length);
      }
      break;
    default:
      internal_error (NULL, "Bad integer kind");
    }
}


/* max_value()-- Given a length (kind), return the maximum signed or
 * unsigned value */

GFC_UINTEGER_LARGEST
max_value (int length, int signed_flag)
{
  GFC_UINTEGER_LARGEST value;
#if defined HAVE_GFC_REAL_16 || defined HAVE_GFC_REAL_10
  int n;
#endif

  switch (length)
    {
#if defined HAVE_GFC_REAL_16 || defined HAVE_GFC_REAL_10
    case 16:
    case 10:
      value = 1;
      for (n = 1; n < 4 * length; n++)
        value = (value << 2) + 3;
      if (! signed_flag)
        value = 2*value+1;
      break;
#endif
    case 8:
      value = signed_flag ? 0x7fffffffffffffff : 0xffffffffffffffff;
      break;
    case 4:
      value = signed_flag ? 0x7fffffff : 0xffffffff;
      break;
    case 2:
      value = signed_flag ? 0x7fff : 0xffff;
      break;
    case 1:
      value = signed_flag ? 0x7f : 0xff;
      break;
    default:
      internal_error (NULL, "Bad integer kind");
    }

  return value;
}


/* convert_real()-- Convert a character representation of a floating
 * point number to the machine number.  Returns nonzero if there is a
 * range problem during conversion.  Note: many architectures
 * (e.g. IA-64, HP-PA) require that the storage pointed to by the dest
 * argument is properly aligned for the type in question.  TODO:
 * handle not-a-numbers and infinities.  */

int
convert_real (st_parameter_dt *dtp, void *dest, const char *buffer, int length)
{
  errno = 0;

  switch (length)
    {
    case 4:
      *((GFC_REAL_4*) dest) =
#if defined(HAVE_STRTOF)
	gfc_strtof (buffer, NULL);
#else
	(GFC_REAL_4) gfc_strtod (buffer, NULL);
#endif
      break;

    case 8:
      *((GFC_REAL_8*) dest) = gfc_strtod (buffer, NULL);
      break;

#if defined(HAVE_GFC_REAL_10) && defined (HAVE_STRTOLD)
    case 10:
      *((GFC_REAL_10*) dest) = gfc_strtold (buffer, NULL);
      break;
#endif

#if defined(HAVE_GFC_REAL_16) && defined (HAVE_STRTOLD)
    case 16:
      *((GFC_REAL_16*) dest) = gfc_strtold (buffer, NULL);
      break;
#endif

    default:
      internal_error (&dtp->common, "Unsupported real kind during IO");
    }

  if (errno == EINVAL)
    {
      generate_error (&dtp->common, LIBERROR_READ_VALUE,
		      "Error during floating point read");
      next_record (dtp, 1);
      return 1;
    }

  return 0;
}


/* read_l()-- Read a logical value */

void
read_l (st_parameter_dt *dtp, const fnode *f, char *dest, int length)
{
  char *p;
  int w;

  w = f->u.w;

  p = read_block_form (dtp, &w);

  if (p == NULL)
    return;

  while (*p == ' ')
    {
      if (--w == 0)
	goto bad;
      p++;
    }

  if (*p == '.')
    {
      if (--w == 0)
	goto bad;
      p++;
    }

  switch (*p)
    {
    case 't':
    case 'T':
      set_integer (dest, (GFC_INTEGER_LARGEST) 1, length);
      break;
    case 'f':
    case 'F':
      set_integer (dest, (GFC_INTEGER_LARGEST) 0, length);
      break;
    default:
    bad:
      generate_error (&dtp->common, LIBERROR_READ_VALUE,
		      "Bad value on logical read");
      next_record (dtp, 1);
      break;
    }
}


static gfc_char4_t
read_utf8 (st_parameter_dt *dtp, int *nbytes) 
{
  static const uchar masks[6] = { 0x7F, 0x1F, 0x0F, 0x07, 0x02, 0x01 };
  static const uchar patns[6] = { 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };
  int i, nb, nread;
  gfc_char4_t c;
  char *s;

  *nbytes = 1;

  s = read_block_form (dtp, nbytes);
  if (s == NULL)
    return 0;

  /* If this is a short read, just return.  */
  if (*nbytes == 0)
    return 0;

  c = (uchar) s[0];
  if (c < 0x80)
    return c;

  /* The number of leading 1-bits in the first byte indicates how many
     bytes follow.  */
  for (nb = 2; nb < 7; nb++)
    if ((c & ~masks[nb-1]) == patns[nb-1])
      goto found;
  goto invalid;
	
 found:
  c = (c & masks[nb-1]);
  nread = nb - 1;

  s = read_block_form (dtp, &nread);
  if (s == NULL)
    return 0;
  /* Decode the bytes read.  */
  for (i = 1; i < nb; i++)
    {
      gfc_char4_t n = *s++;

      if ((n & 0xC0) != 0x80)
	goto invalid;

      c = ((c << 6) + (n & 0x3F));
    }

  /* Make sure the shortest possible encoding was used.  */
  if (c <=      0x7F && nb > 1) goto invalid;
  if (c <=     0x7FF && nb > 2) goto invalid;
  if (c <=    0xFFFF && nb > 3) goto invalid;
  if (c <=  0x1FFFFF && nb > 4) goto invalid;
  if (c <= 0x3FFFFFF && nb > 5) goto invalid;

  /* Make sure the character is valid.  */
  if (c > 0x7FFFFFFF || (c >= 0xD800 && c <= 0xDFFF))
    goto invalid;

  return c;
      
 invalid:
  generate_error (&dtp->common, LIBERROR_READ_VALUE, "Invalid UTF-8 encoding");
  return (gfc_char4_t) '?';
}


static void
read_utf8_char1 (st_parameter_dt *dtp, char *p, int len, int width)
{
  gfc_char4_t c;
  char *dest;
  int nbytes;
  int i, j;

  len = (width < len) ? len : width;

  dest = (char *) p;

  /* Proceed with decoding one character at a time.  */
  for (j = 0; j < len; j++, dest++)
    {
      c = read_utf8 (dtp, &nbytes);

      /* Check for a short read and if so, break out.  */
      if (nbytes == 0)
	break;

      *dest = c > 255 ? '?' : (uchar) c;
    }

  /* If there was a short read, pad the remaining characters.  */
  for (i = j; i < len; i++)
    *dest++ = ' ';
  return;
}

static void
read_default_char1 (st_parameter_dt *dtp, char *p, int len, int width)
{
  char *s;
  int m, n;

  s = read_block_form (dtp, &width);
  
  if (s == NULL)
    return;
  if (width > len)
     s += (width - len);

  m = (width > len) ? len : width;
  memcpy (p, s, m);

  n = len - width;
  if (n > 0)
    memset (p + m, ' ', n);
}


static void
read_utf8_char4 (st_parameter_dt *dtp, void *p, int len, int width)
{
  gfc_char4_t *dest;
  int nbytes;
  int i, j;

  len = (width < len) ? len : width;

  dest = (gfc_char4_t *) p;

  /* Proceed with decoding one character at a time.  */
  for (j = 0; j < len; j++, dest++)
    {
      *dest = read_utf8 (dtp, &nbytes);

      /* Check for a short read and if so, break out.  */
      if (nbytes == 0)
	break;
    }

  /* If there was a short read, pad the remaining characters.  */
  for (i = j; i < len; i++)
    *dest++ = (gfc_char4_t) ' ';
  return;
}


static void
read_default_char4 (st_parameter_dt *dtp, char *p, int len, int width)
{
  char *s;
  gfc_char4_t *dest;
  int m, n;

  s = read_block_form (dtp, &width);
  
  if (s == NULL)
    return;
  if (width > len)
     s += (width - len);

  m = ((int) width > len) ? len : (int) width;
  
  dest = (gfc_char4_t *) p;
  
  for (n = 0; n < m; n++, dest++, s++)
    *dest = (unsigned char ) *s;

  for (n = 0; n < len - (int) width; n++, dest++)
    *dest = (unsigned char) ' ';
}


/* read_a()-- Read a character record into a KIND=1 character destination,
   processing UTF-8 encoding if necessary.  */

void
read_a (st_parameter_dt *dtp, const fnode *f, char *p, int length)
{
  int wi;
  int w;

  wi = f->u.w;
  if (wi == -1) /* '(A)' edit descriptor  */
    wi = length;
  w = wi;

  /* Read in w characters, treating comma as not a separator.  */
  dtp->u.p.sf_read_comma = 0;

  if (dtp->u.p.current_unit->flags.encoding == ENCODING_UTF8)
    read_utf8_char1 (dtp, p, length, w);
  else
    read_default_char1 (dtp, p, length, w);

  dtp->u.p.sf_read_comma =
    dtp->u.p.current_unit->decimal_status == DECIMAL_COMMA ? 0 : 1;
}


/* read_a_char4()-- Read a character record into a KIND=4 character destination,
   processing UTF-8 encoding if necessary.  */

void
read_a_char4 (st_parameter_dt *dtp, const fnode *f, char *p, int length)
{
  int w;

  w = f->u.w;
  if (w == -1) /* '(A)' edit descriptor  */
    w = length;

  /* Read in w characters, treating comma as not a separator.  */
  dtp->u.p.sf_read_comma = 0;

  if (dtp->u.p.current_unit->flags.encoding == ENCODING_UTF8)
    read_utf8_char4 (dtp, p, length, w);
  else
    read_default_char4 (dtp, p, length, w);
  
  dtp->u.p.sf_read_comma =
    dtp->u.p.current_unit->decimal_status == DECIMAL_COMMA ? 0 : 1;
}

/* eat_leading_spaces()-- Given a character pointer and a width,
 * ignore the leading spaces.  */

static char *
eat_leading_spaces (int *width, char *p)
{
  for (;;)
    {
      if (*width == 0 || *p != ' ')
	break;

      (*width)--;
      p++;
    }

  return p;
}


static char
next_char (st_parameter_dt *dtp, char **p, int *w)
{
  char c, *q;

  if (*w == 0)
    return '\0';

  q = *p;
  c = *q++;
  *p = q;

  (*w)--;

  if (c != ' ')
    return c;
  if (dtp->u.p.blank_status != BLANK_UNSPECIFIED)
    return ' ';  /* return a blank to signal a null */ 

  /* At this point, the rest of the field has to be trailing blanks */

  while (*w > 0)
    {
      if (*q++ != ' ')
	return '?';
      (*w)--;
    }

  *p = q;
  return '\0';
}


/* read_decimal()-- Read a decimal integer value.  The values here are
 * signed values. */

void
read_decimal (st_parameter_dt *dtp, const fnode *f, char *dest, int length)
{
  GFC_UINTEGER_LARGEST value, maxv, maxv_10;
  GFC_INTEGER_LARGEST v;
  int w, negative; 
  char c, *p;

  w = f->u.w;

  p = read_block_form (dtp, &w);

  if (p == NULL)
    return;

  p = eat_leading_spaces (&w, p);
  if (w == 0)
    {
      set_integer (dest, (GFC_INTEGER_LARGEST) 0, length);
      return;
    }

  maxv = max_value (length, 1);
  maxv_10 = maxv / 10;

  negative = 0;
  value = 0;

  switch (*p)
    {
    case '-':
      negative = 1;
      /* Fall through */

    case '+':
      p++;
      if (--w == 0)
	goto bad;
      /* Fall through */

    default:
      break;
    }

  /* At this point we have a digit-string */
  value = 0;

  for (;;)
    {
      c = next_char (dtp, &p, &w);
      if (c == '\0')
	break;
	
      if (c == ' ')
        {
	  if (dtp->u.p.blank_status == BLANK_NULL) continue;
	  if (dtp->u.p.blank_status == BLANK_ZERO) c = '0';
        }
        
      if (c < '0' || c > '9')
	goto bad;

      if (value > maxv_10 && compile_options.range_check == 1)
	goto overflow;

      c -= '0';
      value = 10 * value;

      if (value > maxv - c && compile_options.range_check == 1)
	goto overflow;
      value += c;
    }

  v = value;
  if (negative)
    v = -v;

  set_integer (dest, v, length);
  return;

 bad:
  generate_error (&dtp->common, LIBERROR_READ_VALUE,
		  "Bad value during integer read");
  next_record (dtp, 1);
  return;

 overflow:
  generate_error (&dtp->common, LIBERROR_READ_OVERFLOW,
		  "Value overflowed during integer read");
  next_record (dtp, 1);

}


/* read_radix()-- This function reads values for non-decimal radixes.
 * The difference here is that we treat the values here as unsigned
 * values for the purposes of overflow.  If minus sign is present and
 * the top bit is set, the value will be incorrect. */

void
read_radix (st_parameter_dt *dtp, const fnode *f, char *dest, int length,
	    int radix)
{
  GFC_UINTEGER_LARGEST value, maxv, maxv_r;
  GFC_INTEGER_LARGEST v;
  int w, negative;
  char c, *p;

  w = f->u.w;

  p = read_block_form (dtp, &w);

  if (p == NULL)
    return;

  p = eat_leading_spaces (&w, p);
  if (w == 0)
    {
      set_integer (dest, (GFC_INTEGER_LARGEST) 0, length);
      return;
    }

  maxv = max_value (length, 0);
  maxv_r = maxv / radix;

  negative = 0;
  value = 0;

  switch (*p)
    {
    case '-':
      negative = 1;
      /* Fall through */

    case '+':
      p++;
      if (--w == 0)
	goto bad;
      /* Fall through */

    default:
      break;
    }

  /* At this point we have a digit-string */
  value = 0;

  for (;;)
    {
      c = next_char (dtp, &p, &w);
      if (c == '\0')
	break;
      if (c == ' ')
        {
	  if (dtp->u.p.blank_status == BLANK_NULL) continue;
	  if (dtp->u.p.blank_status == BLANK_ZERO) c = '0';
        }

      switch (radix)
	{
	case 2:
	  if (c < '0' || c > '1')
	    goto bad;
	  break;

	case 8:
	  if (c < '0' || c > '7')
	    goto bad;
	  break;

	case 16:
	  switch (c)
	    {
	    case '0':
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	    case '8':
	    case '9':
	      break;

	    case 'a':
	    case 'b':
	    case 'c':
	    case 'd':
	    case 'e':
	    case 'f':
	      c = c - 'a' + '9' + 1;
	      break;

	    case 'A':
	    case 'B':
	    case 'C':
	    case 'D':
	    case 'E':
	    case 'F':
	      c = c - 'A' + '9' + 1;
	      break;

	    default:
	      goto bad;
	    }

	  break;
	}

      if (value > maxv_r)
	goto overflow;

      c -= '0';
      value = radix * value;

      if (maxv - c < value)
	goto overflow;
      value += c;
    }

  v = value;
  if (negative)
    v = -v;

  set_integer (dest, v, length);
  return;

 bad:
  generate_error (&dtp->common, LIBERROR_READ_VALUE,
		  "Bad value during integer read");
  next_record (dtp, 1);
  return;

 overflow:
  generate_error (&dtp->common, LIBERROR_READ_OVERFLOW,
		  "Value overflowed during integer read");
  next_record (dtp, 1);

}


/* read_f()-- Read a floating point number with F-style editing, which
   is what all of the other floating point descriptors behave as.  The
   tricky part is that optional spaces are allowed after an E or D,
   and the implicit decimal point if a decimal point is not present in
   the input.  */

void
read_f (st_parameter_dt *dtp, const fnode *f, char *dest, int length)
{
  int w, seen_dp, exponent;
  int exponent_sign;
  const char *p;
  char *buffer;
  char *out;
  int seen_int_digit; /* Seen a digit before the decimal point?  */
  int seen_dec_digit; /* Seen a digit after the decimal point?  */

  seen_dp = 0;
  seen_int_digit = 0;
  seen_dec_digit = 0;
  exponent_sign = 1;
  exponent = 0;
  w = f->u.w;

  /* Read in the next block.  */
  p = read_block_form (dtp, &w);
  if (p == NULL)
    return;
  p = eat_leading_spaces (&w, (char*) p);
  if (w == 0)
    goto zero;

  /* In this buffer we're going to re-format the number cleanly to be parsed
     by convert_real in the end; this assures we're using strtod from the
     C library for parsing and thus probably get the best accuracy possible.
     This process may add a '+0.0' in front of the number as well as change the
     exponent because of an implicit decimal point or the like.  Thus allocating
     strlen ("+0.0e-1000") == 10 characters plus one for NUL more than the
     original buffer had should be enough.  */
  buffer = gfc_alloca (w + 11);
  out = buffer;

  /* Optional sign */
  if (*p == '-' || *p == '+')
    {
      if (*p == '-')
	*(out++) = '-';
      ++p;
      --w;
    }

  p = eat_leading_spaces (&w, (char*) p);
  if (w == 0)
    goto zero;

  /* Process the mantissa string.  */
  while (w > 0)
    {
      switch (*p)
	{
	case ',':
	  if (dtp->u.p.current_unit->decimal_status != DECIMAL_COMMA)
	    goto bad_float;
	  /* Fall through.  */
	case '.':
	  if (seen_dp)
	    goto bad_float;
	  if (!seen_int_digit)
	    *(out++) = '0';
	  *(out++) = '.';
	  seen_dp = 1;
	  break;

	case ' ':
	  if (dtp->u.p.blank_status == BLANK_ZERO)
	    {
	      *(out++) = '0';
	      goto found_digit;
	    }
	  else if (dtp->u.p.blank_status == BLANK_NULL)
	    break;
	  else
	    /* TODO: Should we check instead that there are only trailing
	       blanks here, as is done below for exponents?  */
	    goto done;
	  /* Fall through.  */
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  *(out++) = *p;
found_digit:
	  if (!seen_dp)
	    seen_int_digit = 1;
	  else
	    seen_dec_digit = 1;
	  break;

	case '-':
	case '+':
	  goto exponent;

	case 'e':
	case 'E':
	case 'd':
	case 'D':
	  ++p;
	  --w;
	  goto exponent;

	default:
	  goto bad_float;
	}

      ++p;
      --w;
    }
  
  /* No exponent has been seen, so we use the current scale factor.  */
  exponent = - dtp->u.p.scale_factor;
  goto done;

  /* At this point the start of an exponent has been found.  */
exponent:
  p = eat_leading_spaces (&w, (char*) p);
  if (*p == '-' || *p == '+')
    {
      if (*p == '-')
	exponent_sign = -1;
      ++p;
      --w;
    }

  /* At this point a digit string is required.  We calculate the value
     of the exponent in order to take account of the scale factor and
     the d parameter before explict conversion takes place.  */

  if (w == 0)
    goto bad_float;

  if (dtp->u.p.blank_status == BLANK_UNSPECIFIED)
    {
      while (w > 0 && isdigit (*p))
	{
	  exponent *= 10;
	  exponent += *p - '0';
	  ++p;
	  --w;
	}
	
      /* Only allow trailing blanks.  */
      while (w > 0)
	{
	  if (*p != ' ')
	    goto bad_float;
	  ++p;
	  --w;
	}
    }    
  else  /* BZ or BN status is enabled.  */
    {
      while (w > 0)
	{
	  if (*p == ' ')
	    {
	      if (dtp->u.p.blank_status == BLANK_ZERO)
		exponent *= 10;
	      else
		assert (dtp->u.p.blank_status == BLANK_NULL);
	    }
	  else if (!isdigit (*p))
	    goto bad_float;
	  else
	    {
	      exponent *= 10;
	      exponent += *p - '0';
	    }

	  ++p;
	  --w;
	}
    }

  exponent *= exponent_sign;

done:
  /* Use the precision specified in the format if no decimal point has been
     seen.  */
  if (!seen_dp)
    exponent -= f->u.real.d;

  /* Output a trailing '0' after decimal point if not yet found.  */
  if (seen_dp && !seen_dec_digit)
    *(out++) = '0';

  /* Print out the exponent to finish the reformatted number.  Maximum 4
     digits for the exponent.  */
  if (exponent != 0)
    {
      int dig;

      *(out++) = 'e';
      if (exponent < 0)
	{
	  *(out++) = '-';
	  exponent = - exponent;
	}

      assert (exponent < 10000);
      for (dig = 3; dig >= 0; --dig)
	{
	  out[dig] = (char) ('0' + exponent % 10);
	  exponent /= 10;
	}
      out += 4;
    }
  *(out++) = '\0';

  /* Do the actual conversion.  */
  convert_real (dtp, dest, buffer, length);

  return;

  /* The value read is zero.  */
zero:
  switch (length)
    {
      case 4:
	*((GFC_REAL_4 *) dest) = 0.0;
	break;

      case 8:
	*((GFC_REAL_8 *) dest) = 0.0;
	break;

#ifdef HAVE_GFC_REAL_10
      case 10:
	*((GFC_REAL_10 *) dest) = 0.0;
	break;
#endif

#ifdef HAVE_GFC_REAL_16
      case 16:
	*((GFC_REAL_16 *) dest) = 0.0;
	break;
#endif

      default:
	internal_error (&dtp->common, "Unsupported real kind during IO");
    }
  return;

bad_float:
  generate_error (&dtp->common, LIBERROR_READ_VALUE,
		  "Bad value during floating point read");
  next_record (dtp, 1);
  return;
}


/* read_x()-- Deal with the X/TR descriptor.  We just read some data
 * and never look at it. */

void
read_x (st_parameter_dt *dtp, int n)
{
  int length;
  char *p, q;

  if ((dtp->u.p.current_unit->pad_status == PAD_NO || is_internal_unit (dtp))
       && dtp->u.p.current_unit->bytes_left < n)
    n = dtp->u.p.current_unit->bytes_left;
    
  if (n == 0)
    return;

  length = n;

  if (is_internal_unit (dtp))
    {
      p = mem_alloc_r (dtp->u.p.current_unit->s, &length);
      if (unlikely (length < n))
	n = length;
      goto done;
    }

  p = fbuf_read (dtp->u.p.current_unit, &length);
  if (p == NULL)
    {
      hit_eof (dtp);
      return;
    }
  
  if (length == 0 && dtp->u.p.item_count == 1)
    {
      if (dtp->u.p.current_unit->pad_status == PAD_NO)
	{
	  hit_eof (dtp);
	  return;
	}
      else
	return;
    }

  n = 0;
  while (n < length)
    {
      q = *p;
      if (q == '\n' || q == '\r')
	{
	  /* Unexpected end of line. Set the position.  */
	  fbuf_seek (dtp->u.p.current_unit, n + 1 ,SEEK_CUR);
	  dtp->u.p.sf_seen_eor = 1;

	  /* If we encounter a CR, it might be a CRLF.  */
	  if (q == '\r') /* Probably a CRLF */
	    {
	      /* See if there is an LF. Use fbuf_read rather then fbuf_getc so
		 the position is not advanced unless it really is an LF.  */
	      int readlen = 1;
	      p = fbuf_read (dtp->u.p.current_unit, &readlen);
	      if (*p == '\n' && readlen == 1)
	        {
		  dtp->u.p.sf_seen_eor = 2;
		  fbuf_seek (dtp->u.p.current_unit, 1 ,SEEK_CUR);
		}
	    }
	  goto done;
	}
      n++;
      p++;
    } 

  fbuf_seek (dtp->u.p.current_unit, n, SEEK_CUR);
  
 done:
  if ((dtp->common.flags & IOPARM_DT_HAS_SIZE) != 0)
    dtp->u.p.size_used += (GFC_IO_INT) n;
  dtp->u.p.current_unit->bytes_left -= n;
  dtp->u.p.current_unit->strm_pos += (gfc_offset) n;
}

