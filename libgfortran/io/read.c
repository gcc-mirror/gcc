/* Copyright (C) 2002-2003 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include "config.h"
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include "libgfortran.h"
#include "io.h"

/* read.c -- Deal with formatted reads */

/* set_integer()-- All of the integer assignments come here to
 * actually place the value into memory.  */

void
set_integer (void *dest, int64_t value, int length)
{

  switch (length)
    {
    case 8:
      *((int64_t *) dest) = value;
      break;
    case 4:
      *((int32_t *) dest) = value;
      break;
    case 2:
      *((int16_t *) dest) = value;
      break;
    case 1:
      *((int8_t *) dest) = value;
      break;
    default:
      internal_error ("Bad integer kind");
    }
}


/* max_value()-- Given a length (kind), return the maximum signed or
 * unsigned value */

uint64_t
max_value (int length, int signed_flag)
{
  uint64_t value;

  switch (length)
    {
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
      internal_error ("Bad integer kind");
    }

  return value;
}


/* convert_real()-- Convert a character representation of a floating
 * point number to the machine number.  Returns nonzero if there is a
 * range problem during conversion.  TODO: handle not-a-numbers and
 * infinities.  Handling of kind 4 is probably wrong because of double
 * rounding. */

int
convert_real (void *dest, const char *buffer, int length)
{

  errno = 0;

  switch (length)
    {
    case 4:
      *((float *) dest) = (float) strtod (buffer, NULL);
      break;
    case 8:
      *((double *) dest) = strtod (buffer, NULL);
      break;
    default:
      internal_error ("Bad real number kind");
    }

  if (errno != 0)
    {
      generate_error (ERROR_READ_VALUE,
		      "Range error during floating point read");
      return 1;
    }

  return 0;
}

static int
convert_precision_real (void *dest, int sign,
                       char *buffer, int length, int exponent)
{
  int w, new_dp_pos, i, slen, k, dp;
  char * p, c;
  double fval;
  float tf;

  fval =0.0;
  tf = 0.0;
  dp = 0;
  new_dp_pos = 0;

  slen = strlen (buffer);
  w = slen;
  p = buffer;

/*  for (i = w - 1; i > 0; i --)
    {
       if (buffer[i] == '0' || buffer[i] == 0)
         buffer[i] = 0;
       else
         break;
    }
*/
  for (i = 0; i < w; i++)
    {
       if (buffer[i] == '.')
         break;
    }

  new_dp_pos = i;
  new_dp_pos += exponent;

  while (w > 0)
    {
      c = *p;
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
          fval = fval * 10.0 + c - '0';
          p++;
          w--;
          break;

        case '.':
          dp = 1;
          p++;
          w--;
          break;

       default:
          p++;
          w--;
          break;
     }
  }

  if (sign)
    fval = - fval;

  i = new_dp_pos - slen + dp;
  k = abs(i);
  tf = 1.0;

  while (k > 0)
    {
       tf *= 10.0 ;
       k -- ;
    }

  if (fval != 0.0)
    {
       if (i < 0)
         {
           fval = fval / tf;
         }
        else
         {
           fval = fval * tf;
         }
    }

  switch (length)
    {
    case 4:
      *((float *) dest) = (float)fval;
      break;
    case 8:
      *((double *) dest) = fval;
      break;
    default:
      internal_error ("Bad real number kind");
    }

  return 0;
}


/* read_l()-- Read a logical value */

void
read_l (fnode * f, char *dest, int length)
{
  char *p;
  int w;

  w = f->u.w;
  p = read_block (&w);
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
      set_integer (dest, 1, length);
      break;
    case 'f':
    case 'F':
      set_integer (dest, 0, length);
      break;
    default:
    bad:
      generate_error (ERROR_READ_VALUE, "Bad value on logical read");
      break;
    }
}


/* read_a()-- Read a character record.  This one is pretty easy. */

void
read_a (fnode * f, char *p, int length)
{
  char *source;
  int w, m, n;

  w = f->u.w;
  if (w == -1) /* '(A)' edit descriptor  */
    w = length;

  source = read_block (&w);
  if (source == NULL)
    return;
  if (w > length)
     source += (w - length);

  m = (w > length) ? length : w;
  memcpy (p, source, m);

  n = length - w;
  if (n > 0)
    memset (p + m, ' ', n);
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
next_char (char **p, int *w)
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
  if (g.blank_status == BLANK_ZERO)
    return '0';

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
read_decimal (fnode * f, char *dest, int length)
{
  unsigned value, maxv, maxv_10;
  int v, w, negative;
  char c, *p;

  w = f->u.w;
  p = read_block (&w);
  if (p == NULL)
    return;

  p = eat_leading_spaces (&w, p);
  if (w == 0)
    {
      set_integer (dest, 0, length);
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
      c = next_char (&p, &w);
      if (c == '\0')
	break;

      if (c < '0' || c > '9')
	goto bad;

      if (value > maxv_10)
	goto overflow;

      c -= '0';
      value = 10 * value;

      if (value > maxv - c)
	goto overflow;
      value += c;
    }

  v = (signed int) value;
  if (negative)
    v = -v;

  set_integer (dest, v, length);
  return;

bad:
  generate_error (ERROR_READ_VALUE, "Bad value during integer read");
  return;

overflow:
  generate_error (ERROR_READ_OVERFLOW,
		  "Value overflowed during integer read");
  return;
}


/* read_radix()-- This function reads values for non-decimal radixes.
 * The difference here is that we treat the values here as unsigned
 * values for the purposes of overflow.  If minus sign is present and
 * the top bit is set, the value will be incorrect. */

void
read_radix (fnode * f, char *dest, int length, int radix)
{
  unsigned value, maxv, maxv_r;
  int v, w, negative;
  char c, *p;

  w = f->u.w;
  p = read_block (&w);
  if (p == NULL)
    return;

  p = eat_leading_spaces (&w, p);
  if (w == 0)
    {
      set_integer (dest, 0, length);
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
      c = next_char (&p, &w);
      if (c == '\0')
	break;

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

  v = (signed int) value;
  if (negative)
    v = -v;

  set_integer (dest, v, length);
  return;

bad:
  generate_error (ERROR_READ_VALUE, "Bad value during integer read");
  return;

overflow:
  generate_error (ERROR_READ_OVERFLOW,
		  "Value overflowed during integer read");
  return;
}


/* read_f()-- Read a floating point number with F-style editing, which
 * is what all of the other floating point descriptors behave as.  The
 * tricky part is that optional spaces are allowed after an E or D,
 * and the implicit decimal point if a decimal point is not present in
 * the input. */

void
read_f (fnode * f, char *dest, int length)
{
  int w, seen_dp, exponent;
  int exponent_sign, val_sign;
  char *p, *buffer, *n;

  val_sign = 0;
  seen_dp = 0;
  w = f->u.w;
  p = read_block (&w);
  if (p == NULL)
    return;

  p = eat_leading_spaces (&w, p);
  if (w == 0)
    {
      switch (length)
	{
	case 4:
	  *((float *) dest) = 0.0;
	  break;

	case 8:
	  *((double *) dest) = 0.0;
	  break;
	}

      return;
    }

  if (w + 2 < SCRATCH_SIZE)
    buffer = scratch;
  else
    buffer = get_mem (w + 2);

  memset(buffer, 0, w + 2);

  n = buffer;

  /* Optional sign */

  if (*p == '-' || *p == '+')
    {
      if (*p == '-')
        val_sign = 1;
      p++;

      if (--w == 0)
	goto bad_float;
    }

  exponent_sign = 1;

  /* A digit (or a '.') is required at this point */

  if (!isdigit (*p) && *p != '.')
    goto bad_float;

  while (w > 0)
    {
      switch (*p)
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
	  *n++ = *p++;
	  w--;
	  break;

	case '.':
	  if (seen_dp)
	    goto bad_float;
	  seen_dp = 1;

	  *n++ = *p++;
	  w--;
	  break;

	case ' ':
	  if (g.blank_status == BLANK_ZERO)
	    *n++ = '0';
	  p++;
	  w--;
	  break;

	case '-':
	  exponent_sign = -1;
	  /* Fall through */

	case '+':
	  p++;
	  w--;
	  goto exp2;

	case 'd':
	case 'e':
	case 'D':
	case 'E':
	  p++;
	  w--;
	  goto exp1;

	default:
	  goto bad_float;
	}
    }

/* No exponent has been seen, so we use the current scale factor */

  exponent = -g.scale_factor;
  goto done;

bad_float:
  generate_error (ERROR_READ_VALUE, "Bad value during floating point read");
  if (buffer != scratch)
     free_mem (buffer);
  return;

/* At this point the start of an exponent has been found */

exp1:
  while (w > 0 && *p == ' ')
    {
      w--;
      p++;
    }

  switch (*p)
    {
    case '-':
      exponent_sign = -1;
      /* Fall through */

    case '+':
      p++;
      w--;
      break;
    }

  if (w == 0)
    goto bad_float;

/* At this point a digit string is required.  We calculate the value
 * of the exponent in order to take account of the scale factor and
 * the d parameter before explict conversion takes place. */

exp2:
  if (!isdigit (*p))
    goto bad_float;

  exponent = *p - '0';
  p++;
  w--;

  while (w > 0 && isdigit (*p))
    {
      exponent = 10 * exponent + *p - '0';
      if (exponent > 999999)
	goto bad_float;

      p++;
      w--;
    }

  /* Only allow trailing blanks */

  while (w > 0)
    {
      if (*p != ' ')
	goto bad_float;
      p++;
      w--;
    }

  exponent = exponent * exponent_sign;

done:
  if (!seen_dp)
    exponent -= f->u.real.d;

  /* The number is syntactically correct and ready for conversion.
   * The only thing that can go wrong at this point is overflow or
   * underflow. */

  convert_precision_real (dest, val_sign, buffer, length, exponent);

  if (buffer != scratch)
     free_mem (buffer);

  return;
}


/* read_x()-- Deal with the X/TR descriptor.  We just read some data
 * and never look at it. */

void
read_x (fnode * f)
{
  int n;

  n = f->u.n;
  read_block (&n);
}
