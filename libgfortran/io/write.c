/* Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Andy Vaught
   Namelist output contibuted by Paul Thomas

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <float.h>
#include <stdio.h>
#include <stdlib.h>
#include "libgfortran.h"
#include "io.h"

#define star_fill(p, n) memset(p, '*', n)


typedef enum
{ SIGN_NONE, SIGN_MINUS, SIGN_PLUS }
sign_t;


void
write_a (st_parameter_dt *dtp, const fnode *f, const char *source, int len)
{
  int wlen;
  char *p;

  wlen = f->u.string.length < 0 ? len : f->u.string.length;

  p = write_block (dtp, wlen);
  if (p == NULL)
    return;

  if (wlen < len)
    memcpy (p, source, wlen);
  else
    {
      memset (p, ' ', wlen - len);
      memcpy (p + wlen - len, source, len);
    }
}

static GFC_INTEGER_LARGEST
extract_int (const void *p, int len)
{
  GFC_INTEGER_LARGEST i = 0;

  if (p == NULL)
    return i;

  switch (len)
    {
    case 1:
      {
	GFC_INTEGER_1 tmp;
	memcpy ((void *) &tmp, p, len);
	i = tmp;
      }
      break;
    case 2:
      {
	GFC_INTEGER_2 tmp;
	memcpy ((void *) &tmp, p, len);
	i = tmp;
      }
      break;
    case 4:
      {
	GFC_INTEGER_4 tmp;
	memcpy ((void *) &tmp, p, len);
	i = tmp;
      }
      break;
    case 8:
      {
	GFC_INTEGER_8 tmp;
	memcpy ((void *) &tmp, p, len);
	i = tmp;
      }
      break;
#ifdef HAVE_GFC_INTEGER_16
    case 16:
      {
	GFC_INTEGER_16 tmp;
	memcpy ((void *) &tmp, p, len);
	i = tmp;
      }
      break;
#endif
    default:
      internal_error (NULL, "bad integer kind");
    }

  return i;
}

static GFC_UINTEGER_LARGEST
extract_uint (const void *p, int len)
{
  GFC_UINTEGER_LARGEST i = 0;

  if (p == NULL)
    return i;

  switch (len)
    {
    case 1:
      {
	GFC_INTEGER_1 tmp;
	memcpy ((void *) &tmp, p, len);
	i = (GFC_UINTEGER_1) tmp;
      }
      break;
    case 2:
      {
	GFC_INTEGER_2 tmp;
	memcpy ((void *) &tmp, p, len);
	i = (GFC_UINTEGER_2) tmp;
      }
      break;
    case 4:
      {
	GFC_INTEGER_4 tmp;
	memcpy ((void *) &tmp, p, len);
	i = (GFC_UINTEGER_4) tmp;
      }
      break;
    case 8:
      {
	GFC_INTEGER_8 tmp;
	memcpy ((void *) &tmp, p, len);
	i = (GFC_UINTEGER_8) tmp;
      }
      break;
#ifdef HAVE_GFC_INTEGER_16
    case 16:
      {
	GFC_INTEGER_16 tmp;
	memcpy ((void *) &tmp, p, len);
	i = (GFC_UINTEGER_16) tmp;
      }
      break;
#endif
    default:
      internal_error (NULL, "bad integer kind");
    }

  return i;
}

static GFC_REAL_LARGEST
extract_real (const void *p, int len)
{
  GFC_REAL_LARGEST i = 0;
  switch (len)
    {
    case 4:
      {
	GFC_REAL_4 tmp;
	memcpy ((void *) &tmp, p, len);
	i = tmp;
      }
      break;
    case 8:
      {
	GFC_REAL_8 tmp;
	memcpy ((void *) &tmp, p, len);
	i = tmp;
      }
      break;
#ifdef HAVE_GFC_REAL_10
    case 10:
      {
	GFC_REAL_10 tmp;
	memcpy ((void *) &tmp, p, len);
	i = tmp;
      }
      break;
#endif
#ifdef HAVE_GFC_REAL_16
    case 16:
      {
	GFC_REAL_16 tmp;
	memcpy ((void *) &tmp, p, len);
	i = tmp;
      }
      break;
#endif
    default:
      internal_error (NULL, "bad real kind");
    }
  return i;
}


/* Given a flag that indicate if a value is negative or not, return a
   sign_t that gives the sign that we need to produce.  */

static sign_t
calculate_sign (st_parameter_dt *dtp, int negative_flag)
{
  sign_t s = SIGN_NONE;

  if (negative_flag)
    s = SIGN_MINUS;
  else
    switch (dtp->u.p.sign_status)
      {
      case SIGN_SP:
	s = SIGN_PLUS;
	break;
      case SIGN_SS:
	s = SIGN_NONE;
	break;
      case SIGN_S:
	s = options.optional_plus ? SIGN_PLUS : SIGN_NONE;
	break;
      }

  return s;
}


/* Returns the value of 10**d.  */

static GFC_REAL_LARGEST
calculate_exp (int d)
{
  int i;
  GFC_REAL_LARGEST r = 1.0;

  for (i = 0; i< (d >= 0 ? d : -d); i++)
    r *= 10;

  r = (d >= 0) ? r : 1.0 / r;

  return r;
}


/* Generate corresponding I/O format for FMT_G output.
   The rules to translate FMT_G to FMT_E or FMT_F from DEC fortran
   LRM (table 11-2, Chapter 11, "I/O Formatting", P11-25) is:

   Data Magnitude                              Equivalent Conversion
   0< m < 0.1-0.5*10**(-d-1)                   Ew.d[Ee]
   m = 0                                       F(w-n).(d-1), n' '
   0.1-0.5*10**(-d-1)<= m < 1-0.5*10**(-d)     F(w-n).d, n' '
   1-0.5*10**(-d)<= m < 10-0.5*10**(-d+1)      F(w-n).(d-1), n' '
   10-0.5*10**(-d+1)<= m < 100-0.5*10**(-d+2)  F(w-n).(d-2), n' '
   ................                           ..........
   10**(d-1)-0.5*10**(-1)<= m <10**d-0.5       F(w-n).0,n(' ')
   m >= 10**d-0.5                              Ew.d[Ee]

   notes: for Gw.d ,  n' ' means 4 blanks
          for Gw.dEe, n' ' means e+2 blanks  */

static fnode *
calculate_G_format (st_parameter_dt *dtp, const fnode *f,
		    GFC_REAL_LARGEST value, int *num_blank)
{
  int e = f->u.real.e;
  int d = f->u.real.d;
  int w = f->u.real.w;
  fnode *newf;
  GFC_REAL_LARGEST m, exp_d;
  int low, high, mid;
  int ubound, lbound;

  newf = get_mem (sizeof (fnode));

  /* Absolute value.  */
  m = (value > 0.0) ? value : -value;

  /* In case of the two data magnitude ranges,
     generate E editing, Ew.d[Ee].  */
  exp_d = calculate_exp (d);
  if ((m > 0.0 && m < 0.1 - 0.05 / exp_d) || (m >= exp_d - 0.5 ))
    {
      newf->format = FMT_E;
      newf->u.real.w = w;
      newf->u.real.d = d;
      newf->u.real.e = e;
      *num_blank = 0;
      return newf;
    }

  /* Use binary search to find the data magnitude range.  */
  mid = 0;
  low = 0;
  high = d + 1;
  lbound = 0;
  ubound = d + 1;

  while (low <= high)
    {
      GFC_REAL_LARGEST temp;
      mid = (low + high) / 2;

      /* 0.1 * 10**mid - 0.5 * 10**(mid-d-1)  */
      temp = 0.1 * calculate_exp (mid) - 0.5 * calculate_exp (mid - d - 1);

      if (m < temp)
        {
          ubound = mid;
          if (ubound == lbound + 1)
            break;
          high = mid - 1;
        }
      else if (m > temp)
        {
          lbound = mid;
          if (ubound == lbound + 1)
            {
              mid ++;
              break;
            }
          low = mid + 1;
        }
      else
        break;
    }

  /* Pad with blanks where the exponent would be.  */
  if (e < 0)
    *num_blank = 4;
  else
    *num_blank = e + 2;

  /* Generate the F editing. F(w-n).(-(mid-d-1)), n' '.  */
  newf->format = FMT_F;
  newf->u.real.w = f->u.real.w - *num_blank;

  /* Special case.  */
  if (m == 0.0)
    newf->u.real.d = d - 1;
  else
    newf->u.real.d = - (mid - d - 1);

  /* For F editing, the scale factor is ignored.  */
  dtp->u.p.scale_factor = 0;
  return newf;
}


/* Output a real number according to its format which is FMT_G free.  */

static void
output_float (st_parameter_dt *dtp, const fnode *f, GFC_REAL_LARGEST value)
{
  /* This must be large enough to accurately hold any value.  */
  char buffer[32];
  char *out;
  char *digits;
  int e;
  char expchar;
  format_token ft;
  int w;
  int d;
  int edigits;
  int ndigits;
  /* Number of digits before the decimal point.  */
  int nbefore;
  /* Number of zeros after the decimal point.  */
  int nzero;
  /* Number of digits after the decimal point.  */
  int nafter;
  /* Number of zeros after the decimal point, whatever the precision.  */
  int nzero_real;
  int leadzero;
  int nblanks;
  int i;
  sign_t sign;
  double abslog;

  ft = f->format;
  w = f->u.real.w;
  d = f->u.real.d;

  nzero_real = -1;


  /* We should always know the field width and precision.  */
  if (d < 0)
    internal_error (&dtp->common, "Unspecified precision");

  /* Use sprintf to print the number in the format +D.DDDDe+ddd
     For an N digit exponent, this gives us (32-6)-N digits after the
     decimal point, plus another one before the decimal point.  */
  sign = calculate_sign (dtp, value < 0.0);
  if (value < 0)
    value = -value;

  /* Printf always prints at least two exponent digits.  */
  if (value == 0)
    edigits = 2;
  else
    {
#if defined(HAVE_GFC_REAL_10) || defined(HAVE_GFC_REAL_16)
      abslog = fabs((double) log10l(value));
#else
      abslog = fabs(log10(value));
#endif
      if (abslog < 100)
	edigits = 2;
      else
        edigits = 1 + (int) log10(abslog);
    }

  if (ft == FMT_F || ft == FMT_EN
      || ((ft == FMT_D || ft == FMT_E) && dtp->u.p.scale_factor != 0))
    {
      /* Always convert at full precision to avoid double rounding.  */
      ndigits = 27 - edigits;
    }
  else
    {
      /* We know the number of digits, so can let printf do the rounding
	 for us.  */
      if (ft == FMT_ES)
	ndigits = d + 1;
      else
	ndigits = d;
      if (ndigits > 27 - edigits)
	ndigits = 27 - edigits;
    }

  /* #   The result will always contain a decimal point, even if no
   *     digits follow it
   *
   * -   The converted value is to be left adjusted on the field boundary
   *
   * +   A sign (+ or -) always be placed before a number
   *
   * 31  minimum field width
   *
   * *   (ndigits-1) is used as the precision
   *
   *   e format: [-]d.ddde±dd where there is one digit before the
   *   decimal-point character and the number of digits after it is
   *   equal to the precision. The exponent always contains at least two
   *   digits; if the value is zero, the exponent is 00.
   */
  sprintf (buffer, "%+-#31.*" GFC_REAL_LARGEST_FORMAT "e",
           ndigits - 1, value);

  /* Check the resulting string has punctuation in the correct places.  */
  if (buffer[2] != '.' || buffer[ndigits + 2] != 'e')
      internal_error (&dtp->common, "printf is broken");

  /* Read the exponent back in.  */
  e = atoi (&buffer[ndigits + 3]) + 1;

  /* Make sure zero comes out as 0.0e0.  */
  if (value == 0.0)
    e = 0;

  /* Normalize the fractional component.  */
  buffer[2] = buffer[1];
  digits = &buffer[2];

  /* Figure out where to place the decimal point.  */
  switch (ft)
    {
    case FMT_F:
      nbefore = e + dtp->u.p.scale_factor;
      if (nbefore < 0)
	{
	  nzero = -nbefore;
          nzero_real = nzero;
	  if (nzero > d)
	    nzero = d;
	  nafter = d - nzero;
	  nbefore = 0;
	}
      else
	{
	  nzero = 0;
	  nafter = d;
	}
      expchar = 0;
      break;

    case FMT_E:
    case FMT_D:
      i = dtp->u.p.scale_factor;
      if (value != 0.0)
	e -= i;
      if (i < 0)
	{
	  nbefore = 0;
	  nzero = -i;
	  nafter = d + i;
	}
      else if (i > 0)
	{
	  nbefore = i;
	  nzero = 0;
	  nafter = (d - i) + 1;
	}
      else /* i == 0 */
	{
	  nbefore = 0;
	  nzero = 0;
	  nafter = d;
	}

      if (ft == FMT_E)
	expchar = 'E';
      else
	expchar = 'D';
      break;

    case FMT_EN:
      /* The exponent must be a multiple of three, with 1-3 digits before
	 the decimal point.  */
      if (value != 0.0)
        e--;
      if (e >= 0)
	nbefore = e % 3;
      else
	{
	  nbefore = (-e) % 3;
	  if (nbefore != 0)
	    nbefore = 3 - nbefore;
	}
      e -= nbefore;
      nbefore++;
      nzero = 0;
      nafter = d;
      expchar = 'E';
      break;

    case FMT_ES:
      if (value != 0.0)
        e--;
      nbefore = 1;
      nzero = 0;
      nafter = d;
      expchar = 'E';
      break;

    default:
      /* Should never happen.  */
      internal_error (&dtp->common, "Unexpected format token");
    }

  /* Round the value.  */
  if (nbefore + nafter == 0)
    {
      ndigits = 0;
      if (nzero_real == d && digits[0] >= '5')
        {
          /* We rounded to zero but shouldn't have */
          nzero--;
          nafter = 1;
          digits[0] = '1';
          ndigits = 1;
        }
    }
  else if (nbefore + nafter < ndigits)
    {
      ndigits = nbefore + nafter;
      i = ndigits;
      if (digits[i] >= '5')
	{
	  /* Propagate the carry.  */
	  for (i--; i >= 0; i--)
	    {
	      if (digits[i] != '9')
		{
		  digits[i]++;
		  break;
		}
	      digits[i] = '0';
	    }

	  if (i < 0)
	    {
	      /* The carry overflowed.  Fortunately we have some spare space
		 at the start of the buffer.  We may discard some digits, but
		 this is ok because we already know they are zero.  */
	      digits--;
	      digits[0] = '1';
	      if (ft == FMT_F)
		{
		  if (nzero > 0)
		    {
		      nzero--;
		      nafter++;
		    }
		  else
		    nbefore++;
		}
	      else if (ft == FMT_EN)
		{
		  nbefore++;
		  if (nbefore == 4)
		    {
		      nbefore = 1;
		      e += 3;
		    }
		}
	      else
		e++;
	    }
	}
    }

  /* Calculate the format of the exponent field.  */
  if (expchar)
    {
      edigits = 1;
      for (i = abs (e); i >= 10; i /= 10)
	edigits++;

      if (f->u.real.e < 0)
	{
	  /* Width not specified.  Must be no more than 3 digits.  */
	  if (e > 999 || e < -999)
	    edigits = -1;
	  else
	    {
	      edigits = 4;
	      if (e > 99 || e < -99)
		expchar = ' ';
	    }
	}
      else
	{
	  /* Exponent width specified, check it is wide enough.  */
	  if (edigits > f->u.real.e)
	    edigits = -1;
	  else
	    edigits = f->u.real.e + 2;
	}
    }
  else
    edigits = 0;

  /* Pick a field size if none was specified.  */
  if (w <= 0)
    w = nbefore + nzero + nafter + (sign != SIGN_NONE ? 2 : 1);

  /* Create the ouput buffer.  */
  out = write_block (dtp, w);
  if (out == NULL)
    return;

  /* Zero values always output as positive, even if the value was negative
     before rounding.  */
  for (i = 0; i < ndigits; i++)
    {
      if (digits[i] != '0')
	break;
    }
  if (i == ndigits)
    sign = calculate_sign (dtp, 0);

  /* Work out how much padding is needed.  */
  nblanks = w - (nbefore + nzero + nafter + edigits + 1);
  if (sign != SIGN_NONE)
    nblanks--;

  /* Check the value fits in the specified field width.  */
  if (nblanks < 0 || edigits == -1)
    {
      star_fill (out, w);
      return;
    }

  /* See if we have space for a zero before the decimal point.  */
  if (nbefore == 0 && nblanks > 0)
    {
      leadzero = 1;
      nblanks--;
    }
  else
    leadzero = 0;

  /* Pad to full field width.  */


  if ( ( nblanks > 0 ) && !dtp->u.p.no_leading_blank)
    {
      memset (out, ' ', nblanks);
      out += nblanks;
    }

  /* Output the initial sign (if any).  */
  if (sign == SIGN_PLUS)
    *(out++) = '+';
  else if (sign == SIGN_MINUS)
    *(out++) = '-';

  /* Output an optional leading zero.  */
  if (leadzero)
    *(out++) = '0';

  /* Output the part before the decimal point, padding with zeros.  */
  if (nbefore > 0)
    {
      if (nbefore > ndigits)
	i = ndigits;
      else
	i = nbefore;

      memcpy (out, digits, i);
      while (i < nbefore)
	out[i++] = '0';

      digits += i;
      ndigits -= i;
      out += nbefore;
    }
  /* Output the decimal point.  */
  *(out++) = '.';

  /* Output leading zeros after the decimal point.  */
  if (nzero > 0)
    {
      for (i = 0; i < nzero; i++)
	*(out++) = '0';
    }

  /* Output digits after the decimal point, padding with zeros.  */
  if (nafter > 0)
    {
      if (nafter > ndigits)
	i = ndigits;
      else
	i = nafter;

      memcpy (out, digits, i);
      while (i < nafter)
	out[i++] = '0';

      digits += i;
      ndigits -= i;
      out += nafter;
    }

  /* Output the exponent.  */
  if (expchar)
    {
      if (expchar != ' ')
	{
	  *(out++) = expchar;
	  edigits--;
	}
#if HAVE_SNPRINTF
      snprintf (buffer, 32, "%+0*d", edigits, e);
#else
      sprintf (buffer, "%+0*d", edigits, e);
#endif
      memcpy (out, buffer, edigits);
    }

  if (dtp->u.p.no_leading_blank)
    {
      out += edigits;
      memset( out , ' ' , nblanks );
      dtp->u.p.no_leading_blank = 0;
    }
}


void
write_l (st_parameter_dt *dtp, const fnode *f, char *source, int len)
{
  char *p;
  GFC_INTEGER_LARGEST n;

  p = write_block (dtp, f->u.w);
  if (p == NULL)
    return;

  memset (p, ' ', f->u.w - 1);
  n = extract_int (source, len);
  p[f->u.w - 1] = (n) ? 'T' : 'F';
}

/* Output a real number according to its format.  */

static void
write_float (st_parameter_dt *dtp, const fnode *f, const char *source, int len)
{
  GFC_REAL_LARGEST n;
  int nb =0, res, save_scale_factor;
  char * p, fin;
  fnode *f2 = NULL;

  n = extract_real (source, len);

  if (f->format != FMT_B && f->format != FMT_O && f->format != FMT_Z)
    {
      res = isfinite (n); 
      if (res == 0)
	{
	  nb =  f->u.real.w;
	  
	  /* If the field width is zero, the processor must select a width 
	     not zero.  4 is chosen to allow output of '-Inf' or '+Inf' */
	     
	  if (nb == 0) nb = 4;
	  p = write_block (dtp, nb);
          if (p == NULL)
            return;
	  if (nb < 3)
	    {
	      memset (p, '*',nb);
	      return;
	    }

	  memset(p, ' ', nb);
	  res = !isnan (n);
	  if (res != 0)
	    {
	      if (signbit(n))
	        {
	        
	          /* If the sign is negative and the width is 3, there is
	             insufficient room to output '-Inf', so output asterisks */
	             
	          if (nb == 3)
	            {
	              memset (p, '*',nb);
	              return;
	            }
	            
	          /* The negative sign is mandatory */
	            
	          fin = '-';
		}    
	      else
	      
	          /* The positive sign is optional, but we output it for
	             consistency */
	             
		  fin = '+';

	      if (nb > 8)
	      
	        /* We have room, so output 'Infinity' */
	        
		memcpy(p + nb - 8, "Infinity", 8);
	      else
	      
	        /* For the case of width equals 8, there is not enough room
	           for the sign and 'Infinity' so we go with 'Inf' */
	            
		memcpy(p + nb - 3, "Inf", 3);
	      if (nb < 9 && nb > 3)
		p[nb - 4] = fin;  /* Put the sign in front of Inf */
	      else if (nb > 8)
		p[nb - 9] = fin;  /* Put the sign in front of Infinity */
	    }
	  else
	    memcpy(p + nb - 3, "NaN", 3);
	  return;
	}
    }

  if (f->format != FMT_G)
    output_float (dtp, f, n);
  else
    {
      save_scale_factor = dtp->u.p.scale_factor;
      f2 = calculate_G_format (dtp, f, n, &nb);
      output_float (dtp, f2, n);
      dtp->u.p.scale_factor = save_scale_factor;
      if (f2 != NULL)
        free_mem(f2);

      if (nb > 0)
        {
	  p = write_block (dtp, nb);
          if (p == NULL)
            return;
          memset (p, ' ', nb);
        }
    }
}


static void
write_int (st_parameter_dt *dtp, const fnode *f, const char *source, int len,
           const char *(*conv) (GFC_UINTEGER_LARGEST, char *, size_t))
{
  GFC_UINTEGER_LARGEST n = 0;
  int w, m, digits, nzero, nblank;
  char *p;
  const char *q;
  char itoa_buf[GFC_BTOA_BUF_SIZE];

  w = f->u.integer.w;
  m = f->u.integer.m;

  n = extract_uint (source, len);

  /* Special case:  */

  if (m == 0 && n == 0)
    {
      if (w == 0)
        w = 1;

      p = write_block (dtp, w);
      if (p == NULL)
        return;

      memset (p, ' ', w);
      goto done;
    }

  q = conv (n, itoa_buf, sizeof (itoa_buf));
  digits = strlen (q);

  /* Select a width if none was specified.  The idea here is to always
     print something.  */

  if (w == 0)
    w = ((digits < m) ? m : digits);

  p = write_block (dtp, w);
  if (p == NULL)
    return;

  nzero = 0;
  if (digits < m)
    nzero = m - digits;

  /* See if things will work.  */

  nblank = w - (nzero + digits);

  if (nblank < 0)
    {
      star_fill (p, w);
      goto done;
    }


  if (!dtp->u.p.no_leading_blank)
    {
      memset (p, ' ', nblank);
      p += nblank;
      memset (p, '0', nzero);
      p += nzero;
      memcpy (p, q, digits);
    }
  else
    {
      memset (p, '0', nzero);
      p += nzero;
      memcpy (p, q, digits);
      p += digits;
      memset (p, ' ', nblank);
      dtp->u.p.no_leading_blank = 0;
    }

 done:
  return;
}

static void
write_decimal (st_parameter_dt *dtp, const fnode *f, const char *source,
	       int len,
               const char *(*conv) (GFC_INTEGER_LARGEST, char *, size_t))
{
  GFC_INTEGER_LARGEST n = 0;
  int w, m, digits, nsign, nzero, nblank;
  char *p;
  const char *q;
  sign_t sign;
  char itoa_buf[GFC_BTOA_BUF_SIZE];

  w = f->u.integer.w;
  m = f->u.integer.m;

  n = extract_int (source, len);

  /* Special case:  */

  if (m == 0 && n == 0)
    {
      if (w == 0)
        w = 1;

      p = write_block (dtp, w);
      if (p == NULL)
        return;

      memset (p, ' ', w);
      goto done;
    }

  sign = calculate_sign (dtp, n < 0);
  if (n < 0)
    n = -n;

  nsign = sign == SIGN_NONE ? 0 : 1;
  q = conv (n, itoa_buf, sizeof (itoa_buf));

  digits = strlen (q);

  /* Select a width if none was specified.  The idea here is to always
     print something.  */

  if (w == 0)
    w = ((digits < m) ? m : digits) + nsign;

  p = write_block (dtp, w);
  if (p == NULL)
    return;

  nzero = 0;
  if (digits < m)
    nzero = m - digits;

  /* See if things will work.  */

  nblank = w - (nsign + nzero + digits);

  if (nblank < 0)
    {
      star_fill (p, w);
      goto done;
    }

  memset (p, ' ', nblank);
  p += nblank;

  switch (sign)
    {
    case SIGN_PLUS:
      *p++ = '+';
      break;
    case SIGN_MINUS:
      *p++ = '-';
      break;
    case SIGN_NONE:
      break;
    }

  memset (p, '0', nzero);
  p += nzero;

  memcpy (p, q, digits);

 done:
  return;
}


/* Convert unsigned octal to ascii.  */

static const char *
otoa (GFC_UINTEGER_LARGEST n, char *buffer, size_t len)
{
  char *p;

  assert (len >= GFC_OTOA_BUF_SIZE);

  if (n == 0)
    return "0";

  p = buffer + GFC_OTOA_BUF_SIZE - 1;
  *p = '\0';

  while (n != 0)
    {
      *--p = '0' + (n & 7);
      n >>= 3;
    }

  return p;
}


/* Convert unsigned binary to ascii.  */

static const char *
btoa (GFC_UINTEGER_LARGEST n, char *buffer, size_t len)
{
  char *p;

  assert (len >= GFC_BTOA_BUF_SIZE);

  if (n == 0)
    return "0";

  p = buffer + GFC_BTOA_BUF_SIZE - 1;
  *p = '\0';

  while (n != 0)
    {
      *--p = '0' + (n & 1);
      n >>= 1;
    }

  return p;
}


void
write_i (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_decimal (dtp, f, p, len, (void *) gfc_itoa);
}


void
write_b (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_int (dtp, f, p, len, btoa);
}


void
write_o (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_int (dtp, f, p, len, otoa);
}

void
write_z (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_int (dtp, f, p, len, xtoa);
}


void
write_d (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float (dtp, f, p, len);
}


void
write_e (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float (dtp, f, p, len);
}


void
write_f (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float (dtp, f, p, len);
}


void
write_en (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float (dtp, f, p, len);
}


void
write_es (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float (dtp, f, p, len);
}


/* Take care of the X/TR descriptor.  */

void
write_x (st_parameter_dt *dtp, int len, int nspaces)
{
  char *p;

  p = write_block (dtp, len);
  if (p == NULL)
    return;

  if (nspaces > 0)
    memset (&p[len - nspaces], ' ', nspaces);
}


/* List-directed writing.  */


/* Write a single character to the output.  Returns nonzero if
   something goes wrong.  */

static int
write_char (st_parameter_dt *dtp, char c)
{
  char *p;

  p = write_block (dtp, 1);
  if (p == NULL)
    return 1;

  *p = c;

  return 0;
}


/* Write a list-directed logical value.  */

static void
write_logical (st_parameter_dt *dtp, const char *source, int length)
{
  write_char (dtp, extract_int (source, length) ? 'T' : 'F');
}


/* Write a list-directed integer value.  */

static void
write_integer (st_parameter_dt *dtp, const char *source, int length)
{
  char *p;
  const char *q;
  int digits;
  int width;
  char itoa_buf[GFC_ITOA_BUF_SIZE];

  q = gfc_itoa (extract_int (source, length), itoa_buf, sizeof (itoa_buf));

  switch (length)
    {
    case 1:
      width = 4;
      break;

    case 2:
      width = 6;
      break;

    case 4:
      width = 11;
      break;

    case 8:
      width = 20;
      break;

    default:
      width = 0;
      break;
    }

  digits = strlen (q);

  if (width < digits)
    width = digits;
  p = write_block (dtp, width);
  if (p == NULL)
    return;
  if (dtp->u.p.no_leading_blank)
    {
      memcpy (p, q, digits);
      memset (p + digits, ' ', width - digits);
    }
  else
    {
      memset (p, ' ', width - digits);
      memcpy (p + width - digits, q, digits);
    }
}


/* Write a list-directed string.  We have to worry about delimiting
   the strings if the file has been opened in that mode.  */

static void
write_character (st_parameter_dt *dtp, const char *source, int length)
{
  int i, extra;
  char *p, d;

  switch (dtp->u.p.current_unit->flags.delim)
    {
    case DELIM_APOSTROPHE:
      d = '\'';
      break;
    case DELIM_QUOTE:
      d = '"';
      break;
    default:
      d = ' ';
      break;
    }

  if (d == ' ')
    extra = 0;
  else
    {
      extra = 2;

      for (i = 0; i < length; i++)
	if (source[i] == d)
	  extra++;
    }

  p = write_block (dtp, length + extra);
  if (p == NULL)
    return;

  if (d == ' ')
    memcpy (p, source, length);
  else
    {
      *p++ = d;

      for (i = 0; i < length; i++)
	{
	  *p++ = source[i];
	  if (source[i] == d)
	    *p++ = d;
	}

      *p = d;
    }
}


/* Output a real number with default format.
   This is 1PG14.7E2 for REAL(4), 1PG23.15E3 for REAL(8),
   1PG24.15E4 for REAL(10) and 1PG40.31E4 for REAL(16).  */

static void
write_real (st_parameter_dt *dtp, const char *source, int length)
{
  fnode f ;
  int org_scale = dtp->u.p.scale_factor;
  f.format = FMT_G;
  dtp->u.p.scale_factor = 1;
  switch (length)
    {
    case 4:
      f.u.real.w = 14;
      f.u.real.d = 7;
      f.u.real.e = 2;
      break;
    case 8:
      f.u.real.w = 23;
      f.u.real.d = 15;
      f.u.real.e = 3;
      break;
    case 10:
      f.u.real.w = 28;
      f.u.real.d = 19;
      f.u.real.e = 4;
      break;
    case 16:
      f.u.real.w = 40;
      f.u.real.d = 31;
      f.u.real.e = 4;
      break;
    default:
      internal_error (&dtp->common, "bad real kind");
      break;
    }
  write_float (dtp, &f, source , length);
  dtp->u.p.scale_factor = org_scale;
}


static void
write_complex (st_parameter_dt *dtp, const char *source, int kind, size_t size)
{
  if (write_char (dtp, '('))
    return;
  write_real (dtp, source, kind);

  if (write_char (dtp, ','))
    return;
  write_real (dtp, source + size / 2, kind);

  write_char (dtp, ')');
}


/* Write the separator between items.  */

static void
write_separator (st_parameter_dt *dtp)
{
  char *p;

  p = write_block (dtp, options.separator_len);
  if (p == NULL)
    return;

  memcpy (p, options.separator, options.separator_len);
}


/* Write an item with list formatting.
   TODO: handle skipping to the next record correctly, particularly
   with strings.  */

static void
list_formatted_write_scalar (st_parameter_dt *dtp, bt type, void *p, int kind,
			     size_t size)
{
  if (dtp->u.p.current_unit == NULL)
    return;

  if (dtp->u.p.first_item)
    {
      dtp->u.p.first_item = 0;
      write_char (dtp, ' ');
    }
  else
    {
      if (type != BT_CHARACTER || !dtp->u.p.char_flag ||
	  dtp->u.p.current_unit->flags.delim != DELIM_NONE)
	write_separator (dtp);
    }

  switch (type)
    {
    case BT_INTEGER:
      write_integer (dtp, p, kind);
      break;
    case BT_LOGICAL:
      write_logical (dtp, p, kind);
      break;
    case BT_CHARACTER:
      write_character (dtp, p, kind);
      break;
    case BT_REAL:
      write_real (dtp, p, kind);
      break;
    case BT_COMPLEX:
      write_complex (dtp, p, kind, size);
      break;
    default:
      internal_error (&dtp->common, "list_formatted_write(): Bad type");
    }

  dtp->u.p.char_flag = (type == BT_CHARACTER);
}


void
list_formatted_write (st_parameter_dt *dtp, bt type, void *p, int kind,
		      size_t size, size_t nelems)
{
  size_t elem;
  char *tmp;

  tmp = (char *) p;

  /* Big loop over all the elements.  */
  for (elem = 0; elem < nelems; elem++)
    {
      dtp->u.p.item_count++;
      list_formatted_write_scalar (dtp, type, tmp + size*elem, kind, size);
    }
}

/*			NAMELIST OUTPUT

   nml_write_obj writes a namelist object to the output stream.  It is called
   recursively for derived type components:
	obj    = is the namelist_info for the current object.
	offset = the offset relative to the address held by the object for
		 derived type arrays.
	base   = is the namelist_info of the derived type, when obj is a
		 component.
	base_name = the full name for a derived type, including qualifiers
		    if any.
   The returned value is a pointer to the object beyond the last one
   accessed, including nested derived types.  Notice that the namelist is
   a linear linked list of objects, including derived types and their
   components.  A tree, of sorts, is implied by the compound names of
   the derived type components and this is how this function recurses through
   the list.  */

/* A generous estimate of the number of characters needed to print
   repeat counts and indices, including commas, asterices and brackets.  */

#define NML_DIGITS 20

static namelist_info *
nml_write_obj (st_parameter_dt *dtp, namelist_info * obj, index_type offset,
	       namelist_info * base, char * base_name)
{
  int rep_ctr;
  int num;
  int nml_carry;
  index_type len;
  index_type obj_size;
  index_type nelem;
  index_type dim_i;
  index_type clen;
  index_type elem_ctr;
  index_type obj_name_len;
  void * p ;
  char cup;
  char * obj_name;
  char * ext_name;
  char rep_buff[NML_DIGITS];
  namelist_info * cmp;
  namelist_info * retval = obj->next;

  /* Write namelist variable names in upper case. If a derived type,
     nothing is output.  If a component, base and base_name are set.  */

  if (obj->type != GFC_DTYPE_DERIVED)
    {
#ifdef HAVE_CRLF
      write_character (dtp, "\r\n ", 3);
#else
      write_character (dtp, "\n ", 2);
#endif
      len = 0;
      if (base)
	{
	  len =strlen (base->var_name);
	  for (dim_i = 0; dim_i < (index_type) strlen (base_name); dim_i++)
            {
	      cup = toupper (base_name[dim_i]);
	      write_character (dtp, &cup, 1);
            }
	}
      for (dim_i =len; dim_i < (index_type) strlen (obj->var_name); dim_i++)
	{
	  cup = toupper (obj->var_name[dim_i]);
	  write_character (dtp, &cup, 1);
	}
      write_character (dtp, "=", 1);
    }

  /* Counts the number of data output on a line, including names.  */

  num = 1;

  len = obj->len;

  switch (obj->type)
    {

    case GFC_DTYPE_REAL:
      obj_size = size_from_real_kind (len);
      break;

    case GFC_DTYPE_COMPLEX:
      obj_size = size_from_complex_kind (len);
      break;

    case GFC_DTYPE_CHARACTER:
      obj_size = obj->string_length;
      break;

    default:
      obj_size = len;      
    }

  if (obj->var_rank)
    obj_size = obj->size;

  /* Set the index vector and count the number of elements.  */

  nelem = 1;
  for (dim_i=0; dim_i < obj->var_rank; dim_i++)
    {
      obj->ls[dim_i].idx = obj->dim[dim_i].lbound;
      nelem = nelem * (obj->dim[dim_i].ubound + 1 - obj->dim[dim_i].lbound);
    }

  /* Main loop to output the data held in the object.  */

  rep_ctr = 1;
  for (elem_ctr = 0; elem_ctr < nelem; elem_ctr++)
    {

      /* Build the pointer to the data value.  The offset is passed by
	 recursive calls to this function for arrays of derived types.
	 Is NULL otherwise.  */

      p = (void *)(obj->mem_pos + elem_ctr * obj_size);
      p += offset;

      /* Check for repeat counts of intrinsic types.  */

      if ((elem_ctr < (nelem - 1)) &&
	  (obj->type != GFC_DTYPE_DERIVED) &&
	  !memcmp (p, (void*)(p + obj_size ), obj_size ))
	{
	  rep_ctr++;
	}

      /* Execute a repeated output.  Note the flag no_leading_blank that
	 is used in the functions used to output the intrinsic types.  */

      else
	{
	  if (rep_ctr > 1)
	    {
	      st_sprintf(rep_buff, " %d*", rep_ctr);
	      write_character (dtp, rep_buff, strlen (rep_buff));
	      dtp->u.p.no_leading_blank = 1;
	    }
	  num++;

	  /* Output the data, if an intrinsic type, or recurse into this
	     routine to treat derived types.  */

	  switch (obj->type)
	    {

	    case GFC_DTYPE_INTEGER:
	      write_integer (dtp, p, len);
              break;

	    case GFC_DTYPE_LOGICAL:
	      write_logical (dtp, p, len);
              break;

	    case GFC_DTYPE_CHARACTER:
	      if (dtp->u.p.nml_delim)
		write_character (dtp, &dtp->u.p.nml_delim, 1);
	      write_character (dtp, p, obj->string_length);
	      if (dtp->u.p.nml_delim)
		write_character (dtp, &dtp->u.p.nml_delim, 1);
              break;

	    case GFC_DTYPE_REAL:
	      write_real (dtp, p, len);
              break;

	    case GFC_DTYPE_COMPLEX:
	      dtp->u.p.no_leading_blank = 0;
	      num++;
              write_complex (dtp, p, len, obj_size);
              break;

	    case GFC_DTYPE_DERIVED:

	      /* To treat a derived type, we need to build two strings:
		 ext_name = the name, including qualifiers that prepends
			    component names in the output - passed to
			    nml_write_obj.
		 obj_name = the derived type name with no qualifiers but %
			    appended.  This is used to identify the
			    components.  */

	      /* First ext_name => get length of all possible components  */

	      ext_name = (char*)get_mem ( (base_name ? strlen (base_name) : 0)
					+ (base ? strlen (base->var_name) : 0)
					+ strlen (obj->var_name)
					+ obj->var_rank * NML_DIGITS
					+ 1);

	      strcpy(ext_name, base_name ? base_name : "");
	      clen = base ? strlen (base->var_name) : 0;
	      strcat (ext_name, obj->var_name + clen);

	      /* Append the qualifier.  */

	      for (dim_i = 0; dim_i < obj->var_rank; dim_i++)
		{
		  strcat (ext_name, dim_i ? "" : "(");
		  clen = strlen (ext_name);
		  st_sprintf (ext_name + clen, "%d", (int) obj->ls[dim_i].idx);
		  strcat (ext_name, (dim_i == obj->var_rank - 1) ? ")" : ",");
		}

	      /* Now obj_name.  */

	      obj_name_len = strlen (obj->var_name) + 1;
	      obj_name = get_mem (obj_name_len+1);
	      strcpy (obj_name, obj->var_name);
	      strcat (obj_name, "%");

	      /* Now loop over the components. Update the component pointer
		 with the return value from nml_write_obj => this loop jumps
		 past nested derived types.  */

	      for (cmp = obj->next;
		   cmp && !strncmp (cmp->var_name, obj_name, obj_name_len);
		   cmp = retval)
		{
		  retval = nml_write_obj (dtp, cmp,
					  (index_type)(p - obj->mem_pos),
					  obj, ext_name);
		}

	      free_mem (obj_name);
	      free_mem (ext_name);
	      goto obj_loop;

            default:
	      internal_error (&dtp->common, "Bad type for namelist write");
            }

	  /* Reset the leading blank suppression, write a comma and, if 5
	     values have been output, write a newline and advance to column
	     2. Reset the repeat counter.  */

	  dtp->u.p.no_leading_blank = 0;
	  write_character (dtp, ",", 1);
	  if (num > 5)
	    {
	      num = 0;
#ifdef HAVE_CRLF
	      write_character (dtp, "\r\n ", 3);
#else
	      write_character (dtp, "\n ", 2);
#endif
	    }
	  rep_ctr = 1;
	}

    /* Cycle through and increment the index vector.  */

obj_loop:

    nml_carry = 1;
    for (dim_i = 0; nml_carry && (dim_i < obj->var_rank); dim_i++)
      {
	obj->ls[dim_i].idx += nml_carry ;
	nml_carry = 0;
	if (obj->ls[dim_i].idx  > (ssize_t)obj->dim[dim_i].ubound)
	  {
	    obj->ls[dim_i].idx = obj->dim[dim_i].lbound;
	    nml_carry = 1;
	  }
       }
    }

  /* Return a pointer beyond the furthest object accessed.  */

  return retval;
}

/* This is the entry function for namelist writes.  It outputs the name
   of the namelist and iterates through the namelist by calls to
   nml_write_obj.  The call below has dummys in the arguments used in
   the treatment of derived types.  */

void
namelist_write (st_parameter_dt *dtp)
{
  namelist_info * t1, *t2, *dummy = NULL;
  index_type i;
  index_type dummy_offset = 0;
  char c;
  char * dummy_name = NULL;
  unit_delim tmp_delim;

  /* Set the delimiter for namelist output.  */

  tmp_delim = dtp->u.p.current_unit->flags.delim;
  dtp->u.p.current_unit->flags.delim = DELIM_NONE;
  switch (tmp_delim)
    {
    case (DELIM_QUOTE):
      dtp->u.p.nml_delim = '"';
      break;

    case (DELIM_APOSTROPHE):
      dtp->u.p.nml_delim = '\'';
      break;

    default:
      dtp->u.p.nml_delim = '\0';
      break;
    }

  write_character (dtp, "&", 1);

  /* Write namelist name in upper case - f95 std.  */

  for (i = 0 ;i < dtp->namelist_name_len ;i++ )
    {
      c = toupper (dtp->namelist_name[i]);
      write_character (dtp, &c ,1);
    }

  if (dtp->u.p.ionml != NULL)
    {
      t1 = dtp->u.p.ionml;
      while (t1 != NULL)
	{
	  t2 = t1;
	  t1 = nml_write_obj (dtp, t2, dummy_offset, dummy, dummy_name);
	}
    }
#ifdef HAVE_CRLF
  write_character (dtp, "  /\r\n", 5);
#else
  write_character (dtp, "  /\n", 4);
#endif

  /* Recover the original delimiter.  */

  dtp->u.p.current_unit->flags.delim = tmp_delim;
}

#undef NML_DIGITS
