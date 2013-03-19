/* Copyright (C) 2002-2013 Free Software Foundation, Inc.
   Contributed by Andy Vaught
   Namelist output contributed by Paul Thomas
   F2003 I/O support contributed by Jerry DeLisle

This file is part of the GNU Fortran runtime library (libgfortran).

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
#include "format.h"
#include "unix.h"
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <errno.h>
#define star_fill(p, n) memset(p, '*', n)

typedef unsigned char uchar;

/* Helper functions for character(kind=4) internal units.  These are needed
   by write_float.def.  */

static void
memcpy4 (gfc_char4_t *dest, const char *source, int k)
{
  int j;
  
  const char *p = source;
  for (j = 0; j < k; j++)
    *dest++ = (gfc_char4_t) *p++;
}

/* This include contains the heart and soul of formatted floating point.  */
#include "write_float.def"

/* Write out default char4.  */

static void
write_default_char4 (st_parameter_dt *dtp, const gfc_char4_t *source,
		     int src_len, int w_len)
{
  char *p;
  int j, k = 0;
  gfc_char4_t c;
  uchar d;
      
  /* Take care of preceding blanks.  */
  if (w_len > src_len)
    {
      k = w_len - src_len;
      p = write_block (dtp, k);
      if (p == NULL)
	return;
      if (is_char4_unit (dtp))
	{
	  gfc_char4_t *p4 = (gfc_char4_t *) p;
	  memset4 (p4, ' ', k);
	}
      else
	memset (p, ' ', k);
    }

  /* Get ready to handle delimiters if needed.  */
  switch (dtp->u.p.current_unit->delim_status)
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

  /* Now process the remaining characters, one at a time.  */
  for (j = 0; j < src_len; j++)
    {
      c = source[j];
      if (is_char4_unit (dtp))
	{
	  gfc_char4_t *q;
	  /* Handle delimiters if any.  */
	  if (c == d && d != ' ')
	    {
	      p = write_block (dtp, 2);
	      if (p == NULL)
		return;
	      q = (gfc_char4_t *) p;
	      *q++ = c;
	    }
	  else
	    {
	      p = write_block (dtp, 1);
	      if (p == NULL)
		return;
	      q = (gfc_char4_t *) p;
	    }
	  *q = c;
	}
      else
	{
	  /* Handle delimiters if any.  */
	  if (c == d && d != ' ')
	    {
	      p = write_block (dtp, 2);
	      if (p == NULL)
		return;
	      *p++ = (uchar) c;
	    }
          else
	    {
	      p = write_block (dtp, 1);
	      if (p == NULL)
		return;
	    }
	    *p = c > 255 ? '?' : (uchar) c;
	}
    }
}


/* Write out UTF-8 converted from char4.  */

static void
write_utf8_char4 (st_parameter_dt *dtp, gfc_char4_t *source,
		     int src_len, int w_len)
{
  char *p;
  int j, k = 0;
  gfc_char4_t c;
  static const uchar masks[6] =  { 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };
  static const uchar limits[6] = { 0x80, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE };
  int nbytes;
  uchar buf[6], d, *q; 

  /* Take care of preceding blanks.  */
  if (w_len > src_len)
    {
      k = w_len - src_len;
      p = write_block (dtp, k);
      if (p == NULL)
	return;
      memset (p, ' ', k);
    }

  /* Get ready to handle delimiters if needed.  */
  switch (dtp->u.p.current_unit->delim_status)
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

  /* Now process the remaining characters, one at a time.  */
  for (j = k; j < src_len; j++)
    {
      c = source[j];
      if (c < 0x80)
	{
	  /* Handle the delimiters if any.  */
	  if (c == d && d != ' ')
	    {
	      p = write_block (dtp, 2);
	      if (p == NULL)
		return;
	      *p++ = (uchar) c;
	    }
	  else
	    {
	      p = write_block (dtp, 1);
	      if (p == NULL)
		return;
	    }
	  *p = (uchar) c;
	}
      else
	{
	  /* Convert to UTF-8 sequence.  */
	  nbytes = 1;
	  q = &buf[6];

	  do
	    {
	      *--q = ((c & 0x3F) | 0x80);
	      c >>= 6;
	      nbytes++;
	    }
	  while (c >= 0x3F || (c & limits[nbytes-1]));

	  *--q = (c | masks[nbytes-1]);

	  p = write_block (dtp, nbytes);
	  if (p == NULL)
	    return;

	  while (q < &buf[6])
	    *p++ = *q++;
	}
    }
}


void
write_a (st_parameter_dt *dtp, const fnode *f, const char *source, int len)
{
  int wlen;
  char *p;

  wlen = f->u.string.length < 0
	 || (f->format == FMT_G && f->u.string.length == 0)
	 ? len : f->u.string.length;

#ifdef HAVE_CRLF
  /* If this is formatted STREAM IO convert any embedded line feed characters
     to CR_LF on systems that use that sequence for newlines.  See F2003
     Standard sections 10.6.3 and 9.9 for further information.  */
  if (is_stream_io (dtp))
    {
      const char crlf[] = "\r\n";
      int i, q, bytes;
      q = bytes = 0;

      /* Write out any padding if needed.  */
      if (len < wlen)
	{
	  p = write_block (dtp, wlen - len);
	  if (p == NULL)
	    return;
	  memset (p, ' ', wlen - len);
	}

      /* Scan the source string looking for '\n' and convert it if found.  */
      for (i = 0; i < wlen; i++)
	{
	  if (source[i] == '\n')
	    {
	      /* Write out the previously scanned characters in the string.  */
	      if (bytes > 0)
		{
		  p = write_block (dtp, bytes);
		  if (p == NULL)
		    return;
		  memcpy (p, &source[q], bytes);
		  q += bytes;
		  bytes = 0;
		}

	      /* Write out the CR_LF sequence.  */ 
	      q++;
	      p = write_block (dtp, 2);
              if (p == NULL)
                return;
	      memcpy (p, crlf, 2);
	    }
	  else
	    bytes++;
	}

      /*  Write out any remaining bytes if no LF was found.  */
      if (bytes > 0)
	{
	  p = write_block (dtp, bytes);
	  if (p == NULL)
	    return;
	  memcpy (p, &source[q], bytes);
	}
    }
  else
    {
#endif
      p = write_block (dtp, wlen);
      if (p == NULL)
	return;

      if (unlikely (is_char4_unit (dtp)))
	{
	  gfc_char4_t *p4 = (gfc_char4_t *) p;
	  if (wlen < len)
	    memcpy4 (p4, source, wlen);
	  else
	    {
	      memset4 (p4, ' ', wlen - len);
	      memcpy4 (p4 + wlen - len, source, len);
	    }
	  return;
	}

      if (wlen < len)
	memcpy (p, source, wlen);
      else
	{
	  memset (p, ' ', wlen - len);
	  memcpy (p + wlen - len, source, len);
	}
#ifdef HAVE_CRLF
    }
#endif
}


/* The primary difference between write_a_char4 and write_a is that we have to
   deal with writing from the first byte of the 4-byte character and pay
   attention to the most significant bytes.  For ENCODING="default" write the
   lowest significant byte. If the 3 most significant bytes contain
   non-zero values, emit a '?'.  For ENCODING="utf-8", convert the UCS-32 value
   to the UTF-8 encoded string before writing out.  */

void
write_a_char4 (st_parameter_dt *dtp, const fnode *f, const char *source, int len)
{
  int wlen;
  gfc_char4_t *q;

  wlen = f->u.string.length < 0
	 || (f->format == FMT_G && f->u.string.length == 0)
	 ? len : f->u.string.length;

  q = (gfc_char4_t *) source;
#ifdef HAVE_CRLF
  /* If this is formatted STREAM IO convert any embedded line feed characters
     to CR_LF on systems that use that sequence for newlines.  See F2003
     Standard sections 10.6.3 and 9.9 for further information.  */
  if (is_stream_io (dtp))
    {
      const gfc_char4_t crlf[] = {0x000d,0x000a};
      int i, bytes;
      gfc_char4_t *qq;
      bytes = 0;

      /* Write out any padding if needed.  */
      if (len < wlen)
	{
	  char *p;
	  p = write_block (dtp, wlen - len);
	  if (p == NULL)
	    return;
	  memset (p, ' ', wlen - len);
	}

      /* Scan the source string looking for '\n' and convert it if found.  */
      qq = (gfc_char4_t *) source;
      for (i = 0; i < wlen; i++)
	{
	  if (qq[i] == '\n')
	    {
	      /* Write out the previously scanned characters in the string.  */
	      if (bytes > 0)
		{
		  if (dtp->u.p.current_unit->flags.encoding == ENCODING_UTF8)
		    write_utf8_char4 (dtp, q, bytes, 0);
		  else
		    write_default_char4 (dtp, q, bytes, 0);
		  bytes = 0;
		}

	      /* Write out the CR_LF sequence.  */ 
	      write_default_char4 (dtp, crlf, 2, 0);
	    }
	  else
	    bytes++;
	}

      /*  Write out any remaining bytes if no LF was found.  */
      if (bytes > 0)
	{
	  if (dtp->u.p.current_unit->flags.encoding == ENCODING_UTF8)
	    write_utf8_char4 (dtp, q, bytes, 0);
	  else
	    write_default_char4 (dtp, q, bytes, 0);
	}
    }
  else
    {
#endif
      if (dtp->u.p.current_unit->flags.encoding == ENCODING_UTF8)
	write_utf8_char4 (dtp, q, len, wlen);
      else
	write_default_char4 (dtp, q, len, wlen);
#ifdef HAVE_CRLF
    }
#endif
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
    case 10:
    case 16:
      {
	GFC_INTEGER_16 tmp = 0;
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


void
write_l (st_parameter_dt *dtp, const fnode *f, char *source, int len)
{
  char *p;
  int wlen;
  GFC_INTEGER_LARGEST n;

  wlen = (f->format == FMT_G && f->u.w == 0) ? 1 : f->u.w;
  
  p = write_block (dtp, wlen);
  if (p == NULL)
    return;

  n = extract_int (source, len);

  if (unlikely (is_char4_unit (dtp)))
    {
      gfc_char4_t *p4 = (gfc_char4_t *) p;
      memset4 (p4, ' ', wlen -1);
      p4[wlen - 1] = (n) ? 'T' : 'F';
      return;
    }

  memset (p, ' ', wlen -1);
  p[wlen - 1] = (n) ? 'T' : 'F';
}


static void
write_boz (st_parameter_dt *dtp, const fnode *f, const char *q, int n)
{
  int w, m, digits, nzero, nblank;
  char *p;

  w = f->u.integer.w;
  m = f->u.integer.m;

  /* Special case:  */

  if (m == 0 && n == 0)
    {
      if (w == 0)
        w = 1;

      p = write_block (dtp, w);
      if (p == NULL)
        return;
      if (unlikely (is_char4_unit (dtp)))
	{
	  gfc_char4_t *p4 = (gfc_char4_t *) p;
	  memset4 (p4, ' ', w);
	}
      else
	memset (p, ' ', w);
      goto done;
    }

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

  if (unlikely (is_char4_unit (dtp)))
    {
      gfc_char4_t *p4 = (gfc_char4_t *) p;
      if (nblank < 0)
	{
	  memset4 (p4, '*', w);
	  return;
	}

      if (!dtp->u.p.no_leading_blank)
	{
	  memset4 (p4, ' ', nblank);
	  q += nblank;
	  memset4 (p4, '0', nzero);
	  q += nzero;
	  memcpy4 (p4, q, digits);
	}
      else
	{
	  memset4 (p4, '0', nzero);
	  q += nzero;
	  memcpy4 (p4, q, digits);
	  q += digits;
	  memset4 (p4, ' ', nblank);
	  dtp->u.p.no_leading_blank = 0;
	}
      return;
    }

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
  m = f->format == FMT_G ? -1 : f->u.integer.m;

  n = extract_int (source, len);

  /* Special case:  */
  if (m == 0 && n == 0)
    {
      if (w == 0)
        w = 1;

      p = write_block (dtp, w);
      if (p == NULL)
        return;
      if (unlikely (is_char4_unit (dtp)))
	{
	  gfc_char4_t *p4 = (gfc_char4_t *) p;
	  memset4 (p4, ' ', w);
	}
      else
	memset (p, ' ', w);
      goto done;
    }

  sign = calculate_sign (dtp, n < 0);
  if (n < 0)
    n = -n;
  nsign = sign == S_NONE ? 0 : 1;
  
  /* conv calls itoa which sets the negative sign needed
     by write_integer. The sign '+' or '-' is set below based on sign
     calculated above, so we just point past the sign in the string
     before proceeding to avoid double signs in corner cases.
     (see PR38504)  */
  q = conv (n, itoa_buf, sizeof (itoa_buf));
  if (*q == '-')
    q++;

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

  if (unlikely (is_char4_unit (dtp)))
    {
      gfc_char4_t * p4 = (gfc_char4_t *) p;
      if (nblank < 0)
	{
	  memset4 (p4, '*', w);
	  goto done;
	}

      memset4 (p4, ' ', nblank);
      p4 += nblank;

      switch (sign)
	{
	case S_PLUS:
	  *p4++ = '+';
	  break;
	case S_MINUS:
	  *p4++ = '-';
	  break;
	case S_NONE:
	  break;
	}

      memset4 (p4, '0', nzero);
      p4 += nzero;

      memcpy4 (p4, q, digits);
      return;
    }

  if (nblank < 0)
    {
      star_fill (p, w);
      goto done;
    }

  memset (p, ' ', nblank);
  p += nblank;

  switch (sign)
    {
    case S_PLUS:
      *p++ = '+';
      break;
    case S_MINUS:
      *p++ = '-';
      break;
    case S_NONE:
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

/* The following three functions, btoa_big, otoa_big, and ztoa_big, are needed
   to convert large reals with kind sizes that exceed the largest integer type
   available on certain platforms.  In these cases, byte by byte conversion is
   performed. Endianess is taken into account.  */

/* Conversion to binary.  */

static const char *
btoa_big (const char *s, char *buffer, int len, GFC_UINTEGER_LARGEST *n)
{
  char *q;
  int i, j;
  
  q = buffer;
  if (big_endian)
    {
      const char *p = s;
      for (i = 0; i < len; i++)
	{
	  char c = *p;

	  /* Test for zero. Needed by write_boz later.  */
	  if (*p != 0)
	    *n = 1;

	  for (j = 0; j < 8; j++)
	    {
	      *q++ = (c & 128) ? '1' : '0';
	      c <<= 1;
	    }
	  p++;
	}
    }
  else
    {
      const char *p = s + len - 1;
      for (i = 0; i < len; i++)
	{
	  char c = *p;

	  /* Test for zero. Needed by write_boz later.  */
	  if (*p != 0)
	    *n = 1;

	  for (j = 0; j < 8; j++)
	    {
	      *q++ = (c & 128) ? '1' : '0';
	      c <<= 1;
	    }
	  p--;
	}
    }

  *q = '\0';

  if (*n == 0)
    return "0";

  /* Move past any leading zeros.  */  
  while (*buffer == '0')
    buffer++;

  return buffer;

}

/* Conversion to octal.  */

static const char *
otoa_big (const char *s, char *buffer, int len, GFC_UINTEGER_LARGEST *n)
{
  char *q;
  int i, j, k;
  uint8_t octet;

  q = buffer + GFC_OTOA_BUF_SIZE - 1;
  *q = '\0';
  i = k = octet = 0;

  if (big_endian)
    {
      const char *p = s + len - 1;
      char c = *p;
      while (i < len)
	{
	  /* Test for zero. Needed by write_boz later.  */
	  if (*p != 0)
	    *n = 1;

	  for (j = 0; j < 3 && i < len; j++)
	    {
	      octet |= (c & 1) << j;
	      c >>= 1;
	      if (++k > 7)
	        {
		  i++;
		  k = 0;
		  c = *--p;
		}
	    }
	  *--q = '0' + octet;
	  octet = 0;
	}
    }
  else
    {
      const char *p = s;
      char c = *p;
      while (i < len)
	{
	  /* Test for zero. Needed by write_boz later.  */
	  if (*p != 0)
	    *n = 1;

	  for (j = 0; j < 3 && i < len; j++)
	    {
	      octet |= (c & 1) << j;
	      c >>= 1;
	      if (++k > 7)
	        {
		  i++;
		  k = 0;
		  c = *++p;
		}
	    }
	  *--q = '0' + octet;
	  octet = 0;
	}
    }

  if (*n == 0)
    return "0";

  /* Move past any leading zeros.  */  
  while (*q == '0')
    q++;

  return q;
}

/* Conversion to hexidecimal.  */

static const char *
ztoa_big (const char *s, char *buffer, int len, GFC_UINTEGER_LARGEST *n)
{
  static char a[16] = {'0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};

  char *q;
  uint8_t h, l;
  int i;
  
  q = buffer;
  
  if (big_endian)
    {
      const char *p = s;
      for (i = 0; i < len; i++)
	{
	  /* Test for zero. Needed by write_boz later.  */
	  if (*p != 0)
	    *n = 1;

	  h = (*p >> 4) & 0x0F;
	  l = *p++ & 0x0F;
	  *q++ = a[h];
	  *q++ = a[l];
	}
    }
  else
    {
      const char *p = s + len - 1;
      for (i = 0; i < len; i++)
	{
	  /* Test for zero. Needed by write_boz later.  */
	  if (*p != 0)
	    *n = 1;

	  h = (*p >> 4) & 0x0F;
	  l = *p-- & 0x0F;
	  *q++ = a[h];
	  *q++ = a[l];
	}
    }

  *q = '\0';
  
  if (*n == 0)
    return "0";
    
  /* Move past any leading zeros.  */  
  while (*buffer == '0')
    buffer++;

  return buffer;
}

/* gfc_itoa()-- Integer to decimal conversion.
   The itoa function is a widespread non-standard extension to standard
   C, often declared in <stdlib.h>.  Even though the itoa defined here
   is a static function we take care not to conflict with any prior
   non-static declaration.  Hence the 'gfc_' prefix, which is normally
   reserved for functions with external linkage.  */

static const char *
gfc_itoa (GFC_INTEGER_LARGEST n, char *buffer, size_t len)
{
  int negative;
  char *p;
  GFC_UINTEGER_LARGEST t;

  assert (len >= GFC_ITOA_BUF_SIZE);

  if (n == 0)
    return "0";

  negative = 0;
  t = n;
  if (n < 0)
    {
      negative = 1;
      t = -n; /*must use unsigned to protect from overflow*/
    }

  p = buffer + GFC_ITOA_BUF_SIZE - 1;
  *p = '\0';

  while (t != 0)
    {
      *--p = '0' + (t % 10);
      t /= 10;
    }

  if (negative)
    *--p = '-';
  return p;
}


void
write_i (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_decimal (dtp, f, p, len, (void *) gfc_itoa);
}


void
write_b (st_parameter_dt *dtp, const fnode *f, const char *source, int len)
{
  const char *p;
  char itoa_buf[GFC_BTOA_BUF_SIZE];
  GFC_UINTEGER_LARGEST n = 0;

  if (len > (int) sizeof (GFC_UINTEGER_LARGEST))
    {
      p = btoa_big (source, itoa_buf, len, &n);
      write_boz (dtp, f, p, n);
    }
  else
    {
      n = extract_uint (source, len);
      p = btoa (n, itoa_buf, sizeof (itoa_buf));
      write_boz (dtp, f, p, n);
    }
}


void
write_o (st_parameter_dt *dtp, const fnode *f, const char *source, int len)
{
  const char *p;
  char itoa_buf[GFC_OTOA_BUF_SIZE];
  GFC_UINTEGER_LARGEST n = 0;
  
  if (len > (int) sizeof (GFC_UINTEGER_LARGEST))
    {
      p = otoa_big (source, itoa_buf, len, &n);
      write_boz (dtp, f, p, n);
    }
  else
    {
      n = extract_uint (source, len);
      p = otoa (n, itoa_buf, sizeof (itoa_buf));
      write_boz (dtp, f, p, n);
    }
}

void
write_z (st_parameter_dt *dtp, const fnode *f, const char *source, int len)
{
  const char *p;
  char itoa_buf[GFC_XTOA_BUF_SIZE];
  GFC_UINTEGER_LARGEST n = 0;

  if (len > (int) sizeof (GFC_UINTEGER_LARGEST))
    {
      p = ztoa_big (source, itoa_buf, len, &n);
      write_boz (dtp, f, p, n);
    }
  else
    {
      n = extract_uint (source, len);
      p = gfc_xtoa (n, itoa_buf, sizeof (itoa_buf));
      write_boz (dtp, f, p, n);
    }
}


void
write_d (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float (dtp, f, p, len, 0);
}


void
write_e (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float (dtp, f, p, len, 0);
}


void
write_f (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float (dtp, f, p, len, 0);
}


void
write_en (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float (dtp, f, p, len, 0);
}


void
write_es (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float (dtp, f, p, len, 0);
}


/* Take care of the X/TR descriptor.  */

void
write_x (st_parameter_dt *dtp, int len, int nspaces)
{
  char *p;

  p = write_block (dtp, len);
  if (p == NULL)
    return;
  if (nspaces > 0 && len - nspaces >= 0)
    {
      if (unlikely (is_char4_unit (dtp)))
	{
	  gfc_char4_t *p4 = (gfc_char4_t *) p;
	  memset4 (&p4[len - nspaces], ' ', nspaces);
	}
      else
	memset (&p[len - nspaces], ' ', nspaces);
    }
}


/* List-directed writing.  */


/* Write a single character to the output.  Returns nonzero if
   something goes wrong.  */

static int
write_char (st_parameter_dt *dtp, int c)
{
  char *p;

  p = write_block (dtp, 1);
  if (p == NULL)
    return 1;
  if (unlikely (is_char4_unit (dtp)))
    {
      gfc_char4_t *p4 = (gfc_char4_t *) p;
      *p4 = c;
      return 0;
    }

  *p = (uchar) c;

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

  if (unlikely (is_char4_unit (dtp)))
    {
      gfc_char4_t *p4 = (gfc_char4_t *) p;
      if (dtp->u.p.no_leading_blank)
	{
	  memcpy4 (p4, q, digits);
	  memset4 (p4 + digits, ' ', width - digits);
	}
      else
	{
	  memset4 (p4, ' ', width - digits);
	  memcpy4 (p4 + width - digits, q, digits);
	}
      return;
    }

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
write_character (st_parameter_dt *dtp, const char *source, int kind, int length)
{
  int i, extra;
  char *p, d;

  switch (dtp->u.p.current_unit->delim_status)
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

  if (kind == 1)
    {
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

      if (unlikely (is_char4_unit (dtp)))
	{
	  gfc_char4_t d4 = (gfc_char4_t) d;
	  gfc_char4_t *p4 = (gfc_char4_t *) p;

	  if (d4 == ' ')
	    memcpy4 (p4, source, length);
	  else
	    {
	      *p4++ = d4;

	      for (i = 0; i < length; i++)
		{
		  *p4++ = (gfc_char4_t) source[i];
		  if (source[i] == d)
		    *p4++ = d4;
		}

	      *p4 = d4;
	    }
	  return;
	}

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
  else
    {
      if (d == ' ')
	{
	  if (dtp->u.p.current_unit->flags.encoding == ENCODING_UTF8)
	    write_utf8_char4 (dtp, (gfc_char4_t *) source, length, 0);
	  else
	    write_default_char4 (dtp, (gfc_char4_t *) source, length, 0);
	}
      else
	{
	  p = write_block (dtp, 1);
	  *p = d;

	  if (dtp->u.p.current_unit->flags.encoding == ENCODING_UTF8)
	    write_utf8_char4 (dtp, (gfc_char4_t *) source, length, 0);
	  else
	    write_default_char4 (dtp, (gfc_char4_t *) source, length, 0);

	  p = write_block (dtp, 1);
	  *p = d;
	}
    }
}


/* Set an fnode to default format.  */

static void
set_fnode_default (st_parameter_dt *dtp, fnode *f, int length)
{
  f->format = FMT_G;
  switch (length)
    {
    case 4:
      f->u.real.w = 16;
      f->u.real.d = 9;
      f->u.real.e = 2;
      break;
    case 8:
      f->u.real.w = 25;
      f->u.real.d = 17;
      f->u.real.e = 3;
      break;
    case 10:
      f->u.real.w = 30;
      f->u.real.d = 21;
      f->u.real.e = 4;
      break;
    case 16:
      f->u.real.w = 45;
      f->u.real.d = 36;
      f->u.real.e = 4;
      break;
    default:
      internal_error (&dtp->common, "bad real kind");
      break;
    }
}

/* Output a real number with default format.  To guarantee that a
   binary -> decimal -> binary roundtrip conversion recovers the
   original value, IEEE 754-2008 requires 9, 17, 21 and 36 significant
   digits for REAL kinds 4, 8, 10, and 16, respectively. Thus, we use
   1PG16.9E2 for REAL(4), 1PG25.17E3 for REAL(8), 1PG30.21E4 for
   REAL(10) and 1PG45.36E4 for REAL(16). The exception is that the
   Fortran standard requires outputting an extra digit when the scale
   factor is 1 and when the magnitude of the value is such that E
   editing is used. However, gfortran compensates for this, and thus
   for list formatted the same number of significant digits is
   generated both when using F and E editing.  */

void
write_real (st_parameter_dt *dtp, const char *source, int length)
{
  fnode f ;
  int org_scale = dtp->u.p.scale_factor;
  dtp->u.p.scale_factor = 1;
  set_fnode_default (dtp, &f, length);
  write_float (dtp, &f, source , length, 1);
  dtp->u.p.scale_factor = org_scale;
}

/* Similar to list formatted REAL output, for kPG0 where k > 0 we
   compensate for the extra digit.  */

void
write_real_g0 (st_parameter_dt *dtp, const char *source, int length, int d)
{
  fnode f;
  int comp_d; 
  set_fnode_default (dtp, &f, length);
  if (d > 0)
    f.u.real.d = d;

  /* Compensate for extra digits when using scale factor, d is not
     specified, and the magnitude is such that E editing is used.  */
  if (dtp->u.p.scale_factor > 0 && d == 0)
    comp_d = 1;
  else
    comp_d = 0;
  dtp->u.p.g0_no_blanks = 1;
  write_float (dtp, &f, source , length, comp_d);
  dtp->u.p.g0_no_blanks = 0;
}


static void
write_complex (st_parameter_dt *dtp, const char *source, int kind, size_t size)
{
  char semi_comma =
	dtp->u.p.current_unit->decimal_status == DECIMAL_POINT ? ',' : ';';

  if (write_char (dtp, '('))
    return;
  write_real (dtp, source, kind);

  if (write_char (dtp, semi_comma))
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
  if (unlikely (is_char4_unit (dtp)))
    {
      gfc_char4_t *p4 = (gfc_char4_t *) p;
      memcpy4 (p4, options.separator, options.separator_len);
    }
  else
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
	dtp->u.p.current_unit->delim_status != DELIM_NONE)
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
      write_character (dtp, p, kind, size);
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
  size_t stride = type == BT_CHARACTER ?
		  size * GFC_SIZE_OF_CHAR_KIND(kind) : size;

  tmp = (char *) p;

  /* Big loop over all the elements.  */
  for (elem = 0; elem < nelems; elem++)
    {
      dtp->u.p.item_count++;
      list_formatted_write_scalar (dtp, type, tmp + elem * stride, kind, size);
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

static void
namelist_write_newline (st_parameter_dt *dtp)
{
  if (!is_internal_unit (dtp))
    {
#ifdef HAVE_CRLF
      write_character (dtp, "\r\n", 1, 2);
#else
      write_character (dtp, "\n", 1, 1);
#endif
      return;
    }

  if (is_array_io (dtp))
    {
      gfc_offset record;
      int finished;
      char *p;
      int length = dtp->u.p.current_unit->bytes_left;

      p = write_block (dtp, length);
      if (p == NULL)
	return;

      if (unlikely (is_char4_unit (dtp)))
	{
	  gfc_char4_t *p4 = (gfc_char4_t *) p;
	  memset4 (p4, ' ', length);
	}
      else
	memset (p, ' ', length);

      /* Now that the current record has been padded out,
	 determine where the next record in the array is. */
      record = next_array_record (dtp, dtp->u.p.current_unit->ls,
				  &finished);
      if (finished)
	dtp->u.p.current_unit->endfile = AT_ENDFILE;
      else
	{
	  /* Now seek to this record */
	  record = record * dtp->u.p.current_unit->recl;

	  if (sseek (dtp->u.p.current_unit->s, record, SEEK_SET) < 0)
	    {
	      generate_error (&dtp->common, LIBERROR_INTERNAL_UNIT, NULL);
	      return;
	    }

	  dtp->u.p.current_unit->bytes_left = dtp->u.p.current_unit->recl;
	}
    }
  else
    write_character (dtp, " ", 1, 1);
}


static namelist_info *
nml_write_obj (st_parameter_dt *dtp, namelist_info * obj, index_type offset,
	       namelist_info * base, char * base_name)
{
  int rep_ctr;
  int num;
  int nml_carry;
  int len;
  index_type obj_size;
  index_type nelem;
  size_t dim_i;
  size_t clen;
  index_type elem_ctr;
  size_t obj_name_len;
  void * p ;
  char cup;
  char * obj_name;
  char * ext_name;
  size_t ext_name_len;
  char rep_buff[NML_DIGITS];
  namelist_info * cmp;
  namelist_info * retval = obj->next;
  size_t base_name_len;
  size_t base_var_name_len;
  size_t tot_len;
  unit_delim tmp_delim;
  
  /* Set the character to be used to separate values
     to a comma or semi-colon.  */

  char semi_comma =
	dtp->u.p.current_unit->decimal_status == DECIMAL_POINT ? ',' : ';';

  /* Write namelist variable names in upper case. If a derived type,
     nothing is output.  If a component, base and base_name are set.  */

  if (obj->type != BT_DERIVED)
    {
      namelist_write_newline (dtp);
      write_character (dtp, " ", 1, 1);

      len = 0;
      if (base)
	{
	  len = strlen (base->var_name);
	  base_name_len = strlen (base_name);
	  for (dim_i = 0; dim_i < base_name_len; dim_i++)
            {
	      cup = toupper ((int) base_name[dim_i]);
	      write_character (dtp, &cup, 1, 1);
            }
	}
      clen = strlen (obj->var_name);
      for (dim_i = len; dim_i < clen; dim_i++)
	{
	  cup = toupper ((int) obj->var_name[dim_i]);
	  write_character (dtp, &cup, 1, 1);
	}
      write_character (dtp, "=", 1, 1);
    }

  /* Counts the number of data output on a line, including names.  */

  num = 1;

  len = obj->len;

  switch (obj->type)
    {

    case BT_REAL:
      obj_size = size_from_real_kind (len);
      break;

    case BT_COMPLEX:
      obj_size = size_from_complex_kind (len);
      break;

    case BT_CHARACTER:
      obj_size = obj->string_length;
      break;

    default:
      obj_size = len;      
    }

  if (obj->var_rank)
    obj_size = obj->size;

  /* Set the index vector and count the number of elements.  */

  nelem = 1;
  for (dim_i = 0; dim_i < (size_t) obj->var_rank; dim_i++)
    {
      obj->ls[dim_i].idx = GFC_DESCRIPTOR_LBOUND(obj, dim_i);
      nelem = nelem * GFC_DESCRIPTOR_EXTENT (obj, dim_i);
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
	  (obj->type != BT_DERIVED) &&
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
	      snprintf(rep_buff, NML_DIGITS, " %d*", rep_ctr);
	      write_character (dtp, rep_buff, 1, strlen (rep_buff));
	      dtp->u.p.no_leading_blank = 1;
	    }
	  num++;

	  /* Output the data, if an intrinsic type, or recurse into this
	     routine to treat derived types.  */

	  switch (obj->type)
	    {

	    case BT_INTEGER:
	      write_integer (dtp, p, len);
              break;

	    case BT_LOGICAL:
	      write_logical (dtp, p, len);
              break;

	    case BT_CHARACTER:
	      tmp_delim = dtp->u.p.current_unit->delim_status;
	      if (dtp->u.p.nml_delim == '"')
		dtp->u.p.current_unit->delim_status = DELIM_QUOTE;
	      if (dtp->u.p.nml_delim == '\'')
		dtp->u.p.current_unit->delim_status = DELIM_APOSTROPHE;
	      write_character (dtp, p, 1, obj->string_length);
		dtp->u.p.current_unit->delim_status = tmp_delim;
              break;

	    case BT_REAL:
	      write_real (dtp, p, len);
              break;

	   case BT_COMPLEX:
	      dtp->u.p.no_leading_blank = 0;
	      num++;
              write_complex (dtp, p, len, obj_size);
              break;

	    case BT_DERIVED:

	      /* To treat a derived type, we need to build two strings:
		 ext_name = the name, including qualifiers that prepends
			    component names in the output - passed to
			    nml_write_obj.
		 obj_name = the derived type name with no qualifiers but %
			    appended.  This is used to identify the
			    components.  */

	      /* First ext_name => get length of all possible components  */

	      base_name_len = base_name ? strlen (base_name) : 0;
	      base_var_name_len = base ? strlen (base->var_name) : 0;
	      ext_name_len = base_name_len + base_var_name_len 
		+ strlen (obj->var_name) + obj->var_rank * NML_DIGITS + 1;
	      ext_name = (char*)xmalloc (ext_name_len);

	      memcpy (ext_name, base_name, base_name_len);
	      clen = strlen (obj->var_name + base_var_name_len);
	      memcpy (ext_name + base_name_len, 
		      obj->var_name + base_var_name_len, clen);
	      
	      /* Append the qualifier.  */

	      tot_len = base_name_len + clen;
	      for (dim_i = 0; dim_i < (size_t) obj->var_rank; dim_i++)
		{
		  if (!dim_i)
		    {
		      ext_name[tot_len] = '(';
		      tot_len++;
		    }
		  snprintf (ext_name + tot_len, ext_name_len - tot_len, "%d", 
			    (int) obj->ls[dim_i].idx);
		  tot_len += strlen (ext_name + tot_len);
		  ext_name[tot_len] = ((int) dim_i == obj->var_rank - 1) ? ')' : ',';
		  tot_len++;
		}

	      ext_name[tot_len] = '\0';

	      /* Now obj_name.  */

	      obj_name_len = strlen (obj->var_name) + 1;
	      obj_name = xmalloc (obj_name_len+1);
	      memcpy (obj_name, obj->var_name, obj_name_len-1);
	      memcpy (obj_name + obj_name_len-1, "%", 2);

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

	      free (obj_name);
	      free (ext_name);
	      goto obj_loop;

            default:
	      internal_error (&dtp->common, "Bad type for namelist write");
            }

	  /* Reset the leading blank suppression, write a comma (or semi-colon)
	     and, if 5 values have been output, write a newline and advance
	     to column 2. Reset the repeat counter.  */

	  dtp->u.p.no_leading_blank = 0;
	  write_character (dtp, &semi_comma, 1, 1);
	  if (num > 5)
	    {
	      num = 0;
	      namelist_write_newline (dtp);
	      write_character (dtp, " ", 1, 1);
	    }
	  rep_ctr = 1;
	}

    /* Cycle through and increment the index vector.  */

obj_loop:

    nml_carry = 1;
    for (dim_i = 0; nml_carry && (dim_i < (size_t) obj->var_rank); dim_i++)
      {
	obj->ls[dim_i].idx += nml_carry ;
	nml_carry = 0;
 	if (obj->ls[dim_i].idx  > GFC_DESCRIPTOR_UBOUND(obj,dim_i))
	  {
 	    obj->ls[dim_i].idx = GFC_DESCRIPTOR_LBOUND(obj,dim_i);
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
  unit_delim tmp_delim = DELIM_UNSPECIFIED;

  /* Set the delimiter for namelist output.  */
  tmp_delim = dtp->u.p.current_unit->delim_status;

  dtp->u.p.nml_delim = tmp_delim == DELIM_APOSTROPHE ? '\'' : '"';

  /* Temporarily disable namelist delimters.  */
  dtp->u.p.current_unit->delim_status = DELIM_NONE;

  write_character (dtp, "&", 1, 1);

  /* Write namelist name in upper case - f95 std.  */
  for (i = 0 ;i < dtp->namelist_name_len ;i++ )
    {
      c = toupper ((int) dtp->namelist_name[i]);
      write_character (dtp, &c, 1 ,1);
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

  namelist_write_newline (dtp);
  write_character (dtp, " /", 1, 2);
  /* Restore the original delimiter.  */
  dtp->u.p.current_unit->delim_status = tmp_delim;
}

#undef NML_DIGITS
