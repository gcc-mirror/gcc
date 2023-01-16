/* Copyright (C) 2002-2023 Free Software Foundation, Inc.
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
#include "fbuf.h"
#include "format.h"
#include "unix.h"
#include <assert.h>
#include <string.h>

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


/* Check the first character in source if we are using CC_FORTRAN
   and set the cc.type appropriately.   The cc.type is used later by write_cc
   to determine the output start-of-record, and next_record_cc to determine the
   output end-of-record.
   This function is called before the output buffer is allocated, so alloc_len
   is set to the appropriate size to allocate.  */

static void
write_check_cc (st_parameter_dt *dtp, const char **source, size_t *alloc_len)
{
  /* Only valid for CARRIAGECONTROL=FORTRAN.  */
  if (dtp->u.p.current_unit->flags.cc != CC_FORTRAN
      || alloc_len == NULL || source == NULL)
    return;

  /* Peek at the first character.  */
  int c = (*alloc_len > 0) ? (*source)[0] : EOF;
  if (c != EOF)
    {
      /* The start-of-record character which will be printed.  */
      dtp->u.p.cc.u.start = '\n';
      /* The number of characters to print at the start-of-record.
	 len  > 1 means copy the SOR character multiple times.
	 len == 0 means no SOR will be output.  */
      dtp->u.p.cc.len = 1;

      switch (c)
	{
	case '+':
	  dtp->u.p.cc.type = CCF_OVERPRINT;
	  dtp->u.p.cc.len = 0;
	  break;
	case '-':
	  dtp->u.p.cc.type = CCF_ONE_LF;
	  dtp->u.p.cc.len = 1;
	  break;
	case '0':
	  dtp->u.p.cc.type = CCF_TWO_LF;
	  dtp->u.p.cc.len = 2;
	  break;
	case '1':
	  dtp->u.p.cc.type = CCF_PAGE_FEED;
	  dtp->u.p.cc.len = 1;
	  dtp->u.p.cc.u.start = '\f';
	  break;
	case '$':
	  dtp->u.p.cc.type = CCF_PROMPT;
	  dtp->u.p.cc.len = 1;
	  break;
	case '\0':
	  dtp->u.p.cc.type = CCF_OVERPRINT_NOA;
	  dtp->u.p.cc.len = 0;
	  break;
	default:
	  /* In the default case we copy ONE_LF.  */
	  dtp->u.p.cc.type = CCF_DEFAULT;
	  dtp->u.p.cc.len = 1;
	  break;
      }

      /* We add n-1 to alloc_len so our write buffer is the right size.
	 We are replacing the first character, and possibly prepending some
	 additional characters.  Note for n==0, we actually subtract one from
	 alloc_len, which is correct, since that character is skipped.  */
      if (*alloc_len > 0)
	{
	  *source += 1;
	  *alloc_len += dtp->u.p.cc.len - 1;
	}
      /* If we have no input, there is no first character to replace.  Make
	 sure we still allocate enough space for the start-of-record string.  */
      else
	*alloc_len = dtp->u.p.cc.len;
    }
}


/* Write the start-of-record character(s) for CC_FORTRAN.
   Also adjusts the 'cc' struct to contain the end-of-record character
   for next_record_cc.
   The source_len is set to the remaining length to copy from the source,
   after the start-of-record string was inserted.  */

static char *
write_cc (st_parameter_dt *dtp, char *p, size_t *source_len)
{
  /* Only valid for CARRIAGECONTROL=FORTRAN.  */
  if (dtp->u.p.current_unit->flags.cc != CC_FORTRAN || source_len == NULL)
    return p;

  /* Write the start-of-record string to the output buffer.  Note that len is
     never more than 2.  */
  if (dtp->u.p.cc.len > 0)
    {
      *(p++) = dtp->u.p.cc.u.start;
      if (dtp->u.p.cc.len > 1)
	  *(p++) = dtp->u.p.cc.u.start;

      /* source_len comes from write_check_cc where it is set to the full
	 allocated length of the output buffer. Therefore we subtract off the
	 length of the SOR string to obtain the remaining source length.  */
      *source_len -= dtp->u.p.cc.len;
    }

  /* Common case.  */
  dtp->u.p.cc.len = 1;
  dtp->u.p.cc.u.end = '\r';

  /* Update end-of-record character for next_record_w.  */
  switch (dtp->u.p.cc.type)
    {
    case CCF_PROMPT:
    case CCF_OVERPRINT_NOA:
      /* No end-of-record.  */
      dtp->u.p.cc.len = 0;
      dtp->u.p.cc.u.end = '\0';
      break;
    case CCF_OVERPRINT:
    case CCF_ONE_LF:
    case CCF_TWO_LF:
    case CCF_PAGE_FEED:
    case CCF_DEFAULT:
    default:
      /* Carriage return.  */
      dtp->u.p.cc.len = 1;
      dtp->u.p.cc.u.end = '\r';
      break;
    }

  return p;
}

void

write_a (st_parameter_dt *dtp, const fnode *f, const char *source, size_t len)
{
  size_t wlen;
  char *p;

  wlen = f->u.string.length < 0
	 || (f->format == FMT_G && f->u.string.length == 0)
    ? len : (size_t) f->u.string.length;

#ifdef HAVE_CRLF
  /* If this is formatted STREAM IO convert any embedded line feed characters
     to CR_LF on systems that use that sequence for newlines.  See F2003
     Standard sections 10.6.3 and 9.9 for further information.  */
  if (is_stream_io (dtp))
    {
      const char crlf[] = "\r\n";
      size_t q, bytes;
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
      for (size_t i = 0; i < wlen; i++)
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
      if (dtp->u.p.current_unit->flags.cc == CC_FORTRAN)
	write_check_cc (dtp, &source, &wlen);

      p = write_block (dtp, wlen);
      if (p == NULL)
	return;

      if (dtp->u.p.current_unit->flags.cc == CC_FORTRAN)
	p = write_cc (dtp, p, &wlen);

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
write_a_char4 (st_parameter_dt *dtp, const fnode *f, const char *source, size_t len)
{
  size_t wlen;
  gfc_char4_t *q;

  wlen = f->u.string.length < 0
	 || (f->format == FMT_G && f->u.string.length == 0)
    ? len : (size_t) f->u.string.length;

  q = (gfc_char4_t *) source;
#ifdef HAVE_CRLF
  /* If this is formatted STREAM IO convert any embedded line feed characters
     to CR_LF on systems that use that sequence for newlines.  See F2003
     Standard sections 10.6.3 and 9.9 for further information.  */
  if (is_stream_io (dtp))
    {
      const gfc_char4_t crlf[] = {0x000d,0x000a};
      size_t bytes;
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
      for (size_t i = 0; i < wlen; i++)
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
# ifdef HAVE_GFC_REAL_17
    case 17:
      {
	GFC_INTEGER_16 tmp = 0;
	memcpy ((void *) &tmp, p, 16);
	i = (GFC_UINTEGER_16) tmp;
      }
      break;
# endif
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
write_boz (st_parameter_dt *dtp, const fnode *f, const char *q, int n, int len)
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

  if (w == DEFAULT_WIDTH)
    w = default_width_for_integer (len);

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
	       int len)
{
  GFC_INTEGER_LARGEST n = 0;
  GFC_UINTEGER_LARGEST absn;
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
    /* Use unsigned to protect from overflow. */
    absn = -(GFC_UINTEGER_LARGEST) n;
  else
    absn = n;
  nsign = sign == S_NONE ? 0 : 1;

  /* gfc_itoa() converts the nonnegative value to decimal representation.  */
  q = gfc_itoa (absn, itoa_buf, sizeof (itoa_buf));
  digits = strlen (q);

  /* Select a width if none was specified.  The idea here is to always
     print something.  */
  if (w == DEFAULT_WIDTH)
    w = default_width_for_integer (len);

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
      gfc_char4_t *p4 = (gfc_char4_t *)p;
      if (nblank < 0)
	{
	  memset4 (p4, '*', w);
	  goto done;
	}

      if (!dtp->u.p.namelist_mode)
	{
	  memset4 (p4, ' ', nblank);
	  p4 += nblank;
	}

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

      if (dtp->u.p.namelist_mode)
	{
	  p4 += digits;
	  memset4 (p4, ' ', nblank);
	}
    }

  if (nblank < 0)
    {
      star_fill (p, w);
      goto done;
    }

  if (!dtp->u.p.namelist_mode)
    {
      memset (p, ' ', nblank);
      p += nblank;
    }

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

  if (dtp->u.p.namelist_mode)
    {
      p += digits;
      memset (p, ' ', nblank);
    }

 done:
  return;
}


/* Convert hexadecimal to ASCII.  */

static const char *
xtoa (GFC_UINTEGER_LARGEST n, char *buffer, size_t len)
{
  int digit;
  char *p;

  assert (len >= GFC_XTOA_BUF_SIZE);

  if (n == 0)
    return "0";

  p = buffer + GFC_XTOA_BUF_SIZE - 1;
  *p = '\0';

  while (n != 0)
    {
      digit = n & 0xF;
      if (digit > 9)
	digit += 'A' - '0' - 10;

      *--p = '0' + digit;
      n >>= 4;
    }

  return p;
}


/* Convert unsigned octal to ASCII.  */

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


/* Convert unsigned binary to ASCII.  */

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

/* The following three functions, btoa_big, otoa_big, and xtoa_big, are needed
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
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
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

  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
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

/* Conversion to hexadecimal.  */

static const char *
xtoa_big (const char *s, char *buffer, int len, GFC_UINTEGER_LARGEST *n)
{
  static char a[16] = {'0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};

  char *q;
  uint8_t h, l;
  int i;

  q = buffer;

  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
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

  /* write_z, which calls xtoa_big, is called from transfer.c,
     formatted_transfer_scalar_write.  There it is passed the kind as
     argument, which means a maximum of 16.  The buffer is large
     enough, but the compiler does not know that, so shut up the
     warning here.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstringop-overflow"
  *q = '\0';
#pragma GCC diagnostic pop

  if (*n == 0)
    return "0";

  /* Move past any leading zeros.  */
  while (*buffer == '0')
    buffer++;

  return buffer;
}


void
write_i (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_decimal (dtp, f, p, len);
}


void
write_b (st_parameter_dt *dtp, const fnode *f, const char *source, int len)
{
  const char *p;
  char itoa_buf[GFC_BTOA_BUF_SIZE];
  GFC_UINTEGER_LARGEST n = 0;

  /* Ensure we end up with a null terminated string.  */
  memset(itoa_buf, '\0', GFC_BTOA_BUF_SIZE);

  if (len > (int) sizeof (GFC_UINTEGER_LARGEST))
    {
      p = btoa_big (source, itoa_buf, len, &n);
      write_boz (dtp, f, p, n, len);
    }
  else
    {
      n = extract_uint (source, len);
      p = btoa (n, itoa_buf, sizeof (itoa_buf));
      write_boz (dtp, f, p, n, len);
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
      write_boz (dtp, f, p, n, len);
    }
  else
    {
      n = extract_uint (source, len);
      p = otoa (n, itoa_buf, sizeof (itoa_buf));
      write_boz (dtp, f, p, n, len);
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
      p = xtoa_big (source, itoa_buf, len, &n);
      write_boz (dtp, f, p, n, len);
    }
  else
    {
      n = extract_uint (source, len);
      p = xtoa (n, itoa_buf, sizeof (itoa_buf));
      write_boz (dtp, f, p, n, len);
    }
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
write_integer (st_parameter_dt *dtp, const char *source, int kind)
{
  int width;
  fnode f;

  switch (kind)
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

    case 16:
      width = 40;
      break;

    default:
      width = 0;
      break;
    }
  f.u.integer.w = width;
  f.u.integer.m = -1;
  f.format = FMT_NONE;
  write_decimal (dtp, &f, source, kind);
}


/* Write a list-directed string.  We have to worry about delimiting
   the strings if the file has been opened in that mode.  */

#define DELIM 1
#define NODELIM 0

static void
write_character (st_parameter_dt *dtp, const char *source, int kind, size_t length, int mode)
{
  size_t extra;
  char *p, d;

  if (mode == DELIM)
    {
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
    }
  else
    d = ' ';

  if (kind == 1)
    {
      if (d == ' ')
	extra = 0;
      else
	{
	  extra = 2;

	  for (size_t i = 0; i < length; i++)
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

	      for (size_t i = 0; i < length; i++)
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

	  for (size_t i = 0; i < length; i++)
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

/* Floating point helper functions.  */

#define BUF_STACK_SZ 384

static int
get_precision (st_parameter_dt *dtp, const fnode *f, const char *source, int kind)
{
  if (f->format != FMT_EN)
    return determine_precision (dtp, f, kind);
  else
    return determine_en_precision (dtp, f, source, kind);
}

/* 4932 is the maximum exponent of long double and quad precision, 3
   extra characters for the sign, the decimal point, and the
   trailing null.  Extra digits are added by the calling functions for
   requested precision. Likewise for float and double.  F0 editing produces
   full precision output.  */
static int
size_from_kind (st_parameter_dt *dtp, const fnode *f, int kind)
{
  int size;

  if ((f->format == FMT_F && f->u.real.w == 0) || f->u.real.w == DEFAULT_WIDTH)
    {
      switch (kind)
      {
	case 4:
	  size = 38 + 3; /* These constants shown for clarity.  */
	  break;
	case 8:
	  size = 308 + 3;
	  break;
	case 10:
	  size = 4932 + 3;
	  break;
	case 16:
#ifdef HAVE_GFC_REAL_17
	case 17:
#endif
	  size = 4932 + 3;
	  break;
	default:
	  internal_error (&dtp->common, "bad real kind");
	  break;
      }
    }
  else
    size = f->u.real.w + 1; /* One byte for a NULL character.  */

  return size;
}

static char *
select_buffer (st_parameter_dt *dtp, const fnode *f, int precision,
	       char *buf, size_t *size, int kind)
{
  char *result;
  
  /* The buffer needs at least one more byte to allow room for
     normalizing and 1 to hold null terminator.  */
  *size = size_from_kind (dtp, f, kind) + precision + 1 + 1;

  if (*size > BUF_STACK_SZ)
     result = xmalloc (*size);
  else
     result = buf;
  return result;
}

static char *
select_string (st_parameter_dt *dtp, const fnode *f, char *buf, size_t *size,
	       int kind)
{
  char *result;
  *size = size_from_kind (dtp, f, kind) + f->u.real.d + 1;
  if (*size > BUF_STACK_SZ)
     result = xmalloc (*size);
  else
     result = buf;
  return result;
}

static void
write_float_string (st_parameter_dt *dtp, char *fstr, size_t len)
{
  char *p = write_block (dtp, len);
  if (p == NULL)
    return;

  if (unlikely (is_char4_unit (dtp)))
    {
      gfc_char4_t *p4 = (gfc_char4_t *) p;
      memcpy4 (p4, fstr, len);
      return;
    }
  memcpy (p, fstr, len);
}


static void
write_float_0 (st_parameter_dt *dtp, const fnode *f, const char *source, int kind)
{
  char buf_stack[BUF_STACK_SZ];
  char str_buf[BUF_STACK_SZ];
  char *buffer, *result;
  size_t buf_size, res_len, flt_str_len;

  /* Precision for snprintf call.  */
  int precision = get_precision (dtp, f, source, kind);

  /* String buffer to hold final result.  */
  result = select_string (dtp, f, str_buf, &res_len, kind);

  buffer = select_buffer (dtp, f, precision, buf_stack, &buf_size, kind);

  get_float_string (dtp, f, source , kind, 0, buffer,
                           precision, buf_size, result, &flt_str_len);
  write_float_string (dtp, result, flt_str_len);

  if (buf_size > BUF_STACK_SZ)
    free (buffer);
  if (res_len > BUF_STACK_SZ)
    free (result);
}

void
write_d (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float_0 (dtp, f, p, len);
}


void
write_e (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float_0 (dtp, f, p, len);
}


void
write_f (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float_0 (dtp, f, p, len);
}


void
write_en (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float_0 (dtp, f, p, len);
}


void
write_es (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float_0 (dtp, f, p, len);
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
      /* Adjust decimal precision depending on binary precision, 106 or 113.  */
#if GFC_REAL_16_DIGITS == 113
      f->u.real.w = 45;
      f->u.real.d = 36;
      f->u.real.e = 4;
#else
      f->u.real.w = 41;
      f->u.real.d = 32;
      f->u.real.e = 4;
#endif
      break;
#ifdef HAVE_GFC_REAL_17
    case 17:
      f->u.real.w = 45;
      f->u.real.d = 36;
      f->u.real.e = 4;
      break;
#endif
    default:
      internal_error (&dtp->common, "bad real kind");
      break;
    }
}

/* Output a real number with default format.
   To guarantee that a binary -> decimal -> binary roundtrip conversion
   recovers the original value, IEEE 754-2008 requires 9, 17, 21 and 36
   significant digits for REAL kinds 4, 8, 10, and 16, respectively.
   Thus, we use 1PG16.9E2 for REAL(4), 1PG25.17E3 for REAL(8), 1PG30.21E4
   for REAL(10) and 1PG45.36E4 for REAL(16). The exception is that the
   Fortran standard requires outputting an extra digit when the scale
   factor is 1 and when the magnitude of the value is such that E
   editing is used. However, gfortran compensates for this, and thus
   for list formatted the same number of significant digits is
   generated both when using F and E editing.  */

void
write_real (st_parameter_dt *dtp, const char *source, int kind)
{
  fnode f ;
  char buf_stack[BUF_STACK_SZ];
  char str_buf[BUF_STACK_SZ];
  char *buffer, *result;
  size_t buf_size, res_len, flt_str_len;
  int orig_scale = dtp->u.p.scale_factor;
  dtp->u.p.scale_factor = 1;
  set_fnode_default (dtp, &f, kind);

  /* Precision for snprintf call.  */
  int precision = get_precision (dtp, &f, source, kind);

  /* String buffer to hold final result.  */
  result = select_string (dtp, &f, str_buf, &res_len, kind);

  /* Scratch buffer to hold final result.  */
  buffer = select_buffer (dtp, &f, precision, buf_stack, &buf_size, kind);
  
  get_float_string (dtp, &f, source , kind, 1, buffer,
                           precision, buf_size, result, &flt_str_len);
  write_float_string (dtp, result, flt_str_len);

  dtp->u.p.scale_factor = orig_scale;
  if (buf_size > BUF_STACK_SZ)
    free (buffer);
  if (res_len > BUF_STACK_SZ)
    free (result);
}

/* Similar to list formatted REAL output, for kPG0 where k > 0 we
   compensate for the extra digit.  */

void
write_real_w0 (st_parameter_dt *dtp, const char *source, int kind,
	       const fnode* f)
{
  fnode ff;
  char buf_stack[BUF_STACK_SZ];
  char str_buf[BUF_STACK_SZ];
  char *buffer, *result;
  size_t buf_size, res_len, flt_str_len;
  int comp_d = 0;

  set_fnode_default (dtp, &ff, kind);

  if (f->u.real.d > 0)
    ff.u.real.d = f->u.real.d;
  ff.format = f->format;

  /* For FMT_G, Compensate for extra digits when using scale factor, d
     is not specified, and the magnitude is such that E editing
     is used.  */
  if (f->format == FMT_G)
    {
      if (dtp->u.p.scale_factor > 0 && f->u.real.d == 0)
	comp_d = 1;
      else
	comp_d = 0;
    }

  if (f->u.real.e >= 0)
    ff.u.real.e = f->u.real.e;

  dtp->u.p.g0_no_blanks = 1;

  /* Precision for snprintf call.  */
  int precision = get_precision (dtp, &ff, source, kind);

  /* String buffer to hold final result.  */
  result = select_string (dtp, &ff, str_buf, &res_len, kind);

  buffer = select_buffer (dtp, &ff, precision, buf_stack, &buf_size, kind);

  get_float_string (dtp, &ff, source , kind, comp_d, buffer,
		    precision, buf_size, result, &flt_str_len);
  write_float_string (dtp, result, flt_str_len);

  dtp->u.p.g0_no_blanks = 0;
  if (buf_size > BUF_STACK_SZ)
    free (buffer);
  if (res_len > BUF_STACK_SZ)
    free (result);
}


static void
write_complex (st_parameter_dt *dtp, const char *source, int kind, size_t size)
{
  char semi_comma =
	dtp->u.p.current_unit->decimal_status == DECIMAL_POINT ? ',' : ';';

  /* Set for no blanks so we get a string result with no leading
     blanks.  We will pad left later.  */
  dtp->u.p.g0_no_blanks = 1;

  fnode f ;
  char buf_stack[BUF_STACK_SZ];
  char str1_buf[BUF_STACK_SZ];
  char str2_buf[BUF_STACK_SZ];
  char *buffer, *result1, *result2;
  size_t buf_size, res_len1, res_len2, flt_str_len1, flt_str_len2;
  int width, lblanks, orig_scale = dtp->u.p.scale_factor;

  dtp->u.p.scale_factor = 1;
  set_fnode_default (dtp, &f, kind);

  /* Set width for two values, parenthesis, and comma.  */
  width = 2 * f.u.real.w + 3;

  /* Set for no blanks so we get a string result with no leading
     blanks.  We will pad left later.  */
  dtp->u.p.g0_no_blanks = 1;

  /* Precision for snprintf call.  */
  int precision = get_precision (dtp, &f, source, kind);

  /* String buffers to hold final result.  */
  result1 = select_string (dtp, &f, str1_buf, &res_len1, kind);
  result2 = select_string (dtp, &f, str2_buf, &res_len2, kind);

  buffer = select_buffer (dtp, &f, precision, buf_stack, &buf_size, kind);

  get_float_string (dtp, &f, source , kind, 0, buffer,
                           precision, buf_size, result1, &flt_str_len1);
  get_float_string (dtp, &f, source + size / 2 , kind, 0, buffer,
                           precision, buf_size, result2, &flt_str_len2);
  if (!dtp->u.p.namelist_mode)
    {
      lblanks = width - flt_str_len1 - flt_str_len2 - 3;
      write_x (dtp, lblanks, lblanks);
    }
  write_char (dtp, '(');
  write_float_string (dtp, result1, flt_str_len1);
  write_char (dtp, semi_comma);
  write_float_string (dtp, result2, flt_str_len2);
  write_char (dtp, ')');

  dtp->u.p.scale_factor = orig_scale;
  dtp->u.p.g0_no_blanks = 0;
  if (buf_size > BUF_STACK_SZ)
    free (buffer);
  if (res_len1 > BUF_STACK_SZ)
    free (result1);
  if (res_len2 > BUF_STACK_SZ)
    free (result2);
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
      if (dtp->u.p.current_unit->flags.cc != CC_FORTRAN)
	write_char (dtp, ' ');
    }
  else
    {
      if (type != BT_CHARACTER || !dtp->u.p.char_flag ||
	  (dtp->u.p.current_unit->delim_status != DELIM_NONE
	   && dtp->u.p.current_unit->delim_status != DELIM_UNSPECIFIED))
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
      write_character (dtp, p, kind, size, DELIM);
      break;
    case BT_REAL:
      write_real (dtp, p, kind);
      break;
    case BT_COMPLEX:
      write_complex (dtp, p, kind, size);
      break;
    case BT_CLASS:
      {
	  int unit = dtp->u.p.current_unit->unit_number;
	  char iotype[] = "LISTDIRECTED";
	  gfc_charlen_type iotype_len = 12;
	  char tmp_iomsg[IOMSG_LEN] = "";
	  char *child_iomsg;
	  gfc_charlen_type child_iomsg_len;
	  int noiostat;
	  int *child_iostat = NULL;
	  gfc_full_array_i4 vlist;

	  GFC_DESCRIPTOR_DATA(&vlist) = NULL;
	  GFC_DIMENSION_SET(vlist.dim[0],1, 0, 0);

	  /* Set iostat, intent(out).  */
	  noiostat = 0;
	  child_iostat = (dtp->common.flags & IOPARM_HAS_IOSTAT) ?
			  dtp->common.iostat : &noiostat;

	  /* Set iomsge, intent(inout).  */
	  if (dtp->common.flags & IOPARM_HAS_IOMSG)
	    {
	      child_iomsg = dtp->common.iomsg;
	      child_iomsg_len = dtp->common.iomsg_len;
	    }
	  else
	    {
	      child_iomsg = tmp_iomsg;
	      child_iomsg_len = IOMSG_LEN;
	    }

	  /* Call the user defined formatted WRITE procedure.  */
	  dtp->u.p.current_unit->child_dtio++;
	  dtp->u.p.fdtio_ptr (p, &unit, iotype, &vlist,
			      child_iostat, child_iomsg,
			      iotype_len, child_iomsg_len);
	  dtp->u.p.current_unit->child_dtio--;
      }
      break;
    default:
      internal_error (&dtp->common, "list_formatted_write(): Bad type");
    }

  fbuf_flush_list (dtp->u.p.current_unit, LIST_WRITING);
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
      write_character (dtp, "\r\n", 1, 2, NODELIM);
#else
      write_character (dtp, "\n", 1, 1, NODELIM);
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
    write_character (dtp, " ", 1, 1, NODELIM);
}


static namelist_info *
nml_write_obj (st_parameter_dt *dtp, namelist_info *obj, index_type offset,
	       namelist_info *base, char *base_name)
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
  void *p;
  char cup;
  char *obj_name;
  char *ext_name;
  char *q;
  size_t ext_name_len;
  char rep_buff[NML_DIGITS];
  namelist_info *cmp;
  namelist_info *retval = obj->next;
  size_t base_name_len;
  size_t base_var_name_len;
  size_t tot_len;

  /* Set the character to be used to separate values
     to a comma or semi-colon.  */

  char semi_comma =
	dtp->u.p.current_unit->decimal_status == DECIMAL_POINT ? ',' : ';';

  /* Write namelist variable names in upper case. If a derived type,
     nothing is output.  If a component, base and base_name are set.  */

  if (obj->type != BT_DERIVED || obj->dtio_sub != NULL)
    {
      namelist_write_newline (dtp);
      write_character (dtp, " ", 1, 1, NODELIM);

      len = 0;
      if (base)
	{
	  len = strlen (base->var_name);
	  base_name_len = strlen (base_name);
	  for (dim_i = 0; dim_i < base_name_len; dim_i++)
            {
	      cup = safe_toupper (base_name[dim_i]);
	      write_character (dtp, &cup, 1, 1, NODELIM);
            }
	}
      clen = strlen (obj->var_name);
      for (dim_i = len; dim_i < clen; dim_i++)
	{
	  cup = safe_toupper (obj->var_name[dim_i]);
	  if (cup == '+')
	    cup = '%';
	  write_character (dtp, &cup, 1, 1, NODELIM);
	}
      write_character (dtp, "=", 1, 1, NODELIM);
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
	  !memcmp (p, (void *)(p + obj_size ), obj_size ))
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
	      write_character (dtp, rep_buff, 1, strlen (rep_buff), NODELIM);
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
	      if (dtp->u.p.current_unit->flags.encoding == ENCODING_UTF8)
		write_character (dtp, p, 4, obj->string_length, DELIM);
	      else
		write_character (dtp, p, 1, obj->string_length, DELIM);
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
	    case BT_CLASS:
	      /* To treat a derived type, we need to build two strings:
		 ext_name = the name, including qualifiers that prepends
			    component names in the output - passed to
			    nml_write_obj.
		 obj_name = the derived type name with no qualifiers but %
			    appended.  This is used to identify the
			    components.  */

	      /* First ext_name => get length of all possible components  */
	      if (obj->dtio_sub != NULL)
		{
		  int unit = dtp->u.p.current_unit->unit_number;
		  char iotype[] = "NAMELIST";
		  gfc_charlen_type iotype_len = 8;
		  char tmp_iomsg[IOMSG_LEN] = "";
		  char *child_iomsg;
		  gfc_charlen_type child_iomsg_len;
		  int noiostat;
		  int *child_iostat = NULL;
		  gfc_full_array_i4 vlist;
		  formatted_dtio dtio_ptr = (formatted_dtio)obj->dtio_sub;

		  GFC_DIMENSION_SET(vlist.dim[0],1, 0, 0);

		  /* Set iostat, intent(out).  */
		  noiostat = 0;
		  child_iostat = (dtp->common.flags & IOPARM_HAS_IOSTAT) ?
				  dtp->common.iostat : &noiostat;

		  /* Set iomsg, intent(inout).  */
		  if (dtp->common.flags & IOPARM_HAS_IOMSG)
		    {
		      child_iomsg = dtp->common.iomsg;
		      child_iomsg_len = dtp->common.iomsg_len;
		    }
		  else
		    {
		      child_iomsg = tmp_iomsg;
		      child_iomsg_len = IOMSG_LEN;
		    }

		  /* Call the user defined formatted WRITE procedure.  */
		  dtp->u.p.current_unit->child_dtio++;
		  if (obj->type == BT_DERIVED)
		    {
		      /* Build a class container.  */
		      gfc_class list_obj;
		      list_obj.data = p;
		      list_obj.vptr = obj->vtable;
		      list_obj.len = 0;
		      dtio_ptr ((void *)&list_obj, &unit, iotype, &vlist,
				child_iostat, child_iomsg,
				iotype_len, child_iomsg_len);
		    }
		  else
		    {
		      dtio_ptr (p, &unit, iotype, &vlist,
				child_iostat, child_iomsg,
				iotype_len, child_iomsg_len);
		    }
		  dtp->u.p.current_unit->child_dtio--;

		  goto obj_loop;
		}

	      base_name_len = base_name ? strlen (base_name) : 0;
	      base_var_name_len = base ? strlen (base->var_name) : 0;
	      ext_name_len = base_name_len + base_var_name_len
		+ strlen (obj->var_name) + obj->var_rank * NML_DIGITS + 1;
	      ext_name = xmalloc (ext_name_len);

	      if (base_name)
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
	      for (q = ext_name; *q; q++)
		if (*q == '+')
		  *q = '%';

	      /* Now obj_name.  */

	      obj_name_len = strlen (obj->var_name) + 1;
	      obj_name = xmalloc (obj_name_len + 1);
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
	  if (obj->type == BT_CHARACTER)
	    {
	      if (dtp->u.p.nml_delim != '\0')
		write_character (dtp, &semi_comma, 1, 1, NODELIM);
	    }
	  else
	    write_character (dtp, &semi_comma, 1, 1, NODELIM);
	  if (num > 5)
	    {
	      num = 0;
	      if (dtp->u.p.nml_delim == '\0')
		write_character (dtp, &semi_comma, 1, 1, NODELIM);
	      namelist_write_newline (dtp);
	      write_character (dtp, " ", 1, 1, NODELIM);
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
  namelist_info *t1, *t2, *dummy = NULL;
  index_type dummy_offset = 0;
  char c;
  char *dummy_name = NULL;

  /* Set the delimiter for namelist output.  */
  switch (dtp->u.p.current_unit->delim_status)
    {
      case DELIM_APOSTROPHE:
        dtp->u.p.nml_delim = '\'';
	break;
      case DELIM_QUOTE:
      case DELIM_UNSPECIFIED:
	dtp->u.p.nml_delim = '"';
	break;
      default:
	dtp->u.p.nml_delim = '\0';
    }

  write_character (dtp, "&", 1, 1, NODELIM);

  /* Write namelist name in upper case - f95 std.  */
  for (gfc_charlen_type i = 0; i < dtp->namelist_name_len; i++ )
    {
      c = safe_toupper (dtp->namelist_name[i]);
      write_character (dtp, &c, 1 ,1, NODELIM);
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
  write_character (dtp, " /", 1, 2, NODELIM);
}

#undef NML_DIGITS
