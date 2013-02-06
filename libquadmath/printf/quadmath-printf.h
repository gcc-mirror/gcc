/* GCC Quad-Precision Math Library
   Copyright (C) 2011 Free Software Foundation, Inc.
   Written by Jakub Jelinek  <jakub@redhat.com>

This file is part of the libquadmath library.
Libquadmath is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libquadmath is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libquadmath; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include <stdlib.h>
#include <stdio.h>
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#ifdef HAVE_LANGINFO_H
#include <langinfo.h>
#endif
#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif
#ifdef HAVE_WCHAR_H
#include <wchar.h>
#endif
#ifdef HAVE_WCTYPE_H
#include <wctype.h>
#endif
#ifdef HAVE_PRINTF_HOOKS
#include <printf.h>
#endif
#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif
#include "quadmath-imp.h"
#include "gmp-impl.h"

#ifdef HAVE_WCHAR_H
#define L_(x) L##x
#else
#define L_(x) x
#undef wchar_t
#undef wint_t
#undef putwc
#undef WEOF
#define wchar_t char
#define wint_t int
#define putwc(c,f) putc(c,f)
#define WEOF EOF
#endif

#ifndef HAVE_CTYPE_H
/* Won't work for EBCDIC.  */
#undef isupper
#undef isdigit
#undef isxdigit
#undef tolower
#define isupper(x) \
  ({__typeof(x) __is_x = (x); __is_x >= 'A' && __is_x <= 'Z'; })
#define isdigit(x) \
  ({__typeof(x) __is_x = (x); __is_x >= '0' && __is_x <= '9'; })
#define isxdigit(x) \
  ({__typeof(x) __is_x = (x); \
    (__is_x >= '0' && __is_x <= '9') \
    || ((x) >= 'A' && (x) <= 'F') \
    || ((x) >= 'a' && (x) <= 'f'); })
#define tolower(x) \
  ({__typeof(x) __is_x = (x); \
    (__is_x >= 'A' && __is_x <= 'Z') ? __is_x - 'A' + 'a' : __is_x; })
#endif

#ifndef CHAR_MAX
#ifdef __CHAR_UNSIGNED__
#define CHAR_MAX (2 * __SCHAR_MAX__ + 1)
#else
#define CHAR_MAX __SCHAR_MAX__
#endif
#endif

#ifndef HAVE_PRINTF_HOOKS
#define printf_info __quadmath_printf_info
struct printf_info
{
  int prec;			/* Precision.  */
  int width;			/* Width.  */
  wchar_t spec;			/* Format letter.  */
  unsigned int is_long_double:1;/* L flag.  */
  unsigned int is_short:1;	/* h flag.  */
  unsigned int is_long:1;	/* l flag.  */
  unsigned int alt:1;		/* # flag.  */
  unsigned int space:1;		/* Space flag.  */
  unsigned int left:1;		/* - flag.  */
  unsigned int showsign:1;	/* + flag.  */
  unsigned int group:1;		/* ' flag.  */
  unsigned int extra:1;		/* For special use.  */
  unsigned int is_char:1;	/* hh flag.  */
  unsigned int wide:1;		/* Nonzero for wide character streams.  */
  unsigned int i18n:1;		/* I flag.  */
  unsigned short int user;	/* Bits for user-installed modifiers.  */
  wchar_t pad;			/* Padding character.  */
};
#endif

struct __quadmath_printf_file
{
  FILE *fp;
  char *str;
  size_t size;
  size_t len;
  int file_p;
};

int
__quadmath_printf_fp (struct __quadmath_printf_file *fp,
		      const struct printf_info *info,
		      const void *const *args) attribute_hidden;
int
__quadmath_printf_fphex (struct __quadmath_printf_file *fp,
			 const struct printf_info *info,
			 const void *const *args) attribute_hidden;

size_t __quadmath_do_pad (struct __quadmath_printf_file *fp, int wide,
			  int c, size_t n) attribute_hidden;

static inline __attribute__((__unused__)) size_t
__quadmath_do_put (struct __quadmath_printf_file *fp, int wide,
		   const char *s, size_t n)
{
  size_t len;
  if (fp->file_p)
    {
      if (wide)
	{
	  size_t cnt;
	  const wchar_t *ls = (const wchar_t *) s;
	  for (cnt = 0; cnt < n; cnt++)
	    if (putwc (ls[cnt], fp->fp) == WEOF)
	      break;
	  return cnt;
	}
      return fwrite (s, 1, n, fp->fp);
    }
  len = MIN (fp->size, n);
  memcpy (fp->str, s, len);
  fp->str += len;
  fp->size -= len;
  fp->len += n;
  return n;
}

static inline __attribute__((__unused__)) int
__quadmath_do_putc (struct __quadmath_printf_file *fp, int wide,
		    wchar_t c)
{
  if (fp->file_p)
    return wide ? (int) putwc (c, fp->fp) : putc (c, fp->fp);
  if (fp->size)
    {
      *(fp->str++) = c;
      fp->size--;
    }
  fp->len++;
  return (unsigned char) c;
}

#define PUT(f, s, n) __quadmath_do_put (f, wide, s, n)
#define PAD(f, c, n) __quadmath_do_pad (f, wide, c, n)
#define PUTC(c, f) __quadmath_do_putc (f, wide, c)

#define nl_langinfo_wc(x) \
  ({ union { const char *mb; wchar_t wc; } u; u.mb = nl_langinfo (x); u.wc; })

#undef _itoa
#define _itoa __quadmath_itoa

#undef NAN
#define NAN __builtin_nanf ("")
