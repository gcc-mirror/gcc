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

#include <config.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include "quadmath-printf.h"

/* Read a simple integer from a string and update the string pointer.
   It is assumed that the first character is a digit.  */
static unsigned int
read_int (const char **pstr)
{
  unsigned int retval = (unsigned char) **pstr - '0';

  while (isdigit ((unsigned char) *++(*pstr)))
    {
      retval *= 10;
      retval += (unsigned char) **pstr - '0';
    }

  return retval;
}

#define PADSIZE 16
static char const blanks[PADSIZE] =
{' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '};
static char const zeroes[PADSIZE] =
{'0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0'};
static wchar_t const wblanks[PADSIZE] =
{
  L_(' '), L_(' '), L_(' '), L_(' '), L_(' '), L_(' '), L_(' '), L_(' '),
  L_(' '), L_(' '), L_(' '), L_(' '), L_(' '), L_(' '), L_(' '), L_(' ')
};
static wchar_t const wzeroes[PADSIZE] =
{
  L_('0'), L_('0'), L_('0'), L_('0'), L_('0'), L_('0'), L_('0'), L_('0'),
  L_('0'), L_('0'), L_('0'), L_('0'), L_('0'), L_('0'), L_('0'), L_('0')
};

attribute_hidden size_t
__quadmath_do_pad (struct __quadmath_printf_file *fp, int wide, int c,
		   size_t n)
{
  ssize_t i;
  char padbuf[PADSIZE];
  wchar_t wpadbuf[PADSIZE];
  const char *padstr;
  size_t w, written = 0;
  if (wide)
    {
      if (c == ' ')
	padstr = (const char *) wblanks;
      else if (c == '0')
	padstr = (const char *) wzeroes;
      else
	{
	  padstr = (const char *) wpadbuf;
	  for (i = 0; i < PADSIZE; i++)
	    wpadbuf[i] = c;
	}
    }
  else
    {
      if (c == ' ')
	padstr = blanks;
      else if (c == '0')
	padstr = zeroes;
      else
	{
	  padstr = (const char *) padbuf;
	  for (i = 0; i < PADSIZE; i++)
	    padbuf[i] = c;
	}
    }
  for (i = n; i >= PADSIZE; i -= PADSIZE)
    {
      w = PUT (fp, (char *) padstr, PADSIZE);
      written += w;
      if (w != PADSIZE)
	return written;
    }
  if (i > 0)
    {
      w = PUT (fp, (char *) padstr, i);
      written += w;
    }
  return written;
}

/* This is a stripped down version of snprintf, which just handles
   a single %eEfFgGaA format entry with Q modifier.  % has to be
   the first character of the format string, no $ can be used.  */
int
quadmath_snprintf (char *str, size_t size, const char *format, ...)
{
  struct printf_info info;
  va_list ap;
  __float128 fpnum, *fpnum_addr = &fpnum, **fpnum_addr2 = &fpnum_addr;
  struct __quadmath_printf_file qfp;

  if (*format++ != '%')
    return -1;

  /* Clear information structure.  */
  memset (&info, '\0', sizeof info);
  /* info.alt = 0;
  info.space = 0;
  info.left = 0;
  info.showsign = 0;
  info.group = 0;
  info.i18n = 0;
  info.extra = 0; */
  info.pad = ' ';
  /* info.wide = 0; */

  /* Check for spec modifiers.  */
  do
    {
      switch (*format)
	{
	case ' ':
	  /* Output a space in place of a sign, when there is no sign.  */
	  info.space = 1;
	  continue;
	case '+':
	  /* Always output + or - for numbers.  */
	  info.showsign = 1;
	  continue;
	case '-':
	  /* Left-justify things.  */
	  info.left = 1;
	  continue;
	case '#':
	  /* Use the "alternate form":
	     Hex has 0x or 0X, FP always has a decimal point.  */
	  info.alt = 1;
	  continue;
	case '0':
	  /* Pad with 0s.  */
	  info.pad = '0';
	  continue;
	case '\'':
	  /* Show grouping in numbers if the locale information
	     indicates any.  */
	  info.group = 1;
	  continue;
	case 'I':
	  /* Use the internationalized form of the output.  Currently
	     means to use the `outdigits' of the current locale.  */
	  info.i18n = 1;
	  continue;
	default:
	  break;
	}
      break;
    }
  while (*++format);

  if (info.left)
    info.pad = ' ';

  va_start (ap, format);

  /* Get the field width.  */
  /* info.width = 0; */
  if (*format == '*')
    {
      /* The field width is given in an argument.
	 A negative field width indicates left justification.  */
      ++format;
      info.width = va_arg (ap, int);
    }
  else if (isdigit (*format))
    /* Constant width specification.  */
    info.width = read_int (&format);

  /* Get the precision.  */
  /* -1 means none given; 0 means explicit 0.  */
  info.prec = -1;
  if (*format == '.')
    {
      ++format;
      if (*format == '*')
	{
	  /* The precision is given in an argument.  */
	  ++format;

	  info.prec = va_arg (ap, int);
	}
      else if (isdigit (*format))
	info.prec = read_int (&format);
      else
	/* "%.?" is treated like "%.0?".  */
	info.prec = 0;
    }

  /* Check for type modifiers.  */
  /* info.is_long_double = 0;
  info.is_short = 0;
  info.is_long = 0;
  info.is_char = 0;
  info.user = 0; */

  /* We require Q modifier.  */
  if (*format++ != 'Q')
    {
      va_end (ap);
      return -1;
    }

  /* Get the format specification.  */
  info.spec = (wchar_t) *format++;
  if (info.spec == L_('\0') || *format != '\0')
    {
      va_end (ap);
      return -1;
    }

  switch (info.spec)
    {
    case L_('e'):
    case L_('E'):
    case L_('f'):
    case L_('F'):
    case L_('g'):
    case L_('G'):
    case L_('a'):
    case L_('A'):
      break;
    default:
      va_end (ap);
      return -1;
    }

  fpnum = va_arg (ap, __float128);
  va_end (ap);

  qfp.fp = NULL;
  qfp.str = str;
  qfp.size = size ? size - 1 : 0;
  qfp.len = 0;
  qfp.file_p = 0;

  if (info.spec == L_('a') || info.spec == L_('A'))
    __quadmath_printf_fphex (&qfp, &info, (const void *const *)&fpnum_addr2);
  else
    __quadmath_printf_fp (&qfp, &info, (const void *const *)&fpnum_addr2);

  if (size)
    *qfp.str = '\0';

  return qfp.len;
}

#ifdef HAVE_PRINTF_HOOKS
static int pa_flt128;
int mod_Q attribute_hidden;

static void
flt128_va (void *mem, va_list *ap)
{ 
  __float128 d = va_arg (*ap, __float128);
  memcpy (mem, &d, sizeof (d));
}

static int
flt128_ais (const struct printf_info *info, size_t n __attribute__ ((unused)),
	    int *argtype, int *size)
{
  if (info->user & mod_Q)
    {
      argtype[0] = pa_flt128;
      size[0] = sizeof (__float128);
      return 1;
    }
#if __GLIBC__ < 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ <= 13)
  /* Workaround bug in glibc printf hook handling.  */
  size[0] = -1;
  switch (info->spec)
    {
    case L_('i'):
    case L_('d'):
    case L_('u'):
    case L_('o'):
    case L_('X'):
    case L_('x'):
#if __LONG_MAX__ != __LONG_LONG_MAX__
      if (info->is_long_double)
	argtype[0] = PA_INT|PA_FLAG_LONG_LONG;
      else
#endif
      if (info->is_long)
	argtype[0] = PA_INT|PA_FLAG_LONG;
      else if (info->is_short)
	argtype[0] = PA_INT|PA_FLAG_SHORT;
      else if (info->is_char)
	argtype[0] = PA_CHAR;
      else
	argtype[0] = PA_INT;
      return 1;
    case L_('e'):
    case L_('E'):
    case L_('f'):
    case L_('F'):
    case L_('g'):
    case L_('G'):
    case L_('a'):
    case L_('A'):
      if (info->is_long_double)
	argtype[0] = PA_DOUBLE|PA_FLAG_LONG_DOUBLE;
      else
	argtype[0] = PA_DOUBLE;
      return 1;
    case L_('c'):
      argtype[0] = PA_CHAR;
      return 1;
    case L_('C'):
      argtype[0] = PA_WCHAR;
      return 1;
    case L_('s'):
      argtype[0] = PA_STRING;
      return 1;
    case L_('S'):
      argtype[0] = PA_WSTRING;
      return 1;
    case L_('p'):
      argtype[0] = PA_POINTER;
      return 1;
    case L_('n'):
      argtype[0] = PA_INT|PA_FLAG_PTR;
      return 1;

    case L_('m'):
    default:
      /* An unknown spec will consume no args.  */
      return 0;
    }
#endif
  return -1;
}

static int
flt128_printf_fp (FILE *fp, const struct printf_info *info,
		  const void *const *args)
{
  struct __quadmath_printf_file qpf
    = { .fp = fp, .str = NULL, .size = 0, .len = 0, .file_p = 1 };

  if ((info->user & mod_Q) == 0)
    return -2;

  return __quadmath_printf_fp (&qpf, info, args);
}

static int
flt128_printf_fphex (FILE *fp, const struct printf_info *info,
		     const void *const *args)
{
  struct __quadmath_printf_file qpf
    = { .fp = fp, .str = NULL, .size = 0, .len = 0, .file_p = 1 };

  if ((info->user & mod_Q) == 0)
    return -2;

  return __quadmath_printf_fphex (&qpf, info, args);
}

__attribute__((constructor)) static void
register_printf_flt128 (void)
{
  pa_flt128 = register_printf_type (flt128_va);
  if (pa_flt128 == -1)
    return;
  mod_Q = register_printf_modifier (L_("Q"));
  if (mod_Q == -1)
    return;
  register_printf_specifier ('f', flt128_printf_fp, flt128_ais);
  register_printf_specifier ('F', flt128_printf_fp, flt128_ais);
  register_printf_specifier ('e', flt128_printf_fp, flt128_ais);
  register_printf_specifier ('E', flt128_printf_fp, flt128_ais);
  register_printf_specifier ('g', flt128_printf_fp, flt128_ais);
  register_printf_specifier ('G', flt128_printf_fp, flt128_ais);
  register_printf_specifier ('a', flt128_printf_fphex, flt128_ais);
  register_printf_specifier ('A', flt128_printf_fphex, flt128_ais);
}

__attribute__((destructor)) static void
unregister_printf_flt128 (void)
{
  /* No way to unregister printf type and modifier currently,
     and only one printf specifier can be registered right now.  */
  if (pa_flt128 == -1 || mod_Q == -1)
    return;
  register_printf_specifier ('f', NULL, NULL);
  register_printf_specifier ('F', NULL, NULL);
  register_printf_specifier ('e', NULL, NULL);
  register_printf_specifier ('E', NULL, NULL);
  register_printf_specifier ('g', NULL, NULL);
  register_printf_specifier ('G', NULL, NULL);
  register_printf_specifier ('a', NULL, NULL);
  register_printf_specifier ('A', NULL, NULL);
}
#endif
