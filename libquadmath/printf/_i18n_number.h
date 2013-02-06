/* Copyright (C) 2000, 2004, 2008 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@gnu.org>, 2000.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

/* Look up the value of the next multibyte character and return its numerical
   value if it is one of the digits known in the locale.  If *DECIDED is
   -1 this means it is not yet decided which form it is and we have to
   search through all available digits.  Otherwise we know which script
   the digits are from.  */
static inline char *
outdigit_value (char *s, int n)
{
  const char *outdigit;
  size_t dlen;

  assert (0 <= n && n <= 9);
  outdigit = nl_langinfo (_NL_CTYPE_OUTDIGIT0_MB + n);
  dlen = strlen (outdigit);

  s -= dlen;
  while (dlen-- > 0)
    s[dlen] = outdigit[dlen];

  return s;
}

/* Look up the value of the next multibyte character and return its numerical
   value if it is one of the digits known in the locale.  If *DECIDED is
   -1 this means it is not yet decided which form it is and we have to
   search through all available digits.  Otherwise we know which script
   the digits are from.  */
static inline wchar_t
outdigitwc_value (int n)
{
  assert (0 <= n && n <= 9);

  return nl_langinfo_wc (_NL_CTYPE_OUTDIGIT0_WC + n);
}

static char *
_i18n_number_rewrite (char *w, char *rear_ptr, char *end)
{
  char decimal[MB_LEN_MAX + 1];
  char thousands[MB_LEN_MAX + 1];

  /* "to_outpunct" is a map from ASCII decimal point and thousands-sep
     to their equivalent in locale. This is defined for locales which
     use extra decimal point and thousands-sep.  */
  wctrans_t map = wctrans ("to_outpunct");
  wint_t wdecimal = towctrans (L_('.'), map);
  wint_t wthousands = towctrans (L_(','), map);

  if (__builtin_expect (map != NULL, 0))
    {
      mbstate_t state;
      memset (&state, '\0', sizeof (state));

      size_t n = wcrtomb (decimal, wdecimal, &state);
      if (n == (size_t) -1)
	memcpy (decimal, ".", 2);
      else
	decimal[n] = '\0';

      memset (&state, '\0', sizeof (state));

      n = wcrtomb (thousands, wthousands, &state);
      if (n == (size_t) -1)
	memcpy (thousands, ",", 2);
      else
	thousands[n] = '\0';
    }

  /* Copy existing string so that nothing gets overwritten.  */
  char *src;
  int use_alloca = (rear_ptr - w) < 4096;
  if (__builtin_expect (use_alloca, 1))
    src = (char *) alloca (rear_ptr - w);
  else
    {
      src = (char *) malloc (rear_ptr - w);
      if (src == NULL)
	/* If we cannot allocate the memory don't rewrite the string.
	   It is better than nothing.  */
	return w;
    }

  memcpy (src, w, rear_ptr - w);
  char *s = src + (rear_ptr - w);

  w = end;

  /* Process all characters in the string.  */
  while (--s >= src)
    {
      if (*s >= '0' && *s <= '9')
	{
	  if (sizeof (char) == 1)
	    w = (char *) outdigit_value ((char *) w, *s - '0');
	  else
	    *--w = (char) outdigitwc_value (*s - '0');
	}
      else if (__builtin_expect (map == NULL, 1) || (*s != '.' && *s != ','))
	*--w = *s;
      else
	{
	  if (sizeof (char) == 1)
	    {
	      const char *outpunct = *s == '.' ? decimal : thousands;
	      size_t dlen = strlen (outpunct);

	      w -= dlen;
	      while (dlen-- > 0)
		w[dlen] = outpunct[dlen];
	    }
	  else
	    *--w = *s == '.' ? (char) wdecimal : (char) wthousands;
	}
    }

  if (! use_alloca)
    free (src);

  return w;
}
