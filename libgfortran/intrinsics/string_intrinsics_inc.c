/* String intrinsics helper functions.
   Copyright (C) 2002-2025 Free Software Foundation, Inc.

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

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


/* Rename the functions.  */
#define concat_string SUFFIX(concat_string)
#define string_len_trim SUFFIX(string_len_trim)
#define adjustl SUFFIX(adjustl)
#define adjustr SUFFIX(adjustr)
#define string_index SUFFIX(string_index)
#define string_scan SUFFIX(string_scan)
#define string_verify SUFFIX(string_verify)
#define string_trim SUFFIX(string_trim)
#define string_minmax SUFFIX(string_minmax)
#define string_split SUFFIX(string_split)
#define zero_length_string SUFFIX(zero_length_string)
#define compare_string SUFFIX(compare_string)


/* The prototypes.  */

extern void concat_string (gfc_charlen_type, CHARTYPE *,
			   gfc_charlen_type, const CHARTYPE *,
			   gfc_charlen_type, const CHARTYPE *);
export_proto(concat_string);

extern void adjustl (CHARTYPE *, gfc_charlen_type, const CHARTYPE *);
export_proto(adjustl);

extern void adjustr (CHARTYPE *, gfc_charlen_type, const CHARTYPE *);
export_proto(adjustr);

extern gfc_charlen_type string_index (gfc_charlen_type, const CHARTYPE *,
				      gfc_charlen_type, const CHARTYPE *,
				      GFC_LOGICAL_4);
export_proto(string_index);

extern gfc_charlen_type string_scan (gfc_charlen_type, const CHARTYPE *,
				     gfc_charlen_type, const CHARTYPE *,
				     GFC_LOGICAL_4);
export_proto(string_scan);

extern gfc_charlen_type string_verify (gfc_charlen_type, const CHARTYPE *,
				       gfc_charlen_type, const CHARTYPE *,
				       GFC_LOGICAL_4);
export_proto(string_verify);

extern void string_trim (gfc_charlen_type *, CHARTYPE **, gfc_charlen_type,
			 const CHARTYPE *);
export_proto(string_trim);

extern void string_minmax (gfc_charlen_type *, CHARTYPE **, int, int, ...);
export_proto(string_minmax);

extern gfc_charlen_type string_split (gfc_charlen_type, const CHARTYPE *,
				      gfc_charlen_type, const CHARTYPE *,
				      gfc_charlen_type, GFC_LOGICAL_4);
export_proto (string_split);

/* Use for functions which can return a zero-length string.  */
static CHARTYPE zero_length_string = 0;


/* Strings of unequal length are extended with pad characters.  */

int
compare_string (gfc_charlen_type len1, const CHARTYPE *s1,
		gfc_charlen_type len2, const CHARTYPE *s2)
{
  const UCHARTYPE *s;
  gfc_charlen_type len;
  int res;

  /* Placate the sanitizer.  */
  if (!s1 && !s2)
    return 0;
  if (!s1)
    return -1;
  if (!s2)
    return 1;

  res = MEMCMP (s1, s2, ((len1 < len2) ? len1 : len2));
  if (res != 0)
    return res;

  if (len1 == len2)
    return 0;

  if (len1 < len2)
    {
      len = len2 - len1;
      s = (UCHARTYPE *) &s2[len1];
      res = -1;
    }
  else
    {
      len = len1 - len2;
      s = (UCHARTYPE *) &s1[len2];
      res = 1;
    }

  while (len--)
    {
      if (*s != ' ')
        {
          if (*s > ' ')
            return res;
          else
            return -res;
        }
      s++;
    }

  return 0;
}
iexport(compare_string);


/* The destination and source should not overlap.  */

void
concat_string (gfc_charlen_type destlen, CHARTYPE * dest,
	       gfc_charlen_type len1, const CHARTYPE * s1,
	       gfc_charlen_type len2, const CHARTYPE * s2)
{
  if (len1 >= destlen)
    {
      memcpy (dest, s1, destlen * sizeof (CHARTYPE));
      return;
    }
  memcpy (dest, s1, len1 * sizeof (CHARTYPE));
  dest += len1;
  destlen -= len1;

  if (len2 >= destlen)
    {
      memcpy (dest, s2, destlen * sizeof (CHARTYPE));
      return;
    }

  memcpy (dest, s2, len2 * sizeof (CHARTYPE));
  MEMSET (&dest[len2], ' ', destlen - len2);
}


/* Return string with all trailing blanks removed.  */

void
string_trim (gfc_charlen_type *len, CHARTYPE **dest, gfc_charlen_type slen,
	     const CHARTYPE *src)
{
  *len = string_len_trim (slen, src);

  if (*len == 0)
    *dest = &zero_length_string;
  else
    {
      /* Allocate space for result string.  */
      *dest = xmallocarray (*len, sizeof (CHARTYPE));

      /* Copy string if necessary.  */
      memcpy (*dest, src, *len * sizeof (CHARTYPE));
    }
}


/* The length of a string not including trailing blanks.  */

gfc_charlen_type
string_len_trim (gfc_charlen_type len, const CHARTYPE *s)
{
  if (len <= 0)
    return 0;

  const size_t long_len = sizeof (unsigned long);

  size_t i = len - 1;

  /* If we've got the standard (KIND=1) character type, we scan the string in
     long word chunks to speed it up (until a long word is hit that does not
     consist of ' 's).  */
  if (sizeof (CHARTYPE) == 1 && i >= long_len)
    {
      size_t starting;
      unsigned long blank_longword;

      /* Handle the first characters until we're aligned on a long word
	 boundary.  Actually, s + i + 1 must be properly aligned, because
	 s + i will be the last byte of a long word read.  */
      starting = (
#ifdef __INTPTR_TYPE__
		  (__INTPTR_TYPE__)
#endif
		  (s + i + 1)) % long_len;
      i -= starting;
      for (; starting > 0; --starting)
	if (s[i + starting] != ' ')
	  return i + starting + 1;

      /* Handle the others in a batch until first non-blank long word is
	 found.  Here again, s + i is the last byte of the current chunk,
	 to it starts at s + i - sizeof (long) + 1.  */

#if __SIZEOF_LONG__ == 4
      blank_longword = 0x20202020L;
#elif __SIZEOF_LONG__ == 8
      blank_longword = 0x2020202020202020L;
#else
      #error Invalid size of long!
#endif

      while (i >= long_len)
	{
	  i -= long_len;
	  if (*((unsigned long*) (s + i + 1)) != blank_longword)
	    {
	      i += long_len;
	      break;
	    }
	}
    }

  /* Simply look for the first non-blank character.  */
  while (s[i] == ' ')
    {
      if (i == 0)
	return 0;
      --i;
    }
  return i + 1;
}


/* Find a substring within a string.  */

gfc_charlen_type
string_index (gfc_charlen_type slen, const CHARTYPE *str,
	      gfc_charlen_type sslen, const CHARTYPE *sstr,
	      GFC_LOGICAL_4 back)
{
  gfc_charlen_type start, last, delta, i;

  if (sslen == 0)
    return back ? (slen + 1) : 1;

  if (sslen > slen)
    return 0;

  if (!back)
    {
      last = slen + 1 - sslen;
      start = 0;
      delta = 1;
    }
  else
    {
      last = -1;
      start = slen - sslen;
      delta = -1;
    }

  for (; start != last; start+= delta)
    {
      for (i = 0; i < sslen; i++)
        {
          if (str[start + i] != sstr[i])
            break;
        }
      if (i == sslen)
        return (start + 1);
    }
  return 0;
}


/* Remove leading blanks from a string, padding at end.  The src and dest
   should not overlap.  */

void
adjustl (CHARTYPE *dest, gfc_charlen_type len, const CHARTYPE *src)
{
  gfc_charlen_type i;

  i = 0;
  while (i < len && src[i] == ' ')
    i++;

  if (i < len)
    memcpy (dest, &src[i], (len - i) * sizeof (CHARTYPE));
  if (i > 0)
    MEMSET (&dest[len - i], ' ', i);
}


/* Remove trailing blanks from a string.  */

void
adjustr (CHARTYPE *dest, gfc_charlen_type len, const CHARTYPE *src)
{
  gfc_charlen_type i;

  i = len;
  while (i > 0 && src[i - 1] == ' ')
    i--;

  if (i < len)
    MEMSET (dest, ' ', len - i);
  memcpy (&dest[len - i], src, i * sizeof (CHARTYPE));
}


/* Scan a string for any one of the characters in a set of characters.  */

gfc_charlen_type
string_scan (gfc_charlen_type slen, const CHARTYPE *str,
	     gfc_charlen_type setlen, const CHARTYPE *set, GFC_LOGICAL_4 back)
{
  gfc_charlen_type i, j;

  if (slen == 0 || setlen == 0)
    return 0;

  if (back)
    {
      for (i = slen; i != 0; i--)
	{
	  for (j = 0; j < setlen; j++)
	    {
	      if (str[i - 1] == set[j])
		return i;
	    }
	}
    }
  else
    {
      for (i = 0; i < slen; i++)
	{
	  for (j = 0; j < setlen; j++)
	    {
	      if (str[i] == set[j])
		return (i + 1);
	    }
	}
    }

  return 0;
}


/* Verify that a set of characters contains all the characters in a
   string by identifying the position of the first character in a
   characters that does not appear in a given set of characters.  */

gfc_charlen_type
string_verify (gfc_charlen_type slen, const CHARTYPE *str,
	       gfc_charlen_type setlen, const CHARTYPE *set,
	       GFC_LOGICAL_4 back)
{
  gfc_charlen_type start, last, delta, i;

  if (slen == 0)
    return 0;

  if (back)
    {
      last = -1;
      start = slen - 1;
      delta = -1;
    }
  else
    {
      last = slen;
      start = 0;
      delta = 1;
    }
  for (; start != last; start += delta)
    {
      for (i = 0; i < setlen; i++)
        {
          if (str[start] == set[i])
            break;
        }
      if (i == setlen)
        return (start + 1);
    }

  return 0;
}


/* MIN and MAX intrinsics for strings.  The front-end makes sure that
   nargs is at least 2.  */

void
string_minmax (gfc_charlen_type *rlen, CHARTYPE **dest, int op, int nargs, ...)
{
  va_list ap;
  int i;
  CHARTYPE *next, *res;
  gfc_charlen_type nextlen, reslen;

  va_start (ap, nargs);
  reslen = va_arg (ap, gfc_charlen_type);
  res = va_arg (ap, CHARTYPE *);
  *rlen = reslen;

  if (res == NULL)
    runtime_error ("First argument of '%s' intrinsic should be present",
		   op > 0 ? "MAX" : "MIN");

  for (i = 1; i < nargs; i++)
    {
      nextlen = va_arg (ap, gfc_charlen_type);
      next = va_arg (ap, CHARTYPE *);

      if (next == NULL)
	{
	  if (i == 1)
	    runtime_error ("Second argument of '%s' intrinsic should be "
			   "present", op > 0 ? "MAX" : "MIN");
	  else
	    continue;
	}

      if (nextlen > *rlen)
	*rlen = nextlen;

      if (op * compare_string (reslen, res, nextlen, next) < 0)
	{
	  reslen = nextlen;
	  res = next;
	}
    }
  va_end (ap);

  if (*rlen == 0)
    *dest = &zero_length_string;
  else
    {
      CHARTYPE *tmp = xmallocarray (*rlen, sizeof (CHARTYPE));
      memcpy (tmp, res, reslen * sizeof (CHARTYPE));
      MEMSET (&tmp[reslen], ' ', *rlen - reslen);
      *dest = tmp;
    }
}

gfc_charlen_type
string_split (gfc_charlen_type stringlen, const CHARTYPE *string,
	      gfc_charlen_type setlen, const CHARTYPE *set,
	      gfc_charlen_type pos, GFC_LOGICAL_4 back)
{
  gfc_charlen_type i, j;

  if (!back)
    {
      if (pos > stringlen)
	runtime_error ("If BACK is present with the value false, the value of "
		       "POS shall be in the range [0, LEN (STRING)], "
		       "where POS = %ld and LEN (STRING) = %ld",
		       pos, stringlen);

      for (i = pos + 1; i <= stringlen; i++)
	{
	  for (j = 0; j < setlen; j++)
	    {
	      if (string[i - 1] == set[j])
		return i;
	    }
	}

      return stringlen + 1;
    }
  else
    {
      if (pos < 1 || pos > (stringlen + 1))
	runtime_error ("If BACK is present with the value true, the value of "
		       "POS shall be in the range [1, LEN (STRING) + 1], "
		       "where POS = %ld and LEN (STRING) = %ld",
		       pos, stringlen);

      for (i = pos - 1; i != 0; i--)
	{
	  for (j = 0; j < setlen; j++)
	    {
	      if (string[i - 1] == set[j])
		return i;
	    }
	}

      return 0;
    }
}
