/* Copyright (C) 2018-2021 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "libgomp.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>  /* For PRIx64.  */
#endif
#ifdef HAVE_UNAME
#include <sys/utsname.h>
#endif

bool
gomp_print_string (const char *str, size_t len)
{
  return fwrite (str, 1, len, stderr) != len;
}

void
gomp_set_affinity_format (const char *format, size_t len)
{
  if (len < gomp_affinity_format_len)
    memcpy (gomp_affinity_format_var, format, len);
  else
    {
      char *p;
      if (gomp_affinity_format_len)
	p = gomp_realloc (gomp_affinity_format_var, len + 1);
      else
	p = gomp_malloc (len + 1);
      memcpy (p, format, len);
      gomp_affinity_format_var = p;
      gomp_affinity_format_len = len + 1;
    }
  gomp_affinity_format_var[len] = '\0';
}

void
omp_set_affinity_format (const char *format)
{
  gomp_set_affinity_format (format, strlen (format));
}

size_t
omp_get_affinity_format (char *buffer, size_t size)
{
  size_t len = strlen (gomp_affinity_format_var);
  if (size)
    {
      if (len < size)
	memcpy (buffer, gomp_affinity_format_var, len + 1);
      else
	{
	  memcpy (buffer, gomp_affinity_format_var, size - 1);
	  buffer[size - 1] = '\0';
	}
    }
  return len;
}

void
gomp_display_string (char *buffer, size_t size, size_t *ret,
		     const char *str, size_t len)
{
  size_t r = *ret;
  if (size && r < size)
    {
      size_t l = len;
      if (size - r < len)
	l = size - r;
      memcpy (buffer + r, str, l);
    }
  *ret += len;
  if (__builtin_expect (r > *ret, 0))
    gomp_fatal ("overflow in omp_capture_affinity");
}

static void
gomp_display_repeat (char *buffer, size_t size, size_t *ret,
		     char c, size_t len)
{
  size_t r = *ret;
  if (size && r < size)
    {
      size_t l = len;
      if (size - r < len)
	l = size - r;
      memset (buffer + r, c, l);
    }
  *ret += len;
  if (__builtin_expect (r > *ret, 0))
    gomp_fatal ("overflow in omp_capture_affinity");
}

static void
gomp_display_num (char *buffer, size_t size, size_t *ret,
		  bool zero, bool right, size_t sz, char *buf)
{
  size_t l = strlen (buf);
  if (sz == (size_t) -1 || l >= sz)
    {
      gomp_display_string (buffer, size, ret, buf, l);
      return;
    }
  if (zero)
    {
      if (buf[0] == '-')
	gomp_display_string (buffer, size, ret, buf, 1);
      else if (buf[0] == '0' && buf[1] == 'x')
	gomp_display_string (buffer, size, ret, buf, 2);
      gomp_display_repeat (buffer, size, ret, '0', sz - l);
      if (buf[0] == '-')
	gomp_display_string (buffer, size, ret, buf + 1, l - 1);
      else if (buf[0] == '0' && buf[1] == 'x')
	gomp_display_string (buffer, size, ret, buf + 2, l - 2);
      else
	gomp_display_string (buffer, size, ret, buf, l);
    }
  else if (right)
    {
      gomp_display_repeat (buffer, size, ret, ' ', sz - l);
      gomp_display_string (buffer, size, ret, buf, l);
    }
  else
    {
      gomp_display_string (buffer, size, ret, buf, l);
      gomp_display_repeat (buffer, size, ret, ' ', sz - l);
    }
}

static void
gomp_display_int (char *buffer, size_t size, size_t *ret,
		  bool zero, bool right, size_t sz, int num)
{
  char buf[3 * sizeof (int) + 2];
  sprintf (buf, "%d", num);
  gomp_display_num (buffer, size, ret, zero, right, sz, buf);
}

static void
gomp_display_string_len (char *buffer, size_t size, size_t *ret,
			 bool right, size_t sz, char *str, size_t len)
{
  if (sz == (size_t) -1 || len >= sz)
    {
      gomp_display_string (buffer, size, ret, str, len);
      return;
    }

  if (right)
    {
      gomp_display_repeat (buffer, size, ret, ' ', sz - len);
      gomp_display_string (buffer, size, ret, str, len);
    }
  else
    {
      gomp_display_string (buffer, size, ret, str, len);
      gomp_display_repeat (buffer, size, ret, ' ', sz - len);
    }
}

static void
gomp_display_hostname (char *buffer, size_t size, size_t *ret,
		       bool right, size_t sz)
{
#ifdef HAVE_GETHOSTNAME
  {
    char buf[256];
    char *b = buf;
    size_t len = 256;
    do
      {
	b[len - 1] = '\0';
	if (gethostname (b, len - 1) == 0)
	  {
	    size_t l = strlen (b);
	    if (l < len - 1)
	      {
		gomp_display_string_len (buffer, size, ret,
					 right, sz, b, l);
		if (b != buf)
		  free (b);
		return;
	      }
	  }
	if (len == 1048576)
	  break;
	len = len * 2;
	if (len == 512)
	  b = gomp_malloc (len);
	else
	  b = gomp_realloc (b, len);
      }
    while (1);
    if (b != buf)
      free (b);
  }
#endif
#ifdef HAVE_UNAME
  {
    struct utsname buf;
    if (uname (&buf) == 0)
      {
	gomp_display_string_len (buffer, size, ret, right, sz,
				 buf.nodename, strlen (buf.nodename));
	return;
      }
  }
#endif
  gomp_display_string_len (buffer, size, ret, right, sz, "node", 4);
}

struct affinity_types_struct {
  char long_str[18];
  char long_len;
  char short_c; };

static struct affinity_types_struct affinity_types[] =
{
#define AFFINITY_TYPE(l, s) \
  { #l, sizeof (#l) - 1, s }
  AFFINITY_TYPE (team_num, 't'),
  AFFINITY_TYPE (num_teams, 'T'),
  AFFINITY_TYPE (nesting_level, 'L'),
  AFFINITY_TYPE (thread_num, 'n'),
  AFFINITY_TYPE (num_threads, 'N'),
  AFFINITY_TYPE (ancestor_tnum, 'a'),
  AFFINITY_TYPE (host, 'H'),
  AFFINITY_TYPE (process_id, 'P'),
  AFFINITY_TYPE (native_thread_id, 'i'),
  AFFINITY_TYPE (thread_affinity, 'A')
#undef AFFINITY_TYPE
};

size_t
gomp_display_affinity (char *buffer, size_t size,
		       const char *format, gomp_thread_handle handle,
		       struct gomp_team_state *ts, unsigned int place)
{
  size_t ret = 0;
  do
    {
      const char *p = strchr (format, '%');
      bool zero = false;
      bool right = false;
      size_t sz = -1;
      char c;
      int val;
      if (p == NULL)
	p = strchr (format, '\0');
      if (p != format)
	gomp_display_string (buffer, size, &ret,
			     format, p - format);
      if (*p == '\0')
	break;
      p++;
      if (*p == '%')
	{
	  gomp_display_string (buffer, size, &ret, "%", 1);
	  format = p + 1;
	  continue;
	}
      if (*p == '0')
	{
	  zero = true;
	  p++;
	  if (*p != '.')
	    gomp_fatal ("leading zero not followed by dot in affinity format");
	}
      if (*p == '.')
	{
	  right = true;
	  p++;
	}
      if (*p >= '1' && *p <= '9')
	{
	  char *end;
	  sz = strtoul (p, &end, 10);
	  p = end;
	}
      else if (zero || right)
	gomp_fatal ("leading zero or right justification in affinity format "
		    "requires size");
      c = *p;
      if (c == '{')
	{
	  int i;
	  for (i = 0;
	       i < sizeof (affinity_types) / sizeof (affinity_types[0]); ++i)
	    if (strncmp (p + 1, affinity_types[i].long_str,
			 affinity_types[i].long_len) == 0
		&& p[affinity_types[i].long_len + 1] == '}')
	      {
		c = affinity_types[i].short_c;
		p += affinity_types[i].long_len + 1;
		break;
	      }
	  if (c == '{')
	    {
	      char *q = strchr (p + 1, '}');
	      if (q)
		gomp_fatal ("unsupported long type name '%.*s' in affinity "
			    "format", (int) (q - (p + 1)), p + 1);
	      else
		gomp_fatal ("unterminated long type name '%s' in affinity "
			    "format", p + 1);
	    }
	}
      switch (c)
	{
	case 't':
	  val = omp_get_team_num ();
	  goto do_int;
	case 'T':
	  val = omp_get_num_teams ();
	  goto do_int;
	case 'L':
	  val = ts->level;
	  goto do_int;
	case 'n':
	  val = ts->team_id;
	  goto do_int;
	case 'N':
	  val = ts->team ? ts->team->nthreads : 1;
	  goto do_int;
	case 'a':
	  val = ts->team ? ts->team->prev_ts.team_id : -1;
	  goto do_int;
	case 'H':
	  gomp_display_hostname (buffer, size, &ret, right, sz);
	  break;
	case 'P':
#ifdef HAVE_GETPID
	  val = getpid ();
#else
	  val = 0;
#endif
	  goto do_int;
	case 'i':
#if defined(LIBGOMP_USE_PTHREADS) && defined(__GNUC__)
	  {
	    char buf[3 * (sizeof (handle) + sizeof (uintptr_t) + sizeof (int))
		     + 4];
	    /* This macro returns expr unmodified for integral or pointer
	       types and 0 for anything else (e.g. aggregates).  */
#define gomp_nonaggregate(expr) \
  __builtin_choose_expr (__builtin_classify_type (expr) == 1		    \
			 || __builtin_classify_type (expr) == 5, expr, 0)
	    /* This macro returns expr unmodified for integral types,
	       (uintptr_t) (expr) for pointer types and 0 for anything else
	       (e.g. aggregates).  */
#define gomp_integral(expr) \
  __builtin_choose_expr (__builtin_classify_type (expr) == 5,		    \
			 (uintptr_t) gomp_nonaggregate (expr),		    \
			 gomp_nonaggregate (expr))

	    if (sizeof (gomp_integral (handle)) == sizeof (unsigned long))
	      sprintf (buf, "0x%lx", (unsigned long) gomp_integral (handle));
#if defined (HAVE_INTTYPES_H) && defined (PRIx64)
	    else if (sizeof (gomp_integral (handle)) == sizeof (uint64_t))
	      sprintf (buf, "0x%" PRIx64, (uint64_t) gomp_integral (handle));
#else
	    else if (sizeof (gomp_integral (handle))
		     == sizeof (unsigned long long))
	      sprintf (buf, "0x%llx",
		       (unsigned long long) gomp_integral (handle));
#endif
	    else
	      sprintf (buf, "0x%x", (unsigned int) gomp_integral (handle));
	    gomp_display_num (buffer, size, &ret, zero, right, sz, buf);
	    break;
	  }
#else
	  val = 0;
	  goto do_int;
#endif
	case 'A':
	  if (sz == (size_t) -1)
	    gomp_display_affinity_place (buffer, size, &ret,
					 place - 1);
	  else if (right)
	    {
	      size_t len = 0;
	      gomp_display_affinity_place (NULL, 0, &len, place - 1);
	      if (len < sz)
		gomp_display_repeat (buffer, size, &ret, ' ', sz - len);
	      gomp_display_affinity_place (buffer, size, &ret, place - 1);
	    }
	  else
	    {
	      size_t start = ret;
	      gomp_display_affinity_place (buffer, size, &ret, place - 1);
	      if (ret - start < sz)
		gomp_display_repeat (buffer, size, &ret, ' ', sz - (ret - start));
	    }
	  break;
	do_int:
	  gomp_display_int (buffer, size, &ret, zero, right, sz, val);
	  break;
	default:
	  gomp_fatal ("unsupported type %c in affinity format", c);
	}
      format = p + 1;
    }
  while (1);
  return ret;
}

size_t
omp_capture_affinity (char *buffer, size_t size, const char *format)
{
  struct gomp_thread *thr = gomp_thread ();
  size_t ret
    = gomp_display_affinity (buffer, size,
			     format && *format
			     ? format : gomp_affinity_format_var,
			     gomp_thread_self (), &thr->ts, thr->place);
  if (size)
    {
      if (ret >= size)
	buffer[size - 1] = '\0';
      else
	buffer[ret] = '\0';
    }
  return ret;
}
ialias (omp_capture_affinity)

void
omp_display_affinity (const char *format)
{
  char buf[512];
  char *b;
  size_t ret = ialias_call (omp_capture_affinity) (buf, sizeof buf, format);
  if (ret < sizeof buf)
    {
      buf[ret] = '\n';
      gomp_print_string (buf, ret + 1);
      return;
    }
  b = gomp_malloc (ret + 1);
  ialias_call (omp_capture_affinity) (b, ret + 1, format);
  b[ret] = '\n';
  gomp_print_string (b, ret + 1);
  free (b);
}

void
gomp_display_affinity_thread (gomp_thread_handle handle,
			      struct gomp_team_state *ts, unsigned int place)
{
  char buf[512];
  char *b;
  size_t ret = gomp_display_affinity (buf, sizeof buf, gomp_affinity_format_var,
				      handle, ts, place);
  if (ret < sizeof buf)
    {
      buf[ret] = '\n';
      gomp_print_string (buf, ret + 1);
      return;
    }
  b = gomp_malloc (ret + 1);
  gomp_display_affinity (b, ret + 1, gomp_affinity_format_var,
  			 handle, ts, place);
  b[ret] = '\n';
  gomp_print_string (b, ret + 1);
  free (b);
}
