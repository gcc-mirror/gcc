/* Demangler test program,
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
   Written by Zack Weinberg <zack@codesourcery.com

   This file is part of GNU libiberty.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. 
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "ansidecl.h"
#include <stdio.h>
#include "libiberty.h"
#include "demangle.h"

struct line
{
  size_t alloced;
  char *data;
};

static unsigned int lineno;

/* Safely read a single line of arbitrary length from standard input.  */

#define LINELEN 80

static void
getline(buf)
     struct line *buf;
{
  char *data = buf->data;
  size_t alloc = buf->alloced;
  size_t count = 0;
  int c;

  if (data == 0)
    {
      data = xmalloc (LINELEN);
      alloc = LINELEN;
    }

  /* Skip comment lines.  */
  while ((c = getchar()) == '#')
    {
      while ((c = getchar()) != EOF && c != '\n');
      lineno++;
    }

  /* c is the first character on the line, and it's not a comment
     line: copy this line into the buffer and return.  */
  while (c != EOF && c != '\n')
    {
      if (count + 1 >= alloc)
	{
	  alloc *= 2;
	  data = xrealloc (data, alloc);
	}
      data[count++] = c;
      c = getchar();
    }
  lineno++;
  data[count] = '\0';

  buf->data = data;
  buf->alloced = alloc;
}

static void
fail (lineno, opts, in, out, exp)
     int lineno;
     const char *opts;
     const char *in;
     const char *out;
     const char *exp;
{
  printf ("\
FAIL at line %d, options %s:\n\
in:  %s\n\
out: %s\n\
exp: %s\n",
	  lineno, opts, in, out != NULL ? out : "(null)", exp);
}

/* The tester operates on a data file consisting of groups of lines:
   options
   input to be demangled
   expected output

   Supported options:
     --format=<name>     Sets the demangling style.
     --no-params         There are two lines of expected output; the first
                         is with DMGL_PARAMS, the second is without it.
     --is-v3-ctor        Calls is_gnu_v3_mangled_ctor on input; expected
                         output is an integer representing ctor_kind.
     --is-v3-dtor        Likewise, but for dtors.

   For compatibility, just in case it matters, the options line may be
   empty, to mean --format=auto.  If it doesn't start with --, then it
   may contain only a format name.
*/

int
main(argc, argv)
     int argc;
     char **argv;
{
  enum demangling_styles style;
  int no_params;
  int is_v3_ctor;
  int is_v3_dtor;
  struct line format;
  struct line input;
  struct line expect;
  char *result;
  int failures = 0;
  int tests = 0;

  if (argc > 1)
    {
      fprintf (stderr, "usage: %s < test-set\n", argv[0]);
      return 2;
    }

  format.data = 0;
  input.data = 0;
  expect.data = 0;

  for (;;)
    {
      getline (&format);
      if (feof (stdin))
	break;

      getline (&input);
      getline (&expect);

      tests++;

      no_params = 0;
      is_v3_ctor = 0;
      is_v3_dtor = 0;
      if (format.data[0] == '\0')
	style = auto_demangling;
      else if (format.data[0] != '-')
	{
	  style = cplus_demangle_name_to_style (format.data);
	  if (style == unknown_demangling)
	    {
	      printf ("FAIL at line %d: unknown demangling style %s\n",
		      lineno, format.data);
	      failures++;
	      continue;
	    }
	}
      else
	{
	  char *p;
	  char *opt;

	  p = format.data;
	  while (*p != '\0')
	    {
	      char c;

	      opt = p;
	      p += strcspn (p, " \t=");
	      c = *p;
	      *p = '\0';
	      if (strcmp (opt, "--format") == 0 && c == '=')
		{
		  char *fstyle;

		  *p = c;
		  ++p;
		  fstyle = p;
		  p += strcspn (p, " \t");
		  c = *p;
		  *p = '\0';
		  style = cplus_demangle_name_to_style (fstyle);
		  if (style == unknown_demangling)
		    {
		      printf ("FAIL at line %d: unknown demangling style %s\n",
			      lineno, fstyle);
		      failures++;
		      continue;
		    }
		}
	      else if (strcmp (opt, "--no-params") == 0)
		no_params = 1;
	      else if (strcmp (opt, "--is-v3-ctor") == 0)
		is_v3_ctor = 1;
	      else if (strcmp (opt, "--is-v3-dtor") == 0)
		is_v3_dtor = 1;
	      else
		{
		  printf ("FAIL at line %d: unrecognized option %s\n",
			  lineno, opt);
		  failures++;
		  continue;
		}
	      *p = c;
	      p += strspn (p, " \t");
	    }
	}

      if (is_v3_ctor || is_v3_dtor)
	{
	  char buf[20];

	  if (is_v3_ctor)
	    {
	      enum gnu_v3_ctor_kinds kc;

	      kc = is_gnu_v3_mangled_ctor (input.data);
	      sprintf (buf, "%d", (int) kc);
	    }
	  else
	    {
	      enum gnu_v3_dtor_kinds kd;

	      kd = is_gnu_v3_mangled_dtor (input.data);
	      sprintf (buf, "%d", (int) kd);
	    }

	  if (strcmp (buf, expect.data) != 0)
	    {
	      fail (lineno, format.data, input.data, buf, expect.data);
	      failures++;
	    }

	  continue;
	}

      cplus_demangle_set_style (style);

      result = cplus_demangle (input.data,
			       DMGL_PARAMS|DMGL_ANSI|DMGL_TYPES);

      if (result
	  ? strcmp (result, expect.data)
	  : strcmp (input.data, expect.data))
	{
	  fail (lineno, format.data, input.data, result, expect.data);
	  failures++;
	}
      free (result);

      if (no_params)
	{
	  getline (&expect);
	  result = cplus_demangle (input.data, DMGL_ANSI|DMGL_TYPES);

	  if (result
	      ? strcmp (result, expect.data)
	      : strcmp (input.data, expect.data))
	    {
	      fail (lineno, format.data, input.data, result, expect.data);
	      failures++;
	    }
	  free (result);
	}
    }

  free (format.data);
  free (input.data);
  free (expect.data);

  printf ("%s: %d tests, %d failures\n", argv[0], tests, failures);
  return failures ? 1 : 0;
}
