/* Copyright (C) 2002-2017 Free Software Foundation, Inc.
   Contributed by Andy Vaught

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

#include "libgfortran.h"

#include <string.h>
#include <strings.h>
#include <ctype.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


/* Implementation of secure_getenv() for targets where it is not
   provided. */

#ifdef FALLBACK_SECURE_GETENV

#if SUPPORTS_WEAKREF && defined(HAVE___SECURE_GETENV)
static char* weak_secure_getenv (const char*)
  __attribute__((__weakref__("__secure_getenv")));
#endif

char *
secure_getenv (const char *name)
{
#if SUPPORTS_WEAKREF && defined(HAVE___SECURE_GETENV)
  if (weak_secure_getenv)
    return weak_secure_getenv (name);
#endif

  if ((getuid () == geteuid ()) && (getgid () == getegid ()))
    return getenv (name);
  else
    return NULL;
}
#endif



/* Examine the environment for controlling aspects of the program's
   execution.  Our philosophy here that the environment should not prevent
   the program from running, so any invalid value will be ignored.  */


options_t options;

typedef struct variable
{
  const char *name;
  int default_value;
  int *var;           
  void (*init) (struct variable *);
}
variable;

static void init_unformatted (variable *);


/* Initialize an integer environment variable.  */

static void
init_integer (variable * v)
{
  char *p, *q;

  p = getenv (v->name);
  if (p == NULL)
    return;

  for (q = p; *q; q++)
    if (!isdigit (*q) && (p != q || *q != '-'))
      return;

  *v->var = atoi (p);
}


/* Initialize an integer environment variable which has to be positive.  */

static void
init_unsigned_integer (variable * v)
{
  char *p, *q;

  p = getenv (v->name);
  if (p == NULL)
    return;

  for (q = p; *q; q++)
    if (!isdigit (*q))
      return;

  *v->var = atoi (p);
}


/* Initialize a boolean environment variable. We only look at the first
   letter of the value. */

static void
init_boolean (variable * v)
{
  char *p;

  p = getenv (v->name);
  if (p == NULL)
    return;

  if (*p == '1' || *p == 'Y' || *p == 'y')
    *v->var = 1;
  else if (*p == '0' || *p == 'N' || *p == 'n')
    *v->var = 0;
}


/* Initialize a list output separator.  It may contain any number of spaces
   and at most one comma.  */

static void
init_sep (variable * v)
{
  int seen_comma;
  char *p;

  p = getenv (v->name);
  if (p == NULL)
    goto set_default;

  options.separator = p;
  options.separator_len = strlen (p);

  /* Make sure the separator is valid */

  if (options.separator_len == 0)
    goto set_default;
  seen_comma = 0;

  while (*p)
    {
      if (*p == ',')
	{
	  if (seen_comma)
	    goto set_default;
	  seen_comma = 1;
	  p++;
	  continue;
	}

      if (*p++ != ' ')
	goto set_default;
    }

  return;

set_default:
  options.separator = " ";
  options.separator_len = 1;
}


static variable variable_table[] = {

  /* Unit number that will be preconnected to standard input */
  { "GFORTRAN_STDIN_UNIT", GFC_STDIN_UNIT_NUMBER, &options.stdin_unit,
    init_integer },

  /* Unit number that will be preconnected to standard output */
  { "GFORTRAN_STDOUT_UNIT", GFC_STDOUT_UNIT_NUMBER, &options.stdout_unit,
    init_integer },

  /* Unit number that will be preconnected to standard error */
  { "GFORTRAN_STDERR_UNIT", GFC_STDERR_UNIT_NUMBER, &options.stderr_unit,
    init_integer },

  /* If TRUE, all output will be unbuffered */
  { "GFORTRAN_UNBUFFERED_ALL", 0, &options.all_unbuffered, init_boolean },

  /* If TRUE, output to preconnected units will be unbuffered */
  { "GFORTRAN_UNBUFFERED_PRECONNECTED", 0, &options.unbuffered_preconnected,
    init_boolean },

  /* Whether to print filename and line number on runtime error */
  { "GFORTRAN_SHOW_LOCUS", 1, &options.locus, init_boolean },

  /* Print optional plus signs in numbers where permitted */
  { "GFORTRAN_OPTIONAL_PLUS", 0, &options.optional_plus, init_boolean },

  /* Default maximum record length for sequential files */
  { "GFORTRAN_DEFAULT_RECL", DEFAULT_RECL, &options.default_recl,
    init_unsigned_integer },

  /* Separator to use when writing list output */
  { "GFORTRAN_LIST_SEPARATOR", 0, NULL, init_sep },

  /* Set the default data conversion for unformatted I/O */
  { "GFORTRAN_CONVERT_UNIT", 0, 0, init_unformatted },

  /* Print out a backtrace if possible on runtime error */
  { "GFORTRAN_ERROR_BACKTRACE", -1, &options.backtrace, init_boolean },

  { NULL, 0, NULL, NULL }
};


/* Initialize most runtime variables from
 * environment variables. */

void
init_variables (void)
{
  variable *v;

  for (v = variable_table; v->name; v++)
    {
      if (v->var)
	*v->var = v->default_value;
      v->init (v);
    }
}


/* This is the handling of the GFORTRAN_CONVERT_UNITS environment variable.
   It is called from environ.c to parse this variable, and from
   open.c to determine if the user specified a default for an
   unformatted file.
   The syntax of the environment variable is, in bison grammar:

   GFORTRAN_CONVERT_UNITS: mode | mode ';' exception ;
   mode: 'native' | 'swap' | 'big_endian' | 'little_endian' ;
   exception: mode ':' unit_list | unit_list ;
   unit_list: unit_spec | unit_list unit_spec ;
   unit_spec: INTEGER | INTEGER '-' INTEGER ;
*/

/* Defines for the tokens.  Other valid tokens are ',', ':', '-'.  */


#define NATIVE   257
#define SWAP     258
#define BIG      259
#define LITTLE   260
/* Some space for additional tokens later.  */
#define INTEGER  273
#define END      (-1)
#define ILLEGAL  (-2)

typedef struct
{
  int unit;
  unit_convert conv;
} exception_t;


static char *p;            /* Main character pointer for parsing.  */
static char *lastpos;      /* Auxiliary pointer, for backing up.  */
static int unit_num;       /* The last unit number read.  */
static int unit_count;     /* The number of units found. */
static int do_count;       /* Parsing is done twice - first to count the number
			      of units, then to fill in the table.  This
			      variable controls what to do.  */
static exception_t *elist; /* The list of exceptions to the default. This is
			      sorted according to unit number.  */
static int n_elist;        /* Number of exceptions to the default.  */

static unit_convert endian; /* Current endianness.  */

static unit_convert def; /* Default as specified (if any).  */

/* Search for a unit number, using a binary search.  The
   first argument is the unit number to search for.  The second argument
   is a pointer to an index.
   If the unit number is found, the function returns 1, and the index
   is that of the element.
   If the unit number is not found, the function returns 0, and the
   index is the one where the element would be inserted.  */

static int
search_unit (int unit, int *ip)
{
  int low, high, mid;

  if (n_elist == 0)
    {
      *ip = 0;
      return 0;
    }

  low = 0;
  high = n_elist - 1;

  do 
    {
      mid = (low + high) / 2;
      if (unit == elist[mid].unit)
	{
	  *ip = mid;
	  return 1;
	}
      else if (unit > elist[mid].unit)
	low = mid + 1;
      else
	high = mid - 1;
    } while (low <= high);

  if (unit > elist[mid].unit)
    *ip = mid + 1;
  else
    *ip = mid;

  return 0;
}

/* This matches a keyword.  If it is found, return the token supplied,
   otherwise return ILLEGAL.  */

static int
match_word (const char *word, int tok)
{
  int res;

  if (strncasecmp (p, word, strlen (word)) == 0)
    {
      p += strlen (word);
      res = tok;
    }
  else
    res = ILLEGAL;
  return res;
}

/* Match an integer and store its value in unit_num.  This only works
   if p actually points to the start of an integer.  The caller has
   to ensure this.  */

static int
match_integer (void)
{
  unit_num = 0;
  while (isdigit (*p))
    unit_num = unit_num * 10 + (*p++ - '0');
  return INTEGER;
}

/* This reads the next token from the GFORTRAN_CONVERT_UNITS variable.
   Returned values are the different tokens.  */

static int
next_token (void)
{
  int result;

  lastpos = p;
  switch (*p)
    {
    case '\0':
      result = END;
      break;
      
    case ':':
    case ',': 
    case '-':
    case ';':
      result = *p;
      p++;
      break;

    case 'b':
    case 'B':
      result = match_word ("big_endian", BIG);
      break;

    case 'l':
    case 'L':
      result = match_word ("little_endian", LITTLE);
      break;

    case 'n':
    case 'N':
      result = match_word ("native", NATIVE);
      break;

    case 's':
    case 'S':
      result = match_word ("swap", SWAP);
      break;

    case '1': case '2': case '3': case '4': case '5':
    case '6': case '7': case '8': case '9':
      result = match_integer ();
      break;

    default:
      result = ILLEGAL;
      break;
    }
  return result;
}

/* Back up the last token by setting back the character pointer.  */

static void
push_token (void)
{
  p = lastpos;
}

/* This is called when a unit is identified.  If do_count is nonzero,
   increment the number of units by one.  If do_count is zero,
   put the unit into the table.  */

static void
mark_single (int unit)
{
  int i,j;

  if (do_count)
    {
      unit_count++;
      return;
    }
  if (search_unit (unit, &i))
    {
      elist[i].conv = endian;
    }
  else
    {
      for (j=n_elist-1; j>=i; j--)
	elist[j+1] = elist[j];

      n_elist += 1;
      elist[i].unit = unit;
      elist[i].conv = endian;
    }
}

/* This is called when a unit range is identified.  If do_count is
   nonzero, increase the number of units.  If do_count is zero,
   put the unit into the table.  */

static void
mark_range (int unit1, int unit2)
{
  int i;
  if (do_count)
    unit_count += abs (unit2 - unit1) + 1;
  else
    {
      if (unit2 < unit1)
	for (i=unit2; i<=unit1; i++)
	  mark_single (i);
      else
	for (i=unit1; i<=unit2; i++)
	  mark_single (i);
    }
}

/* Parse the GFORTRAN_CONVERT_UNITS variable.  This is called
   twice, once to count the units and once to actually mark them in
   the table.  When counting, we don't check for double occurrences
   of units.  */

static int
do_parse (void)
{
  int tok;
  int unit1;
  int continue_ulist;
  char *start;

  unit_count = 0;

  start = p;

  /* Parse the string.  First, let's look for a default.  */
  tok = next_token ();
  switch (tok)
    {
    case NATIVE:
      endian = GFC_CONVERT_NATIVE;
      break;

    case SWAP:
      endian = GFC_CONVERT_SWAP;
      break;

    case BIG:
      endian = GFC_CONVERT_BIG;
      break;

    case LITTLE:
      endian = GFC_CONVERT_LITTLE;
      break;

    case INTEGER:
      /* A leading digit means that we are looking at an exception.
	 Reset the position to the beginning, and continue processing
	 at the exception list.  */
      p = start;
      goto exceptions;
      break;

    case END:
      goto end;
      break;

    default:
      goto error;
      break;
    }

  tok = next_token ();
  switch (tok)
    {
    case ';':
      def = endian;
      break;

    case ':':
      /* This isn't a default after all.  Reset the position to the
	 beginning, and continue processing at the exception list.  */
      p = start;
      goto exceptions;
      break;

    case END:
      def = endian;
      goto end;
      break;

    default:
      goto error;
      break;
    }

 exceptions:

  /* Loop over all exceptions.  */
  while(1)
    {
      tok = next_token ();
      switch (tok)
	{
	case NATIVE:
	  if (next_token () != ':')
	    goto error;
	  endian = GFC_CONVERT_NATIVE;
	  break;

	case SWAP:
	  if (next_token () != ':')
	    goto error;
	  endian = GFC_CONVERT_SWAP;
	  break;

	case LITTLE:
	  if (next_token () != ':')
	    goto error;
	  endian = GFC_CONVERT_LITTLE;
	  break;

	case BIG:
	  if (next_token () != ':')
	    goto error;
	  endian = GFC_CONVERT_BIG;
	  break;

	case INTEGER:
	  push_token ();
	  break;

	case END:
	  goto end;
	  break;

	default:
	  goto error;
	  break;
	}
      /* We arrive here when we want to parse a list of
	 numbers.  */
      continue_ulist = 1;
      do
	{
	  tok = next_token ();
	  if (tok != INTEGER)
	    goto error;

	  unit1 = unit_num;
	  tok = next_token ();
	  /* The number can be followed by a - and another number,
	     which means that this is a unit range, a comma
	     or a semicolon.  */
	  if (tok == '-')
	    {
	      if (next_token () != INTEGER)
		goto error;

	      mark_range (unit1, unit_num);
	      tok = next_token ();
	      if (tok == END)
		goto end;
	      else if (tok == ';')
		continue_ulist = 0;
	      else if (tok != ',')
		goto error;
	    }
	  else
	    {
	      mark_single (unit1);
	      switch (tok)
		{
		case ';':
		  continue_ulist = 0;
		  break;

		case ',':
		  break;

		case END:
		  goto end;
		  break;

		default:
		  goto error;
		}
	    }
	} while (continue_ulist);
    }
 end:
  return 0;
 error:
  def = GFC_CONVERT_NONE;
  return -1;
}

void init_unformatted (variable * v)
{
  char *val;
  val = getenv (v->name);
  def = GFC_CONVERT_NONE;
  n_elist = 0;

  if (val == NULL)
    return;
  do_count = 1;
  p = val;
  do_parse ();
  if (do_count <= 0)
    {
      n_elist = 0;
      elist = NULL;
    }
  else
    {
      elist = xmallocarray (unit_count, sizeof (exception_t));
      do_count = 0;
      p = val;
      do_parse ();
    }
}

/* Get the default conversion for for an unformatted unit.  */

unit_convert
get_unformatted_convert (int unit)
{
  int i;

  if (elist == NULL)
    return def;
  else if (search_unit (unit, &i))
    return elist[i].conv;
  else
    return def;
}
