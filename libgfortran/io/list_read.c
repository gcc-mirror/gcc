/* Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Andy Vaught
   Namelist input contributed by Paul Thomas

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include "config.h"
#include <string.h>
#include <ctype.h>
#include "libgfortran.h"
#include "io.h"


/* List directed input.  Several parsing subroutines are practically
   reimplemented from formatted input, the reason being that there are
   all kinds of small differences between formatted and list directed
   parsing.  */


/* Subroutines for reading characters from the input.  Because a
   repeat count is ambiguous with an integer, we have to read the
   whole digit string before seeing if there is a '*' which signals
   the repeat count.  Since we can have a lot of potential leading
   zeros, we have to be able to back up by arbitrary amount.  Because
   the input might not be seekable, we have to buffer the data
   ourselves.  Data is buffered in scratch[] until it becomes too
   large, after which we start allocating memory on the heap.  */

static int repeat_count, saved_length, saved_used;
static int input_complete, at_eol, comma_flag;
static char last_char, *saved_string;
static bt saved_type;

/* A namelist specific flag used in the list directed library
   to flag that calls are being made from namelist read (eg. to ignore
   comments or to treat '/' as a terminator)  */

static int namelist_mode;

/* A namelist specific flag used in the list directed library to flag
   read errors and return, so that an attempt can be made to read a
   new object name.  */

static int nml_read_error;

/* Storage area for values except for strings.  Must be large enough
   to hold a complex value (two reals) of the largest kind.  */

static char value[20];

#define CASE_DIGITS   case '0': case '1': case '2': case '3': case '4': \
                      case '5': case '6': case '7': case '8': case '9'

#define CASE_SEPARATORS  case ' ': case ',': case '/': case '\n': case '\t': \
                         case '\r'

/* This macro assumes that we're operating on a variable.  */

#define is_separator(c) (c == '/' ||  c == ',' || c == '\n' || c == ' ' \
                         || c == '\t' || c == '\r')

/* Maximum repeat count.  Less than ten times the maximum signed int32.  */

#define MAX_REPEAT 200000000


/* Save a character to a string buffer, enlarging it as necessary.  */

static void
push_char (char c)
{
  char *new;

  if (saved_string == NULL)
    {
      saved_string = scratch;
      memset (saved_string,0,SCRATCH_SIZE);
      saved_length = SCRATCH_SIZE;
      saved_used = 0;
    }

  if (saved_used >= saved_length)
    {
      saved_length = 2 * saved_length;
      new = get_mem (2 * saved_length);

      memset (new,0,2 * saved_length);

      memcpy (new, saved_string, saved_used);
      if (saved_string != scratch)
	free_mem (saved_string);

      saved_string = new;
    }

  saved_string[saved_used++] = c;
}


/* Free the input buffer if necessary.  */

static void
free_saved (void)
{
  if (saved_string == NULL)
    return;

  if (saved_string != scratch)
    free_mem (saved_string);

  saved_string = NULL;
}


static char
next_char (void)
{
  int length;
  char c, *p;

  if (last_char != '\0')
    {
      at_eol = 0;
      c = last_char;
      last_char = '\0';
      goto done;
    }

  length = 1;

  p = salloc_r (current_unit->s, &length);
  if (p == NULL)
    {
      generate_error (ERROR_OS, NULL);
      return '\0';
    }

  if (length == 0)
    {
      /* For internal files return a newline instead of signalling EOF.  */
      /* ??? This isn't quite right, but we don't handle internal files
	 with multiple records.  */
      if (is_internal_unit ())
	c = '\n';
      else
	longjmp (g.eof_jump, 1);
    }
  else
    c = *p;

done:
  at_eol = (c == '\n' || c == '\r');
  return c;
}


/* Push a character back onto the input.  */

static void
unget_char (char c)
{
  last_char = c;
}


/* Skip over spaces in the input.  Returns the nonspace character that
   terminated the eating and also places it back on the input.  */

static char
eat_spaces (void)
{
  char c;

  do
    {
      c = next_char ();
    }
  while (c == ' ' || c == '\t');

  unget_char (c);
  return c;
}


/* Skip over a separator.  Technically, we don't always eat the whole
   separator.  This is because if we've processed the last input item,
   then a separator is unnecessary.  Plus the fact that operating
   systems usually deliver console input on a line basis.

   The upshot is that if we see a newline as part of reading a
   separator, we stop reading.  If there are more input items, we
   continue reading the separator with finish_separator() which takes
   care of the fact that we may or may not have seen a comma as part
   of the separator.  */

static void
eat_separator (void)
{
  char c;

  eat_spaces ();
  comma_flag = 0;

  c = next_char ();
  switch (c)
    {
    case ',':
      comma_flag = 1;
      eat_spaces ();
      break;

    case '/':
      input_complete = 1;
      break;

    case '\n':
    case '\r':
      at_eol = 1;
      break;

    case '!':
      if (namelist_mode)
	{			/* Eat a namelist comment.  */
	  do
	    c = next_char ();
	  while (c != '\n');

	  break;
	}

      /* Fall Through...  */

    default:
      unget_char (c);
      break;
    }
}


/* Finish processing a separator that was interrupted by a newline.
   If we're here, then another data item is present, so we finish what
   we started on the previous line.  */

static void
finish_separator (void)
{
  char c;

 restart:
  eat_spaces ();

  c = next_char ();
  switch (c)
    {
    case ',':
      if (comma_flag)
	unget_char (c);
      else
	{
	  c = eat_spaces ();
	  if (c == '\n')
	    goto restart;
	}

      break;

    case '/':
      input_complete = 1;
      if (!namelist_mode) next_record (0);
      break;

    case '\n':
    case '\r':
      goto restart;

    case '!':
      if (namelist_mode)
	{
	  do
	    c = next_char ();
	  while (c != '\n');

	  goto restart;
	}

    default:
      unget_char (c);
      break;
    }
}

/* This function is needed to catch bad conversions so that namelist can
   attempt to see if saved_string contains a new object name rather than
   a bad value.  */

static int
nml_bad_return (char c)
{
  if (namelist_mode)
    {
      nml_read_error = 1;
      unget_char(c);
      return 1;
    }
  return 0;
}

/* Convert an unsigned string to an integer.  The length value is -1
   if we are working on a repeat count.  Returns nonzero if we have a
   range problem.  As a side effect, frees the saved_string.  */

static int
convert_integer (int length, int negative)
{
  char c, *buffer, message[100];
  int m;
  int64_t v, max, max10;

  buffer = saved_string;
  v = 0;

  max = (length == -1) ? MAX_REPEAT : max_value (length, 1);
  max10 = max / 10;

  for (;;)
    {
      c = *buffer++;
      if (c == '\0')
	break;
      c -= '0';

      if (v > max10)
	goto overflow;
      v = 10 * v;

      if (v > max - c)
	goto overflow;
      v += c;
    }

  m = 0;

  if (length != -1)
    {
      if (negative)
	v = -v;
      set_integer (value, v, length);
    }
  else
    {
      repeat_count = v;

      if (repeat_count == 0)
	{
	  st_sprintf (message, "Zero repeat count in item %d of list input",
		      g.item_count);

	  generate_error (ERROR_READ_VALUE, message);
	  m = 1;
	}
    }

  free_saved ();
  return m;

 overflow:
  if (length == -1)
    st_sprintf (message, "Repeat count overflow in item %d of list input",
		g.item_count);
  else
    st_sprintf (message, "Integer overflow while reading item %d",
		g.item_count);

  free_saved ();
  generate_error (ERROR_READ_VALUE, message);

  return 1;
}


/* Parse a repeat count for logical and complex values which cannot
   begin with a digit.  Returns nonzero if we are done, zero if we
   should continue on.  */

static int
parse_repeat (void)
{
  char c, message[100];
  int repeat;

  c = next_char ();
  switch (c)
    {
    CASE_DIGITS:
      repeat = c - '0';
      break;

    CASE_SEPARATORS:
      unget_char (c);
      eat_separator ();
      return 1;

    default:
      unget_char (c);
      return 0;
    }

  for (;;)
    {
      c = next_char ();
      switch (c)
	{
	CASE_DIGITS:
	  repeat = 10 * repeat + c - '0';

	  if (repeat > MAX_REPEAT)
	    {
	      st_sprintf (message,
			  "Repeat count overflow in item %d of list input",
			  g.item_count);

	      generate_error (ERROR_READ_VALUE, message);
	      return 1;
	    }

	  break;

	case '*':
	  if (repeat == 0)
	    {
	      st_sprintf (message,
			  "Zero repeat count in item %d of list input",
			  g.item_count);

	      generate_error (ERROR_READ_VALUE, message);
	      return 1;
	    }

	  goto done;

	default:
	  goto bad_repeat;
	}
    }

 done:
  repeat_count = repeat;
  return 0;

 bad_repeat:
  st_sprintf (message, "Bad repeat count in item %d of list input",
	      g.item_count);

  generate_error (ERROR_READ_VALUE, message);
  return 1;
}


/* Read a logical character on the input.  */

static void
read_logical (int length)
{
  char c, message[100];
  int v;

  if (parse_repeat ())
    return;

  c = next_char ();
  switch (c)
    {
    case 't':
    case 'T':
      v = 1;
      break;
    case 'f':
    case 'F':
      v = 0;
      break;

    case '.':
      c = next_char ();
      switch (c)
	{
	case 't':
	case 'T':
	  v = 1;
	  break;
	case 'f':
	case 'F':
	  v = 0;
	  break;
	default:
	  goto bad_logical;
	}

      break;

    CASE_SEPARATORS:
      unget_char (c);
      eat_separator ();
      return;			/* Null value.  */

    default:
      goto bad_logical;
    }

  saved_type = BT_LOGICAL;
  saved_length = length;

  /* Eat trailing garbage.  */
  do
    {
      c = next_char ();
    }
  while (!is_separator (c));

  unget_char (c);
  eat_separator ();
  free_saved ();
  set_integer ((int *) value, v, length);

  return;

 bad_logical:

  if (nml_bad_return (c))
    return;

  st_sprintf (message, "Bad logical value while reading item %d",
	      g.item_count);

  generate_error (ERROR_READ_VALUE, message);
}


/* Reading integers is tricky because we can actually be reading a
   repeat count.  We have to store the characters in a buffer because
   we could be reading an integer that is larger than the default int
   used for repeat counts.  */

static void
read_integer (int length)
{
  char c, message[100];
  int negative;

  negative = 0;

  c = next_char ();
  switch (c)
    {
    case '-':
      negative = 1;
      /* Fall through...  */

    case '+':
      c = next_char ();
      goto get_integer;

    CASE_SEPARATORS:		/* Single null.  */
      unget_char (c);
      eat_separator ();
      return;

    CASE_DIGITS:
      push_char (c);
      break;

    default:
      goto bad_integer;
    }

  /* Take care of what may be a repeat count.  */

  for (;;)
    {
      c = next_char ();
      switch (c)
	{
	CASE_DIGITS:
	  push_char (c);
	  break;

	case '*':
	  push_char ('\0');
	  goto repeat;

	CASE_SEPARATORS:	/* Not a repeat count.  */
	  goto done;

	default:
	  goto bad_integer;
	}
    }

 repeat:
  if (convert_integer (-1, 0))
    return;

  /* Get the real integer.  */

  c = next_char ();
  switch (c)
    {
    CASE_DIGITS:
      break;

    CASE_SEPARATORS:
      unget_char (c);
      eat_separator ();
      return;

    case '-':
      negative = 1;
      /* Fall through...  */

    case '+':
      c = next_char ();
      break;
    }

 get_integer:
  if (!isdigit (c))
    goto bad_integer;
  push_char (c);

  for (;;)
    {
      c = next_char ();
      switch (c)
	{
	CASE_DIGITS:
	  push_char (c);
	  break;

	CASE_SEPARATORS:
	  goto done;

	default:
	  goto bad_integer;
	}
    }

 bad_integer:

  if (nml_bad_return (c))
    return;

  free_saved ();

  st_sprintf (message, "Bad integer for item %d in list input", g.item_count);
  generate_error (ERROR_READ_VALUE, message);

  return;

 done:
  unget_char (c);
  eat_separator ();

  push_char ('\0');
  if (convert_integer (length, negative))
    {
       free_saved ();
       return;
    }

  free_saved ();
  saved_type = BT_INTEGER;
}


/* Read a character variable.  */

static void
read_character (int length)
{
  char c, quote, message[100];

  quote = ' ';			/* Space means no quote character.  */

  c = next_char ();
  switch (c)
    {
    CASE_DIGITS:
      push_char (c);
      break;

    CASE_SEPARATORS:
      unget_char (c);		/* NULL value.  */
      eat_separator ();
      return;

    case '"':
    case '\'':
      quote = c;
      goto get_string;

    default:
      push_char (c);
      goto get_string;
    }

  /* Deal with a possible repeat count.  */

  for (;;)
    {
      c = next_char ();
      switch (c)
	{
	CASE_DIGITS:
	  push_char (c);
	  break;

	CASE_SEPARATORS:
	  unget_char (c);
	  goto done;		/* String was only digits!  */

	case '*':
	  push_char ('\0');
	  goto got_repeat;

	default:
	  push_char (c);
	  goto get_string;	/* Not a repeat count after all.  */
	}
    }

 got_repeat:
  if (convert_integer (-1, 0))
    return;

  /* Now get the real string.  */

  c = next_char ();
  switch (c)
    {
    CASE_SEPARATORS:
      unget_char (c);		/* Repeated NULL values.  */
      eat_separator ();
      return;

    case '"':
    case '\'':
      quote = c;
      break;

    default:
      push_char (c);
      break;
    }

 get_string:
  for (;;)
    {
      c = next_char ();
      switch (c)
	{
	case '"':
	case '\'':
	  if (c != quote)
	    {
	      push_char (c);
	      break;
	    }

	  /* See if we have a doubled quote character or the end of
	     the string.  */

	  c = next_char ();
	  if (c == quote)
	    {
	      push_char (quote);
	      break;
	    }

	  unget_char (c);
	  goto done;

	CASE_SEPARATORS:
	  if (quote == ' ')
	    {
	      unget_char (c);
	      goto done;
	    }

	  if (c != '\n')
	    push_char (c);
	  break;

	default:
	  push_char (c);
	  break;
	}
    }

  /* At this point, we have to have a separator, or else the string is
     invalid.  */
 done:
  c = next_char ();
  if (is_separator (c))
    {
      unget_char (c);
      eat_separator ();
      saved_type = BT_CHARACTER;
    }
  else
    {
      free_saved ();
      st_sprintf (message, "Invalid string input in item %d", g.item_count);
      generate_error (ERROR_READ_VALUE, message);
    }
}


/* Parse a component of a complex constant or a real number that we
   are sure is already there.  This is a straight real number parser.  */

static int
parse_real (void *buffer, int length)
{
  char c, message[100];
  int m, seen_dp;

  c = next_char ();
  if (c == '-' || c == '+')
    {
      push_char (c);
      c = next_char ();
    }

  if (!isdigit (c) && c != '.')
    goto bad;

  push_char (c);

  seen_dp = (c == '.') ? 1 : 0;

  for (;;)
    {
      c = next_char ();
      switch (c)
	{
	CASE_DIGITS:
	  push_char (c);
	  break;

	case '.':
	  if (seen_dp)
	    goto bad;

	  seen_dp = 1;
	  push_char (c);
	  break;

	case 'e':
	case 'E':
	case 'd':
	case 'D':
	  push_char ('e');
	  goto exp1;

	case '-':
	case '+':
	  push_char ('e');
	  push_char (c);
	  c = next_char ();
	  goto exp2;

	CASE_SEPARATORS:
	  unget_char (c);
	  goto done;

	default:
	  goto done;
	}
    }

 exp1:
  c = next_char ();
  if (c != '-' && c != '+')
    push_char ('+');
  else
    {
      push_char (c);
      c = next_char ();
    }

 exp2:
  if (!isdigit (c))
    goto bad;
  push_char (c);

  for (;;)
    {
      c = next_char ();
      switch (c)
	{
	CASE_DIGITS:
	  push_char (c);
	  break;

	CASE_SEPARATORS:
	  unget_char (c);
	  goto done;

	default:
	  goto done;
	}
    }

 done:
  unget_char (c);
  push_char ('\0');

  m = convert_real (buffer, saved_string, length);
  free_saved ();

  return m;

 bad:
  free_saved ();
  st_sprintf (message, "Bad floating point number for item %d", g.item_count);
  generate_error (ERROR_READ_VALUE, message);

  return 1;
}


/* Reading a complex number is straightforward because we can tell
   what it is right away.  */

static void
read_complex (int length)
{
  char message[100];
  char c;

  if (parse_repeat ())
    return;

  c = next_char ();
  switch (c)
    {
    case '(':
      break;

    CASE_SEPARATORS:
      unget_char (c);
      eat_separator ();
      return;

    default:
      goto bad_complex;
    }

  eat_spaces ();
  if (parse_real (value, length))
    return;

  eat_spaces ();
  if (next_char () != ',')
    goto bad_complex;

  eat_spaces ();
  if (parse_real (value + length, length))
    return;

  eat_spaces ();
  if (next_char () != ')')
    goto bad_complex;

  c = next_char ();
  if (!is_separator (c))
    goto bad_complex;

  unget_char (c);
  eat_separator ();

  free_saved ();
  saved_type = BT_COMPLEX;
  return;

 bad_complex:

  if (nml_bad_return (c))
    return;

  st_sprintf (message, "Bad complex value in item %d of list input",
	      g.item_count);

  generate_error (ERROR_READ_VALUE, message);
}


/* Parse a real number with a possible repeat count.  */

static void
read_real (int length)
{
  char c, message[100];
  int seen_dp;

  seen_dp = 0;

  c = next_char ();
  switch (c)
    {
    CASE_DIGITS:
      push_char (c);
      break;

    case '.':
      push_char (c);
      seen_dp = 1;
      break;

    case '+':
    case '-':
      goto got_sign;

    CASE_SEPARATORS:
      unget_char (c);		/* Single null.  */
      eat_separator ();
      return;

    default:
      goto bad_real;
    }

  /* Get the digit string that might be a repeat count.  */

  for (;;)
    {
      c = next_char ();
      switch (c)
	{
	CASE_DIGITS:
	  push_char (c);
	  break;

	case '.':
          if (seen_dp)
            goto bad_real;

	  seen_dp = 1;
	  push_char (c);
	  goto real_loop;

	case 'E':
	case 'e':
	case 'D':
	case 'd':
	  goto exp1;

	case '+':
	case '-':
	  push_char ('e');
	  push_char (c);
	  c = next_char ();
	  goto exp2;

	case '*':
	  push_char ('\0');
	  goto got_repeat;

	CASE_SEPARATORS:
          if (c != '\n' &&  c != ',' && c != '\r')
            unget_char (c);

	  goto done;

	default:
	  goto bad_real;
	}
    }

 got_repeat:
  if (convert_integer (-1, 0))
    return;

  /* Now get the number itself.  */

  c = next_char ();
  if (is_separator (c))
    {				/* Repeated null value.  */
      unget_char (c);
      eat_separator ();
      return;
    }

  if (c != '-' && c != '+')
    push_char ('+');
  else
    {
    got_sign:
      push_char (c);
      c = next_char ();
    }

  if (!isdigit (c) && c != '.')
    goto bad_real;

  if (c == '.')
    {
      if (seen_dp)
        goto bad_real;
      else
        seen_dp = 1;
    }

  push_char (c);

 real_loop:
  for (;;)
    {
      c = next_char ();
      switch (c)
	{
	CASE_DIGITS:
	  push_char (c);
	  break;

	CASE_SEPARATORS:
	  goto done;

	case '.':
	  if (seen_dp)
	    goto bad_real;

	  seen_dp = 1;
	  push_char (c);
	  break;

	case 'E':
	case 'e':
	case 'D':
	case 'd':
	  goto exp1;

	case '+':
	case '-':
	  push_char ('e');
	  push_char (c);
	  c = next_char ();
	  goto exp2;

	default:
	  goto bad_real;
	}
    }

 exp1:
  push_char ('e');

  c = next_char ();
  if (c != '+' && c != '-')
    push_char ('+');
  else
    {
      push_char (c);
      c = next_char ();
    }

 exp2:
  if (!isdigit (c))
    goto bad_real;
  push_char (c);

  for (;;)
    {
      c = next_char ();

      switch (c)
	{
	CASE_DIGITS:
	  push_char (c);
	  break;

	CASE_SEPARATORS:
	  goto done;

	default:
	  goto bad_real;
	}
    }

 done:
  unget_char (c);
  eat_separator ();
  push_char ('\0');
  if (convert_real (value, saved_string, length))
    return;

  free_saved ();
  saved_type = BT_REAL;
  return;

 bad_real:

  if (nml_bad_return (c))
    return;

  st_sprintf (message, "Bad real number in item %d of list input",
	      g.item_count);

  generate_error (ERROR_READ_VALUE, message);
}


/* Check the current type against the saved type to make sure they are
   compatible.  Returns nonzero if incompatible.  */

static int
check_type (bt type, int len)
{
  char message[100];

  if (saved_type != BT_NULL && saved_type != type)
    {
      st_sprintf (message, "Read type %s where %s was expected for item %d",
		  type_name (saved_type), type_name (type), g.item_count);

      generate_error (ERROR_READ_VALUE, message);
      return 1;
    }

  if (saved_type == BT_NULL || saved_type == BT_CHARACTER)
    return 0;

  if (saved_length != len)
    {
      st_sprintf (message,
		  "Read kind %d %s where kind %d is required for item %d",
		  saved_length, type_name (saved_type), len, g.item_count);
      generate_error (ERROR_READ_VALUE, message);
      return 1;
    }

  return 0;
}


/* Top level data transfer subroutine for list reads.  Because we have
   to deal with repeat counts, the data item is always saved after
   reading, usually in the value[] array.  If a repeat count is
   greater than one, we copy the data item multiple times.  */

void
list_formatted_read (bt type, void *p, int len)
{
  char c;
  int m;

  namelist_mode = 0;

  if (setjmp (g.eof_jump))
    {
      generate_error (ERROR_END, NULL);
      return;
    }

  if (g.first_item)
    {
      g.first_item = 0;
      input_complete = 0;
      repeat_count = 1;
      at_eol = 0;

      c = eat_spaces ();
      if (is_separator (c))
	{			/* Found a null value.  */
	  eat_separator ();
	  repeat_count = 0;
	  if (at_eol)
            finish_separator ();
          else
            return;
	}

    }
  else
    {
      if (input_complete)
	return;

      if (repeat_count > 0)
	{
	  if (check_type (type, len))
	    return;
	  goto set_value;
	}

      if (at_eol)
        finish_separator ();
      else
        {
          eat_spaces ();
          /* trailing spaces prior to end of line */
          if (at_eol)
            finish_separator ();
        }

      saved_type = BT_NULL;
      repeat_count = 1;
    }

  switch (type)
    {
    case BT_INTEGER:
      read_integer (len);
      break;
    case BT_LOGICAL:
      read_logical (len);
      break;
    case BT_CHARACTER:
      read_character (len);
      break;
    case BT_REAL:
      read_real (len);
      break;
    case BT_COMPLEX:
      read_complex (len);
      break;
    default:
      internal_error ("Bad type for list read");
    }

  if (saved_type != BT_CHARACTER && saved_type != BT_NULL)
    saved_length = len;

  if (ioparm.library_return != LIBRARY_OK)
    return;

 set_value:
  switch (saved_type)
    {
    case BT_COMPLEX:
      len = 2 * len;
      /* Fall through.  */

    case BT_INTEGER:
    case BT_REAL:
    case BT_LOGICAL:
      memcpy (p, value, len);
      break;

    case BT_CHARACTER:
      if (saved_string)
       { 
          m = (len < saved_used) ? len : saved_used;
          memcpy (p, saved_string, m);
       }
      else    
	/* Just delimiters encountered, nothing to copy but SPACE.  */
        m = 0;

      if (m < len)
	memset (((char *) p) + m, ' ', len - m);
      break;

    case BT_NULL:
      break;
    }

  if (--repeat_count <= 0)
    free_saved ();
}

void
init_at_eol(void)
{
  at_eol = 0;
}

/* Finish a list read.  */

void
finish_list_read (void)
{
  char c;

  free_saved ();

  if (at_eol)
    {
      at_eol = 0;
      return;
    }

  do
    {
      c = next_char ();
    }
  while (c != '\n');
}

/*			NAMELIST INPUT

void namelist_read (void)
calls:
   static void nml_match_name (char *name, int len)
   static int nml_query (void)
   static int nml_get_obj_data (void)
calls:
      static void nml_untouch_nodes (void)
      static namelist_info * find_nml_node (char * var_name)
      static int nml_parse_qualifier(descriptor_dimension * ad,
				     nml_loop_spec * ls, int rank)
      static void nml_touch_nodes (namelist_info * nl)
      static int nml_read_obj (namelist_info * nl, index_type offset)
calls:
      -itself-  */

/* Carries error messages from the qualifier parser.  */
static char parse_err_msg[30];

/* Carries error messages for error returns.  */
static char nml_err_msg[100];

/* Pointer to the previously read object, in case attempt is made to read
   new object name.  Should this fail, error message can give previous
   name.  */

static namelist_info * prev_nl;

/* Lower index for substring qualifier.  */

static index_type clow;

/* Upper index for substring qualifier.  */

static index_type chigh;

/* Inputs a rank-dimensional qualifier, which can contain
   singlets, doublets, triplets or ':' with the standard meanings.  */

static try
nml_parse_qualifier(descriptor_dimension * ad,
		    nml_loop_spec * ls, int rank)
{
  int dim;
  int indx;
  int neg;
  int null_flag;
  char c;

  /* The next character in the stream should be the '('.  */

  c = next_char ();

  /* Process the qualifier, by dimension and triplet.  */

  for (dim=0; dim < rank; dim++ )
    {
      for (indx=0; indx<3; indx++)
	{
	  free_saved ();
	  eat_spaces ();
	  neg = 0;

	  /*process a potential sign.  */

	  c = next_char ();
	  switch (c)
	    {
	    case '-':
	      neg = 1;
	      break;

	    case '+':
	      break;

	    default:
	      unget_char (c);
	      break;
	    }

	  /*process characters up to the next ':' , ',' or ')'  */

	  for (;;)
	    {
	      c = next_char ();

	      switch (c)
		{
		case ':':
		  break;

		case ',': case ')':
		  if ( (c==',' && dim == rank -1)
		    || (c==')' && dim  < rank -1))
		    {
		      st_sprintf (parse_err_msg,
				  "Bad number of index fields");
		      goto err_ret;
		    }
		  break;

		CASE_DIGITS:
		  push_char (c);
		  continue;

		case ' ': case '\t':
		  eat_spaces ();
		  c = next_char ();
		  break;

		default:
		  st_sprintf (parse_err_msg, "Bad character in index");
		  goto err_ret;
		}

	      if (( c==',' || c==')') && indx==0 && saved_string == 0 )
		{
		  st_sprintf (parse_err_msg, "Null index field");
		  goto err_ret;
		}

	      if ( ( c==':' && indx==1 && saved_string == 0)
		|| (indx==2 && saved_string == 0))
		{
		  st_sprintf(parse_err_msg, "Bad index triplet");
		  goto err_ret;
		}

	      /* If '( : ? )' or '( ? : )' break and flag read failure.  */
	      null_flag = 0;
	      if ( (c==':'  && indx==0 && saved_string == 0)
		|| (indx==1 && saved_string == 0))
		{
		  null_flag = 1;
		  break;
		}

	      /* Now read the index.  */

	      if (convert_integer (sizeof(int),neg))
		{
		  st_sprintf (parse_err_msg, "Bad integer in index");
		  goto err_ret;
		}
	      break;
	    }

	  /*feed the index values to the triplet arrays.  */

	  if (!null_flag)
	    {
	      if (indx == 0)
		ls[dim].start = *(int *)value;
	      if (indx == 1)
		ls[dim].end   = *(int *)value;
	      if (indx == 2)
		ls[dim].step  = *(int *)value;
	    }

	  /*singlet or doublet indices  */

	  if (c==',' || c==')')
	    {
	      if (indx == 0)
		{
		  ls[dim].start = *(int *)value;
		  ls[dim].end = *(int *)value;
		}
	      break;
	    }
	}

      /*Check the values of the triplet indices.  */

      if ( (ls[dim].start > (ssize_t)ad[dim].ubound) 
	|| (ls[dim].start < (ssize_t)ad[dim].lbound)
	|| (ls[dim].end   > (ssize_t)ad[dim].ubound)
	|| (ls[dim].end   < (ssize_t)ad[dim].lbound))
	{
	  st_sprintf (parse_err_msg, "Index %d out of range", dim + 1);
	  goto err_ret;
	}
      if (((ls[dim].end - ls[dim].start ) * ls[dim].step < 0)
	|| (ls[dim].step == 0))
	{
	  st_sprintf (parse_err_msg, "Bad range in index %d", dim + 1);
	  goto err_ret;
	}

      /* Initialise the loop index counter.  */

      ls[dim].idx = ls[dim].start;

    }
  eat_spaces ();
  return SUCCESS;

err_ret:

  return FAILURE;
}

static namelist_info *
find_nml_node (char * var_name)
{
  namelist_info * t = ionml;
  while (t != NULL)
    {
      if (strcmp (var_name,t->var_name) == 0)
	{
	  t->touched = 1;
	  return t;
	}
      t = t->next;
    }
  return NULL;
}

/* Visits all the components of a derived type that have
   not explicitly been identified in the namelist input.
   touched is set and the loop specification initialised 
   to default values  */

static void
nml_touch_nodes (namelist_info * nl)
{
  index_type len = strlen (nl->var_name) + 1;
  int dim;
  char * ext_name = (char*)get_mem (len + 1);
  strcpy (ext_name, nl->var_name);
  strcat (ext_name, "%");
  for (nl = nl->next; nl; nl = nl->next)
    {
      if (strncmp (nl->var_name, ext_name, len) == 0)
	{
	  nl->touched = 1;
	  for (dim=0; dim < nl->var_rank; dim++)
	    {
	      nl->ls[dim].step = 1;
	      nl->ls[dim].end = nl->dim[dim].ubound;
	      nl->ls[dim].start = nl->dim[dim].lbound;
	      nl->ls[dim].idx = nl->ls[dim].start;
	    }
	}
      else
	break;
    }
  free_mem (ext_name);
  return;
}

/* Resets touched for the entire list of nml_nodes, ready for a
   new object.  */

static void
nml_untouch_nodes (void)
{
  namelist_info * t;
  for (t = ionml; t; t = t->next)
    t->touched = 0;
  return;
}

/* Attempts to input name to namelist name.  Returns nml_read_error = 1
   on no match.  */

static void
nml_match_name (char *name, index_type len)
{
  index_type i;
  char c;
  nml_read_error = 0;
  for (i = 0; i < len; i++)
    {
      c = next_char ();
      if (tolower (c) != tolower (name[i]))
	{
	  nml_read_error = 1;
	  break;
	}
    }
}

/* If the namelist read is from stdin, output the current state of the
   namelist to stdout.  This is used to implement the non-standard query
   features, ? and =?. If c == '=' the full namelist is printed. Otherwise
   the names alone are printed.  */

static void
nml_query (char c)
{
  gfc_unit * temp_unit;
  namelist_info * nl;
  index_type len;
  char * p;

  if (current_unit->unit_number != options.stdin_unit)
    return;

  /* Store the current unit and transfer to stdout.  */

  temp_unit = current_unit;
  current_unit = find_unit (options.stdout_unit);

  if (current_unit)
    {
      g.mode =WRITING;
      next_record (0);

      /* Write the namelist in its entirety.  */

      if (c == '=')
	namelist_write ();

      /* Or write the list of names.  */

      else
	{

	  /* "&namelist_name\n"  */

	  len = ioparm.namelist_name_len;
	  p = write_block (len + 2);
	  if (!p)
	    goto query_return;
	  memcpy (p, "&", 1);
	  memcpy ((char*)(p + 1), ioparm.namelist_name, len);
	  memcpy ((char*)(p + len + 1), "\n", 1);
	  for (nl =ionml; nl; nl = nl->next)
	    {

	      /* " var_name\n"  */

	      len = strlen (nl->var_name);
	      p = write_block (len + 2);
	      if (!p)
		goto query_return;
	      memcpy (p, " ", 1);
	      memcpy ((char*)(p + 1), nl->var_name, len);
	      memcpy ((char*)(p + len + 1), "\n", 1);
	    }

	  /* "&end\n"  */

	  p = write_block (5);
	  if (!p)
	    goto query_return;
	  memcpy (p, "&end\n", 5);
	}

      /* Flush the stream to force immediate output.  */

      flush (current_unit->s);
    }

query_return:

  /* Restore the current unit.  */

  current_unit = temp_unit;
  g.mode = READING;
  return;
}

/* Reads and stores the input for the namelist object nl.  For an array,
   the function loops over the ranges defined by the loop specification.
   This default to all the data or to the specification from a qualifier.
   nml_read_obj recursively calls itself to read derived types. It visits
   all its own components but only reads data for those that were touched
   when the name was parsed.  If a read error is encountered, an attempt is
   made to return to read a new object name because the standard allows too
   little data to be available.  On the other hand, too much data is an
   error.  */

static try
nml_read_obj (namelist_info * nl, index_type offset)
{

  namelist_info * cmp;
  char * obj_name;
  int nml_carry;
  int len;
  int dim;
  index_type dlen;
  index_type m;
  index_type obj_name_len;
  void * pdata ;

  /* This object not touched in name parsing.  */

  if (!nl->touched)
    return SUCCESS;

  repeat_count = 0;
  eat_spaces();

  len = nl->len;
  switch (nl->type)
  {

    case GFC_DTYPE_INTEGER:
    case GFC_DTYPE_LOGICAL:
    case GFC_DTYPE_REAL:
      dlen = len;
      break;

    case GFC_DTYPE_COMPLEX:
      dlen = 2* len;
      break;

    case GFC_DTYPE_CHARACTER:
      dlen = chigh ? (chigh - clow + 1) : nl->string_length;
      break;

    default:
      dlen = 0;
    }

  do
    {

      /* Update the pointer to the data, using the current index vector  */

      pdata = (void*)(nl->mem_pos + offset);
      for (dim = 0; dim < nl->var_rank; dim++)
	pdata = (void*)(pdata + (nl->ls[dim].idx - nl->dim[dim].lbound) *
		 nl->dim[dim].stride * nl->size);

      /* Reset the error flag and try to read next value, if 
	 repeat_count=0  */

      nml_read_error = 0;
      nml_carry = 0;
      if (--repeat_count <= 0)
	{
	  if (input_complete)
	    return SUCCESS;
	  if (at_eol)
	    finish_separator ();
	  if (input_complete)
	    return SUCCESS;

	  /* GFC_TYPE_UNKNOWN through for nulls and is detected
	     after the switch block.  */

	  saved_type = GFC_DTYPE_UNKNOWN;
	  free_saved ();
 
          switch (nl->type)
	  {
	  case GFC_DTYPE_INTEGER:
              read_integer (len);
              break;

	  case GFC_DTYPE_LOGICAL:
              read_logical (len);
              break;

	  case GFC_DTYPE_CHARACTER:
              read_character (len);
              break;

	  case GFC_DTYPE_REAL:
              read_real (len);
              break;

	  case GFC_DTYPE_COMPLEX:
              read_complex (len);
              break;

	  case GFC_DTYPE_DERIVED:
	    obj_name_len = strlen (nl->var_name) + 1;
	    obj_name = get_mem (obj_name_len+1);
	    strcpy (obj_name, nl->var_name);
	    strcat (obj_name, "%");

	    /* Now loop over the components. Update the component pointer
	       with the return value from nml_write_obj.  This loop jumps
	       past nested derived types by testing if the potential 
	       component name contains '%'.  */

	    for (cmp = nl->next;
		 cmp &&
		   !strncmp (cmp->var_name, obj_name, obj_name_len) &&
		   !strchr (cmp->var_name + obj_name_len, '%');
		 cmp = cmp->next)
	      {

		if (nml_read_obj (cmp, (index_type)(pdata - nl->mem_pos)) == FAILURE)
		  {
		    free_mem (obj_name);
		    return FAILURE;
		  }

		if (input_complete)
		  {
		    free_mem (obj_name);
		    return SUCCESS;
		  }
	      }

	    free_mem (obj_name);
	    goto incr_idx;

          default:
	    st_sprintf (nml_err_msg, "Bad type for namelist object %s",
			nl->var_name );
	    internal_error (nml_err_msg);
	    goto nml_err_ret;
          }
        }

      /* The standard permits array data to stop short of the number of
	 elements specified in the loop specification.  In this case, we
	 should be here with nml_read_error != 0.  Control returns to 
	 nml_get_obj_data and an attempt is made to read object name.  */

      prev_nl = nl;
      if (nml_read_error)
	return SUCCESS;

      if (saved_type == GFC_DTYPE_UNKNOWN)
	goto incr_idx;


      /* Note the switch from GFC_DTYPE_type to BT_type at this point.
	 This comes about because the read functions return BT_types.  */

      switch (saved_type)
      {

	case BT_COMPLEX:
	case BT_REAL:
	case BT_INTEGER:
	case BT_LOGICAL:
	  memcpy (pdata, value, dlen);
	  break;

	case BT_CHARACTER:
	  m = (dlen < saved_used) ? dlen : saved_used;
	  pdata = (void*)( pdata + clow - 1 );
	  memcpy (pdata, saved_string, m);
	  if (m < dlen)
	    memset ((void*)( pdata + m ), ' ', dlen - m);
	break;

	default:
	  break;
      }

      /* Break out of loop if scalar.  */

      if (!nl->var_rank)
	break;

      /* Now increment the index vector.  */

incr_idx:

      nml_carry = 1;
      for (dim = 0; dim < nl->var_rank; dim++)
	{
	  nl->ls[dim].idx += nml_carry * nl->ls[dim].step;
	  nml_carry = 0;
	  if (((nl->ls[dim].step > 0) && (nl->ls[dim].idx > nl->ls[dim].end))
	      ||
	      ((nl->ls[dim].step < 0) && (nl->ls[dim].idx < nl->ls[dim].end)))
	    {
	      nl->ls[dim].idx = nl->ls[dim].start;
	      nml_carry = 1;
	    }
        }
    } while (!nml_carry);

  if (repeat_count > 1)
    {
       st_sprintf (nml_err_msg, "Repeat count too large for namelist object %s" ,
		   nl->var_name );
       goto nml_err_ret;
    }
  return SUCCESS;

nml_err_ret:

  return FAILURE;
}

/* Parses the object name, including array and substring qualifiers.  It
   iterates over derived type components, touching those components and
   setting their loop specifications, if there is a qualifier.  If the
   object is itself a derived type, its components and subcomponents are
   touched.  nml_read_obj is called at the end and this reads the data in
   the manner specified by the object name.  */

static try
nml_get_obj_data (void)
{
  char c;
  char * ext_name;
  namelist_info * nl;
  namelist_info * first_nl;
  namelist_info * root_nl;
  int dim;
  int component_flag;

  /* Look for end of input or object name.  If '?' or '=?' are encountered
     in stdin, print the node names or the namelist to stdout.  */

  eat_separator ();
  if (input_complete)
    return SUCCESS;

  if ( at_eol )
    finish_separator ();
  if (input_complete)
    return SUCCESS;

  c = next_char ();
  switch (c)
    {
    case '=':
      c = next_char ();
      if (c != '?')
	{
	  st_sprintf (nml_err_msg, "namelist read: missplaced = sign");
	  goto nml_err_ret;
	}
      nml_query ('=');
      return SUCCESS;

    case '?':
      nml_query ('?');
      return SUCCESS;

    case '$':
    case '&':
      nml_match_name ("end", 3);
      if (nml_read_error)
	{
	  st_sprintf (nml_err_msg, "namelist not terminated with / or &end");
	  goto nml_err_ret;
	}
    case '/':
      input_complete = 1;
      return SUCCESS;

    default :
      break;
    }

  /* Untouch all nodes of the namelist and reset the flag that is set for
     derived type components.  */

  nml_untouch_nodes();
  component_flag = 0;

  /* Get the object name - should '!' and '\n' be permitted separators?  */

get_name:

  free_saved ();

  do
    {
      push_char(tolower(c));
      c = next_char ();
    } while (!( c=='=' || c==' ' || c=='\t' || c =='(' || c =='%' ));

  unget_char (c);

  /* Check that the name is in the namelist and get pointer to object.
     Three error conditions exist: (i) An attempt is being made to
     identify a non-existent object, following a failed data read or
     (ii) The object name does not exist or (iii) Too many data items
     are present for an object.  (iii) gives the same error message
     as (i)  */

  push_char ('\0');

  if (component_flag)
    {
      ext_name = (char*)get_mem (strlen (root_nl->var_name)
				  + (saved_string ? strlen (saved_string) : 0)
				  + 1);
      strcpy (ext_name, root_nl->var_name);
      strcat (ext_name, saved_string);
      nl = find_nml_node (ext_name);
      free_mem (ext_name);
    }
  else
    nl = find_nml_node (saved_string);

  if (nl == NULL)
    {
      if (nml_read_error && prev_nl)
	st_sprintf (nml_err_msg, "Bad data for namelist object %s",
		    prev_nl->var_name);

      else
	st_sprintf (nml_err_msg, "Cannot match namelist object name %s",
		    saved_string);

      goto nml_err_ret;
    }

  /* Get the length, data length, base pointer and rank of the variable.
     Set the default loop specification first.  */

  for (dim=0; dim < nl->var_rank; dim++)
    {
      nl->ls[dim].step = 1;
      nl->ls[dim].end = nl->dim[dim].ubound;
      nl->ls[dim].start = nl->dim[dim].lbound;
      nl->ls[dim].idx = nl->ls[dim].start;
    }

/* Check to see if there is a qualifier: if so, parse it.*/

  if (c == '(' && nl->var_rank)
    {
      if (nml_parse_qualifier (nl->dim, nl->ls, nl->var_rank) == FAILURE)
	{
	  st_sprintf (nml_err_msg, "%s for namelist variable %s",
		      parse_err_msg, nl->var_name);
	  goto nml_err_ret;
	}
      c = next_char ();
      unget_char (c);
    }

  /* Now parse a derived type component. The root namelist_info address
     is backed up, as is the previous component level.  The  component flag
     is set and the iteration is made by jumping back to get_name.  */

  if (c == '%')
    {

      if (nl->type != GFC_DTYPE_DERIVED)
	{
	  st_sprintf (nml_err_msg, "Attempt to get derived component for %s",
		      nl->var_name);
	  goto nml_err_ret;
	}

      if (!component_flag)
	first_nl = nl;

      root_nl = nl;
      component_flag = 1;
      c = next_char ();
      goto get_name;

    }

  /* Parse a character qualifier, if present.  chigh = 0 is a default
     that signals that the string length = string_length.  */

  clow = 1;
  chigh = 0;

  if (c == '(' && nl->type == GFC_DTYPE_CHARACTER)
    {
      descriptor_dimension chd[1] = {1, clow, nl->string_length};
      nml_loop_spec ind[1] = {1, clow, nl->string_length, 1};

      if (nml_parse_qualifier (chd, ind, 1) == FAILURE)
	{
	  st_sprintf (nml_err_msg, "%s for namelist variable %s",
		      parse_err_msg, nl->var_name);
	  goto nml_err_ret;
	}

      clow = ind[0].start;
      chigh = ind[0].end;

      if (ind[0].step != 1)
	{
	  st_sprintf (nml_err_msg,
		      "Bad step in substring for namelist object %s",
		      nl->var_name);
	  goto nml_err_ret;
	}

      c = next_char ();
      unget_char (c);
    }

  /* If a derived type touch its components and restore the root
     namelist_info if we have parsed a qualified derived type
     component.  */

  if (nl->type == GFC_DTYPE_DERIVED)
    nml_touch_nodes (nl);
  if (component_flag)
    nl = first_nl;

  /*make sure no extraneous qualifiers are there.*/

  if (c == '(')
    {
      st_sprintf (nml_err_msg, "Qualifier for a scalar or non-character"
		  " namelist object %s", nl->var_name);
      goto nml_err_ret;
    }

/* According to the standard, an equal sign MUST follow an object name. The
   following is possibly lax - it allows comments, blank lines and so on to
   intervene.  eat_spaces (); c = next_char (); would be compliant*/

  free_saved ();

  eat_separator ();
  if (input_complete)
    return SUCCESS;

  if (at_eol)
    finish_separator ();
  if (input_complete)
    return SUCCESS;

  c = next_char ();

  if (c != '=')
    {
      st_sprintf (nml_err_msg, "Equal sign must follow namelist object name %s",
		  nl->var_name);
      goto nml_err_ret;
    }

  if (nml_read_obj (nl, 0) == FAILURE)
    goto nml_err_ret;

  return SUCCESS;

nml_err_ret:

  return FAILURE;
}

/* Entry point for namelist input.  Goes through input until namelist name
  is matched.  Then cycles through nml_get_obj_data until the input is
  completed or there is an error.  */

void
namelist_read (void)
{
  char c;

  namelist_mode = 1;
  input_complete = 0;

  if (setjmp (g.eof_jump))
    {
      generate_error (ERROR_END, NULL);
      return;
    }

  /* Look for &namelist_name .  Skip all characters, testing for $nmlname.
     Exit on success or EOF. If '?' or '=?' encountered in stdin, print
     node names or namelist on stdout.  */

find_nml_name:
  switch (c = next_char ())
    {
    case '$':
    case '&':
          break;

    case '=':
      c = next_char ();
      if (c == '?')
	nml_query ('=');
      else
	unget_char (c);
      goto find_nml_name;

    case '?':
      nml_query ('?');

    default:
      goto find_nml_name;
    }

  /* Match the name of the namelist.  */

  nml_match_name (ioparm.namelist_name, ioparm.namelist_name_len);

  if (nml_read_error)
    goto find_nml_name;

  /* Ready to read namelist objects.  If there is an error in input
     from stdin, output the error message and continue.  */

  while (!input_complete)
    {
      if (nml_get_obj_data ()  == FAILURE)
	{
	  if (current_unit->unit_number != options.stdin_unit)
	    goto nml_err_ret;

	  st_printf ("%s\n", nml_err_msg);
	  flush (find_unit (options.stderr_unit)->s);
        }

   }

  return;

  /* All namelist error calls return from here */

nml_err_ret:

  generate_error (ERROR_READ_VALUE , nml_err_msg);
  return;
}
