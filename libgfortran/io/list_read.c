/* Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

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

static int repeat_count, saved_length, saved_used, input_complete, at_eol;
static int comma_flag, namelist_mode;

static char last_char, *saved_string;
static bt saved_type;



/* Storage area for values except for strings.  Must be large enough
   to hold a complex value (two reals) of the largest kind.  */

static char value[20];

#define CASE_DIGITS   case '0': case '1': case '2': case '3': case '4': \
                      case '5': case '6': case '7': case '8': case '9'

#define CASE_SEPARATORS  case ' ': case ',': case '/': case '\n': case '\t'

/* This macro assumes that we're operating on a variable.  */

#define is_separator(c) (c == '/' ||  c == ',' || c == '\n' || c == ' ' \
                         || c == '\t')

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
  at_eol = (c == '\n');
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
      next_record (0);
      at_eol = 1;
      break;

    case '\n':
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
      next_record (0);
      break;

    case '\n':
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
          if (c != '\n')
            unget_char (c);    /* Real number that is just a digit-string.  */
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
	  unget_char (c);
	  eat_separator ();
	  goto done;

	default:
	  goto bad_real;
	}
    }

done:
  push_char ('\0');
  if (convert_real (value, saved_string, length))
    return;

  free_saved ();
  saved_type = BT_REAL;
  return;

bad_real:
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
init_at_eol()
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

static namelist_info *
find_nml_node (char * var_name)
{
   namelist_info * t = ionml;
   while (t != NULL)
     {
       if (strcmp (var_name,t->var_name) == 0)
         {
           t->value_acquired = 1;
           return t;
         }
       t = t->next;
     }
  return NULL;
}

static void
match_namelist_name (char *name, int len)
{
  int name_len;
  char c;
  char * namelist_name = name;

  name_len = 0;
  /* Match the name of the namelist.  */

  if (tolower (next_char ()) != tolower (namelist_name[name_len++]))
    {
    wrong_name:
      generate_error (ERROR_READ_VALUE, "Wrong namelist name found");
      return;
    }

  while (name_len < len)
    {
      c = next_char ();
      if (tolower (c) != tolower (namelist_name[name_len++]))
        goto wrong_name;
    }
}


/********************************************************************
      Namelist reads
********************************************************************/

/* Process a namelist read.  This subroutine initializes things,
   positions to the first element and 
   FIXME: was this comment ever complete?  */

void
namelist_read (void)
{
  char c;
  int name_matched, next_name ;
  namelist_info * nl;
  int len, m;
  void * p;

  namelist_mode = 1;

  if (setjmp (g.eof_jump))
    {
      generate_error (ERROR_END, NULL);
      return;
    }

restart:
  c = next_char ();
  switch (c)
    {
    case ' ':
      goto restart;
    case '!':
      do
        c = next_char ();
      while (c != '\n');

      goto restart;

    case '&':
      break;

    default:
      generate_error (ERROR_READ_VALUE, "Invalid character in namelist");
      return;
    }

  /* Match the name of the namelist.  */
  match_namelist_name(ioparm.namelist_name, ioparm.namelist_name_len);

  /* Ready to read namelist elements.  */
  while (!input_complete)
    {
      c = next_char ();
      switch (c)
        {
        case '/':
          input_complete = 1;
          next_record (0);
          break;
        case '&':
          match_namelist_name("end",3);
          return;
        case '\\':
          return;
        case ' ':
        case '\n':
        case '\t':
          break;
        case ',':
          next_name = 1;
          break;

        case '=':
          name_matched = 1;
          nl = find_nml_node (saved_string);
          if (nl == NULL)
            internal_error ("Can not match a namelist variable");
          free_saved();

          len = nl->len;
          p = nl->mem_pos;

          /* skip any blanks or tabs after the = */
          eat_spaces ();
 
          switch (nl->type)
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
              internal_error ("Bad type for namelist read");
            }

           switch (saved_type)
            {
            case BT_COMPLEX:
              len = 2 * len;
              /* Fall through...  */

            case BT_INTEGER:
            case BT_REAL:
            case BT_LOGICAL:
              memcpy (p, value, len);
              break;

            case BT_CHARACTER:
              m = (len < saved_used) ? len : saved_used;
              memcpy (p, saved_string, m);

              if (m < len)
                memset (((char *) p) + m, ' ', len - m);
              break;

            case BT_NULL:
              break;
            }

          break;

        default :
          push_char(tolower(c));
          break;
        }
   }
}
