/* Copyright (C) 2002, 2003, 2004, 2005, 2007, 2008, 2009
   Free Software Foundation, Inc.
   Contributed by Andy Vaught
   Namelist input contributed by Paul Thomas
   F2003 I/O support contributed by Jerry DeLisle

This file is part of the GNU Fortran 95 runtime library (libgfortran).

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
#include <string.h>
#include <stdlib.h>
#include <ctype.h>


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
   ourselves.  */

#define CASE_DIGITS   case '0': case '1': case '2': case '3': case '4': \
                      case '5': case '6': case '7': case '8': case '9'

#define CASE_SEPARATORS  case ' ': case ',': case '/': case '\n': case '\t': \
                         case '\r': case ';'

/* This macro assumes that we're operating on a variable.  */

#define is_separator(c) (c == '/' ||  c == ',' || c == '\n' || c == ' ' \
                         || c == '\t' || c == '\r' || c == ';')

/* Maximum repeat count.  Less than ten times the maximum signed int32.  */

#define MAX_REPEAT 200000000

#ifndef HAVE_SNPRINTF
# undef snprintf
# define snprintf(str, size, ...) sprintf (str, __VA_ARGS__)
#endif

/* Save a character to a string buffer, enlarging it as necessary.  */

static void
push_char (st_parameter_dt *dtp, char c)
{
  char *new;

  if (dtp->u.p.saved_string == NULL)
    {
      dtp->u.p.saved_string = get_mem (SCRATCH_SIZE);
      // memset below should be commented out.
      memset (dtp->u.p.saved_string, 0, SCRATCH_SIZE);
      dtp->u.p.saved_length = SCRATCH_SIZE;
      dtp->u.p.saved_used = 0;
    }

  if (dtp->u.p.saved_used >= dtp->u.p.saved_length)
    {
      dtp->u.p.saved_length = 2 * dtp->u.p.saved_length;
      new = realloc (dtp->u.p.saved_string, dtp->u.p.saved_length);
      if (new == NULL)
	generate_error (&dtp->common, LIBERROR_OS, NULL);
      dtp->u.p.saved_string = new;
      
      // Also this should not be necessary.
      memset (new + dtp->u.p.saved_used, 0, 
	      dtp->u.p.saved_length - dtp->u.p.saved_used);

    }

  dtp->u.p.saved_string[dtp->u.p.saved_used++] = c;
}


/* Free the input buffer if necessary.  */

static void
free_saved (st_parameter_dt *dtp)
{
  if (dtp->u.p.saved_string == NULL)
    return;

  free_mem (dtp->u.p.saved_string);

  dtp->u.p.saved_string = NULL;
  dtp->u.p.saved_used = 0;
}


/* Free the line buffer if necessary.  */

static void
free_line (st_parameter_dt *dtp)
{
  dtp->u.p.item_count = 0;
  dtp->u.p.line_buffer_enabled = 0;

  if (dtp->u.p.line_buffer == NULL)
    return;

  free_mem (dtp->u.p.line_buffer);
  dtp->u.p.line_buffer = NULL;
}


static char
next_char (st_parameter_dt *dtp)
{
  ssize_t length;
  gfc_offset record;
  char c;
  int cc;

  if (dtp->u.p.last_char != '\0')
    {
      dtp->u.p.at_eol = 0;
      c = dtp->u.p.last_char;
      dtp->u.p.last_char = '\0';
      goto done;
    }

  /* Read from line_buffer if enabled.  */

  if (dtp->u.p.line_buffer_enabled)
    {
      dtp->u.p.at_eol = 0;

      c = dtp->u.p.line_buffer[dtp->u.p.item_count];
      if (c != '\0' && dtp->u.p.item_count < 64)
	{
	  dtp->u.p.line_buffer[dtp->u.p.item_count] = '\0';
	  dtp->u.p.item_count++;
	  goto done;
	}

      dtp->u.p.item_count = 0;
      dtp->u.p.line_buffer_enabled = 0;
    }    

  /* Handle the end-of-record and end-of-file conditions for
     internal array unit.  */
  if (is_array_io (dtp))
    {
      if (dtp->u.p.at_eof)
	longjmp (*dtp->u.p.eof_jump, 1);

      /* Check for "end-of-record" condition.  */
      if (dtp->u.p.current_unit->bytes_left == 0)
	{
	  int finished;

	  c = '\n';
	  record = next_array_record (dtp, dtp->u.p.current_unit->ls,
				      &finished);

	  /* Check for "end-of-file" condition.  */      
	  if (finished)
	    {
	      dtp->u.p.at_eof = 1;
	      goto done;
	    }

	  record *= dtp->u.p.current_unit->recl;
	  if (sseek (dtp->u.p.current_unit->s, record, SEEK_SET) < 0)
	    longjmp (*dtp->u.p.eof_jump, 1);

	  dtp->u.p.current_unit->bytes_left = dtp->u.p.current_unit->recl;
	  goto done;
	}
    }

  /* Get the next character and handle end-of-record conditions.  */

  if (is_internal_unit (dtp))
    {
      length = sread (dtp->u.p.current_unit->s, &c, 1);
      if (length < 0)
	{
	  generate_error (&dtp->common, LIBERROR_OS, NULL);
	  return '\0';
	}
  
      if (is_array_io (dtp))
	{
	  /* Check whether we hit EOF.  */ 
	  if (length == 0)
	    {
	      generate_error (&dtp->common, LIBERROR_INTERNAL_UNIT, NULL);
	      return '\0';
	    } 
	  dtp->u.p.current_unit->bytes_left--;
	}
      else
	{
	  if (dtp->u.p.at_eof) 
	    longjmp (*dtp->u.p.eof_jump, 1);
	  if (length == 0)
	    {
	      c = '\n';
	      dtp->u.p.at_eof = 1;
	    }
	}
    }
  else
    {
      cc = fbuf_getc (dtp->u.p.current_unit);

      if (cc == EOF)
	{
	  if (dtp->u.p.current_unit->endfile == AT_ENDFILE)
	    longjmp (*dtp->u.p.eof_jump, 1);
	  dtp->u.p.current_unit->endfile = AT_ENDFILE;
	  c = '\n';
	}
      else
	c = (char) cc;
      if (is_stream_io (dtp) && cc != EOF)
	dtp->u.p.current_unit->strm_pos++;

    }
done:
  dtp->u.p.at_eol = (c == '\n' || c == '\r');
  return c;
}


/* Push a character back onto the input.  */

static void
unget_char (st_parameter_dt *dtp, char c)
{
  dtp->u.p.last_char = c;
}


/* Skip over spaces in the input.  Returns the nonspace character that
   terminated the eating and also places it back on the input.  */

static char
eat_spaces (st_parameter_dt *dtp)
{
  char c;

  do
    {
      c = next_char (dtp);
    }
  while (c == ' ' || c == '\t');

  unget_char (dtp, c);
  return c;
}


/* This function reads characters through to the end of the current line and
   just ignores them.  */

static void
eat_line (st_parameter_dt *dtp)
{
  char c;
  if (!is_internal_unit (dtp))
    do
      c = next_char (dtp);
    while (c != '\n');
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
eat_separator (st_parameter_dt *dtp)
{
  char c, n;

  eat_spaces (dtp);
  dtp->u.p.comma_flag = 0;

  c = next_char (dtp);
  switch (c)
    {
    case ',':
      if (dtp->u.p.current_unit->decimal_status == DECIMAL_COMMA)
	{
	  unget_char (dtp, c);
	  break;
	}
      /* Fall through.  */
    case ';':
      dtp->u.p.comma_flag = 1;
      eat_spaces (dtp);
      break;

    case '/':
      dtp->u.p.input_complete = 1;
      break;

    case '\r':
      dtp->u.p.at_eol = 1;
      n = next_char(dtp);
      if (n != '\n')
	{
	  unget_char (dtp, n);
	  break;
	}
    /* Fall through.  */
    case '\n':
      dtp->u.p.at_eol = 1;
      if (dtp->u.p.namelist_mode)
	{
	  do
	    {
	      c = next_char (dtp);
	      if (c == '!')
		{
		  eat_line (dtp);
		  c = next_char (dtp);
		  if (c == '!')
		    {
		      eat_line (dtp);
		      c = next_char (dtp);
		    }
		}
	    }
	  while (c == '\n' || c == '\r' || c == ' ' || c == '\t');
	  unget_char (dtp, c);
	}
      break;

    case '!':
      if (dtp->u.p.namelist_mode)
	{			/* Eat a namelist comment.  */
	  do
	    c = next_char (dtp);
	  while (c != '\n');

	  break;
	}

      /* Fall Through...  */

    default:
      unget_char (dtp, c);
      break;
    }
}


/* Finish processing a separator that was interrupted by a newline.
   If we're here, then another data item is present, so we finish what
   we started on the previous line.  */

static void
finish_separator (st_parameter_dt *dtp)
{
  char c;

 restart:
  eat_spaces (dtp);

  c = next_char (dtp);
  switch (c)
    {
    case ',':
      if (dtp->u.p.comma_flag)
	unget_char (dtp, c);
      else
	{
	  c = eat_spaces (dtp);
	  if (c == '\n' || c == '\r')
	    goto restart;
	}

      break;

    case '/':
      dtp->u.p.input_complete = 1;
      if (!dtp->u.p.namelist_mode)
	return;
      break;

    case '\n':
    case '\r':
      goto restart;

    case '!':
      if (dtp->u.p.namelist_mode)
	{
	  do
	    c = next_char (dtp);
	  while (c != '\n');

	  goto restart;
	}

    default:
      unget_char (dtp, c);
      break;
    }
}


/* This function is needed to catch bad conversions so that namelist can
   attempt to see if dtp->u.p.saved_string contains a new object name rather
   than a bad value.  */

static int
nml_bad_return (st_parameter_dt *dtp, char c)
{
  if (dtp->u.p.namelist_mode)
    {
      dtp->u.p.nml_read_error = 1;
      unget_char (dtp, c);
      return 1;
    }
  return 0;
}

/* Convert an unsigned string to an integer.  The length value is -1
   if we are working on a repeat count.  Returns nonzero if we have a
   range problem.  As a side effect, frees the dtp->u.p.saved_string.  */

static int
convert_integer (st_parameter_dt *dtp, int length, int negative)
{
  char c, *buffer, message[100];
  int m;
  GFC_INTEGER_LARGEST v, max, max10;

  buffer = dtp->u.p.saved_string;
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
      set_integer (dtp->u.p.value, v, length);
    }
  else
    {
      dtp->u.p.repeat_count = v;

      if (dtp->u.p.repeat_count == 0)
	{
	  sprintf (message, "Zero repeat count in item %d of list input",
		   dtp->u.p.item_count);

	  generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
	  m = 1;
	}
    }

  free_saved (dtp);
  return m;

 overflow:
  if (length == -1)
    sprintf (message, "Repeat count overflow in item %d of list input",
	     dtp->u.p.item_count);
  else
    sprintf (message, "Integer overflow while reading item %d",
	     dtp->u.p.item_count);

  free_saved (dtp);
  generate_error (&dtp->common, LIBERROR_READ_VALUE, message);

  return 1;
}


/* Parse a repeat count for logical and complex values which cannot
   begin with a digit.  Returns nonzero if we are done, zero if we
   should continue on.  */

static int
parse_repeat (st_parameter_dt *dtp)
{
  char c, message[100];
  int repeat;

  c = next_char (dtp);
  switch (c)
    {
    CASE_DIGITS:
      repeat = c - '0';
      break;

    CASE_SEPARATORS:
      unget_char (dtp, c);
      eat_separator (dtp);
      return 1;

    default:
      unget_char (dtp, c);
      return 0;
    }

  for (;;)
    {
      c = next_char (dtp);
      switch (c)
	{
	CASE_DIGITS:
	  repeat = 10 * repeat + c - '0';

	  if (repeat > MAX_REPEAT)
	    {
	      sprintf (message,
		       "Repeat count overflow in item %d of list input",
		       dtp->u.p.item_count);

	      generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
	      return 1;
	    }

	  break;

	case '*':
	  if (repeat == 0)
	    {
	      sprintf (message,
		       "Zero repeat count in item %d of list input",
		       dtp->u.p.item_count);

	      generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
	      return 1;
	    }

	  goto done;

	default:
	  goto bad_repeat;
	}
    }

 done:
  dtp->u.p.repeat_count = repeat;
  return 0;

 bad_repeat:

  eat_line (dtp);
  free_saved (dtp);
  sprintf (message, "Bad repeat count in item %d of list input",
	   dtp->u.p.item_count);
  generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
  return 1;
}


/* To read a logical we have to look ahead in the input stream to make sure
    there is not an equal sign indicating a variable name.  To do this we use 
    line_buffer to point to a temporary buffer, pushing characters there for
    possible later reading. */

static void
l_push_char (st_parameter_dt *dtp, char c)
{
  if (dtp->u.p.line_buffer == NULL)
    {
      dtp->u.p.line_buffer = get_mem (SCRATCH_SIZE);
      memset (dtp->u.p.line_buffer, 0, SCRATCH_SIZE);
    }

  dtp->u.p.line_buffer[dtp->u.p.item_count++] = c;
}


/* Read a logical character on the input.  */

static void
read_logical (st_parameter_dt *dtp, int length)
{
  char c, message[100];
  int i, v;

  if (parse_repeat (dtp))
    return;

  c = tolower (next_char (dtp));
  l_push_char (dtp, c);
  switch (c)
    {
    case 't':
      v = 1;
      c = next_char (dtp);
      l_push_char (dtp, c);

      if (!is_separator(c))
	goto possible_name;

      unget_char (dtp, c);
      break;
    case 'f':
      v = 0;
      c = next_char (dtp);
      l_push_char (dtp, c);

      if (!is_separator(c))
	goto possible_name;

      unget_char (dtp, c);
      break;

    case '.':
      c = tolower (next_char (dtp));
      switch (c)
	{
	  case 't':
	    v = 1;
	    break;
	  case 'f':
	    v = 0;
	    break;
	  default:
	    goto bad_logical;
	}

      break;

    CASE_SEPARATORS:
      unget_char (dtp, c);
      eat_separator (dtp);
      return;			/* Null value.  */

    default:
      /* Save the character in case it is the beginning
	 of the next object name. */
      unget_char (dtp, c);
      goto bad_logical;
    }

  dtp->u.p.saved_type = BT_LOGICAL;
  dtp->u.p.saved_length = length;

  /* Eat trailing garbage.  */
  do
    {
      c = next_char (dtp);
    }
  while (!is_separator (c));

  unget_char (dtp, c);
  eat_separator (dtp);
  set_integer ((int *) dtp->u.p.value, v, length);
  free_line (dtp);

  return;

 possible_name:

  for(i = 0; i < 63; i++)
    {
      c = next_char (dtp);
      if (is_separator(c))
	{
	  /* All done if this is not a namelist read.  */
	  if (!dtp->u.p.namelist_mode)
	    goto logical_done;

	  unget_char (dtp, c);
	  eat_separator (dtp);
	  c = next_char (dtp);
	  if (c != '=')
	    {
	      unget_char (dtp, c);
	      goto logical_done;
	    }
	}
 
      l_push_char (dtp, c);
      if (c == '=')
	{
	  dtp->u.p.nml_read_error = 1;
	  dtp->u.p.line_buffer_enabled = 1;
	  dtp->u.p.item_count = 0;
	  return;
	}
      
    }

 bad_logical:

  free_line (dtp);

  if (nml_bad_return (dtp, c))
    return;

  eat_line (dtp);
  free_saved (dtp);
  sprintf (message, "Bad logical value while reading item %d",
	      dtp->u.p.item_count);
  generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
  return;

 logical_done:

  dtp->u.p.saved_type = BT_LOGICAL;
  dtp->u.p.saved_length = length;
  set_integer ((int *) dtp->u.p.value, v, length);
  free_saved (dtp);
  free_line (dtp);
}


/* Reading integers is tricky because we can actually be reading a
   repeat count.  We have to store the characters in a buffer because
   we could be reading an integer that is larger than the default int
   used for repeat counts.  */

static void
read_integer (st_parameter_dt *dtp, int length)
{
  char c, message[100];
  int negative;

  negative = 0;

  c = next_char (dtp);
  switch (c)
    {
    case '-':
      negative = 1;
      /* Fall through...  */

    case '+':
      c = next_char (dtp);
      goto get_integer;

    CASE_SEPARATORS:		/* Single null.  */
      unget_char (dtp, c);
      eat_separator (dtp);
      return;

    CASE_DIGITS:
      push_char (dtp, c);
      break;

    default:
      goto bad_integer;
    }

  /* Take care of what may be a repeat count.  */

  for (;;)
    {
      c = next_char (dtp);
      switch (c)
	{
	CASE_DIGITS:
	  push_char (dtp, c);
	  break;

	case '*':
	  push_char (dtp, '\0');
	  goto repeat;

	CASE_SEPARATORS:	/* Not a repeat count.  */
	  goto done;

	default:
	  goto bad_integer;
	}
    }

 repeat:
  if (convert_integer (dtp, -1, 0))
    return;

  /* Get the real integer.  */

  c = next_char (dtp);
  switch (c)
    {
    CASE_DIGITS:
      break;

    CASE_SEPARATORS:
      unget_char (dtp, c);
      eat_separator (dtp);
      return;

    case '-':
      negative = 1;
      /* Fall through...  */

    case '+':
      c = next_char (dtp);
      break;
    }

 get_integer:
  if (!isdigit (c))
    goto bad_integer;
  push_char (dtp, c);

  for (;;)
    {
      c = next_char (dtp);
      switch (c)
	{
	CASE_DIGITS:
	  push_char (dtp, c);
	  break;

	CASE_SEPARATORS:
	  goto done;

	default:
	  goto bad_integer;
	}
    }

 bad_integer:

  if (nml_bad_return (dtp, c))
    return;
  
  eat_line (dtp);
  free_saved (dtp);
  sprintf (message, "Bad integer for item %d in list input",
	      dtp->u.p.item_count);
  generate_error (&dtp->common, LIBERROR_READ_VALUE, message);

  return;

 done:
  unget_char (dtp, c);
  eat_separator (dtp);

  push_char (dtp, '\0');
  if (convert_integer (dtp, length, negative))
    {
       free_saved (dtp);
       return;
    }

  free_saved (dtp);
  dtp->u.p.saved_type = BT_INTEGER;
}


/* Read a character variable.  */

static void
read_character (st_parameter_dt *dtp, int length __attribute__ ((unused)))
{
  char c, quote, message[100];

  quote = ' ';			/* Space means no quote character.  */

  c = next_char (dtp);
  switch (c)
    {
    CASE_DIGITS:
      push_char (dtp, c);
      break;

    CASE_SEPARATORS:
      unget_char (dtp, c);		/* NULL value.  */
      eat_separator (dtp);
      return;

    case '"':
    case '\'':
      quote = c;
      goto get_string;

    default:
      if (dtp->u.p.namelist_mode)
	{
	  unget_char (dtp, c);
	  return;
	}

      push_char (dtp, c);
      goto get_string;
    }

  /* Deal with a possible repeat count.  */

  for (;;)
    {
      c = next_char (dtp);
      switch (c)
	{
	CASE_DIGITS:
	  push_char (dtp, c);
	  break;

	CASE_SEPARATORS:
	  unget_char (dtp, c);
	  goto done;		/* String was only digits!  */

	case '*':
	  push_char (dtp, '\0');
	  goto got_repeat;

	default:
	  push_char (dtp, c);
	  goto get_string;	/* Not a repeat count after all.  */
	}
    }

 got_repeat:
  if (convert_integer (dtp, -1, 0))
    return;

  /* Now get the real string.  */

  c = next_char (dtp);
  switch (c)
    {
    CASE_SEPARATORS:
      unget_char (dtp, c);		/* Repeated NULL values.  */
      eat_separator (dtp);
      return;

    case '"':
    case '\'':
      quote = c;
      break;

    default:
      push_char (dtp, c);
      break;
    }

 get_string:
  for (;;)
    {
      c = next_char (dtp);
      switch (c)
	{
	case '"':
	case '\'':
	  if (c != quote)
	    {
	      push_char (dtp, c);
	      break;
	    }

	  /* See if we have a doubled quote character or the end of
	     the string.  */

	  c = next_char (dtp);
	  if (c == quote)
	    {
	      push_char (dtp, quote);
	      break;
	    }

	  unget_char (dtp, c);
	  goto done;

	CASE_SEPARATORS:
	  if (quote == ' ')
	    {
	      unget_char (dtp, c);
	      goto done;
	    }

	  if (c != '\n' && c != '\r')
	    push_char (dtp, c);
	  break;

	default:
	  push_char (dtp, c);
	  break;
	}
    }

  /* At this point, we have to have a separator, or else the string is
     invalid.  */
 done:
  c = next_char (dtp);
  if (is_separator (c) || c == '!')
    {
      unget_char (dtp, c);
      eat_separator (dtp);
      dtp->u.p.saved_type = BT_CHARACTER;
      free_line (dtp);
    }
  else
    {
      free_saved (dtp);
      sprintf (message, "Invalid string input in item %d",
		  dtp->u.p.item_count);
      generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
    }
}


/* Parse a component of a complex constant or a real number that we
   are sure is already there.  This is a straight real number parser.  */

static int
parse_real (st_parameter_dt *dtp, void *buffer, int length)
{
  char c, message[100];
  int m, seen_dp;

  c = next_char (dtp);
  if (c == '-' || c == '+')
    {
      push_char (dtp, c);
      c = next_char (dtp);
    }

  if (c == ',' && dtp->u.p.current_unit->decimal_status == DECIMAL_COMMA)
    c = '.';
  
  if (!isdigit (c) && c != '.')
    {
      if (c == 'i' || c == 'I' || c == 'n' || c == 'N')
	goto inf_nan;
      else
	goto bad;
    }

  push_char (dtp, c);

  seen_dp = (c == '.') ? 1 : 0;

  for (;;)
    {
      c = next_char (dtp);
      if (c == ',' && dtp->u.p.current_unit->decimal_status == DECIMAL_COMMA)
	c = '.';
      switch (c)
	{
	CASE_DIGITS:
	  push_char (dtp, c);
	  break;

	case '.':
	  if (seen_dp)
	    goto bad;

	  seen_dp = 1;
	  push_char (dtp, c);
	  break;

	case 'e':
	case 'E':
	case 'd':
	case 'D':
	  push_char (dtp, 'e');
	  goto exp1;

	case '-':
	case '+':
	  push_char (dtp, 'e');
	  push_char (dtp, c);
	  c = next_char (dtp);
	  goto exp2;

	CASE_SEPARATORS:
	  unget_char (dtp, c);
	  goto done;

	default:
	  goto done;
	}
    }

 exp1:
  c = next_char (dtp);
  if (c != '-' && c != '+')
    push_char (dtp, '+');
  else
    {
      push_char (dtp, c);
      c = next_char (dtp);
    }

 exp2:
  if (!isdigit (c))
    goto bad;

  push_char (dtp, c);

  for (;;)
    {
      c = next_char (dtp);
      switch (c)
	{
	CASE_DIGITS:
	  push_char (dtp, c);
	  break;

	CASE_SEPARATORS:
	  unget_char (dtp, c);
	  goto done;

	default:
	  goto done;
	}
    }

 done:
  unget_char (dtp, c);
  push_char (dtp, '\0');

  m = convert_real (dtp, buffer, dtp->u.p.saved_string, length);
  free_saved (dtp);

  return m;

 inf_nan:
  /* Match INF and Infinity.  */
  if ((c == 'i' || c == 'I')
      && ((c = next_char (dtp)) == 'n' || c == 'N')
      && ((c = next_char (dtp)) == 'f' || c == 'F'))
    {
	c = next_char (dtp);
	if ((c != 'i' && c != 'I')
	    || ((c == 'i' || c == 'I')
		&& ((c = next_char (dtp)) == 'n' || c == 'N')
		&& ((c = next_char (dtp)) == 'i' || c == 'I')
		&& ((c = next_char (dtp)) == 't' || c == 'T')
		&& ((c = next_char (dtp)) == 'y' || c == 'Y')
		&& (c = next_char (dtp))))
	  {
	     if (is_separator (c))
	       unget_char (dtp, c);
	     push_char (dtp, 'i');
	     push_char (dtp, 'n');
	     push_char (dtp, 'f');
	     goto done;
	  }
    } /* Match NaN.  */
  else if (((c = next_char (dtp)) == 'a' || c == 'A')
	   && ((c = next_char (dtp)) == 'n' || c == 'N')
	   && (c = next_char (dtp)))
    {
      if (is_separator (c))
	unget_char (dtp, c);
      push_char (dtp, 'n');
      push_char (dtp, 'a');
      push_char (dtp, 'n');
      goto done;
    }

 bad:

  if (nml_bad_return (dtp, c))
    return 0;

  eat_line (dtp);
  free_saved (dtp);
  sprintf (message, "Bad floating point number for item %d",
	      dtp->u.p.item_count);
  generate_error (&dtp->common, LIBERROR_READ_VALUE, message);

  return 1;
}


/* Reading a complex number is straightforward because we can tell
   what it is right away.  */

static void
read_complex (st_parameter_dt *dtp, void * dest, int kind, size_t size)
{
  char message[100];
  char c;

  if (parse_repeat (dtp))
    return;

  c = next_char (dtp);
  switch (c)
    {
    case '(':
      break;

    CASE_SEPARATORS:
      unget_char (dtp, c);
      eat_separator (dtp);
      return;

    default:
      goto bad_complex;
    }

  eat_spaces (dtp);
  if (parse_real (dtp, dest, kind))
    return;

eol_1:
  eat_spaces (dtp);
  c = next_char (dtp);
  if (c == '\n' || c== '\r')
    goto eol_1;
  else
    unget_char (dtp, c);

  if (next_char (dtp)
      !=  (dtp->u.p.current_unit->decimal_status == DECIMAL_POINT ? ',' : ';'))
    goto bad_complex;

eol_2:
  eat_spaces (dtp);
  c = next_char (dtp);
  if (c == '\n' || c== '\r')
    goto eol_2;
  else
    unget_char (dtp, c);

  if (parse_real (dtp, dest + size / 2, kind))
    return;

  eat_spaces (dtp);
  if (next_char (dtp) != ')')
    goto bad_complex;

  c = next_char (dtp);
  if (!is_separator (c))
    goto bad_complex;

  unget_char (dtp, c);
  eat_separator (dtp);

  free_saved (dtp);
  dtp->u.p.saved_type = BT_COMPLEX;
  return;

 bad_complex:

  if (nml_bad_return (dtp, c))
    return;

  eat_line (dtp);
  free_saved (dtp);
  sprintf (message, "Bad complex value in item %d of list input",
	      dtp->u.p.item_count);
  generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
}


/* Parse a real number with a possible repeat count.  */

static void
read_real (st_parameter_dt *dtp, void * dest, int length)
{
  char c, message[100];
  int seen_dp;
  int is_inf;

  seen_dp = 0;

  c = next_char (dtp);
  if (c == ',' && dtp->u.p.current_unit->decimal_status == DECIMAL_COMMA)
    c = '.';
  switch (c)
    {
    CASE_DIGITS:
      push_char (dtp, c);
      break;

    case '.':
      push_char (dtp, c);
      seen_dp = 1;
      break;

    case '+':
    case '-':
      goto got_sign;

    CASE_SEPARATORS:
      unget_char (dtp, c);		/* Single null.  */
      eat_separator (dtp);
      return;

    case 'i':
    case 'I':
    case 'n':
    case 'N':
      goto inf_nan;

    default:
      goto bad_real;
    }

  /* Get the digit string that might be a repeat count.  */

  for (;;)
    {
      c = next_char (dtp);
      if (c == ',' && dtp->u.p.current_unit->decimal_status == DECIMAL_COMMA)
	c = '.';
      switch (c)
	{
	CASE_DIGITS:
	  push_char (dtp, c);
	  break;

	case '.':
	  if (seen_dp)
	    goto bad_real;

	  seen_dp = 1;
	  push_char (dtp, c);
	  goto real_loop;

	case 'E':
	case 'e':
	case 'D':
	case 'd':
	  goto exp1;

	case '+':
	case '-':
	  push_char (dtp, 'e');
	  push_char (dtp, c);
	  c = next_char (dtp);
	  goto exp2;

	case '*':
	  push_char (dtp, '\0');
	  goto got_repeat;

	CASE_SEPARATORS:
          if (c != '\n' && c != ',' && c != '\r' && c != ';')
	    unget_char (dtp, c);
	  goto done;

	default:
	  goto bad_real;
	}
    }

 got_repeat:
  if (convert_integer (dtp, -1, 0))
    return;

  /* Now get the number itself.  */

  c = next_char (dtp);
  if (is_separator (c))
    {				/* Repeated null value.  */
      unget_char (dtp, c);
      eat_separator (dtp);
      return;
    }

  if (c != '-' && c != '+')
    push_char (dtp, '+');
  else
    {
    got_sign:
      push_char (dtp, c);
      c = next_char (dtp);
    }

  if (c == ',' && dtp->u.p.current_unit->decimal_status == DECIMAL_COMMA)
    c = '.';

  if (!isdigit (c) && c != '.')
    {
      if (c == 'i' || c == 'I' || c == 'n' || c == 'N')
	goto inf_nan;
      else
	goto bad_real;
    }

  if (c == '.')
    {
      if (seen_dp)
        goto bad_real;
      else
        seen_dp = 1;
    }

  push_char (dtp, c);

 real_loop:
  for (;;)
    {
      c = next_char (dtp);
      if (c == ',' && dtp->u.p.current_unit->decimal_status == DECIMAL_COMMA)
	c = '.';
      switch (c)
	{
	CASE_DIGITS:
	  push_char (dtp, c);
	  break;

	CASE_SEPARATORS:
	  goto done;

	case '.':
	  if (seen_dp)
	    goto bad_real;

	  seen_dp = 1;
	  push_char (dtp, c);
	  break;

	case 'E':
	case 'e':
	case 'D':
	case 'd':
	  goto exp1;

	case '+':
	case '-':
	  push_char (dtp, 'e');
	  push_char (dtp, c);
	  c = next_char (dtp);
	  goto exp2;

	default:
	  goto bad_real;
	}
    }

 exp1:
  push_char (dtp, 'e');

  c = next_char (dtp);
  if (c != '+' && c != '-')
    push_char (dtp, '+');
  else
    {
      push_char (dtp, c);
      c = next_char (dtp);
    }

 exp2:
  if (!isdigit (c))
    goto bad_real;
  push_char (dtp, c);

  for (;;)
    {
      c = next_char (dtp);

      switch (c)
	{
	CASE_DIGITS:
	  push_char (dtp, c);
	  break;

	CASE_SEPARATORS:
	  goto done;

	default:
	  goto bad_real;
	}
    }

 done:
  unget_char (dtp, c);
  eat_separator (dtp);
  push_char (dtp, '\0');
  if (convert_real (dtp, dest, dtp->u.p.saved_string, length))
    return;

  free_saved (dtp);
  dtp->u.p.saved_type = BT_REAL;
  return;

 inf_nan:
  l_push_char (dtp, c);
  is_inf = 0;

  /* Match INF and Infinity.  */
  if (c == 'i' || c == 'I')
    {
      c = next_char (dtp);
      l_push_char (dtp, c);
      if (c != 'n' && c != 'N')
	goto unwind;
      c = next_char (dtp);
      l_push_char (dtp, c);
      if (c != 'f' && c != 'F')
	goto unwind;
      c = next_char (dtp);
      l_push_char (dtp, c);
      if (!is_separator (c))
	{
	  if (c != 'i' && c != 'I')
	    goto unwind;
	  c = next_char (dtp);
	  l_push_char (dtp, c);
	  if (c != 'n' && c != 'N')
	    goto unwind;
	  c = next_char (dtp);
	  l_push_char (dtp, c);
	  if (c != 'i' && c != 'I')
	    goto unwind;
	  c = next_char (dtp);
	  l_push_char (dtp, c);
	  if (c != 't' && c != 'T')
	    goto unwind;
	  c = next_char (dtp);
	  l_push_char (dtp, c);
	  if (c != 'y' && c != 'Y')
	    goto unwind;
	  c = next_char (dtp);
	  l_push_char (dtp, c);
	}
	is_inf = 1;
    } /* Match NaN.  */
  else
    {
      c = next_char (dtp);
      l_push_char (dtp, c);
      if (c != 'a' && c != 'A')
	goto unwind;
      c = next_char (dtp);
      l_push_char (dtp, c);
      if (c != 'n' && c != 'N')
	goto unwind;
      c = next_char (dtp);
      l_push_char (dtp, c);
    }

  if (!is_separator (c))
    goto unwind;

  if (dtp->u.p.namelist_mode)
    {	
      if (c == ' ' || c =='\n' || c == '\r')
	{
	  do
	    c = next_char (dtp);
	  while (c == ' ' || c =='\n' || c == '\r');

	  l_push_char (dtp, c);

	  if (c == '=')
	    goto unwind;
	}
    }

  if (is_inf)
    {
      push_char (dtp, 'i');
      push_char (dtp, 'n');
      push_char (dtp, 'f');
    }
  else
    {
      push_char (dtp, 'n');
      push_char (dtp, 'a');
      push_char (dtp, 'n');
    }

  free_line (dtp);
  goto done;

 unwind:
  if (dtp->u.p.namelist_mode)
    {
      dtp->u.p.nml_read_error = 1;
      dtp->u.p.line_buffer_enabled = 1;
      dtp->u.p.item_count = 0;
      return;
    }

 bad_real:

  if (nml_bad_return (dtp, c))
    return;

  eat_line (dtp);
  free_saved (dtp);
  sprintf (message, "Bad real number in item %d of list input",
	      dtp->u.p.item_count);
  generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
}


/* Check the current type against the saved type to make sure they are
   compatible.  Returns nonzero if incompatible.  */

static int
check_type (st_parameter_dt *dtp, bt type, int len)
{
  char message[100];

  if (dtp->u.p.saved_type != BT_NULL && dtp->u.p.saved_type != type)
    {
      sprintf (message, "Read type %s where %s was expected for item %d",
		  type_name (dtp->u.p.saved_type), type_name (type),
		  dtp->u.p.item_count);

      generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
      return 1;
    }

  if (dtp->u.p.saved_type == BT_NULL || dtp->u.p.saved_type == BT_CHARACTER)
    return 0;

  if (dtp->u.p.saved_length != len)
    {
      sprintf (message,
		  "Read kind %d %s where kind %d is required for item %d",
		  dtp->u.p.saved_length, type_name (dtp->u.p.saved_type), len,
		  dtp->u.p.item_count);
      generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
      return 1;
    }

  return 0;
}


/* Top level data transfer subroutine for list reads.  Because we have
   to deal with repeat counts, the data item is always saved after
   reading, usually in the dtp->u.p.value[] array.  If a repeat count is
   greater than one, we copy the data item multiple times.  */

static void
list_formatted_read_scalar (st_parameter_dt *dtp, volatile bt type, void *p,
			    int kind, size_t size)
{
  char c;
  gfc_char4_t *q;
  int i, m;
  jmp_buf eof_jump;

  dtp->u.p.namelist_mode = 0;

  dtp->u.p.eof_jump = &eof_jump;
  if (setjmp (eof_jump))
    {
      generate_error (&dtp->common, LIBERROR_END, NULL);
    if (!is_internal_unit (dtp))
      {
        dtp->u.p.current_unit->endfile = AFTER_ENDFILE;
        dtp->u.p.current_unit->current_record = 0;
      }
      goto cleanup;
    }

  if (dtp->u.p.first_item)
    {
      dtp->u.p.first_item = 0;
      dtp->u.p.input_complete = 0;
      dtp->u.p.repeat_count = 1;
      dtp->u.p.at_eol = 0;
      
      c = eat_spaces (dtp);
      if (is_separator (c))
	{
	  /* Found a null value.  */
	  eat_separator (dtp);
	  dtp->u.p.repeat_count = 0;

	  /* eat_separator sets this flag if the separator was a comma.  */
	  if (dtp->u.p.comma_flag)
	    goto cleanup;

	  /* eat_separator sets this flag if the separator was a \n or \r.  */
	  if (dtp->u.p.at_eol)
	    finish_separator (dtp);
	  else
	    goto cleanup;
	}

    }
  else
    {
      if (dtp->u.p.repeat_count > 0)
	{
	  if (check_type (dtp, type, kind))
	    return;
	  goto set_value;
	}
	
      if (dtp->u.p.input_complete)
	goto cleanup;

      if (dtp->u.p.input_complete)
	goto cleanup;

      if (dtp->u.p.at_eol)
	finish_separator (dtp);
      else
        {
	  eat_spaces (dtp);
          /* Trailing spaces prior to end of line.  */
	  if (dtp->u.p.at_eol)
	    finish_separator (dtp);
        }

      dtp->u.p.saved_type = BT_NULL;
      dtp->u.p.repeat_count = 1;
    }

  switch (type)
    {
    case BT_INTEGER:
      read_integer (dtp, kind);
      break;
    case BT_LOGICAL:
      read_logical (dtp, kind);
      break;
    case BT_CHARACTER:
      read_character (dtp, kind);
      break;
    case BT_REAL:
      read_real (dtp, p, kind);
      /* Copy value back to temporary if needed.  */
      if (dtp->u.p.repeat_count > 0)
	memcpy (dtp->u.p.value, p, kind);
      break;
    case BT_COMPLEX:
      read_complex (dtp, p, kind, size);
      /* Copy value back to temporary if needed.  */
      if (dtp->u.p.repeat_count > 0)
	memcpy (dtp->u.p.value, p, size);
      break;
    default:
      internal_error (&dtp->common, "Bad type for list read");
    }

  if (dtp->u.p.saved_type != BT_CHARACTER && dtp->u.p.saved_type != BT_NULL)
    dtp->u.p.saved_length = size;

  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    goto cleanup;

 set_value:
  switch (dtp->u.p.saved_type)
    {
    case BT_COMPLEX:
    case BT_REAL:
      if (dtp->u.p.repeat_count > 0)
	memcpy (p, dtp->u.p.value, size);
      break;

    case BT_INTEGER:
    case BT_LOGICAL:
      memcpy (p, dtp->u.p.value, size);
      break;

    case BT_CHARACTER:
      if (dtp->u.p.saved_string)
	{
	  m = ((int) size < dtp->u.p.saved_used)
	      ? (int) size : dtp->u.p.saved_used;
	  if (kind == 1)
	    memcpy (p, dtp->u.p.saved_string, m);
	  else
	    {
	      q = (gfc_char4_t *) p;
	      for (i = 0; i < m; i++)
		q[i] = (unsigned char) dtp->u.p.saved_string[i];
	    }
	}
      else
	/* Just delimiters encountered, nothing to copy but SPACE.  */
        m = 0;

      if (m < (int) size)
	{
	  if (kind == 1)
	    memset (((char *) p) + m, ' ', size - m);
	  else
	    {
	      q = (gfc_char4_t *) p;
	      for (i = m; i < (int) size; i++)
		q[i] = (unsigned char) ' ';
	    }
	}
      break;

    case BT_NULL:
      break;
    }

  if (--dtp->u.p.repeat_count <= 0)
    free_saved (dtp);

cleanup:
  dtp->u.p.eof_jump = NULL;
}


void
list_formatted_read (st_parameter_dt *dtp, bt type, void *p, int kind,
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
      list_formatted_read_scalar (dtp, type, tmp + stride*elem, kind, size);
    }
}


/* Finish a list read.  */

void
finish_list_read (st_parameter_dt *dtp)
{
  char c;

  free_saved (dtp);

  fbuf_flush (dtp->u.p.current_unit, dtp->u.p.mode);

  if (dtp->u.p.at_eol)
    {
      dtp->u.p.at_eol = 0;
      return;
    }

  do
    {
      c = next_char (dtp);
    }
  while (c != '\n');

  if (dtp->u.p.current_unit->endfile != NO_ENDFILE)
    {
      generate_error (&dtp->common, LIBERROR_END, NULL);
      dtp->u.p.current_unit->endfile = AFTER_ENDFILE;
      dtp->u.p.current_unit->current_record = 0;
    }
}

/*			NAMELIST INPUT

void namelist_read (st_parameter_dt *dtp)
calls:
   static void nml_match_name (char *name, int len)
   static int nml_query (st_parameter_dt *dtp)
   static int nml_get_obj_data (st_parameter_dt *dtp,
				namelist_info **prev_nl, char *, size_t)
calls:
      static void nml_untouch_nodes (st_parameter_dt *dtp)
      static namelist_info * find_nml_node (st_parameter_dt *dtp,
					    char * var_name)
      static int nml_parse_qualifier(descriptor_dimension * ad,
				     array_loop_spec * ls, int rank, char *)
      static void nml_touch_nodes (namelist_info * nl)
      static int nml_read_obj (namelist_info *nl, index_type offset,
			       namelist_info **prev_nl, char *, size_t,
			       index_type clow, index_type chigh)
calls:
      -itself-  */

/* Inputs a rank-dimensional qualifier, which can contain
   singlets, doublets, triplets or ':' with the standard meanings.  */

static try
nml_parse_qualifier (st_parameter_dt *dtp, descriptor_dimension *ad,
		     array_loop_spec *ls, int rank, char *parse_err_msg,
		     int *parsed_rank)
{
  int dim;
  int indx;
  int neg;
  int null_flag;
  int is_array_section, is_char;
  char c;

  is_char = 0;
  is_array_section = 0;
  dtp->u.p.expanded_read = 0;

  /* See if this is a character substring qualifier we are looking for.  */
  if (rank == -1)
    {
      rank = 1;
      is_char = 1;
    }

  /* The next character in the stream should be the '('.  */

  c = next_char (dtp);

  /* Process the qualifier, by dimension and triplet.  */

  for (dim=0; dim < rank; dim++ )
    {
      for (indx=0; indx<3; indx++)
	{
	  free_saved (dtp);
	  eat_spaces (dtp);
	  neg = 0;

	  /* Process a potential sign.  */
	  c = next_char (dtp);
	  switch (c)
	    {
	    case '-':
	      neg = 1;
	      break;

	    case '+':
	      break;

	    default:
	      unget_char (dtp, c);
	      break;
	    }

	  /* Process characters up to the next ':' , ',' or ')'.  */
	  for (;;)
	    {
	      c = next_char (dtp);

	      switch (c)
		{
		case ':':
                  is_array_section = 1;
		  break;

		case ',': case ')':
		  if ((c==',' && dim == rank -1)
		      || (c==')' && dim < rank -1))
		    {
		      if (is_char)
		        sprintf (parse_err_msg, "Bad substring qualifier");
		      else
			sprintf (parse_err_msg, "Bad number of index fields");
		      goto err_ret;
		    }
		  break;

		CASE_DIGITS:
		  push_char (dtp, c);
		  continue;

		case ' ': case '\t':
		  eat_spaces (dtp);
		  c = next_char (dtp);
		  break;

		default:
		  if (is_char)
		    sprintf (parse_err_msg,
			     "Bad character in substring qualifier");
		  else
		    sprintf (parse_err_msg, "Bad character in index");
		  goto err_ret;
		}

	      if ((c == ',' || c == ')') && indx == 0
		  && dtp->u.p.saved_string == 0)
		{
		  if (is_char)
		    sprintf (parse_err_msg, "Null substring qualifier");
		  else
		    sprintf (parse_err_msg, "Null index field");
		  goto err_ret;
		}

	      if ((c == ':' && indx == 1 && dtp->u.p.saved_string == 0)
		  || (indx == 2 && dtp->u.p.saved_string == 0))
		{
		  if (is_char)
		    sprintf (parse_err_msg, "Bad substring qualifier");
		  else
		    sprintf (parse_err_msg, "Bad index triplet");
		  goto err_ret;
		}

	      if (is_char && !is_array_section)
		{
		  sprintf (parse_err_msg,
			   "Missing colon in substring qualifier");
		  goto err_ret;
		}

	      /* If '( : ? )' or '( ? : )' break and flag read failure.  */
	      null_flag = 0;
	      if ((c == ':' && indx == 0 && dtp->u.p.saved_string == 0)
		  || (indx==1 && dtp->u.p.saved_string == 0))
		{
		  null_flag = 1;
		  break;
		}

	      /* Now read the index.  */
	      if (convert_integer (dtp, sizeof(ssize_t), neg))
		{
		  if (is_char)
		    sprintf (parse_err_msg, "Bad integer substring qualifier");
		  else
		    sprintf (parse_err_msg, "Bad integer in index");
		  goto err_ret;
		}
	      break;
	    }

	  /* Feed the index values to the triplet arrays.  */
	  if (!null_flag)
	    {
	      if (indx == 0)
		memcpy (&ls[dim].start, dtp->u.p.value, sizeof(ssize_t));
	      if (indx == 1)
		memcpy (&ls[dim].end, dtp->u.p.value, sizeof(ssize_t));
	      if (indx == 2)
		memcpy (&ls[dim].step, dtp->u.p.value, sizeof(ssize_t));
	    }

	  /* Singlet or doublet indices.  */
	  if (c==',' || c==')')
	    {
	      if (indx == 0)
		{
		  memcpy (&ls[dim].start, dtp->u.p.value, sizeof(ssize_t));

		  /*  If -std=f95/2003 or an array section is specified,
		      do not allow excess data to be processed.  */
                  if (is_array_section == 1
		      || compile_options.allow_std < GFC_STD_GNU)
		    ls[dim].end = ls[dim].start;
		  else
		    dtp->u.p.expanded_read = 1;
		}

	      /* Check for non-zero rank.  */
	      if (is_array_section == 1 && ls[dim].start != ls[dim].end)
		*parsed_rank = 1;

	      break;
	    }
	}

      /* Check the values of the triplet indices.  */
      if ((ls[dim].start > (ssize_t)ad[dim].ubound)
	  || (ls[dim].start < (ssize_t)ad[dim].lbound)
	  || (ls[dim].end > (ssize_t)ad[dim].ubound)
	  || (ls[dim].end < (ssize_t)ad[dim].lbound))
	{
	  if (is_char)
	    sprintf (parse_err_msg, "Substring out of range");
	  else
	    sprintf (parse_err_msg, "Index %d out of range", dim + 1);
	  goto err_ret;
	}

      if (((ls[dim].end - ls[dim].start ) * ls[dim].step < 0)
	  || (ls[dim].step == 0))
	{
	  sprintf (parse_err_msg, "Bad range in index %d", dim + 1);
	  goto err_ret;
	}

      /* Initialise the loop index counter.  */
      ls[dim].idx = ls[dim].start;
    }
  eat_spaces (dtp);
  return SUCCESS;

err_ret:

  return FAILURE;
}

static namelist_info *
find_nml_node (st_parameter_dt *dtp, char * var_name)
{
  namelist_info * t = dtp->u.p.ionml;
  while (t != NULL)
    {
      if (strcmp (var_name, t->var_name) == 0)
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
  memcpy (ext_name, nl->var_name, len-1);
  memcpy (ext_name + len - 1, "%", 2);
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
nml_untouch_nodes (st_parameter_dt *dtp)
{
  namelist_info * t;
  for (t = dtp->u.p.ionml; t; t = t->next)
    t->touched = 0;
  return;
}

/* Attempts to input name to namelist name.  Returns
   dtp->u.p.nml_read_error = 1 on no match.  */

static void
nml_match_name (st_parameter_dt *dtp, const char *name, index_type len)
{
  index_type i;
  char c;
  dtp->u.p.nml_read_error = 0;
  for (i = 0; i < len; i++)
    {
      c = next_char (dtp);
      if (tolower (c) != tolower (name[i]))
	{
	  dtp->u.p.nml_read_error = 1;
	  break;
	}
    }
}

/* If the namelist read is from stdin, output the current state of the
   namelist to stdout.  This is used to implement the non-standard query
   features, ? and =?. If c == '=' the full namelist is printed. Otherwise
   the names alone are printed.  */

static void
nml_query (st_parameter_dt *dtp, char c)
{
  gfc_unit * temp_unit;
  namelist_info * nl;
  index_type len;
  char * p;
#ifdef HAVE_CRLF
  static const index_type endlen = 3;
  static const char endl[] = "\r\n";
  static const char nmlend[] = "&end\r\n";
#else
  static const index_type endlen = 2;
  static const char endl[] = "\n";
  static const char nmlend[] = "&end\n";
#endif

  if (dtp->u.p.current_unit->unit_number != options.stdin_unit)
    return;

  /* Store the current unit and transfer to stdout.  */

  temp_unit = dtp->u.p.current_unit;
  dtp->u.p.current_unit = find_unit (options.stdout_unit);

  if (dtp->u.p.current_unit)
    {
      dtp->u.p.mode = WRITING;
      next_record (dtp, 0);

      /* Write the namelist in its entirety.  */

      if (c == '=')
	namelist_write (dtp);

      /* Or write the list of names.  */

      else
	{
	  /* "&namelist_name\n"  */

	  len = dtp->namelist_name_len;
	  p = write_block (dtp, len + endlen);
          if (!p)
            goto query_return;
	  memcpy (p, "&", 1);
	  memcpy ((char*)(p + 1), dtp->namelist_name, len);
	  memcpy ((char*)(p + len + 1), &endl, endlen - 1);
	  for (nl = dtp->u.p.ionml; nl; nl = nl->next)
	    {
	      /* " var_name\n"  */

	      len = strlen (nl->var_name);
              p = write_block (dtp, len + endlen);
	      if (!p)
		goto query_return;
	      memcpy (p, " ", 1);
	      memcpy ((char*)(p + 1), nl->var_name, len);
	      memcpy ((char*)(p + len + 1), &endl, endlen - 1);
	    }

	  /* "&end\n"  */

          p = write_block (dtp, endlen + 3);
	    goto query_return;
          memcpy (p, &nmlend, endlen + 3);
	}

      /* Flush the stream to force immediate output.  */

      fbuf_flush (dtp->u.p.current_unit, WRITING);
      sflush (dtp->u.p.current_unit->s);
      unlock_unit (dtp->u.p.current_unit);
    }

query_return:

  /* Restore the current unit.  */

  dtp->u.p.current_unit = temp_unit;
  dtp->u.p.mode = READING;
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
nml_read_obj (st_parameter_dt *dtp, namelist_info * nl, index_type offset,
	      namelist_info **pprev_nl, char *nml_err_msg,
	      size_t nml_err_msg_size, index_type clow, index_type chigh)
{
  namelist_info * cmp;
  char * obj_name;
  int nml_carry;
  int len;
  int dim;
  index_type dlen;
  index_type m;
  size_t obj_name_len;
  void * pdata;

  /* This object not touched in name parsing.  */

  if (!nl->touched)
    return SUCCESS;

  dtp->u.p.repeat_count = 0;
  eat_spaces (dtp);

  len = nl->len;
  switch (nl->type)
  {
    case GFC_DTYPE_INTEGER:
    case GFC_DTYPE_LOGICAL:
      dlen = len;
      break;

    case GFC_DTYPE_REAL:
      dlen = size_from_real_kind (len);
      break;

    case GFC_DTYPE_COMPLEX:
      dlen = size_from_complex_kind (len);
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
	 dtp->u.p.repeat_count=0  */

      dtp->u.p.nml_read_error = 0;
      nml_carry = 0;
      if (--dtp->u.p.repeat_count <= 0)
	{
	  if (dtp->u.p.input_complete)
	    return SUCCESS;
	  if (dtp->u.p.at_eol)
	    finish_separator (dtp);
	  if (dtp->u.p.input_complete)
	    return SUCCESS;

	  /* GFC_TYPE_UNKNOWN through for nulls and is detected
	     after the switch block.  */

	  dtp->u.p.saved_type = GFC_DTYPE_UNKNOWN;
	  free_saved (dtp);

          switch (nl->type)
	  {
	  case GFC_DTYPE_INTEGER:
	      read_integer (dtp, len);
              break;

	  case GFC_DTYPE_LOGICAL:
	      read_logical (dtp, len);
              break;

	  case GFC_DTYPE_CHARACTER:
	      read_character (dtp, len);
              break;

	  case GFC_DTYPE_REAL:
	    /* Need to copy data back from the real location to the temp in order
	       to handle nml reads into arrays.  */
	    read_real (dtp, pdata, len);
	    memcpy (dtp->u.p.value, pdata, dlen);
	    break;

	  case GFC_DTYPE_COMPLEX:
	    /* Same as for REAL, copy back to temp.  */
	    read_complex (dtp, pdata, len, dlen);
	    memcpy (dtp->u.p.value, pdata, dlen);
	    break;

	  case GFC_DTYPE_DERIVED:
	    obj_name_len = strlen (nl->var_name) + 1;
	    obj_name = get_mem (obj_name_len+1);
	    memcpy (obj_name, nl->var_name, obj_name_len-1);
	    memcpy (obj_name + obj_name_len - 1, "%", 2);

	    /* If reading a derived type, disable the expanded read warning
	       since a single object can have multiple reads.  */
	    dtp->u.p.expanded_read = 0;

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

		if (nml_read_obj (dtp, cmp, (index_type)(pdata - nl->mem_pos),
				  pprev_nl, nml_err_msg, nml_err_msg_size,
				  clow, chigh) == FAILURE)
		  {
		    free_mem (obj_name);
		    return FAILURE;
		  }

		if (dtp->u.p.input_complete)
		  {
		    free_mem (obj_name);
		    return SUCCESS;
		  }
	      }

	    free_mem (obj_name);
	    goto incr_idx;

          default:
	    snprintf (nml_err_msg, nml_err_msg_size,
		      "Bad type for namelist object %s", nl->var_name);
	    internal_error (&dtp->common, nml_err_msg);
	    goto nml_err_ret;
          }
        }

      /* The standard permits array data to stop short of the number of
	 elements specified in the loop specification.  In this case, we
	 should be here with dtp->u.p.nml_read_error != 0.  Control returns to
	 nml_get_obj_data and an attempt is made to read object name.  */

      *pprev_nl = nl;
      if (dtp->u.p.nml_read_error)
	{
	  dtp->u.p.expanded_read = 0;
	  return SUCCESS;
	}

      if (dtp->u.p.saved_type == GFC_DTYPE_UNKNOWN)
	{
	  dtp->u.p.expanded_read = 0;
	  goto incr_idx;
	}

      /* Note the switch from GFC_DTYPE_type to BT_type at this point.
	 This comes about because the read functions return BT_types.  */

      switch (dtp->u.p.saved_type)
      {

	case BT_COMPLEX:
	case BT_REAL:
	case BT_INTEGER:
	case BT_LOGICAL:
	  memcpy (pdata, dtp->u.p.value, dlen);
	  break;

	case BT_CHARACTER:
	  m = (dlen < dtp->u.p.saved_used) ? dlen : dtp->u.p.saved_used;
	  pdata = (void*)( pdata + clow - 1 );
	  memcpy (pdata, dtp->u.p.saved_string, m);
	  if (m < dlen)
	    memset ((void*)( pdata + m ), ' ', dlen - m);
	  break;

	default:
	  break;
      }

      /* Warn if a non-standard expanded read occurs. A single read of a
	 single object is acceptable.  If a second read occurs, issue a warning
	 and set the flag to zero to prevent further warnings.  */
      if (dtp->u.p.expanded_read == 2)
	{
	  notify_std (&dtp->common, GFC_STD_GNU, "Non-standard expanded namelist read.");
	  dtp->u.p.expanded_read = 0;
	}

      /* If the expanded read warning flag is set, increment it,
	 indicating that a single read has occurred.  */
      if (dtp->u.p.expanded_read >= 1)
	dtp->u.p.expanded_read++;

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

  if (dtp->u.p.repeat_count > 1)
    {
      snprintf (nml_err_msg, nml_err_msg_size,
		"Repeat count too large for namelist object %s", nl->var_name);
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
nml_get_obj_data (st_parameter_dt *dtp, namelist_info **pprev_nl,
		  char *nml_err_msg, size_t nml_err_msg_size)
{
  char c;
  namelist_info * nl;
  namelist_info * first_nl = NULL;
  namelist_info * root_nl = NULL;
  int dim, parsed_rank;
  int component_flag;
  index_type clow, chigh;
  int non_zero_rank_count;

  /* Look for end of input or object name.  If '?' or '=?' are encountered
     in stdin, print the node names or the namelist to stdout.  */

  eat_separator (dtp);
  if (dtp->u.p.input_complete)
    return SUCCESS;

  if (dtp->u.p.at_eol)
    finish_separator (dtp);
  if (dtp->u.p.input_complete)
    return SUCCESS;

  c = next_char (dtp);
  switch (c)
    {
    case '=':
      c = next_char (dtp);
      if (c != '?')
	{
	  sprintf (nml_err_msg, "namelist read: misplaced = sign");
	  goto nml_err_ret;
	}
      nml_query (dtp, '=');
      return SUCCESS;

    case '?':
      nml_query (dtp, '?');
      return SUCCESS;

    case '$':
    case '&':
      nml_match_name (dtp, "end", 3);
      if (dtp->u.p.nml_read_error)
	{
	  sprintf (nml_err_msg, "namelist not terminated with / or &end");
	  goto nml_err_ret;
	}
    case '/':
      dtp->u.p.input_complete = 1;
      return SUCCESS;

    default :
      break;
    }

  /* Untouch all nodes of the namelist and reset the flag that is set for
     derived type components.  */

  nml_untouch_nodes (dtp);
  component_flag = 0;
  non_zero_rank_count = 0;

  /* Get the object name - should '!' and '\n' be permitted separators?  */

get_name:

  free_saved (dtp);

  do
    {
      if (!is_separator (c))
	push_char (dtp, tolower(c));
      c = next_char (dtp);
    } while (!( c=='=' || c==' ' || c=='\t' || c =='(' || c =='%' ));

  unget_char (dtp, c);

  /* Check that the name is in the namelist and get pointer to object.
     Three error conditions exist: (i) An attempt is being made to
     identify a non-existent object, following a failed data read or
     (ii) The object name does not exist or (iii) Too many data items
     are present for an object.  (iii) gives the same error message
     as (i)  */

  push_char (dtp, '\0');

  if (component_flag)
    {
      size_t var_len = strlen (root_nl->var_name);
      size_t saved_len
	= dtp->u.p.saved_string ? strlen (dtp->u.p.saved_string) : 0;
      char ext_name[var_len + saved_len + 1];

      memcpy (ext_name, root_nl->var_name, var_len);
      if (dtp->u.p.saved_string)
	memcpy (ext_name + var_len, dtp->u.p.saved_string, saved_len);
      ext_name[var_len + saved_len] = '\0';
      nl = find_nml_node (dtp, ext_name);
    }
  else
    nl = find_nml_node (dtp, dtp->u.p.saved_string);

  if (nl == NULL)
    {
      if (dtp->u.p.nml_read_error && *pprev_nl)
	snprintf (nml_err_msg, nml_err_msg_size,
		  "Bad data for namelist object %s", (*pprev_nl)->var_name);

      else
	snprintf (nml_err_msg, nml_err_msg_size,
		  "Cannot match namelist object name %s",
		  dtp->u.p.saved_string);

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
      parsed_rank = 0;
      if (nml_parse_qualifier (dtp, nl->dim, nl->ls, nl->var_rank,
			       nml_err_msg, &parsed_rank) == FAILURE)
	{
	  char *nml_err_msg_end = strchr (nml_err_msg, '\0');
	  snprintf (nml_err_msg_end,
		    nml_err_msg_size - (nml_err_msg_end - nml_err_msg),
		    " for namelist variable %s", nl->var_name);
	  goto nml_err_ret;
	}

      if (parsed_rank > 0)
	non_zero_rank_count++;

      c = next_char (dtp);
      unget_char (dtp, c);
    }
  else if (nl->var_rank > 0)
    non_zero_rank_count++;

  /* Now parse a derived type component. The root namelist_info address
     is backed up, as is the previous component level.  The  component flag
     is set and the iteration is made by jumping back to get_name.  */

  if (c == '%')
    {
      if (nl->type != GFC_DTYPE_DERIVED)
	{
	  snprintf (nml_err_msg, nml_err_msg_size,
		    "Attempt to get derived component for %s", nl->var_name);
	  goto nml_err_ret;
	}

      if (!component_flag)
	first_nl = nl;

      root_nl = nl;
      component_flag = 1;
      c = next_char (dtp);
      goto get_name;
    }

  /* Parse a character qualifier, if present.  chigh = 0 is a default
     that signals that the string length = string_length.  */

  clow = 1;
  chigh = 0;

  if (c == '(' && nl->type == GFC_DTYPE_CHARACTER)
    {
      descriptor_dimension chd[1] = { {1, clow, nl->string_length} };
      array_loop_spec ind[1] = { {1, clow, nl->string_length, 1} };

      if (nml_parse_qualifier (dtp, chd, ind, -1, nml_err_msg, &parsed_rank)
	  == FAILURE)
	{
	  char *nml_err_msg_end = strchr (nml_err_msg, '\0');
	  snprintf (nml_err_msg_end,
		    nml_err_msg_size - (nml_err_msg_end - nml_err_msg),
		    " for namelist variable %s", nl->var_name);
	  goto nml_err_ret;
	}

      clow = ind[0].start;
      chigh = ind[0].end;

      if (ind[0].step != 1)
	{
	  snprintf (nml_err_msg, nml_err_msg_size,
		    "Step not allowed in substring qualifier"
		    " for namelist object %s", nl->var_name);
	  goto nml_err_ret;
	}

      c = next_char (dtp);
      unget_char (dtp, c);
    }

  /* If a derived type touch its components and restore the root
     namelist_info if we have parsed a qualified derived type
     component.  */

  if (nl->type == GFC_DTYPE_DERIVED)
    nml_touch_nodes (nl);
  if (component_flag && nl->var_rank > 0)
    nl = first_nl;

  /* Make sure no extraneous qualifiers are there.  */

  if (c == '(')
    {
      snprintf (nml_err_msg, nml_err_msg_size,
		"Qualifier for a scalar or non-character namelist object %s",
		nl->var_name);
      goto nml_err_ret;
    }

  /* Make sure there is no more than one non-zero rank object.  */
  if (non_zero_rank_count > 1)
    {
      snprintf (nml_err_msg, nml_err_msg_size,
		"Multiple sub-objects with non-zero rank in namelist object %s",
		nl->var_name);
      non_zero_rank_count = 0;
      goto nml_err_ret;
    }

/* According to the standard, an equal sign MUST follow an object name. The
   following is possibly lax - it allows comments, blank lines and so on to
   intervene.  eat_spaces (dtp); c = next_char (dtp); would be compliant*/

  free_saved (dtp);

  eat_separator (dtp);
  if (dtp->u.p.input_complete)
    return SUCCESS;

  if (dtp->u.p.at_eol)
    finish_separator (dtp);
  if (dtp->u.p.input_complete)
    return SUCCESS;

  c = next_char (dtp);

  if (c != '=')
    {
      snprintf (nml_err_msg, nml_err_msg_size,
		"Equal sign must follow namelist object name %s",
		nl->var_name);
      goto nml_err_ret;
    }

  if (first_nl != NULL && first_nl->var_rank > 0)
    nl = first_nl;
  
  if (nml_read_obj (dtp, nl, 0, pprev_nl, nml_err_msg, nml_err_msg_size,
		    clow, chigh) == FAILURE)
    goto nml_err_ret;

  return SUCCESS;

nml_err_ret:

  return FAILURE;
}

/* Entry point for namelist input.  Goes through input until namelist name
  is matched.  Then cycles through nml_get_obj_data until the input is
  completed or there is an error.  */

void
namelist_read (st_parameter_dt *dtp)
{
  char c;
  jmp_buf eof_jump;
  char nml_err_msg[200];
  /* Pointer to the previously read object, in case attempt is made to read
     new object name.  Should this fail, error message can give previous
     name.  */
  namelist_info *prev_nl = NULL;

  dtp->u.p.namelist_mode = 1;
  dtp->u.p.input_complete = 0;
  dtp->u.p.expanded_read = 0;

  dtp->u.p.eof_jump = &eof_jump;
  if (setjmp (eof_jump))
    {
      dtp->u.p.eof_jump = NULL;
      generate_error (&dtp->common, LIBERROR_END, NULL);
      return;
    }

  /* Look for &namelist_name .  Skip all characters, testing for $nmlname.
     Exit on success or EOF. If '?' or '=?' encountered in stdin, print
     node names or namelist on stdout.  */

find_nml_name:
  switch (c = next_char (dtp))
    {
    case '$':
    case '&':
          break;

    case '!':
      eat_line (dtp);
      goto find_nml_name;

    case '=':
      c = next_char (dtp);
      if (c == '?')
	nml_query (dtp, '=');
      else
	unget_char (dtp, c);
      goto find_nml_name;

    case '?':
      nml_query (dtp, '?');

    default:
      goto find_nml_name;
    }

  /* Match the name of the namelist.  */

  nml_match_name (dtp, dtp->namelist_name, dtp->namelist_name_len);

  if (dtp->u.p.nml_read_error)
    goto find_nml_name;

  /* A trailing space is required, we give a little lattitude here, 10.9.1.  */ 
  c = next_char (dtp);
  if (!is_separator(c) && c != '!')
    {
      unget_char (dtp, c);
      goto find_nml_name;
    }

  unget_char (dtp, c);
  eat_separator (dtp);

  /* Ready to read namelist objects.  If there is an error in input
     from stdin, output the error message and continue.  */

  while (!dtp->u.p.input_complete)
    {
      if (nml_get_obj_data (dtp, &prev_nl, nml_err_msg, sizeof nml_err_msg)
			    == FAILURE)
	{
	  gfc_unit *u;

	  if (dtp->u.p.current_unit->unit_number != options.stdin_unit)
	    goto nml_err_ret;

	  u = find_unit (options.stderr_unit);
	  st_printf ("%s\n", nml_err_msg);
	  if (u != NULL)
	    {
	      sflush (u->s);
	      unlock_unit (u);
	    }
        }

   }

  dtp->u.p.eof_jump = NULL;
  free_saved (dtp);
  free_line (dtp);
  return;

  /* All namelist error calls return from here */

nml_err_ret:

  dtp->u.p.eof_jump = NULL;
  free_saved (dtp);
  free_line (dtp);
  generate_error (&dtp->common, LIBERROR_READ_VALUE, nml_err_msg);
  return;
}
