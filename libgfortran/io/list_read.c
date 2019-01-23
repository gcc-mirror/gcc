/* Copyright (C) 2002-2019 Free Software Foundation, Inc.
   Contributed by Andy Vaught
   Namelist input contributed by Paul Thomas
   F2003 I/O support contributed by Jerry DeLisle

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


#include "io.h"
#include "fbuf.h"
#include "unix.h"
#include <string.h>
#include <ctype.h>

typedef unsigned char uchar;


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

#define CASE_SEPARATORS /* Fall through. */ \
			case ' ': case ',': case '/': case '\n': \
			case '\t': case '\r': case ';'

/* This macro assumes that we're operating on a variable.  */

#define is_separator(c) (c == '/' ||  c == ',' || c == '\n' || c == ' ' \
                         || c == '\t' || c == '\r' || c == ';' || \
			 (dtp->u.p.namelist_mode && c == '!'))

/* Maximum repeat count.  Less than ten times the maximum signed int32.  */

#define MAX_REPEAT 200000000


#define MSGLEN 100


/* Wrappers for calling the current worker functions.  */

#define next_char(dtp) ((dtp)->u.p.current_unit->next_char_fn_ptr (dtp))
#define push_char(dtp, c) ((dtp)->u.p.current_unit->push_char_fn_ptr (dtp, c))

/* Worker function to save a default KIND=1 character to a string
   buffer, enlarging it as necessary.  */

static void
push_char_default (st_parameter_dt *dtp, int c)
{


  if (dtp->u.p.saved_string == NULL)
    {
      /* Plain malloc should suffice here, zeroing not needed?  */
      dtp->u.p.saved_string = xcalloc (SCRATCH_SIZE, 1);
      dtp->u.p.saved_length = SCRATCH_SIZE;
      dtp->u.p.saved_used = 0;
    }

  if (dtp->u.p.saved_used >= dtp->u.p.saved_length)
    {
      dtp->u.p.saved_length = 2 * dtp->u.p.saved_length;
      dtp->u.p.saved_string =
	xrealloc (dtp->u.p.saved_string, dtp->u.p.saved_length);
    }

  dtp->u.p.saved_string[dtp->u.p.saved_used++] = (char) c;
}


/* Worker function to save a KIND=4 character to a string buffer,
   enlarging the buffer as necessary.  */
static void
push_char4 (st_parameter_dt *dtp, int c)
{
  gfc_char4_t *p = (gfc_char4_t *) dtp->u.p.saved_string;

  if (p == NULL)
    {
      dtp->u.p.saved_string = xcalloc (SCRATCH_SIZE, sizeof (gfc_char4_t));
      dtp->u.p.saved_length = SCRATCH_SIZE;
      dtp->u.p.saved_used = 0;
      p = (gfc_char4_t *) dtp->u.p.saved_string;
    }

  if (dtp->u.p.saved_used >= dtp->u.p.saved_length)
    {
      dtp->u.p.saved_length = 2 * dtp->u.p.saved_length;
      dtp->u.p.saved_string =
	xrealloc (dtp->u.p.saved_string,
		  dtp->u.p.saved_length * sizeof (gfc_char4_t));
      p = (gfc_char4_t *) dtp->u.p.saved_string;
    }

  p[dtp->u.p.saved_used++] = c;
}


/* Free the input buffer if necessary.  */

static void
free_saved (st_parameter_dt *dtp)
{
  if (dtp->u.p.saved_string == NULL)
    return;

  free (dtp->u.p.saved_string);

  dtp->u.p.saved_string = NULL;
  dtp->u.p.saved_used = 0;
}


/* Free the line buffer if necessary.  */

static void
free_line (st_parameter_dt *dtp)
{
  dtp->u.p.line_buffer_pos = 0;
  dtp->u.p.line_buffer_enabled = 0;

  if (dtp->u.p.line_buffer == NULL)
    return;

  free (dtp->u.p.line_buffer);
  dtp->u.p.line_buffer = NULL;
}


/* Unget saves the last character so when reading the next character,
   we need to check to see if there is a character waiting.  Similar,
   if the line buffer is being used to read_logical, check it too.  */

static int
check_buffers (st_parameter_dt *dtp)
{
  int c;

  c = '\0';
  if (dtp->u.p.current_unit->last_char != EOF - 1)
    {
      dtp->u.p.at_eol = 0;
      c = dtp->u.p.current_unit->last_char;
      dtp->u.p.current_unit->last_char = EOF - 1;
      goto done;
    }

  /* Read from line_buffer if enabled.  */

  if (dtp->u.p.line_buffer_enabled)
    {
      dtp->u.p.at_eol = 0;

      c = dtp->u.p.line_buffer[dtp->u.p.line_buffer_pos];
      if (c != '\0' && dtp->u.p.line_buffer_pos < 64)
	{
	  dtp->u.p.line_buffer[dtp->u.p.line_buffer_pos] = '\0';
	  dtp->u.p.line_buffer_pos++;
	  goto done;
	}

      dtp->u.p.line_buffer_pos = 0;
      dtp->u.p.line_buffer_enabled = 0;
    }

done:
  dtp->u.p.at_eol = (c == '\n' || c == '\r' || c == EOF);
  return c;
}


/* Worker function for default character encoded file.  */
static int
next_char_default (st_parameter_dt *dtp)
{
  int c;

  /* Always check the unget and line buffer first.  */
  if ((c = check_buffers (dtp)))
    return c;

  c = fbuf_getc (dtp->u.p.current_unit);
  if (c != EOF && is_stream_io (dtp))
    dtp->u.p.current_unit->strm_pos++;

  dtp->u.p.at_eol = (c == '\n' || c == EOF);
  return c;
}


/* Worker function for internal and array I/O units.  */
static int
next_char_internal (st_parameter_dt *dtp)
{
  ssize_t length;
  gfc_offset record;
  int c;

  /* Always check the unget and line buffer first.  */
  if ((c = check_buffers (dtp)))
    return c;

  /* Handle the end-of-record and end-of-file conditions for
     internal array unit.  */
  if (is_array_io (dtp))
    {
      if (dtp->u.p.at_eof)
	return EOF;

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
	    return EOF;

	  dtp->u.p.current_unit->bytes_left = dtp->u.p.current_unit->recl;
	  goto done;
	}
    }

  /* Get the next character and handle end-of-record conditions.  */
  if (likely (dtp->u.p.current_unit->bytes_left > 0))
    {
      if (unlikely (is_char4_unit(dtp))) /* Check for kind=4 internal unit.  */
       length = sread (dtp->u.p.current_unit->s, &c, 1);
      else
       {
	 char cc;
	 length = sread (dtp->u.p.current_unit->s, &cc, 1);
	 c = cc;
       }
    }
  else
    length = 0;

  if (unlikely (length < 0))
    {
      generate_error (&dtp->common, LIBERROR_OS, NULL);
      return '\0';
    }

  if (is_array_io (dtp))
    {
      /* Check whether we hit EOF.  */
      if (unlikely (length == 0))
	{
	  generate_error (&dtp->common, LIBERROR_INTERNAL_UNIT, NULL);
	  return '\0';
	}
    }
  else
    {
      if (dtp->u.p.at_eof)
	return EOF;
      if (length == 0)
	{
	  c = '\n';
	  dtp->u.p.at_eof = 1;
	}
    }
  dtp->u.p.current_unit->bytes_left--;

done:
  dtp->u.p.at_eol = (c == '\n' || c == EOF);
  return c;
}


/* Worker function for UTF encoded files.  */
static int
next_char_utf8 (st_parameter_dt *dtp)
{
  static const uchar masks[6] = { 0x7F, 0x1F, 0x0F, 0x07, 0x02, 0x01 };
  static const uchar patns[6] = { 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };
  int i, nb;
  gfc_char4_t c;

  /* Always check the unget and line buffer first.  */
  if (!(c = check_buffers (dtp)))
    c = fbuf_getc (dtp->u.p.current_unit);

  if (c < 0x80)
    goto utf_done;

  /* The number of leading 1-bits in the first byte indicates how many
     bytes follow.  */
  for (nb = 2; nb < 7; nb++)
    if ((c & ~masks[nb-1]) == patns[nb-1])
      goto found;
  goto invalid;

 found:
  c = (c & masks[nb-1]);

  /* Decode the bytes read.  */
  for (i = 1; i < nb; i++)
    {
      gfc_char4_t n = fbuf_getc (dtp->u.p.current_unit);
      if ((n & 0xC0) != 0x80)
	goto invalid;
      c = ((c << 6) + (n & 0x3F));
    }

  /* Make sure the shortest possible encoding was used.  */
  if (c <=      0x7F && nb > 1) goto invalid;
  if (c <=     0x7FF && nb > 2) goto invalid;
  if (c <=    0xFFFF && nb > 3) goto invalid;
  if (c <=  0x1FFFFF && nb > 4) goto invalid;
  if (c <= 0x3FFFFFF && nb > 5) goto invalid;

  /* Make sure the character is valid.  */
  if (c > 0x7FFFFFFF || (c >= 0xD800 && c <= 0xDFFF))
    goto invalid;

utf_done:
  dtp->u.p.at_eol = (c == '\n' || c == (gfc_char4_t) EOF);
  return (int) c;

 invalid:
  generate_error (&dtp->common, LIBERROR_READ_VALUE, "Invalid UTF-8 encoding");
  return (gfc_char4_t) '?';
}

/* Push a character back onto the input.  */

static void
unget_char (st_parameter_dt *dtp, int c)
{
  dtp->u.p.current_unit->last_char = c;
}


/* Skip over spaces in the input.  Returns the nonspace character that
   terminated the eating and also places it back on the input.  */

static int
eat_spaces (st_parameter_dt *dtp)
{
  int c;

  /* If internal character array IO, peak ahead and seek past spaces.
     This is an optimization unique to character arrays with large
     character lengths (PR38199).  This code eliminates numerous calls
     to next_character.  */
  if (is_array_io (dtp) && (dtp->u.p.current_unit->last_char == EOF - 1))
    {
      gfc_offset offset = stell (dtp->u.p.current_unit->s);
      gfc_offset i;

      if (is_char4_unit(dtp)) /* kind=4 */
	{
	  for (i = 0; i < dtp->u.p.current_unit->bytes_left; i++)
	    {
	      if (dtp->internal_unit[(offset + i) * sizeof (gfc_char4_t)]
		  != (gfc_char4_t)' ')
	        break;
	    }
	}
      else
	{
	  for (i = 0; i < dtp->u.p.current_unit->bytes_left; i++)
	    {
	      if (dtp->internal_unit[offset + i] != ' ')
	        break;
	    }
	}

      if (i != 0)
	{
	  sseek (dtp->u.p.current_unit->s, offset + i, SEEK_SET);
	  dtp->u.p.current_unit->bytes_left -= i;
	}
    }

  /* Now skip spaces, EOF and EOL are handled in next_char.  */
  do
    c = next_char (dtp);
  while (c != EOF && (c == ' ' || c == '\r' || c == '\t'));

  unget_char (dtp, c);
  return c;
}


/* This function reads characters through to the end of the current
   line and just ignores them.  Returns 0 for success and LIBERROR_END
   if it hit EOF.  */

static int
eat_line (st_parameter_dt *dtp)
{
  int c;

  do
    c = next_char (dtp);
  while (c != EOF && c != '\n');
  if (c == EOF)
    return LIBERROR_END;
  return 0;
}


/* Skip over a separator.  Technically, we don't always eat the whole
   separator.  This is because if we've processed the last input item,
   then a separator is unnecessary.  Plus the fact that operating
   systems usually deliver console input on a line basis.

   The upshot is that if we see a newline as part of reading a
   separator, we stop reading.  If there are more input items, we
   continue reading the separator with finish_separator() which takes
   care of the fact that we may or may not have seen a comma as part
   of the separator.

   Returns 0 for success, and non-zero error code otherwise.  */

static int
eat_separator (st_parameter_dt *dtp)
{
  int c, n;
  int err = 0;

  eat_spaces (dtp);
  dtp->u.p.comma_flag = 0;

  if ((c = next_char (dtp)) == EOF)
    return LIBERROR_END;
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
      if ((n = next_char(dtp)) == EOF)
	return LIBERROR_END;
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
	      if ((c = next_char (dtp)) == EOF)
		  return LIBERROR_END;
	      if (c == '!')
		{
		  err = eat_line (dtp);
		  if (err)
		    return err;
		  c = '\n';
		}
	    }
	  while (c == '\n' || c == '\r' || c == ' ' || c == '\t');
	  unget_char (dtp, c);
	}
      break;

    case '!':
      /* Eat a namelist comment.  */
      if (dtp->u.p.namelist_mode)
	{
	  err = eat_line (dtp);
	  if (err)
	    return err;

	  break;
	}

      /* Fall Through...  */

    default:
      unget_char (dtp, c);
      break;
    }
  return err;
}


/* Finish processing a separator that was interrupted by a newline.
   If we're here, then another data item is present, so we finish what
   we started on the previous line.  Return 0 on success, error code
   on failure.  */

static int
finish_separator (st_parameter_dt *dtp)
{
  int c;
  int err = LIBERROR_OK;

 restart:
  eat_spaces (dtp);

  if ((c = next_char (dtp)) == EOF)
    return LIBERROR_END;
  switch (c)
    {
    case ',':
      if (dtp->u.p.comma_flag)
	unget_char (dtp, c);
      else
	{
	  if ((c = eat_spaces (dtp)) == EOF)
	    return LIBERROR_END;
	  if (c == '\n' || c == '\r')
	    goto restart;
	}

      break;

    case '/':
      dtp->u.p.input_complete = 1;
      if (!dtp->u.p.namelist_mode)
	return err;
      break;

    case '\n':
    case '\r':
      goto restart;

    case '!':
      if (dtp->u.p.namelist_mode)
	{
	  err = eat_line (dtp);
	  if (err)
	    return err;
	  goto restart;
	}
      /* Fall through.  */
    default:
      unget_char (dtp, c);
      break;
    }
  return err;
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
  char c, *buffer, message[MSGLEN];
  int m;
  GFC_UINTEGER_LARGEST v, max, max10;
  GFC_INTEGER_LARGEST value;

  buffer = dtp->u.p.saved_string;
  v = 0;

  if (length == -1)
    max = MAX_REPEAT;
  else
    {
      max = si_max (length);
      if (negative)
	max++;
    }
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
	value = -v;
      else
	value = v;
      set_integer (dtp->u.p.value, value, length);
    }
  else
    {
      dtp->u.p.repeat_count = v;

      if (dtp->u.p.repeat_count == 0)
	{
	  snprintf (message, MSGLEN, "Zero repeat count in item %d of list input",
		   dtp->u.p.item_count);

	  generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
	  m = 1;
	}
    }

  free_saved (dtp);
  return m;

 overflow:
  if (length == -1)
    snprintf (message, MSGLEN, "Repeat count overflow in item %d of list input",
	     dtp->u.p.item_count);
  else
    snprintf (message, MSGLEN, "Integer overflow while reading item %d",
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
  char message[MSGLEN];
  int c, repeat;

  if ((c = next_char (dtp)) == EOF)
    goto bad_repeat;
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
	      snprintf (message, MSGLEN,
		       "Repeat count overflow in item %d of list input",
		       dtp->u.p.item_count);

	      generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
	      return 1;
	    }

	  break;

	case '*':
	  if (repeat == 0)
	    {
	      snprintf (message, MSGLEN,
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

  free_saved (dtp);
  if (c == EOF)
    {
      free_line (dtp);
      hit_eof (dtp);
      return 1;
    }
  else
    eat_line (dtp);
  snprintf (message, MSGLEN, "Bad repeat count in item %d of list input",
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
    dtp->u.p.line_buffer = xcalloc (SCRATCH_SIZE, 1);

  dtp->u.p.line_buffer[dtp->u.p.line_buffer_pos++] = c;
}


/* Read a logical character on the input.  */

static void
read_logical (st_parameter_dt *dtp, int length)
{
  char message[MSGLEN];
  int c, i, v;

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

      if (!is_separator(c) && c != EOF)
	goto possible_name;

      unget_char (dtp, c);
      break;
    case 'f':
      v = 0;
      c = next_char (dtp);
      l_push_char (dtp, c);

      if (!is_separator(c) && c != EOF)
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

    case '!':
      if (!dtp->u.p.namelist_mode)
        goto bad_logical;

    CASE_SEPARATORS:
    case EOF:
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
    c = next_char (dtp);
  while (c != EOF && !is_separator (c));

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
	  dtp->u.p.line_buffer_pos = 0;
	  return;
	}

    }

 bad_logical:

  if (nml_bad_return (dtp, c))
    {
      free_line (dtp);
      return;
    }


  free_saved (dtp);
  if (c == EOF)
    {
      free_line (dtp);
      hit_eof (dtp);
      return;
    }
  else if (c != '\n')
    eat_line (dtp);
  snprintf (message, MSGLEN, "Bad logical value while reading item %d",
	      dtp->u.p.item_count);
  free_line (dtp);
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
  char message[MSGLEN];
  int c, negative;

  negative = 0;

  c = next_char (dtp);
  switch (c)
    {
    case '-':
      negative = 1;
      /* Fall through...  */

    case '+':
      if ((c = next_char (dtp)) == EOF)
	goto bad_integer;
      goto get_integer;

    case '!':
      if (!dtp->u.p.namelist_mode)
        goto bad_integer;

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

	case '!':
	  if (!dtp->u.p.namelist_mode)
	    goto bad_integer;

	CASE_SEPARATORS:	/* Not a repeat count.  */
	case EOF:
	  goto done;

	default:
	  goto bad_integer;
	}
    }

 repeat:
  if (convert_integer (dtp, -1, 0))
    return;

  /* Get the real integer.  */

  if ((c = next_char (dtp)) == EOF)
    goto bad_integer;
  switch (c)
    {
    CASE_DIGITS:
      break;

    case '!':
      if (!dtp->u.p.namelist_mode)
        goto bad_integer;

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

	case '!':
	  if (!dtp->u.p.namelist_mode)
	    goto bad_integer;

	CASE_SEPARATORS:
	case EOF:
	  goto done;

	default:
	  goto bad_integer;
	}
    }

 bad_integer:

  if (nml_bad_return (dtp, c))
    return;

  free_saved (dtp);
  if (c == EOF)
    {
      free_line (dtp);
      hit_eof (dtp);
      return;
    }
  else if (c != '\n')
    eat_line (dtp);

  snprintf (message, MSGLEN, "Bad integer for item %d in list input",
	      dtp->u.p.item_count);
  free_line (dtp);
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
  char quote, message[MSGLEN];
  int c;

  quote = ' ';			/* Space means no quote character.  */

  if ((c = next_char (dtp)) == EOF)
    goto eof;
  switch (c)
    {
    CASE_DIGITS:
      push_char (dtp, c);
      break;

    CASE_SEPARATORS:
    case EOF:
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
	case EOF:
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

  if ((c = next_char (dtp)) == EOF)
    goto eof;
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
      if ((c = next_char (dtp)) == EOF)
	goto done_eof;
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

	  if ((c = next_char (dtp)) == EOF)
	    goto done_eof;
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
 done_eof:
  if (is_separator (c) || c == EOF)
    {
      unget_char (dtp, c);
      eat_separator (dtp);
      dtp->u.p.saved_type = BT_CHARACTER;
    }
  else
    {
      free_saved (dtp);
      snprintf (message, MSGLEN, "Invalid string input in item %d",
		  dtp->u.p.item_count);
      generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
    }
  free_line (dtp);
  return;

 eof:
  free_saved (dtp);
  free_line (dtp);
  hit_eof (dtp);
}


/* Parse a component of a complex constant or a real number that we
   are sure is already there.  This is a straight real number parser.  */

static int
parse_real (st_parameter_dt *dtp, void *buffer, int length)
{
  char message[MSGLEN];
  int c, m, seen_dp;

  if ((c = next_char (dtp)) == EOF)
    goto bad;

  if (c == '-' || c == '+')
    {
      push_char (dtp, c);
      if ((c = next_char (dtp)) == EOF)
	goto bad;
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
      if ((c = next_char (dtp)) == EOF)
	goto bad;
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
	case 'q':
	case 'Q':
	  push_char (dtp, 'e');
	  goto exp1;

	case '-':
	case '+':
	  push_char (dtp, 'e');
	  push_char (dtp, c);
	  if ((c = next_char (dtp)) == EOF)
	    goto bad;
	  goto exp2;

	case '!':
	  if (!dtp->u.p.namelist_mode)
	    goto bad;

	CASE_SEPARATORS:
	case EOF:
	  goto done;

	default:
	  goto done;
	}
    }

 exp1:
  if ((c = next_char (dtp)) == EOF)
    goto bad;
  if (c != '-' && c != '+')
    push_char (dtp, '+');
  else
    {
      push_char (dtp, c);
      c = next_char (dtp);
    }

 exp2:
  if (!isdigit (c))
    {
      /* Extension: allow default exponent of 0 when omitted.  */
      if (dtp->common.flags & IOPARM_DT_DEC_EXT)
	{
	  push_char (dtp, '0');
	  goto done;
	}
      else
	goto bad_exponent;
    }

  push_char (dtp, c);

  for (;;)
    {
      if ((c = next_char (dtp)) == EOF)
	goto bad;
      switch (c)
	{
	CASE_DIGITS:
	  push_char (dtp, c);
	  break;

	case '!':
	  if (!dtp->u.p.namelist_mode)
	    goto bad;

	CASE_SEPARATORS:
	case EOF:
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

 done_infnan:
  unget_char (dtp, c);
  push_char (dtp, '\0');

  m = convert_infnan (dtp, buffer, dtp->u.p.saved_string, length);
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
	     if (is_separator (c) || (c == EOF))
	       unget_char (dtp, c);
	     push_char (dtp, 'i');
	     push_char (dtp, 'n');
	     push_char (dtp, 'f');
	     goto done_infnan;
	  }
    } /* Match NaN.  */
  else if (((c = next_char (dtp)) == 'a' || c == 'A')
	   && ((c = next_char (dtp)) == 'n' || c == 'N')
	   && (c = next_char (dtp)))
    {
      if (is_separator (c) || (c == EOF))
	unget_char (dtp, c);
      push_char (dtp, 'n');
      push_char (dtp, 'a');
      push_char (dtp, 'n');

      /* Match "NAN(alphanum)".  */
      if (c == '(')
	{
	  for ( ; c != ')'; c = next_char (dtp))
	    if (is_separator (c))
	      goto bad;

	  c = next_char (dtp);
	  if (is_separator (c) || (c == EOF))
	    unget_char (dtp, c);
	}
      goto done_infnan;
    }

 bad:

  if (nml_bad_return (dtp, c))
    return 0;

 bad_exponent:

  free_saved (dtp);
  if (c == EOF)
    {
      free_line (dtp);
      hit_eof (dtp);
      return 1;
    }
  else if (c != '\n')
    eat_line (dtp);

  snprintf (message, MSGLEN, "Bad complex floating point "
	    "number for item %d", dtp->u.p.item_count);
  free_line (dtp);
  generate_error (&dtp->common, LIBERROR_READ_VALUE, message);

  return 1;
}


/* Reading a complex number is straightforward because we can tell
   what it is right away.  */

static void
read_complex (st_parameter_dt *dtp, void *dest, int kind, size_t size)
{
  char message[MSGLEN];
  int c;

  if (parse_repeat (dtp))
    return;

  c = next_char (dtp);
  switch (c)
    {
    case '(':
      break;

    case '!':
      if (!dtp->u.p.namelist_mode)
	goto bad_complex;

    CASE_SEPARATORS:
    case EOF:
      unget_char (dtp, c);
      eat_separator (dtp);
      return;

    default:
      goto bad_complex;
    }

eol_1:
  eat_spaces (dtp);
  c = next_char (dtp);
  if (c == '\n' || c== '\r')
    goto eol_1;
  else
    unget_char (dtp, c);

  if (parse_real (dtp, dest, kind))
    return;

eol_2:
  eat_spaces (dtp);
  c = next_char (dtp);
  if (c == '\n' || c== '\r')
    goto eol_2;
  else
    unget_char (dtp, c);

  if (next_char (dtp)
      !=  (dtp->u.p.current_unit->decimal_status == DECIMAL_POINT ? ',' : ';'))
    goto bad_complex;

eol_3:
  eat_spaces (dtp);
  c = next_char (dtp);
  if (c == '\n' || c== '\r')
    goto eol_3;
  else
    unget_char (dtp, c);

  if (parse_real (dtp, dest + size / 2, kind))
    return;

eol_4:
  eat_spaces (dtp);
  c = next_char (dtp);
  if (c == '\n' || c== '\r')
    goto eol_4;
  else
    unget_char (dtp, c);

  if (next_char (dtp) != ')')
    goto bad_complex;

  c = next_char (dtp);
  if (!is_separator (c) && (c != EOF))
    goto bad_complex;

  unget_char (dtp, c);
  eat_separator (dtp);

  free_saved (dtp);
  dtp->u.p.saved_type = BT_COMPLEX;
  return;

 bad_complex:

  if (nml_bad_return (dtp, c))
    return;

  free_saved (dtp);
  if (c == EOF)
    {
      free_line (dtp);
      hit_eof (dtp);
      return;
    }
  else if (c != '\n')
    eat_line (dtp);

  snprintf (message, MSGLEN, "Bad complex value in item %d of list input",
	      dtp->u.p.item_count);
  free_line (dtp);
  generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
}


/* Parse a real number with a possible repeat count.  */

static void
read_real (st_parameter_dt *dtp, void *dest, int length)
{
  char message[MSGLEN];
  int c;
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

    case '!':
      if (!dtp->u.p.namelist_mode)
	goto bad_real;

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
	case 'Q':
	case 'q':
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

	case '!':
	  if (!dtp->u.p.namelist_mode)
	    goto bad_real;

	CASE_SEPARATORS:
	case EOF:
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

  if ((c = next_char (dtp)) == EOF)
    goto bad_real;
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
      if ((c = next_char (dtp)) == EOF)
	goto bad_real;
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

	case '!':
	  if (!dtp->u.p.namelist_mode)
	    goto bad_real;

	CASE_SEPARATORS:
	case EOF:
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
	case 'Q':
	case 'q':
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

  if ((c = next_char (dtp)) == EOF)
    goto bad_real;
  if (c != '+' && c != '-')
    push_char (dtp, '+');
  else
    {
      push_char (dtp, c);
      c = next_char (dtp);
    }

 exp2:
  if (!isdigit (c))
    {
      /* Extension: allow default exponent of 0 when omitted.  */
      if (dtp->common.flags & IOPARM_DT_DEC_EXT)
	{
	  push_char (dtp, '0');
	  goto done;
	}
      else
	goto bad_exponent;
    }

  push_char (dtp, c);

  for (;;)
    {
      c = next_char (dtp);

      switch (c)
	{
	CASE_DIGITS:
	  push_char (dtp, c);
	  break;

	case '!':
	  if (!dtp->u.p.namelist_mode)
	    goto bad_real;

	CASE_SEPARATORS:
	case EOF:
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
    {
      free_saved (dtp);
      return;
    }

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
      if (!is_separator (c) && (c != EOF))
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

      /* Match NAN(alphanum).  */
      if (c == '(')
	{
	  for (c = next_char (dtp); c != ')'; c = next_char (dtp))
	    if (is_separator (c))
	      goto unwind;
	    else
	      l_push_char (dtp, c);

	  l_push_char (dtp, ')');
	  c = next_char (dtp);
	  l_push_char (dtp, c);
	}
    }

  if (!is_separator (c) && (c != EOF))
    goto unwind;

  if (dtp->u.p.namelist_mode)
    {
      if (c == ' ' || c =='\n' || c == '\r')
	{
	  do
	    {
	      if ((c = next_char (dtp)) == EOF)
		goto bad_real;
	    }
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
  unget_char (dtp, c);
  eat_separator (dtp);
  push_char (dtp, '\0');
  if (convert_infnan (dtp, dest, dtp->u.p.saved_string, length))
    return;

  free_saved (dtp);
  dtp->u.p.saved_type = BT_REAL;
  return;

 unwind:
  if (dtp->u.p.namelist_mode)
    {
      dtp->u.p.nml_read_error = 1;
      dtp->u.p.line_buffer_enabled = 1;
      dtp->u.p.line_buffer_pos = 0;
      return;
    }

 bad_real:

  if (nml_bad_return (dtp, c))
    return;

 bad_exponent:

  free_saved (dtp);
  if (c == EOF)
    {
      free_line (dtp);
      hit_eof (dtp);
      return;
    }
  else if (c != '\n')
    eat_line (dtp);

  snprintf (message, MSGLEN, "Bad real number in item %d of list input",
	      dtp->u.p.item_count);
  free_line (dtp);
  generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
}


/* Check the current type against the saved type to make sure they are
   compatible.  Returns nonzero if incompatible.  */

static int
check_type (st_parameter_dt *dtp, bt type, int kind)
{
  char message[MSGLEN];

  if (dtp->u.p.saved_type != BT_UNKNOWN && dtp->u.p.saved_type != type)
    {
      snprintf (message, MSGLEN, "Read type %s where %s was expected for item %d",
		  type_name (dtp->u.p.saved_type), type_name (type),
		  dtp->u.p.item_count);
      free_line (dtp);
      generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
      return 1;
    }

  if (dtp->u.p.saved_type == BT_UNKNOWN || dtp->u.p.saved_type == BT_CHARACTER)
    return 0;

  if ((type != BT_COMPLEX && dtp->u.p.saved_length != kind)
      || (type == BT_COMPLEX && dtp->u.p.saved_length != kind*2))
    {
      snprintf (message, MSGLEN,
		  "Read kind %d %s where kind %d is required for item %d",
		  type == BT_COMPLEX ? dtp->u.p.saved_length / 2
				     : dtp->u.p.saved_length,
		  type_name (dtp->u.p.saved_type), kind,
		  dtp->u.p.item_count);
      free_line (dtp);
      generate_error (&dtp->common, LIBERROR_READ_VALUE, message);
      return 1;
    }

  return 0;
}


/* Initialize the function pointers to select the correct versions of
   next_char and push_char depending on what we are doing.  */

static void
set_workers (st_parameter_dt *dtp)
{
  if (dtp->u.p.current_unit->flags.encoding == ENCODING_UTF8)
    {
      dtp->u.p.current_unit->next_char_fn_ptr = &next_char_utf8;
      dtp->u.p.current_unit->push_char_fn_ptr = &push_char4;
    }
  else if (is_internal_unit (dtp))
    {
      dtp->u.p.current_unit->next_char_fn_ptr = &next_char_internal;
      dtp->u.p.current_unit->push_char_fn_ptr = &push_char_default;
    }
  else
    {
      dtp->u.p.current_unit->next_char_fn_ptr = &next_char_default;
      dtp->u.p.current_unit->push_char_fn_ptr = &push_char_default;
    }

}

/* Top level data transfer subroutine for list reads.  Because we have
   to deal with repeat counts, the data item is always saved after
   reading, usually in the dtp->u.p.value[] array.  If a repeat count is
   greater than one, we copy the data item multiple times.  */

static int
list_formatted_read_scalar (st_parameter_dt *dtp, bt type, void *p,
			    int kind, size_t size)
{
  gfc_char4_t *q, *r;
  size_t m;
  int c;
  int err = 0;

  /* Set the next_char and push_char worker functions.  */
  set_workers (dtp);

  if (dtp->u.p.first_item)
    {
      dtp->u.p.first_item = 0;
      dtp->u.p.input_complete = 0;
      dtp->u.p.repeat_count = 1;
      dtp->u.p.at_eol = 0;

      if ((c = eat_spaces (dtp)) == EOF)
	{
	  err = LIBERROR_END;
	  goto cleanup;
	}
      if (is_separator (c))
	{
	  /* Found a null value.  */
	  dtp->u.p.repeat_count = 0;
	  eat_separator (dtp);

	  /* Set end-of-line flag.  */
	  if (c == '\n' || c == '\r')
	    {
	      dtp->u.p.at_eol = 1;
	      if (finish_separator (dtp) == LIBERROR_END)
		{
		  err = LIBERROR_END;
		  goto cleanup;
		}
	    }
	  else
	    goto cleanup;
	}
    }
  else
    {
      if (dtp->u.p.repeat_count > 0)
	{
	  if (check_type (dtp, type, kind))
	    return err;
	  goto set_value;
	}

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

      dtp->u.p.saved_type = BT_UNKNOWN;
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
	memcpy (dtp->u.p.value, p, size);
      break;
    case BT_COMPLEX:
      read_complex (dtp, p, kind, size);
      /* Copy value back to temporary if needed.  */
      if (dtp->u.p.repeat_count > 0)
	memcpy (dtp->u.p.value, p, size);
      break;
    case BT_CLASS:
      {
	  int unit = dtp->u.p.current_unit->unit_number;
	  char iotype[] = "LISTDIRECTED";
          gfc_charlen_type iotype_len = 12;
	  char tmp_iomsg[IOMSG_LEN] = "";
	  char *child_iomsg;
	  gfc_charlen_type child_iomsg_len;
	  int noiostat;
	  int *child_iostat = NULL;
	  gfc_full_array_i4 vlist;

	  GFC_DESCRIPTOR_DATA(&vlist) = NULL;
	  GFC_DIMENSION_SET(vlist.dim[0],1, 0, 0);

	  /* Set iostat, intent(out).  */
	  noiostat = 0;
	  child_iostat = (dtp->common.flags & IOPARM_HAS_IOSTAT) ?
			  dtp->common.iostat : &noiostat;

	  /* Set iomsge, intent(inout).  */
	  if (dtp->common.flags & IOPARM_HAS_IOMSG)
	    {
	      child_iomsg = dtp->common.iomsg;
	      child_iomsg_len = dtp->common.iomsg_len;
	    }
	  else
	    {
	      child_iomsg = tmp_iomsg;
	      child_iomsg_len = IOMSG_LEN;
	    }

	  /* Call the user defined formatted READ procedure.  */
	  dtp->u.p.current_unit->child_dtio++;
	  dtp->u.p.fdtio_ptr (p, &unit, iotype, &vlist,
			      child_iostat, child_iomsg,
			      iotype_len, child_iomsg_len);
	  dtp->u.p.child_saved_iostat = *child_iostat;
	  dtp->u.p.current_unit->child_dtio--;
      }
      break;
    default:
      internal_error (&dtp->common, "Bad type for list read");
    }

  if (dtp->u.p.saved_type != BT_CHARACTER && dtp->u.p.saved_type != BT_UNKNOWN)
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
	  m = (size < (size_t) dtp->u.p.saved_used)
	    ? size : (size_t) dtp->u.p.saved_used;

	  q = (gfc_char4_t *) p;
	  r = (gfc_char4_t *) dtp->u.p.saved_string;
	  if (dtp->u.p.current_unit->flags.encoding == ENCODING_UTF8)
	    for (size_t i = 0; i < m; i++)
	      *q++ = *r++;
	  else
	    {
	      if (kind == 1)
		memcpy (p, dtp->u.p.saved_string, m);
	      else
		for (size_t i = 0; i < m; i++)
		  *q++ = *r++;
	    }
	}
      else
	/* Just delimiters encountered, nothing to copy but SPACE.  */
        m = 0;

      if (m < size)
	{
	  if (kind == 1)
	    memset (((char *) p) + m, ' ', size - m);
	  else
	    {
	      q = (gfc_char4_t *) p;
	      for (size_t i = m; i < size; i++)
		q[i] = (unsigned char) ' ';
	    }
	}
      break;

    case BT_UNKNOWN:
      break;

    default:
      internal_error (&dtp->common, "Bad type for list read");
    }

  if (--dtp->u.p.repeat_count <= 0)
    free_saved (dtp);

cleanup:
  /* err may have been set above from finish_separator, so if it is set
     trigger the hit_eof. The hit_eof will set bits in common.flags.  */
  if (err == LIBERROR_END)
    {
      free_line (dtp);
      hit_eof (dtp);
    }
  /* Now we check common.flags for any errors that could have occurred in
     a READ elsewhere such as in read_integer.  */
  err = dtp->common.flags & IOPARM_LIBRETURN_MASK;
  fbuf_flush_list (dtp->u.p.current_unit, LIST_READING);
  return err;
}


void
list_formatted_read (st_parameter_dt *dtp, bt type, void *p, int kind,
		     size_t size, size_t nelems)
{
  size_t elem;
  char *tmp;
  size_t stride = type == BT_CHARACTER ?
		  size * GFC_SIZE_OF_CHAR_KIND(kind) : size;
  int err;

  tmp = (char *) p;

  /* Big loop over all the elements.  */
  for (elem = 0; elem < nelems; elem++)
    {
      dtp->u.p.item_count++;
      err = list_formatted_read_scalar (dtp, type, tmp + stride*elem,
					kind, size);
      if (err)
	break;
    }
}


/* Finish a list read.  */

void
finish_list_read (st_parameter_dt *dtp)
{
  free_saved (dtp);

  fbuf_flush (dtp->u.p.current_unit, dtp->u.p.mode);

  if (dtp->u.p.at_eol)
    {
      dtp->u.p.at_eol = 0;
      return;
    }

  if (!is_internal_unit (dtp))
    {
      int c;

      /* Set the next_char and push_char worker functions.  */
      set_workers (dtp);

      if (likely (dtp->u.p.child_saved_iostat == LIBERROR_OK))
	{
	  c = next_char (dtp);
	  if (c == EOF)
	    {
	      free_line (dtp);
	      hit_eof (dtp);
	      return;
	    }
	  if (c != '\n')
	    eat_line (dtp);
	}
    }

  free_line (dtp);

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
      static namelist_info *find_nml_node (st_parameter_dt *dtp,
					   char *var_name)
      static int nml_parse_qualifier(descriptor_dimension *ad,
				     array_loop_spec *ls, int rank, char *)
      static void nml_touch_nodes (namelist_info *nl)
      static int nml_read_obj (namelist_info *nl, index_type offset,
			       namelist_info **prev_nl, char *, size_t,
			       index_type clow, index_type chigh)
calls:
      -itself-  */

/* Inputs a rank-dimensional qualifier, which can contain
   singlets, doublets, triplets or ':' with the standard meanings.  */

static bool
nml_parse_qualifier (st_parameter_dt *dtp, descriptor_dimension *ad,
		     array_loop_spec *ls, int rank, bt nml_elem_type,
		     char *parse_err_msg, size_t parse_err_msg_size,
		     int *parsed_rank)
{
  int dim;
  int indx;
  int neg;
  int null_flag;
  int is_array_section, is_char;
  int c;

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

  if ((c = next_char (dtp)) == EOF)
    goto err_ret;

  /* Process the qualifier, by dimension and triplet.  */

  for (dim=0; dim < rank; dim++ )
    {
      for (indx=0; indx<3; indx++)
	{
	  free_saved (dtp);
	  eat_spaces (dtp);
	  neg = 0;

	  /* Process a potential sign.  */
	  if ((c = next_char (dtp)) == EOF)
	    goto err_ret;
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
		case EOF:
		  goto err_ret;

		case ':':
                  is_array_section = 1;
		  break;

		case ',': case ')':
		  if ((c==',' && dim == rank -1)
		      || (c==')' && dim < rank -1))
		    {
		      if (is_char)
		        snprintf (parse_err_msg, parse_err_msg_size,
				  "Bad substring qualifier");
		      else
			snprintf (parse_err_msg, parse_err_msg_size,
				 "Bad number of index fields");
		      goto err_ret;
		    }
		  break;

		CASE_DIGITS:
		  push_char (dtp, c);
		  continue;

		case ' ': case '\t': case '\r': case '\n':
		  eat_spaces (dtp);
		  break;

		default:
		  if (is_char)
		    snprintf (parse_err_msg, parse_err_msg_size,
			     "Bad character in substring qualifier");
		  else
		    snprintf (parse_err_msg, parse_err_msg_size,
			      "Bad character in index");
		  goto err_ret;
		}

	      if ((c == ',' || c == ')') && indx == 0
		  && dtp->u.p.saved_string == 0)
		{
		  if (is_char)
		    snprintf (parse_err_msg, parse_err_msg_size,
			      "Null substring qualifier");
		  else
		    snprintf (parse_err_msg, parse_err_msg_size,
			      "Null index field");
		  goto err_ret;
		}

	      if ((c == ':' && indx == 1 && dtp->u.p.saved_string == 0)
		  || (indx == 2 && dtp->u.p.saved_string == 0))
		{
		  if (is_char)
		    snprintf (parse_err_msg, parse_err_msg_size,
			      "Bad substring qualifier");
		  else
		    snprintf (parse_err_msg, parse_err_msg_size,
			      "Bad index triplet");
		  goto err_ret;
		}

	      if (is_char && !is_array_section)
		{
		  snprintf (parse_err_msg, parse_err_msg_size,
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
	      if (convert_integer (dtp, sizeof(index_type), neg))
		{
		  if (is_char)
		    snprintf (parse_err_msg, parse_err_msg_size,
			      "Bad integer substring qualifier");
		  else
		    snprintf (parse_err_msg, parse_err_msg_size,
			      "Bad integer in index");
		  goto err_ret;
		}
	      break;
	    }

	  /* Feed the index values to the triplet arrays.  */
	  if (!null_flag)
	    {
	      if (indx == 0)
		memcpy (&ls[dim].start, dtp->u.p.value, sizeof(index_type));
	      if (indx == 1)
		memcpy (&ls[dim].end, dtp->u.p.value, sizeof(index_type));
	      if (indx == 2)
		memcpy (&ls[dim].step, dtp->u.p.value, sizeof(index_type));
	    }

	  /* Singlet or doublet indices.  */
	  if (c==',' || c==')')
	    {
	      if (indx == 0)
		{
		  memcpy (&ls[dim].start, dtp->u.p.value, sizeof(index_type));

		  /*  If -std=f95/2003 or an array section is specified,
		      do not allow excess data to be processed.  */
		  if (is_array_section == 1
		      || !(compile_options.allow_std & GFC_STD_GNU)
		      || nml_elem_type == BT_DERIVED)
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

      if (is_array_section == 1 && dtp->u.p.expanded_read == 1)
	{
	  int i;
	  dtp->u.p.expanded_read = 0;
	  for (i = 0; i < dim; i++)
	    ls[i].end = ls[i].start;
	}

      /* Check the values of the triplet indices.  */
      if ((ls[dim].start > GFC_DIMENSION_UBOUND(ad[dim]))
	   || (ls[dim].start < GFC_DIMENSION_LBOUND(ad[dim]))
	   || (ls[dim].end > GFC_DIMENSION_UBOUND(ad[dim]))
	   || (ls[dim].end < GFC_DIMENSION_LBOUND(ad[dim])))
	{
	  if (is_char)
	    snprintf (parse_err_msg, parse_err_msg_size,
		      "Substring out of range");
	  else
	    snprintf (parse_err_msg, parse_err_msg_size,
		      "Index %d out of range", dim + 1);
	  goto err_ret;
	}

      if (((ls[dim].end - ls[dim].start ) * ls[dim].step < 0)
	  || (ls[dim].step == 0))
	{
	  snprintf (parse_err_msg, parse_err_msg_size,
		   "Bad range in index %d", dim + 1);
	  goto err_ret;
	}

      /* Initialise the loop index counter.  */
      ls[dim].idx = ls[dim].start;
    }
  eat_spaces (dtp);
  return true;

err_ret:

  /* The EOF error message is issued by hit_eof. Return true so that the
     caller does not use parse_err_msg and parse_err_msg_size to generate
     an unrelated error message.  */
  if (c == EOF)
    {
      hit_eof (dtp);
      dtp->u.p.input_complete = 1;
      return true;
    }
  return false;
}


static bool
extended_look_ahead (char *p, char *q)
{
  char *r, *s;

  /* Scan ahead to find a '%' in the p string.  */
  for(r = p, s = q; *r && *s; s++)
    if ((*s == '%' || *s == '+') && strcmp (r + 1, s + 1) == 0)
      return true;
  return false;
}


static bool
strcmp_extended_type (char *p, char *q)
{
  char *r, *s;

  for (r = p, s = q; *r && *s; r++, s++)
    {
      if (*r != *s)
	{
	  if (*r == '%' && *s == '+' && extended_look_ahead (r, s))
	    return true;
	  break;
	}
    }
  return false;
}


static namelist_info *
find_nml_node (st_parameter_dt *dtp, char *var_name)
{
  namelist_info *t = dtp->u.p.ionml;
  while (t != NULL)
    {
      if (strcmp (var_name, t->var_name) == 0)
	{
	  t->touched = 1;
	  return t;
	}
      if (strcmp_extended_type (var_name, t->var_name))
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
nml_touch_nodes (namelist_info *nl)
{
  index_type len = strlen (nl->var_name) + 1;
  int dim;
  char *ext_name = xmalloc (len + 1);
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
	      nl->ls[dim].end = GFC_DESCRIPTOR_UBOUND(nl,dim);
	      nl->ls[dim].start = GFC_DESCRIPTOR_LBOUND(nl,dim);
	      nl->ls[dim].idx = nl->ls[dim].start;
	    }
	}
      else
	break;
    }
  free (ext_name);
  return;
}

/* Resets touched for the entire list of nml_nodes, ready for a
   new object.  */

static void
nml_untouch_nodes (st_parameter_dt *dtp)
{
  namelist_info *t;
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
  int c;

  dtp->u.p.nml_read_error = 0;
  for (i = 0; i < len; i++)
    {
      c = next_char (dtp);
      if (c == EOF || (tolower (c) != tolower (name[i])))
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
  gfc_unit *temp_unit;
  namelist_info *nl;
  index_type len;
  char *p;
#ifdef HAVE_CRLF
  static const index_type endlen = 2;
  static const char endl[] = "\r\n";
  static const char nmlend[] = "&end\r\n";
#else
  static const index_type endlen = 1;
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
	  p = write_block (dtp, len - 1 + endlen);
          if (!p)
            goto query_return;
	  memcpy (p, "&", 1);
	  memcpy ((char*)(p + 1), dtp->namelist_name, len);
	  memcpy ((char*)(p + len + 1), &endl, endlen);
	  for (nl = dtp->u.p.ionml; nl; nl = nl->next)
	    {
	      /* " var_name\n"  */

	      len = strlen (nl->var_name);
              p = write_block (dtp, len + endlen);
	      if (!p)
		goto query_return;
	      memcpy (p, " ", 1);
	      memcpy ((char*)(p + 1), nl->var_name, len);
	      memcpy ((char*)(p + len + 1), &endl, endlen);
	    }

	  /* "&end\n"  */

          p = write_block (dtp, endlen + 4);
	  if (!p)
	    goto query_return;
          memcpy (p, &nmlend, endlen + 4);
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

static bool
nml_read_obj (st_parameter_dt *dtp, namelist_info *nl, index_type offset,
	      namelist_info **pprev_nl, char *nml_err_msg,
	      size_t nml_err_msg_size, index_type clow, index_type chigh)
{
  namelist_info *cmp;
  char *obj_name;
  int nml_carry;
  int len;
  int dim;
  index_type dlen;
  index_type m;
  size_t obj_name_len;
  void *pdata;
  gfc_class list_obj;

  /* If we have encountered a previous read error or this object has not been
     touched in name parsing, just return.  */
  if (dtp->u.p.nml_read_error || !nl->touched)
    return true;

  dtp->u.p.item_count++;  /* Used in error messages.  */
  dtp->u.p.repeat_count = 0;
  eat_spaces (dtp);

  len = nl->len;
  switch (nl->type)
  {
    case BT_INTEGER:
    case BT_LOGICAL:
      dlen = len;
      break;

    case BT_REAL:
      dlen = size_from_real_kind (len);
      break;

    case BT_COMPLEX:
      dlen = size_from_complex_kind (len);
      break;

    case BT_CHARACTER:
      dlen = chigh ? (chigh - clow + 1) : nl->string_length;
      break;

    default:
      dlen = 0;
    }

  do
    {
      /* Update the pointer to the data, using the current index vector  */

      if ((nl->type == BT_DERIVED || nl->type == BT_CLASS)
	  && nl->dtio_sub != NULL)
	{
	  pdata = NULL;  /* Not used under these conidtions.  */
	  if (nl->type == BT_CLASS)
	    list_obj.data = ((gfc_class*)nl->mem_pos)->data;
	  else
	    list_obj.data = (void *)nl->mem_pos;

	  for (dim = 0; dim < nl->var_rank; dim++)
	    list_obj.data = list_obj.data + (nl->ls[dim].idx
	      - GFC_DESCRIPTOR_LBOUND(nl,dim))
	      * GFC_DESCRIPTOR_STRIDE(nl,dim) * nl->size;
	}
      else
	{
	  pdata = (void*)(nl->mem_pos + offset);
	  for (dim = 0; dim < nl->var_rank; dim++)
	    pdata = (void*)(pdata + (nl->ls[dim].idx
	      - GFC_DESCRIPTOR_LBOUND(nl,dim))
	      * GFC_DESCRIPTOR_STRIDE(nl,dim) * nl->size);
	}

      /* If we are finished with the repeat count, try to read next value.  */

      nml_carry = 0;
      if (--dtp->u.p.repeat_count <= 0)
	{
	  if (dtp->u.p.input_complete)
	    return true;
	  if (dtp->u.p.at_eol)
	    finish_separator (dtp);
	  if (dtp->u.p.input_complete)
	    return true;

	  dtp->u.p.saved_type = BT_UNKNOWN;
	  free_saved (dtp);

          switch (nl->type)
	  {
	  case BT_INTEGER:
	    read_integer (dtp, len);
            break;

	  case BT_LOGICAL:
	    read_logical (dtp, len);
	    break;

	  case BT_CHARACTER:
	    read_character (dtp, len);
	    break;

	  case BT_REAL:
	    /* Need to copy data back from the real location to the temp in
	       order to handle nml reads into arrays.  */
	    read_real (dtp, pdata, len);
	    memcpy (dtp->u.p.value, pdata, dlen);
	    break;

	  case BT_COMPLEX:
	    /* Same as for REAL, copy back to temp.  */
	    read_complex (dtp, pdata, len, dlen);
	    memcpy (dtp->u.p.value, pdata, dlen);
	    break;

	  case BT_DERIVED:
	  case BT_CLASS:
	    /* If this object has a User Defined procedure, call it.  */
	    if (nl->dtio_sub != NULL)
	      {
		int unit = dtp->u.p.current_unit->unit_number;
		char iotype[] = "NAMELIST";
		gfc_charlen_type iotype_len = 8;
		char tmp_iomsg[IOMSG_LEN] = "";
		char *child_iomsg;
		gfc_charlen_type child_iomsg_len;
		int noiostat;
		int *child_iostat = NULL;
		gfc_full_array_i4 vlist;
		formatted_dtio dtio_ptr = (formatted_dtio)nl->dtio_sub;

		GFC_DESCRIPTOR_DATA(&vlist) = NULL;
		GFC_DIMENSION_SET(vlist.dim[0],1, 0, 0);
		
		list_obj.vptr = nl->vtable;
		list_obj.len = 0;

		/* Set iostat, intent(out).  */
		noiostat = 0;
		child_iostat = (dtp->common.flags & IOPARM_HAS_IOSTAT) ?
				dtp->common.iostat : &noiostat;

		/* Set iomsg, intent(inout).  */
		if (dtp->common.flags & IOPARM_HAS_IOMSG)
		  {
		    child_iomsg = dtp->common.iomsg;
		    child_iomsg_len = dtp->common.iomsg_len;
		  }
		else
		  {
		    child_iomsg = tmp_iomsg;
		    child_iomsg_len = IOMSG_LEN;
		  }

		/* Call the user defined formatted READ procedure.  */
		dtp->u.p.current_unit->child_dtio++;
		dtio_ptr ((void *)&list_obj, &unit, iotype, &vlist,
			  child_iostat, child_iomsg,
			  iotype_len, child_iomsg_len);
		dtp->u.p.child_saved_iostat = *child_iostat;
		dtp->u.p.current_unit->child_dtio--;
		goto incr_idx;
	      }

	    /* Must be default derived type namelist read.  */
	    obj_name_len = strlen (nl->var_name) + 1;
	    obj_name = xmalloc (obj_name_len+1);
	    memcpy (obj_name, nl->var_name, obj_name_len-1);
	    memcpy (obj_name + obj_name_len - 1, "%", 2);

	    /* If reading a derived type, disable the expanded read warning
	       since a single object can have multiple reads.  */
	    dtp->u.p.expanded_read = 0;

	    /* Now loop over the components.  */

	    for (cmp = nl->next;
		 cmp &&
		   !strncmp (cmp->var_name, obj_name, obj_name_len);
		 cmp = cmp->next)
	      {
		/* Jump over nested derived type by testing if the potential
		   component name contains '%'.  */
		if (strchr (cmp->var_name + obj_name_len, '%'))
		    continue;

		if (!nml_read_obj (dtp, cmp, (index_type)(pdata - nl->mem_pos),
				  pprev_nl, nml_err_msg, nml_err_msg_size,
				  clow, chigh))
		  {
		    free (obj_name);
		    return false;
		  }

		if (dtp->u.p.input_complete)
		  {
		    free (obj_name);
		    return true;
		  }
	      }

	    free (obj_name);
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
	  return true;
	}

      if (dtp->u.p.saved_type == BT_UNKNOWN)
	{
	  dtp->u.p.expanded_read = 0;
	  goto incr_idx;
	}

      switch (dtp->u.p.saved_type)
      {

	case BT_COMPLEX:
	case BT_REAL:
	case BT_INTEGER:
	case BT_LOGICAL:
	  memcpy (pdata, dtp->u.p.value, dlen);
	  break;

	case BT_CHARACTER:
	  if (dlen < dtp->u.p.saved_used)
	    {
	      if (compile_options.bounds_check)
		{
		  snprintf (nml_err_msg, nml_err_msg_size,
			    "Namelist object '%s' truncated on read.",
			    nl->var_name);
		  generate_warning (&dtp->common, nml_err_msg);
		}
	      m = dlen;
	    }
	  else
	    m = dtp->u.p.saved_used;

	  if (dtp->u.p.current_unit->flags.encoding == ENCODING_UTF8)
	    {
	      gfc_char4_t *q4, *p4 = pdata;
	      int i;

	      q4 = (gfc_char4_t *) dtp->u.p.saved_string;
	      p4 += clow -1;
	      for (i = 0; i < m; i++)
		*p4++ = *q4++;
	      if (m < dlen)
		for (i = 0; i < dlen - m; i++)
		  *p4++ = (gfc_char4_t) ' ';
	    }
	  else
	    {
	      pdata = (void*)( pdata + clow - 1 );
	      memcpy (pdata, dtp->u.p.saved_string, m);
	      if (m < dlen)
		memset ((void*)( pdata + m ), ' ', dlen - m);
	    }
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
  return true;

nml_err_ret:

  return false;
}

/* Parses the object name, including array and substring qualifiers.  It
   iterates over derived type components, touching those components and
   setting their loop specifications, if there is a qualifier.  If the
   object is itself a derived type, its components and subcomponents are
   touched.  nml_read_obj is called at the end and this reads the data in
   the manner specified by the object name.  */

static bool
nml_get_obj_data (st_parameter_dt *dtp, namelist_info **pprev_nl,
		  char *nml_err_msg, size_t nml_err_msg_size)
{
  int c;
  namelist_info *nl;
  namelist_info *first_nl = NULL;
  namelist_info *root_nl = NULL;
  int dim, parsed_rank;
  int component_flag, qualifier_flag;
  index_type clow, chigh;
  int non_zero_rank_count;

  /* Look for end of input or object name.  If '?' or '=?' are encountered
     in stdin, print the node names or the namelist to stdout.  */

  eat_separator (dtp);
  if (dtp->u.p.input_complete)
    return true;

  if (dtp->u.p.at_eol)
    finish_separator (dtp);
  if (dtp->u.p.input_complete)
    return true;

  if ((c = next_char (dtp)) == EOF)
    goto nml_err_ret;
  switch (c)
    {
    case '=':
      if ((c = next_char (dtp)) == EOF)
	goto nml_err_ret;
      if (c != '?')
	{
	  snprintf (nml_err_msg, nml_err_msg_size,
		    "namelist read: misplaced = sign");
	  goto nml_err_ret;
	}
      nml_query (dtp, '=');
      return true;

    case '?':
      nml_query (dtp, '?');
      return true;

    case '$':
    case '&':
      nml_match_name (dtp, "end", 3);
      if (dtp->u.p.nml_read_error)
	{
	  snprintf (nml_err_msg, nml_err_msg_size,
		    "namelist not terminated with / or &end");
	  goto nml_err_ret;
	}
      /* Fall through.  */
    case '/':
      dtp->u.p.input_complete = 1;
      return true;

    default :
      break;
    }

  /* Untouch all nodes of the namelist and reset the flags that are set for
     derived type components.  */

  nml_untouch_nodes (dtp);
  component_flag = 0;
  qualifier_flag = 0;
  non_zero_rank_count = 0;

  /* Get the object name - should '!' and '\n' be permitted separators?  */

get_name:

  free_saved (dtp);

  do
    {
      if (!is_separator (c))
	push_char_default (dtp, tolower(c));
      if ((c = next_char (dtp)) == EOF)
	goto nml_err_ret;
    }
  while (!( c=='=' || c==' ' || c=='\t' || c =='(' || c =='%' ));

  unget_char (dtp, c);

  /* Check that the name is in the namelist and get pointer to object.
     Three error conditions exist: (i) An attempt is being made to
     identify a non-existent object, following a failed data read or
     (ii) The object name does not exist or (iii) Too many data items
     are present for an object.  (iii) gives the same error message
     as (i)  */

  push_char_default (dtp, '\0');

  if (component_flag)
    {
#define EXT_STACK_SZ 100
      char ext_stack[EXT_STACK_SZ];
      char *ext_name;
      size_t var_len = strlen (root_nl->var_name);
      size_t saved_len
	= dtp->u.p.saved_string ? strlen (dtp->u.p.saved_string) : 0;
      size_t ext_size = var_len + saved_len + 1;

      if (ext_size > EXT_STACK_SZ)
	ext_name = xmalloc (ext_size);
      else
	ext_name = ext_stack;

      memcpy (ext_name, root_nl->var_name, var_len);
      if (dtp->u.p.saved_string)
	memcpy (ext_name + var_len, dtp->u.p.saved_string, saved_len);
      ext_name[var_len + saved_len] = '\0';
      nl = find_nml_node (dtp, ext_name);

      if (ext_size > EXT_STACK_SZ)
	free (ext_name);
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
      nl->ls[dim].end = GFC_DESCRIPTOR_UBOUND(nl,dim);
      nl->ls[dim].start = GFC_DESCRIPTOR_LBOUND(nl,dim);
      nl->ls[dim].idx = nl->ls[dim].start;
    }

/* Check to see if there is a qualifier: if so, parse it.*/

  if (c == '(' && nl->var_rank)
    {
      parsed_rank = 0;
      if (!nml_parse_qualifier (dtp, nl->dim, nl->ls, nl->var_rank,
			       nl->type, nml_err_msg, nml_err_msg_size,
			       &parsed_rank))
	{
	  char *nml_err_msg_end = strchr (nml_err_msg, '\0');
	  snprintf (nml_err_msg_end,
		    nml_err_msg_size - (nml_err_msg_end - nml_err_msg),
		    " for namelist variable %s", nl->var_name);
	  goto nml_err_ret;
	}
      if (parsed_rank > 0)
	non_zero_rank_count++;

      qualifier_flag = 1;

      if ((c = next_char (dtp)) == EOF)
	goto nml_err_ret;
      unget_char (dtp, c);
    }
  else if (nl->var_rank > 0)
    non_zero_rank_count++;

  /* Now parse a derived type component. The root namelist_info address
     is backed up, as is the previous component level.  The  component flag
     is set and the iteration is made by jumping back to get_name.  */

  if (c == '%')
    {
      if (nl->type != BT_DERIVED)
	{
	  snprintf (nml_err_msg, nml_err_msg_size,
		    "Attempt to get derived component for %s", nl->var_name);
	  goto nml_err_ret;
	}

      /* Don't move first_nl further in the list if a qualifier was found.  */
      if ((*pprev_nl == NULL && !qualifier_flag) || !component_flag)
	first_nl = nl;

      root_nl = nl;

      component_flag = 1;
      if ((c = next_char (dtp)) == EOF)
	goto nml_err_ret;
      goto get_name;
    }

  /* Parse a character qualifier, if present.  chigh = 0 is a default
     that signals that the string length = string_length.  */

  clow = 1;
  chigh = 0;

  if (c == '(' && nl->type == BT_CHARACTER)
    {
      descriptor_dimension chd[1] = { {1, clow, nl->string_length} };
      array_loop_spec ind[1] = { {1, clow, nl->string_length, 1} };

      if (!nml_parse_qualifier (dtp, chd, ind, -1, nl->type,
				nml_err_msg, nml_err_msg_size, &parsed_rank))
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

      if ((c = next_char (dtp)) == EOF)
	goto nml_err_ret;
      unget_char (dtp, c);
    }

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
    return true;

  if (dtp->u.p.at_eol)
    finish_separator (dtp);
  if (dtp->u.p.input_complete)
    return true;

  if ((c = next_char (dtp)) == EOF)
    goto nml_err_ret;

  if (c != '=')
    {
      snprintf (nml_err_msg, nml_err_msg_size,
		"Equal sign must follow namelist object name %s",
		nl->var_name);
      goto nml_err_ret;
    }

  /* If a derived type, touch its components and restore the root
     namelist_info if we have parsed a qualified derived type
     component.  */

  if (nl->type == BT_DERIVED && nl->dtio_sub == NULL)
    nml_touch_nodes (nl);

  if (first_nl)
    {
      if (first_nl->var_rank == 0)
	{
	  if (component_flag && qualifier_flag)
	    nl = first_nl;
	}
      else
	nl = first_nl;
    }

  dtp->u.p.nml_read_error = 0;
  if (!nml_read_obj (dtp, nl, 0, pprev_nl, nml_err_msg, nml_err_msg_size,
		    clow, chigh))
    goto nml_err_ret;

  return true;

nml_err_ret:

  /* The EOF error message is issued by hit_eof. Return true so that the
     caller does not use nml_err_msg and nml_err_msg_size to generate
     an unrelated error message.  */
  if (c == EOF)
    {
      dtp->u.p.input_complete = 1;
      unget_char (dtp, c);
      hit_eof (dtp);
      return true;
    }
  return false;
}

/* Entry point for namelist input.  Goes through input until namelist name
  is matched.  Then cycles through nml_get_obj_data until the input is
  completed or there is an error.  */

void
namelist_read (st_parameter_dt *dtp)
{
  int c;
  char nml_err_msg[200];

  /* Initialize the error string buffer just in case we get an unexpected fail
     somewhere and end up at nml_err_ret.  */
  strcpy (nml_err_msg, "Internal namelist read error");

  /* Pointer to the previously read object, in case attempt is made to read
     new object name.  Should this fail, error message can give previous
     name.  */
  namelist_info *prev_nl = NULL;

  dtp->u.p.input_complete = 0;
  dtp->u.p.expanded_read = 0;

  /* Set the next_char and push_char worker functions.  */
  set_workers (dtp);

  /* Look for &namelist_name .  Skip all characters, testing for $nmlname.
     Exit on success or EOF. If '?' or '=?' encountered in stdin, print
     node names or namelist on stdout.  */

find_nml_name:
  c = next_char (dtp);
  switch (c)
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
      goto find_nml_name;

    case EOF:
      return;

    default:
      goto find_nml_name;
    }

  /* Match the name of the namelist.  */

  nml_match_name (dtp, dtp->namelist_name, dtp->namelist_name_len);

  if (dtp->u.p.nml_read_error)
    goto find_nml_name;

  /* A trailing space is required, we give a little latitude here, 10.9.1.  */
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
      if (!nml_get_obj_data (dtp, &prev_nl, nml_err_msg, sizeof nml_err_msg))
	goto nml_err_ret;

      /* Reset the previous namelist pointer if we know we are not going
	 to be doing multiple reads within a single namelist object.  */
      if (prev_nl && prev_nl->var_rank == 0)
	prev_nl = NULL;
    }

  free_saved (dtp);
  free_line (dtp);
  return;


nml_err_ret:

  /* All namelist error calls return from here */
  free_saved (dtp);
  free_line (dtp);
  generate_error (&dtp->common, LIBERROR_READ_VALUE, nml_err_msg);
  return;
}
