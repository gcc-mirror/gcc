/* Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007
   Free Software Foundation, Inc.
   Contributed by Andy Vaught

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
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


/* format.c-- parse a FORMAT string into a binary format suitable for
 * interpretation during I/O statements */

#include "io.h"
#include <ctype.h>
#include <string.h>

#define FARRAY_SIZE 64

typedef struct fnode_array
{
  struct fnode_array *next;
  fnode array[FARRAY_SIZE];
}
fnode_array;

typedef struct format_data
{
  char *format_string, *string;
  const char *error;
  format_token saved_token;
  int value, format_string_len, reversion_ok;
  fnode *avail;
  const fnode *saved_format;
  fnode_array *last;
  fnode_array array;
}
format_data;

static const fnode colon_node = { FMT_COLON, 0, NULL, NULL, {{ 0, 0, 0 }}, 0,
				  NULL };

/* Error messages */

static const char posint_required[] = "Positive width required in format",
  period_required[] = "Period required in format",
  nonneg_required[] = "Nonnegative width required in format",
  unexpected_element[] = "Unexpected element in format",
  unexpected_end[] = "Unexpected end of format string",
  bad_string[] = "Unterminated character constant in format",
  bad_hollerith[] = "Hollerith constant extends past the end of the format",
  reversion_error[] = "Exhausted data descriptors in format";


/* next_char()-- Return the next character in the format string.
 * Returns -1 when the string is done.  If the literal flag is set,
 * spaces are significant, otherwise they are not. */

static int
next_char (format_data *fmt, int literal)
{
  int c;

  do
    {
      if (fmt->format_string_len == 0)
	return -1;

      fmt->format_string_len--;
      c = toupper (*fmt->format_string++);
    }
  while ((c == ' ' || c == '\t') && !literal);

  return c;
}


/* unget_char()-- Back up one character position. */

#define unget_char(fmt) \
  { fmt->format_string--; fmt->format_string_len++; }


/* get_fnode()-- Allocate a new format node, inserting it into the
 * current singly linked list.  These are initially allocated from the
 * static buffer. */

static fnode *
get_fnode (format_data *fmt, fnode **head, fnode **tail, format_token t)
{
  fnode *f;

  if (fmt->avail == &fmt->last->array[FARRAY_SIZE])
    {
      fmt->last->next = get_mem (sizeof (fnode_array));
      fmt->last = fmt->last->next;
      fmt->last->next = NULL;
      fmt->avail = &fmt->last->array[0];
    }
  f = fmt->avail++;
  memset (f, '\0', sizeof (fnode));

  if (*head == NULL)
    *head = *tail = f;
  else
    {
      (*tail)->next = f;
      *tail = f;
    }

  f->format = t;
  f->repeat = -1;
  f->source = fmt->format_string;
  return f;
}


/* free_format_data()-- Free all allocated format data.  */

void
free_format_data (st_parameter_dt *dtp)
{
  fnode_array *fa, *fa_next;
  format_data *fmt = dtp->u.p.fmt;

  if (fmt == NULL)
    return;

  for (fa = fmt->array.next; fa; fa = fa_next)
    {
      fa_next = fa->next;
      free_mem (fa);
    }

  free_mem (fmt);
  dtp->u.p.fmt = NULL;
}


/* format_lex()-- Simple lexical analyzer for getting the next token
 * in a FORMAT string.  We support a one-level token pushback in the
 * fmt->saved_token variable. */

static format_token
format_lex (format_data *fmt)
{
  format_token token;
  int negative_flag;
  int c;
  char delim;

  if (fmt->saved_token != FMT_NONE)
    {
      token = fmt->saved_token;
      fmt->saved_token = FMT_NONE;
      return token;
    }

  negative_flag = 0;
  c = next_char (fmt, 0);

  switch (c)
    {
    case '-':
      negative_flag = 1;
      /* Fall Through */

    case '+':
      c = next_char (fmt, 0);
      if (!isdigit (c))
	{
	  token = FMT_UNKNOWN;
	  break;
	}

      fmt->value = c - '0';

      for (;;)
	{
	  c = next_char (fmt, 0);
	  if (!isdigit (c))
	    break;

	  fmt->value = 10 * fmt->value + c - '0';
	}

      unget_char (fmt);

      if (negative_flag)
	fmt->value = -fmt->value;
      token = FMT_SIGNED_INT;
      break;

    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      fmt->value = c - '0';

      for (;;)
	{
	  c = next_char (fmt, 0);
	  if (!isdigit (c))
	    break;

	  fmt->value = 10 * fmt->value + c - '0';
	}

      unget_char (fmt);
      token = (fmt->value == 0) ? FMT_ZERO : FMT_POSINT;
      break;

    case '.':
      token = FMT_PERIOD;
      break;

    case ',':
      token = FMT_COMMA;
      break;

    case ':':
      token = FMT_COLON;
      break;

    case '/':
      token = FMT_SLASH;
      break;

    case '$':
      token = FMT_DOLLAR;
      break;

    case 'T':
      switch (next_char (fmt, 0))
	{
	case 'L':
	  token = FMT_TL;
	  break;
	case 'R':
	  token = FMT_TR;
	  break;
	default:
	  token = FMT_T;
	  unget_char (fmt);
	  break;
	}

      break;

    case '(':
      token = FMT_LPAREN;
      break;

    case ')':
      token = FMT_RPAREN;
      break;

    case 'X':
      token = FMT_X;
      break;

    case 'S':
      switch (next_char (fmt, 0))
	{
	case 'S':
	  token = FMT_SS;
	  break;
	case 'P':
	  token = FMT_SP;
	  break;
	default:
	  token = FMT_S;
	  unget_char (fmt);
	  break;
	}

      break;

    case 'B':
      switch (next_char (fmt, 0))
	{
	case 'N':
	  token = FMT_BN;
	  break;
	case 'Z':
	  token = FMT_BZ;
	  break;
	default:
	  token = FMT_B;
	  unget_char (fmt);
	  break;
	}

      break;

    case '\'':
    case '"':
      delim = c;

      fmt->string = fmt->format_string;
      fmt->value = 0;		/* This is the length of the string */

      for (;;)
	{
	  c = next_char (fmt, 1);
	  if (c == -1)
	    {
	      token = FMT_BADSTRING;
	      fmt->error = bad_string;
	      break;
	    }

	  if (c == delim)
	    {
	      c = next_char (fmt, 1);

	      if (c == -1)
		{
		  token = FMT_BADSTRING;
		  fmt->error = bad_string;
		  break;
		}

	      if (c != delim)
		{
		  unget_char (fmt);
		  token = FMT_STRING;
		  break;
		}
	    }

	  fmt->value++;
	}

      break;

    case 'P':
      token = FMT_P;
      break;

    case 'I':
      token = FMT_I;
      break;

    case 'O':
      token = FMT_O;
      break;

    case 'Z':
      token = FMT_Z;
      break;

    case 'F':
      token = FMT_F;
      break;

    case 'E':
      switch (next_char (fmt, 0))
	{
	case 'N':
	  token = FMT_EN;
	  break;
	case 'S':
	  token = FMT_ES;
	  break;
	default:
	  token = FMT_E;
	  unget_char (fmt);
	  break;
	}

      break;

    case 'G':
      token = FMT_G;
      break;

    case 'H':
      token = FMT_H;
      break;

    case 'L':
      token = FMT_L;
      break;

    case 'A':
      token = FMT_A;
      break;

    case 'D':
      token = FMT_D;
      break;

    case -1:
      token = FMT_END;
      break;

    default:
      token = FMT_UNKNOWN;
      break;
    }

  return token;
}


/* parse_format_list()-- Parse a format list.  Assumes that a left
 * paren has already been seen.  Returns a list representing the
 * parenthesis node which contains the rest of the list. */

static fnode *
parse_format_list (st_parameter_dt *dtp)
{
  fnode *head, *tail;
  format_token t, u, t2;
  int repeat;
  format_data *fmt = dtp->u.p.fmt;

  head = tail = NULL;

  /* Get the next format item */
 format_item:
  t = format_lex (fmt);
 format_item_1:
  switch (t)
    {
    case FMT_POSINT:
      repeat = fmt->value;

      t = format_lex (fmt);
      switch (t)
	{
	case FMT_LPAREN:
	  get_fnode (fmt, &head, &tail, FMT_LPAREN);
	  tail->repeat = repeat;
	  tail->u.child = parse_format_list (dtp);
	  if (fmt->error != NULL)
	    goto finished;

	  goto between_desc;

	case FMT_SLASH:
	  get_fnode (fmt, &head, &tail, FMT_SLASH);
	  tail->repeat = repeat;
	  goto optional_comma;

	case FMT_X:
	  get_fnode (fmt, &head, &tail, FMT_X);
	  tail->repeat = 1;
	  tail->u.k = fmt->value;
	  goto between_desc;

	case FMT_P:
	  goto p_descriptor;

	default:
	  goto data_desc;
	}

    case FMT_LPAREN:
      get_fnode (fmt, &head, &tail, FMT_LPAREN);
      tail->repeat = 1;
      tail->u.child = parse_format_list (dtp);
      if (fmt->error != NULL)
	goto finished;

      goto between_desc;

    case FMT_SIGNED_INT:	/* Signed integer can only precede a P format.  */
    case FMT_ZERO:		/* Same for zero.  */
      t = format_lex (fmt);
      if (t != FMT_P)
	{
	  fmt->error = "Expected P edit descriptor in format";
	  goto finished;
	}

    p_descriptor:
      get_fnode (fmt, &head, &tail, FMT_P);
      tail->u.k = fmt->value;
      tail->repeat = 1;

      t = format_lex (fmt);
      if (t == FMT_F || t == FMT_EN || t == FMT_ES || t == FMT_D
	  || t == FMT_G || t == FMT_E)
	{
	  repeat = 1;
	  goto data_desc;
	}

      fmt->saved_token = t;
      goto optional_comma;

    case FMT_P:		/* P and X require a prior number */
      fmt->error = "P descriptor requires leading scale factor";
      goto finished;

    case FMT_X:
/*
   EXTENSION!

   If we would be pedantic in the library, we would have to reject
   an X descriptor without an integer prefix:

      fmt->error = "X descriptor requires leading space count";
      goto finished;

   However, this is an extension supported by many Fortran compilers,
   including Cray, HP, AIX, and IRIX.  Therefore, we allow it in the
   runtime library, and make the front end reject it if the compiler
   is in pedantic mode.  The interpretation of 'X' is '1X'.
*/
      get_fnode (fmt, &head, &tail, FMT_X);
      tail->repeat = 1;
      tail->u.k = 1;
      goto between_desc;

    case FMT_STRING:
      get_fnode (fmt, &head, &tail, FMT_STRING);

      tail->u.string.p = fmt->string;
      tail->u.string.length = fmt->value;
      tail->repeat = 1;
      goto optional_comma;

    case FMT_S:
    case FMT_SS:
    case FMT_SP:
    case FMT_BN:
    case FMT_BZ:
      get_fnode (fmt, &head, &tail, t);
      tail->repeat = 1;
      goto between_desc;

    case FMT_COLON:
      get_fnode (fmt, &head, &tail, FMT_COLON);
      tail->repeat = 1;
      goto optional_comma;

    case FMT_SLASH:
      get_fnode (fmt, &head, &tail, FMT_SLASH);
      tail->repeat = 1;
      tail->u.r = 1;
      goto optional_comma;

    case FMT_DOLLAR:
      get_fnode (fmt, &head, &tail, FMT_DOLLAR);
      tail->repeat = 1;
      notify_std (&dtp->common, GFC_STD_GNU, "Extension: $ descriptor");
      goto between_desc;

    case FMT_T:
    case FMT_TL:
    case FMT_TR:
      t2 = format_lex (fmt);
      if (t2 != FMT_POSINT)
	{
	  fmt->error = posint_required;
	  goto finished;
	}
      get_fnode (fmt, &head, &tail, t);
      tail->u.n = fmt->value;
      tail->repeat = 1;
      goto between_desc;

    case FMT_I:
    case FMT_B:
    case FMT_O:
    case FMT_Z:
    case FMT_E:
    case FMT_EN:
    case FMT_ES:
    case FMT_D:
    case FMT_L:
    case FMT_A:
    case FMT_F:
    case FMT_G:
      repeat = 1;
      goto data_desc;

    case FMT_H:
      get_fnode (fmt, &head, &tail, FMT_STRING);

      if (fmt->format_string_len < 1)
	{
	  fmt->error = bad_hollerith;
	  goto finished;
	}

      tail->u.string.p = fmt->format_string;
      tail->u.string.length = 1;
      tail->repeat = 1;

      fmt->format_string++;
      fmt->format_string_len--;

      goto between_desc;

    case FMT_END:
      fmt->error = unexpected_end;
      goto finished;

    case FMT_BADSTRING:
      goto finished;

    case FMT_RPAREN:
      goto finished;

    default:
      fmt->error = unexpected_element;
      goto finished;
    }

  /* In this state, t must currently be a data descriptor.  Deal with
     things that can/must follow the descriptor */
 data_desc:
  switch (t)
    {
    case FMT_P:
      t = format_lex (fmt);
      if (t == FMT_POSINT)
	{
	  fmt->error = "Repeat count cannot follow P descriptor";
	  goto finished;
	}

      fmt->saved_token = t;
      get_fnode (fmt, &head, &tail, FMT_P);

      goto optional_comma;

    case FMT_L:
      t = format_lex (fmt);
      if (t != FMT_POSINT)
	{
	  if (notification_std(GFC_STD_GNU) == ERROR)
	    {
	      fmt->error = posint_required;
	      goto finished;
	    }
	  else
	    {
	      fmt->saved_token = t;
	      fmt->value = 1;	/* Default width */
	      notify_std (&dtp->common, GFC_STD_GNU, posint_required);
	    }
	}

      get_fnode (fmt, &head, &tail, FMT_L);
      tail->u.n = fmt->value;
      tail->repeat = repeat;
      break;

    case FMT_A:
      t = format_lex (fmt);
      if (t != FMT_POSINT)
	{
	  fmt->saved_token = t;
	  fmt->value = -1;		/* Width not present */
	}

      get_fnode (fmt, &head, &tail, FMT_A);
      tail->repeat = repeat;
      tail->u.n = fmt->value;
      break;

    case FMT_D:
    case FMT_E:
    case FMT_F:
    case FMT_G:
    case FMT_EN:
    case FMT_ES:
      get_fnode (fmt, &head, &tail, t);
      tail->repeat = repeat;

      u = format_lex (fmt);
      if (t == FMT_F || dtp->u.p.mode == WRITING)
	{
	  if (u != FMT_POSINT && u != FMT_ZERO)
	    {
	      fmt->error = nonneg_required;
	      goto finished;
	    }
	}
      else
	{
	  if (u != FMT_POSINT)
	    {
	      fmt->error = posint_required;
	      goto finished;
	    }
	}

      tail->u.real.w = fmt->value;
      t2 = t;
      t = format_lex (fmt);
      if (t != FMT_PERIOD)
	{
	  /* We treat a missing decimal descriptor as 0.  Note: This is only
	     allowed if -std=legacy, otherwise an error occurs.  */
	  if (compile_options.warn_std != 0)
	    {
	      fmt->error = period_required;
	      goto finished;
	    }
	  fmt->saved_token = t;
	  tail->u.real.d = 0;
	  break;
	}

      t = format_lex (fmt);
      if (t != FMT_ZERO && t != FMT_POSINT)
	{
	  fmt->error = nonneg_required;
	  goto finished;
	}

      tail->u.real.d = fmt->value;

      if (t == FMT_D || t == FMT_F)
	break;

      tail->u.real.e = -1;

      /* Look for optional exponent */
      t = format_lex (fmt);
      if (t != FMT_E)
	fmt->saved_token = t;
      else
	{
	  t = format_lex (fmt);
	  if (t != FMT_POSINT)
	    {
	      fmt->error = "Positive exponent width required in format";
	      goto finished;
	    }

	  tail->u.real.e = fmt->value;
	}

      break;

    case FMT_H:
      if (repeat > fmt->format_string_len)
	{
	  fmt->error = bad_hollerith;
	  goto finished;
	}

      get_fnode (fmt, &head, &tail, FMT_STRING);

      tail->u.string.p = fmt->format_string;
      tail->u.string.length = repeat;
      tail->repeat = 1;

      fmt->format_string += fmt->value;
      fmt->format_string_len -= repeat;

      break;

    case FMT_I:
    case FMT_B:
    case FMT_O:
    case FMT_Z:
      get_fnode (fmt, &head, &tail, t);
      tail->repeat = repeat;

      t = format_lex (fmt);

      if (dtp->u.p.mode == READING)
	{
	  if (t != FMT_POSINT)
	    {
	      fmt->error = posint_required;
	      goto finished;
	    }
	}
      else
	{
	  if (t != FMT_ZERO && t != FMT_POSINT)
	    {
	      fmt->error = nonneg_required;
	      goto finished;
	    }
	}

      tail->u.integer.w = fmt->value;
      tail->u.integer.m = -1;

      t = format_lex (fmt);
      if (t != FMT_PERIOD)
	{
	  fmt->saved_token = t;
	}
      else
	{
	  t = format_lex (fmt);
	  if (t != FMT_ZERO && t != FMT_POSINT)
	    {
	      fmt->error = nonneg_required;
	      goto finished;
	    }

	  tail->u.integer.m = fmt->value;
	}

      if (tail->u.integer.w != 0 && tail->u.integer.m > tail->u.integer.w)
	{
	  fmt->error = "Minimum digits exceeds field width";
	  goto finished;
	}

      break;

    default:
      fmt->error = unexpected_element;
      goto finished;
    }

  /* Between a descriptor and what comes next */
 between_desc:
  t = format_lex (fmt);
  switch (t)
    {
    case FMT_COMMA:
      goto format_item;

    case FMT_RPAREN:
      goto finished;

    case FMT_SLASH:
    case FMT_COLON:
      get_fnode (fmt, &head, &tail, t);
      tail->repeat = 1;
      goto optional_comma;

    case FMT_END:
      fmt->error = unexpected_end;
      goto finished;

    default:
      /* Assume a missing comma, this is a GNU extension */
      goto format_item_1;
    }

  /* Optional comma is a weird between state where we've just finished
     reading a colon, slash or P descriptor. */
 optional_comma:
  t = format_lex (fmt);
  switch (t)
    {
    case FMT_COMMA:
      break;

    case FMT_RPAREN:
      goto finished;

    default:			/* Assume that we have another format item */
      fmt->saved_token = t;
      break;
    }

  goto format_item;

 finished:
  return head;
}


/* format_error()-- Generate an error message for a format statement.
 * If the node that gives the location of the error is NULL, the error
 * is assumed to happen at parse time, and the current location of the
 * parser is shown.
 *
 * We generate a message showing where the problem is.  We take extra
 * care to print only the relevant part of the format if it is longer
 * than a standard 80 column display. */

void
format_error (st_parameter_dt *dtp, const fnode *f, const char *message)
{
  int width, i, j, offset;
  char *p, buffer[300];
  format_data *fmt = dtp->u.p.fmt;

  if (f != NULL)
    fmt->format_string = f->source;

  sprintf (buffer, "%s\n", message);

  j = fmt->format_string - dtp->format;

  offset = (j > 60) ? j - 40 : 0;

  j -= offset;
  width = dtp->format_len - offset;

  if (width > 80)
    width = 80;

  /* Show the format */

  p = strchr (buffer, '\0');

  memcpy (p, dtp->format + offset, width);

  p += width;
  *p++ = '\n';

  /* Show where the problem is */

  for (i = 1; i < j; i++)
    *p++ = ' ';

  *p++ = '^';
  *p = '\0';

  generate_error (&dtp->common, LIBERROR_FORMAT, buffer);
}


/* parse_format()-- Parse a format string.  */

void
parse_format (st_parameter_dt *dtp)
{
  format_data *fmt;

  dtp->u.p.fmt = fmt = get_mem (sizeof (format_data));
  fmt->format_string = dtp->format;
  fmt->format_string_len = dtp->format_len;

  fmt->string = NULL;
  fmt->saved_token = FMT_NONE;
  fmt->error = NULL;
  fmt->value = 0;

  /* Initialize variables used during traversal of the tree */

  fmt->reversion_ok = 0;
  fmt->saved_format = NULL;

  /* Allocate the first format node as the root of the tree */

  fmt->last = &fmt->array;
  fmt->last->next = NULL;
  fmt->avail = &fmt->array.array[0];

  memset (fmt->avail, 0, sizeof (*fmt->avail));
  fmt->avail->format = FMT_LPAREN;
  fmt->avail->repeat = 1;
  fmt->avail++;

  if (format_lex (fmt) == FMT_LPAREN)
    fmt->array.array[0].u.child = parse_format_list (dtp);
  else
    fmt->error = "Missing initial left parenthesis in format";

  if (fmt->error)
    format_error (dtp, NULL, fmt->error);
}


/* revert()-- Do reversion of the format.  Control reverts to the left
 * parenthesis that matches the rightmost right parenthesis.  From our
 * tree structure, we are looking for the rightmost parenthesis node
 * at the second level, the first level always being a single
 * parenthesis node.  If this node doesn't exit, we use the top
 * level. */

static void
revert (st_parameter_dt *dtp)
{
  fnode *f, *r;
  format_data *fmt = dtp->u.p.fmt;

  dtp->u.p.reversion_flag = 1;

  r = NULL;

  for (f = fmt->array.array[0].u.child; f; f = f->next)
    if (f->format == FMT_LPAREN)
      r = f;

  /* If r is NULL because no node was found, the whole tree will be used */

  fmt->array.array[0].current = r;
  fmt->array.array[0].count = 0;
}


/* next_format0()-- Get the next format node without worrying about
 * reversion.  Returns NULL when we hit the end of the list.
 * Parenthesis nodes are incremented after the list has been
 * exhausted, other nodes are incremented before they are returned. */

static const fnode *
next_format0 (fnode * f)
{
  const fnode *r;

  if (f == NULL)
    return NULL;

  if (f->format != FMT_LPAREN)
    {
      f->count++;
      if (f->count <= f->repeat)
	return f;

      f->count = 0;
      return NULL;
    }

  /* Deal with a parenthesis node */

  for (; f->count < f->repeat; f->count++)
    {
      if (f->current == NULL)
	f->current = f->u.child;

      for (; f->current != NULL; f->current = f->current->next)
	{
	  r = next_format0 (f->current);
	  if (r != NULL)
	    return r;
	}
    }

  f->count = 0;
  return NULL;
}


/* next_format()-- Return the next format node.  If the format list
 * ends up being exhausted, we do reversion.  Reversion is only
 * allowed if the we've seen a data descriptor since the
 * initialization or the last reversion.  We return NULL if there
 * are no more data descriptors to return (which is an error
 * condition). */

const fnode *
next_format (st_parameter_dt *dtp)
{
  format_token t;
  const fnode *f;
  format_data *fmt = dtp->u.p.fmt;

  if (fmt->saved_format != NULL)
    {				/* Deal with a pushed-back format node */
      f = fmt->saved_format;
      fmt->saved_format = NULL;
      goto done;
    }

  f = next_format0 (&fmt->array.array[0]);
  if (f == NULL)
    {
      if (!fmt->reversion_ok)
	return NULL;

      fmt->reversion_ok = 0;
      revert (dtp);

      f = next_format0 (&fmt->array.array[0]);
      if (f == NULL)
	{
	  format_error (dtp, NULL, reversion_error);
	  return NULL;
	}

      /* Push the first reverted token and return a colon node in case
       * there are no more data items. */

      fmt->saved_format = f;
      return &colon_node;
    }

  /* If this is a data edit descriptor, then reversion has become OK. */
 done:
  t = f->format;

  if (!fmt->reversion_ok &&
      (t == FMT_I || t == FMT_B || t == FMT_O || t == FMT_Z || t == FMT_F ||
       t == FMT_E || t == FMT_EN || t == FMT_ES || t == FMT_G || t == FMT_L ||
       t == FMT_A || t == FMT_D))
    fmt->reversion_ok = 1;
  return f;
}


/* unget_format()-- Push the given format back so that it will be
 * returned on the next call to next_format() without affecting
 * counts.  This is necessary when we've encountered a data
 * descriptor, but don't know what the data item is yet.  The format
 * node is pushed back, and we return control to the main program,
 * which calls the library back with the data item (or not). */

void
unget_format (st_parameter_dt *dtp, const fnode *f)
{
  dtp->u.p.fmt->saved_format = f;
}

