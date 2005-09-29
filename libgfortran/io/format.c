/* Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* format.c-- parse a FORMAT string into a binary format suitable for
 * interpretation during I/O statements */

#include "config.h"
#include <ctype.h>
#include <string.h>
#include "libgfortran.h"
#include "io.h"



/* Number of format nodes that we can store statically before we have
 * to resort to dynamic allocation.  The root node is array[0]. */

#define FARRAY_SIZE 200

static fnode *avail, array[FARRAY_SIZE];

/* Local variables for checking format strings.  The saved_token is
 * used to back up by a single format token during the parsing process. */

static char *format_string, *string;
static const char *error;
static format_token saved_token;
static int value, format_string_len, reversion_ok;

static fnode *saved_format, colon_node = { FMT_COLON };

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
next_char (int literal)
{
  int c;

  do
    {
      if (format_string_len == 0)
	return -1;

      format_string_len--;
      c = toupper (*format_string++);
    }
  while (c == ' ' && !literal);

  return c;
}


/* unget_char()-- Back up one character position. */

#define unget_char() { format_string--;  format_string_len++; }


/* get_fnode()-- Allocate a new format node, inserting it into the
 * current singly linked list.  These are initially allocated from the
 * static buffer. */

static fnode *
get_fnode (fnode ** head, fnode ** tail, format_token t)
{
  fnode *f;

  if (avail - array >= FARRAY_SIZE)
    f = get_mem (sizeof (fnode));
  else
    {
      f = avail++;
      memset (f, '\0', sizeof (fnode));
    }

  if (*head == NULL)
    *head = *tail = f;
  else
    {
      (*tail)->next = f;
      *tail = f;
    }

  f->format = t;
  f->repeat = -1;
  f->source = format_string;
  return f;
}


/* free_fnode()-- Recursive function to free the given fnode and
 * everything it points to.  We only have to actually free something
 * if it is outside of the static array. */

static void
free_fnode (fnode * f)
{
  fnode *next;

  for (; f; f = next)
    {
      next = f->next;

      if (f->format == FMT_LPAREN)
	free_fnode (f->u.child);
      if (f < array || f >= array + FARRAY_SIZE)
	free_mem (f);
    }
}


/* free_fnodes()-- Free the current tree of fnodes.  We only have to
 * traverse the tree if some nodes were allocated dynamically. */

void
free_fnodes (void)
{
  if (avail - array >= FARRAY_SIZE)
    free_fnode (&array[0]);

  avail = array;
  memset(array, 0, sizeof(avail[0]) * FARRAY_SIZE);
}


/* format_lex()-- Simple lexical analyzer for getting the next token
 * in a FORMAT string.  We support a one-level token pushback in the
 * saved_token variable. */

static format_token
format_lex (void)
{
  format_token token;
  int negative_flag;
  int c;
  char delim;

  if (saved_token != FMT_NONE)
    {
      token = saved_token;
      saved_token = FMT_NONE;
      return token;
    }

  negative_flag = 0;
  c = next_char (0);

  switch (c)
    {
    case '-':
      negative_flag = 1;
      /* Fall Through */

    case '+':
      c = next_char (0);
      if (!isdigit (c))
	{
	  token = FMT_UNKNOWN;
	  break;
	}

      value = c - '0';

      for (;;)
	{
	  c = next_char (0);
	  if (!isdigit (c))
	    break;

	  value = 10 * value + c - '0';
	}

      unget_char ();

      if (negative_flag)
	value = -value;
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
      value = c - '0';

      for (;;)
	{
	  c = next_char (0);
	  if (!isdigit (c))
	    break;

	  value = 10 * value + c - '0';
	}

      unget_char ();
      token = (value == 0) ? FMT_ZERO : FMT_POSINT;
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
      switch (next_char (0))
	{
	case 'L':
	  token = FMT_TL;
	  break;
	case 'R':
	  token = FMT_TR;
	  break;
	default:
	  token = FMT_T;
	  unget_char ();
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
      switch (next_char (0))
	{
	case 'S':
	  token = FMT_SS;
	  break;
	case 'P':
	  token = FMT_SP;
	  break;
	default:
	  token = FMT_S;
	  unget_char ();
	  break;
	}

      break;

    case 'B':
      switch (next_char (0))
	{
	case 'N':
	  token = FMT_BN;
	  break;
	case 'Z':
	  token = FMT_BZ;
	  break;
	default:
	  token = FMT_B;
	  unget_char ();
	  break;
	}

      break;

    case '\'':
    case '"':
      delim = c;

      string = format_string;
      value = 0;		/* This is the length of the string */

      for (;;)
	{
	  c = next_char (1);
	  if (c == -1)
	    {
	      token = FMT_BADSTRING;
	      error = bad_string;
	      break;
	    }

	  if (c == delim)
	    {
	      c = next_char (1);

	      if (c == -1)
		{
		  token = FMT_BADSTRING;
		  error = bad_string;
		  break;
		}

	      if (c != delim)
		{
		  unget_char ();
		  token = FMT_STRING;
		  break;
		}
	    }

	  value++;
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
      switch (next_char (0))
	{
	case 'N':
	  token = FMT_EN;
	  break;
	case 'S':
	  token = FMT_ES;
	  break;
	default:
	  token = FMT_E;
	  unget_char ();
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
parse_format_list (void)
{
  fnode *head, *tail;
  format_token t, u, t2;
  int repeat;

  head = tail = NULL;

  /* Get the next format item */
 format_item:
  t = format_lex ();
 format_item_1:
  switch (t)
    {
    case FMT_POSINT:
      repeat = value;

      t = format_lex ();
      switch (t)
	{
	case FMT_LPAREN:
	  get_fnode (&head, &tail, FMT_LPAREN);
	  tail->repeat = repeat;
	  tail->u.child = parse_format_list ();
	  if (error != NULL)
	    goto finished;

	  goto between_desc;

	case FMT_SLASH:
	  get_fnode (&head, &tail, FMT_SLASH);
	  tail->repeat = repeat;
	  goto optional_comma;

	case FMT_X:
	  get_fnode (&head, &tail, FMT_X);
	  tail->repeat = 1;
	  tail->u.k = value;
	  goto between_desc;

	case FMT_P:
	  goto p_descriptor;

	default:
	  goto data_desc;
	}

    case FMT_LPAREN:
      get_fnode (&head, &tail, FMT_LPAREN);
      tail->repeat = 1;
      tail->u.child = parse_format_list ();
      if (error != NULL)
	goto finished;

      goto between_desc;

    case FMT_SIGNED_INT:	/* Signed integer can only precede a P format.  */
    case FMT_ZERO:		/* Same for zero.  */
      t = format_lex ();
      if (t != FMT_P)
	{
	  error = "Expected P edit descriptor in format";
	  goto finished;
	}

    p_descriptor:
      get_fnode (&head, &tail, FMT_P);
      tail->u.k = value;
      tail->repeat = 1;

      t = format_lex ();
      if (t == FMT_F || t == FMT_EN || t == FMT_ES || t == FMT_D
	  || t == FMT_G || t == FMT_E)
	{
	  repeat = 1;
	  goto data_desc;
	}

      saved_token = t;
      goto optional_comma;

    case FMT_P:		/* P and X require a prior number */
      error = "P descriptor requires leading scale factor";
      goto finished;

    case FMT_X:
/*
   EXTENSION!

   If we would be pedantic in the library, we would have to reject
   an X descriptor without an integer prefix:

      error = "X descriptor requires leading space count";
      goto finished;

   However, this is an extension supported by many Fortran compilers,
   including Cray, HP, AIX, and IRIX.  Therefore, we allow it in the
   runtime library, and make the front end reject it if the compiler
   is in pedantic mode.  The interpretation of 'X' is '1X'.
*/
      get_fnode (&head, &tail, FMT_X);
      tail->repeat = 1;
      tail->u.k = 1;
      goto between_desc;

    case FMT_STRING:
      get_fnode (&head, &tail, FMT_STRING);

      tail->u.string.p = string;
      tail->u.string.length = value;
      tail->repeat = 1;
      goto optional_comma;

    case FMT_S:
    case FMT_SS:
    case FMT_SP:
    case FMT_BN:
    case FMT_BZ:
      get_fnode (&head, &tail, t);
      tail->repeat = 1;
      goto between_desc;

    case FMT_COLON:
      get_fnode (&head, &tail, FMT_COLON);
      tail->repeat = 1;
      goto optional_comma;

    case FMT_SLASH:
      get_fnode (&head, &tail, FMT_SLASH);
      tail->repeat = 1;
      tail->u.r = 1;
      goto optional_comma;

    case FMT_DOLLAR:
      get_fnode (&head, &tail, FMT_DOLLAR);
      tail->repeat = 1;
      notify_std (GFC_STD_GNU, "Extension: $ descriptor");
      goto between_desc;

    case FMT_T:
    case FMT_TL:
    case FMT_TR:
      t2 = format_lex ();
      if (t2 != FMT_POSINT)
	{
	  error = posint_required;
	  goto finished;
	}
      get_fnode (&head, &tail, t);
      tail->u.n = value;
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
      get_fnode (&head, &tail, FMT_STRING);

      if (format_string_len < 1)
	{
	  error = bad_hollerith;
	  goto finished;
	}

      tail->u.string.p = format_string;
      tail->u.string.length = 1;
      tail->repeat = 1;

      format_string++;
      format_string_len--;

      goto between_desc;

    case FMT_END:
      error = unexpected_end;
      goto finished;

    case FMT_BADSTRING:
      goto finished;

    case FMT_RPAREN:
      goto finished;

    default:
      error = unexpected_element;
      goto finished;
    }

  /* In this state, t must currently be a data descriptor.  Deal with
     things that can/must follow the descriptor */
 data_desc:
  switch (t)
    {
    case FMT_P:
      t = format_lex ();
      if (t == FMT_POSINT)
	{
	  error = "Repeat count cannot follow P descriptor";
	  goto finished;
	}

      saved_token = t;
      get_fnode (&head, &tail, FMT_P);

      goto optional_comma;

    case FMT_L:
      t = format_lex ();
      if (t != FMT_POSINT)
	{
	  error = posint_required;
	  goto finished;
	}

      get_fnode (&head, &tail, FMT_L);
      tail->u.n = value;
      tail->repeat = repeat;
      break;

    case FMT_A:
      t = format_lex ();
      if (t != FMT_POSINT)
	{
	  saved_token = t;
	  value = -1;		/* Width not present */
	}

      get_fnode (&head, &tail, FMT_A);
      tail->repeat = repeat;
      tail->u.n = value;
      break;

    case FMT_D:
    case FMT_E:
    case FMT_F:
    case FMT_G:
    case FMT_EN:
    case FMT_ES:
      get_fnode (&head, &tail, t);
      tail->repeat = repeat;

      u = format_lex ();
      if (t == FMT_F || g.mode == WRITING)
	{
	  if (u != FMT_POSINT && u != FMT_ZERO)
	    {
	      error = nonneg_required;
	      goto finished;
	    }
	}
      else
	{
	  if (u != FMT_POSINT)
	    {
	      error = posint_required;
	      goto finished;
	    }
	}

      tail->u.real.w = value;
      t2 = t;
      t = format_lex ();
      if (t != FMT_PERIOD)
	{
	  error = period_required;
	  goto finished;
	}

      t = format_lex ();
      if (t != FMT_ZERO && t != FMT_POSINT)
	{
	  error = nonneg_required;
	  goto finished;
	}

      tail->u.real.d = value;

      if (t == FMT_D || t == FMT_F)
	break;

      tail->u.real.e = -1;

      /* Look for optional exponent */
      t = format_lex ();
      if (t != FMT_E)
	saved_token = t;
      else
	{
	  t = format_lex ();
	  if (t != FMT_POSINT)
	    {
	      error = "Positive exponent width required in format";
	      goto finished;
	    }

	  tail->u.real.e = value;
	}

      break;

    case FMT_H:
      if (repeat > format_string_len)
	{
	  error = bad_hollerith;
	  goto finished;
	}

      get_fnode (&head, &tail, FMT_STRING);

      tail->u.string.p = format_string;
      tail->u.string.length = repeat;
      tail->repeat = 1;

      format_string += value;
      format_string_len -= repeat;

      break;

    case FMT_I:
    case FMT_B:
    case FMT_O:
    case FMT_Z:
      get_fnode (&head, &tail, t);
      tail->repeat = repeat;

      t = format_lex ();

      if (g.mode == READING)
	{
	  if (t != FMT_POSINT)
	    {
	      error = posint_required;
	      goto finished;
	    }
	}
      else
	{
	  if (t != FMT_ZERO && t != FMT_POSINT)
	    {
	      error = nonneg_required;
	      goto finished;
	    }
	}

      tail->u.integer.w = value;
      tail->u.integer.m = -1;

      t = format_lex ();
      if (t != FMT_PERIOD)
	{
	  saved_token = t;
	}
      else
	{
	  t = format_lex ();
	  if (t != FMT_ZERO && t != FMT_POSINT)
	    {
	      error = nonneg_required;
	      goto finished;
	    }

	  tail->u.integer.m = value;
	}

      if (tail->u.integer.w != 0 && tail->u.integer.m > tail->u.integer.w)
	{
	  error = "Minimum digits exceeds field width";
	  goto finished;
	}

      break;

    default:
      error = unexpected_element;
      goto finished;
    }

  /* Between a descriptor and what comes next */
 between_desc:
  t = format_lex ();
  switch (t)
    {
    case FMT_COMMA:
      goto format_item;

    case FMT_RPAREN:
      goto finished;

    case FMT_SLASH:
      get_fnode (&head, &tail, FMT_SLASH);
      tail->repeat = 1;

      /* Fall Through */

    case FMT_COLON:
      goto optional_comma;

    case FMT_END:
      error = unexpected_end;
      goto finished;

    default:
      /* Assume a missing comma, this is a GNU extension */
      goto format_item_1;
    }

  /* Optional comma is a weird between state where we've just finished
     reading a colon, slash or P descriptor. */
 optional_comma:
  t = format_lex ();
  switch (t)
    {
    case FMT_COMMA:
      break;

    case FMT_RPAREN:
      goto finished;

    default:			/* Assume that we have another format item */
      saved_token = t;
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
 * After freeing any dynamically allocated fnodes, generate a message
 * showing where the problem is.  We take extra care to print only the
 * relevant part of the format if it is longer than a standard 80
 * column display. */

void
format_error (fnode * f, const char *message)
{
  int width, i, j, offset;
  char *p, buffer[300];

  if (f != NULL)
    format_string = f->source;

  free_fnodes ();

  st_sprintf (buffer, "%s\n", message);

  j = format_string - ioparm.format;

  offset = (j > 60) ? j - 40 : 0;

  j -= offset;
  width = ioparm.format_len - offset;

  if (width > 80)
    width = 80;

  /* Show the format */

  p = strchr (buffer, '\0');

  memcpy (p, ioparm.format + offset, width);

  p += width;
  *p++ = '\n';

  /* Show where the problem is */

  for (i = 1; i < j; i++)
    *p++ = ' ';

  *p++ = '^';
  *p = '\0';

  generate_error (ERROR_FORMAT, buffer);
}


/* parse_format()-- Parse a format string.  */

void
parse_format (void)
{
  format_string = ioparm.format;
  format_string_len = ioparm.format_len;

  saved_token = FMT_NONE;
  error = NULL;

  /* Initialize variables used during traversal of the tree */

  reversion_ok = 0;
  g.reversion_flag = 0;
  saved_format = NULL;

  /* Allocate the first format node as the root of the tree */

  avail = array;

  avail->format = FMT_LPAREN;
  avail->repeat = 1;
  avail++;

  if (format_lex () == FMT_LPAREN)
    array[0].u.child = parse_format_list ();
  else
    error = "Missing initial left parenthesis in format";

  if (error)
    format_error (NULL, error);
}


/* revert()-- Do reversion of the format.  Control reverts to the left
 * parenthesis that matches the rightmost right parenthesis.  From our
 * tree structure, we are looking for the rightmost parenthesis node
 * at the second level, the first level always being a single
 * parenthesis node.  If this node doesn't exit, we use the top
 * level. */

static void
revert (void)
{
  fnode *f, *r;

  g.reversion_flag = 1;

  r = NULL;

  for (f = array[0].u.child; f; f = f->next)
    if (f->format == FMT_LPAREN)
      r = f;

  /* If r is NULL because no node was found, the whole tree will be used */

  array[0].current = r;
  array[0].count = 0;
}


/* next_format0()-- Get the next format node without worrying about
 * reversion.  Returns NULL when we hit the end of the list.
 * Parenthesis nodes are incremented after the list has been
 * exhausted, other nodes are incremented before they are returned. */

static fnode *
next_format0 (fnode * f)
{
  fnode *r;

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
 * initialization or the last reversion.  We return NULL if the there
 * are no more data descriptors to return (which is an error
 * condition). */

fnode *
next_format (void)
{
  format_token t;
  fnode *f;

  if (saved_format != NULL)
    {				/* Deal with a pushed-back format node */
      f = saved_format;
      saved_format = NULL;
      goto done;
    }

  f = next_format0 (&array[0]);
  if (f == NULL)
    {
      if (!reversion_ok)
	{
	  return NULL;
	}

      reversion_ok = 0;
      revert ();

      f = next_format0 (&array[0]);
      if (f == NULL)
	{
	  format_error (NULL, reversion_error);
	  return NULL;
	}

      /* Push the first reverted token and return a colon node in case
       * there are no more data items. */

      saved_format = f;
      return &colon_node;
    }

  /* If this is a data edit descriptor, then reversion has become OK. */
 done:
  t = f->format;

  if (!reversion_ok &&
      (t == FMT_I || t == FMT_B || t == FMT_O || t == FMT_Z || t == FMT_F ||
       t == FMT_E || t == FMT_EN || t == FMT_ES || t == FMT_G || t == FMT_L ||
       t == FMT_A || t == FMT_D))
    reversion_ok = 1;
  return f;
}


/* unget_format()-- Push the given format back so that it will be
 * returned on the next call to next_format() without affecting
 * counts.  This is necessary when we've encountered a data
 * descriptor, but don't know what the data item is yet.  The format
 * node is pushed back, and we return control to the main program,
 * which calls the library back with the data item (or not). */

void
unget_format (fnode * f)
{
  saved_format = f;
}




#if 0

static void dump_format1 (fnode * f);

/* dump_format0()-- Dump a single format node */

void
dump_format0 (fnode * f)
{
  char *p;
  int i;

  switch (f->format)
    {
    case FMT_COLON:
      st_printf (" :");
      break;
    case FMT_SLASH:
      st_printf (" %d/", f->u.r);
      break;
    case FMT_DOLLAR:
      st_printf (" $");
      break;
    case FMT_T:
      st_printf (" T%d", f->u.n);
      break;
    case FMT_TR:
      st_printf (" TR%d", f->u.n);
      break;
    case FMT_TL:
      st_printf (" TL%d", f->u.n);
      break;
    case FMT_X:
      st_printf (" %dX", f->u.n);
      break;
    case FMT_S:
      st_printf (" S");
      break;
    case FMT_SS:
      st_printf (" SS");
      break;
    case FMT_SP:
      st_printf (" SP");
      break;

    case FMT_LPAREN:
      if (f->repeat == 1)
	st_printf (" (");
      else
	st_printf (" %d(", f->repeat);

      dump_format1 (f->u.child);
      st_printf (" )");
      break;

    case FMT_STRING:
      st_printf (" '");
      p = f->u.string.p;
      for (i = f->u.string.length; i > 0; i--)
	st_printf ("%c", *p++);

      st_printf ("'");
      break;

    case FMT_P:
      st_printf (" %dP", f->u.k);
      break;
    case FMT_I:
      st_printf (" %dI%d.%d", f->repeat, f->u.integer.w, f->u.integer.m);
      break;

    case FMT_B:
      st_printf (" %dB%d.%d", f->repeat, f->u.integer.w, f->u.integer.m);
      break;

    case FMT_O:
      st_printf (" %dO%d.%d", f->repeat, f->u.integer.w, f->u.integer.m);
      break;

    case FMT_Z:
      st_printf (" %dZ%d.%d", f->repeat, f->u.integer.w, f->u.integer.m);
      break;

    case FMT_BN:
      st_printf (" BN");
      break;
    case FMT_BZ:
      st_printf (" BZ");
      break;
    case FMT_D:
      st_printf (" %dD%d.%d", f->repeat, f->u.real.w, f->u.real.d);
      break;

    case FMT_EN:
      st_printf (" %dEN%d.%dE%d", f->repeat, f->u.real.w, f->u.real.d,
		 f->u.real.e);
      break;

    case FMT_ES:
      st_printf (" %dES%d.%dE%d", f->repeat, f->u.real.w, f->u.real.d,
		 f->u.real.e);
      break;

    case FMT_F:
      st_printf (" %dF%d.%d", f->repeat, f->u.real.w, f->u.real.d);
      break;

    case FMT_E:
      st_printf (" %dE%d.%dE%d", f->repeat, f->u.real.w, f->u.real.d,
		 f->u.real.e);
      break;

    case FMT_G:
      st_printf (" %dG%d.%dE%d", f->repeat, f->u.real.w, f->u.real.d,
		 f->u.real.e);
      break;

    case FMT_L:
      st_printf (" %dL%d", f->repeat, f->u.w);
      break;
    case FMT_A:
      st_printf (" %dA%d", f->repeat, f->u.w);
      break;

    default:
      st_printf (" ???");
      break;
    }
}


/* dump_format1()-- Dump a string of format nodes */

static void
dump_format1 (fnode * f)
{
  for (; f; f = f->next)
    dump_format1 (f);
}

/* dump_format()-- Dump the whole format node tree */

void
dump_format (void)
{
  st_printf ("format = ");
  dump_format0 (&array[0]);
  st_printf ("\n");
}


void
next_test (void)
{
  fnode *f;
  int i;

  for (i = 0; i < 20; i++)
    {
      f = next_format ();
      if (f == NULL)
	{
	  st_printf ("No format!\n");
	  break;
	}

      dump_format1 (f);
      st_printf ("\n");
    }
}

#endif
