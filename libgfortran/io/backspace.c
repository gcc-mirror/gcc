/* Copyright (C) 2002-2003 Free Software Foundation, Inc.
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
#include "libgfortran.h"
#include "io.h"

/* backspace.c -- Implement the BACKSPACE statement */

/* formatted_backspace(void)-- Move the file back one line.  The
 * current position is after the newline that terminates the previous
 * record, and we have to sift backwards to find the newline before
 * that or the start of the file, whichever comes first. */

#define READ_CHUNK 4096

static void
formatted_backspace (void)
{
  gfc_offset base;
  char *p;
  int n;

  base = file_position (current_unit->s) - 1;

  do
    {
      n = (base < READ_CHUNK) ? base : READ_CHUNK;
      base -= n;

      p = salloc_r_at (current_unit->s, &n, base);
      if (p == NULL)
	goto io_error;

      /* Because we've moved backwords from the current position, it
       * should not be possible to get a short read.  Because it isn't
       * clear what to do about such thing, we ignore the possibility. */

      /* There is no memrchr() in the C library, so we have to do it
       * ourselves. */

      n--;
      while (n >= 0)
	{
	  if (p[n] == '\n')
	    {
	      base += n + 1;
	      goto done;
	    }

	  n--;
	}

    }
  while (base != 0);

/* base is the new pointer.  Seek to it exactly */

done:
  if (sseek (current_unit->s, base) == FAILURE)
    goto io_error;
  current_unit->last_record--;

  return;

io_error:
  generate_error (ERROR_OS, NULL);
}


/* unformatted_backspace()-- Move the file backwards for an
 * unformatted sequential file.  We are guaranteed to be between
 * records on entry and we have to shift to the previous record.  */

static void
unformatted_backspace (void)
{
  gfc_offset *p, new;
  int length;

  length = sizeof (gfc_offset);

  p = (gfc_offset *) salloc_r_at (current_unit->s, &length,
				file_position (current_unit->s) - length);
  if (p == NULL)
    goto io_error;

  new = file_position (current_unit->s) - *p - length;
  if (sseek (current_unit->s, new) == FAILURE)
    goto io_error;

  current_unit->last_record--;
  return;

io_error:
  generate_error (ERROR_OS, NULL);
}


void
st_backspace (void)
{
  gfc_unit *u;

  library_start ();

  u = find_unit (ioparm.unit);
  if (u == NULL)
    {
      generate_error (ERROR_BAD_UNIT, NULL);
      goto done;
    }

  current_unit = u;

  /* Ignore direct access.  Non-advancing I/O is only allowed for
   * formatted sequential I/O and the next direct access transfer
   * repositions the file anyway. */

  if (u->flags.access == ACCESS_DIRECT)
    goto done;

  /* Check for special cases involving the ENDFILE record first */

  if (u->endfile == AFTER_ENDFILE)
    u->endfile = AT_ENDFILE;
  else
    {
      if (u->current_record)
	next_record (1);

      if (file_position (u->s) == 0)
	goto done;		/* Common special case */

      if (u->flags.form == FORM_FORMATTED)
	formatted_backspace ();
      else
	unformatted_backspace ();
    }

done:
  library_end ();
}
