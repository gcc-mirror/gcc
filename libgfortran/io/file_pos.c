/* Copyright (C) 2002-2003, 2005 Free Software Foundation, Inc.
   Contributed by Andy Vaught and Janne Blomqvist

This file is part of the GNU Fortran runtime library (libgfortran).

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

#include "config.h"
#include <string.h>
#include "libgfortran.h"
#include "io.h"

/* file_pos.c-- Implement the file positioning statements, i.e. BACKSPACE,
   ENDFILE, and REWIND as well as the FLUSH statement.  */


/* formatted_backspace(fpp, u)-- Move the file back one line.  The
   current position is after the newline that terminates the previous
   record, and we have to sift backwards to find the newline before
   that or the start of the file, whichever comes first.  */

#define READ_CHUNK 4096

static void
formatted_backspace (st_parameter_filepos *fpp, gfc_unit *u)
{
  gfc_offset base;
  char *p;
  int n;

  base = file_position (u->s) - 1;

  do
    {
      n = (base < READ_CHUNK) ? base : READ_CHUNK;
      base -= n;

      p = salloc_r_at (u->s, &n, base);
      if (p == NULL)
	goto io_error;

      /* We have moved backwards from the current position, it should
         not be possible to get a short read.  Because it is not
         clear what to do about such thing, we ignore the possibility.  */

      /* There is no memrchr() in the C library, so we have to do it
         ourselves.  */

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

  /* base is the new pointer.  Seek to it exactly.  */
 done:
  if (sseek (u->s, base) == FAILURE)
    goto io_error;
  u->last_record--;
  u->endfile = NO_ENDFILE;

  return;

 io_error:
  generate_error (&fpp->common, ERROR_OS, NULL);
}


/* unformatted_backspace(fpp) -- Move the file backwards for an unformatted
   sequential file.  We are guaranteed to be between records on entry and 
   we have to shift to the previous record.  */

static void
unformatted_backspace (st_parameter_filepos *fpp, gfc_unit *u)
{
  gfc_offset m, new;
  GFC_INTEGER_4 m4;
  GFC_INTEGER_8 m8;
  int length, length_read;
  char *p;

  if (compile_options.record_marker == 0)
    length = sizeof (gfc_offset);
  else
    length = compile_options.record_marker;

  length_read = length;

  p = salloc_r_at (u->s, &length_read,
		   file_position (u->s) - length);
  if (p == NULL || length_read != length)
    goto io_error;

  /* Only CONVERT_NATIVE and CONVERT_SWAP are valid here.  */
  if (u->flags.convert == CONVERT_NATIVE)
    {
      switch (compile_options.record_marker)
	{
	case 0:
	  memcpy (&m, p, sizeof(gfc_offset));
	  break;

	case sizeof(GFC_INTEGER_4):
	  memcpy (&m4, p, sizeof (m4));
	  m = m4;
	  break;

	case sizeof(GFC_INTEGER_8):
	  memcpy (&m8, p, sizeof (m8));
	  m = m8;
	  break;

	default:
	  runtime_error ("Illegal value for record marker");
	  break;
	}
    }
  else
    {
      switch (compile_options.record_marker)
	{
	case 0:
	  reverse_memcpy (&m, p, sizeof(gfc_offset));
	  break;

	case sizeof(GFC_INTEGER_4):
	  reverse_memcpy (&m4, p, sizeof (m4));
	  m = m4;
	  break;

	case sizeof(GFC_INTEGER_8):
	  reverse_memcpy (&m8, p, sizeof (m8));
	  m = m8;
	  break;

	default:
	  runtime_error ("Illegal value for record marker");
	  break;
	}

    }

  if ((new = file_position (u->s) - m - 2*length) < 0)
    new = 0;

  if (sseek (u->s, new) == FAILURE)
    goto io_error;

  u->last_record--;
  return;

 io_error:
  generate_error (&fpp->common, ERROR_OS, NULL);
}


extern void st_backspace (st_parameter_filepos *);
export_proto(st_backspace);

void
st_backspace (st_parameter_filepos *fpp)
{
  gfc_unit *u;

  library_start (&fpp->common);

  u = find_unit (fpp->common.unit);
  if (u == NULL)
    {
      generate_error (&fpp->common, ERROR_BAD_UNIT, NULL);
      goto done;
    }

  /* Ignore direct access.  Non-advancing I/O is only allowed for formatted
     sequential I/O and the next direct access transfer repositions the file 
     anyway.  */

  if (u->flags.access == ACCESS_DIRECT)
    goto done;

  /* Check for special cases involving the ENDFILE record first.  */

  if (u->endfile == AFTER_ENDFILE)
    {
      u->endfile = AT_ENDFILE;
      flush (u->s);
      struncate (u->s);
    }
  else
    {
      if (file_position (u->s) == 0)
	goto done;		/* Common special case */

      if (u->mode == WRITING)
	{
	  flush (u->s);
	  struncate (u->s);
	  u->mode = READING;
        }

      if (u->flags.form == FORM_FORMATTED)
	formatted_backspace (fpp, u);
      else
	unformatted_backspace (fpp, u);

      u->endfile = NO_ENDFILE;
      u->current_record = 0;
      u->bytes_left = 0;
    }

 done:
  if (u != NULL)
    unlock_unit (u);

  library_end ();
}


extern void st_endfile (st_parameter_filepos *);
export_proto(st_endfile);

void
st_endfile (st_parameter_filepos *fpp)
{
  gfc_unit *u;

  library_start (&fpp->common);

  u = find_unit (fpp->common.unit);
  if (u != NULL)
    {
      if (u->current_record)
	{
	  st_parameter_dt dtp;
	  dtp.common = fpp->common;
	  memset (&dtp.u.p, 0, sizeof (dtp.u.p));
	  dtp.u.p.current_unit = u;
	  next_record (&dtp, 1);
	}

      flush (u->s);
      struncate (u->s);
      u->endfile = AFTER_ENDFILE;
      unlock_unit (u);
    }

  library_end ();
}


extern void st_rewind (st_parameter_filepos *);
export_proto(st_rewind);

void
st_rewind (st_parameter_filepos *fpp)
{
  gfc_unit *u;

  library_start (&fpp->common);

  u = find_unit (fpp->common.unit);
  if (u != NULL)
    {
      if (u->flags.access != ACCESS_SEQUENTIAL)
	generate_error (&fpp->common, ERROR_BAD_OPTION,
			"Cannot REWIND a file opened for DIRECT access");
      else
	{
	  /* Flush the buffers.  If we have been writing to the file, the last
	       written record is the last record in the file, so truncate the
	       file now.  Reset to read mode so two consecutive rewind
	       statements do not delete the file contents.  */
	  flush (u->s);
	  if (u->mode == WRITING)
	    struncate (u->s);

	  u->mode = READING;
	  u->last_record = 0;
	  if (sseek (u->s, 0) == FAILURE)
	    generate_error (&fpp->common, ERROR_OS, NULL);

	  u->endfile = NO_ENDFILE;
	  u->current_record = 0;
	  u->bytes_left = 0;
          u->read_bad = 0;
	  test_endfile (u);
	}
      /* Update position for INQUIRE.  */
      u->flags.position = POSITION_REWIND;
      unlock_unit (u);
    }

  library_end ();
}


extern void st_flush (st_parameter_filepos *);
export_proto(st_flush);

void
st_flush (st_parameter_filepos *fpp)
{
  gfc_unit *u;

  library_start (&fpp->common);

  u = find_unit (fpp->common.unit);
  if (u != NULL)
    {
      flush (u->s);
      unlock_unit (u);
    }
  else
    /* FLUSH on unconnected unit is illegal: F95 std., 9.3.5. */ 
    generate_error (&fpp->common, ERROR_BAD_OPTION,
			"Specified UNIT in FLUSH is not connected");

  library_end ();
}
