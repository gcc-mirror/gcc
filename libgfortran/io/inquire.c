/* Copyright (C) 2002-2003 Free Software Foundation, Inc.
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


/* Implement the non-IOLENGTH variant of the INQUIRY statement */

#include "config.h"
#include "libgfortran.h"
#include "io.h"


static char undefined[] = "UNDEFINED";


/* inquire_via_unit()-- Inquiry via unit number.  The unit might not exist. */

static void
inquire_via_unit (gfc_unit * u)
{
  const char *p;

  if (ioparm.exist != NULL)
  {
    if (ioparm.unit >= 0)
      *ioparm.exist = 1;
    else
      *ioparm.exist = 0;
  }

  if (ioparm.opened != NULL)
    *ioparm.opened = (u != NULL);

  if (ioparm.number != NULL)
    *ioparm.number = (u != NULL) ? u->unit_number : -1;

  if (ioparm.named != NULL)
    *ioparm.named = (u != NULL && u->flags.status != STATUS_SCRATCH);

  if (ioparm.name != NULL && u != NULL && u->flags.status != STATUS_SCRATCH)
    fstrcpy (ioparm.name, ioparm.name_len, u->file, u->file_len);

  if (ioparm.access != NULL)
    {
      if (u == NULL)
	p = undefined;
      else
	switch (u->flags.access)
	  {
	  case ACCESS_SEQUENTIAL:
	    p = "SEQUENTIAL";
	    break;
	  case ACCESS_DIRECT:
	    p = "DIRECT";
	    break;
	  default:
	    internal_error ("inquire_via_unit(): Bad access");
	  }

      cf_strcpy (ioparm.access, ioparm.access_len, p);
    }

  if (ioparm.sequential != NULL)
    {
      if (u == NULL)
	p = inquire_sequential (NULL, 0);
      else
	{
          /* disallow an open direct access file to be accessed sequentially */
          if (u->flags.access == ACCESS_DIRECT)
            p = "NO";
          else   
            p = inquire_sequential (u->file, u->file_len);
	}

      cf_strcpy (ioparm.sequential, ioparm.sequential_len, p);
    }

  if (ioparm.direct != NULL)
    {
      p = (u == NULL) ? inquire_direct (NULL, 0) :
	inquire_direct (u->file, u->file_len);

      cf_strcpy (ioparm.direct, ioparm.direct_len, p);
    }

  if (ioparm.form != NULL)
    {
      if (u == NULL)
	p = undefined;
      else
	switch (u->flags.form)
	  {
	  case FORM_FORMATTED:
	    p = "FORMATTED";
	    break;
	  case FORM_UNFORMATTED:
	    p = "UNFORMATTED";
	    break;
	  default:
	    internal_error ("inquire_via_unit(): Bad form");
	  }

      cf_strcpy (ioparm.form, ioparm.form_len, p);
    }

  if (ioparm.formatted != NULL)
    {
      p = (u == NULL) ? inquire_formatted (NULL, 0) :
	inquire_formatted (u->file, u->file_len);

      cf_strcpy (ioparm.formatted, ioparm.formatted_len, p);
    }

  if (ioparm.unformatted != NULL)
    {
      p = (u == NULL) ? inquire_unformatted (NULL, 0) :
	inquire_unformatted (u->file, u->file_len);

      cf_strcpy (ioparm.unformatted, ioparm.unformatted_len, p);
    }

  if (ioparm.recl_out != NULL)
    *ioparm.recl_out = (u != NULL) ? u->recl : 0;

  if (ioparm.nextrec != NULL)
    *ioparm.nextrec = (u != NULL) ? u->last_record + 1 : 0;

  if (ioparm.blank != NULL)
    {
      if (u == NULL)
	p = undefined;
      else
	switch (u->flags.blank)
	  {
	  case BLANK_NULL:
          p = "NULL";
	    break;
	  case BLANK_ZERO:
	    p = "ZERO";
	    break;
	  default:
	    internal_error ("inquire_via_unit(): Bad blank");
	  }

      cf_strcpy (ioparm.blank, ioparm.blank_len, p);
    }

  if (ioparm.position != NULL)
    {
      if (u == NULL || u->flags.access == ACCESS_DIRECT)
        p = undefined;
      else
        switch (u->flags.position)
          {
             case POSITION_REWIND:
               p = "REWIND";
               break;
             case POSITION_APPEND:
               p = "APPEND";
               break;
             case POSITION_ASIS:
               p = "ASIS";
               break;
             default:
               /* if not direct access, it must be
                  either REWIND, APPEND, or ASIS.
                  ASIS seems to be the best default */
               p = "ASIS";
               break;
          }
      cf_strcpy (ioparm.position, ioparm.position_len, p);
    }

  if (ioparm.action != NULL)
    {
      if (u == NULL)
	p = undefined;
      else
	switch (u->flags.action)
	  {
	  case ACTION_READ:
	    p = "READ";
	    break;
	  case ACTION_WRITE:
	    p = "WRITE";
	    break;
	  case ACTION_READWRITE:
	    p = "READWRITE";
	    break;
	  default:
	    internal_error ("inquire_via_unit(): Bad action");
	  }

      cf_strcpy (ioparm.action, ioparm.action_len, p);
    }

  if (ioparm.read != NULL)
    {
      p = (u == NULL) ? inquire_read (NULL, 0) :
	inquire_read (u->file, u->file_len);

      cf_strcpy (ioparm.read, ioparm.read_len, p);
    }

  if (ioparm.write != NULL)
    {
      p = (u == NULL) ? inquire_write (NULL, 0) :
	inquire_write (u->file, u->file_len);

      cf_strcpy (ioparm.write, ioparm.write_len, p);
    }

  if (ioparm.readwrite != NULL)
    {
      p = (u == NULL) ? inquire_readwrite (NULL, 0) :
	inquire_readwrite (u->file, u->file_len);

      cf_strcpy (ioparm.readwrite, ioparm.readwrite_len, p);
    }

  if (ioparm.delim != NULL)
    {
      if (u == NULL || u->flags.form != FORM_FORMATTED)
	p = undefined;
      else
	switch (u->flags.delim)
	  {
	  case DELIM_NONE:
	    p = "NONE";
	    break;
	  case DELIM_QUOTE:
	    p = "QUOTE";
	    break;
	  case DELIM_APOSTROPHE:
	    p = "APOSTROPHE";
	    break;
	  default:
	    internal_error ("inquire_via_unit(): Bad delim");
	  }

      cf_strcpy (ioparm.delim, ioparm.delim_len, p);
    }

  if (ioparm.pad != NULL)
    {
      if (u == NULL || u->flags.form != FORM_FORMATTED)
	p = undefined;
      else
	switch (u->flags.pad)
	  {
	  case PAD_NO:
	    p = "NO";
	    break;
	  case PAD_YES:
	    p = "YES";
	    break;
	  default:
	    internal_error ("inquire_via_unit(): Bad pad");
	  }

      cf_strcpy (ioparm.pad, ioparm.pad_len, p);
    }
}


/* inquire_via_filename()-- Inquiry via filename.  This subroutine is
 * only used if the filename is *not* connected to a unit number. */

static void
inquire_via_filename (void)
{
  const char *p;

  if (ioparm.exist != NULL)
    *ioparm.exist = file_exists ();

  if (ioparm.opened != NULL)
    *ioparm.opened = 0;

  if (ioparm.number != NULL)
    *ioparm.number = -1;

  if (ioparm.named != NULL)
    *ioparm.named = 1;

  if (ioparm.name != NULL)
    fstrcpy (ioparm.name, ioparm.name_len, ioparm.file, ioparm.file_len);

  if (ioparm.access != NULL)
    cf_strcpy (ioparm.access, ioparm.access_len, undefined);

  if (ioparm.sequential != NULL)
    {
      p = inquire_sequential (ioparm.file, ioparm.file_len);
      cf_strcpy (ioparm.sequential, ioparm.sequential_len, p);
    }

  if (ioparm.direct != NULL)
    {
      p = inquire_direct (ioparm.file, ioparm.file_len);
      cf_strcpy (ioparm.direct, ioparm.direct_len, p);
    }

  if (ioparm.form != NULL)
    cf_strcpy (ioparm.form, ioparm.form_len, undefined);

  if (ioparm.formatted != NULL)
    {
      p = inquire_formatted (ioparm.file, ioparm.file_len);
      cf_strcpy (ioparm.formatted, ioparm.formatted_len, p);
    }

  if (ioparm.unformatted != NULL)
    {
      p = inquire_unformatted (ioparm.file, ioparm.file_len);
      cf_strcpy (ioparm.unformatted, ioparm.unformatted_len, p);
    }

  if (ioparm.recl_out != NULL)
    *ioparm.recl_out = 0;

  if (ioparm.nextrec != NULL)
    *ioparm.nextrec = 0;

  if (ioparm.blank != NULL)
    cf_strcpy (ioparm.blank, ioparm.blank_len, undefined);

  if (ioparm.position != NULL)
    cf_strcpy (ioparm.position, ioparm.position_len, undefined);

  if (ioparm.access != NULL)
    cf_strcpy (ioparm.access, ioparm.access_len, undefined);

  if (ioparm.read != NULL)
    {
      p = inquire_read (ioparm.file, ioparm.file_len);
      cf_strcpy (ioparm.read, ioparm.read_len, p);
    }

  if (ioparm.write != NULL)
    {
      p = inquire_write (ioparm.file, ioparm.file_len);
      cf_strcpy (ioparm.write, ioparm.write_len, p);
    }

  if (ioparm.readwrite != NULL)
    {
      p = inquire_read (ioparm.file, ioparm.file_len);
      cf_strcpy (ioparm.readwrite, ioparm.readwrite_len, p);
    }

  if (ioparm.delim != NULL)
    cf_strcpy (ioparm.delim, ioparm.delim_len, undefined);

  if (ioparm.pad != NULL)
    cf_strcpy (ioparm.pad, ioparm.pad_len, undefined);

}


/* Library entry point for the INQUIRE statement (non-IOLENGTH
   form).  */

extern void st_inquire (void);
export_proto(st_inquire);

void
st_inquire (void)
{
  gfc_unit *u;

  library_start ();

  if (ioparm.file == NULL)
    inquire_via_unit (find_unit (ioparm.unit));
  else
    {
      u = find_file ();
      if (u == NULL)
	inquire_via_filename ();
      else
	inquire_via_unit (u);
    }

  library_end ();
}
