/* Copyright (C) 2002, 2003, 2005, 2007 Free Software Foundation, Inc.
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

#include "io.h"


static const char undefined[] = "UNDEFINED";


/* inquire_via_unit()-- Inquiry via unit number.  The unit might not exist. */

static void
inquire_via_unit (st_parameter_inquire *iqp, gfc_unit * u)
{
  const char *p;
  GFC_INTEGER_4 cf = iqp->common.flags;

  if ((cf & IOPARM_INQUIRE_HAS_EXIST) != 0)
    {
      *iqp->exist = (iqp->common.unit >= 0
		     && iqp->common.unit <= GFC_INTEGER_4_HUGE);

      if ((cf & IOPARM_INQUIRE_HAS_FILE) == 0)
	{
	  if (!(*iqp->exist))
	    *iqp->common.iostat = LIBERROR_BAD_UNIT;
	  *iqp->exist = *iqp->exist
			&& (*iqp->common.iostat != LIBERROR_BAD_UNIT);
	}
    }

  if ((cf & IOPARM_INQUIRE_HAS_OPENED) != 0)
    *iqp->opened = (u != NULL);

  if ((cf & IOPARM_INQUIRE_HAS_NUMBER) != 0)
    *iqp->number = (u != NULL) ? u->unit_number : -1;

  if ((cf & IOPARM_INQUIRE_HAS_NAMED) != 0)
    *iqp->named = (u != NULL && u->flags.status != STATUS_SCRATCH);

  if ((cf & IOPARM_INQUIRE_HAS_NAME) != 0
      && u != NULL && u->flags.status != STATUS_SCRATCH)
    fstrcpy (iqp->name, iqp->name_len, u->file, u->file_len);

  if ((cf & IOPARM_INQUIRE_HAS_ACCESS) != 0)
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
	  case ACCESS_STREAM:
	    p = "STREAM";
	    break;
	  default:
	    internal_error (&iqp->common, "inquire_via_unit(): Bad access");
	  }

      cf_strcpy (iqp->access, iqp->access_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_SEQUENTIAL) != 0)
    {
      if (u == NULL)
	p = inquire_sequential (NULL, 0);
      else
	switch (u->flags.access)
	  {
	  case ACCESS_DIRECT:
	  case ACCESS_STREAM:
	    p = "NO";
	    break;
	  case ACCESS_SEQUENTIAL:
	    p = "YES";
	    break;
	  default:
	    internal_error (&iqp->common, "inquire_via_unit(): Bad access");
	  }

      cf_strcpy (iqp->sequential, iqp->sequential_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_DIRECT) != 0)
    {
      if (u == NULL)
	p = inquire_direct (NULL, 0);
      else
	switch (u->flags.access)
	  {
	  case ACCESS_SEQUENTIAL:
	  case ACCESS_STREAM:
	    p = "NO";
	    break;
	  case ACCESS_DIRECT:
	    p = "YES";
	    break;
	  default:
	    internal_error (&iqp->common, "inquire_via_unit(): Bad access");
	  }

      cf_strcpy (iqp->direct, iqp->direct_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_FORM) != 0)
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
	    internal_error (&iqp->common, "inquire_via_unit(): Bad form");
	  }

      cf_strcpy (iqp->form, iqp->form_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_FORMATTED) != 0)
    {
      if (u == NULL)
	p = inquire_formatted (NULL, 0);
      else
	switch (u->flags.form)
	  {
	  case FORM_FORMATTED:
	    p = "YES";
	    break;
	  case FORM_UNFORMATTED:
	    p = "NO";
	    break;
	  default:
	    internal_error (&iqp->common, "inquire_via_unit(): Bad form");
	  }

      cf_strcpy (iqp->formatted, iqp->formatted_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_UNFORMATTED) != 0)
    {
      if (u == NULL)
	p = inquire_unformatted (NULL, 0);
      else
	switch (u->flags.form)
	  {
	  case FORM_FORMATTED:
	    p = "NO";
	    break;
	  case FORM_UNFORMATTED:
	    p = "YES";
	    break;
	  default:
	    internal_error (&iqp->common, "inquire_via_unit(): Bad form");
	  }

      cf_strcpy (iqp->unformatted, iqp->unformatted_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_RECL_OUT) != 0)
    *iqp->recl_out = (u != NULL) ? u->recl : 0;

  if ((cf & IOPARM_INQUIRE_HAS_STRM_POS_OUT) != 0)
    *iqp->strm_pos_out = (u != NULL) ? u->strm_pos : 0;

  if ((cf & IOPARM_INQUIRE_HAS_NEXTREC) != 0)
    {
      /* This only makes sense in the context of DIRECT access.  */
      if (u != NULL && u->flags.access == ACCESS_DIRECT)
	*iqp->nextrec = u->last_record + 1;
      else
	*iqp->nextrec = 0;
    }

  if ((cf & IOPARM_INQUIRE_HAS_BLANK) != 0)
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
	    internal_error (&iqp->common, "inquire_via_unit(): Bad blank");
	  }

      cf_strcpy (iqp->blank, iqp->blank_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_POSITION) != 0)
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
      cf_strcpy (iqp->position, iqp->position_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_ACTION) != 0)
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
	    internal_error (&iqp->common, "inquire_via_unit(): Bad action");
	  }

      cf_strcpy (iqp->action, iqp->action_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_READ) != 0)
    {
      p = (u == NULL) ? inquire_read (NULL, 0) :
	inquire_read (u->file, u->file_len);

      cf_strcpy (iqp->read, iqp->read_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_WRITE) != 0)
    {
      p = (u == NULL) ? inquire_write (NULL, 0) :
	inquire_write (u->file, u->file_len);

      cf_strcpy (iqp->write, iqp->write_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_READWRITE) != 0)
    {
      p = (u == NULL) ? inquire_readwrite (NULL, 0) :
	inquire_readwrite (u->file, u->file_len);

      cf_strcpy (iqp->readwrite, iqp->readwrite_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_DELIM) != 0)
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
	    internal_error (&iqp->common, "inquire_via_unit(): Bad delim");
	  }

      cf_strcpy (iqp->delim, iqp->delim_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_PAD) != 0)
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
	    internal_error (&iqp->common, "inquire_via_unit(): Bad pad");
	  }

      cf_strcpy (iqp->pad, iqp->pad_len, p);
    }
 
  if ((cf & IOPARM_INQUIRE_HAS_CONVERT) != 0)
    {
      if (u == NULL)
	p = undefined;
      else
	switch (u->flags.convert)
	  {
	    /*  l8_to_l4_offset is 0 for little-endian, 1 for big-endian.  */
	  case GFC_CONVERT_NATIVE:
	    p = l8_to_l4_offset ? "BIG_ENDIAN" : "LITTLE_ENDIAN";
	    break;

	  case GFC_CONVERT_SWAP:
	    p = l8_to_l4_offset ? "LITTLE_ENDIAN" : "BIG_ENDIAN";
	    break;

	  default:
	    internal_error (&iqp->common, "inquire_via_unit(): Bad convert");
	  }

      cf_strcpy (iqp->convert, iqp->convert_len, p);
    }
}


/* inquire_via_filename()-- Inquiry via filename.  This subroutine is
 * only used if the filename is *not* connected to a unit number. */

static void
inquire_via_filename (st_parameter_inquire *iqp)
{
  const char *p;
  GFC_INTEGER_4 cf = iqp->common.flags;

  if ((cf & IOPARM_INQUIRE_HAS_EXIST) != 0)
    *iqp->exist = file_exists (iqp->file, iqp->file_len);

  if ((cf & IOPARM_INQUIRE_HAS_OPENED) != 0)
    *iqp->opened = 0;

  if ((cf & IOPARM_INQUIRE_HAS_NUMBER) != 0)
    *iqp->number = -1;

  if ((cf & IOPARM_INQUIRE_HAS_NAMED) != 0)
    *iqp->named = 1;

  if ((cf & IOPARM_INQUIRE_HAS_NAME) != 0)
    fstrcpy (iqp->name, iqp->name_len, iqp->file, iqp->file_len);

  if ((cf & IOPARM_INQUIRE_HAS_ACCESS) != 0)
    cf_strcpy (iqp->access, iqp->access_len, undefined);

  if ((cf & IOPARM_INQUIRE_HAS_SEQUENTIAL) != 0)
    {
      p = "UNKNOWN";
      cf_strcpy (iqp->sequential, iqp->sequential_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_DIRECT) != 0)
    {
      p = "UNKNOWN";
      cf_strcpy (iqp->direct, iqp->direct_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_FORM) != 0)
    cf_strcpy (iqp->form, iqp->form_len, undefined);

  if ((cf & IOPARM_INQUIRE_HAS_FORMATTED) != 0)
    {
      p = "UNKNOWN";
      cf_strcpy (iqp->formatted, iqp->formatted_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_UNFORMATTED) != 0)
    {
      p = "UNKNOWN";
      cf_strcpy (iqp->unformatted, iqp->unformatted_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_RECL_OUT) != 0)
    *iqp->recl_out = 0;

  if ((cf & IOPARM_INQUIRE_HAS_NEXTREC) != 0)
    *iqp->nextrec = 0;

  if ((cf & IOPARM_INQUIRE_HAS_BLANK) != 0)
    cf_strcpy (iqp->blank, iqp->blank_len, undefined);

  if ((cf & IOPARM_INQUIRE_HAS_POSITION) != 0)
    cf_strcpy (iqp->position, iqp->position_len, undefined);

  if ((cf & IOPARM_INQUIRE_HAS_ACCESS) != 0)
    cf_strcpy (iqp->access, iqp->access_len, undefined);

  if ((cf & IOPARM_INQUIRE_HAS_READ) != 0)
    {
      p = inquire_read (iqp->file, iqp->file_len);
      cf_strcpy (iqp->read, iqp->read_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_WRITE) != 0)
    {
      p = inquire_write (iqp->file, iqp->file_len);
      cf_strcpy (iqp->write, iqp->write_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_READWRITE) != 0)
    {
      p = inquire_read (iqp->file, iqp->file_len);
      cf_strcpy (iqp->readwrite, iqp->readwrite_len, p);
    }

  if ((cf & IOPARM_INQUIRE_HAS_DELIM) != 0)
    cf_strcpy (iqp->delim, iqp->delim_len, undefined);

  if ((cf & IOPARM_INQUIRE_HAS_PAD) != 0)
    cf_strcpy (iqp->pad, iqp->pad_len, undefined);
}


/* Library entry point for the INQUIRE statement (non-IOLENGTH
   form).  */

extern void st_inquire (st_parameter_inquire *);
export_proto(st_inquire);

void
st_inquire (st_parameter_inquire *iqp)
{
  gfc_unit *u;

  library_start (&iqp->common);

  if ((iqp->common.flags & IOPARM_INQUIRE_HAS_FILE) == 0)
    {
      u = find_unit (iqp->common.unit);
      inquire_via_unit (iqp, u);
    }
  else
    {
      u = find_file (iqp->file, iqp->file_len);
      if (u == NULL)
	inquire_via_filename (iqp);
      else
	inquire_via_unit (iqp, u);
    }
  if (u != NULL)
    unlock_unit (u);

  library_end ();
}
