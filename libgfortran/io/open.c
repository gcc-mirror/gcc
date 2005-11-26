/* Copyright (C) 2002, 2003, 2004, 2005
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

#include "config.h"
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "libgfortran.h"
#include "io.h"


static const st_option access_opt[] = {
  {"sequential", ACCESS_SEQUENTIAL},
  {"direct", ACCESS_DIRECT},
  {"append", ACCESS_APPEND},
  {NULL, 0}
};

static const st_option action_opt[] =
{
  { "read", ACTION_READ},
  { "write", ACTION_WRITE},
  { "readwrite", ACTION_READWRITE},
  { NULL, 0}
};

static const st_option blank_opt[] =
{
  { "null", BLANK_NULL},
  { "zero", BLANK_ZERO},
  { NULL, 0}
};

static const st_option delim_opt[] =
{
  { "none", DELIM_NONE},
  { "apostrophe", DELIM_APOSTROPHE},
  { "quote", DELIM_QUOTE},
  { NULL, 0}
};

static const st_option form_opt[] =
{
  { "formatted", FORM_FORMATTED},
  { "unformatted", FORM_UNFORMATTED},
  { NULL, 0}
};

static const st_option position_opt[] =
{
  { "asis", POSITION_ASIS},
  { "rewind", POSITION_REWIND},
  { "append", POSITION_APPEND},
  { NULL, 0}
};

static const st_option status_opt[] =
{
  { "unknown", STATUS_UNKNOWN},
  { "old", STATUS_OLD},
  { "new", STATUS_NEW},
  { "replace", STATUS_REPLACE},
  { "scratch", STATUS_SCRATCH},
  { NULL, 0}
};

static const st_option pad_opt[] =
{
  { "yes", PAD_YES},
  { "no", PAD_NO},
  { NULL, 0}
};


/* Given a unit, test to see if the file is positioned at the terminal
   point, and if so, change state from NO_ENDFILE flag to AT_ENDFILE.
   This prevents us from changing the state from AFTER_ENDFILE to
   AT_ENDFILE.  */

void
test_endfile (gfc_unit * u)
{
  if (u->endfile == NO_ENDFILE && file_length (u->s) == file_position (u->s))
    u->endfile = AT_ENDFILE;
}


/* Change the modes of a file, those that are allowed * to be
   changed.  */

static void
edit_modes (st_parameter_open *opp, gfc_unit * u, unit_flags * flags)
{
  /* Complain about attempts to change the unchangeable.  */

  if (flags->status != STATUS_UNSPECIFIED &&
      u->flags.status != flags->status)
    generate_error (&opp->common, ERROR_BAD_OPTION,
		    "Cannot change STATUS parameter in OPEN statement");

  if (flags->access != ACCESS_UNSPECIFIED && u->flags.access != flags->access)
    generate_error (&opp->common, ERROR_BAD_OPTION,
		    "Cannot change ACCESS parameter in OPEN statement");

  if (flags->form != FORM_UNSPECIFIED && u->flags.form != flags->form)
    generate_error (&opp->common, ERROR_BAD_OPTION,
		    "Cannot change FORM parameter in OPEN statement");

  if ((opp->common.flags & IOPARM_OPEN_HAS_RECL_IN)
      && opp->recl_in != u->recl)
    generate_error (&opp->common, ERROR_BAD_OPTION,
		    "Cannot change RECL parameter in OPEN statement");

  if (flags->action != ACTION_UNSPECIFIED && u->flags.access != flags->access)
    generate_error (&opp->common, ERROR_BAD_OPTION,
		    "Cannot change ACTION parameter in OPEN statement");

  /* Status must be OLD if present.  */

  if (flags->status != STATUS_UNSPECIFIED && flags->status != STATUS_OLD &&
      flags->status != STATUS_UNKNOWN)
    generate_error (&opp->common, ERROR_BAD_OPTION,
		    "OPEN statement must have a STATUS of OLD or UNKNOWN");

  if (u->flags.form == FORM_UNFORMATTED)
    {
      if (flags->delim != DELIM_UNSPECIFIED)
	generate_error (&opp->common, ERROR_OPTION_CONFLICT,
			"DELIM parameter conflicts with UNFORMATTED form in "
			"OPEN statement");

      if (flags->blank != BLANK_UNSPECIFIED)
	generate_error (&opp->common, ERROR_OPTION_CONFLICT,
			"BLANK parameter conflicts with UNFORMATTED form in "
			"OPEN statement");

      if (flags->pad != PAD_UNSPECIFIED)
	generate_error (&opp->common, ERROR_OPTION_CONFLICT,
			"PAD paramter conflicts with UNFORMATTED form in "
			"OPEN statement");
    }

  if ((opp->common.flags & IOPARM_LIBRETURN_MASK) == IOPARM_LIBRETURN_OK)
    {
      /* Change the changeable:  */
      if (flags->blank != BLANK_UNSPECIFIED)
	u->flags.blank = flags->blank;
      if (flags->delim != DELIM_UNSPECIFIED)
	u->flags.delim = flags->delim;
      if (flags->pad != PAD_UNSPECIFIED)
	u->flags.pad = flags->pad;
    }

  /* Reposition the file if necessary.  */

  switch (flags->position)
    {
    case POSITION_UNSPECIFIED:
    case POSITION_ASIS:
      break;

    case POSITION_REWIND:
      if (sseek (u->s, 0) == FAILURE)
	goto seek_error;

      u->current_record = 0;
      u->last_record = 0;

      test_endfile (u);		/* We might be at the end.  */
      break;

    case POSITION_APPEND:
      if (sseek (u->s, file_length (u->s)) == FAILURE)
	goto seek_error;

      u->current_record = 0;
      u->endfile = AT_ENDFILE;	/* We are at the end.  */
      break;

    seek_error:
      generate_error (&opp->common, ERROR_OS, NULL);
      break;
    }

  unlock_unit (u);
}


/* Open an unused unit.  */

gfc_unit *
new_unit (st_parameter_open *opp, gfc_unit *u, unit_flags * flags)
{
  gfc_unit *u2;
  stream *s;
  char tmpname[5 /* fort. */ + 10 /* digits of unit number */ + 1 /* 0 */];

  /* Change unspecifieds to defaults.  Leave (flags->action ==
     ACTION_UNSPECIFIED) alone so open_external() can set it based on
     what type of open actually works.  */

  if (flags->access == ACCESS_UNSPECIFIED)
    flags->access = ACCESS_SEQUENTIAL;

  if (flags->form == FORM_UNSPECIFIED)
    flags->form = (flags->access == ACCESS_SEQUENTIAL)
      ? FORM_FORMATTED : FORM_UNFORMATTED;


  if (flags->delim == DELIM_UNSPECIFIED)
    flags->delim = DELIM_NONE;
  else
    {
      if (flags->form == FORM_UNFORMATTED)
	{
	  generate_error (&opp->common, ERROR_OPTION_CONFLICT,
			  "DELIM parameter conflicts with UNFORMATTED form in "
			  "OPEN statement");
	  goto fail;
	}
    }

  if (flags->blank == BLANK_UNSPECIFIED)
    flags->blank = BLANK_NULL;
  else
    {
      if (flags->form == FORM_UNFORMATTED)
	{
	  generate_error (&opp->common, ERROR_OPTION_CONFLICT,
			  "BLANK parameter conflicts with UNFORMATTED form in "
			  "OPEN statement");
	  goto fail;
	}
    }

  if (flags->pad == PAD_UNSPECIFIED)
    flags->pad = PAD_YES;
  else
    {
      if (flags->form == FORM_UNFORMATTED)
	{
	  generate_error (&opp->common, ERROR_OPTION_CONFLICT,
			  "PAD paramter conflicts with UNFORMATTED form in "
			  "OPEN statement");
	  goto fail;
	}
    }

  if (flags->position != POSITION_ASIS && flags->access == ACCESS_DIRECT)
   {
     generate_error (&opp->common, ERROR_OPTION_CONFLICT,
                     "ACCESS parameter conflicts with SEQUENTIAL access in "
                     "OPEN statement");
     goto fail;
   }
  else
   if (flags->position == POSITION_UNSPECIFIED)
     flags->position = POSITION_ASIS;


  if (flags->status == STATUS_UNSPECIFIED)
    flags->status = STATUS_UNKNOWN;

  /* Checks.  */

  if (flags->access == ACCESS_DIRECT
      && (opp->common.flags & IOPARM_OPEN_HAS_RECL_IN) == 0)
    {
      generate_error (&opp->common, ERROR_MISSING_OPTION,
		      "Missing RECL parameter in OPEN statement");
      goto fail;
    }

  if ((opp->common.flags & IOPARM_OPEN_HAS_RECL_IN) && opp->recl_in <= 0)
    {
      generate_error (&opp->common, ERROR_BAD_OPTION,
		      "RECL parameter is non-positive in OPEN statement");
      goto fail;
    }

  switch (flags->status)
    {
    case STATUS_SCRATCH:
      if ((opp->common.flags & IOPARM_OPEN_HAS_FILE) == 0)
	{
	  opp->file = NULL;
	  break;
	}

      generate_error (&opp->common, ERROR_BAD_OPTION,
		      "FILE parameter must not be present in OPEN statement");
      goto fail;

    case STATUS_OLD:
    case STATUS_NEW:
    case STATUS_REPLACE:
    case STATUS_UNKNOWN:
      if ((opp->common.flags & IOPARM_OPEN_HAS_FILE))
	break;

      opp->file = tmpname;
      opp->file_len = sprintf(opp->file, "fort.%d", opp->common.unit);
      break;

    default:
      internal_error (&opp->common, "new_unit(): Bad status");
    }

  /* Make sure the file isn't already open someplace else.
     Do not error if opening file preconnected to stdin, stdout, stderr.  */

  u2 = NULL;
  if ((opp->common.flags & IOPARM_OPEN_HAS_FILE) != 0)
    u2 = find_file (opp->file, opp->file_len);
  if (u2 != NULL
      && (options.stdin_unit < 0 || u2->unit_number != options.stdin_unit)
      && (options.stdout_unit < 0 || u2->unit_number != options.stdout_unit)
      && (options.stderr_unit < 0 || u2->unit_number != options.stderr_unit))
    {
      unlock_unit (u2);
      generate_error (&opp->common, ERROR_ALREADY_OPEN, NULL);
      goto cleanup;
    }

  if (u2 != NULL)
    unlock_unit (u2);

  /* Open file.  */

  s = open_external (opp, flags);
  if (s == NULL)
    {
      generate_error (&opp->common, ERROR_OS, NULL);
      goto cleanup;
    }

  if (flags->status == STATUS_NEW || flags->status == STATUS_REPLACE)
    flags->status = STATUS_OLD;

  /* Create the unit structure.  */

  u->file = get_mem (opp->file_len);
  if (u->unit_number != opp->common.unit)
    internal_error (&opp->common, "Unit number changed");
  u->s = s;
  u->flags = *flags;
  u->read_bad = 0;
  u->endfile = NO_ENDFILE;
  u->last_record = 0;
  u->current_record = 0;
  u->mode = READING;
  u->maxrec = 0;
  u->bytes_left = 0;

  if (flags->position == POSITION_APPEND)
    {
      if (sseek (u->s, file_length (u->s)) == FAILURE)
	generate_error (&opp->common, ERROR_OS, NULL);
      u->endfile = AT_ENDFILE;
    }

  /* Unspecified recl ends up with a processor dependent value.  */

  if ((opp->common.flags & IOPARM_OPEN_HAS_RECL_IN))
    u->recl = opp->recl_in;
  else
    u->recl = max_offset;

  /* If the file is direct access, calculate the maximum record number
     via a division now instead of letting the multiplication overflow
     later.  */

  if (flags->access == ACCESS_DIRECT)
    u->maxrec = max_offset / u->recl;

  memmove (u->file, opp->file, opp->file_len);
  u->file_len = opp->file_len;

  /* Curiously, the standard requires that the
     position specifier be ignored for new files so a newly connected
     file starts out that the initial point.  We still need to figure
     out if the file is at the end or not.  */

  test_endfile (u);

  if (flags->status == STATUS_SCRATCH && opp->file != NULL)
    free_mem (opp->file);
  return u;

 cleanup:

  /* Free memory associated with a temporary filename.  */

  if (flags->status == STATUS_SCRATCH && opp->file != NULL)
    free_mem (opp->file);

 fail:

  close_unit (u);
  return NULL;
}


/* Open a unit which is already open.  This involves changing the
   modes or closing what is there now and opening the new file.  */

static void
already_open (st_parameter_open *opp, gfc_unit * u, unit_flags * flags)
{
  if ((opp->common.flags & IOPARM_OPEN_HAS_FILE) == 0)
    {
      edit_modes (opp, u, flags);
      return;
    }

  /* If the file is connected to something else, close it and open a
     new unit.  */

  if (!compare_file_filename (u, opp->file, opp->file_len))
    {
#if !HAVE_UNLINK_OPEN_FILE
      char *path = NULL;
      if (u->file && u->flags.status == STATUS_SCRATCH)
	{
	  path = (char *) gfc_alloca (u->file_len + 1);
	  unpack_filename (path, u->file, u->file_len);
	}
#endif

      if (sclose (u->s) == FAILURE)
	{
	  unlock_unit (u);
	  generate_error (&opp->common, ERROR_OS,
			  "Error closing file in OPEN statement");
	  return;
	}

      u->s = NULL;
      if (u->file)
	free_mem (u->file);
      u->file = NULL;
      u->file_len = 0;

#if !HAVE_UNLINK_OPEN_FILE
      if (path != NULL)
	unlink (path);
#endif

      u = new_unit (opp, u, flags);
      if (u != NULL)
	unlock_unit (u);
      return;
    }

  edit_modes (opp, u, flags);
}


/* Open file.  */

extern void st_open (st_parameter_open *opp);
export_proto(st_open);

void
st_open (st_parameter_open *opp)
{
  unit_flags flags;
  gfc_unit *u = NULL;
  GFC_INTEGER_4 cf = opp->common.flags;
 
  library_start (&opp->common);

  /* Decode options.  */

  flags.access = !(cf & IOPARM_OPEN_HAS_ACCESS) ? ACCESS_UNSPECIFIED :
    find_option (&opp->common, opp->access, opp->access_len,
		 access_opt, "Bad ACCESS parameter in OPEN statement");

  flags.action = !(cf & IOPARM_OPEN_HAS_ACTION) ? ACTION_UNSPECIFIED :
    find_option (&opp->common, opp->action, opp->action_len,
		 action_opt, "Bad ACTION parameter in OPEN statement");

  flags.blank = !(cf & IOPARM_OPEN_HAS_BLANK) ? BLANK_UNSPECIFIED :
    find_option (&opp->common, opp->blank, opp->blank_len,
		 blank_opt, "Bad BLANK parameter in OPEN statement");

  flags.delim = !(cf & IOPARM_OPEN_HAS_DELIM) ? DELIM_UNSPECIFIED :
    find_option (&opp->common, opp->delim, opp->delim_len,
		 delim_opt, "Bad DELIM parameter in OPEN statement");

  flags.pad = !(cf & IOPARM_OPEN_HAS_PAD) ? PAD_UNSPECIFIED :
    find_option (&opp->common, opp->pad, opp->pad_len,
		 pad_opt, "Bad PAD parameter in OPEN statement");

  flags.form = !(cf & IOPARM_OPEN_HAS_FORM) ? FORM_UNSPECIFIED :
    find_option (&opp->common, opp->form, opp->form_len,
		 form_opt, "Bad FORM parameter in OPEN statement");

  flags.position = !(cf & IOPARM_OPEN_HAS_POSITION) ? POSITION_UNSPECIFIED :
    find_option (&opp->common, opp->position, opp->position_len,
		 position_opt, "Bad POSITION parameter in OPEN statement");

  flags.status = !(cf & IOPARM_OPEN_HAS_STATUS) ? STATUS_UNSPECIFIED :
    find_option (&opp->common, opp->status, opp->status_len,
		 status_opt, "Bad STATUS parameter in OPEN statement");

  if (opp->common.unit < 0)
    generate_error (&opp->common, ERROR_BAD_OPTION,
		    "Bad unit number in OPEN statement");

  if (flags.position != POSITION_UNSPECIFIED
      && flags.access == ACCESS_DIRECT)
    generate_error (&opp->common, ERROR_BAD_OPTION,
		    "Cannot use POSITION with direct access files");

  if (flags.access == ACCESS_APPEND)
    {
      if (flags.position != POSITION_UNSPECIFIED
	  && flags.position != POSITION_APPEND)
	generate_error (&opp->common, ERROR_BAD_OPTION,
			"Conflicting ACCESS and POSITION flags in"
			" OPEN statement");

      notify_std (GFC_STD_GNU,
		  "Extension: APPEND as a value for ACCESS in OPEN statement");
      flags.access = ACCESS_SEQUENTIAL;
      flags.position = POSITION_APPEND;
    }

  if (flags.position == POSITION_UNSPECIFIED)
    flags.position = POSITION_ASIS;

  if ((opp->common.flags & IOPARM_LIBRETURN_MASK) == IOPARM_LIBRETURN_OK)
    {
      u = find_or_create_unit (opp->common.unit);

      if (u->s == NULL)
	{
	  u = new_unit (opp, u, &flags);
	  if (u != NULL)
	    unlock_unit (u);
	}
      else
	already_open (opp, u, &flags);
    }

  library_end ();
}
