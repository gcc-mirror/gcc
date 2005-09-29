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

#include "config.h"
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "libgfortran.h"
#include "io.h"


static const st_option access_opt[] = {
  {"sequential", ACCESS_SEQUENTIAL},
  {"direct", ACCESS_DIRECT},
  {NULL}
};

static const st_option action_opt[] =
{
  { "read", ACTION_READ},
  { "write", ACTION_WRITE},
  { "readwrite", ACTION_READWRITE},
  { NULL}
};

static const st_option blank_opt[] =
{
  { "null", BLANK_NULL},
  { "zero", BLANK_ZERO},
  { NULL}
};

static const st_option delim_opt[] =
{
  { "none", DELIM_NONE},
  { "apostrophe", DELIM_APOSTROPHE},
  { "quote", DELIM_QUOTE},
  { NULL}
};

static const st_option form_opt[] =
{
  { "formatted", FORM_FORMATTED},
  { "unformatted", FORM_UNFORMATTED},
  { NULL}
};

static const st_option position_opt[] =
{
  { "asis", POSITION_ASIS},
  { "rewind", POSITION_REWIND},
  { "append", POSITION_APPEND},
  { NULL}
};

static const st_option status_opt[] =
{
  { "unknown", STATUS_UNKNOWN},
  { "old", STATUS_OLD},
  { "new", STATUS_NEW},
  { "replace", STATUS_REPLACE},
  { "scratch", STATUS_SCRATCH},
  { NULL}
};

static const st_option pad_opt[] =
{
  { "yes", PAD_YES},
  { "no", PAD_NO},
  { NULL}
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
edit_modes (gfc_unit * u, unit_flags * flags)
{
  /* Complain about attempts to change the unchangeable.  */

  if (flags->status != STATUS_UNSPECIFIED &&
      u->flags.status != flags->position)
    generate_error (ERROR_BAD_OPTION,
		    "Cannot change STATUS parameter in OPEN statement");

  if (flags->access != ACCESS_UNSPECIFIED && u->flags.access != flags->access)
    generate_error (ERROR_BAD_OPTION,
		    "Cannot change ACCESS parameter in OPEN statement");

  if (flags->form != FORM_UNSPECIFIED && u->flags.form != flags->form)
    generate_error (ERROR_BAD_OPTION,
		    "Cannot change FORM parameter in OPEN statement");

  if (ioparm.recl_in != 0 && ioparm.recl_in != u->recl)
    generate_error (ERROR_BAD_OPTION,
		    "Cannot change RECL parameter in OPEN statement");

  if (flags->action != ACTION_UNSPECIFIED && u->flags.access != flags->access)
    generate_error (ERROR_BAD_OPTION,
		    "Cannot change ACTION parameter in OPEN statement");

  /* Status must be OLD if present.  */

  if (flags->status != STATUS_UNSPECIFIED && flags->status != STATUS_OLD)
    generate_error (ERROR_BAD_OPTION,
		    "OPEN statement must have a STATUS of OLD");

  if (u->flags.form == FORM_UNFORMATTED)
    {
      if (flags->delim != DELIM_UNSPECIFIED)
	generate_error (ERROR_OPTION_CONFLICT,
			"DELIM parameter conflicts with UNFORMATTED form in "
			"OPEN statement");

      if (flags->blank != BLANK_UNSPECIFIED)
	generate_error (ERROR_OPTION_CONFLICT,
			"BLANK parameter conflicts with UNFORMATTED form in "
			"OPEN statement");

      if (flags->pad != PAD_UNSPECIFIED)
	generate_error (ERROR_OPTION_CONFLICT,
			"PAD paramter conflicts with UNFORMATTED form in "
			"OPEN statement");
    }

  if (ioparm.library_return == LIBRARY_OK)
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
      generate_error (ERROR_OS, NULL);
      break;
    }
}


/* Open an unused unit.  */

void
new_unit (unit_flags * flags)
{
  gfc_unit *u;
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
	  generate_error (ERROR_OPTION_CONFLICT,
			  "DELIM parameter conflicts with UNFORMATTED form in "
			  "OPEN statement");
	  goto cleanup;
	}
    }

  if (flags->blank == BLANK_UNSPECIFIED)
    flags->blank = BLANK_NULL;
  else
    {
      if (flags->form == FORM_UNFORMATTED)
	{
	  generate_error (ERROR_OPTION_CONFLICT,
			  "BLANK parameter conflicts with UNFORMATTED form in "
			  "OPEN statement");
	  goto cleanup;
	}
    }

  if (flags->pad == PAD_UNSPECIFIED)
    flags->pad = PAD_YES;
  else
    {
      if (flags->form == FORM_UNFORMATTED)
	{
	  generate_error (ERROR_OPTION_CONFLICT,
			  "PAD paramter conflicts with UNFORMATTED form in "
			  "OPEN statement");
	  goto cleanup;
	}
    }

  if (flags->position != POSITION_ASIS && flags->access == ACCESS_DIRECT)
   {
     generate_error (ERROR_OPTION_CONFLICT,
                     "ACCESS parameter conflicts with SEQUENTIAL access in "
                     "OPEN statement");
     goto cleanup;
   }
  else
   if (flags->position == POSITION_UNSPECIFIED)
     flags->position = POSITION_ASIS;


  if (flags->status == STATUS_UNSPECIFIED)
    flags->status = STATUS_UNKNOWN;

  /* Checks.  */

  if (flags->access == ACCESS_DIRECT && ioparm.recl_in == 0)
    {
      generate_error (ERROR_MISSING_OPTION,
		      "Missing RECL parameter in OPEN statement");
      goto cleanup;
    }

  if (ioparm.recl_in != 0 && ioparm.recl_in <= 0)
    {
      generate_error (ERROR_BAD_OPTION,
		      "RECL parameter is non-positive in OPEN statement");
      goto cleanup;
    }

  switch (flags->status)
    {
    case STATUS_SCRATCH:
      if (ioparm.file == NULL)
	break;

      generate_error (ERROR_BAD_OPTION,
		      "FILE parameter must not be present in OPEN statement");
      return;

    case STATUS_OLD:
    case STATUS_NEW:
    case STATUS_REPLACE:
    case STATUS_UNKNOWN:
      if (ioparm.file != NULL)
	break;

      ioparm.file = tmpname;
      ioparm.file_len = sprintf(ioparm.file, "fort.%d", ioparm.unit);
      break;

    default:
      internal_error ("new_unit(): Bad status");
    }

  /* Make sure the file isn't already open someplace else.
     Do not error if opening file preconnected to stdin, stdout, stderr.  */

  u = find_file ();
  if (u != NULL
      && (options.stdin_unit < 0 || u->unit_number != options.stdin_unit)
      && (options.stdout_unit < 0 || u->unit_number != options.stdout_unit)
      && (options.stderr_unit < 0 || u->unit_number != options.stderr_unit))
    {
      generate_error (ERROR_ALREADY_OPEN, NULL);
      goto cleanup;
    }

  /* Open file.  */

  s = open_external (flags);
  if (s == NULL)
    {
      generate_error (ERROR_OS, NULL);
      goto cleanup;
    }

  if (flags->status == STATUS_NEW || flags->status == STATUS_REPLACE)
    flags->status = STATUS_OLD;

  /* Create the unit structure.  */

  u = get_mem (sizeof (gfc_unit) + ioparm.file_len);
  memset (u, '\0', sizeof (gfc_unit) + ioparm.file_len);

  u->unit_number = ioparm.unit;
  u->s = s;
  u->flags = *flags;

  if (flags->position == POSITION_APPEND)
  {
    if (sseek (u->s, file_length (u->s)) == FAILURE)
      generate_error (ERROR_OS, NULL);
    u->endfile = AT_ENDFILE;
  }

  /* Unspecified recl ends up with a processor dependent value.  */

  u->recl = (ioparm.recl_in != 0) ? ioparm.recl_in : g.max_offset;
  u->last_record = 0;
  u->current_record = 0;

  /* If the file is direct access, calculate the maximum record number
     via a division now instead of letting the multiplication overflow
     later.  */

  if (flags->access == ACCESS_DIRECT)
    u->maxrec = g.max_offset / u->recl;

  memmove (u->file, ioparm.file, ioparm.file_len);
  u->file_len = ioparm.file_len;

  insert_unit (u);

  /* The file is now connected.  Errors after this point leave the
     file connected.  Curiously, the standard requires that the
     position specifier be ignored for new files so a newly connected
     file starts out that the initial point.  We still need to figure
     out if the file is at the end or not.  */

  test_endfile (u);

 cleanup:

  /* Free memory associated with a temporary filename.  */

  if (flags->status == STATUS_SCRATCH)
    free_mem (ioparm.file);
}


/* Open a unit which is already open.  This involves changing the
   modes or closing what is there now and opening the new file.  */

static void
already_open (gfc_unit * u, unit_flags * flags)
{
  if (ioparm.file == NULL)
    {
      edit_modes (u, flags);
      return;
    }

  /* If the file is connected to something else, close it and open a
     new unit.  */

  if (!compare_file_filename (u->s, ioparm.file, ioparm.file_len))
    {
      if (close_unit (u))
	{
	  generate_error (ERROR_OS, "Error closing file in OPEN statement");
	  return;
	}

      new_unit (flags);
      return;
    }

  edit_modes (u, flags);
}


/* Open file.  */

extern void st_open (void);
export_proto(st_open);

void
st_open (void)
{
  unit_flags flags;
  gfc_unit *u = NULL;
 
  library_start ();

  /* Decode options.  */

  flags.access = (ioparm.access == NULL) ? ACCESS_UNSPECIFIED :
    find_option (ioparm.access, ioparm.access_len, access_opt,
		 "Bad ACCESS parameter in OPEN statement");

  flags.action = (ioparm.action == NULL) ? ACTION_UNSPECIFIED :
    find_option (ioparm.action, ioparm.action_len, action_opt,
		 "Bad ACTION parameter in OPEN statement");

  flags.blank = (ioparm.blank == NULL) ? BLANK_UNSPECIFIED :
    find_option (ioparm.blank, ioparm.blank_len, blank_opt,
		 "Bad BLANK parameter in OPEN statement");

  flags.delim = (ioparm.delim == NULL) ? DELIM_UNSPECIFIED :
    find_option (ioparm.delim, ioparm.delim_len, delim_opt,
		 "Bad DELIM parameter in OPEN statement");

  flags.pad = (ioparm.pad == NULL) ? PAD_UNSPECIFIED :
    find_option (ioparm.pad, ioparm.pad_len, pad_opt,
		 "Bad PAD parameter in OPEN statement");

  flags.form = (ioparm.form == NULL) ? FORM_UNSPECIFIED :
    find_option (ioparm.form, ioparm.form_len, form_opt,
		 "Bad FORM parameter in OPEN statement");

  flags.position = (ioparm.position == NULL) ? POSITION_UNSPECIFIED :
    find_option (ioparm.position, ioparm.position_len, position_opt,
		 "Bad POSITION parameter in OPEN statement");

  flags.status = (ioparm.status == NULL) ? STATUS_UNSPECIFIED :
    find_option (ioparm.status, ioparm.status_len, status_opt,
		 "Bad STATUS parameter in OPEN statement");

  if (ioparm.unit < 0)
    generate_error (ERROR_BAD_OPTION, "Bad unit number in OPEN statement");

  if (flags.position != POSITION_UNSPECIFIED
      && flags.access == ACCESS_DIRECT)
    generate_error (ERROR_BAD_OPTION,
		    "Cannot use POSITION with direct access files");

  if (flags.position == POSITION_UNSPECIFIED)
    flags.position = POSITION_ASIS;

  if (ioparm.library_return != LIBRARY_OK)
  {
    library_end ();
    return;
  }

  u = find_unit (ioparm.unit);

  if (u == NULL)
    new_unit (&flags);
  else
    already_open (u, &flags);

  library_end ();
}
