/* Copyright (C) 2002-2021 Free Software Foundation, Inc.
   Contributed by Andy Vaught
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
#include "async.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <string.h>
#include <errno.h>


static const st_option access_opt[] = {
  {"sequential", ACCESS_SEQUENTIAL},
  {"direct", ACCESS_DIRECT},
  {"append", ACCESS_APPEND},
  {"stream", ACCESS_STREAM},
  {NULL, 0}
};

static const st_option action_opt[] =
{
  { "read", ACTION_READ},
  { "write", ACTION_WRITE},
  { "readwrite", ACTION_READWRITE},
  { NULL, 0}
};

static const st_option share_opt[] =
{
  { "denyrw", SHARE_DENYRW },
  { "denynone", SHARE_DENYNONE },
  { NULL, 0}
};

static const st_option cc_opt[] =
{
  { "list", CC_LIST },
  { "fortran", CC_FORTRAN },
  { "none", CC_NONE },
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

static const st_option decimal_opt[] =
{
  { "point", DECIMAL_POINT},
  { "comma", DECIMAL_COMMA},
  { NULL, 0}
};

static const st_option encoding_opt[] =
{
  { "utf-8", ENCODING_UTF8},
  { "default", ENCODING_DEFAULT},
  { NULL, 0}
};

static const st_option round_opt[] =
{
  { "up", ROUND_UP},
  { "down", ROUND_DOWN},
  { "zero", ROUND_ZERO},
  { "nearest", ROUND_NEAREST},
  { "compatible", ROUND_COMPATIBLE},
  { "processor_defined", ROUND_PROCDEFINED},
  { NULL, 0}
};

static const st_option sign_opt[] =
{
  { "plus", SIGN_PLUS},
  { "suppress", SIGN_SUPPRESS},
  { "processor_defined", SIGN_PROCDEFINED},
  { NULL, 0}
};

static const st_option convert_opt[] =
{
  { "native", GFC_CONVERT_NATIVE},
  { "swap", GFC_CONVERT_SWAP},
  { "big_endian", GFC_CONVERT_BIG},
  { "little_endian", GFC_CONVERT_LITTLE},
  { NULL, 0}
};

static const st_option async_opt[] =
{
  { "yes", ASYNC_YES},
  { "no", ASYNC_NO},
  { NULL, 0}
};

/* Given a unit, test to see if the file is positioned at the terminal
   point, and if so, change state from NO_ENDFILE flag to AT_ENDFILE.
   This prevents us from changing the state from AFTER_ENDFILE to
   AT_ENDFILE.  */

static void
test_endfile (gfc_unit *u)
{
  if (u->endfile == NO_ENDFILE)
    { 
      gfc_offset sz = ssize (u->s);
      if (sz == 0 || sz == stell (u->s))
	u->endfile = AT_ENDFILE;
    }
}


/* Change the modes of a file, those that are allowed * to be
   changed.  */

static void
edit_modes (st_parameter_open *opp, gfc_unit *u, unit_flags *flags)
{
  /* Complain about attempts to change the unchangeable.  */

  if (flags->status != STATUS_UNSPECIFIED && flags->status != STATUS_OLD && 
      u->flags.status != flags->status)
    generate_error (&opp->common, LIBERROR_BAD_OPTION,
		    "Cannot change STATUS parameter in OPEN statement");

  if (flags->access != ACCESS_UNSPECIFIED && u->flags.access != flags->access)
    generate_error (&opp->common, LIBERROR_BAD_OPTION,
		    "Cannot change ACCESS parameter in OPEN statement");

  if (flags->form != FORM_UNSPECIFIED && u->flags.form != flags->form)
    generate_error (&opp->common, LIBERROR_BAD_OPTION,
		    "Cannot change FORM parameter in OPEN statement");

  if ((opp->common.flags & IOPARM_OPEN_HAS_RECL_IN)
      && opp->recl_in != u->recl)
    generate_error (&opp->common, LIBERROR_BAD_OPTION,
		    "Cannot change RECL parameter in OPEN statement");

  if (flags->action != ACTION_UNSPECIFIED && u->flags.action != flags->action)
    generate_error (&opp->common, LIBERROR_BAD_OPTION,
		    "Cannot change ACTION parameter in OPEN statement");

  if (flags->share != SHARE_UNSPECIFIED && u->flags.share != flags->share)
    generate_error (&opp->common, LIBERROR_BAD_OPTION,
		    "Cannot change SHARE parameter in OPEN statement");

  if (flags->cc != CC_UNSPECIFIED && u->flags.cc != flags->cc)
    generate_error (&opp->common, LIBERROR_BAD_OPTION,
		  "Cannot change CARRIAGECONTROL parameter in OPEN statement");

  /* Status must be OLD if present.  */

  if (flags->status != STATUS_UNSPECIFIED && flags->status != STATUS_OLD &&
      flags->status != STATUS_UNKNOWN)
    {
      if (flags->status == STATUS_SCRATCH)
	notify_std (&opp->common, GFC_STD_GNU,
		    "OPEN statement must have a STATUS of OLD or UNKNOWN");
      else
	generate_error (&opp->common, LIBERROR_BAD_OPTION,
		    "OPEN statement must have a STATUS of OLD or UNKNOWN");
    }

  if (u->flags.form == FORM_UNFORMATTED)
    {
      if (flags->delim != DELIM_UNSPECIFIED)
	generate_error (&opp->common, LIBERROR_OPTION_CONFLICT,
			"DELIM parameter conflicts with UNFORMATTED form in "
			"OPEN statement");

      if (flags->blank != BLANK_UNSPECIFIED)
	generate_error (&opp->common, LIBERROR_OPTION_CONFLICT,
			"BLANK parameter conflicts with UNFORMATTED form in "
			"OPEN statement");

      if (flags->pad != PAD_UNSPECIFIED)
	generate_error (&opp->common, LIBERROR_OPTION_CONFLICT,
			"PAD parameter conflicts with UNFORMATTED form in "
			"OPEN statement");

      if (flags->decimal != DECIMAL_UNSPECIFIED)
	generate_error (&opp->common, LIBERROR_OPTION_CONFLICT,
			"DECIMAL parameter conflicts with UNFORMATTED form in "
			"OPEN statement");

      if (flags->encoding != ENCODING_UNSPECIFIED)
	generate_error (&opp->common, LIBERROR_OPTION_CONFLICT,
			"ENCODING parameter conflicts with UNFORMATTED form in "
			"OPEN statement");

      if (flags->round != ROUND_UNSPECIFIED)
	generate_error (&opp->common, LIBERROR_OPTION_CONFLICT,
			"ROUND parameter conflicts with UNFORMATTED form in "
			"OPEN statement");

      if (flags->sign != SIGN_UNSPECIFIED)
	generate_error (&opp->common, LIBERROR_OPTION_CONFLICT,
			"SIGN parameter conflicts with UNFORMATTED form in "
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
      if (flags->decimal != DECIMAL_UNSPECIFIED)
	u->flags.decimal = flags->decimal;
      if (flags->encoding != ENCODING_UNSPECIFIED)
	u->flags.encoding = flags->encoding;
      if (flags->async != ASYNC_UNSPECIFIED)
	u->flags.async = flags->async;
      if (flags->round != ROUND_UNSPECIFIED)
	u->flags.round = flags->round;
      if (flags->sign != SIGN_UNSPECIFIED)
	u->flags.sign = flags->sign;

      /* Reposition the file if necessary.  */
    
      switch (flags->position)
	{
	case POSITION_UNSPECIFIED:
	case POSITION_ASIS:
	  break;
    
	case POSITION_REWIND:
	  if (sseek (u->s, 0, SEEK_SET) != 0)
	    goto seek_error;
    
	  u->current_record = 0;
	  u->last_record = 0;
    
	  test_endfile (u);
	  break;
    
	case POSITION_APPEND:
	  if (sseek (u->s, 0, SEEK_END) < 0)
	    goto seek_error;
    
	  if (flags->access != ACCESS_STREAM)
	    u->current_record = 0;
    
	  u->endfile = AT_ENDFILE;	/* We are at the end.  */
	  break;
    
	seek_error:
	  generate_error (&opp->common, LIBERROR_OS, NULL);
	  break;
	}
    }

  unlock_unit (u);
}


/* Open an unused unit.  */

gfc_unit *
new_unit (st_parameter_open *opp, gfc_unit *u, unit_flags *flags)
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

  if (flags->async == ASYNC_UNSPECIFIED)
    flags->async = ASYNC_NO;

  if (flags->status == STATUS_UNSPECIFIED)
    flags->status = STATUS_UNKNOWN;

  if (flags->cc == CC_UNSPECIFIED)
    flags->cc = flags->form == FORM_UNFORMATTED ? CC_NONE : CC_LIST;
  else if (flags->form == FORM_UNFORMATTED && flags->cc != CC_NONE)
    {
      generate_error (&opp->common, LIBERROR_OPTION_CONFLICT,
	  "CARRIAGECONTROL parameter conflicts with UNFORMATTED form in "
	  "OPEN statement");
      goto fail;
    }

  /* Checks.  */

  if (flags->delim != DELIM_UNSPECIFIED
      && flags->form == FORM_UNFORMATTED)
    {
      generate_error (&opp->common, LIBERROR_OPTION_CONFLICT,
		      "DELIM parameter conflicts with UNFORMATTED form in "
		      "OPEN statement");
      goto fail;
    }

  if (flags->blank == BLANK_UNSPECIFIED)
    flags->blank = BLANK_NULL;
  else
    {
      if (flags->form == FORM_UNFORMATTED)
	{
	  generate_error (&opp->common, LIBERROR_OPTION_CONFLICT,
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
	  generate_error (&opp->common, LIBERROR_OPTION_CONFLICT,
			  "PAD parameter conflicts with UNFORMATTED form in "
			  "OPEN statement");
	  goto fail;
	}
    }

  if (flags->decimal == DECIMAL_UNSPECIFIED)
    flags->decimal = DECIMAL_POINT;
  else
    {
      if (flags->form == FORM_UNFORMATTED)
	{
	  generate_error (&opp->common, LIBERROR_OPTION_CONFLICT,
			  "DECIMAL parameter conflicts with UNFORMATTED form "
			  "in OPEN statement");
	  goto fail;
	}
    }

  if (flags->encoding == ENCODING_UNSPECIFIED)
    flags->encoding = ENCODING_DEFAULT;
  else
    {
      if (flags->form == FORM_UNFORMATTED)
	{
	  generate_error (&opp->common, LIBERROR_OPTION_CONFLICT,
			  "ENCODING parameter conflicts with UNFORMATTED form in "
			  "OPEN statement");
	  goto fail;
	}
    }

  /* NB: the value for ROUND when it's not specified by the user does not
         have to be PROCESSOR_DEFINED; the standard says that it is
	 processor dependent, and requires that it is one of the
	 possible value (see F2003, 9.4.5.13).  */
  if (flags->round == ROUND_UNSPECIFIED)
    flags->round = ROUND_PROCDEFINED;
  else
    {
      if (flags->form == FORM_UNFORMATTED)
	{
	  generate_error (&opp->common, LIBERROR_OPTION_CONFLICT,
			  "ROUND parameter conflicts with UNFORMATTED form in "
			  "OPEN statement");
	  goto fail;
	}
    }

  if (flags->sign == SIGN_UNSPECIFIED)
    flags->sign = SIGN_PROCDEFINED;
  else
    {
      if (flags->form == FORM_UNFORMATTED)
	{
	  generate_error (&opp->common, LIBERROR_OPTION_CONFLICT,
			  "SIGN parameter conflicts with UNFORMATTED form in "
			  "OPEN statement");
	  goto fail;
	}
    }

  if (flags->position != POSITION_ASIS && flags->access == ACCESS_DIRECT)
   {
     generate_error (&opp->common, LIBERROR_OPTION_CONFLICT,
                     "ACCESS parameter conflicts with SEQUENTIAL access in "
                     "OPEN statement");
     goto fail;
   }
  else
   if (flags->position == POSITION_UNSPECIFIED)
     flags->position = POSITION_ASIS;

  if (flags->access == ACCESS_DIRECT
      && (opp->common.flags & IOPARM_OPEN_HAS_RECL_IN) == 0)
    {
      generate_error (&opp->common, LIBERROR_MISSING_OPTION,
		      "Missing RECL parameter in OPEN statement");
      goto fail;
    }

  if ((opp->common.flags & IOPARM_OPEN_HAS_RECL_IN) && opp->recl_in <= 0)
    {
      generate_error (&opp->common, LIBERROR_BAD_OPTION,
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

      generate_error (&opp->common, LIBERROR_BAD_OPTION,
		      "FILE parameter must not be present in OPEN statement");
      goto fail;

    case STATUS_OLD:
    case STATUS_NEW:
    case STATUS_REPLACE:
    case STATUS_UNKNOWN:
      if ((opp->common.flags & IOPARM_OPEN_HAS_FILE))
	break;

      opp->file = tmpname;
      opp->file_len = snprintf(opp->file, sizeof (tmpname), "fort.%d", 
			       (int) opp->common.unit);
      break;

    default:
      internal_error (&opp->common, "new_unit(): Bad status");
    }

  /* Make sure the file isn't already open someplace else.
     Do not error if opening file preconnected to stdin, stdout, stderr.  */

  u2 = NULL;
  if ((opp->common.flags & IOPARM_OPEN_HAS_FILE) != 0
      && !(compile_options.allow_std & GFC_STD_F2018))
    u2 = find_file (opp->file, opp->file_len);
  if (u2 != NULL
      && (options.stdin_unit < 0 || u2->unit_number != options.stdin_unit)
      && (options.stdout_unit < 0 || u2->unit_number != options.stdout_unit)
      && (options.stderr_unit < 0 || u2->unit_number != options.stderr_unit))
    {
      unlock_unit (u2);
      generate_error (&opp->common, LIBERROR_ALREADY_OPEN, NULL);
      goto cleanup;
    }

  if (u2 != NULL)
    unlock_unit (u2);

  /* If the unit specified is preconnected with a file specified to be open,
     then clear the format buffer.  */
  if ((opp->common.unit == options.stdin_unit ||
       opp->common.unit == options.stdout_unit ||
       opp->common.unit == options.stderr_unit)
      && (opp->common.flags & IOPARM_OPEN_HAS_FILE) != 0)
    fbuf_destroy (u);

  /* Open file.  */

  s = open_external (opp, flags);
  if (s == NULL)
    {
      char errbuf[256];
      char *path = fc_strdup (opp->file, opp->file_len);
      size_t msglen = opp->file_len + 22 + sizeof (errbuf);
      char *msg = xmalloc (msglen);
      snprintf (msg, msglen, "Cannot open file '%s': %s", path,
		gf_strerror (errno, errbuf, sizeof (errbuf)));
      generate_error (&opp->common, LIBERROR_OS, msg);
      free (msg);
      free (path);
      goto cleanup;
    }

  if (flags->status == STATUS_NEW || flags->status == STATUS_REPLACE)
    flags->status = STATUS_OLD;

  /* Create the unit structure.  */

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
  u->saved_pos = 0;

  if (flags->position == POSITION_APPEND)
    {
      if (sseek (u->s, 0, SEEK_END) < 0)
	{
	  generate_error (&opp->common, LIBERROR_OS, NULL);
	  goto cleanup;
	}
      u->endfile = AT_ENDFILE;
    }

  /* Unspecified recl ends up with a processor dependent value.  */

  if ((opp->common.flags & IOPARM_OPEN_HAS_RECL_IN))
    {
      u->flags.has_recl = 1;
      u->recl = opp->recl_in;
      u->recl_subrecord = u->recl;
      u->bytes_left = u->recl;
    }
  else
    {
      u->flags.has_recl = 0;
      u->recl = default_recl;
      if (compile_options.max_subrecord_length)
	{
	  u->recl_subrecord = compile_options.max_subrecord_length;
	}
      else
	{
	  switch (compile_options.record_marker)
	    {
	    case 0:
	      /* Fall through */
	    case sizeof (GFC_INTEGER_4):
	      u->recl_subrecord = GFC_MAX_SUBRECORD_LENGTH;
	      break;

	    case sizeof (GFC_INTEGER_8):
	      u->recl_subrecord = max_offset - 16;
	      break;

	    default:
	      runtime_error ("Illegal value for record marker");
	      break;
	    }
	}
    }

  /* If the file is direct access, calculate the maximum record number
     via a division now instead of letting the multiplication overflow
     later.  */

  if (flags->access == ACCESS_DIRECT)
    u->maxrec = max_offset / u->recl;
  
  if (flags->access == ACCESS_STREAM)
    {
      u->maxrec = max_offset;
      /* F2018 (N2137) 12.10.2.26: If the connection is for stream
	 access recl is assigned the value -2.  */
      u->recl = -2;
      u->bytes_left = 1;
      u->strm_pos = stell (u->s) + 1;
    }

  u->filename = fc_strdup (opp->file, opp->file_len);

  /* Curiously, the standard requires that the
     position specifier be ignored for new files so a newly connected
     file starts out at the initial point.  We still need to figure
     out if the file is at the end or not.  */

  test_endfile (u);

  if (flags->status == STATUS_SCRATCH && opp->file != NULL)
    free (opp->file);
    
  if (flags->form == FORM_FORMATTED)
    {
      if ((opp->common.flags & IOPARM_OPEN_HAS_RECL_IN))
        fbuf_init (u, u->recl);
      else
        fbuf_init (u, 0);
    }
  else
    u->fbuf = NULL;

  /* Check if asynchrounous.  */
  if (flags->async == ASYNC_YES)
    init_async_unit (u);
  else
    u->au = NULL;

  return u;

 cleanup:

  /* Free memory associated with a temporary filename.  */

  if (flags->status == STATUS_SCRATCH && opp->file != NULL)
    free (opp->file);

 fail:

  close_unit (u);
  return NULL;
}


/* Open a unit which is already open.  This involves changing the
   modes or closing what is there now and opening the new file.  */

static void
already_open (st_parameter_open *opp, gfc_unit *u, unit_flags *flags)
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
      if (sclose (u->s) == -1)
	{
	  unlock_unit (u);
	  generate_error (&opp->common, LIBERROR_OS,
			  "Error closing file in OPEN statement");
	  return;
	}

      u->s = NULL;
 
#if !HAVE_UNLINK_OPEN_FILE
      if (u->filename && u->flags.status == STATUS_SCRATCH)
	remove (u->filename);
#endif
      free (u->filename);
      u->filename = NULL;
      
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
  unit_convert conv;
 
  library_start (&opp->common);

  /* Decode options.  */
  flags.readonly = !(cf & IOPARM_OPEN_HAS_READONLY) ? 0 : opp->readonly;

  flags.access = !(cf & IOPARM_OPEN_HAS_ACCESS) ? ACCESS_UNSPECIFIED :
    find_option (&opp->common, opp->access, opp->access_len,
		 access_opt, "Bad ACCESS parameter in OPEN statement");

  flags.action = !(cf & IOPARM_OPEN_HAS_ACTION) ? ACTION_UNSPECIFIED :
    find_option (&opp->common, opp->action, opp->action_len,
		 action_opt, "Bad ACTION parameter in OPEN statement");

  flags.cc = !(cf & IOPARM_OPEN_HAS_CC) ? CC_UNSPECIFIED :
    find_option (&opp->common, opp->cc, opp->cc_len,
		 cc_opt, "Bad CARRIAGECONTROL parameter in OPEN statement");

  flags.share = !(cf & IOPARM_OPEN_HAS_SHARE) ? SHARE_UNSPECIFIED :
    find_option (&opp->common, opp->share, opp->share_len,
		 share_opt, "Bad SHARE parameter in OPEN statement");

  flags.blank = !(cf & IOPARM_OPEN_HAS_BLANK) ? BLANK_UNSPECIFIED :
    find_option (&opp->common, opp->blank, opp->blank_len,
		 blank_opt, "Bad BLANK parameter in OPEN statement");

  flags.delim = !(cf & IOPARM_OPEN_HAS_DELIM) ? DELIM_UNSPECIFIED :
    find_option (&opp->common, opp->delim, opp->delim_len,
		 delim_opt, "Bad DELIM parameter in OPEN statement");

  flags.pad = !(cf & IOPARM_OPEN_HAS_PAD) ? PAD_UNSPECIFIED :
    find_option (&opp->common, opp->pad, opp->pad_len,
		 pad_opt, "Bad PAD parameter in OPEN statement");

  flags.decimal = !(cf & IOPARM_OPEN_HAS_DECIMAL) ? DECIMAL_UNSPECIFIED :
    find_option (&opp->common, opp->decimal, opp->decimal_len,
		 decimal_opt, "Bad DECIMAL parameter in OPEN statement");

  flags.encoding = !(cf & IOPARM_OPEN_HAS_ENCODING) ? ENCODING_UNSPECIFIED :
    find_option (&opp->common, opp->encoding, opp->encoding_len,
		 encoding_opt, "Bad ENCODING parameter in OPEN statement");

  flags.async = !(cf & IOPARM_OPEN_HAS_ASYNCHRONOUS) ? ASYNC_UNSPECIFIED :
    find_option (&opp->common, opp->asynchronous, opp->asynchronous_len,
		 async_opt, "Bad ASYNCHRONOUS parameter in OPEN statement");

  flags.round = !(cf & IOPARM_OPEN_HAS_ROUND) ? ROUND_UNSPECIFIED :
    find_option (&opp->common, opp->round, opp->round_len,
		 round_opt, "Bad ROUND parameter in OPEN statement");

  flags.sign = !(cf & IOPARM_OPEN_HAS_SIGN) ? SIGN_UNSPECIFIED :
    find_option (&opp->common, opp->sign, opp->sign_len,
		 sign_opt, "Bad SIGN parameter in OPEN statement");

  flags.form = !(cf & IOPARM_OPEN_HAS_FORM) ? FORM_UNSPECIFIED :
    find_option (&opp->common, opp->form, opp->form_len,
		 form_opt, "Bad FORM parameter in OPEN statement");

  flags.position = !(cf & IOPARM_OPEN_HAS_POSITION) ? POSITION_UNSPECIFIED :
    find_option (&opp->common, opp->position, opp->position_len,
		 position_opt, "Bad POSITION parameter in OPEN statement");

  flags.status = !(cf & IOPARM_OPEN_HAS_STATUS) ? STATUS_UNSPECIFIED :
    find_option (&opp->common, opp->status, opp->status_len,
		 status_opt, "Bad STATUS parameter in OPEN statement");

  /* First, we check wether the convert flag has been set via environment
     variable.  This overrides the convert tag in the open statement.  */

  conv = get_unformatted_convert (opp->common.unit);

  if (conv == GFC_CONVERT_NONE)
    {
      /* Nothing has been set by environment variable, check the convert tag.  */
      if (cf & IOPARM_OPEN_HAS_CONVERT)
	conv = find_option (&opp->common, opp->convert, opp->convert_len,
			    convert_opt,
			    "Bad CONVERT parameter in OPEN statement");
      else
	conv = compile_options.convert;
    }
  
  switch (conv)
    {
    case GFC_CONVERT_NATIVE:
    case GFC_CONVERT_SWAP:
      break;
      
    case GFC_CONVERT_BIG:
      conv = __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__ ? GFC_CONVERT_NATIVE : GFC_CONVERT_SWAP;
      break;
      
    case GFC_CONVERT_LITTLE:
      conv = __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__ ? GFC_CONVERT_SWAP : GFC_CONVERT_NATIVE;
      break;
      
    default:
      internal_error (&opp->common, "Illegal value for CONVERT");
      break;
    }

  flags.convert = conv;

  if (flags.position != POSITION_UNSPECIFIED
      && flags.access == ACCESS_DIRECT)
    generate_error (&opp->common, LIBERROR_BAD_OPTION,
		    "Cannot use POSITION with direct access files");

  if (flags.readonly
      && flags.action != ACTION_UNSPECIFIED && flags.action != ACTION_READ)
    generate_error (&opp->common, LIBERROR_BAD_OPTION,
		    "ACTION conflicts with READONLY in OPEN statement");

  if (flags.access == ACCESS_APPEND)
    {
      if (flags.position != POSITION_UNSPECIFIED
	  && flags.position != POSITION_APPEND)
	generate_error (&opp->common, LIBERROR_BAD_OPTION,
			"Conflicting ACCESS and POSITION flags in"
			" OPEN statement");

      notify_std (&opp->common, GFC_STD_GNU,
		  "Extension: APPEND as a value for ACCESS in OPEN statement");
      flags.access = ACCESS_SEQUENTIAL;
      flags.position = POSITION_APPEND;
    }

  if (flags.position == POSITION_UNSPECIFIED)
    flags.position = POSITION_ASIS;

  if ((opp->common.flags & IOPARM_LIBRETURN_MASK) == IOPARM_LIBRETURN_OK)
    {
      if ((opp->common.flags & IOPARM_OPEN_HAS_NEWUNIT))
	opp->common.unit = newunit_alloc ();
      else if (opp->common.unit < 0)
	{
	  u = find_unit (opp->common.unit);
	  if (u == NULL) /* Negative unit and no NEWUNIT-created unit found.  */
	    {
	      generate_error (&opp->common, LIBERROR_BAD_OPTION,
			      "Bad unit number in OPEN statement");
	      library_end ();
	      return;
	    }
	}

      if (u == NULL)
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
    
  if ((opp->common.flags & IOPARM_OPEN_HAS_NEWUNIT)
      && (opp->common.flags & IOPARM_LIBRETURN_MASK) == IOPARM_LIBRETURN_OK)
    *opp->newunit = opp->common.unit;
  
  library_end ();
}
