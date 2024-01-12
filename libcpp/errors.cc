/* Default error handlers for CPP Library.
   Copyright (C) 1986-2024 Free Software Foundation, Inc.
   Written by Per Bothner, 1994.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "internal.h"

/* Get a location_t for the current location in PFILE,
   generally that of the previously lexed token.  */

location_t
cpp_diagnostic_get_current_location (cpp_reader *pfile)
{
  if (CPP_OPTION (pfile, traditional))
    {
      if (pfile->state.in_directive)
	return pfile->directive_line;
      else
	return pfile->line_table->highest_line;
    }
  /* We don't want to refer to a token before the beginning of the
     current run -- that is invalid.  */
  else if (pfile->cur_token == pfile->cur_run->base)
    {
      return 0;
    }
  else
    {
      return pfile->cur_token[-1].src_loc;
    }
}

/* Print a diagnostic at the given location.  */

ATTRIBUTE_CPP_PPDIAG (5, 0)
static bool
cpp_diagnostic_at (cpp_reader * pfile, enum cpp_diagnostic_level level,
		   enum cpp_warning_reason reason, rich_location *richloc,
		   const char *msgid, va_list *ap)
{
  bool ret;

  if (!pfile->cb.diagnostic)
    abort ();
  ret = pfile->cb.diagnostic (pfile, level, reason, richloc, _(msgid), ap);

  return ret;
}

/* Print a diagnostic at the location of the previously lexed token.  */

ATTRIBUTE_CPP_PPDIAG (4, 0)
static bool
cpp_diagnostic (cpp_reader * pfile, enum cpp_diagnostic_level level,
		enum cpp_warning_reason reason,
		const char *msgid, va_list *ap)
{
  location_t src_loc = cpp_diagnostic_get_current_location (pfile);
  rich_location richloc (pfile->line_table, src_loc);
  return cpp_diagnostic_at (pfile, level, reason, &richloc, msgid, ap);
}

/* Print a warning or error, depending on the value of LEVEL.  */

bool
cpp_error (cpp_reader * pfile, enum cpp_diagnostic_level level,
	   const char *msgid, ...)
{
  va_list ap;
  bool ret;

  va_start (ap, msgid);

  ret = cpp_diagnostic (pfile, level, CPP_W_NONE, msgid, &ap);

  va_end (ap);
  return ret;
}

/* Print a warning.  The warning reason may be given in REASON.  */

bool
cpp_warning (cpp_reader * pfile, enum cpp_warning_reason reason,
	     const char *msgid, ...)
{
  va_list ap;
  bool ret;

  va_start (ap, msgid);

  ret = cpp_diagnostic (pfile, CPP_DL_WARNING, reason, msgid, &ap);

  va_end (ap);
  return ret;
}

/* Print a pedantic warning.  The warning reason may be given in REASON.  */

bool
cpp_pedwarning (cpp_reader * pfile, enum cpp_warning_reason reason,
		const char *msgid, ...)
{
  va_list ap;
  bool ret;

  va_start (ap, msgid);

  ret = cpp_diagnostic (pfile, CPP_DL_PEDWARN, reason, msgid, &ap);

  va_end (ap);
  return ret;
}

/* Print a warning, including system headers.  The warning reason may be
   given in REASON.  */

bool
cpp_warning_syshdr (cpp_reader * pfile, enum cpp_warning_reason reason,
		    const char *msgid, ...)
{
  va_list ap;
  bool ret;

  va_start (ap, msgid);

  ret = cpp_diagnostic (pfile, CPP_DL_WARNING_SYSHDR, reason, msgid, &ap);

  va_end (ap);
  return ret;
}

/* As cpp_warning above, but use RICHLOC as the location of the diagnostic.  */

bool cpp_warning_at (cpp_reader *pfile, enum cpp_warning_reason reason,
		     rich_location *richloc, const char *msgid, ...)
{
  va_list ap;
  bool ret;

  va_start (ap, msgid);

  ret = cpp_diagnostic_at (pfile, CPP_DL_WARNING, reason, richloc,
			   msgid, &ap);

  va_end (ap);
  return ret;

}

/* As cpp_pedwarning above, but use RICHLOC as the location of the
   diagnostic.  */

bool
cpp_pedwarning_at (cpp_reader * pfile, enum cpp_warning_reason reason,
		   rich_location *richloc, const char *msgid, ...)
{
  va_list ap;
  bool ret;

  va_start (ap, msgid);

  ret = cpp_diagnostic_at (pfile, CPP_DL_PEDWARN, reason, richloc,
			   msgid, &ap);

  va_end (ap);
  return ret;
}

/* Print a diagnostic at a specific location.  */

ATTRIBUTE_CPP_PPDIAG (6, 0)
static bool
cpp_diagnostic_with_line (cpp_reader * pfile, enum cpp_diagnostic_level level,
			  enum cpp_warning_reason reason,
			  location_t src_loc, unsigned int column,
			  const char *msgid, va_list *ap)
{
  bool ret;
  
  if (!pfile->cb.diagnostic)
    abort ();
  rich_location richloc (pfile->line_table, src_loc);
  if (column)
    richloc.override_column (column);
  ret = pfile->cb.diagnostic (pfile, level, reason, &richloc, _(msgid), ap);

  return ret;
}

/* Print a warning or error, depending on the value of LEVEL.  */

bool
cpp_error_with_line (cpp_reader *pfile, enum cpp_diagnostic_level level,
		     location_t src_loc, unsigned int column,
		     const char *msgid, ...)
{
  va_list ap;
  bool ret;

  va_start (ap, msgid);

  ret = cpp_diagnostic_with_line (pfile, level, CPP_W_NONE, src_loc,
                                  column, msgid, &ap);

  va_end (ap);
  return ret;
}

/* Print a warning.  The warning reason may be given in REASON.  */

bool
cpp_warning_with_line (cpp_reader *pfile, enum cpp_warning_reason reason,
		       location_t src_loc, unsigned int column,
		       const char *msgid, ...)
{
  va_list ap;
  bool ret;

  va_start (ap, msgid);

  ret = cpp_diagnostic_with_line (pfile, CPP_DL_WARNING, reason, src_loc,
                                  column, msgid, &ap);

  va_end (ap);
  return ret;
}

/* Print a pedantic warning.  The warning reason may be given in REASON.  */

bool
cpp_pedwarning_with_line (cpp_reader *pfile, enum cpp_warning_reason reason,
			  location_t src_loc, unsigned int column,
			  const char *msgid, ...)
{
  va_list ap;
  bool ret;

  va_start (ap, msgid);

  ret = cpp_diagnostic_with_line (pfile, CPP_DL_PEDWARN, reason, src_loc,
                                  column, msgid, &ap);

  va_end (ap);
  return ret;
}

/* Print a warning, including system headers.  The warning reason may be
   given in REASON.  */

bool
cpp_warning_with_line_syshdr (cpp_reader *pfile, enum cpp_warning_reason reason,
			      location_t src_loc, unsigned int column,
			      const char *msgid, ...)
{
  va_list ap;
  bool ret;

  va_start (ap, msgid);

  ret = cpp_diagnostic_with_line (pfile, CPP_DL_WARNING_SYSHDR, reason, src_loc,
                                  column, msgid, &ap);

  va_end (ap);
  return ret;
}

/* As cpp_error, but use SRC_LOC as the location of the error, without
   a column override.  */

bool
cpp_error_at (cpp_reader * pfile, enum cpp_diagnostic_level level,
	      location_t src_loc, const char *msgid, ...)
{
  va_list ap;
  bool ret;

  va_start (ap, msgid);

  rich_location richloc (pfile->line_table, src_loc);
  ret = cpp_diagnostic_at (pfile, level, CPP_W_NONE, &richloc,
			   msgid, &ap);

  va_end (ap);
  return ret;
}

/* As cpp_error, but use RICHLOC as the location of the error, without
   a column override.  */

bool
cpp_error_at (cpp_reader * pfile, enum cpp_diagnostic_level level,
	      rich_location *richloc, const char *msgid, ...)
{
  va_list ap;
  bool ret;

  va_start (ap, msgid);

  ret = cpp_diagnostic_at (pfile, level, CPP_W_NONE, richloc,
			   msgid, &ap);

  va_end (ap);
  return ret;
}

/* Print a warning or error, depending on the value of LEVEL.  Include
   information from errno.  */

bool
cpp_errno (cpp_reader *pfile, enum cpp_diagnostic_level level,
	   const char *msgid)
{
  return cpp_error (pfile, level, "%s: %s", _(msgid), xstrerror (errno));
}

/* Print a warning or error, depending on the value of LEVEL.  Include
   information from errno.  Unlike cpp_errno, the argument is a filename
   that is not localized, but "" is replaced with localized "stdout".  */

bool
cpp_errno_filename (cpp_reader *pfile, enum cpp_diagnostic_level level,
		    const char *filename,
		    location_t loc)
{
  if (filename[0] == '\0')
    filename = _("stdout");

  return cpp_error_at (pfile, level, loc, "%s: %s", filename,
		       xstrerror (errno));
}

cpp_auto_suppress_diagnostics::cpp_auto_suppress_diagnostics (cpp_reader *pfile)
  : m_pfile (pfile), m_cb (pfile->cb.diagnostic)
{
  m_pfile->cb.diagnostic
    = [] (cpp_reader *, cpp_diagnostic_level, cpp_warning_reason,
	  rich_location *, const char *, va_list *)
    {
      return true;
    };
}

cpp_auto_suppress_diagnostics::~cpp_auto_suppress_diagnostics ()
{
  m_pfile->cb.diagnostic = m_cb;
}
