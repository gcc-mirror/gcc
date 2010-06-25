/* Handling of compile-time options that influence the library.
   Copyright (C) 2005, 2007, 2009, 2010 Free Software Foundation, Inc.

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

#include "libgfortran.h"

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif


/* Useful compile-time options will be stored in here.  */
compile_options_t compile_options;


/* A signal handler to allow us to output a backtrace.  */
void
handler (int signum)
{
  const char * name = NULL, * desc = NULL;

  switch (signum)
    {
#if defined(SIGSEGV)
      case SIGSEGV:
	name = "SIGSEGV";
	desc = "Segmentation fault";
	break;
#endif

#if defined(SIGBUS)
      case SIGBUS:
	name = "SIGBUS";
	desc = "Bus error";
	break;
#endif

#if defined(SIGILL)
      case SIGILL:
	name = "SIGILL";
	desc = "Illegal instruction";
	break;
#endif

#if defined(SIGFPE)
      case SIGFPE:
	name = "SIGFPE";
	desc = "Floating-point exception";
	break;
#endif
    }

  if (name)
    st_printf ("\nProgram received signal %d (%s): %s.\n", signum, name, desc);
  else
    st_printf ("\nProgram received signal %d.\n", signum);

  sys_exit (5);
}


/* Set the usual compile-time options.  */
extern void set_options (int , int []);
export_proto(set_options);

void
set_options (int num, int options[])
{
  if (num >= 1)
    compile_options.warn_std = options[0];
  if (num >= 2)
    compile_options.allow_std = options[1];
  if (num >= 3)
    compile_options.pedantic = options[2];
  if (num >= 4)
    compile_options.dump_core = options[3];
  if (num >= 5)
    compile_options.backtrace = options[4];
  if (num >= 6)
    compile_options.sign_zero = options[5];
  if (num >= 7)
    compile_options.bounds_check = options[6];
  if (num >= 8)
    compile_options.range_check = options[7];

  /* If backtrace is required, we set signal handlers on most common
     signals.  */
#if defined(HAVE_SIGNAL) && (defined(SIGSEGV) || defined(SIGBUS) \
			     || defined(SIGILL) || defined(SIGFPE))
  if (compile_options.backtrace)
    {
#if defined(SIGSEGV)
      signal (SIGSEGV, handler);
#endif

#if defined(SIGBUS)
      signal (SIGBUS, handler);
#endif

#if defined(SIGILL)
      signal (SIGILL, handler);
#endif

#if defined(SIGFPE)
      signal (SIGFPE, handler);
#endif
    }
#endif

}


/* Default values for the compile-time options.  Keep in sync with
   gcc/fortran/options.c (gfc_init_options).  */
void
init_compile_options (void)
{
  compile_options.warn_std = GFC_STD_F95_DEL | GFC_STD_LEGACY;
  compile_options.allow_std = GFC_STD_F95_OBS | GFC_STD_F95_DEL
    | GFC_STD_F2003 | GFC_STD_F2008 | GFC_STD_F95 | GFC_STD_F77
    | GFC_STD_F2008_OBS | GFC_STD_GNU | GFC_STD_LEGACY;
  compile_options.pedantic = 0;
  compile_options.dump_core = 0;
  compile_options.backtrace = 0;
  compile_options.sign_zero = 1;
  compile_options.range_check = 1;
}

/* Function called by the front-end to tell us the
   default for unformatted data conversion.  */

extern void set_convert (int);
export_proto (set_convert);

void
set_convert (int conv)
{
  compile_options.convert = conv;
}

extern void set_record_marker (int);
export_proto (set_record_marker);


void
set_record_marker (int val)
{

  switch(val)
    {
    case 4:
      compile_options.record_marker = sizeof (GFC_INTEGER_4);
      break;

    case 8:
      compile_options.record_marker = sizeof (GFC_INTEGER_8);
      break;

    default:
      runtime_error ("Invalid value for record marker");
      break;
    }
}

extern void set_max_subrecord_length (int);
export_proto (set_max_subrecord_length);

void set_max_subrecord_length(int val)
{
  if (val > GFC_MAX_SUBRECORD_LENGTH || val < 1)
    {
      runtime_error ("Invalid value for maximum subrecord length");
      return;
    }

  compile_options.max_subrecord_length = val;
}
