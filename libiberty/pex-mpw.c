/* Utilities to execute a program in a subprocess (possibly linked by pipes
   with other subprocesses), and wait for it.  MPW specialization.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2003
   Free Software Foundation, Inc.

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "pex-common.h"

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

/* MPW pexecute doesn't actually run anything; instead, it writes out
   script commands that, when run, will do the actual executing.

   For example, in GCC's case, GCC will write out several script commands:

   cpp ...
   cc1 ...
   as ...
   ld ...

   and then exit.  None of the above programs will have run yet.  The task
   that called GCC will then execute the script and cause cpp,etc. to run.
   The caller must invoke pfinish before calling exit.  This adds
   the finishing touches to the generated script.  */

static int first_time = 1;

extern void mpwify_filename PARAMS ((const char *, char *));

int
pexecute (program, argv, this_pname, temp_base, errmsg_fmt, errmsg_arg, flags)
     const char *program;
     char * const *argv;
     const char *this_pname;
     const char *temp_base;
     char **errmsg_fmt, **errmsg_arg;
     int flags;
{
  char tmpprogram[255];
  char *cp, *tmpname;
  int i;

  mpwify_filename (program, tmpprogram);
  if (first_time)
    {
      printf ("Set Failed 0\n");
      first_time = 0;
    }

  fputs ("If {Failed} == 0\n", stdout);
  /* If being verbose, output a copy of the command.  It should be
     accurate enough and escaped enough to be "clickable".  */
  if (flags & PEXECUTE_VERBOSE)
    {
      fputs ("\tEcho ", stdout);
      fputc ('\'', stdout);
      fputs (tmpprogram, stdout);
      fputc ('\'', stdout);
      fputc (' ', stdout);
      for (i=1; argv[i]; i++)
	{
	  fputc ('\'', stdout);
	  /* See if we have an argument that needs fixing.  */
	  if (strchr(argv[i], '/'))
	    {
	      tmpname = (char *) xmalloc (256);
	      mpwify_filename (argv[i], tmpname);
	      argv[i] = tmpname;
	    }
	  for (cp = argv[i]; *cp; cp++)
	    {
	      /* Write an Option-d escape char in front of special chars.  */
	      if (strchr("'+", *cp))
		fputc ('\266', stdout);
	      fputc (*cp, stdout);
	    }
	  fputc ('\'', stdout);
	  fputc (' ', stdout);
	}
      fputs ("\n", stdout);
    }
  fputs ("\t", stdout);
  fputs (tmpprogram, stdout);
  fputc (' ', stdout);

  for (i=1; argv[i]; i++)
    {
      /* See if we have an argument that needs fixing.  */
      if (strchr(argv[i], '/'))
	{
	  tmpname = (char *) xmalloc (256);
	  mpwify_filename (argv[i], tmpname);
	  argv[i] = tmpname;
	}
      if (strchr (argv[i], ' '))
	fputc ('\'', stdout);
      for (cp = argv[i]; *cp; cp++)
	{
	  /* Write an Option-d escape char in front of special chars.  */
	  if (strchr("'+", *cp))
	    fputc ('\266', stdout);
	  fputc (*cp, stdout);
	}
      if (strchr (argv[i], ' '))
	fputc ('\'', stdout);
      fputc (' ', stdout);
    }

  fputs ("\n", stdout);

  /* Output commands that arrange to clean up and exit if a failure occurs.
     We have to be careful to collect the status from the program that was
     run, rather than some other script command.  Also, we don't exit
     immediately, since necessary cleanups are at the end of the script.  */
  fputs ("\tSet TmpStatus {Status}\n", stdout);
  fputs ("\tIf {TmpStatus} != 0\n", stdout);
  fputs ("\t\tSet Failed {TmpStatus}\n", stdout);
  fputs ("\tEnd\n", stdout);
  fputs ("End\n", stdout);

  /* We're just composing a script, can't fail here.  */
  return 0;
}

int
pwait (pid, status, flags)
     int pid;
     int *status;
     int flags;
{
  *status = 0;
  return 0;
}

/* Write out commands that will exit with the correct error code
   if something in the script failed.  */

void
pfinish ()
{
  printf ("\tExit \"{Failed}\"\n");
}

