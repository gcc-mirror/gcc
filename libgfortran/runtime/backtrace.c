/* Copyright (C) 2006, 2007 Free Software Foundation, Inc.
   Contributed by François-Xavier Coudert

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
along with libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "libgfortran.h"

#include <string.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_INTPTR_T
# define INTPTR_T intptr_t
#else
# define INTPTR_T int
#endif

#ifdef HAVE_EXECINFO_H
#include <execinfo.h>
#endif

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include <ctype.h>


/* Macros for common sets of capabilities: can we fork and exec, can
   we use glibc-style backtrace functions, and can we use pipes.  */
#define CAN_FORK (defined(HAVE_FORK) && defined(HAVE_EXECVP) \
		  && defined(HAVE_WAIT))
#define GLIBC_BACKTRACE (defined(HAVE_BACKTRACE) \
			 && defined(HAVE_BACKTRACE_SYMBOLS))
#define CAN_PIPE (CAN_FORK && defined(HAVE_PIPE) \
		  && defined(HAVE_DUP2) && defined(HAVE_FDOPEN) \
		  && defined(HAVE_CLOSE))


#if GLIBC_BACKTRACE && CAN_PIPE
static char *
local_strcasestr (const char *s1, const char *s2)
{
#ifdef HAVE_STRCASESTR
  return strcasestr (s1, s2);
#else

  const char *p = s1;
  const size_t len = strlen (s2);
  const char u = *s2, v = isupper((int) *s2) ? tolower((int) *s2)
				  : (islower((int) *s2) ? toupper((int) *s2)
							: *s2);

  while (1)
    {
      while (*p != u && *p != v && *p)
	p++;
      if (*p == 0)
	return NULL;
      if (strncasecmp (p, s2, len) == 0)
	return (char *)p;
    }
#endif
}
#endif


#if GLIBC_BACKTRACE
static void
dump_glibc_backtrace (int depth, char *str[])
{
  int i;

  for (i = 0; i < depth; i++)
    st_printf ("  + %s\n", str[i]);

  free (str);
}
#endif

/* show_backtrace displays the backtrace, currently obtained by means of
   the glibc backtrace* functions.  */
void
show_backtrace (void)
{
#if GLIBC_BACKTRACE

#define DEPTH 50
#define BUFSIZE 1024

  void *trace[DEPTH];
  char **str;
  int depth;

  depth = backtrace (trace, DEPTH);
  if (depth <= 0)
    return;

  str = backtrace_symbols (trace, depth);

#if CAN_PIPE

#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif

#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif

#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif

  /* We attempt to extract file and line information from addr2line.  */
  do
  {
    /* Local variables.  */
    int f[2], pid, line, i;
    FILE *output;
    char addr_buf[DEPTH][GFC_XTOA_BUF_SIZE], func[BUFSIZE], file[BUFSIZE];
    char *p, *end;
    const char *addr[DEPTH];

    /* Write the list of addresses in hexadecimal format.  */
    for (i = 0; i < depth; i++)
      addr[i] = xtoa ((GFC_UINTEGER_LARGEST) (INTPTR_T) trace[i], addr_buf[i],
		      sizeof (addr_buf[i]));

    /* Don't output an error message if something goes wrong, we'll simply
       fall back to the pstack and glibc backtraces.  */
    if (pipe (f) != 0)
      break;
    if ((pid = fork ()) == -1)
      break;

    if (pid == 0)
      {
	/* Child process.  */
#define NUM_FIXEDARGS 5
	char *arg[DEPTH+NUM_FIXEDARGS+1];

	close (f[0]);
	close (STDIN_FILENO);
	close (STDERR_FILENO);

	if (dup2 (f[1], STDOUT_FILENO) == -1)
	  _exit (0);
	close (f[1]);

	arg[0] = (char *) "addr2line";
	arg[1] = (char *) "-e";
	arg[2] = full_exe_path ();
	arg[3] = (char *) "-f";
	arg[4] = (char *) "-s";
	for (i = 0; i < depth; i++)
	  arg[NUM_FIXEDARGS+i] = (char *) addr[i];
	arg[NUM_FIXEDARGS+depth] = NULL;
	execvp (arg[0], arg);
	_exit (0);
#undef NUM_FIXEDARGS
      }

    /* Father process.  */
    close (f[1]);
    wait (NULL);
    output = fdopen (f[0], "r");
    i = -1;

    if (fgets (func, sizeof(func), output))
      {
	st_printf ("\nBacktrace for this error:\n");

	do
	  {
	    if (! fgets (file, sizeof(file), output))
	      goto fallback;

	    i++;

	    for (p = func; *p != '\n' && *p != '\r'; p++)
	      ;

	    *p = '\0';

	    /* Try to recognize the internal libgfortran functions.  */
	    if (strncasecmp (func, "*_gfortran", 10) == 0
		|| strncasecmp (func, "_gfortran", 9) == 0
		|| strcmp (func, "main") == 0 || strcmp (func, "_start") == 0
		|| strcmp (func, "_gfortrani_handler") == 0)
	      continue;

	    if (local_strcasestr (str[i], "libgfortran.so") != NULL
		|| local_strcasestr (str[i], "libgfortran.dylib") != NULL
		|| local_strcasestr (str[i], "libgfortran.a") != NULL)
	      continue;

	    /* If we only have the address, use the glibc backtrace.  */
	    if (func[0] == '?' && func[1] == '?' && file[0] == '?'
		&& file[1] == '?')
	      {
	        st_printf ("  + %s\n", str[i]);
	        continue;
	      }

	    /* Extract the line number.  */
	    for (end = NULL, p = file; *p; p++)
	      if (*p == ':')
		end = p;
	    if (end != NULL)
	      {
		*end = '\0';
		line = atoi (++end);
	      }
	    else
	      line = -1;

	    if (strcmp (func, "MAIN__") == 0)
	      st_printf ("  + in the main program\n");
	    else
	      st_printf ("  + function %s (0x%s)\n", func, addr[i]);

	    if (line <= 0 && strcmp (file, "??") == 0)
	      continue;

	    if (line <= 0)
	      st_printf ("    from file %s\n", file);
	    else
	      st_printf ("    at line %d of file %s\n", line, file);
	  }
	while (fgets (func, sizeof(func), output));

	free (str);
	return;

fallback:
	st_printf ("** Something went wrong while running addr2line. **\n"
		   "** Falling back  to a simpler  backtrace scheme. **\n");
      }
    }
  while (0);

#undef DEPTH
#undef BUFSIZE

#endif
#endif

#if CAN_FORK && defined(HAVE_GETPPID)
  /* Try to call pstack.  */
  do
  {
    /* Local variables.  */
    int pid;

    /* Don't output an error message if something goes wrong, we'll simply
       fall back to the pstack and glibc backtraces.  */
    if ((pid = fork ()) == -1)
      break;

    if (pid == 0)
      {
	/* Child process.  */
#define NUM_ARGS 2
	char *arg[NUM_ARGS+1];
	char buf[20];

	st_printf ("\nBacktrace for this error:\n");
	arg[0] = (char *) "pstack";
#ifdef HAVE_SNPRINTF
	snprintf (buf, sizeof(buf), "%d", (int) getppid ());
#else
	sprintf (buf, "%d", (int) getppid ());
#endif
	arg[1] = buf;
	arg[2] = NULL;
	execvp (arg[0], arg);
#undef NUM_ARGS

	/* pstack didn't work, so we fall back to dumping the glibc
	   backtrace if we can.  */
#if GLIBC_BACKTRACE
	dump_glibc_backtrace (depth, str);
#else
	st_printf ("  unable to produce a backtrace, sorry!\n");
#endif

	_exit (0);
      }

    /* Father process.  */
    wait (NULL);
    return;
  }
  while(0);
#endif

#if GLIBC_BACKTRACE
  /* Fallback to the glibc backtrace.  */
  st_printf ("\nBacktrace for this error:\n");
  dump_glibc_backtrace (depth, str);
#endif
}
