// name-finder.cc - Convert addresses to names

/* Copyright (C) 2000, 2002  Free Software Foundation, Inc

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/**
 * @author Andrew Haley <aph@cygnus.com>
 * @date Jan 6  2000
 */

/* _Jv_name_finder is a class wrapper around a mechanism that can
   convert address of methods to their names and the names of files in
   which they appear.

   Right now, the only implementation of this involves running a copy
   of addr2line, but at some point it is worth building this
   functionality into libgcj, if only for embedded systems.  */


#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <config.h>

#include <string.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java/lang/Object.h>
#include <java-threads.h>
#include <java/lang/Throwable.h>
#include <java/io/PrintStream.h>
#include <java/io/PrintWriter.h>

#include <sys/types.h>

#include <stdlib.h>
#include <stdio.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#include <name-finder.h>

/* Create a new name finder which will perform address lookups on an
   executable. */

_Jv_name_finder::_Jv_name_finder (char *executable)
{
#if defined (HAVE_PIPE) && defined (HAVE_FORK) && defined (HAVE_EXECVP)
  error = 0;

  // Initialize file descriptors so that shutdown works properly.
  f_pipe[0] = -1;
  f_pipe[1] = -1;
  b_pipe[0] = -1;
  b_pipe[1] = -1;
  b_pipe_fd = NULL;

  char *argv[6];
  {
    int arg = 0;
#ifdef __ia64__
    argv[arg++] = "addr2name.awk";
#else
    argv[arg++] = "addr2line";
    argv[arg++] = "-C";
    argv[arg++] = "-f";
    argv[arg++] = "-e";
#endif
    argv[arg++] = executable;
    argv[arg] = NULL;
  }

  error |= pipe (f_pipe) < 0;
  error |= pipe (b_pipe) < 0;

  if (error)
    return;

  pid = fork ();
  if (pid == 0)
    {
      close (f_pipe[1]);
      close (b_pipe[0]);
      dup2 (f_pipe[0], fileno (stdin));
      dup2 (b_pipe[1], fileno (stdout));
      execvp (argv[0], argv);
      _exit (127);
    }

  // Close child end of pipes.  Set local descriptors to -1 so we
  // don't try to close the fd again.
  close (f_pipe [0]);
  f_pipe[0] = -1;
  close (b_pipe [1]);
  b_pipe[1] = -1;

  if (pid < 0)
    {
      error |= 1; 
      return;
    }

  b_pipe_fd = fdopen (b_pipe[0], "r");
  error |= !b_pipe_fd;

  if (! error)
    {
      // Don't try to close the fd twice.
      b_pipe[0] = -1;
    }
#endif
}

/* Convert a pointer to hex. */

void
_Jv_name_finder::toHex (void *p)
{
  typedef unsigned word_t __attribute ((mode (word)));
  word_t n = (word_t) p;
  int digits = sizeof (void *) * 2;

  strcpy (hex, "0x");
  for (int i = digits - 1; i >= 0; i--)
    {
      int digit = n % 16;
      
      n /= 16;
      hex[i+2] = digit > 9 ? 'a' + digit - 10 : '0' + digit; 
    }
  hex [digits+2] = 0;
}   

/* Given a pointer to a function or method, try to convert it into a
   name and the appropriate line and source file.  The caller passes
   the code pointer in p.

   Returns false if the lookup fails.  Even if this happens, the field
   he will have been correctly filled in with the pointer.  */

bool
_Jv_name_finder::lookup (void *p)
{
  extern char **_Jv_argv;
  toHex (p);
      
#if defined (HAVE_DLFCN_H) && defined (HAVE_DLADDR)
  {
    Dl_info dl_info;
    
    if (dladdr (p, &dl_info))
      {
        if (dl_info.dli_fname)
	  strncpy (file_name, dl_info.dli_fname, sizeof file_name);
	if (dl_info.dli_sname)
	  strncpy (method_name, dl_info.dli_sname, sizeof method_name);
       
       /* Don't trust dladdr() if the address is from the main program. */
       if (dl_info.dli_fname != NULL
           && dl_info.dli_sname != NULL
	   && (_Jv_argv == NULL || strcmp (file_name, _Jv_argv[0]) != 0))
         return true;
      }
  }
#endif

#if defined (HAVE_PIPE) && defined (HAVE_FORK) && defined (HAVE_EXECVP)
  if (error)
    return false;

  error |= write (f_pipe[1], hex, strlen (hex)) < 0;
  if (error)
    return false;
  error |= write (f_pipe[1], "\n", 1) < 0;
  if (error)
    return false;

  error |= (fgets (method_name, sizeof method_name, b_pipe_fd) == NULL);
  if (error)
    return false;
  error |= (fgets (file_name, sizeof file_name, b_pipe_fd) == NULL);
  if (error)
    return false;

  char *newline = strchr (method_name, '\n');
  if (newline)
    *newline = 0;
  newline = strchr (file_name, '\n');
  if (newline)
    *newline = 0;

  return true;

#else
  return false;
#endif /* defined (HAVE_PIPE) && defined (HAVE_FORK) && defined (HAVE_EXECVP) */
}
