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
  demangling_error = lookup_error = 0;

  // Initialize file descriptors so that shutdown works properly.
  f_pipe[0] = -1;
  f_pipe[1] = -1;
  b_pipe[0] = -1;
  b_pipe[1] = -1;
  b_pipe_fd = NULL;

  f2_pipe[0] = -1;
  f2_pipe[1] = -1;
  b2_pipe[0] = -1;
  b2_pipe[1] = -1;
  b2_pipe_fd = NULL;

  // addr2line helper process.

  char *argv[5];
  {
    int arg = 0;
#ifdef __ia64__
    argv[arg++] = "addr2name.awk";
#else
    argv[arg++] = "addr2line";
    argv[arg++] = "-f";
    argv[arg++] = "-e";
#endif
    argv[arg++] = executable;
    argv[arg] = NULL;
  }

  lookup_error |= pipe (f_pipe) < 0;
  lookup_error |= pipe (b_pipe) < 0;

  if (lookup_error)
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
      lookup_error |= 1; 
      return;
    }

  b_pipe_fd = fdopen (b_pipe[0], "r");
  lookup_error |= !b_pipe_fd;

  if (! lookup_error)
    {
      // Don't try to close the fd twice.
      b_pipe[0] = -1;
    }

  // c++filt helper process.
  
  char *argv2[4];
  argv2[0] = "c++filt";
  argv2[1] = "-s";
  argv2[2] = "java";
  argv2[3] = NULL;

  demangling_error |= pipe (f2_pipe) < 0;
  demangling_error |= pipe (b2_pipe) < 0;

  if (demangling_error)
    return;

  pid2 = fork ();
  if (pid2 == 0)
    {
      close (f2_pipe[1]);
      close (b2_pipe[0]);
      dup2 (f2_pipe[0], fileno (stdin));
      dup2 (b2_pipe[1], fileno (stdout));
      execvp (argv2[0], argv2);
      _exit (127);
    }

  // Close child end of pipes.  Set local descriptors to -1 so we
  // don't try to close the fd again.
  close (f2_pipe [0]);
  f2_pipe[0] = -1;
  close (b2_pipe [1]);
  b2_pipe[1] = -1;

  if (pid2 < 0)
    {
      demangling_error |= 1; 
      return;
    }

  b2_pipe_fd = fdopen (b2_pipe[0], "r");
  demangling_error |= !b2_pipe_fd;

  if (! demangling_error)
    {
      // Don't try to close the fd twice.
      b2_pipe[0] = -1;
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

/* Creates a StackTraceElement given a string and a filename.
   Splits the given string into the class and method part.
   The string s will be a demangled to a fully qualified java method string.
   The string f will be decomposed into a file name and a possible line number.
   The given strings will be altered.  */

java::lang::StackTraceElement*
_Jv_name_finder::createStackTraceElement(char *s, char *f)
{
  char *c;
  char *class_name = NULL;
  char *method_name = NULL;

#if defined (HAVE_PIPE) && defined (HAVE_FORK) && defined (HAVE_EXECVP)
  if (demangling_error)
    goto fail;

  demangling_error |= write (f2_pipe[1], s, strlen (s)) < 0;
  if (demangling_error)
    goto fail;
  demangling_error |= write (f2_pipe[1], "\n", 1) < 0;
  if (demangling_error)
    goto fail;

  char name[1024];
  demangling_error |= (fgets (name, sizeof name, b2_pipe_fd) == NULL);
  if (demangling_error)
    goto fail;

  c = strchr (name, '\n');
  if (c)
    *c = 0;
  s = name;
#endif

  c = strchr (s, '(');
  if (c)
    {
      while(c-->s)
	if (*c == '.')
	  break;

      if (*c == '.')
	{
	  *c = 0;
	  class_name = s;
	  method_name = c+1;
	}
      else
	{
	  class_name = NULL;
	  method_name = s;
	}
    }
  else
    {
      class_name = NULL;
      method_name = s;
    }

  // Get line number
  int line_number;
  c = strrchr (f, ':');
  if (c)
    {
      if (c[1] != 0)
	line_number = atoi(c+1);
      else
	line_number = -1;
      *c = 0;
    }
  else
    {
      line_number = -1;
      c = strchr (f, '\n');
      if (c)
	*c = 0;
    }

 fail:
  return new java::lang::StackTraceElement(
		  f ? JvNewStringLatin1 (f) : NULL,
		  line_number,
		  class_name ? JvNewStringLatin1 (class_name) : NULL,
		  JvNewStringLatin1 (method_name ? method_name : s),
		  false);
}

/* Given a pointer to a function or method, try to convert it into a
   name and the appropriate line and source file.  The caller passes
   the code pointer in p.

   Returns false if the lookup fails.  Even if this happens, the field
   he will have been correctly filled in with the pointer.  */

java::lang::StackTraceElement*
_Jv_name_finder::lookup (void *p)
{
  extern char **_Jv_argv;
  toHex (p);
      
  char name[1024];
  char file_name[1024];

  file_name[0] = 0;

#if defined (HAVE_DLFCN_H) && defined (HAVE_DLADDR)
  {
    Dl_info dl_info;
    
    if (dladdr (p, &dl_info))
      {
        if (dl_info.dli_fname)
	  strncpy (file_name, dl_info.dli_fname, sizeof file_name);
	if (dl_info.dli_sname)
	  strncpy (name, dl_info.dli_sname, sizeof name);
       
       /* Don't trust dladdr() if the address is from the main program. */
       if (dl_info.dli_fname != NULL
           && dl_info.dli_sname != NULL
	   && (_Jv_argv == NULL || strcmp (file_name, _Jv_argv[0]) != 0))
         return createStackTraceElement (name, file_name);
      }
  }
#endif

  memcpy (name, hex, strlen (hex) + 1);

#if defined (HAVE_PIPE) && defined (HAVE_FORK) && defined (HAVE_EXECVP)
  if (lookup_error)
    goto fail;

  lookup_error |= write (f_pipe[1], hex, strlen (hex)) < 0;
  if (lookup_error)
    goto fail;
  lookup_error |= write (f_pipe[1], "\n", 1) < 0;
  if (lookup_error)
    goto fail;

  lookup_error |= (fgets (name, sizeof name, b_pipe_fd) == NULL);
  if (lookup_error)
    goto fail;
  lookup_error |= (fgets (file_name, sizeof file_name, b_pipe_fd) == NULL);
  if (lookup_error)
    goto fail;

  {
    char *newline = strchr (name, '\n');
    if (newline)
      *newline = 0;
  }
#endif /* defined (HAVE_PIPE) && defined (HAVE_FORK) && defined (HAVE_EXECVP) */

 fail:
  return (createStackTraceElement (name, file_name));
}
