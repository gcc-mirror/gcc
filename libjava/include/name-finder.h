// name-finder.h - Convert addresses to names

/* Copyright (C) 2000  Red Hat Inc

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/**
 * @author Andrew Haley <aph@cygnus.com>
 * @date Jan 6  2000
 */

#include <gcj/cni.h>
#include <jvm.h>

#include <sys/types.h>

#include <string.h>
#include <stdio.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* _Jv_name_finder is a class wrapper around a mechanism that can
   convert addresses of methods to their names and the names of files
   in which they appear. */

class _Jv_name_finder
{
public:  
  _Jv_name_finder (char *executable);
  ~_Jv_name_finder ()
    {
#if defined (HAVE_PIPE) && defined (HAVE_FORK)
      close (f_pipe[1]);
      fclose (b_pipe_fd);
#endif
    }  

/* Given a pointer to a function or method, try to convert it into a
   name and the appropriate line and source file.  The caller passes
   the code pointer in p.

   Returns false if the lookup fails.  Even if this happens, the field
   hex will have been correctly filled in with the pointer. 

   The other fields are method_name and file_name, which lookup will
   attempt to fill appropriately.  If the lookup has failed, these
   fields contain garbage.*/
  bool lookup (void *p);

  char method_name[1024];
  char file_name[1024];
  char hex[sizeof (void *) * 2 + 5];

private:
  void toHex (void *p);
#if defined (HAVE_PIPE) && defined (HAVE_FORK)
  int pid;
  int f_pipe[2], b_pipe[2];
  FILE *b_pipe_fd;
  int error;
#endif
};

