/* Copyright (C) 1995, 1997 Free Software Foundation, Inc.
This file is part of GNU Fortran libU77 library.

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with GNU Fortran; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <stdio.h>
#if HAVE_STDLIB_H
#  include <stdlib.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>		/* for ENOSYS */
#include "f2c.h"

/* lstat isn't posix */

void g_char (const char *a, ftnlen alen, char *b);

integer
G77_lstat_0 (const char *name, integer statb[13], const ftnlen Lname)
{
#if HAVE_LSTAT
  char *buff;
  int err;
  struct stat buf;

  buff = malloc (Lname + 1);
  if (buff == NULL)
    return -1;
  g_char (name, Lname, buff);
  err = lstat (buff, &buf);
  free (buff);
  statb[0] = buf.st_dev;
  statb[1] = buf.st_ino;
  statb[2] = buf.st_mode;
  statb[3] = buf.st_nlink;
  statb[4] = buf.st_uid;
  statb[5] = buf.st_gid;
#if HAVE_ST_RDEV
  statb[6] = buf.st_rdev;
#else
  statb[6] = 0;
#endif
  statb[7] = buf.st_size;
  statb[8] = buf.st_atime;
  statb[9] = buf.st_mtime;
  statb[10] = buf.st_ctime;
#if HAVE_ST_BLKSIZE
  statb[11] = buf.st_blksize;
#else
  statb[11] = -1;
#endif
#if HAVE_ST_BLOCKS
  statb[12] = buf.st_blocks;
#else
  statb[12] = -1;
#endif
  return err;
#else /* !HAVE_LSTAT */
  return errno = ENOSYS;
#endif /* !HAVE_LSTAT */
}
