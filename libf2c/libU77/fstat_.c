/* Copyright (C) 1995 Free Software Foundation, Inc.
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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "f2c.h"
#include "fio.h"
#include <sys/types.h>
#include <sys/stat.h>

extern integer G77_fnum_0 (const integer *);

integer
G77_fstat_0 (const integer * lunit, integer statb[13])
{
  int err;
  struct stat buf;

  if (f__init != 1) f_init();
  err = fstat (G77_fnum_0 (lunit), &buf);
  statb[0] = buf.st_dev;
  statb[1] = buf.st_ino;
  statb[2] = buf.st_mode;
  statb[3] = buf.st_nlink;
  statb[4] = buf.st_uid;
  statb[5] = buf.st_gid;
#if HAVE_ST_RDEV
  statb[6] = buf.st_rdev;	/* not posix */
#else
  statb[6] = 0;
#endif
  statb[7] = buf.st_size;
  statb[8] = buf.st_atime;
  statb[9] = buf.st_mtime;
  statb[10] = buf.st_ctime;
#if HAVE_ST_BLKSIZE
  statb[11] = buf.st_blksize;	/* not posix */
#else
  statb[11] = -1;
#endif
#if HAVE_ST_BLOCKS
  statb[12] = buf.st_blocks;	/* not posix */
#else
  statb[12] = -1;
#endif
  return err;
}
