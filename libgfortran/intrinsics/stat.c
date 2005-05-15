/* Implementation of the STAT and FSTAT intrinsics.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.
   Contributed by Steven G. Kargl <kargls@comcast.net>.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

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

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "libgfortran.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <errno.h>

#include "../io/io.h"

/* SUBROUTINE STAT(FILE, SARRAY, STATUS)
   CHARACTER(len=*), INTENT(IN) :: FILE
   INTEGER, INTENT(OUT), :: SARRAY(13)
   INTEGER, INTENT(OUT), OPTIONAL :: STATUS

   FUNCTION STAT(FILE, SARRAY)
   INTEGER STAT
   CHARACTER(len=*), INTENT(IN) :: FILE
   INTEGER, INTENT(OUT), :: SARRAY(13)  */

extern void stat_i4_sub (char *, gfc_array_i4 *, GFC_INTEGER_4 *,
			 gfc_charlen_type);
iexport_proto(stat_i4_sub);

void
stat_i4_sub (char *name, gfc_array_i4 *sarray, GFC_INTEGER_4 *status,
	     gfc_charlen_type name_len)
{
  int val;
  char *str;
  struct stat sb;

  /* If the rank of the array is not 1, abort.  */
  if (GFC_DESCRIPTOR_RANK (sarray) != 1)
    runtime_error ("Array rank of SARRAY is not 1.");

  /* If the array is too small, abort.  */
  if (sarray->dim[0].ubound + 1 - sarray->dim[0].lbound < 13)
    runtime_error ("Array size of SARRAY is too small.");

  if (sarray->dim[0].stride == 0)
    sarray->dim[0].stride = 1;

  /* Trim trailing spaces from name.  */
  while (name_len > 0 && name[name_len - 1] == ' ')
    name_len--;

  /* Make a null terminated copy of the string.  */
  str = gfc_alloca (name_len + 1);
  memcpy (str, name, name_len);
  str[name_len] = '\0';

  val = stat(str, &sb);

  if (val == 0)
    {
      /* Device ID  */
      sarray->data[0 * sarray->dim[0].stride] = sb.st_dev;

      /* Inode number  */
      sarray->data[1 * sarray->dim[0].stride] = sb.st_ino;

      /* File mode  */
      sarray->data[2 * sarray->dim[0].stride] = sb.st_mode;

      /* Number of (hard) links  */
      sarray->data[3 * sarray->dim[0].stride] = sb.st_nlink;

      /* Owner's uid  */
      sarray->data[4 * sarray->dim[0].stride] = sb.st_uid;

      /* Owner's gid  */
      sarray->data[5 * sarray->dim[0].stride] = sb.st_gid;

      /* ID of device containing directory entry for file (0 if not available) */
#if HAVE_STRUCT_STAT_ST_RDEV
      sarray->data[6 * sarray->dim[0].stride] = sb.st_rdev;
#else
      sarray->data[6 * sarray->dim[0].stride] = 0;
#endif

      /* File size (bytes)  */
      sarray->data[7 * sarray->dim[0].stride] = sb.st_size;

      /* Last access time  */
      sarray->data[8 * sarray->dim[0].stride] = sb.st_atime;

      /* Last modification time  */
      sarray->data[9 * sarray->dim[0].stride] = sb.st_mtime;

      /* Last file status change time  */
      sarray->data[10 * sarray->dim[0].stride] = sb.st_ctime;

      /* Preferred I/O block size (-1 if not available)  */
#if HAVE_STRUCT_STAT_ST_BLKSIZE
      sarray->data[11 * sarray->dim[0].stride] = sb.st_blksize;
#else
      sarray->data[11 * sarray->dim[0].stride] = -1;
#endif

      /* Number of blocks allocated (-1 if not available)  */
#if HAVE_STRUCT_STAT_ST_BLOCKS
      sarray->data[12 * sarray->dim[0].stride] = sb.st_blocks;
#else
      sarray->data[12 * sarray->dim[0].stride] = -1;
#endif
    }

  if (status != NULL)
    *status = (val == 0) ? 0 : errno;
}
iexport(stat_i4_sub);

extern void stat_i8_sub (char *, gfc_array_i8 *, GFC_INTEGER_8 *,
			 gfc_charlen_type);
iexport_proto(stat_i8_sub);

void
stat_i8_sub (char *name, gfc_array_i8 *sarray, GFC_INTEGER_8 *status,
	     gfc_charlen_type name_len)
{
  int val;
  char *str;
  struct stat sb;

  /* If the rank of the array is not 1, abort.  */
  if (GFC_DESCRIPTOR_RANK (sarray) != 1)
    runtime_error ("Array rank of SARRAY is not 1.");

  /* If the array is too small, abort.  */
  if (sarray->dim[0].ubound + 1 - sarray->dim[0].lbound < 13)
    runtime_error ("Array size of SARRAY is too small.");

  if (sarray->dim[0].stride == 0)
    sarray->dim[0].stride = 1;

  /* Trim trailing spaces from name.  */
  while (name_len > 0 && name[name_len - 1] == ' ')
    name_len--;

  /* Make a null terminated copy of the string.  */
  str = gfc_alloca (name_len + 1);
  memcpy (str, name, name_len);
  str[name_len] = '\0';

  val = stat(str, &sb);

  if (val == 0)
    {
      /* Device ID  */
      sarray->data[0] = sb.st_dev;

      /* Inode number  */
      sarray->data[sarray->dim[0].stride] = sb.st_ino;

      /* File mode  */
      sarray->data[2 * sarray->dim[0].stride] = sb.st_mode;

      /* Number of (hard) links  */
      sarray->data[3 * sarray->dim[0].stride] = sb.st_nlink;

      /* Owner's uid  */
      sarray->data[4 * sarray->dim[0].stride] = sb.st_uid;

      /* Owner's gid  */
      sarray->data[5 * sarray->dim[0].stride] = sb.st_gid;

      /* ID of device containing directory entry for file (0 if not available) */
#if HAVE_STRUCT_STAT_ST_RDEV
      sarray->data[6 * sarray->dim[0].stride] = sb.st_rdev;
#else
      sarray->data[6 * sarray->dim[0].stride] = 0;
#endif

      /* File size (bytes)  */
      sarray->data[7 * sarray->dim[0].stride] = sb.st_size;

      /* Last access time  */
      sarray->data[8 * sarray->dim[0].stride] = sb.st_atime;

      /* Last modification time  */
      sarray->data[9 * sarray->dim[0].stride] = sb.st_mtime;

      /* Last file status change time  */
      sarray->data[10 * sarray->dim[0].stride] = sb.st_ctime;

      /* Preferred I/O block size (-1 if not available)  */
#if HAVE_STRUCT_STAT_ST_BLKSIZE
      sarray->data[11 * sarray->dim[0].stride] = sb.st_blksize;
#else
      sarray->data[11 * sarray->dim[0].stride] = -1;
#endif

      /* Number of blocks allocated (-1 if not available)  */
#if HAVE_STRUCT_STAT_ST_BLOCKS
      sarray->data[12 * sarray->dim[0].stride] = sb.st_blocks;
#else
      sarray->data[12 * sarray->dim[0].stride] = -1;
#endif
    }

  if (status != NULL)
    *status = (val == 0) ? 0 : errno;
}
iexport(stat_i8_sub);

extern GFC_INTEGER_4 stat_i4 (char *, gfc_array_i4 *, gfc_charlen_type);
export_proto(stat_i4);

GFC_INTEGER_4
stat_i4 (char *name, gfc_array_i4 *sarray, gfc_charlen_type name_len)
{
  GFC_INTEGER_4 val;
  stat_i4_sub (name, sarray, &val, name_len);
  return val;
}

extern GFC_INTEGER_8 stat_i8 (char *, gfc_array_i8 *, gfc_charlen_type);
export_proto(stat_i8);

GFC_INTEGER_8
stat_i8 (char *name, gfc_array_i8 *sarray, gfc_charlen_type name_len)
{
  GFC_INTEGER_8 val;
  stat_i8_sub (name, sarray, &val, name_len);
  return val;
}


/* SUBROUTINE FSTAT(UNIT, SARRAY, STATUS)
   INTEGER, INTENT(IN) :: UNIT
   INTEGER, INTENT(OUT) :: SARRAY(13)
   INTEGER, INTENT(OUT), OPTIONAL :: STATUS

   FUNCTION FSTAT(UNIT, SARRAY)
   INTEGER FSTAT
   INTEGER, INTENT(IN) :: UNIT
   INTEGER, INTENT(OUT) :: SARRAY(13)  */

extern void fstat_i4_sub (GFC_INTEGER_4 *, gfc_array_i4 *, GFC_INTEGER_4 *);
iexport_proto(fstat_i4_sub);

void
fstat_i4_sub (GFC_INTEGER_4 *unit, gfc_array_i4 *sarray, GFC_INTEGER_4 *status)
{
  int val;
  struct stat sb;

  /* If the rank of the array is not 1, abort.  */
  if (GFC_DESCRIPTOR_RANK (sarray) != 1)
    runtime_error ("Array rank of SARRAY is not 1.");

  /* If the array is too small, abort.  */
  if (sarray->dim[0].ubound + 1 - sarray->dim[0].lbound < 13)
    runtime_error ("Array size of SARRAY is too small.");

  if (sarray->dim[0].stride == 0)
    sarray->dim[0].stride = 1;

  /* Convert Fortran unit number to C file descriptor.  */
  val = unit_to_fd (*unit);
  if (val >= 0)
    val = fstat(val, &sb);

  if (val == 0)
    {
      /* Device ID  */
      sarray->data[0 * sarray->dim[0].stride] = sb.st_dev;

      /* Inode number  */
      sarray->data[1 * sarray->dim[0].stride] = sb.st_ino;

      /* File mode  */
      sarray->data[2 * sarray->dim[0].stride] = sb.st_mode;

      /* Number of (hard) links  */
      sarray->data[3 * sarray->dim[0].stride] = sb.st_nlink;

      /* Owner's uid  */
      sarray->data[4 * sarray->dim[0].stride] = sb.st_uid;

      /* Owner's gid  */
      sarray->data[5 * sarray->dim[0].stride] = sb.st_gid;

      /* ID of device containing directory entry for file (0 if not available) */
#if HAVE_STRUCT_STAT_ST_RDEV
      sarray->data[6 * sarray->dim[0].stride] = sb.st_rdev;
#else
      sarray->data[6 * sarray->dim[0].stride] = 0;
#endif

      /* File size (bytes)  */
      sarray->data[7 * sarray->dim[0].stride] = sb.st_size;

      /* Last access time  */
      sarray->data[8 * sarray->dim[0].stride] = sb.st_atime;

      /* Last modification time  */
      sarray->data[9 * sarray->dim[0].stride] = sb.st_mtime;

      /* Last file status change time  */
      sarray->data[10 * sarray->dim[0].stride] = sb.st_ctime;

      /* Preferred I/O block size (-1 if not available)  */
#if HAVE_STRUCT_STAT_ST_BLKSIZE
      sarray->data[11 * sarray->dim[0].stride] = sb.st_blksize;
#else
      sarray->data[11 * sarray->dim[0].stride] = -1;
#endif

      /* Number of blocks allocated (-1 if not available)  */
#if HAVE_STRUCT_STAT_ST_BLOCKS
      sarray->data[12 * sarray->dim[0].stride] = sb.st_blocks;
#else
      sarray->data[12 * sarray->dim[0].stride] = -1;
#endif
    }

  if (status != NULL)
    *status = (val == 0) ? 0 : errno;
}
iexport(fstat_i4_sub);

extern void fstat_i8_sub (GFC_INTEGER_8 *, gfc_array_i8 *, GFC_INTEGER_8 *);
iexport_proto(fstat_i8_sub);

void
fstat_i8_sub (GFC_INTEGER_8 *unit, gfc_array_i8 *sarray, GFC_INTEGER_8 *status)
{
  int val;
  struct stat sb;

  /* If the rank of the array is not 1, abort.  */
  if (GFC_DESCRIPTOR_RANK (sarray) != 1)
    runtime_error ("Array rank of SARRAY is not 1.");

  /* If the array is too small, abort.  */
  if (sarray->dim[0].ubound + 1 - sarray->dim[0].lbound < 13)
    runtime_error ("Array size of SARRAY is too small.");

  if (sarray->dim[0].stride == 0)
    sarray->dim[0].stride = 1;

  /* Convert Fortran unit number to C file descriptor.  */
  val = unit_to_fd ((int) *unit);
  if (val >= 0)
    val = fstat(val, &sb);

  if (val == 0)
    {
      /* Device ID  */
      sarray->data[0] = sb.st_dev;

      /* Inode number  */
      sarray->data[sarray->dim[0].stride] = sb.st_ino;

      /* File mode  */
      sarray->data[2 * sarray->dim[0].stride] = sb.st_mode;

      /* Number of (hard) links  */
      sarray->data[3 * sarray->dim[0].stride] = sb.st_nlink;

      /* Owner's uid  */
      sarray->data[4 * sarray->dim[0].stride] = sb.st_uid;

      /* Owner's gid  */
      sarray->data[5 * sarray->dim[0].stride] = sb.st_gid;

      /* ID of device containing directory entry for file (0 if not available) */
#if HAVE_STRUCT_STAT_ST_RDEV
      sarray->data[6 * sarray->dim[0].stride] = sb.st_rdev;
#else
      sarray->data[6 * sarray->dim[0].stride] = 0;
#endif

      /* File size (bytes)  */
      sarray->data[7 * sarray->dim[0].stride] = sb.st_size;

      /* Last access time  */
      sarray->data[8 * sarray->dim[0].stride] = sb.st_atime;

      /* Last modification time  */
      sarray->data[9 * sarray->dim[0].stride] = sb.st_mtime;

      /* Last file status change time  */
      sarray->data[10 * sarray->dim[0].stride] = sb.st_ctime;

      /* Preferred I/O block size (-1 if not available)  */
#if HAVE_STRUCT_STAT_ST_BLKSIZE
      sarray->data[11 * sarray->dim[0].stride] = sb.st_blksize;
#else
      sarray->data[11 * sarray->dim[0].stride] = -1;
#endif

      /* Number of blocks allocated (-1 if not available)  */
#if HAVE_STRUCT_STAT_ST_BLOCKS
      sarray->data[12 * sarray->dim[0].stride] = sb.st_blocks;
#else
      sarray->data[12 * sarray->dim[0].stride] = -1;
#endif
    }

  if (status != NULL)
    *status = (val == 0) ? 0 : errno;
}
iexport(fstat_i8_sub);

extern GFC_INTEGER_4 fstat_i4 (GFC_INTEGER_4 *, gfc_array_i4 *);
export_proto(fstat_i4);

GFC_INTEGER_4
fstat_i4 (GFC_INTEGER_4 *unit, gfc_array_i4 *sarray)
{
  GFC_INTEGER_4 val;
  fstat_i4_sub (unit, sarray, &val);
  return val;
}

extern GFC_INTEGER_8 fstat_i8 (GFC_INTEGER_8 *, gfc_array_i8 *);
export_proto(fstat_i8);

GFC_INTEGER_8
fstat_i8 (GFC_INTEGER_8 *unit, gfc_array_i8 *sarray)
{
  GFC_INTEGER_8 val;
  fstat_i8_sub (unit, sarray, &val);
  return val;
}
