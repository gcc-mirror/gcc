/* Implementation of the STAT and FSTAT intrinsics.
   Copyright (C) 2004-2020 Free Software Foundation, Inc.
   Contributed by Steven G. Kargl <kargls@comcast.net>.

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

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

#include <errno.h>

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif



#ifdef HAVE_STAT

/* SUBROUTINE STAT(FILE, SARRAY, STATUS)
   CHARACTER(len=*), INTENT(IN) :: FILE
   INTEGER, INTENT(OUT), :: SARRAY(13)
   INTEGER, INTENT(OUT), OPTIONAL :: STATUS

   FUNCTION STAT(FILE, SARRAY)
   INTEGER STAT
   CHARACTER(len=*), INTENT(IN) :: FILE
   INTEGER, INTENT(OUT), :: SARRAY(13)  */

/*extern void stat_i4_sub_0 (char *, gfc_array_i4 *, GFC_INTEGER_4 *,
			   gfc_charlen_type, int);
internal_proto(stat_i4_sub_0);*/

static void
stat_i4_sub_0 (char *name, gfc_array_i4 *sarray, GFC_INTEGER_4 *status,
	       gfc_charlen_type name_len, int is_lstat __attribute__ ((unused)))
{
  int val;
  char *str;
  struct stat sb;

  /* If the rank of the array is not 1, abort.  */
  if (GFC_DESCRIPTOR_RANK (sarray) != 1)
    runtime_error ("Array rank of SARRAY is not 1.");

  /* If the array is too small, abort.  */
  if (GFC_DESCRIPTOR_EXTENT(sarray,0) < 13)
    runtime_error ("Array size of SARRAY is too small.");

  /* Make a null terminated copy of the string.  */
  str = fc_strdup (name, name_len);

  /* On platforms that don't provide lstat(), we use stat() instead.  */
#ifdef HAVE_LSTAT
  if (is_lstat)
    val = lstat(str, &sb);
  else
#endif
    val = stat(str, &sb);

  free (str);

  if (val == 0)
    {
      index_type stride = GFC_DESCRIPTOR_STRIDE(sarray,0);

      /* Device ID  */
      sarray->base_addr[0 * stride] = sb.st_dev;

      /* Inode number  */
      sarray->base_addr[1 * stride] = sb.st_ino;

      /* File mode  */
      sarray->base_addr[2 * stride] = sb.st_mode;

      /* Number of (hard) links  */
      sarray->base_addr[3 * stride] = sb.st_nlink;

      /* Owner's uid  */
      sarray->base_addr[4 * stride] = sb.st_uid;

      /* Owner's gid  */
      sarray->base_addr[5 * stride] = sb.st_gid;

      /* ID of device containing directory entry for file (0 if not available) */
#if HAVE_STRUCT_STAT_ST_RDEV
      sarray->base_addr[6 * stride] = sb.st_rdev;
#else
      sarray->base_addr[6 * stride] = 0;
#endif

      /* File size (bytes)  */
      sarray->base_addr[7 * stride] = sb.st_size;

      /* Last access time  */
      sarray->base_addr[8 * stride] = sb.st_atime;

      /* Last modification time  */
      sarray->base_addr[9 * stride] = sb.st_mtime;

      /* Last file status change time  */
      sarray->base_addr[10 * stride] = sb.st_ctime;

      /* Preferred I/O block size (-1 if not available)  */
#if HAVE_STRUCT_STAT_ST_BLKSIZE
      sarray->base_addr[11 * stride] = sb.st_blksize;
#else
      sarray->base_addr[11 * stride] = -1;
#endif

      /* Number of blocks allocated (-1 if not available)  */
#if HAVE_STRUCT_STAT_ST_BLOCKS
      sarray->base_addr[12 * stride] = sb.st_blocks;
#else
      sarray->base_addr[12 * stride] = -1;
#endif
    }

  if (status != NULL)
    *status = (val == 0) ? 0 : errno;
}


extern void stat_i4_sub (char *, gfc_array_i4 *, GFC_INTEGER_4 *,
			 gfc_charlen_type);
iexport_proto(stat_i4_sub);

void
stat_i4_sub (char *name, gfc_array_i4 *sarray, GFC_INTEGER_4 *status,
	     gfc_charlen_type name_len)
{
  stat_i4_sub_0 (name, sarray, status, name_len, 0);
}
iexport(stat_i4_sub);


extern void lstat_i4_sub (char *, gfc_array_i4 *, GFC_INTEGER_4 *,
			 gfc_charlen_type);
iexport_proto(lstat_i4_sub);

void
lstat_i4_sub (char *name, gfc_array_i4 *sarray, GFC_INTEGER_4 *status,
	     gfc_charlen_type name_len)
{
  stat_i4_sub_0 (name, sarray, status, name_len, 1);
}
iexport(lstat_i4_sub);



static void
stat_i8_sub_0 (char *name, gfc_array_i8 *sarray, GFC_INTEGER_8 *status,
	       gfc_charlen_type name_len, int is_lstat __attribute__ ((unused)))
{
  int val;
  char *str;
  struct stat sb;

  /* If the rank of the array is not 1, abort.  */
  if (GFC_DESCRIPTOR_RANK (sarray) != 1)
    runtime_error ("Array rank of SARRAY is not 1.");

  /* If the array is too small, abort.  */
  if (GFC_DESCRIPTOR_EXTENT(sarray,0) < 13)
    runtime_error ("Array size of SARRAY is too small.");

  /* Make a null terminated copy of the string.  */
  str = fc_strdup (name, name_len);

  /* On platforms that don't provide lstat(), we use stat() instead.  */
#ifdef HAVE_LSTAT
  if (is_lstat)
    val = lstat(str, &sb);
  else
#endif
    val = stat(str, &sb);

  free (str);

  if (val == 0)
    {
      index_type stride = GFC_DESCRIPTOR_STRIDE(sarray,0);

      /* Device ID  */
      sarray->base_addr[0] = sb.st_dev;

      /* Inode number  */
      sarray->base_addr[stride] = sb.st_ino;

      /* File mode  */
      sarray->base_addr[2 * stride] = sb.st_mode;

      /* Number of (hard) links  */
      sarray->base_addr[3 * stride] = sb.st_nlink;

      /* Owner's uid  */
      sarray->base_addr[4 * stride] = sb.st_uid;

      /* Owner's gid  */
      sarray->base_addr[5 * stride] = sb.st_gid;

      /* ID of device containing directory entry for file (0 if not available) */
#if HAVE_STRUCT_STAT_ST_RDEV
      sarray->base_addr[6 * stride] = sb.st_rdev;
#else
      sarray->base_addr[6 * stride] = 0;
#endif

      /* File size (bytes)  */
      sarray->base_addr[7 * stride] = sb.st_size;

      /* Last access time  */
      sarray->base_addr[8 * stride] = sb.st_atime;

      /* Last modification time  */
      sarray->base_addr[9 * stride] = sb.st_mtime;

      /* Last file status change time  */
      sarray->base_addr[10 * stride] = sb.st_ctime;

      /* Preferred I/O block size (-1 if not available)  */
#if HAVE_STRUCT_STAT_ST_BLKSIZE
      sarray->base_addr[11 * stride] = sb.st_blksize;
#else
      sarray->base_addr[11 * stride] = -1;
#endif

      /* Number of blocks allocated (-1 if not available)  */
#if HAVE_STRUCT_STAT_ST_BLOCKS
      sarray->base_addr[12 * stride] = sb.st_blocks;
#else
      sarray->base_addr[12 * stride] = -1;
#endif
    }

  if (status != NULL)
    *status = (val == 0) ? 0 : errno;
}


extern void stat_i8_sub (char *, gfc_array_i8 *, GFC_INTEGER_8 *,
			 gfc_charlen_type);
iexport_proto(stat_i8_sub);

void
stat_i8_sub (char *name, gfc_array_i8 *sarray, GFC_INTEGER_8 *status,
	     gfc_charlen_type name_len)
{
  stat_i8_sub_0 (name, sarray, status, name_len, 0);
}

iexport(stat_i8_sub);


extern void lstat_i8_sub (char *, gfc_array_i8 *, GFC_INTEGER_8 *,
			 gfc_charlen_type);
iexport_proto(lstat_i8_sub);

void
lstat_i8_sub (char *name, gfc_array_i8 *sarray, GFC_INTEGER_8 *status,
	     gfc_charlen_type name_len)
{
  stat_i8_sub_0 (name, sarray, status, name_len, 1);
}

iexport(lstat_i8_sub);


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


/* SUBROUTINE LSTAT(FILE, SARRAY, STATUS)
   CHARACTER(len=*), INTENT(IN) :: FILE
   INTEGER, INTENT(OUT), :: SARRAY(13)
   INTEGER, INTENT(OUT), OPTIONAL :: STATUS

   FUNCTION LSTAT(FILE, SARRAY)
   INTEGER LSTAT
   CHARACTER(len=*), INTENT(IN) :: FILE
   INTEGER, INTENT(OUT), :: SARRAY(13)  */

extern GFC_INTEGER_4 lstat_i4 (char *, gfc_array_i4 *, gfc_charlen_type);
export_proto(lstat_i4);

GFC_INTEGER_4
lstat_i4 (char *name, gfc_array_i4 *sarray, gfc_charlen_type name_len)
{
  GFC_INTEGER_4 val;
  lstat_i4_sub (name, sarray, &val, name_len);
  return val;
}

extern GFC_INTEGER_8 lstat_i8 (char *, gfc_array_i8 *, gfc_charlen_type);
export_proto(lstat_i8);

GFC_INTEGER_8
lstat_i8 (char *name, gfc_array_i8 *sarray, gfc_charlen_type name_len)
{
  GFC_INTEGER_8 val;
  lstat_i8_sub (name, sarray, &val, name_len);
  return val;
}

#endif


#ifdef HAVE_FSTAT

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
  if (GFC_DESCRIPTOR_EXTENT(sarray,0) < 13)
    runtime_error ("Array size of SARRAY is too small.");

  /* Convert Fortran unit number to C file descriptor.  */
  val = unit_to_fd (*unit);
  if (val >= 0)
    val = fstat(val, &sb);

  if (val == 0)
    {
      index_type stride = GFC_DESCRIPTOR_STRIDE(sarray,0);

      /* Device ID  */
      sarray->base_addr[0 * stride] = sb.st_dev;

      /* Inode number  */
      sarray->base_addr[1 * stride] = sb.st_ino;

      /* File mode  */
      sarray->base_addr[2 * stride] = sb.st_mode;

      /* Number of (hard) links  */
      sarray->base_addr[3 * stride] = sb.st_nlink;

      /* Owner's uid  */
      sarray->base_addr[4 * stride] = sb.st_uid;

      /* Owner's gid  */
      sarray->base_addr[5 * stride] = sb.st_gid;

      /* ID of device containing directory entry for file (0 if not available) */
#if HAVE_STRUCT_STAT_ST_RDEV
      sarray->base_addr[6 * stride] = sb.st_rdev;
#else
      sarray->base_addr[6 * stride] = 0;
#endif

      /* File size (bytes)  */
      sarray->base_addr[7 * stride] = sb.st_size;

      /* Last access time  */
      sarray->base_addr[8 * stride] = sb.st_atime;

      /* Last modification time  */
      sarray->base_addr[9 * stride] = sb.st_mtime;

      /* Last file status change time  */
      sarray->base_addr[10 * stride] = sb.st_ctime;

      /* Preferred I/O block size (-1 if not available)  */
#if HAVE_STRUCT_STAT_ST_BLKSIZE
      sarray->base_addr[11 * stride] = sb.st_blksize;
#else
      sarray->base_addr[11 * stride] = -1;
#endif

      /* Number of blocks allocated (-1 if not available)  */
#if HAVE_STRUCT_STAT_ST_BLOCKS
      sarray->base_addr[12 * stride] = sb.st_blocks;
#else
      sarray->base_addr[12 * stride] = -1;
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
  if (GFC_DESCRIPTOR_EXTENT(sarray,0) < 13)
    runtime_error ("Array size of SARRAY is too small.");

  /* Convert Fortran unit number to C file descriptor.  */
  val = unit_to_fd ((int) *unit);
  if (val >= 0)
    val = fstat(val, &sb);

  if (val == 0)
    {
      index_type stride = GFC_DESCRIPTOR_STRIDE(sarray,0);

      /* Device ID  */
      sarray->base_addr[0] = sb.st_dev;

      /* Inode number  */
      sarray->base_addr[stride] = sb.st_ino;

      /* File mode  */
      sarray->base_addr[2 * stride] = sb.st_mode;

      /* Number of (hard) links  */
      sarray->base_addr[3 * stride] = sb.st_nlink;

      /* Owner's uid  */
      sarray->base_addr[4 * stride] = sb.st_uid;

      /* Owner's gid  */
      sarray->base_addr[5 * stride] = sb.st_gid;

      /* ID of device containing directory entry for file (0 if not available) */
#if HAVE_STRUCT_STAT_ST_RDEV
      sarray->base_addr[6 * stride] = sb.st_rdev;
#else
      sarray->base_addr[6 * stride] = 0;
#endif

      /* File size (bytes)  */
      sarray->base_addr[7 * stride] = sb.st_size;

      /* Last access time  */
      sarray->base_addr[8 * stride] = sb.st_atime;

      /* Last modification time  */
      sarray->base_addr[9 * stride] = sb.st_mtime;

      /* Last file status change time  */
      sarray->base_addr[10 * stride] = sb.st_ctime;

      /* Preferred I/O block size (-1 if not available)  */
#if HAVE_STRUCT_STAT_ST_BLKSIZE
      sarray->base_addr[11 * stride] = sb.st_blksize;
#else
      sarray->base_addr[11 * stride] = -1;
#endif

      /* Number of blocks allocated (-1 if not available)  */
#if HAVE_STRUCT_STAT_ST_BLOCKS
      sarray->base_addr[12 * stride] = sb.st_blocks;
#else
      sarray->base_addr[12 * stride] = -1;
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

#endif
