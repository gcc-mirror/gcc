/* Implementation of the FGET, FGETC, FPUT, FPUTC, FLUSH 
   FTELL, TTYNAM and ISATTY intrinsics.
   Copyright (C) 2005, 2007 Free Software Foundation, Inc.

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
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "io.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <string.h>

static const int five = 5;
static const int six = 6;

extern int PREFIX(fgetc) (const int *, char *, gfc_charlen_type);
export_proto_np(PREFIX(fgetc));

int
PREFIX(fgetc) (const int * unit, char * c, gfc_charlen_type c_len)
{
  int ret;
  size_t s;
  gfc_unit * u = find_unit (*unit);

  if (u == NULL)
    return -1;

  s = 1;
  memset (c, ' ', c_len);
  ret = sread (u->s, c, &s);
  unlock_unit (u);

  if (ret != 0)
    return ret;

  if (s != 1)
    return -1;
  else
    return 0;
}


#define FGETC_SUB(kind) \
  extern void fgetc_i ## kind ## _sub \
    (const int *, char *, GFC_INTEGER_ ## kind *, gfc_charlen_type); \
  export_proto(fgetc_i ## kind ## _sub); \
  void fgetc_i ## kind ## _sub \
  (const int * unit, char * c, GFC_INTEGER_ ## kind * st, gfc_charlen_type c_len) \
    { if (st != NULL) \
        *st = PREFIX(fgetc) (unit, c, c_len); \
      else \
        PREFIX(fgetc) (unit, c, c_len); }

FGETC_SUB(1)
FGETC_SUB(2)
FGETC_SUB(4)
FGETC_SUB(8)


extern int PREFIX(fget) (char *, gfc_charlen_type);
export_proto_np(PREFIX(fget));

int
PREFIX(fget) (char * c, gfc_charlen_type c_len)
{
  return PREFIX(fgetc) (&five, c, c_len);
}


#define FGET_SUB(kind) \
  extern void fget_i ## kind ## _sub \
    (char *, GFC_INTEGER_ ## kind *, gfc_charlen_type); \
  export_proto(fget_i ## kind ## _sub); \
  void fget_i ## kind ## _sub \
  (char * c, GFC_INTEGER_ ## kind * st, gfc_charlen_type c_len) \
    { if (st != NULL) \
        *st = PREFIX(fgetc) (&five, c, c_len); \
      else \
        PREFIX(fgetc) (&five, c, c_len); }

FGET_SUB(1)
FGET_SUB(2)
FGET_SUB(4)
FGET_SUB(8)



extern int PREFIX(fputc) (const int *, char *, gfc_charlen_type);
export_proto_np(PREFIX(fputc));

int
PREFIX(fputc) (const int * unit, char * c,
	       gfc_charlen_type c_len __attribute__((unused)))
{
  size_t s;
  int ret;
  gfc_unit * u = find_unit (*unit);

  if (u == NULL)
    return -1;

  s = 1;
  ret = swrite (u->s, c, &s);
  unlock_unit (u);
  return ret;
}


#define FPUTC_SUB(kind) \
  extern void fputc_i ## kind ## _sub \
    (const int *, char *, GFC_INTEGER_ ## kind *, gfc_charlen_type); \
  export_proto(fputc_i ## kind ## _sub); \
  void fputc_i ## kind ## _sub \
  (const int * unit, char * c, GFC_INTEGER_ ## kind * st, gfc_charlen_type c_len) \
    { if (st != NULL) \
        *st = PREFIX(fputc) (unit, c, c_len); \
      else \
        PREFIX(fputc) (unit, c, c_len); }

FPUTC_SUB(1)
FPUTC_SUB(2)
FPUTC_SUB(4)
FPUTC_SUB(8)


extern int PREFIX(fput) (char *, gfc_charlen_type);
export_proto_np(PREFIX(fput));

int
PREFIX(fput) (char * c, gfc_charlen_type c_len)
{
  return PREFIX(fputc) (&six, c, c_len);
}


#define FPUT_SUB(kind) \
  extern void fput_i ## kind ## _sub \
    (char *, GFC_INTEGER_ ## kind *, gfc_charlen_type); \
  export_proto(fput_i ## kind ## _sub); \
  void fput_i ## kind ## _sub \
  (char * c, GFC_INTEGER_ ## kind * st, gfc_charlen_type c_len) \
    { if (st != NULL) \
        *st = PREFIX(fputc) (&six, c, c_len); \
      else \
        PREFIX(fputc) (&six, c, c_len); }

FPUT_SUB(1)
FPUT_SUB(2)
FPUT_SUB(4)
FPUT_SUB(8)


/* SUBROUTINE FLUSH(UNIT)
   INTEGER, INTENT(IN), OPTIONAL :: UNIT  */

extern void flush_i4 (GFC_INTEGER_4 *);
export_proto(flush_i4);

void
flush_i4 (GFC_INTEGER_4 *unit)
{
  gfc_unit *us;

  /* flush all streams */
  if (unit == NULL)
    flush_all_units ();
  else
    {
      us = find_unit (*unit);
      if (us != NULL)
	{
	  flush (us->s);
	  unlock_unit (us);
	}
    }
}


extern void flush_i8 (GFC_INTEGER_8 *);
export_proto(flush_i8);

void
flush_i8 (GFC_INTEGER_8 *unit)
{
  gfc_unit *us;

  /* flush all streams */
  if (unit == NULL)
    flush_all_units ();
  else
    {
      us = find_unit (*unit);
      if (us != NULL)
	{
	  flush (us->s);
	  unlock_unit (us);
	}
    }
}

/* FSEEK intrinsic */

extern void fseek_sub (int *, GFC_IO_INT *, int *, int *);
export_proto(fseek_sub);

void
fseek_sub (int * unit, GFC_IO_INT * offset, int * whence, int * status)
{
  gfc_unit * u = find_unit (*unit);
  try result = FAILURE;

  if (u != NULL && is_seekable(u->s))
    {
      if (*whence == 0)
        result = sseek(u->s, *offset);                       /* SEEK_SET */
      else if (*whence == 1)
        result = sseek(u->s, file_position(u->s) + *offset); /* SEEK_CUR */
      else if (*whence == 2)
        result = sseek(u->s, file_length(u->s) + *offset);   /* SEEK_END */

      unlock_unit (u);
    }

  if (status)
    *status = (result == FAILURE ? -1 : 0);
}



/* FTELL intrinsic */

extern size_t PREFIX(ftell) (int *);
export_proto_np(PREFIX(ftell));

size_t
PREFIX(ftell) (int * unit)
{
  gfc_unit * u = find_unit (*unit);
  size_t ret;
  if (u == NULL)
    return ((size_t) -1);
  ret = (size_t) stream_offset (u->s);
  unlock_unit (u);
  return ret;
}

#define FTELL_SUB(kind) \
  extern void ftell_i ## kind ## _sub (int *, GFC_INTEGER_ ## kind *); \
  export_proto(ftell_i ## kind ## _sub); \
  void \
  ftell_i ## kind ## _sub (int * unit, GFC_INTEGER_ ## kind * offset) \
  { \
    gfc_unit * u = find_unit (*unit); \
    if (u == NULL) \
      *offset = -1; \
    else \
      { \
	*offset = stream_offset (u->s); \
	unlock_unit (u); \
      } \
  }

FTELL_SUB(1)
FTELL_SUB(2)
FTELL_SUB(4)
FTELL_SUB(8)



/* LOGICAL FUNCTION ISATTY(UNIT)
   INTEGER, INTENT(IN) :: UNIT */

extern GFC_LOGICAL_4 isatty_l4 (int *);
export_proto(isatty_l4);

GFC_LOGICAL_4
isatty_l4 (int *unit)
{
  gfc_unit *u;
  GFC_LOGICAL_4 ret = 0;

  u = find_unit (*unit);
  if (u != NULL)
    {
      ret = (GFC_LOGICAL_4) stream_isatty (u->s);
      unlock_unit (u);
    }
  return ret;
}


extern GFC_LOGICAL_8 isatty_l8 (int *);
export_proto(isatty_l8);

GFC_LOGICAL_8
isatty_l8 (int *unit)
{
  gfc_unit *u;
  GFC_LOGICAL_8 ret = 0;

  u = find_unit (*unit);
  if (u != NULL)
    {
      ret = (GFC_LOGICAL_8) stream_isatty (u->s);
      unlock_unit (u);
    }
  return ret;
}


/* SUBROUTINE TTYNAM(UNIT,NAME)
   INTEGER,SCALAR,INTENT(IN) :: UNIT
   CHARACTER,SCALAR,INTENT(OUT) :: NAME */

extern void ttynam_sub (int *, char *, gfc_charlen_type);
export_proto(ttynam_sub);

void
ttynam_sub (int *unit, char * name, gfc_charlen_type name_len)
{
  gfc_unit *u;
  char * n;
  int i;

  memset (name, ' ', name_len);
  u = find_unit (*unit);
  if (u != NULL)
    {
      n = stream_ttyname (u->s);
      if (n != NULL)
	{
	  i = 0;
	  while (*n && i < name_len)
	    name[i++] = *(n++);
	}
      unlock_unit (u);
    }
}


extern void ttynam (char **, gfc_charlen_type *, int);
export_proto(ttynam);

void
ttynam (char ** name, gfc_charlen_type * name_len, int unit)
{
  gfc_unit *u;

  u = find_unit (unit);
  if (u != NULL)
    {
      *name = stream_ttyname (u->s);
      if (*name != NULL)
	{
	  *name_len = strlen (*name);
	  *name = strdup (*name);
	  unlock_unit (u);
	  return;
	}
      unlock_unit (u);
    }

  *name_len = 0;
  *name = NULL;
}
