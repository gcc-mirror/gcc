/* Implementation of the FGET, FGETC, FPUT, FPUTC, FLUSH 
   FTELL, TTYNAM and ISATTY intrinsics.
   Copyright (C) 2005, 2007, 2009, 2010, 2011, 2012 Free Software
   Foundation, Inc.

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

#include "io.h"
#include "fbuf.h"
#include "unix.h"
#include <stdlib.h>
#include <string.h>


static const int five = 5;
static const int six = 6;

extern int PREFIX(fgetc) (const int *, char *, gfc_charlen_type);
export_proto_np(PREFIX(fgetc));

int
PREFIX(fgetc) (const int * unit, char * c, gfc_charlen_type c_len)
{
  int ret;
  gfc_unit * u = find_unit (*unit);

  if (u == NULL)
    return -1;

  fbuf_reset (u);
  if (u->mode == WRITING)
    {
      sflush (u->s);
      u->mode = READING;
    }

  memset (c, ' ', c_len);
  ret = sread (u->s, c, 1);
  unlock_unit (u);

  if (ret < 0)
    return ret;

  if (ret != 1)
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
  ssize_t s;
  gfc_unit * u = find_unit (*unit);

  if (u == NULL)
    return -1;

  fbuf_reset (u);
  if (u->mode == READING)
    {
      sflush (u->s);
      u->mode = WRITING;
    }

  s = swrite (u->s, c, 1);
  unlock_unit (u);
  if (s < 0)
    return -1;
  return 0;
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
	  sflush (us->s);
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
	  sflush (us->s);
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
  ssize_t result = -1;

  if (u != NULL)
    {
      result = sseek(u->s, *offset, *whence);

      unlock_unit (u);
    }

  if (status)
    *status = (result < 0 ? -1 : 0);
}



/* FTELL intrinsic */

static gfc_offset
gf_ftell (int unit)
{
  gfc_unit * u = find_unit (unit);
  if (u == NULL)
    return -1;
  int pos = fbuf_reset (u);
  if (pos != 0)
    sseek (u->s, pos, SEEK_CUR);
  gfc_offset ret = stell (u->s);
  unlock_unit (u);
  return ret;
}


/* Here is the ftell function with an incorrect return type; retained
   due to ABI compatibility.  */

extern size_t PREFIX(ftell) (int *);
export_proto_np(PREFIX(ftell));

size_t
PREFIX(ftell) (int * unit)
{
  return gf_ftell (*unit);
}


/* Here is the ftell function with the correct return type, ensuring
   that large files can be supported as long as the target supports
   large integers; as of 4.8 the FTELL intrinsic function will call
   this one instead of the old ftell above.  */

extern GFC_IO_INT PREFIX(ftell2) (int *);
export_proto_np(PREFIX(ftell2));

GFC_IO_INT
PREFIX(ftell2) (int * unit)
{
  return gf_ftell (*unit);
}


#define FTELL_SUB(kind) \
  extern void ftell_i ## kind ## _sub (int *, GFC_INTEGER_ ## kind *); \
  export_proto(ftell_i ## kind ## _sub); \
  void \
  ftell_i ## kind ## _sub (int * unit, GFC_INTEGER_ ## kind * offset) \
  { \
    *offset = gf_ftell (*unit);			\
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
  int nlen;
  int err = 1;

  u = find_unit (*unit);
  if (u != NULL)
    {
      err = stream_ttyname (u->s, name, name_len);
      if (err == 0)
	{
	  nlen = strlen (name);
	  memset (&name[nlen], ' ', name_len - nlen);
	}

      unlock_unit (u);
    }
  if (err != 0)
    memset (name, ' ', name_len);
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
      *name = xmalloc (TTY_NAME_MAX);
      int err = stream_ttyname (u->s, *name, TTY_NAME_MAX);
      if (err == 0)
	{
	  *name_len = strlen (*name);
	  unlock_unit (u);
	  return;
	}
      free (*name);
      unlock_unit (u);
    }

  *name_len = 0;
  *name = NULL;
}
