/* Implementation of the FGET, FGETC, FPUT and FPUTC intrinsics.
   Copyright (C) 2005 Free Software Foundation, Inc.
   Contributed by François-Xavier Coudert <coudert@clipper.ens.fr>

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

#include "config.h"
#include "libgfortran.h"

#include <string.h>

#include "../io/io.h"

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
  gfc_unit * u = find_unit (*unit);

  if (u == NULL)
    return -1;

  s = 1;
  return swrite (u->s, c, &s);
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

