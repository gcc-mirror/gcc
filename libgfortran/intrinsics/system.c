/* Implementation of the SYSTEM intrinsic.
   Copyright (C) 2004, 2007 Free Software Foundation, Inc.
   Contributed by Tobias Schlüter.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combined
executable.)

Libgfortran is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with libgfortran; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "libgfortran.h"
#include <string.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

extern void system_sub (const char *fcmd, GFC_INTEGER_4 * status,
			gfc_charlen_type cmd_len);
iexport_proto(system_sub);

void
system_sub (const char *fcmd, GFC_INTEGER_4 *status, gfc_charlen_type cmd_len)
{
  char cmd[cmd_len + 1];
  int stat;

  /* Flush all I/O units before executing the command.  */
  flush_all_units();

  memcpy (cmd, fcmd, cmd_len);
  cmd[cmd_len] = '\0';

  stat = system (cmd);
  if (status)
    *status = stat;
}
iexport(system_sub);

extern GFC_INTEGER_4 PREFIX(system) (const char *, gfc_charlen_type);
export_proto_np(PREFIX(system));

GFC_INTEGER_4
PREFIX(system) (const char *fcmd, gfc_charlen_type cmd_len)
{
  GFC_INTEGER_4 stat;
  system_sub (fcmd, &stat, cmd_len);
  return stat;
}
