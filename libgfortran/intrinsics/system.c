/* Implementation of the SYSTEM intrinsic.
   Copyright (C) 2004, 2007, 2009, 2011 Free Software Foundation, Inc.
   Contributed by Tobias Schlüter.

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

Libgfortran is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libgfortran.h"
#include <string.h>
#include <stdlib.h>

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
