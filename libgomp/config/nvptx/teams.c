/* Copyright (C) 2015-2024 Free Software Foundation, Inc.
   Contributed by Alexander Monakov <amonakov@ispras.ru>

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This file defines OpenMP API entry points that accelerator targets are
   expected to replace.  */

#include "libgomp.h"

extern int __gomp_team_num __attribute__((shared));

void
GOMP_teams_reg (void (*fn) (void *), void *data, unsigned int num_teams,
		unsigned int thread_limit, unsigned int flags)
{
  (void) fn;
  (void) data;
  (void) flags;
  (void) num_teams;
  (void) thread_limit;
}

int
omp_get_num_teams (void)
{
  return gomp_num_teams_var + 1;
}

int
omp_get_team_num (void)
{
  return __gomp_team_num;
}

ialias (omp_get_num_teams)
ialias (omp_get_team_num)
