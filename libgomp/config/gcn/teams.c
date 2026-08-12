/* Copyright (C) 2015-2026 Free Software Foundation, Inc.

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

/* For a distribute construct with static schedule, return the team ID and
   number of teams packed into a single complex value. NITER is the total
   number of iterations.  */

_Complex int
GOMP_distribute_static_worksharing (unsigned long long niter
				    __attribute__ ((unused)))
{
  int __lds *gomp_team_num = (int __lds *) GOMP_TEAM_NUM;
  unsigned tid = *gomp_team_num;
  unsigned nteams = gomp_num_teams_var + 1;
  return nteams + tid * 1I;
}

/* OMPT variant enabled by -fopenmp-ompt.  */

_Complex int
GOMP_distribute_static_worksharing_start (unsigned long long niter
					  __attribute__ ((unused)))
{
  int __lds *gomp_team_num = (int __lds *) GOMP_TEAM_NUM;
  unsigned tid = *gomp_team_num;
  unsigned nteams = gomp_num_teams_var + 1;
  return nteams + tid * 1I;
}

/* Stub for OMPT callback enabled by -fopenmp-ompt=extended. START is the
   starting index of the chunk in the logical iteration space. ITERATIONS is the
   number of iterations in the chunk.  */

void
GOMP_distribute_static_worksharing_dispatch (unsigned long long start
					     __attribute__ ((unused)),
					     unsigned long long iterations
					     __attribute__ ((unused)))
{}

/* Stub for OMPT callback enabled by -fopenmp-ompt.  */

void
GOMP_distribute_static_worksharing_end (void)
{}

int
omp_get_num_teams (void)
{
  return gomp_num_teams_var + 1;
}

int
omp_get_num_teams_dim (int dim)
{
  if (dim == 0)
    return omp_get_num_teams ();
  return 1;
}

int
omp_get_team_num (void)
{
  int __lds *gomp_team_num = (int __lds *) GOMP_TEAM_NUM;
  return *gomp_team_num;
}

int
omp_get_team_num_dim (int dim)
{
  if (dim == 0)
    return omp_get_team_num ();
  return 0;
}

ialias (omp_get_num_teams)
ialias (omp_get_num_teams_dim)
ialias (omp_get_team_num)
ialias (omp_get_team_num_dim)
