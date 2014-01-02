/* Copyright (C) 2011-2014 Free Software Foundation, Inc.
   Contributed by Uros Bizjak <ubizjak@gmail.com>

   This file is part of the GNU OpenMP Library (libgomp).

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

#ifndef GOMP_PROC_H
#define GOMP_PROC_H 1

#include <sched.h>

#ifdef HAVE_PTHREAD_AFFINITY_NP
extern unsigned long gomp_cpuset_size attribute_hidden;
extern cpu_set_t *gomp_cpusetp attribute_hidden;
extern unsigned long gomp_cpuset_popcount (unsigned long, cpu_set_t *)
     attribute_hidden;
#endif

#endif /* GOMP_PROC_H */
