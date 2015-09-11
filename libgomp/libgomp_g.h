/* Copyright (C) 2005-2015 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

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

/* This file contains prototypes of functions in the external ABI.
   This file is included by files in the testsuite.  */

#ifndef LIBGOMP_G_H 
#define LIBGOMP_G_H 1

#include <stdbool.h>
#include <stddef.h>

/* barrier.c */

extern void GOMP_barrier (void);
extern bool GOMP_barrier_cancel (void);

/* critical.c */

extern void GOMP_critical_start (void);
extern void GOMP_critical_end (void);
extern void GOMP_critical_name_start (void **);
extern void GOMP_critical_name_end (void **);
extern void GOMP_atomic_start (void);
extern void GOMP_atomic_end (void);

/* loop.c */

extern bool GOMP_loop_static_start (long, long, long, long, long *, long *);
extern bool GOMP_loop_dynamic_start (long, long, long, long, long *, long *);
extern bool GOMP_loop_guided_start (long, long, long, long, long *, long *);
extern bool GOMP_loop_runtime_start (long, long, long, long *, long *);

extern bool GOMP_loop_ordered_static_start (long, long, long, long,
					    long *, long *);
extern bool GOMP_loop_ordered_dynamic_start (long, long, long, long,
					     long *, long *);
extern bool GOMP_loop_ordered_guided_start (long, long, long, long,
					    long *, long *);
extern bool GOMP_loop_ordered_runtime_start (long, long, long, long *, long *);

extern bool GOMP_loop_static_next (long *, long *);
extern bool GOMP_loop_dynamic_next (long *, long *);
extern bool GOMP_loop_guided_next (long *, long *);
extern bool GOMP_loop_runtime_next (long *, long *);

extern bool GOMP_loop_ordered_static_next (long *, long *);
extern bool GOMP_loop_ordered_dynamic_next (long *, long *);
extern bool GOMP_loop_ordered_guided_next (long *, long *);
extern bool GOMP_loop_ordered_runtime_next (long *, long *);

extern void GOMP_parallel_loop_static_start (void (*)(void *), void *,
					     unsigned, long, long, long, long);
extern void GOMP_parallel_loop_dynamic_start (void (*)(void *), void *,
					     unsigned, long, long, long, long);
extern void GOMP_parallel_loop_guided_start (void (*)(void *), void *,
					     unsigned, long, long, long, long);
extern void GOMP_parallel_loop_runtime_start (void (*)(void *), void *,
					      unsigned, long, long, long);
extern void GOMP_parallel_loop_static (void (*)(void *), void *,
				       unsigned, long, long, long, long,
				       unsigned);
extern void GOMP_parallel_loop_dynamic (void (*)(void *), void *,
					unsigned, long, long, long, long,
					unsigned);
extern void GOMP_parallel_loop_guided (void (*)(void *), void *,
				       unsigned, long, long, long, long,
				       unsigned);
extern void GOMP_parallel_loop_runtime (void (*)(void *), void *,
					unsigned, long, long, long,
					unsigned);

extern void GOMP_loop_end (void);
extern void GOMP_loop_end_nowait (void);
extern bool GOMP_loop_end_cancel (void);

/* loop_ull.c */

extern bool GOMP_loop_ull_static_start (bool, unsigned long long,
					unsigned long long,
					unsigned long long,
					unsigned long long,
					unsigned long long *,
					unsigned long long *);
extern bool GOMP_loop_ull_dynamic_start (bool, unsigned long long,
					 unsigned long long,
					 unsigned long long,
					 unsigned long long,
					 unsigned long long *,
					 unsigned long long *);
extern bool GOMP_loop_ull_guided_start (bool, unsigned long long,
					unsigned long long,
					unsigned long long,
					unsigned long long,
					unsigned long long *,
					unsigned long long *);
extern bool GOMP_loop_ull_runtime_start (bool, unsigned long long,
					 unsigned long long,
					 unsigned long long,
					 unsigned long long *,
					 unsigned long long *);

extern bool GOMP_loop_ull_ordered_static_start (bool, unsigned long long,
						unsigned long long,
						unsigned long long,
						unsigned long long,
						unsigned long long *,
						unsigned long long *);
extern bool GOMP_loop_ull_ordered_dynamic_start (bool, unsigned long long,
						 unsigned long long,
						 unsigned long long,
						 unsigned long long,
						 unsigned long long *,
						 unsigned long long *);
extern bool GOMP_loop_ull_ordered_guided_start (bool, unsigned long long,
						unsigned long long,
						unsigned long long,
						unsigned long long,
						unsigned long long *,
						unsigned long long *);
extern bool GOMP_loop_ull_ordered_runtime_start (bool, unsigned long long,
						 unsigned long long,
						 unsigned long long,
						 unsigned long long *,
						 unsigned long long *);

extern bool GOMP_loop_ull_static_next (unsigned long long *,
				       unsigned long long *);
extern bool GOMP_loop_ull_dynamic_next (unsigned long long *,
					unsigned long long *);
extern bool GOMP_loop_ull_guided_next (unsigned long long *,
				       unsigned long long *);
extern bool GOMP_loop_ull_runtime_next (unsigned long long *,
					unsigned long long *);

extern bool GOMP_loop_ull_ordered_static_next (unsigned long long *,
					       unsigned long long *);
extern bool GOMP_loop_ull_ordered_dynamic_next (unsigned long long *,
						unsigned long long *);
extern bool GOMP_loop_ull_ordered_guided_next (unsigned long long *,
					       unsigned long long *);
extern bool GOMP_loop_ull_ordered_runtime_next (unsigned long long *,
						unsigned long long *);

/* ordered.c */

extern void GOMP_ordered_start (void);
extern void GOMP_ordered_end (void);

/* parallel.c */

extern void GOMP_parallel_start (void (*) (void *), void *, unsigned);
extern void GOMP_parallel_end (void);
extern void GOMP_parallel (void (*) (void *), void *, unsigned, unsigned);
extern bool GOMP_cancel (int, bool);
extern bool GOMP_cancellation_point (int);

/* task.c */

extern void GOMP_task (void (*) (void *), void *, void (*) (void *, void *),
		       long, long, bool, unsigned, void **);
extern void GOMP_taskwait (void);
extern void GOMP_taskyield (void);
extern void GOMP_taskgroup_start (void);
extern void GOMP_taskgroup_end (void);

/* sections.c */

extern unsigned GOMP_sections_start (unsigned);
extern unsigned GOMP_sections_next (void);
extern void GOMP_parallel_sections_start (void (*) (void *), void *,
					  unsigned, unsigned);
extern void GOMP_parallel_sections (void (*) (void *), void *,
				    unsigned, unsigned, unsigned);
extern void GOMP_sections_end (void);
extern void GOMP_sections_end_nowait (void);
extern bool GOMP_sections_end_cancel (void);

/* single.c */

extern bool GOMP_single_start (void);
extern void *GOMP_single_copy_start (void);
extern void GOMP_single_copy_end (void *);

/* target.c */

extern void GOMP_target (int, void (*) (void *), const void *,
			 size_t, void **, size_t *, unsigned char *);
extern void GOMP_target_data (int, const void *,
			      size_t, void **, size_t *, unsigned char *);
extern void GOMP_target_end_data (void);
extern void GOMP_target_update (int, const void *,
				size_t, void **, size_t *, unsigned char *);
extern void GOMP_teams (unsigned int, unsigned int);

/* oacc-parallel.c */

extern void GOACC_data_start (int, size_t, void **, size_t *,
			      unsigned short *);
extern void GOACC_data_end (void);
extern void GOACC_enter_exit_data (int, size_t, void **,
				   size_t *, unsigned short *, int, int, ...);
extern void GOACC_parallel (int, void (*) (void *), size_t,
			    void **, size_t *, unsigned short *, int, int, int,
			    int, int, ...);
extern void GOACC_update (int, size_t, void **, size_t *,
			  unsigned short *, int, int, ...);
extern void GOACC_wait (int, int, ...);
extern int GOACC_get_num_threads (void);
extern int GOACC_get_thread_num (void);

#endif /* LIBGOMP_G_H */
