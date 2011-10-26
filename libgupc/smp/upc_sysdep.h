/* GNU UPC Runtime Operating System and Target Platform Dependent Support
   Copyright (C) 2003 Free Software Foundation, Inc.
   Free Software Foundation, Inc. 
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   As a special exception, if you link this library with files
   compiled with a GNU compiler to produce an executable, this does
   not cause the resulting executable to be covered by the GNU General
   Public License.  This exception does not however invalidate any
   other reasons why the executable file might be covered by the GNU
   General Public License.  */

#ifndef _UPC_OS_H_
#define _UPC_OS_H_


/* An actual heap is required only for the SGI Irix
   based systems, because spin lock related data
   structures must live there.  The runtime doesn't
   otherwise need its own heap, so a null handle is
   passed around.  */
#ifdef __sgi__
typedef usptr_t os_heap_t;
#else
typedef void *os_heap_t;
#endif
typedef os_heap_t *os_heap_p;

#ifdef __sgi__
typedef volatile ptrdiff_t os_atomic_t;
#else
typedef volatile int os_atomic_t;
#endif
typedef os_atomic_t *os_atomic_p;

#define OS_ATOMIC_WORD_SIZE (sizeof(os_atomic_t))
#define OS_BITS_PER_ATOMIC_WORD (OS_ATOMIC_WORD_SIZE * 8)

#ifdef __sgi__
typedef ulock_t os_lock_t;
#else
typedef os_atomic_t os_lock_t;
#endif
typedef os_lock_t *os_lock_p;

/* Number of cpu's available */
extern int __upc_num_cpus;

/* Max. number of iterations to poll waiting for a
 * spinlock loop condition to be satisfied.
 */
#define OS_MAX_SPIN_COUNT (__upc_num_cpus > 1 ? 500 : 0)

/* Give up control of the cpu for a small time interval. */
#ifdef __sgi__
#define __upc_yield_cpu() do { sginap(0); } while (0)
#else
# ifdef _POSIX_PRIORITY_SCHEDULING
# define __upc_yield_cpu() do { sched_yield(); } while (0)
# else
# define __upc_yield_cpu() do { usleep(1000L); } while (0)
# endif
#endif

/* Keep spinning until PREDICATE is true,
 * (this needs to be a macro, to ensure that
 * PREDICATE is re-evaluated on each iteration. */
#define __upc_spin_until(PREDICATE) \
    { \
      int i = 0; \
      while (!(PREDICATE)) \
	{ \
	  if (++i >= OS_MAX_SPIN_COUNT) \
	    { \
	      __upc_yield_cpu (); \
	      i = 0; \
	    } \
	} \
    }

extern void __upc_sys_init (void);

extern int __upc_atomic_cas (os_atomic_p, os_atomic_t, os_atomic_t);
extern int __upc_atomic_get_bit (os_atomic_p, int);
extern void __upc_atomic_set_bit (os_atomic_p, int);

extern void __upc_init_lock (os_lock_p);
extern void __upc_acquire_lock (os_lock_p);
extern int __upc_try_acquire_lock (os_lock_p);
extern void __upc_release_lock (os_lock_p);

extern os_heap_p __upc_create_runtime_heap (size_t, const char **);
extern void *__upc_runtime_alloc (size_t, os_heap_p *, const char **);
extern int __upc_create_global_mem_file (char *tmp_fname, const char **err_msg);
extern char *__upc_strsignal (int);

#endif /* !_UPC_OS_H_ */
